(ns gesso.live.outbox.http-operation
  "Selected canonical HTMX command transport. The checked assembly supplies
   both the physical route and the exact prepared server. The application's
   request gate and registered operation remain trusted, not certified."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [gesso.http :as http]
            [gesso.live.application-preflight :as application]
            [gesso.live.outbox.http-route :as route]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.optimistic.choreo :as choreo]
            [gesso.live.optimistic.server :as server]
            [gesso.live.ui :as ui]
            [ring.util.codec :as codec])
  (:import [java.io InputStream PushbackReader StringReader]
           [java.nio.charset StandardCharsets]))

(def command-parameter "__gesso_live_optimistic_command")
(def ^:private max-form-bytes 65536)

(defn- reject! [reason data]
  (throw (ex-info "Canonical HTMX operation transport not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-http-operation
                         :reason reason))))

(defn- selected-routes! [assembly prepared routes]
  (let [realizations (get-in assembly [:operation-acquisition-assembly
                                      :acquisition-assembly :realizations])
        other-paths (mapcat (fn [[_ realization]]
                              (keep #(get-in realization [% :path])
                                    [:fragment-route :stream-route])) realizations)]
    (reduce (fn [selected {:keys [path method operation required-transport] :as route}]
              (let [parts (route/route-segments path)]
                (when-not (and parts (= :post method)
                               (= :htmx required-transport)
                               (contains? #{nil 200} (get-in prepared [:operations operation
                                                                     :response-status-contract :status])))
                  (reject! :unsupported-command-route {:route-id (:route-id route)}))
                (when (some #(route/templates-overlap? path %)
                            (concat (map :path selected) other-paths))
                  (reject! :ambiguous-command-route {:path path}))
                (conj selected (assoc route :transport-segments parts))))
            [] (sort-by (comp pr-str :route-id) (vals routes)))))

(defn- response [status]
  {:status status :headers {"cache-control" "no-store"
                            "content-type" "text/plain; charset=utf-8"}
   :body "Command request not admitted."})

(defn- decode-form [text]
  ;; Reject ambiguous/malformed fields rather than silently dropping them.
  ;; The selected command transport uses single-valued form fields only.
  (reduce (fn [form field]
            (let [[k v] (str/split field #"=" 2)
                  key (codec/form-decode-str k)
                  value (when v (codec/form-decode-str v))]
              (if (and (string? key) (not (str/blank? key)) (string? value)
                       (not (contains? form key)))
                (assoc form key value)
                (reduced nil))))
          {} (str/split text #"&" -1)))

(defn- read-form [request]
  (let [content-type (get-in request [:headers "content-type"] "")
        body (:body request)]
    (when (and (string? content-type)
               (= "application/x-www-form-urlencoded"
                  (str/lower-case (str/trim (first (str/split content-type #";" 2)))))
               (not (seq (:query-string request))))
      (let [bytes (cond
                    (string? body) (.getBytes ^String body StandardCharsets/UTF_8)
                    (instance? InputStream body) (.readNBytes ^InputStream body (inc max-form-bytes)))]
        (when (and bytes (<= (alength ^bytes bytes) max-form-bytes))
          (let [form (decode-form (String. ^bytes bytes StandardCharsets/UTF_8))
                encoded (get form command-parameter)]
            (when (string? encoded)
              (with-open [reader (PushbackReader. (StringReader. encoded))]
                (let [wire (edn/read {:eof ::eof} reader)]
                  (when (= ::eof (edn/read {:eof ::eof} reader))
                    {:form form :command (server/decode-command wire)}))))))))))

(defn wrap-handler
  "Own all methods on every admitted HTMX POST operation path.
   Simple :parameter segments bind exact same-named string command arguments.
   No application-specific coercion or alternative parameter mapping.
   Require an explicit trusted request gate (including CSRF/authentication
   policy as appropriate). Parse only the bounded raw UTF-8 form body, not
   caller-supplied Ring params. Execute the admitted operation only when the
   wire operation agrees with the selected physical route. No alternate
   executor or response builder is accepted. Other paths remain trusted
   fallback code; complete writer coverage is not established. The returned
   marker is a settlement handoff, not evidence of browser receipt."
  ([assembly fallback options] (wrap-handler assembly fallback options nil))
  ([assembly fallback options bind-publication]
  (when-not (and (map? options) (= #{:authorize-request?} (set (keys options)))
                 (fn? (:authorize-request? options)))
    (reject! :invalid-command-options {}))
  (when-not (and (fn? fallback) (application/application-assembly? assembly))
    (reject! :invalid-command-assembly {}))
  (let [execution (get-in assembly [:operation-acquisition-assembly :execution-assembly])
        prepared (:server execution)
        routes (selected-routes! assembly prepared (get-in execution [:route-assembly :routes]))
        authorize? (:authorize-request? options)]
    (fn [request]
      (if-let [route (route/matching-route routes (:uri request))]
        (cond
          (not= :post (:request-method request)) (response 405)
          (not (application/application-assembly? assembly))
          (reject! :stale-command-assembly {})
          :else
          (if-let [{:keys [form command]} (try (read-form request) (catch Exception _ nil))]
            (let [captured (route/captured-arguments route (:uri request))]
              (if-not (and (some? captured)
                           (every? (fn [[k v]] (= v (get (:arguments command) k))) captured)
                           (try (choreo/require-operation (:operation route) command)
                                (catch Exception _ false)))
                (response 400)
                (let [context (-> request
                                  (dissoc :params :query-params :route-params :path-params
                                          publication/preparation-key)
                                  (assoc :form-params form :path-params captured))]
                  (if-not (true? (authorize? context))
                    (response 403)
                    (let [context (cond-> context
                                    bind-publication
                                    (assoc publication/preparation-key (bind-publication command)))
                          send (server/run-command prepared context command)
                          result (http/html-response (ui/optimistic-settlement-marker (:settlement send)))]
                      ;; Serialize before recording handoff. No exception from
                      ;; execution or serialization becomes a fabricated rejection.
                      (server/complete-settlement-send send)
                      (update result :headers assoc "cache-control" "no-store"
                              "HX-Reswap" "none"))))))
            (response 400)))
        (fallback (dissoc request publication/preparation-key)))))))

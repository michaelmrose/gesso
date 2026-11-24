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
            [gesso.live.ui :as ui])
  (:import [java.io ByteArrayOutputStream InputStream PushbackReader StringReader]
           [java.nio ByteBuffer CharBuffer]
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

(defn- run-command-or-deny [prepared context command bind-publication]
  (try
    (if bind-publication
      (server/run-command
       prepared context command
       (fn [boundary]
         (assoc (:ctx boundary) publication/preparation-key
                (bind-publication boundary))))
      (server/run-command prepared context command))
    (catch clojure.lang.ExceptionInfo failure
      (let [{:keys [error/type error/kind error/details]} (ex-data failure)]
        ;; Both denial classes have the same public response. Neither is a
        ;; semantic settlement. Unresolved and arbitrary execution failures
        ;; must escape, since a transaction may already have committed.
        (if (and (= :gesso.live.optimistic.resolved-operation/invalid-adapter type)
                 (= :resolution-not-settleable kind)
                 (#{:unavailable :incompatible} (:status details)))
          nil
          (throw failure))))))

(defn- utf8-text [^ByteBuffer bytes]
  ;; A new decoder reports malformed/unmappable input by default. Neither
  ;; raw request bytes nor percent escapes may introduce replacement text.
  (str (.decode (.newDecoder StandardCharsets/UTF_8) bytes)))

(defn- hex-digit [n]
  (cond
    (<= 48 n 57) (- n 48)
    (<= 65 n 70) (- n 55)
    (<= 97 n 102) (- n 87)
    :else (throw (IllegalArgumentException. "Malformed form escape"))))

(defn- decode-field [^String text]
  ;; The raw body was already decoded strictly. Re-encode this field, decode
  ;; form escapes exactly once, then validate the resulting UTF-8 bytes.
  (let [bytes (.getBytes text StandardCharsets/UTF_8)
        length (alength bytes)
        out (ByteArrayOutputStream. length)]
    (loop [i 0]
      (if (= i length)
        (utf8-text (ByteBuffer/wrap (.toByteArray out)))
        (let [n (aget bytes i)]
          (case n
            37 (do
                 (when (>= (+ i 2) length)
                   (throw (IllegalArgumentException. "Incomplete form escape")))
                 (.write out (int (+ (* 16 (hex-digit (aget bytes (inc i))))
                                     (hex-digit (aget bytes (+ i 2))))))
                 (recur (+ i 3)))
            43 (do (.write out 32) (recur (inc i)))
            (do (.write out (int n)) (recur (inc i)))))))))

(defn- decode-form [text]
  ;; Reject ambiguous/malformed fields rather than silently dropping them.
  ;; The selected command transport uses single-valued form fields only.
  (reduce (fn [form field]
            (let [[k v] (str/split field #"=" 2)
                  key (decode-field k)
                  value (when v (decode-field v))]
              (if (and (string? key) (not (str/blank? key)) (string? value)
                       (not (contains? form key)))
                (assoc form key value)
                (reduced nil))))
          {} (str/split text #"&" -1)))

(defn- read-body [body]
  ;; A physical read failure is not evidence of malformed client data.
  ;; Let the owning transport handle it, preserving cancellation semantics.
  (if (instance? InputStream body)
    (try
      (ByteBuffer/wrap (.readNBytes ^InputStream body (inc max-form-bytes)))
      (catch InterruptedException cause
        (.interrupt (Thread/currentThread))
        (throw cause)))
    body))

(defn- read-single-wire [encoded]
  ;; An EDN value must never be able to impersonate the end of input.
  ;; Limit the overflow recovery to the untrusted recursive EDN reader;
  ;; neither command validation nor application execution is covered by it.
  (let [eof (Object.)]
    (try
      (with-open [reader (PushbackReader. (StringReader. encoded))]
        (let [wire (edn/read {:eof eof} reader)]
          (when (and (not (identical? eof wire))
                     (identical? eof (edn/read {:eof eof} reader)))
            wire)))
      (catch StackOverflowError _ nil))))

(defn- parse-body [body]
  (try
    (let [^ByteBuffer bytes (cond
                              (and (string? body) (<= (count body) max-form-bytes))
                              (.encode (.newEncoder StandardCharsets/UTF_8) (CharBuffer/wrap ^String body))
                              (instance? ByteBuffer body) body)]
      (when (and bytes (<= (.remaining bytes) max-form-bytes))
        (let [form (decode-form (utf8-text bytes))
              encoded (get form command-parameter)]
          (when (string? encoded)
            (when-let [wire (read-single-wire encoded)]
              {:form form :command (server/decode-command wire)})))))
    (catch InterruptedException cause
      (.interrupt (Thread/currentThread))
      (throw cause))
    (catch Exception _ nil)))

(defn- utf8-form-request? [request]
  ;; This is a selected raw UTF-8 transport, not a generic MIME decoder.
  ;; Do not silently ignore contradictory or ambiguous representation headers.
  (let [headers (:headers request)
        content-type (get headers "content-type")
        encoding (get headers "content-encoding")]
    (and (string? content-type)
         (re-matches #"(?i)[ \t]*application/x-www-form-urlencoded[ \t]*(?:;[ \t]*charset[ \t]*=[ \t]*(?:utf-8|\"utf-8\")[ \t]*)?"
                     content-type)
         (or (not (contains? headers "content-encoding"))
             (and (string? encoding)
                  (re-matches #"(?i)[ \t]*identity[ \t]*" encoding))))))

(defn- read-form [request]
  (let [body (:body request)]
    (when (and (utf8-form-request? request)
               (not (seq (:query-string request))))
      ;; Only the parser classifies malformed input. Physical reads happen
      ;; outside its exception boundary, before admission or authorization.
      (when (or (string? body) (instance? InputStream body))
        (parse-body (read-body body))))))

(defn wrap-handler
  "Own all methods on every admitted HTMX POST operation path.
   Simple :parameter segments bind exact same-named string command arguments.
   No application-specific coercion or alternative parameter mapping.
   Require an explicit trusted request gate (including CSRF/authentication
   policy as appropriate). Parse only the bounded raw UTF-8 form body, not
   caller-supplied Ring params. Admit only absent or UTF-8 charset and
   absent or identity content encoding. Execute the admitted operation only when the
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
          (if-let [{:keys [form command]} (read-form request)]
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
                    (if-let [send (run-command-or-deny prepared context command bind-publication)]
                      (let [result (http/html-response (ui/optimistic-settlement-marker (:settlement send)))]
                        ;; Serialize before recording handoff. No exception from
                        ;; execution or serialization becomes a fabricated rejection.
                        (server/complete-settlement-send send)
                        (update result :headers assoc "cache-control" "no-store"
                                "HX-Reswap" "none"))
                      (response 403))))))
            (response 400)))
        (fallback (dissoc request publication/preparation-key)))))))

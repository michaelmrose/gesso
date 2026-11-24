(ns gesso.live.outbox.http-operation-test
  (:require [clojure.edn :as edn]
            [clojure.xml :as xml]
            [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.optimistic.protocol :as protocol]
            [ring.util.codec :as codec]
            [xtdb.api :as xt])
  (:import [java.io ByteArrayInputStream IOException InputStream InterruptedIOException]
           [java.nio.charset StandardCharsets]
           [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- with-routes [execute! specs rendered-path f]
  (let [ops (assoc (fixture/standard-operations) :request/claim
                   (fixture/trusted-operation :request/claim
                    {:published-change-topics #{:request} :execute! execute!}))]
    (fixture/with-surfaced-application
     {:operations ops :route-capabilities (if specs
                                           (fixture/custom-route-capabilities specs)
                                           (fixture/route-capabilities ops))
      :rendered-surfaces {:board [:main (fixture/rendered-operation-button
                                        (fixture/render-context ops) :request/claim rendered-path)]}}
     (fn [{:keys [assembly report]}]
       (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
       (f assembly)))))

(defn- with-assembly [execute! f]
  (with-routes execute! nil "/operations/request/claim" f))

(def ^:private parameter-routes
  {:request/claim {:operation :request/claim :path "/operations/request/:request-id/claim"}
   :request/cancel {:operation :request/cancel :path "/operations/request/cancel"}
   :request/reassign {:operation :request/reassign :path "/operations/request/reassign"}})

(defn- command [operation]
  (protocol/command {:command-id (identity/command-id "v2362-command")
                     :execution-id (identity/execution-id "v2362-execution")
                     :operation operation :arguments {:request-id "r1"}
                     :observed-basis fixture/rendered-basis}))

(defn- form [operation token]
  (codec/form-encode {transport/command-parameter (pr-str (protocol/command->wire (command operation)))
                      "csrf" token}))

(defn- request [body]
  {:request-method :post :uri "/operations/request/claim"
   :headers {"content-type" "application/x-www-form-urlencoded"} :body body})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest assembled-transport-rejects-bypasses-before-authority
  (let [calls (atom []) gate (atom []) fallback (atom 0)
        boom (ex-info "unknown executor result" {})]
    (with-assembly (fn [ctx] (swap! calls conj ctx) (throw boom))
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly
                                             (fn [_] (swap! fallback inc) {:status 200})
                                             {:authorize-request? (fn [r]
                                                                    (swap! gate conj r)
                                                                    (= "valid" (get-in r [:form-params "csrf"])))})
              valid (form :request/claim "valid")]
          (is (= :invalid-command-options (reason #(transport/wrap-handler assembly identity {}))))
          (doseq [r [(request "") (request (form :request/cancel "valid"))
                     (request (codec/form-encode {transport/command-parameter
                                                  (pr-str (dissoc (protocol/command->wire (command :request/claim)) :observed-basis))}))
                     (request (str valid "&" transport/command-parameter "=%ZZ"))
                     (request (str valid "&csrf=valid"))
                     (request (codec/form-encode {transport/command-parameter
                                                  (str (pr-str (protocol/command->wire (command :request/claim))) " nil")}))
                     (request (apply str (repeat 65537 "x")))
                     (assoc (request valid) :query-string "csrf=valid")
                     (assoc (request "") :params {transport/command-parameter valid})]]
            (is (= 400 (:status (handler r)))))
          (is (= 405 (:status (handler (assoc (request valid) :request-method :get)))))
          (is (empty? @gate))
          (is (= 403 (:status (handler (request (form :request/claim "wrong"))))))
          (is (empty? @calls))
          (is (zero? @fallback))
          (is (identical? boom (try (handler (request valid)) nil (catch Exception e e))))
          (is (= 1 (count @calls)))
          (is (= fixture/trusted-principal (:principal (first @calls))))
          (is (= 200 (:status (handler {:uri "/other"}))))
          (is (= 1 @fallback)))))))

(deftest assembled-operation-over-owned-http-commits-and-returns-correlated-settlement
  (let [intent-id (random-uuid) calls (atom []) gates (atom 0)
        result* (atom nil)]
    (with-routes
      (fn [{:keys [ctx arguments principal] :as invocation}]
        (swap! calls conj invocation)
        (let [node (:xtdb/connectable ctx)
              id (:request-id arguments)
              plan (outbox/prepare! {:intent-ids [intent-id]
                                     :source-id (get ctx managed/source-id-key)
                                     :operation :request/claim :purpose :gesso.live/refresh}
                                    [{:topic :request :id id :change/kind :updated}])
              tx (xt/execute-tx node (into (vec (:guard-ops plan))
                                           [[:put-docs :requests {:xt/id id :status :claimed}]
                                            (:tx-op plan)]))
              result {:resolution :confirmed :outcome :request/claimed
                      :authoritative (protocol/authoritative
                                      {:presence :present
                                       :basis {:tx-id (:tx-id tx) :system-time (str (:system-time tx))}
                                       :projection {:request/id id :request/status :claimed}})}]
          (reset! result* result)
          result))
      parameter-routes "/operations/request/r1/claim"
      (fn [assembly]
        (let [started (host/start! assembly (fn [_] (g/html-response [:div "read"]))
                                   {:xtdb-config {} :source-id "v2362-operation-host"
                                    :rules-context {} :worker-options {:worker-id "v2362" :poll-ms 20}
                                    :live-options {:rules [{:when-topic :request
                                                           :expand (fn [_ change] [(select-keys change [:topic :id])])}]}
                                    :http-jdk {} :assembled-operations
                                    {:authorize-request? (fn [ctx]
                                                           (swap! gates inc)
                                                           (= "valid" (get-in ctx [:form-params "csrf"])))}})
              listener (get started host/listener-key)
              node (:xtdb/connectable started)
              client (HttpClient/newHttpClient)
              send-at! (fn [path body extra-headers]
                         (let [builder (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))
                                           (.timeout (Duration/ofSeconds 10))
                                           (.header "content-type" "application/x-www-form-urlencoded")
                                           (.POST (if (bytes? body)
                                                    (HttpRequest$BodyPublishers/ofByteArray body)
                                                    (HttpRequest$BodyPublishers/ofString body))))]
                           (doseq [[k v] extra-headers] (.header builder k v))
                           (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))))
              path "/operations/request/r1/claim"
              send! #(send-at! path % [])]
          (try
            (let [valid (form :request/claim "valid")]
              (doseq [bad-path ["/operations/request/%721/claim"
                                "/operations%2Frequest/r1/claim"]]
                (is (= 400 (.statusCode (send-at! bad-path valid [])))))
              (is (= 400 (.statusCode (send-at! path valid [["Content-Type" "text/plain"]]))))
              (is (= 400 (.statusCode (send-at! path valid [["Authorization" "one"]
                                                           ["authorization" "two"]])))))
            ;; Exercise the hardened parser through the *installed* listener
            ;; and host, not just a synthetic Ring request. These requests
            ;; must neither reach the application gate nor write anything.
            (let [valid (form :request/claim "valid")
                  wire (pr-str (protocol/command->wire (command :request/claim)))
                  malformed-wire (codec/form-encode
                                  {transport/command-parameter
                                   (str wire " :gesso.live.outbox.http-operation/eof nil")
                                   "csrf" "valid"})
                  invalid-bytes (byte-array (concat (.getBytes (str valid "&extra=")
                                                               StandardCharsets/UTF_8)
                                                    [(unchecked-byte 0xff)]))]
              (doseq [body [(str valid "&extra=%FF")
                            invalid-bytes
                            (str valid "&%63srf=valid")
                            malformed-wire]]
                (is (= 400 (.statusCode (send! body)))))
              (is (= 400 (.statusCode (send-at! path valid [["Content-Encoding" "gzip"]]))))
              (let [bad-method (-> (HttpRequest/newBuilder (URI/create (str (:base-url listener) path)))
                                   (.timeout (Duration/ofSeconds 10))
                                   (.header "content-type" "application/x-www-form-urlencoded")
                                   (.method "post" (HttpRequest$BodyPublishers/ofString valid))
                                   (.build))]
                (is (= 400 (.statusCode (.send client bad-method (HttpResponse$BodyHandlers/ofString)))))))
            (is (zero? @gates))
            (is (empty? @calls))
            (is (empty? (xt/q node ["SELECT _id FROM requests"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
            (is (= 403 (.statusCode (send! (form :request/claim "wrong")))))
            (is (= 400 (.statusCode (send! (form :request/reassign "valid")))))
            (is (empty? @calls))
            (is (empty? (xt/q node ["SELECT _id FROM requests"])))
            (is (empty? (xt/q node ["SELECT _id FROM gesso_live_outbox"])))
            (let [response (send! (form :request/claim "valid"))
                  settlement (protocol/settlement (merge @result* (select-keys (command :request/claim)
                                                                            [:command-id :execution-id])))]
              (is (= 200 (.statusCode response)))
              (is (= "none" (.orElse (.firstValue (.headers response) "HX-Reswap") "")))
              (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (.body response) StandardCharsets/UTF_8)))]
                (is (= :template (:tag marker)))
                (is (= settlement (protocol/wire->settlement
                                   (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))))))
            (is (= [{:status :claimed}] (xt/q node ["SELECT status FROM requests WHERE _id = ?" "r1"])))
            (is (= [{:xt/id intent-id}] (xt/q node ["SELECT _id FROM gesso_live_outbox WHERE _id = ?" intent-id])))
            (is (= 1 (count @calls)))
            (is (= 2 @gates))
            (is (identical? node (get-in (first @calls) [:ctx :xtdb/connectable])))
            (is (= fixture/trusted-principal (:principal (first @calls))))
            (is (= {:request-id "r1"} (get-in (first @calls) [:ctx :path-params])))
            (finally (host/stop! started) (.close client))))))))

(deftest parameter-route-binds-command-arguments-before-authority
  (let [calls (atom []) gates (atom []) fallbacks (atom [])]
    (with-routes
      (fn [invocation]
        (swap! calls conj invocation)
        {:resolution :rejected :reason :test-only})
      parameter-routes "/operations/request/r1/claim"
      (fn [assembly]
        (let [handler (transport/wrap-handler
                       assembly (fn [r] (swap! fallbacks conj r) {:status 404})
                       {:authorize-request? (fn [r] (swap! gates conj r) true)})
              valid (assoc (request (form :request/claim "valid"))
                           :uri "/operations/request/r1/claim")]
          (doseq [uri ["/operations/request/r2/claim"
                       "/operations/request/../claim"
                       "/operations/request/has%20space/claim"
                       "/operations/request/r1/claim?x=1"
                       "/operations/request/r1/claim#fragment"]]
            (is (= 400 (:status (handler (assoc valid :uri uri))))))
          (doseq [arguments [{} {:request-id :r1} {:request-id 1}]]
            (let [wire (protocol/command->wire (assoc (command :request/claim)
                                                     :arguments arguments))]
              (is (= 400 (:status (handler (assoc valid :body
                                                  (codec/form-encode {transport/command-parameter (pr-str wire)}))))))))
          (is (= 405 (:status (handler (assoc valid :request-method :get)))))
          (is (empty? @calls))
          (is (empty? @gates))
          (is (empty? @fallbacks))
          (is (= 200 (:status (handler (assoc valid :path-params {:request-id "forged"}
                                                   :route-params {:request-id "forged"}
                                                   :params {:request-id "forged"})))))
          (is (= 1 (count @calls) (count @gates)))
          (is (= {:request-id "r1"} (:path-params (first @gates))))
          (is (= {:request-id "r1"} (get-in (first @calls) [:ctx :path-params])))
          (is (not-any? #(contains? (first @gates) %) [:params :route-params]))
          (is (= 404 (:status (handler (assoc valid :uri "/not-owned")))))
          (is (= 1 (count @fallbacks))))))))

(deftest parameter-route-rejects-overlapping-and-unsupported-templates
  (doseq [[specs rendered-path expected]
          [[(assoc-in parameter-routes [:request/cancel :path] "/operations/request/r2/claim")
            "/operations/request/r1/claim" :ambiguous-command-route]
           [(assoc-in parameter-routes [:request/cancel :path] "/operations/:kind/r2/claim")
            "/operations/request/r1/claim" :ambiguous-command-route]
           [(assoc-in parameter-routes [:request/claim :path] "/fragments/:request-id")
            "/fragments/r1" :ambiguous-command-route]
           [(assoc-in parameter-routes [:request/claim :path] "/operations/:request-id/:request-id/claim")
            "/operations/r1/r1/claim" :unsupported-command-route]]]
    (with-routes
      (fn [_] (throw (ex-info "unexpected execution" {}))) specs rendered-path
      (fn [assembly]
        (is (= expected (reason #(transport/wrap-handler assembly identity
                                                        {:authorize-request? (constantly true)}))))))))

(deftest command-form-rejects-invalid-utf8-before-authority
  (let [gates (atom []) calls (atom []) fallbacks (atom [])]
    (with-assembly (fn [ctx] (swap! calls conj ctx) {:resolution :rejected :reason :test-only})
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly
                                             (fn [ctx] (swap! fallbacks conj ctx) {:status 404})
                                             {:authorize-request? (fn [ctx] (swap! gates conj (:form-params ctx)) false)})
              valid (form :request/claim "valid")
              prefix (.getBytes (str valid "&extra=") StandardCharsets/UTF_8)]
          (doseq [bad ["%FF" "%C0%AF" "%ED%A0%80" "%E2%82" "%" "%GG" "%1"]]
            (is (= 400 (:status (handler (request (str valid "&extra=" bad))))) bad))
          (is (= 400 (:status (handler (request (str valid "&%FF=x"))))))
          (is (= 400 (:status (handler (request (ByteArrayInputStream.
                                                  (byte-array (concat prefix [(unchecked-byte 0xff)]))))))))
          (is (= 400 (:status (handler (request (str valid "&%63srf=valid"))))))
          (is (= 400 (:status (handler (request (str valid "&extra=" (char 0xd800)))))))
          (is (empty? @gates))
          (is (empty? @calls))
          (is (empty? @fallbacks))
          ;; Valid Unicode, literal percent signs, and plus/space retain their
          ;; exact once-decoded meaning at the original request gate.
          (is (= 403 (:status (handler (request (str valid "&extra=%E9%9B%AA+%25FF%2B"))))))
          (is (= "雪 %FF+" (get (peek @gates) "extra")))
          (is (= 403 (:status (handler (request (ByteArrayInputStream.
                                                  (.getBytes (str valid "&extra=雪") StandardCharsets/UTF_8)))))))
          (is (= "雪" (get (peek @gates) "extra")))
          (let [escaped (apply str (repeat 16000 "%41"))]
            (is (= 403 (:status (handler (request (str valid "&extra=" escaped))))))
            (is (= (apply str (repeat 16000 "A")) (get (peek @gates) "extra"))))
          (is (= 3 (count @gates)))
          (is (empty? @calls)))))))

(deftest command-body-read-failure-is-not-a-malformed-request
  (let [gates (atom []) calls (atom []) fallbacks (atom [])]
    (with-assembly (fn [ctx] (swap! calls conj ctx) {:resolution :rejected :reason :test-only})
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly
                                             (fn [ctx] (swap! fallbacks conj ctx) {:status 404})
                                             {:authorize-request? (fn [ctx] (swap! gates conj ctx) false)})
              interrupted (InterruptedException. "cancelled body read")]
          (doseq [failure [(IOException. "connection lost")
                           (InterruptedIOException. "read timed out")
                           (IllegalStateException. "body adapter failed")
                           interrupted]]
            (let [body (proxy [InputStream] []
                         (read
                           ([] (throw failure))
                           ([_] (throw failure))
                           ([_ _ _] (throw failure))))]
              (try
                (let [[caught interrupted?]
                      (try (handler (request body)) nil
                           (catch Exception e
                             [e (.isInterrupted (Thread/currentThread))]))]
                  ;; Capture the flag before reporting assertions: the test
                  ;; reporter's synchronization may consume interruption.
                  (is (identical? failure caught))
                  (is (= (identical? failure interrupted) interrupted?)))
                (finally (Thread/interrupted)))))
          (is (empty? @gates))
          (is (empty? @calls))
          (is (empty? @fallbacks))
          ;; Ordinary parse failures still get a client rejection; a later
          ;; well-formed request can reach the original gate normally.
          (is (= 400 (:status (handler (request (ByteArrayInputStream.
                                                  (byte-array [(unchecked-byte 0xff)])))))))
          (is (= 403 (:status (handler (request (form :request/claim "valid"))))))
          (is (= 1 (count @gates)))
          (is (empty? @calls)))))))

(deftest command-envelope-requires-unambiguous-end-of-input
  (let [gates (atom []) calls (atom []) fallbacks (atom [])]
    (with-assembly (fn [ctx] (swap! calls conj ctx) {:resolution :rejected :reason :test-only})
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly
                                             (fn [ctx] (swap! fallbacks conj ctx) {:status 404})
                                             {:authorize-request? (fn [ctx] (swap! gates conj ctx) false)})
              wire (pr-str (protocol/command->wire (command :request/claim)))
              body (fn [encoded]
                     (codec/form-encode {transport/command-parameter encoded "csrf" "valid"}))
              sentinel ":gesso.live.outbox.http-operation/eof"
              deep (str (apply str (repeat 9000 "[")) "nil"
                        (apply str (repeat 9000 "]")))]
          (doseq [suffix [(str " " sentinel)
                          (str " " sentinel " nil")
                          (str " " sentinel " [")
                          " nil" " {}"]]
            (is (= 400 (:status (handler (request (body (str wire suffix)))))) suffix))
          ;; A body below the byte limit can still exceed the EDN reader's
          ;; recursive stack capacity. Neither first nor trailing values may
          ;; escape the malformed-input boundary in that case.
          (doseq [encoded [deep (str wire " " deep)]]
            (let [form-body (body encoded)]
              (is (< (count form-body) 65536))
              (is (= 400 (try (:status (handler (request form-body)))
                              (catch StackOverflowError _ :reader-overflow))))))
          (is (empty? @gates))
          (is (empty? @calls))
          (is (empty? @fallbacks))
          (is (= 403 (:status (handler (request (body (str wire " \n; trailing comment\n")))))))
          (is (= 1 (count @gates)))
          (is (empty? @calls)))))))

(deftest command-form-requires-the-declared-utf8-transport
  (let [gates (atom []) calls (atom [])]
    (with-assembly (fn [ctx] (swap! calls conj ctx) {:resolution :rejected :reason :test-only})
      (fn [assembly]
        (let [handler (transport/wrap-handler assembly (constantly {:status 404})
                                             {:authorize-request? (fn [ctx] (swap! gates conj ctx) false)})
              valid (form :request/claim "valid")
              media "application/x-www-form-urlencoded"
              unread (proxy [InputStream] []
                       (read ([] (throw (AssertionError. "Unsupported body read")))
                             ([_] (throw (AssertionError. "Unsupported body read")))
                             ([_ _ _] (throw (AssertionError. "Unsupported body read")))))]
          (doseq [headers [{"content-type" (str media "; charset=ISO-8859-1")}
                           {"content-type" (str media "; charset=UTF-16")}
                           {"content-type" (str media "; charset=UTF-8; charset=UTF-8")}
                           {"content-type" (str media "; charset=UTF-8, text/plain")}
                           {"content-type" (str media "; charset=")}
                           {"content-type" (str media ";")}
                           {"content-type" (str media "; unknown=value")}
                           {"content-type" media "content-encoding" "gzip"}
                           {"content-type" media "content-encoding" "identity, gzip"}
                           {"content-type" media "content-encoding" ""}]]
            (is (= 400 (:status (handler (assoc (request valid) :headers headers)))) headers))
          (is (empty? @gates))
          (is (= 400 (:status (handler (assoc (request unread) :headers
                                             {"content-type" media "content-encoding" "gzip"})))))
          (doseq [headers [{"content-type" media}
                           {"content-type" (str media "; charset=UTF-8")}
                           {"content-type" "Application/X-WWW-Form-Urlencoded ; CHARSET = \"utf-8\""}
                           {"content-type" media "content-encoding" "Identity"}]]
            (is (= 403 (:status (handler (assoc (request valid) :headers headers)))) headers))
          (is (= 4 (count @gates)))
          (is (empty? @calls)))))))

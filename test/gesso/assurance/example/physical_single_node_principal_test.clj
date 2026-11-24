(ns gesso.assurance.example.physical-single-node-principal-test
  "v1508: Permanent adversarial coverage for the *uncertified* v1507
   request-principal wrapper around a real single-node Biff/XTDB fixture.

   This tests source and route closure, denial before model transaction,
   host callback shape and invocation count, unsupported Ring requests,
   changed grants, source substitution, and bypasses that remain possible.

   IMPORTANT: An application-supplied callback is not authenticated merely
   because its answer has a valid shape. The guarded wrapper is now the
   original Biff-installed handler; the unguarded delegate is still exposed
   to these diagnostic callbacks and could be deliberately called directly.
   Neither this fixture nor these tests establish trusted-session verification,
   CSRF enforcement, external listener mediation, certified executors, or
   physical refinement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-principal :as subject]
            [gesso.assurance.example.physical-single-node-http :as http]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.model.tx :as tx])
  (:import (com.sun.net.httpserver HttpHandler HttpServer)
           (java.net HttpURLConnection InetSocketAddress URL)
           (java.nio.charset StandardCharsets)))

(def ^:private view "/__gesso_proving/slot")
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")
(def ^:private base-grants
  {:actor/reserver #{:slot/reserve}
   :actor/releaser #{:slot/release}})

(defn- request [method uri]
  {:request-method method :uri uri})

(defn- policy
  ([acquire!] (policy base-grants acquire!))
  ([grants acquire!]
   {:catalog-id :slot/reservation-lifecycle
    :policy-id :proving/limited-principal-grants
    :grants grants
    :read-principals #{:actor/reserver :actor/releaser}
    :acquire-principal! acquire!}))

(defn- authenticated [id]
  {:status :authenticated :principal-id id})

(defn- reject-reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (when (:physical-single-node-principal/rejection (ex-data e))
        (:reason (ex-data e))))))

(defn- exception-reason [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:reason (ex-data e)))))

(defn- unchanged! [read! before]
  (is (= before (read!)) "Denied or malformed requests cannot change XTDB"))

(defn- assert-conditional-report! [report]
  (is (= subject/report-type (:type report)))
  (is (= subject/report-version (:version report)))
  (is (= :slot/reservation-lifecycle (:catalog-id report)))
  (is (= :proving/limited-principal-grants (:policy-id report)))
  (is (= 2 (:declared-operation-count report)))
  (is (= 2 (:declared-principal-count report)))
  (is (= :checked-against-catalog-and-routes (:source-grant-coverage report)))
  (is (= :checked-but-not-enforced-here (:read-principal-declaration report)))
  (is (= :rendered-state-concealed (:write-only-success-response report)))
  (is (= :before-delegated-handler (:request-time-grant-enforcement report)))
  (is (= :one-per-known-mutating-request (:callback-invocations report)))
  (is (= :at-most-once (:handler-delegation report)))
  (is (= :trusted-not-authenticated (:host-acquisition-callback report)))
  (is (= :original-biff-start-selected-wrapper
         (:biff-installed-principal-handler report)))
  (is (= :not-established (:physical-network-listener report)))
  (is (= :not-established (:abstract-policy-equivalence report)))
  (is (= :not-authenticated (:physical-commit-provenance report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (every? (:open-obligations report)
              [:host-principal-acquisition-authenticity-not-established
               :host-session-and-credential-verification-not-established
               :principal-callback-purity-not-established
               :csrf-defense-not-installed
               :principal-identities-not-persisted-in-physical-domain
               :abstract-guard-authorization-equivalence-not-established
               :physical-listener-and-principal-provenance-not-established
               :certified-plan-executor-not-issued
               :complete-writer-coverage-not-established
               :physical-refinement-not-established])))

(defn- with-loopback-biff-selected-handler!
  "Use an actual JDK socket to exercise ONLY Biff's installed Ring handler.
   This is a test transport, not a Biff-owned or authenticated deployment."
  [installed f]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (try
      (.createContext server "/"
                      (reify HttpHandler
                        (handle [_ exchange]
                          (try
                            (let [method (keyword (.toLowerCase
                                                   (.getRequestMethod exchange)))
                                  uri (.getPath (.getRequestURI exchange))
                                  response (installed {:request-method method
                                                       :uri uri})
                                  payload (.getBytes ^String (str (:body response))
                                                     StandardCharsets/UTF_8)]
                              (.sendResponseHeaders exchange
                                                    (int (:status response))
                                                    (long (alength payload)))
                              (with-open [output (.getResponseBody exchange)]
                                (.write output payload)))
                            (finally (.close exchange))))))
      (.start server)
      (f (str "http://127.0.0.1:" (.getPort (.getAddress server))))
      (finally
        (.stop server 0)))))

(defn- socket-status! [base method path]
  (let [^HttpURLConnection connection
        (.openConnection (URL. (str base path)))]
    (try
      (.setRequestMethod connection (.toUpperCase (name method)))
      (.setConnectTimeout connection 3000)
      (.setReadTimeout connection 3000)
      (.getResponseCode connection)
      (finally (.disconnect connection)))))

(deftest real-socket-consumes-original-biff-principal-handler
  ;; A real HTTP bridge serves the handler selected by Biff startup.
  ;; The principal callback is intentionally trusted application code;
  ;; this does not certify its credential checks or the bridge as Biff Netty.
  (let [who (atom :actor/releaser)
        calls (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_]
               (swap! calls inc)
               (authenticated @who)))
     (fn [{:keys [system principal-handler delegate-handler read!]}]
       (let [installed (:biff.ring/handler system)]
         (is (identical? principal-handler installed))
         (is (not (identical? delegate-handler installed)))
         (with-loopback-biff-selected-handler!
           installed
           (fn [base]
             (is (= 200 (socket-status! base :get view)))
             (is (= 403 (socket-status! base :post reserve)))
             (is (= 0 (:revision (read!))))
             (reset! who :actor/reserver)
             (is (= 200 (socket-status! base :post reserve)))
             (is (= 1 (:revision (read!))))
             (is (= 403 (socket-status! base :post release)))
             (is (= 1 (:revision (read!))))
             (reset! who :actor/releaser)
             (is (= 200 (socket-status! base :post release)))
             (is (= 2 (:revision (read!))))))
         (is (= 4 @calls)
             "The selected Biff handler rechecks trusted grants per mutation"))))))

(deftest admitted-source-installs-principal-wrapper-in-original-biff-start
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/reserver)))
   (fn [{:keys [handler principal-handler delegate-handler system
                 read! principal-report http-routes http-report]}]
     (assert-conditional-report! principal-report)
     (is (fn? principal-handler))
     (is (identical? handler principal-handler))
     (is (not (identical? handler delegate-handler)))
     (is (identical? handler (:biff.ring/handler system)))
     (is (= http-routes (:routes http-report)))
     (is (= 200 (:status (principal-handler (request :get view)))))
     (is (= 0 (:revision (read!)))))))

(deftest request-time-grants-enforce-reserve-and-release
  (let [current (atom :actor/reserver)
        calls (atom [])]
    (subject/with-principal-proving-application!
     (policy (fn [req]
               (swap! calls conj req)
               (authenticated @current)))
     (fn [{:keys [principal-handler read! abstract!]}]
       (let [before (read!)]
         (is (= 403 (:status (principal-handler (request :post release)))))
         (unchanged! read! before))
       (is (= 200 (:status (principal-handler (request :post reserve)))))
       (is (= :reserved (:slot/status (abstract!))))
       (is (= 1 (:revision (read!))))
       (reset! current :actor/releaser)
       (let [before (read!)]
         (is (= 403 (:status (principal-handler (request :post reserve)))))
         (unchanged! read! before))
       (is (= 200 (:status (principal-handler (request :post release)))))
       (is (= 2 (:revision (read!))))
       (is (= :available (:slot/status (abstract!))))
       (is (= 4 (count @calls)))
       (is (every? #(= :post (:request-method %)) @calls))))))

(deftest denied-requests-never-reach-model-transaction
  (let [answer (atom {:status :anonymous})
        acquire-count (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! acquire-count inc) @answer))
     (fn [{:keys [principal-handler read!]}]
       (let [original @#'tx/transact!
             submissions (atom 0)
             before (read!)]
         (with-redefs [tx/transact! (fn [ctx plan]
                                     (swap! submissions inc)
                                     (original ctx plan))]
           (is (= 401 (:status (principal-handler (request :post reserve)))))
           (reset! answer (authenticated :actor/unknown))
           (is (= 403 (:status (principal-handler (request :post reserve)))))
           (reset! answer (authenticated :actor/releaser))
           (is (= 403 (:status (principal-handler (request :post reserve)))))
           (is (zero? @submissions))
           (reset! answer (authenticated :actor/reserver))
           (is (= 200 (:status (principal-handler (request :post reserve)))))
           (is (= 1 @submissions) "Admitted request submits exactly once"))
         (is (= 4 @acquire-count))
         (is (= 1 (:revision (read!))))
         (is (= 0 (:revision before))))))))

(deftest malformed-acquisition-results-reject-without-physical-writes
  (let [answer (atom nil)
        calls (atom 0)
        invalid [nil [] false "authenticated" :actor/reserver
                 {} {:status :authenticated}
                 {:status :anonymous :principal-id :actor/reserver}
                 {:status :unauthenticated}
                 {:status :authenticated :principal-id nil}
                 {:status :authenticated :principal-id "actor/reserver"}
                 {:status :authenticated :principal-id :unqualified}
                 {:status :authenticated :principal-id :actor/reserver :certified? true}
                 {:principal-id :actor/reserver :status :authenticated :roles #{} }
                 (with-meta (authenticated :actor/reserver) {:trusted true})
                 (sorted-map :status :anonymous)]]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc) @answer))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)]
         (doseq [value invalid]
           (reset! answer value)
           (is (= :malformed-principal-acquisition-result
                  (reject-reason #(principal-handler (request :post reserve))))
               (pr-str value)))
         (is (= (count invalid) @calls))
         (unchanged! read! before))))))

(deftest anonymous-and-unknown-identities-have-distinct-denials
  (let [answer (atom {:status :anonymous})]
    (subject/with-principal-proving-application!
     (policy (fn [_] @answer))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)
             anonymous (principal-handler (request :post reserve))]
         (is (= 401 (:status anonymous)))
         (is (= "Principal required" (:body anonymous)))
         (is (= "no-store" (get-in anonymous [:headers "cache-control"])))
         (reset! answer (authenticated :actor/missing))
         (let [unknown (principal-handler (request :post reserve))]
           (is (= 403 (:status unknown)))
           (is (= "Operation forbidden" (:body unknown)))
           (is (= "no-store" (get-in unknown [:headers "cache-control"]))))
         (unchanged! read! before))))))

(deftest host-callback-exceptions-do-not-retry-or-run-transaction
  (let [calls (atom 0)
        submissions (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc)
               (throw (ex-info "Synthetic host provider failure"
                               {:provider-failure true}))))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)]
         (with-redefs [tx/transact! (fn [& _]
                                     (swap! submissions inc)
                                     (throw (ex-info "Unexpected transaction" {})))]
           (is (= true
                  (try (principal-handler (request :post reserve))
                       nil
                       (catch clojure.lang.ExceptionInfo e
                         (:provider-failure (ex-data e))))))
           (is (= 1 @calls))
           (is (zero? @submissions)))
         (unchanged! read! before))))))

(deftest forged-ring-principal-and-roles-cannot-overrule-host-callback
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/releaser)))
   (fn [{:keys [principal-handler read!]}]
     (let [before (read!)
           forged (assoc (request :post reserve)
                         :principal-id :actor/reserver
                         :user {:id :actor/reserver}
                         :roles #{:actor/reserver}
                         :session {:principal-id :actor/reserver}
                         :headers {"x-principal" "actor/reserver"})]
       (is (= 403 (:status (principal-handler forged))))
       (unchanged! read! before)))))

(deftest callback-trust-is-explicit-not-authentication-proof
  ;; A deliberately unsafe host callback can read an attacker-controlled
  ;; header. The wrapper cannot prove the host's identity acquisition is sound.
  (subject/with-principal-proving-application!
   (policy (fn [req]
             (authenticated
              (keyword "actor" (get-in req [:headers "x-unsafe-actor"] "missing")))))
   (fn [{:keys [principal-handler principal-report read!]}]
     (is (= 200
            (:status (principal-handler
                      (assoc (request :post reserve)
                             :headers {"x-unsafe-actor" "reserver"})))))
     (is (= 1 (:revision (read!))))
     (is (= :trusted-not-authenticated
            (:host-acquisition-callback principal-report)))
     (is (contains? (:open-obligations principal-report)
                    :host-session-and-credential-verification-not-established)))))

(deftest unsupported-methods-routes-and-read-only-requests-skip-auth-provider
  (let [calls (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc)
               (authenticated :actor/reserver)))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)]
         (doseq [[req status] [[(request :get view) 200]
                               [(request :get reserve) 405]
                               [(request :put reserve) 405]
                               [(request :delete release) 405]
                               [(request :post view) 405]
                               [(request :post "/__gesso_proving/missing") 404]
                               [(request :post (str reserve "/")) 404]
                               [(request :post (str reserve "?fake=1")) 404]]]
           (is (= status (:status (principal-handler req))) (pr-str req)))
         (is (zero? @calls))
         (unchanged! read! before))))))

(deftest forged-path-and-route-params-are-rejected-before-authentication
  (let [calls (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc)
               (authenticated :actor/reserver)))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)]
         (doseq [field [:path-params :route-params]
                 value [nil {} {:slot-id "different"}]]
           (is (= 400 (:status
                       (principal-handler (assoc (request :post reserve)
                                                 field value))))))
         (is (zero? @calls))
         (unchanged! read! before))))))

(deftest malformed-ring-requests-are-not-considered-authenticated
  (let [calls (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc)
               (authenticated :actor/reserver)))
     (fn [{:keys [principal-handler read!]}]
       (let [before (read!)]
         (is (= :invalid-ring-request
                (try (principal-handler nil)
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (:reason (ex-data e))))))
         (doseq [req [{:request-method nil :uri reserve}
                      {:request-method "POST" :uri reserve}
                      {:request-method :post :uri 100}]]
           (is (= 400 (:status (principal-handler req)))))
         (is (zero? @calls))
         (unchanged! read! before))))))

(deftest the-original-biff-handler-enforces-grants-before-xtdb
  ;; The original Biff-installed handler is NOW the protected wrapper.
  ;; The unguarded delegate remains reachable by a deliberately trusted
  ;; test callback, so this does not establish universal writer coverage.
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/releaser)))
   (fn [{:keys [principal-handler handler delegate-handler system
                principal-report read!]}]
     (let [installed (:biff.ring/handler system)
           original @#'tx/transact!
           submissions (atom 0)]
       (is (identical? principal-handler installed))
       (is (identical? handler installed))
       (is (not (identical? delegate-handler installed)))
       (with-redefs [tx/transact! (fn [& args]
                                   (swap! submissions inc)
                                   (apply original args))]
         (is (= 403 (:status (installed (request :post reserve)))))
         (is (zero? @submissions)
             "Denied Biff-installed request must not reach physical writer")
         (is (= 0 (:revision (read!)))))
       (is (= :original-biff-start-selected-wrapper
              (:biff-installed-principal-handler principal-report)))
       (is (= :not-established
              (:physical-network-listener principal-report)))))))

(deftest deliberate-delegate-bypass-remains-an-open-trust-boundary
  ;; Test code holds the unguarded delegate. The selected Biff path cannot
  ;; accidentally bypass the grant check, but this is not a theorem about
  ;; other handlers, model callers, or lower-level XTDB writers.
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/releaser)))
   (fn [{:keys [principal-handler delegate-handler system read!]}]
     (is (= 403 (:status ((:biff.ring/handler system)
                          (request :post reserve)))))
     (is (= 200 (:status (delegate-handler (request :post reserve)))))
     (is (= 1 (:revision (read!))))
     (is (= 403 (:status (principal-handler (request :post reserve))))))))

(deftest authorization-does-not-bypass-the-original-business-guard
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/reserver)))
   (fn [{:keys [principal-handler read!]}]
     (is (= 200 (:status (principal-handler (request :post reserve)))))
     (is (= :operation-precondition-not-met
            (exception-reason #(principal-handler (request :post reserve)))))
     (is (= 1 (:revision (read!))) "No automatic retry or second commit"))))

(deftest mutable-external-principal-decision-is-rechecked-per-request
  (let [who (atom :actor/releaser)
        calls (atom 0)]
    (subject/with-principal-proving-application!
     (policy (fn [_] (swap! calls inc)
               (authenticated @who)))
     (fn [{:keys [principal-handler read!]}]
       (is (= 403 (:status (principal-handler (request :post reserve)))))
       (reset! who :actor/reserver)
       (is (= 200 (:status (principal-handler (request :post reserve)))))
       (is (= 2 @calls))
       (is (= 1 (:revision (read!))))))))

(deftest malformed-policy-shapes-reject-before-xtdb-startup
  (let [base (policy (constantly {:status :anonymous}))
        candidates [(assoc base :extra :certified)
                    (dissoc base :grants)
                    (with-meta base {:claimed-proof true})
                    (assoc base :catalog-id :forged/catalog)
                    (assoc base :policy-id :unqualified)
                    (assoc base :acquire-principal! nil)
                    (assoc base :grants (with-meta base-grants {:certified true}))]
        starts (atom 0)]
    (with-redefs [physical/with-proving-application!
                  (fn [& _] (swap! starts inc)
                    (throw (ex-info "Should reject before DB init" {})))]
      (doseq [candidate candidates]
        (is (contains? #{:invalid-policy-source-shape
                         :policy-not-bound-to-catalog-and-http-operations}
                       (reject-reason
                        #(subject/with-principal-proving-application!
                          candidate identity))))))
    (is (zero? @starts))))

(deftest malformed-independent-read-principals-reject-before-xtdb-startup
  (let [base (policy (constantly {:status :anonymous}))
        invalid [nil #{} [] {:actor/read-only true} :actor/read-only
                 #{:unqualified} #{"actor/reserver"} #{nil}
                 (with-meta #{:actor/reserver} {:certified true})
                 (set (map #(keyword "actor" (str "reader" %))
                           (range 65)))]]
    (doseq [invalid-readers invalid]
      (is (= :invalid-independent-read-principals
             (reject-reason
              #(subject/with-principal-proving-application!
                (assoc base :read-principals invalid-readers)
                (fn [_] (is false "invalid read-policy callback")))))
          (str "Invalid readers: " (pr-str invalid-readers))))))

(deftest malformed-grants-reject-before-xtdb-startup
  (let [f (constantly {:status :anonymous})
        candidates [{}
                    {:actor/reserver #{:slot/reserve}}
                    {:actor/reserver #{:slot/reserve :slot/undeclared}
                     :actor/releaser #{:slot/release}}
                    {:actor/reserver #{}
                     :actor/releaser #{:slot/release}}
                    {:unqualified #{:slot/reserve}
                     :actor/releaser #{:slot/release}}
                    {:actor/reserver [:slot/reserve]
                     :actor/releaser #{:slot/release}}
                    {:actor/reserver (with-meta #{:slot/reserve} {:proved true})
                     :actor/releaser #{:slot/release}}
                    (into {} (map (fn [i] [(keyword "actor" (str "p" i))
                                           (if (= i 64) #{:slot/release}
                                               #{:slot/reserve})])
                                  (range 65)))]
        starts (atom 0)]
    (with-redefs [physical/with-proving-application!
                  (fn [& _] (swap! starts inc)
                    (throw (ex-info "Should reject before DB init" {})))]
      (doseq [grants candidates]
        (is (= :policy-not-bound-to-catalog-and-http-operations
               (reject-reason
                #(subject/with-principal-proving-application!
                  (policy grants f) identity)))
            (pr-str (count grants)))))
    (is (zero? @starts))))

(deftest source-substitution-rejects-before-opening-xtdb
  (let [base (policy (constantly {:status :anonymous}))
        starts (atom 0)
        original-catalog @#'pilot/reservation-catalog]
    (with-redefs [physical/with-proving-application!
                  (fn [& _] (swap! starts inc)
                    (throw (ex-info "DB must not start" {})))
                  pilot/reservation-catalog
                  (fn [] (original-catalog))]
      (is (= :substituted-source-or-http-constructor
             (reject-reason #(subject/with-principal-proving-application!
                              base identity)))))
    (with-redefs [physical/with-proving-application!
                  (fn [& _] (swap! starts inc)
                    (throw (ex-info "DB must not start" {})))
                  http/with-http-proving-application!
                  (fn [& _] (throw (ex-info "Substituted" {})))]
      (is (= :substituted-source-or-http-constructor
             (reject-reason #(subject/with-principal-proving-application!
                              base identity)))))
    (is (zero? @starts))))

(deftest invalid-lifetime-callback-rejected-before-startup
  (let [starts (atom 0)
        base (policy (constantly {:status :anonymous}))]
    (with-redefs [physical/with-proving-application!
                  (fn [& _] (swap! starts inc)
                    (throw (ex-info "DB must not start" {})))]
      (is (= :invalid-test-callback
             (reject-reason #(subject/with-principal-proving-application!
                              base nil)))))
    (is (zero? @starts))))

(deftest recognized-request-is-rejected-by-original-handler-on-forged-route-params
  (subject/with-principal-proving-application!
   (policy (constantly (authenticated :actor/reserver)))
   (fn [{:keys [principal-handler read!]}]
     (let [before (read!)]
       (is (= 400 (:status
                   (principal-handler
                    (assoc (request :post reserve)
                           :route-params {:id "not-admitted"})))))
       (unchanged! read! before)))))

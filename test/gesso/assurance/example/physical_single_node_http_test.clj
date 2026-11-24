(ns gesso.assurance.example.physical-single-node-http-test
  "v1496: permanent, adversarial end-to-end checks of v1494/v1495's REAL
   embedded-XTDB -> Gesso model.tx -> Biff/Ring reservation fixture.

   This is intentionally an *uncertified* proving application. The tests
   establish observed execution, negative HTTP routing, single submission,
   guarded rejection, report candor, and Biff lifecycle behavior. They do
   NOT establish issued certified-plan executors, canonical application
   preflight, physical writer completeness, authenticated authorization,
   network deployment, or universal physical refinement.

   In particular, replacing tx/transact! with a function returning a forged
   :committed map can produce a 200 without writing. That is an explicit
   absence of authenticated commit provenance, NOT a positive guarantee."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-http :as subject]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private view "/__gesso_proving/slot")
(def ^:private reserve "/__gesso_proving/slot/reserve")
(def ^:private release "/__gesso_proving/slot/release")

(defn- request [method path]
  {:request-method method :uri path})

(defn- reason [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (or (:physical-single-node-http/rejection (ex-data e))
                 (:physical-single-node-proving/rejection (ex-data e)))
           (:reason (ex-data e))
           (throw e)))))

(defn- assert-open-obligations! [report]
  (is (= :gesso.assurance.example.physical-single-node-http/local-biff-fixture
         (:type report)))
  (is (= 1 (:version report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (= :not-established (:issued-certified-plan-executors report)))
  (is (= :not-established (:installed-checked-router report)))
  (is (= :not-established (:canonical-application-preflight report)))
  (is (= :not-established (:host-deployment report)))
  (is (= :not-established (:writer-completeness report)))
  (is (= :not-established (:physical-refinement report)))
  (is (= :unqualified-v1494-fields
         (get-in report [:certified-plan-compatibility :persisted-fields])))
  (is (= :not-admitted-by-v1251
         (get-in report [:certified-plan-compatibility :application-extra-assertions])))
  (is (every? (:open-obligations report)
              [:v1494-operations-not-issued-certified-plan-executors
               :gesso-application-assembly-and-canonical-html-preflight-not-installed
               :request-principal-authorization-and-csrf-not-checked
               :http-server-consumption-of-biff-handler-not-authenticated
               :complete-physical-writer-coverage-not-established
               :universal-refinement-not-established])))

(deftest explicitly-unavailable-aleph-adapter-rejects-before-physical-start
  ;; This checks absence, not a simulated successful Aleph server. The real
  ;; server and streaming body are mandatory in the separate integration lane.
  (let [starts (atom 0)
        resolve-original @#'clojure.core/requiring-resolve]
    (with-redefs [clojure.core/requiring-resolve
                  (fn [sym]
                    (if (= 'aleph.http/start-server sym)
                      (throw (java.io.FileNotFoundException. "Aleph unavailable"))
                      (resolve-original sym)))
                  physical/with-proving-application!
                  (fn [& _] (swap! starts inc))]
      (is (= :aleph-test-adapter-unavailable
             (reason #(subject/with-http-proving-application!
                       "missing-aleph" (fn [_ h] h) :biff-aleph-http1
                       (fn [_] (is false "Unexpected application startup"))))))
      (is (zero? @starts)))))

(deftest real-embedded-xtdb-get-reserve-release-and-readback
  (subject/with-http-proving-application!
   (fn [{:keys [handler system read! abstract! http-report report]}]
     (is (identical? handler (:biff.ring/handler system)))
     (assert-open-obligations! http-report)
     (is (= :not-issued (:certificate report)))
     (is (= #{:initializer/bootstrap :operation/reserve :operation/release}
            (:declared-example-writer-paths report)))
     (let [before (read!)
           get-result (handler (request :get view))]
       (is (= 200 (:status get-result)))
       (is (re-find #"available" (:body get-result)))
       (is (= "text/html; charset=utf-8"
              (get-in get-result [:headers "content-type"])))
       (is (= before (read!)) "A GET cannot mutate the slot"))
     (let [result (handler (request :post reserve))]
       (is (= 200 (:status result)))
       (is (re-find #"reserved" (:body result)))
       (is (= 1 (:revision (read!))))
       (is (= :reserved (:slot/status (abstract!))))
       (is (= 1 (:slot/active-claims (abstract!)))))
     (let [result (handler (request :post release))]
       (is (= 200 (:status result)))
       (is (re-find #"available" (:body result)))
       (is (= 2 (:revision (read!))))
       (is (= :available (:slot/status (abstract!))))
       (is (= 0 (:slot/active-claims (abstract!))))
       (is (= "preserved" (:audit-tag (read!))))))))

(deftest postcommit-readback-failure-preserves-reported-commit
  ;; The physical transaction commits before the HTTP handler reads XTDB to
  ;; render its response. A transient SQL/read failure after the commit must
  ;; not masquerade as a failed command and invite another mutation attempt.
  (subject/with-http-proving-application!
   (fn [{:keys [handler system read! http-report]}]
     (is (identical? handler (:biff.ring/handler system)))
     (let [original-tx @#'tx/transact!
           original-q @#'xt/q
           committed? (atom false)
           tx-calls (atom 0)
           failed-reads (atom 0)
           result (with-redefs
                    [tx/transact!
                     (fn [& args]
                       (swap! tx-calls inc)
                       (let [outcome (apply original-tx args)]
                         (when (= :committed (:commit/status outcome))
                           (reset! committed? true))
                         outcome))
                     xt/q
                     (fn [& args]
                       ;; xt/q delegates two-argument calls to a three-arg
                       ;; implementation. Only reject one top-level read
                       ;; after the actual XTDB mutation returned committed.
                       (when (and @committed? (= 2 (count args)))
                         (swap! failed-reads inc)
                         (throw (ex-info "Synthetic postcommit XTDB outage"
                                         {:synthetic :postcommit-readback})))
                       (apply original-q args))]
                    (handler (request :post reserve)))]
       (is @committed?)
       (is (= 1 @tx-calls) "Do not replay a protected submission")
       (is (= 1 @failed-reads) "Exactly one physical readback was attempted")
       (is (= 200 (:status result))
           "A postcommit view failure must not make the write look failed")
       (is (= "unknown" (get-in result [:headers "x-gesso-proving-readback"])))
       (is (= "true" (get-in result [:headers "HX-Refresh"])))
       (is (= "no-store" (get-in result [:headers "cache-control"])))
       (is (not (re-find #"data-slot-status" (:body result)))
           "Never fabricate a rendered authoritative state")
       (is (= :preserve-reported-commit-and-request-refresh
              (:postcommit-render-failure http-report)))
       (is (= 1 (:revision (read!)))
           "The real committed operation still appears exactly once in XTDB")
       (is (= "reserved" (:status (read!))))
       (is (= :operation-precondition-not-met
              (reason #(handler (request :post reserve))))
           "Second reserve is rejected by actual authoritative state")
       (is (= 1 (:revision (read!))))))))

(deftest get-failure-is-never-promoted-to-a-successful-read
  ;; The postcommit fallback must never apply to an ordinary GET, which has
  ;; no definitive reported write result to preserve.
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [original-q @#'xt/q
           queries (atom 0)
           observation (with-redefs
                         [xt/q (fn [& args]
                                 (when (= 2 (count args))
                                   (swap! queries inc)
                                   (throw (ex-info "Synthetic GET read outage"
                                                   {:synthetic :get-read})))
                                 (apply original-q args))]
                         (try
                           (handler (request :get view))
                           :unexpected-success
                           (catch clojure.lang.ExceptionInfo e
                             (:synthetic (ex-data e)))))]
       (is (= :get-read observation))
       (is (= 1 @queries))
       (is (= 0 (:revision (read!))))))))

(deftest rendered-response-uses-one-checked-xtdb-snapshot
  ;; The old renderer independently called :read! and :abstract!, which
  ;; each queried XTDB. A concurrent reservation could land between those
  ;; reads and make a valid GET falsely reject a mixed-revision response.
  ;; Reject any second query rather than trusting a nondeterministic race.
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [original-q @#'xt/q]
       (doseq [[status revision]
               [["available" 0] ["reserved" 1] ["available" 2]]]
         (let [queries (atom 0)
               result (with-redefs [xt/q (fn [& args]
                                          ;; xt/q arity-2 delegates to arity-3.
                                          ;; Count only the external arity-2
                                          ;; query, not that internal call.
                                          (when (= 2 (count args))
                                            (let [count* (swap! queries inc)]
                                              (when (> count* 1)
                                                (throw (ex-info
                                                        "Second physical read during HTTP render"
                                                        {:read-count count*})))))
                                          (apply original-q args))]
                        (handler (request :get view)))]
           (is (= 1 @queries)
               "The response must derive its status and revision from one read")
           (is (= 200 (:status result)))
           (is (re-find (re-pattern (str "data-slot-status=\"" status "\""))
                        (:body result)))
           (is (re-find (re-pattern (str "data-slot-revision=\"" revision "\""))
                        (:body result)))
           (is (= revision (:revision (read!)))
               "Rendering must not modify authoritative XTDB state"))
         (case revision
           0 (is (= 200 (:status (handler (request :post reserve)))))
           1 (is (= 200 (:status (handler (request :post release)))))
           nil))))))

(deftest unsupported-methods-and-unknown-routes-do-not-mutate
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [before (read!)]
       (doseq [[req status] [[(request :post view) 405]
                             [(request :put view) 405]
                             [(request :get reserve) 405]
                             [(request :get release) 405]
                             [(request :delete reserve) 405]
                             [(request :post "/somewhere-else") 404]
                             [(request :post (str reserve "/") ) 404]
                             [(request :post (str reserve "?fake=1")) 404]
                             [(request :get "/") 404]]]
         (let [result (handler req)]
           (is (= status (:status result)) (pr-str req))
           (is (= "no-store" (get-in result [:headers "cache-control"])))))
       (is (= before (read!)) "Invalid routes must not execute an operation")))))

(deftest malformed-ring-input-and-parameter-forgery-fail-closed
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [before (read!)]
       (is (= :invalid-ring-request (reason #(handler nil))))
       (is (= :invalid-ring-request (reason #(handler []))))
       (doseq [req [{:request-method nil :uri view}
                    {:request-method :get :uri nil}
                    {:request-method "POST" :uri reserve}
                    {:request-method :post :uri 42}
                    (assoc (request :post reserve) :path-params {})
                    (assoc (request :post reserve) :route-params {:id "forged"})
                    (assoc (request :get view) :path-params {:id "forged"})]]
         (is (= 400 (:status (handler req))) (pr-str req)))
       (is (= before (read!)))))))

(deftest repeated-reserve-is-rejected-not-reported-as-success
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (is (= 200 (:status (handler (request :post reserve)))))
     (let [before (read!)]
       (is (= :operation-precondition-not-met
              (reason #(handler (request :post reserve)))))
       (is (= before (read!)))
       (is (= 1 (:revision (read!))))))))

(deftest release-before-reserve-and-duplicate-release-are-rejected
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (is (= :operation-precondition-not-met
            (reason #(handler (request :post release)))))
     (is (= 0 (:revision (read!))))
     (is (= 200 (:status (handler (request :post reserve)))))
     (is (= 200 (:status (handler (request :post release)))))
     (is (= :operation-precondition-not-met
            (reason #(handler (request :post release)))))
     (is (= 2 (:revision (read!)))))))

(deftest each-valid-post-submits-one-guarded-gesso-model-plan
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [original @#'tx/transact!
           observed (atom [])]
       (with-redefs [tx/transact!
                     (fn [ctx plan]
                       (swap! observed conj [ctx plan])
                       (original ctx plan))]
         (is (= 200 (:status (handler (request :post reserve))))))
       (is (= 1 (count @observed)) "Exactly one transaction, no replay")
       (let [[ctx plan] (first @observed)]
         (is (some? (:biff.xtdb/node ctx)))
         (is (= 1 (count (:commands plan))))
         (is (= 1 (count (:assertions plan))))
         (is (false? (:emit plan)))
         (let [prepared (tx/prepare ctx plan)
               ops (:tx-ops prepared)]
           (is (<= 2 (count ops)))
           (is (every? #(and (vector? %)
                             (string? (first %))
                             (re-find #"(?i)^ASSERT\b" (first %)))
                       (butlast ops))
               "Assertions must precede physical write")
           (is (= :put-docs (first (last ops))))))
       (is (= 1 (:revision (read!))))))))

(deftest ambiguous-outcomes-and-missing-results-never-become-http-success
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [before (read!)
           calls (atom 0)]
       (doseq [outcome [nil {}
                        {:commit/status :commit-unknown}
                        {:commit/status :rejected}
                        {:commit/status :already-incorporated}
                        {:commit/status :unknown}
                        {:commit/status false}]]
         (reset! calls 0)
         (is (= :nondefinitive-physical-outcome
                (with-redefs [tx/transact! (fn [_ _]
                                             (swap! calls inc)
                                             outcome)]
                  (reason #(handler (request :post reserve)))))
             (pr-str outcome))
         (is (= 1 @calls) "An ambiguous outcome cannot induce a retry")
         (is (= before (read!)) "An injected result did not mutate XTDB"))))))

(deftest execution-exceptions-propagate-without-retrying
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [calls (atom 0)
           before (read!)
           caught (with-redefs [tx/transact!
                                (fn [_ _]
                                  (swap! calls inc)
                                  (throw (ex-info "synthetic failure" {:probe :v1496}))) ]
                    (try
                      (handler (request :post reserve))
                      :unexpected-success
                      (catch clojure.lang.ExceptionInfo e
                        (:probe (ex-data e)))))]
       (is (= :v1496 caught))
       (is (= 1 @calls))
       (is (= before (read!)))))))

(deftest a-forged-committed-return-is-not-an-authenticated-commit
  (subject/with-http-proving-application!
   (fn [{:keys [handler read! http-report]}]
     ;; Explicit negative boundary: v1495 trusts the return map supplied by
     ;; tx/transact!, it does not authenticate a physical commit receipt.
     ;; This must NOT be mistaken for a certified physical correspondence.
     (let [before (read!)
           calls (atom 0)
           result (with-redefs [tx/transact!
                                (fn [_ _]
                                  (swap! calls inc)
                                  {:commit/status :committed})]
                    (handler (request :post reserve)))]
       (is (= 1 @calls))
       (is (= 200 (:status result))
           "Exposes current trust boundary, not a production guarantee")
       (is (= before (read!)) "Fabricated result did not commit")
       (is (= :not-established (:physical-refinement http-report)))
       (is (= :not-issued (:certificate http-report)))))))

(deftest read-only-get-never-invokes-the-model-transaction-submitter
  (subject/with-http-proving-application!
   (fn [{:keys [handler read!]}]
     (let [before (read!)
           calls (atom 0)
           result (with-redefs [tx/transact!
                                (fn [& _]
                                  (swap! calls inc)
                                  (throw (ex-info "unexpected write" {})))]
                    (handler (request :get view)))]
       (is (= 200 (:status result)))
       (is (zero? @calls))
       (is (= before (read!)))))))

(deftest the-biff-module-start-and-init-boundaries-are-single-use
  (subject/with-http-proving-application!
   (fn [{:keys [handler system module http-report]}]
     (is (= subject/module-id (:biff.core/id module)))
     (is (identical? handler (:biff.ring/handler system)))
     (is (= :repeated-module-init
            (reason #((:biff.core/init module) {}))))
     (is (= :repeated-module-start
            (reason #((:biff.core/start module) system))))
     (assert-open-obligations! http-report))))

(deftest a-replaced-handler-is-not-an-accepted-biff-start
  (subject/with-http-proving-application!
   (fn [{:keys [system module]}]
     ;; Biff already invoked the authentic start callback once. Replacing
     ;; the slot cannot be silently rechecked with that consumed callback.
     (is (= :repeated-module-start
            (reason #((:biff.core/start module)
                      (assoc system :biff.ring/handler (fn [_] {:status 200})))))))))

(deftest rejected-test-callback-and-invalid-identifiers-fail-closed
  (is (= :invalid-test-callback
         (reason #(subject/with-http-proving-application! nil))))
  (is (= :unsupported-slot-id
         (reason #(subject/with-http-proving-application! "invalid/id" identity))))
  (is (= :unsupported-slot-id
         (reason #(subject/with-http-proving-application! "" identity)))))

(deftest actual-xtdb-audit-change-invalidates-in-flight-guarded-write
  (subject/with-http-proving-application!
   (fn [{:keys [handler node read!]}]
     (let [original @#'tx/transact!
           calls (atom 0)
           before (read!)
           result (with-redefs [tx/transact!
                                (fn [ctx plan]
                                  (swap! calls inc)
                                  ;; This is an adversarial external writer,
                                  ;; outside Gesso's certified writer coverage.
                                  (xt/execute-tx node
                                                 [[:put-docs physical/table
                                                   (assoc (read!) :audit-tag "changed")]])
                                  (original ctx plan))]
                    (try
                      (handler (request :post reserve))
                      :unexpected-success
                      (catch Throwable e
                        (if (= :xtdb/assert-failed
                               (:xtdb.error/code (ex-data e)))
                          :assert-failed
                          (throw e)))))
           after (read!)]
       (is (= 1 @calls))
       (is (= :assert-failed result))
       (is (= 0 (:revision after)) "Aborted reserve must not bump revision")
       (is (= "available" (:status after)))
       (is (= 0 (:claims after)))
       (is (= "changed" (:audit-tag after)))
       (is (= (:created-at before) (:created-at after)))))))

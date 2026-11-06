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

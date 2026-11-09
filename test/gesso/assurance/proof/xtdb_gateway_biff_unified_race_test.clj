(ns gesso.assurance.proof.xtdb-gateway-biff-unified-race-test
  "v1595: permanent real-Biff/XTDB simultaneous-prestate correspondence tests.

   Two separate checked Ring requests reach their certified handlers ONLY
   after both have acquired their own request-scoped, identity-registered,
   ONE-capability complete-CAS monotonic writer. A barrier then releases
   their competing submissions. The physical database must incorporate at
   most one of the incompatible same-version updates, and the losing
   request must report an XTDB transaction assertion conflict.

   These bounded physical observations do NOT establish deployment-wide
   writer closure, target-table business authenticity, serializability of
   unrelated transactions, exact causation, ABA exclusion, liveness under
   arbitrary failures, or universal application refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test :as fixture]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test :as prestate]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as writers]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan-test])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private bounded-policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 30000})
(def ^:private wait-seconds 40)
(def ^:dynamic *modules* [])

(defn- fixture-helper [name]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      name)]
    (when-not v
      (throw (ex-info "Missing certified XTDB fixture"
                      {:reason :missing-certified-xtdb-fixture})))
    (var-get v)))

(defn- with-real-biff-and-xtdb [run]
  ((var-get (ns-resolve
             'gesso.assurance.proof.xtdb-gateway-biff-plan-bound-prestate-test
             'with-real-biff-and-xtdb)) run))

(use-fixtures :once with-real-biff-and-xtdb)

(defn- before [] ((fixture-helper 'before)))
(defn- after [b] ((fixture-helper 'after) b))
(defn- candidate [b a] ((fixture-helper 'candidate) b a))
(defn- put! [b] ((fixture-helper 'put!) b))
(defn- physical [] ((fixture-helper 'physical)))

(defn- checked-case! [source! arrived release bounded?]
  (let [{:keys [executor source target!]} prestate/*case*
        prepared-sources (atom 0)
        handler-entries (atom 0)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input!
                  (fn [_]
                    ;; This is called AFTER issuance of the request's
                    ;; acquired-prestate writer but BEFORE its submission.
                    (swap! handler-entries inc)
                    (.countDown arrived)
                    (when-not (.await release wait-seconds TimeUnit/SECONDS)
                      (throw (ex-info "Timed out before competing submissions"
                                      {:reason :race-release-timeout})))
                    (parallel/invoke-current-request-writer! writers/path)
                    ((var-get (ns-resolve 'gesso.model.certified-plan-test
                                          'invocation)) :reconcile))
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract (assoc-in (:contract prestate/*case*)
                           [:semantic-route-handlers operation] handler)
        resolver (provenance/issue-certified-plan-bound-literal-route-target-resolver!
                  contract {operation target!})
        host (adoption/issue-checked-biff-host-module! contract)
        modules [app/application-handler-module (:module host)]
        intermediate (binding [*modules* modules]
                       (app/start-biff-application!
                        (:application-assembly contract) {} #'*modules*
                        [app/application-handler-module-id adoption/module-id]))
        wrapped (fn [req]
                  (swap! prepared-sources inc)
                  (source! req))
        issued (if bounded?
                 (parallel/issue-unified-plan-bound-bounded-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writers/path}
                  prestate/*node* wrapped :revision bounded-policy resolver)
                 (parallel/issue-unified-plan-bound-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writers/path}
                  prestate/*node* wrapped :revision resolver))
        module (:module issued)
        _ ((:biff.core/init module) nil)
        final ((:biff.core/start module) intermediate)
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (:handle issued) contract final)]
    {:handler (get final app/biff-ring-handler-key)
     :prepared-sources prepared-sources
     :handler-entries handler-entries
     :report report}))

(defn- result-of [handler request]
  (try
    {:status (:status (handler request))}
    (catch Throwable t
      ;; XTDB assertion conflicts can be Errors, not ExceptionInfo.
      ;; Preserve the unexpected failure category without leaking the row.
      {:reason (or (:reason (ex-data t))
                   (:xtdb.error/code (ex-data t)))
       :exception-class (.getName (class t))})))

(defn- compete! [bounded?]
  (let [b (before)
        _ (put! b)
        arrived (CountDownLatch. 2)
        release (CountDownLatch. 1)
        after-left (assoc (after b) :note "left-wins")
        after-right (assoc (after b) :note "right-wins")
        c (checked-case!
           (fn [req]
             (candidate b (case (:race-side req)
                            :left after-left
                            :right after-right)))
           arrived release bounded?)
        handler (:handler c)
        outcomes [(future (result-of handler (assoc request :race-side :left)))
                  (future (result-of handler (assoc request :race-side :right)))]]
    (try
      (let [both-prepared? (.await arrived wait-seconds TimeUnit/SECONDS)]
        (is both-prepared?
            "Both independent requests must acquire an XTDB prestate before either submits")
        (when both-prepared?
          (is (= 2 @(:prepared-sources c)))
          (is (= 2 @(:handler-entries c)))
          (is (= b (physical))
              "Neither request may mutate XTDB merely by acquiring its writer")))
      (.countDown release)
      (let [results (mapv #(deref % (* 1000 wait-seconds) ::timed-out) outcomes)
            successes (filter #(= {:status 200} %) results)
            conflicts (filter #(= :xtdb/assert-failed (:reason %)) results)
            actual (physical)]
        (is (not-any? #{::timed-out} results)
            "Both request-scoped submissions must terminate after the barrier")
        (is (= 1 (count successes))
            "Exactly one competing request may report a completed write")
        (is (= 1 (count conflicts))
            "The losing request must surface a transaction-time CAS conflict")
        (is (contains? #{after-left after-right} actual)
            "The physical document must contain exactly one admitted after-state")
        (is (= 1 (:revision actual))
            "Two concurrent same-revision requests must not produce two advances")
        (is (= :one-prepared-acquired-gateway-for-this-submission
               (get-in c [:report :prestate-submission-correspondence])))
        (is (= (if bounded?
                 :bounded-fair-permits-with-request-local-registries
                 :unbounded-per-issued-mediator)
               (get-in c [:report :parallel-admission-policy])))
        (is (= :not-established (get-in c [:report :operation-refinement])))
        (is (= :not-established (get-in c [:report :other-writer-closure])))
        results)
      (finally
        (.countDown release)
        (doseq [f outcomes] (future-cancel f))))))

(deftest unbounded-unified-requests-compete-after-both-prestates-are-acquired
  (testing "two genuinely overlapping Biff requests cannot both commit one version"
    (compete! false)))

(deftest bounded-unified-requests-are-not-silently-serialized
  (testing "two bounded active permits admit both requests before either submits"
    (compete! true)))

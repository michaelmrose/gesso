(ns gesso.assurance.proof.xtdb-gateway-biff-unified-conflict-recovery-test
  "v1596: permanent real-Biff/XTDB recovery correspondence after a guarded
   competing-write conflict in v1593's one-gateway request mediator.

   Both initial requests independently acquire a valid guarded prestate before
   either submits. The losing handler tries its SAME registered callback again
   while still on its owning request thread: that consumed submission MUST be
   rejected. Only a FRESH HTTP request, with a NEW candidate prepared from the
   subsequently observed authoritative state, may advance the document again.

   This is a bounded physical adversarial regression, not a universal proof of
   retry liveness, external writer closure, physical causality, ABA exclusion,
   target-table business truth, or abstract operation refinement."
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
(def ^:private policy
  {:max-active-requests 2 :max-pending-requests 2 :wait-timeout-ms 30000})
(def ^:private wait-seconds 40)
(def ^:dynamic *modules* [])

(defn- fixture-helper [name]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      name)]
    (when-not v
      (throw (ex-info "Missing real XTDB fixture"
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

(defn- failure-reason [t]
  (or (:reason (ex-data t))
      (:xtdb.error/code (ex-data t))))

(defn- outcome [f]
  (try {:status (:status (f))}
       (catch Throwable t
         {:reason (failure-reason t)
          :exception-class (.getName (class t))})))

(defn- checked-case! [source! arrived release bounded? reuse-observed]
  (let [{:keys [executor source target!]} prestate/*case*
        constructed (atom 0)
        entered (atom 0)
        handler (certified/issue-certified-handler!
                 {:executor executor
                  :current-source source
                  :request->input!
                  (fn [req]
                    (swap! entered inc)
                    (when (#{:left :right} (:race-side req))
                      (.countDown arrived)
                      (when-not (.await release wait-seconds TimeUnit/SECONDS)
                        (throw (ex-info "Competing writes were not released"
                                        {:reason :race-release-timeout}))))
                    (try
                      (parallel/invoke-current-request-writer! writers/path)
                      (catch Throwable first-failure
                        ;; Attempt reuse *inside the original owning request*:
                        ;; failure must be one-shot consumption, not merely
                        ;; the later request-scope or wrong-thread rejection.
                        (swap! reuse-observed conj
                               {:request (:race-side req)
                                :first (failure-reason first-failure)
                                :second (:reason
                                         (outcome
                                          #(parallel/invoke-current-request-writer!
                                            writers/path)))})
                        (throw first-failure)))
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
        wrapped (fn [req] (swap! constructed inc) (source! req))
        issued (if bounded?
                 (parallel/issue-unified-plan-bound-bounded-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writers/path}
                  prestate/*node* wrapped :revision policy resolver)
                 (parallel/issue-unified-plan-bound-isolated-parallel-biff-request-module!
                  (:handle host) contract writers/*source* {operation writers/path}
                  prestate/*node* wrapped :revision resolver))
        module (:module issued)
        _ ((:biff.core/init module) nil)
        final ((:biff.core/start module) intermediate)
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (:handle issued) contract final)]
    {:handler (get final app/biff-ring-handler-key)
     :constructed constructed
     :entered entered
     :report report}))

(defn- exercise-recovery! [bounded?]
  (let [initial (before)
        _ (put! initial)
        left-after (assoc (after initial) :note "initial-left")
        right-after (assoc (after initial) :note "initial-right")
        recovery-source (atom nil)
        arrived (CountDownLatch. 2)
        release (CountDownLatch. 1)
        reuse-observed (atom [])
        app-case (checked-case!
                  (fn [req]
                    (case (:race-side req)
                      :left (candidate initial left-after)
                      :right (candidate initial right-after)
                      :stale (candidate initial left-after)
                      :recovery (or @recovery-source
                                    (throw (ex-info "No recovery prestate"
                                                    {:reason :missing-recovery-prestate})))
                      (throw (ex-info "Unsupported test request"
                                      {:reason :invalid-race-request}))))
                  arrived release bounded? reuse-observed)
        handler (:handler app-case)
        running [(future (outcome #(handler (assoc request :race-side :left))))
                 (future (outcome #(handler (assoc request :race-side :right))))]]
    (try
      (let [both-prepared? (.await arrived wait-seconds TimeUnit/SECONDS)]
        (is both-prepared? "Both request-local writers must exist before submission")
        (when both-prepared?
          (is (= 2 @(:constructed app-case) @(:entered app-case)))
          (is (= initial (physical))
              "The externally acquired prestates must not have written")))
      (.countDown release)
      (let [results (mapv #(deref % (* 1000 wait-seconds) ::timed-out) running)
            winning (physical)]
        (is (not-any? #{::timed-out} results))
        (is (= 1 (count (filter #(= 200 (:status %)) results)))
            "One and only one of the conflicting writes must commit")
        (is (= 1 (count (filter #(= :xtdb/assert-failed (:reason %)) results)))
            "The loser must observe the physical assertion failure")
        (is (contains? #{left-after right-after} winning))
        (is (= 1 (:revision winning)))
        (is (= 1 (count @reuse-observed))
            "Only the losing handler should attempt to reuse its writer")
        (is (contains? #{:left :right} (:request (first @reuse-observed))))
        (is (= :xtdb/assert-failed (:first (first @reuse-observed))))
        (is (= :one-shot-already-attempted (:second (first @reuse-observed)))
            "The losing writer cannot be reused even within its owning handler")
        (is (= 2 @(:entered app-case)))
        ;; Replaying an old source is not recovery, even though a new
        ;; request would receive an entirely fresh writer issuance.
        (let [stale (outcome #(handler (assoc request :race-side :stale)))]
          (is (nil? (:status stale)))
          (is (= :guarded-external-prestate-acquisition-failed
                 (:reason stale))
              "A replayed source must fail the independently acquired prestate check")
          (is (= winning (physical))
              "A new request with the stale prestate must not modify XTDB")
          (is (= 2 @(:entered app-case))
              "Stale candidate is rejected before invoking the handler"))
        ;; Recovery must be a *new* request, after authoritative observation.
        ;; It cannot reopen or reuse the losing request's capability.
        (let [fresh-after (-> (after winning)
                              (assoc :note "fresh-recovery-after-conflict"))]
          (reset! recovery-source (candidate winning fresh-after))
          (let [fresh (outcome #(handler (assoc request :race-side :recovery)))]
            (is (= {:status 200} fresh)
                "A fresh candidate from committed state can advance again")
            (is (= fresh-after (physical)))
            (is (= 2 (:revision (physical))))
            (is (= 3 @(:entered app-case))
                "Recovery obtains a fresh request-local registered writer")
            (is (= 1 (count @reuse-observed))
                "Only the original loser attempted in-request reuse")))
        (is (= :one-prepared-acquired-gateway-for-this-submission
               (get-in app-case [:report :prestate-submission-correspondence])))
        (is (= :not-established (get-in app-case [:report :operation-refinement])))
        (is (= :not-established (get-in app-case [:report :other-writer-closure])))
        (is (= (if bounded?
                 :bounded-fair-permits-with-request-local-registries
                 :unbounded-per-issued-mediator)
               (get-in app-case [:report :parallel-admission-policy]))))
      (finally
        (.countDown release)
        (doseq [f running] (future-cancel f))))))

(deftest unbounded-conflict-consumes-original-permit-but-fresh-request-can-recover
  (testing "a losing unified writer cannot retry; a new authoritative prestate can"
    (exercise-recovery! false)))

(deftest bounded-conflict-consumes-original-permit-but-fresh-request-can-recover
  (testing "bounded concurrent admission does not prevent a later fresh recovery"
    (exercise-recovery! true)))

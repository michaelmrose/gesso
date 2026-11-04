(ns gesso.assurance.xtdb-inductive-claim-concurrency-test
  "v1305: bounded adversarial physical-execution checks for the v1304
   induction-bound Claim pilot. These are observed single-node races, retries,
   aborted assertions, and reconciliations; NOT a general concurrency theorem.

   The actual v1252 construction has :request/status and :request/version,
   but no owner field. Its conditionally checked invariant is the restricted
   request-status domain. A single physical winner in these experiments is
   NOT a proof of unique business ownership or authorization correctness.

   The fixture reuses v1304's original test-owned induction/plan assembly and
   invokes the real issued executor and embedded XTDB. The only synchronization
   is an observation callback at :prepared, AFTER canonical plan checking.
   No XTDB execute-tx, canonical transaction function, proof checker, or
   certified-plan implementation is replaced in the concurrency path.

   Rejected physical assertions are not business rejections: an absent command
   record leaves a command unresolved. These bounded observations cannot
   authenticate arbitrary transaction payloads or establish universal
   refinement, write-path completeness, or deployment adoption."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.observed-correspondence :as joined]
            [gesso.assurance.xtdb-inductive-claim-integration-test]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.transition-observation :as observation]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private claim-operation :example/claim)
(def ^:dynamic *pilot* nil)

(defn- pilot-function [name]
  (let [v (ns-resolve 'gesso.assurance.xtdb-inductive-claim-integration-test name)]
    (when-not (and v (fn? @v))
      (throw (ex-info "Missing original v1304 pilot fixture" {:name name})))
    @v))

(defn- physical-row [node table id]
  (first (xt/q node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- seed! []
  (let [node (:node *pilot*)
        source (get-in *pilot* [:package :plan-source])
        before ((:current-document! source) {} {})]
    (xt/execute-tx node [[:put-docs :requests before]])
    before))

(defn- binding-for [principal]
  (assoc ((pilot-function 'command-binding))
         :principal (identity/principal principal)))

(defn- invoke! [binding mode callback]
  (let [executor (get-in *pilot* [:package :executor])]
    (executor {:biff.xtdb/node (:node *pilot*)
               resolved/observation-context-key callback}
              {:binding binding :mode mode})))

(defn- event-outcome [events]
  (first (filter #(= :physical-outcome (:phase %)) events)))

(defn- physical-record [binding]
  (physical-row (:node *pilot*) :gesso_commands
                (resolution/record-id binding)))

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- race!
  ([] (race! [(binding-for :helper/alice) (binding-for :helper/bob)]))
  ([bindings]
  (let [before (seed!)
        ready (CountDownLatch. 2)
        start (CountDownLatch. 1)
        events [(atom []) (atom [])]
        observer (fn [event index]
                   (swap! (nth events index) conj event)
                   (when (= :prepared (:phase event))
                     (.countDown ready)
                     (when-not (.await start 30 TimeUnit/SECONDS)
                       (throw (ex-info "Timed out at prepared race gate" {})))))
        tasks (mapv (fn [i]
                      (future (invoke! (nth bindings i) :submit
                                       #(observer % i))))
                    (range 2))]
    (try
      (let [both-ready? (.await ready 30 TimeUnit/SECONDS)]
        (.countDown start)
        (let [results (mapv #(deref % 60000 ::timeout) tasks)
              outcomes (mapv #(event-outcome @%) events)
              physical-statuses (mapv :physical/status outcomes)
              winner-index (first (keep-indexed
                                   (fn [i status] (when (= :committed status) i))
                                   physical-statuses))]
          {:ready? both-ready? :before before :bindings bindings
           :results results :outcomes outcomes :physical-statuses physical-statuses
           :winner-index winner-index :events (mapv deref events)
           :after (physical-row (:node *pilot*) :requests (:xt/id before))
           :records (mapv physical-record bindings)}))
      (finally
        (.countDown start)
        (doseq [task tasks]
          (when-not (realized? task) (future-cancel task))))))))

(use-fixtures
  :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (let [package ((pilot-function 'source-package) node)]
        (fixture/with-closed-application
          {:operations {claim-operation
                        (fixture/trusted-operation
                         claim-operation {:published-change-topics #{}})}}
          (fn [{:keys [assembly]}]
            (binding [*pilot*
                      {:node node :package package
                       :application-source
                       ((pilot-function 'source-for-application)
                        assembly package)}]
              (run))))))))

(deftest race-between-different-command-identities-has-one-durable-winner
  (dotimes [round 2]
    (testing (str "bounded induction-bound XTDB race " round)
      (let [{:keys [ready? bindings results outcomes physical-statuses
                    winner-index before after records events]} (race!)]
        (is ready? "Both invocations reach their verified prepared boundary")
        (is (not-any? #{::timeout} results))
        (is (= #{:incorporated :unresolved} (set (map :status results))))
        (is (= #{:committed :rejected} (set physical-statuses)))
        (is (= 1 (count (filter #{:committed} physical-statuses))))
        (is (= :xtdb-explicit-assertion-failure
               (:physical/source (nth outcomes (bit-xor winner-index 1)))))
        (is (= [[:prepared :physical-outcome] [:prepared :physical-outcome]]
               (mapv #(mapv :phase %) events)))
        (is (= (assoc before :request/status :claimed
                             :request/version (inc (:request/version before)))
               after))
        (is (= 1 (count (filter some? records))))
        (is (= (nth bindings winner-index)
               (:gesso.command/binding (nth records winner-index))))
        (is (nil? (nth records (bit-xor winner-index 1))))
        (is (= :incorporated
               (:gesso.command/status (nth records winner-index))))))))


(deftest concurrent-submissions-with-identical-command-id-never-create-two-records
  (let [shared (binding-for :helper/alice)
        {:keys [ready? results outcomes physical-statuses after records]} (race! [shared shared])]
    (is ready?)
    (is (not-any? #{::timeout} results))
    (is (= #{:committed :rejected} (set physical-statuses)))
    (is (= 1 (count (filter #{:committed} physical-statuses))))
    ;; Both observations address ONE durable identity, regardless of how
    ;; either invocation resolves the failed physical attempt.
    (is (= 1 (count (set (keep :xt/id records)))))
    (is (= shared (:gesso.command/binding (first records))))
    (is (= shared (:gesso.command/binding (second records))))
    (is (= :claimed (:request/status after)))
    (is (= 3 (:request/version after)))
    (is (not= [:incorporated :incorporated] (mapv :status results)))
    (is (every? #(= :physical-outcome (:phase %)) outcomes))))

(deftest successful-command-replays-and-reconciles-without-a-second-write
  (let [{:keys [bindings winner-index records]} (race!)
        winner (nth bindings winner-index)
        expected-record (nth records winner-index)
        events (atom [])
        replay (invoke! winner :submit #(swap! events conj %))
        reconcile (invoke! winner :reconcile #(swap! events conj %))]
    (is (= [:replay :replay] (mapv :status [replay reconcile])))
    (is (= (:outcome replay) (:outcome reconcile)))
    (is (empty? @events) "No second prepared event or physical submission")
    (is (= expected-record (physical-record winner)))
    (is (= :none
           (:authority (induction-plan/describe-inductive-plan
                        (get-in *pilot* [:package :executor])))))))

(deftest losing-command-remains-unresolved-and-is-not-resubmitted-on-reconcile
  (let [{:keys [bindings winner-index records]} (race!)
        loser (nth bindings (bit-xor winner-index 1))
        events (atom [])
        recovered (invoke! loser :reconcile #(swap! events conj %))]
    (is (= :unresolved (:status recovered)))
    (is (empty? @events))
    (is (nil? (physical-record loser)))
    (is (= 1 (count (filter some? records))))
    (is (not= :rejected (:status recovered))
        "A failed XTDB assertion is not a durable business rejection")))

(deftest a-durable-command-record-cannot-be-borrowed-by-another-principal
  (let [{:keys [bindings winner-index]} (race!)
        winner (nth bindings winner-index)
        stolen (assoc winner :principal (identity/principal :helper/charlie))
        events (atom [])
        result (invoke! stolen :reconcile #(swap! events conj %))]
    (is (= :incompatible (:status result)))
    (is (empty? @events))
    (is (= winner (:gesso.command/binding (physical-record winner))))))

(deftest a-stale-physical-version-is-an-xtdb-failure-not-a-false-claim
  (let [before (seed!)
        stale (assoc before :request/version (inc (:request/version before)))
        _ (xt/execute-tx (:node *pilot*) [[:put-docs :requests stale]])
        binding (binding-for :helper/alice)
        events (atom [])
        result (invoke! binding :submit #(swap! events conj %))
        outcome (event-outcome @events)]
    (is (= :unresolved (:status result)))
    (is (= [:prepared :physical-outcome] (mapv :phase @events)))
    (is (= :rejected (:physical/status outcome)))
    (is (= :xtdb-explicit-assertion-failure (:physical/source outcome)))
    (is (= stale (physical-row (:node *pilot*) :requests (:xt/id before))))
    (is (nil? (physical-record binding)))))

(deftest an-aborted-attempt-does-not-become-a-replay-or-automatic-retry
  (let [before (seed!)
        changed (assoc before :request/status :claimed :request/version 3)
        _ (xt/execute-tx (:node *pilot*) [[:put-docs :requests changed]])
        binding (binding-for :helper/alice)
        first-events (atom [])
        first-attempt (invoke! binding :submit #(swap! first-events conj %))
        reconcile-events (atom [])
        reconcile (invoke! binding :reconcile #(swap! reconcile-events conj %))]
    (is (= :unresolved (:status first-attempt)))
    (is (= :rejected (:physical/status (event-outcome @first-events))))
    (is (= :unresolved (:status reconcile)))
    (is (empty? @reconcile-events))
    (is (nil? (physical-record binding)))
    (is (= changed (physical-row (:node *pilot*) :requests (:xt/id before))))
    (is (= :not-established
           (:physical-correspondence
            (induction-plan/describe-inductive-plan
             (get-in *pilot* [:package :executor])))))))


(deftest repeated-explicit-submit-after-physical-abort-is-not-an-automatic-retry
  (let [before (seed!)
        stale (assoc before :request/version 5)
        _ (xt/execute-tx (:node *pilot*) [[:put-docs :requests stale]])
        binding (binding-for :helper/alice)
        first-events (atom [])
        second-events (atom [])
        read-events (atom [])
        first-attempt (invoke! binding :submit #(swap! first-events conj %))
        second-attempt (invoke! binding :submit #(swap! second-events conj %))
        check-only (invoke! binding :reconcile #(swap! read-events conj %))]
    ;; These are TWO distinct, deliberately requested trusted :submit calls.
    ;; The runtime must NOT interpret them as a safe recovery protocol; the
    ;; caller must handle unresolved identity through its own reconciliation.
    (is (= [:unresolved :unresolved :unresolved]
           (mapv :status [first-attempt second-attempt check-only])))
    (is (= [:prepared :physical-outcome] (mapv :phase @first-events)))
    (is (= [:prepared :physical-outcome] (mapv :phase @second-events)))
    (is (= :rejected (:physical/status (event-outcome @first-events))))
    (is (= :rejected (:physical/status (event-outcome @second-events))))
    (is (empty? @read-events))
    (is (nil? (physical-record binding)))
    (is (= stale (physical-row (:node *pilot*) :requests (:xt/id before))))))

(deftest observation-window-with-an-intervening-commit-must-fail-closed
  (let [before (seed!)
        binding (binding-for :helper/alice)
        capture (atom nil)
        actual-commit (atom nil)
        events (atom [])
        observer (fn [event]
                   (swap! events conj event)
                   (case (:phase event)
                     :prepared
                     (let [plan (:plan event)
                           record (:record event)
                           command-target [:gesso_commands (:xt/id record)]
                           handle (observation/begin-observation!
                                   {:node (:node *pilot*)
                                    :incorporation {:plan plan :record record
                                                    :scope resolution/assurance-scope}
                                    :targets [[:requests (:xt/id before)]
                                              command-target]})]
                       (reset! capture handle)
                       ;; An unrelated physical write lands INSIDE the observed
                       ;; window; v1298 must not call it a single isolated step.
                       (xt/execute-tx (:node *pilot*)
                                      [[:put-docs :unrelated
                                        {:xt/id (str (random-uuid))}]]))
                     :physical-outcome
                     (reset! actual-commit event)
                     nil))
        result (invoke! binding :submit observer)
        receipt (:physical/receipt @actual-commit)
        tx-key ((pilot-function 'receipt-key) receipt)]
    (is (= :incorporated (:status result)))
    (is (= :committed (:physical/status @actual-commit)))
    (is (some? @capture))
    (is (= :non-isolated-or-mismatched-ledger-window
           (reason #(observation/complete-observation!
                     @capture {:tx-key tx-key}))))
    (is (some? (physical-record binding)))
    (is (= :claimed
           (:request/status (physical-row (:node *pilot*) :requests
                                          (:xt/id before)))))))

(deftest isolated-post-contention-claim-can-be-compared-with-abstract-step
  (let [sample ((pilot-function 'run-claim!) (:node *pilot*)
                (:package *pilot*) {})
        result (joined/check-observed-correspondence!
                {:application-source (:application-source *pilot*)
                 :transition-source
                 ((pilot-function 'transition-source)
                  (:package *pilot*) sample)})]
    (is (= :incorporated (get-in sample [:result :status])))
    (is (true? (:diagnostic-conforms? result)))
    (is (= :corresponds-to-declared-step (:diagnostic-correspondence result)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance result)))
    (is (= :not-established (:authenticated-physical-correspondence result)))
    (is (= :not-established (:universal-xtdb-refinement result)))
    (is (= :none (:authority result)))))

(deftest concurrency-and-replay-cannot-upgrade-original-inductive-proof
  (let [{:keys [package]} *pilot*
        report (induction-plan/describe-inductive-plan (:executor package))]
    (is (= :conditionally-checked-in-declared-abstract-machine
           (:inductive-invariant report)))
    (is (= :none (:authority report)))
    (doseq [premise [:prestate-to-commit-atomic-guard-enforcement
                     :physical-write-path-completeness
                     :abstract-to-xtdb-transition-correspondence
                     :authorization-and-callback-purity
                     :induction-checker-independent-soundness]]
      (is (contains? (:open-obligations report) premise)))
    (doseq [status [:atomicity-assurance :physical-correspondence
                   :physical-write-path-completeness :actual-router-adoption]]
      (is (= :not-established (get report status))))))

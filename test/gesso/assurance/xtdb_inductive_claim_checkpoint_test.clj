(ns gesso.assurance.xtdb-inductive-claim-checkpoint-test
  "v1307: bounded, Gesso-local physical-correspondence acceptance checkpoint.

   The original v1252 Claim specification, v1290 issued executor, v1294
   checked application, v1298 persisted observation and v1302 diagnostic
   correspondence are exercised together against embedded XTDB. A second
   in-process fixture exercises real competing submissions. Corrupted commits
   are test-only changes at the final transaction boundary, after canonical
   plan admission. The tests do NOT prove universal XTDB refinement, owner
   uniqueness, authenticated witness provenance, complete production writers,
   authorization or the correctness of the trusted read callback.

   Reuse the actual v1304/v1305 proving fixtures, not a second illustrative
   operation language. In particular, a committed command without its domain
   write must be diagnosed as nonconforming even though its returned outcome
   says :incorporated. A failed physical assertion has NO commit witness."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.observed-correspondence :as correspondence]
            [gesso.assurance.xtdb-inductive-claim-integration-test]
            [gesso.assurance.xtdb-inductive-claim-concurrency-test]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private operation :example/claim)
(def ^:dynamic *checkpoint* nil)

(defn- borrowed [namespace symbol]
  (let [v (ns-resolve namespace symbol)]
    (when-not (and v (fn? @v))
      (throw (ex-info "Missing established Gesso test fixture"
                      {:namespace namespace :symbol symbol})))
    @v))

(defn- pilot [symbol]
  (borrowed 'gesso.assurance.xtdb-inductive-claim-integration-test symbol))

(defn- concurrent [symbol]
  (borrowed 'gesso.assurance.xtdb-inductive-claim-concurrency-test symbol))

(defn- read-row [node table id]
  (first (xt/q node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- observed! [sample]
  (correspondence/check-observed-correspondence!
   {:application-source (:application-source *checkpoint*)
    :transition-source ((pilot 'transition-source)
                        (:package *checkpoint*) sample)}))

(defn- caught [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- write-mutation! [change]
  ;; The production admission path is unchanged. Replace only the final
  ;; submission, and keep the original committed XTDB result observable.
  (let [submit tx/transact!
        attempts (atom [])
        sample (with-redefs [tx/transact!
                             (fn [ctx plan]
                               (let [altered (change plan)]
                                 (swap! attempts conj {:declared plan :actual altered})
                                 (submit ctx altered)))]
                 ((pilot 'run-claim!) (:node *checkpoint*)
                  (:package *checkpoint*) {}))]
    (assoc sample :submitted @attempts)))

(defn- edit-claim [f]
  (fn [plan]
    (update plan :commands
            (fn [commands]
              (mapv #(if (= :claim (:model/operation %)) (f %) %) commands)))))

(defn- change-poststate [f]
  (edit-claim #(update % :model/after f)))

(defn- claim-record [binding]
  (read-row (:node *checkpoint*) :gesso_commands
            (resolution/record-id binding)))

(defn- checked-boundaries! [r]
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :not-established (:universal-xtdb-refinement r)))
  (is (= :not-established (:authenticated-physical-correspondence r)))
  (is (= :not-established (:physical-write-path-completeness r)))
  (is (= :not-established (:application-invariant-physical-preservation r)))
  (is (= :not-established (:production-certification r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (= :caller-supplied-unauthenticated
         (:physical-observation-provenance r)))
  (is (contains? (:open-obligations r)
                 :physical-observation-origin-is-unauthenticated)))

(use-fixtures
  :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (let [package ((pilot 'source-package) node)]
        (fixture/with-closed-application
          {:operations {operation (fixture/trusted-operation
                                   operation {:published-change-topics #{}})}}
          (fn [{:keys [assembly]}]
            (let [application-source ((pilot 'source-for-application)
                                      assembly package)
                  context {:node node :package package
                           :application-source application-source}]
              (binding [*checkpoint* context]
                (let [good ((pilot 'run-claim!) node package {})
                      no-domain ((pilot 'run-claim!) node package
                                 {:record-only? true})
                      wrong-status (write-mutation!
                                    (change-poststate
                                     #(assoc % :request/status :open)))
                      wrong-frame (write-mutation!
                                   (change-poststate
                                    #(assoc % :request/score 99)))
                      aborted (write-mutation!
                               (edit-claim
                                #(assoc-in % [:model/expected :model/checks]
                                           [[:request/version 999]])))
                      race (binding [gesso.assurance.xtdb-inductive-claim-concurrency-test/*pilot*
                                     context]
                             ((concurrent 'race!)))
                      samples {:good good :no-domain no-domain
                               :wrong-status wrong-status
                               :wrong-frame wrong-frame
                               :aborted aborted :race race}]
                  (binding [*checkpoint* (assoc context :samples samples)]
                    (run)))))))))))

(defn- sample [key] (get-in *checkpoint* [:samples key]))

(deftest checkpoint-admits-exact-source-bound-claim-plan
  (let [{:keys [executor induction-source]} (:package *checkpoint*)
        r (induction-plan/require-current-inductive-plan!
           executor induction-source)]
    (is (= #{operation} (:catalog-operation-ids r)))
    (is (= :conditionally-checked-in-declared-abstract-machine
           (:inductive-invariant r)))
    (is (= :not-established (:physical-correspondence r)))
    (is (= :none (:authority r)))))

(deftest actual-issued-executor-commits-declared-claim
  (let [{:keys [result before events physical-outcome
                observed-request observed-record binding]} (sample :good)]
    (is (= :incorporated (:status result)))
    (is (= :committed (get-in result [:transaction :commit/status])))
    (is (= [:prepared :physical-outcome] (mapv :phase events)))
    (is (= :committed (:physical/status physical-outcome)))
    (is (= :claimed (:request/status observed-request)))
    (is (= (inc (:request/version before)) (:request/version observed-request)))
    (is (= (:request/score before) (:request/score observed-request)))
    (is (= :incorporated (:gesso.command/status observed-record)))
    (is (= binding (:gesso.command/binding observed-record)))))

(deftest accepted-physical-observation-corresponds-diagnostically
  (let [r (observed! (sample :good))]
    (is (true? (:diagnostic-conforms? r)))
    (is (= :corresponds-to-declared-step (:diagnostic-correspondence r)))
    (is (= :exact-source-and-issued-executor-rechecked
           (:plan-correspondence r)))
    (checked-boundaries! r)))

(deftest persisted-command-without-domain-update-diverges
  (let [{:keys [result before observed-request observed-record
                physical-outcome]} (sample :no-domain)
        r (observed! (sample :no-domain))]
    (is (= :incorporated (:status result)))
    (is (= :committed (:physical/status physical-outcome)))
    (is (= before observed-request))
    (is (= :incorporated (:gesso.command/status observed-record)))
    (is (false? (:diagnostic-conforms? r)))
    (is (= :diverges-from-declared-step (:diagnostic-correspondence r)))
    (is (false? (get-in r [:observed-checks :abstract-assignments])))
    (checked-boundaries! r)))

(deftest committed-wrong-status-must-not-refine-claim
  (let [s (sample :wrong-status)
        r (observed! s)]
    (is (= 1 (count (:submitted s))))
    (is (= :committed (get-in s [:physical-outcome :physical/status])))
    (is (= :open (get-in s [:observed-request :request/status])))
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:observed-checks :abstract-assignments])))
    (checked-boundaries! r)))

(deftest committed-framed-field-mutation-must-not-refine-claim
  (let [s (sample :wrong-frame)
        r (observed! s)]
    (is (= 1 (count (:submitted s))))
    (is (= :committed (get-in s [:physical-outcome :physical/status])))
    (is (= 99 (get-in s [:observed-request :request/score])))
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:observed-checks :abstract-frame])))
    (checked-boundaries! r)))

(deftest failed-xtdb-assertion-cannot-produce-commit-witness
  (let [s (sample :aborted)]
    (is (= 1 (count (:submitted s))))
    (is (= :unresolved (get-in s [:result :status])))
    (is (= :rejected (get-in s [:physical-outcome :physical/status])))
    (is (= :xtdb-explicit-assertion-failure
           (get-in s [:physical-outcome :physical/source])))
    (is (nil? (:witness s)))
    (is (nil? (:observed-record s)))
    (is (some? (caught #(observed! s)))
        "No observation can be compared after an aborted physical submit")))

(deftest two-contenders-have-one-observed-durable-winner
  (let [{:keys [ready? results bindings records winner-index
                physical-statuses before after]} (sample :race)]
    (is ready?)
    (is (not-any? #{::timeout} results))
    (is (= #{:incorporated :unresolved} (set (map :status results))))
    (is (= #{:committed :rejected} (set physical-statuses)))
    (is (= 1 (count (filter #{:committed} physical-statuses))))
    (is (= 1 (count (filter some? records))))
    (is (= (nth bindings winner-index)
           (:gesso.command/binding (nth records winner-index))))
    (is (= :claimed (:request/status after)))
    (is (= (inc (:request/version before)) (:request/version after)))))

(deftest loser-reconcile-does-not-resubmit-or-claim-rejection
  (let [{:keys [bindings winner-index]} (sample :race)
        loser (nth bindings (bit-xor winner-index 1))
        executor (get-in *checkpoint* [:package :executor])
        events (atom [])
        r (executor {:biff.xtdb/node (:node *checkpoint*)
                     resolved/observation-context-key #(swap! events conj %)}
                    {:binding loser :mode :reconcile})]
    (is (= :unresolved (:status r)))
    (is (empty? @events))
    (is (nil? (claim-record loser)))
    (is (not= :rejected (:status r)))))

(deftest winner-durable-replay-is-not-a-second-submission
  (let [{:keys [bindings winner-index records]} (sample :race)
        winner (nth bindings winner-index)
        executor (get-in *checkpoint* [:package :executor])
        events (atom [])
        invoke (fn [mode]
                 (executor {:biff.xtdb/node (:node *checkpoint*)
                            resolved/observation-context-key
                            #(swap! events conj %)}
                           {:binding winner :mode mode}))
        replay (invoke :submit)
        reconcile (invoke :reconcile)]
    (is (= [:replay :replay] (mapv :status [replay reconcile])))
    (is (= (:outcome replay) (:outcome reconcile)))
    (is (empty? @events))
    (is (= (nth records winner-index) (claim-record winner)))))

(deftest changed-abstract-guard-cannot-borrow-observed-proof
  (let [s (sample :good)
        transition ((pilot 'transition-source) (:package *checkpoint*) s)
        altered (assoc-in transition [:construction :guard :expression]
                          [:literal true])
        e (caught #(correspondence/check-observed-correspondence!
                    {:application-source (:application-source *checkpoint*)
                     :transition-source altered}))]
    (is (= :gesso.assurance.observed-correspondence/invalid-observed-correspondence
           (:error/type e)))
    (is (= :observation-not-bound-to-issued-plan (:reason e)))))

(deftest fabricated-valid-looking-observation-never-authenticates-itself
  (let [s (sample :good)
        fabricated (assoc-in s [:witness :physical-tx-id] 90000000)
        r (observed! fabricated)]
    (is (true? (:diagnostic-conforms? r)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance r)))
    (checked-boundaries! r)))

(deftest forged-authority-claim-fails-closed
  (let [s (sample :good)
        fabricated (assoc-in s [:witness :authority] :proved)
        e (caught #(observed! fabricated))]
    (is (= :gesso.model.transition-conformance/invalid-transition-conformance
           (:error/type e)))
    (is (= :invalid-observation-claims (:reason e)))))

(deftest physical-correspondence-checkpoint-remains-empirical
  (let [good (observed! (sample :good))
        wrong (observed! (sample :wrong-status))]
    (is (not= (:diagnostic-correspondence good)
              (:diagnostic-correspondence wrong)))
    (is (= :one-supplied-unauthenticated-physical-observation
           (:scope good)))
    (is (= :non-authoritative-observed-application-step-diagnostic
           (:evidence-class good)))
    (doseq [obligation [:physical-observation-origin-is-unauthenticated
                        :single-observation-does-not-establish-universal-refinement
                        :all-physical-writers-are-not-established
                        :independent-induction-checker-soundness]]
      (is (contains? (:open-obligations good) obligation)))
    (checked-boundaries! good)
    (checked-boundaries! wrong)))

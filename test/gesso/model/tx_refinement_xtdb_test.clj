(ns gesso.model.tx-refinement-xtdb-test
  "Permanent controlled-XTDB regressions for direct, ledger-linked, bracketed, and ledger-window observations.

   Exercises the actual collector: it independently reads the declared target
   domain, constructs and submits canonical tx/prepare operations, classifies
   the resulting XTDB outcome, and compares poststate to the bounded serial
   reference model. Tests intentionally distinguish invalid input rejected
   *before* any physical I/O from real assertion rejection after submission.

   A successful report is diagnostic evidence for this controlled execution,
   not an authenticated XTDB receipt or a general refinement theorem. The
   fixture assumes exclusive control of its embedded node and target IDs.
   Ledger-linked tests inspect XTDB tx keys against the pinned xt.txs row,
   including missing, contradictory, and unavailable evidence. Bracketed
   tests independently inspect the before-ledger high watermark and its
   ordering relative to the submitted physical transaction. None of these
   diagnostics attest a production submission or certify physical refinement.
   Ledger-window regressions additionally check phase-local ledger ceilings,
   transaction-ID adjacency, detected intervening writes, and explicitly
   unverifiable isolation assumptions. The v1017 opt-in repeatable-window
   regressions replay the exact snapshot-token and clock for both phases,
   and test false replay evidence without resubmitting the transaction.
   v1028 exercises the v1027 composed conditional-step boundary, separating
   local validity from observed physical agreement, without certification."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-refinement-prestate :as prestate]
   [gesso.model.tx-refinement-xtdb :as collector]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))

(defn- node []
  (or @!node
      (throw (ex-info "The embedded XTDB fixture is not running." {}))))

(defn- with-real-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- request [prefix]
  {:xt/id (str prefix "-" (random-uuid))
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [prefix]
  {:xt/id (str prefix "-" (random-uuid))
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(defn- advance [document changes]
  (-> document
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- plan [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(defn- claim [document owner]
  (command/update-command
   :requests :claim document (advance document {:owner owner}) version))

(defn- suspend [document]
  (command/update-command
   :memberships :suspend document (advance document {:active false}) version))

(defn- seed! [table document]
  (xt/execute-tx (node) [[:put-docs table document]])
  document)

(defn- physical-document [table id]
  (let [table-name (case table
                     :requests "requests"
                     :memberships "memberships")
        rows (xt/q (node) [(str "SELECT * FROM " table-name " WHERE _id = ?") id])]
    (is (<= (count rows) 1) "A target must identify at most one XTDB row.")
    (first rows)))

(defn- collect! [a-plan targets]
  (collector/observe-and-compare! {:node (node)
                                   :plan a-plan
                                   :targets targets}))

(defn- error-type [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:error/type (ex-data e)))))

(defn- check-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (every? true? (vals (get-in report [:comparison :checks]))))
  (is (= :direct-controlled-xtdb-observation (:evidence-kind report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (not (contains? report :recorded-tx-ops)))
  (is (not (contains? report :observed-before)))
  (is (not (contains? report :observed-after)))
  (is (not (contains? report :reference-before)))
  (is (not (contains? report :plan)))
  report)

(deftest collector-scope-never-promotes-observation-to-certification-test
  (let [{:keys [collector collected assumed not-established] :as scope}
        collector/collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-step-observation-v1 collector))
    (is (= 4 (count (select-keys scope
                                 [:collector :collected :assumed :not-established]))))
    (doseq [claim [:reference-prestate-from-physical-reads
                   :complete-explicit-target-reads
                   :operations-from-production-tx-prepare
                   :same-operations-passed-to-xtdb-execute-tx
                   :definitive-xtdb-outcome-classification
                   :physical-poststate-reads
                   :recorded-step-comparison]]
      (is (contains? collected claim)))
    (doseq [assumption [:exclusive-control-of-tracked-documents-during-collection
                        :all-semantic-read-and-write-dependencies-identified
                        :consistent-series-of-separate-xtdb-reads
                        :supplied-node-is-the-intended-controlled-database]]
      (is (contains? assumed assumption)))
    (doseq [unproved [:authenticated-physical-submission-receipt
                      :production-live-execution-correspondence
                      :one-pinned-snapshot-across-all-target-reads
                      :physical-read-completeness-outside-declared-targets
                      :absence-of-external-writers
                      :general-xtdb-step-refinement
                      :safe-unknown-outcome-reconciliation
                      :authorization-and-business-invariants
                      :durable-command-incorporation-and-replay
                      :browser-or-choreography-correspondence]]
      (is (contains? not-established unproved))
      (is (not (contains? collected unproved))))))

(deftest physical-claim-and-stale-retry-correspond-test
  (let [req (seed! :requests (request "collected-claim"))
        member (seed! :memberships (membership "collected-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-a")]
                     (command/guard :memberships member version))
        before (physical-document :requests (:xt/id req))
        accepted (check-report! (collect! action [rt mt]) :committed)
        after (physical-document :requests (:xt/id req))
        retry (check-report! (collect! action [rt mt]) :rejected)]
    (is (= "unclaimed" (:owner before)))
    (is (= "helper-a" (:owner after)))
    (is (= 1 (:revision after)))
    (is (= :xtdb/assert-failed (get-in retry [:execution :error/code])))
    (is (= :command-version
           (get-in retry [:comparison :failed-precondition-kind])))
    (is (= after (physical-document :requests (:xt/id req))))
    (is (= 2 (:collected-target-count accepted)))
    (is (pos? (:submitted-operation-count accepted)))
    (is (= (:submitted-operation-count accepted)
           (:submitted-operation-count retry)))
    (is (true? (get-in accepted [:comparison :checks :recorded-submission])))
    (is (true? (get-in retry [:comparison :checks :recorded-submission])))))

(deftest stale-membership-dependency-rejects-otherwise-current-claim-test
  (let [req (seed! :requests (request "collected-auth"))
        member (seed! :memberships (membership "collected-auth-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-b")]
                     (command/guard :memberships member version))]
    (xt/execute-tx (node) [[:put-docs :memberships (advance member {:active false})]])
    (let [report (check-report! (collect! action [rt mt]) :rejected)]
      (is (= :xtdb/assert-failed (get-in report [:execution :error/code])))
      (is (= :dependency-version
             (get-in report [:comparison :failed-precondition-kind])))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (false? (:active (physical-document :memberships (:xt/id member))))))))

(deftest rejected-multi-document-transaction-is-atomic-test
  (let [req (seed! :requests (request "collected-atomic"))
        member (seed! :memberships (membership "collected-atomic-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-c") (suspend member)])]
    ;; Make the second write stale after the plan was constructed.
    (xt/execute-tx (node) [[:put-docs :memberships (advance member {:active false})]])
    (let [before-member (physical-document :memberships (:xt/id member))
          report (check-report! (collect! action [rt mt]) :rejected)]
      (is (= :xtdb/assert-failed (get-in report [:execution :error/code])))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (= before-member (physical-document :memberships (:xt/id member))))
      (is (= 2 (:collected-target-count report))))))

(deftest create-then-duplicate-create-correspond-test
  (let [req (request "collected-create")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])]
    (is (nil? (physical-document :requests (:xt/id req))))
    (let [created (check-report! (collect! action [rt]) :committed)
          duplicate (check-report! (collect! action [rt]) :rejected)]
      (is (= 1 (:collected-target-count created)))
      (is (= req (physical-document :requests (:xt/id req))))
      (is (= :document-absent
             (get-in duplicate [:comparison :failed-precondition-kind])))
      (is (= :xtdb/assert-failed (get-in duplicate [:execution :error/code]))))))

(deftest nil-fields-are-projected-at-the-physical-boundary-test
  (let [req (assoc (request "collected-nil") :optional nil)
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        report (check-report! (collect! action [rt]) :committed)
        physical (physical-document :requests (:xt/id req))]
    (is (true? (get-in report [:comparison :checks :poststate])))
    (is (not (contains? physical :optional)))
    (is (= (dissoc req :optional) physical))))

(deftest field-count-assertion-checks-xtdb-persisted-fields-test
  (let [req (seed! :requests (assoc (request "collected-count") :nullable nil))
        rt [:requests (:xt/id req)]
        updated (claim req "helper-d")
        base (plan [updated])
        count-assert (tx/assert-document-field-count :requests (:xt/id req) 5)
        allowed (check-report!
                 (collect! (assoc base :assertions [count-assert]) [rt])
                 :committed)
        stale (check-report!
               (collect! (assoc base :assertions [count-assert]) [rt])
               :rejected)]
    (is (= 1 (:revision (physical-document :requests (:xt/id req)))))
    (is (= "helper-d" (:owner (physical-document :requests (:xt/id req)))))
    (is (true? (get-in allowed [:comparison :checks :recorded-submission])))
    (is (= :command-version
           (get-in stale [:comparison :failed-precondition-kind])))))

(deftest supplemental-observation-targets-are-preserved-test
  (let [req (seed! :requests (request "collected-extra"))
        spectator (seed! :memberships (membership "collected-extra-spectator"))
        rt [:requests (:xt/id req)]
        st [:memberships (:xt/id spectator)]
        action (plan [(claim req "helper-e")])
        report (check-report! (collect! action [rt st]) :committed)]
    (is (= 2 (:collected-target-count report)))
    (is (= spectator (physical-document :memberships (:xt/id spectator))))
    (is (= "helper-e" (:owner (physical-document :requests (:xt/id req)))))))

(deftest missing-read-and-write-targets-fail-before-physical-io-test
  (let [req (request "collected-missing")
        member (membership "collected-missing-member")
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "helper-f")]
                     (command/guard :memberships member version))
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (is (= ::collector/untracked-dependencies
             (error-type #(collect! action [rt]))))
      (is (= ::collector/untracked-dependencies
             (error-type #(collect! action [mt]))))
      (is (= [] @calls)))))

(deftest unsupported-table-and-plan-fail-before-any-physical-io-test
  (let [req (request "collected-unsafe")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (testing "Extra tracked targets with SQL-unsafe table identifiers"
        (is (= ::collector/unsupported-table
               (error-type #(collect! action [rt [:bad-name/unsafe-table "x"]])))))
      (testing "Unknown SQL assertion is never assumed true"
        (is (= :gesso.model.tx-model/unsupported-plan
               (error-type #(collect! (assoc action :assertions [{:assert [:select 1]}])
                                     [rt])))))
      (is (= [] @calls)))))

(deftest input-shape-and-target-integrity-fail-before-physical-io-test
  (let [req (request "collected-input")
        rt [:requests (:xt/id req)]
        base {:node (node)
              :plan (plan [(command/create :requests req version)])
              :targets [rt]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (doseq [invalid [(dissoc base :targets)
                       (assoc base :execution {:status :committed})
                       (assoc base :observed-before {})
                       (assoc base :recorded-tx-ops [])
                       (assoc base :node nil)]]
        (is (some? (error-type #(collector/observe-and-compare! invalid)))))
      (is (= ::collector/invalid-input
             (error-type #(collector/observe-and-compare!
                           (assoc base :execution {:status :committed})))))
      (is (= ::collector/missing-node
             (error-type #(collector/observe-and-compare!
                           (assoc base :node nil)))))
      (doseq [targets [[] [rt rt] [[:requests nil]] [:requests]]]
        (is (= :gesso.model.tx-refinement/invalid-targets
               (error-type #(collector/observe-and-compare!
                             (assoc base :targets targets))))))
      (is (= [] @calls)))))

(deftest unknown-execution-error-is-propagated-without-retry-test
  (let [req (seed! :requests (request "collected-unknown"))
        rt [:requests (:xt/id req)]
        original (physical-document :requests (:xt/id req))
        submission-count (atom 0)
        failure (ex-info "The execution outcome is unknown"
                         {:error/type ::indeterminate-execution})
        action (plan [(claim req "helper-g")])]
    (is (identical?
         failure
         (try
           (with-redefs [xt/execute-tx (fn [& _]
                                         (swap! submission-count inc)
                                         (throw failure))]
             (collect! action [rt]))
           nil
           (catch clojure.lang.ExceptionInfo error
             error))))
    (is (= 1 @submission-count))
    (is (= original (physical-document :requests (:xt/id req))))))

;; v1008's snapshot-pinned collection is deliberately narrower than a
;; transaction-internal snapshot guarantee. These checks exercise actual XTDB
;; query options and adversarial cross-phase changes, not merely the collector's
;; declared scope map.

(deftest pinned-basis-is-reused-within-each-physical-phase-test
  (let [req (seed! :requests (request "pin-basis-request"))
        member (seed! :memberships (membership "pin-basis-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        original-query xt/q
        calls (atom [])
        action (plan [(claim req "pin-helper")]
                     (command/guard :memberships member version))
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [query (second args)]
                           ;; xt/q's 2-arity delegates to its 3-arity.
                           ;; Count the physical query only at that boundary.
                           (when (= 3 (count args))
                             (swap! calls conj
                                    (if (= "SHOW SNAPSHOT_TOKEN" query)
                                      {:kind :basis}
                                      {:kind :read
                                       :query query
                                       :options (nth args 2)})))
                           (apply original-query args)))]
                 (collect! action [rt mt]))
        [basis-1 before-1 before-2 basis-2 after-1 after-2] @calls
        before-basis (:options before-1)
        after-basis (:options after-1)]
    (check-report! report :committed)
    (is (= [:basis :read :read :basis :read :read]
           (mapv :kind @calls)))
    (is (= :basis (:kind basis-1)))
    (is (= :basis (:kind basis-2)))
    (is (= before-basis (:options before-2)))
    (is (= after-basis (:options after-2)))
    (is (= #{:snapshot-token :current-time} (set (keys before-basis))))
    (is (= #{:snapshot-token :current-time} (set (keys after-basis))))
    (is (string? (:snapshot-token before-basis)))
    (is (seq (:snapshot-token before-basis)))
    (is (instance? java.time.Instant (:current-time before-basis)))
    (is (instance? java.time.Instant (:current-time after-basis)))
    (is (not= (:snapshot-token before-basis)
              (:snapshot-token after-basis)))
    (is (= :separate-pinned-before-and-after-phases
           (:observation-basis report)))
    (is (contains? (get-in report [:collection-scope :collected])
                   :phase-local-pinned-snapshot-token-and-clock))
    (is (contains? (get-in report [:collection-scope :not-established])
                   :transaction-internal-snapshot-equals-observed-prestate))
    (is (contains? (get-in report [:collection-scope :not-established])
                   :same-snapshot-for-before-and-after-phases))))

(deftest missing-or-malformed-before-snapshot-token-fails-before-submission-test
  (let [req (request "pin-invalid-before")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        invalid-rows [nil [] {} [{}] [{:snapshot-token nil}]
                      [{:snapshot-token ""}] [{:snapshot-token 17}]
                      [{:snapshot-token "one"} {:snapshot-token "two"}]]]
    (doseq [rows invalid-rows]
      (let [calls (atom [])]
        (with-redefs [xt/q (fn [& args]
                             ;; This stub returns immediately at the public
                             ;; 2-arity, so no inner 3-arity query is issued.
                             (swap! calls conj [:query (second args)])
                             rows)
                      xt/execute-tx (fn [& _]
                                      (swap! calls conj [:submit])
                                      (throw (ex-info "unexpected submission" {})))]
          (is (= ::collector/missing-snapshot-token
                 (error-type #(collect! action [rt])))
              (str "Invalid basis shape should fail closed: " (pr-str rows)))
          (is (= [[:query "SHOW SNAPSHOT_TOKEN"]] @calls)
              "No target read or transaction may follow a malformed basis."))))
    (is (nil? (physical-document :requests (:xt/id req))))))

(deftest unavailable-after-snapshot-does-not-turn-commit-into-rejection-test
  (let [req (request "pin-invalid-after")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        original-query xt/q
        original-execute xt/execute-tx
        basis-count (atom 0)
        submissions (atom 0)
        failure
        (try
          (with-redefs [xt/q (fn [& args]
                               (if (and (= 3 (count args))
                                        (= "SHOW SNAPSHOT_TOKEN" (second args))
                                        (= 2 (swap! basis-count inc)))
                                 [{:snapshot-token nil}]
                                 (apply original-query args)))
                        xt/execute-tx (fn [& args]
                                        (when (= 3 (count args))
                                          (swap! submissions inc))
                                        (apply original-execute args))]
            (collect! action [rt]))
          nil
          (catch clojure.lang.ExceptionInfo error error))]
    (is (= ::collector/missing-snapshot-token
           (:error/type (ex-data failure))))
    (is (= 2 @basis-count))
    (is (= 1 @submissions) "A committed command must never be resubmitted.")
    (is (= req (physical-document :requests (:xt/id req)))
        "The physical commit happened despite the failed post-observation.")))

(deftest pinned-before-phase-does-not-conceal-cross-phase-interference-test
  (let [req (seed! :requests (request "pin-mid-read-request"))
        spectator (seed! :memberships (membership "pin-mid-read-member"))
        rt [:requests (:xt/id req)]
        st [:memberships (:xt/id spectator)]
        original-query xt/q
        original-execute xt/execute-tx
        injected? (atom false)
        member-reads (atom [])
        changed-member (advance spectator {:active false})
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [query (second args)
                               rows (apply original-query args)]
                           ;; Mutate after the first target was read, but before
                           ;; the second target is read from the pinned basis.
                           (when (and (vector? query)
                                      (= (:xt/id req) (second query))
                                      (compare-and-set! injected? false true))
                             (original-execute (node)
                                               [[:put-docs :memberships changed-member]]))
                           (when (and (vector? query)
                                      (= (:xt/id spectator) (second query)))
                             (swap! member-reads conj (first rows)))
                           rows))]
                 (collect! (plan [(claim req "mid-read-helper")]) [rt st]))]
    (is @injected?)
    (is (= [spectator changed-member] @member-reads)
        "The pinned before-phase sees the old member; the later phase sees the write.")
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (get-in report [:comparison :checks :prestate])))
    (is (false? (get-in report [:comparison :checks :poststate])))
    (is (= [st] (get-in report [:comparison :poststate-mismatches])))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (false? (:certifies-physical-execution? report)))))

(deftest writer-between-before-phase-and-submit-breaks-correspondence-test
  (let [req (seed! :requests (request "pin-pre-submit"))
        rt [:requests (:xt/id req)]
        original-execute xt/execute-tx
        submissions (atom 0)
        external (advance req {:owner "outside-writer"})
        report (with-redefs
                 [xt/execute-tx (fn [& args]
                                  (if (= 2 (count args))
                                    (apply original-execute args)
                                    (let [[db operations] args]
                                      (swap! submissions inc)
                                      ;; Call original 3-arity to avoid a second
                                      ;; interception through its 2-arity.
                                      (original-execute db
                                                        [[:put-docs :requests external]] {})
                                      (apply original-execute args))))]
                 (collect! (plan [(claim req "expected-helper")]) [rt]))]
    (is (= 1 @submissions) "The collector itself submits once.")
    (is (= :rejected (get-in report [:execution :status])))
    (is (= :xtdb/assert-failed (get-in report [:execution :error/code])))
    (is (false? (get-in report [:comparison :checks :outcome-status])))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (= external (physical-document :requests (:xt/id req))))
    (is (contains? (get-in report [:collection-scope :assumed])
                   :exclusive-control-of-tracked-documents-during-collection))))

(deftest writer-after-submit-before-after-phase-is-detected-test
  (let [req (seed! :requests (request "pin-post-submit"))
        rt [:requests (:xt/id req)]
        original-execute xt/execute-tx
        submissions (atom 0)
        externally-replaced (advance (advance req {:owner "expected-helper"})
                                     {:owner "late-external-writer"})
        ;; This writer runs after execute-tx has definitively committed but
        ;; before the collector acquires the after-phase snapshot.
        report (with-redefs
                 [xt/execute-tx (fn [& args]
                                  (if (= 2 (count args))
                                    (apply original-execute args)
                                    (let [[db _operations] args]
                                      (swap! submissions inc)
                                      (let [result (apply original-execute args)]
                                        (original-execute db
                                                          [[:put-docs :requests externally-replaced]] {})
                                        result))))]
                 (collect! (plan [(claim req "expected-helper")]) [rt]))]
    (is (= 1 @submissions))
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (get-in report [:comparison :checks :outcome-status])))
    (is (false? (get-in report [:comparison :checks :poststate])))
    (is (= [rt] (get-in report [:comparison :poststate-mismatches])))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (false? (:certifies-physical-execution? report)))
    (is (= externally-replaced
           (physical-document :requests (:xt/id req))))))


;; v1010 ledger-linkage: only transaction ID, definitive disposition and
;; physical system time are checked. Do not confuse a matching xt.txs row
;; with independent attestation of the submitted transaction payload.

(defn- collect-ledger! [a-plan targets]
  (collector/observe-and-compare-ledger! {:node (node)
                                         :plan a-plan
                                         :targets targets}))

(def ^:private ledger-checks
  #{:tx-ledger-record-present
    :tx-ledger-id
    :tx-ledger-commit-status
    :tx-ledger-system-time})

(def ^:private ledger-sql
  "SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?")

(defn- check-ledger-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (true? (:transaction-ledger-matches? report)))
  (is (every? true? (vals (select-keys (get-in report [:comparison :checks])
                                       ledger-checks))))
  (is (= ledger-checks
         (set (filter ledger-checks
                      (keys (get-in report [:comparison :checks]))))))
  (is (= :direct-controlled-xtdb-ledger-linked-observation
         (:evidence-kind report)))
  (is (= :gesso.model.tx/direct-xtdb-ledger-linked-step-v1
         (get-in report [:collection-scope :collector])))
  (is (integer? (:physical-tx-id report)))
  (is (<= 0 (:physical-tx-id report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (not-any? #(contains? report %)
                [:node :plan :tx-key :recorded-tx-ops
                 :observed-before :observed-after :reference-before
                 :snapshot-token :system-time]))
  report)

(deftest ledger-scope-preserves-open-assumptions-test
  (let [{:keys [collector collected assumed not-established]}
        collector/ledger-collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-ledger-linked-step-v1 collector))
    (doseq [fact [:tx-key-from-physical-execute-tx
                 :xt-txs-record-from-pinned-post-observation
                 :ledger-id-system-time-and-commit-status-comparison]]
      (is (contains? collected fact)))
    (is (contains? assumed :trustworthy-xtdb-tx-key-and-system-ledger))
    (doseq [open [:authenticated-physical-submission-receipt
                 :production-live-execution-correspondence
                 :transaction-internal-snapshot-equals-observed-prestate
                 :absence-of-external-writers
                 :general-xtdb-step-refinement
                 :authorization-and-business-invariants]]
      (is (contains? not-established open))
      (is (not (contains? collected open))))))

(deftest ledger-commit-and-stale-rejection-have-distinct-physical-ids-test
  (let [req (seed! :requests (request "ledger-claim"))
        member (seed! :memberships (membership "ledger-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "ledger-helper")]
                     (command/guard :memberships member version))
        commit (check-ledger-report! (collect-ledger! action [rt mt]) :committed)
        retry (check-ledger-report! (collect-ledger! action [rt mt]) :rejected)]
    (is (< (:physical-tx-id commit) (:physical-tx-id retry)))
    (is (= :xtdb/assert-failed (get-in retry [:execution :error/code])))
    (is (= :command-version
           (get-in retry [:comparison :failed-precondition-kind])))
    (is (= 1 (:revision (physical-document :requests (:xt/id req)))))
    (is (= "ledger-helper" (:owner (physical-document :requests (:xt/id req)))))
    (is (true? (get-in commit [:comparison :checks :recorded-submission])))
    (is (true? (get-in retry [:comparison :checks :recorded-submission])))))

(deftest ledger-duplicate-create-and-guard-rejection-correspond-test
  (let [new-req (request "ledger-create")
        rt [:requests (:xt/id new-req)]
        create (plan [(command/create :requests new-req version)])
        first-result (check-ledger-report! (collect-ledger! create [rt]) :committed)
        duplicate (check-ledger-report! (collect-ledger! create [rt]) :rejected)
        membership-doc (seed! :memberships (membership "ledger-guard"))
        mt [:memberships (:xt/id membership-doc)]
        claim-action (plan [(claim new-req "ledger-wrong-guard")]
                           (command/guard :memberships membership-doc version))]
    (xt/execute-tx (node)
                   [[:put-docs :memberships
                     (advance membership-doc {:active false})]])
    (let [guard (check-ledger-report! (collect-ledger! claim-action [rt mt]) :rejected)]
      (is (< (:physical-tx-id first-result) (:physical-tx-id duplicate)))
      (is (= :dependency-version
             (get-in guard [:comparison :failed-precondition-kind])))
      (is (= new-req (physical-document :requests (:xt/id new-req)))))))

(deftest ledger-query-uses-the-exact-post-observation-basis-test
  (let [req (seed! :requests (request "ledger-basis"))
        rt [:requests (:xt/id req)]
        calls (atom [])
        original-query xt/q
        report (with-redefs
                 [xt/q (fn [& args]
                         (when (= 3 (count args))
                           (let [query (second args)]
                             (swap! calls conj
                                    {:kind (cond
                                             (= query "SHOW SNAPSHOT_TOKEN") :basis
                                             (and (vector? query) (= ledger-sql (first query))) :ledger
                                             :else :document)
                                     :query query
                                     :options (nth args 2)})))
                         (apply original-query args))]
                 (collect-ledger! (plan [(claim req "basis-helper")]) [rt]))
        [before-basis before-read after-basis after-read ledger] @calls]
    (check-ledger-report! report :committed)
    (is (= [:basis :document :basis :document :ledger]
           (mapv :kind @calls)))
    (is (= (:options after-read) (:options ledger)))
    (is (not= (:options before-read) (:options ledger)))
    (is (= #{:snapshot-token :current-time}
           (set (keys (:options ledger)))))
    (is (= ledger-sql (first (:query ledger))))
    (is (= [(:physical-tx-id report)] (subvec (:query ledger) 1)))
    (is (= :basis (:kind before-basis)))
    (is (= :basis (:kind after-basis)))
    (is (not= (:snapshot-token (:options before-read))
              (:snapshot-token (:options after-read))))))

(defn- poison-ledger
  "Intercept ONLY the physical ledger query. All document reads, snapshots,
   and the single submitted transaction remain real."
  [transform thunk]
  (let [real-query xt/q]
    (with-redefs [xt/q (fn [& args]
                         (let [query (second args)
                               rows (apply real-query args)]
                           (if (and (vector? query)
                                    (= ledger-sql (first query)))
                             (transform rows)
                             rows)))]
      (thunk))))

(deftest missing-duplicate-and-malformed-ledger-rows-fail-comparison-test
  (doseq [[label alter expected]
          [["missing" (constantly []) :tx-ledger-record-present]
           ["duplicate" (fn [rows] [(first rows) (first rows)]) :tx-ledger-record-present]
           ["map-not-sequence" (constantly {}) :tx-ledger-record-present]
           ["nil-row" (constantly [nil]) :tx-ledger-record-present]
           ["wrong-id" (fn [rows] [(update (first rows) :xt/id inc)]) :tx-ledger-id]
           ["wrong-status" (fn [rows] [(update (first rows) :committed not)]) :tx-ledger-commit-status]
           ["absent-status" (fn [rows] [(dissoc (first rows) :committed)]) :tx-ledger-commit-status]
           ["string-status" (fn [rows] [(assoc (first rows) :committed "true")]) :tx-ledger-commit-status]
           ["wrong-time" (fn [rows] [(update (first rows) :system-time
                                              #(.plusSeconds ^java.time.ZonedDateTime % 10))]) :tx-ledger-system-time]
           ["missing-time" (fn [rows] [(dissoc (first rows) :system-time)]) :tx-ledger-system-time]
           ["string-time" (fn [rows] [(assoc (first rows) :system-time "not-time")]) :tx-ledger-system-time]]]
    (testing label
      (let [req (seed! :requests (request (str "ledger-poison-" label)))
            rt [:requests (:xt/id req)]
            report (poison-ledger alter
                                  #(collect-ledger! (plan [(claim req "poison-helper")]) [rt]))
            checks (get-in report [:comparison :checks])]
        (is (= :committed (get-in report [:execution :status])))
        (is (false? (:transaction-ledger-matches? report)))
        (is (false? (get-in report [:comparison :corresponds?])))
        (is (false? (get checks expected)))
        (is (true? (:recorded-submission checks)))
        (is (true? (:poststate checks)))
        (is (= "poison-helper" (:owner (physical-document :requests (:xt/id req)))))
        (is (false? (:certifies-physical-execution? report)))))))

(deftest contradicted-rejection-ledger-cannot-be-reported-corresponding-test
  (let [req (seed! :requests (request "ledger-reject-contradiction"))
        rt [:requests (:xt/id req)]
        action (plan [(claim req "reject-first")])
        _ (check-ledger-report! (collect-ledger! action [rt]) :committed)
        rejected (poison-ledger
                   (fn [rows] [(assoc (first rows) :committed true)])
                   #(collect-ledger! action [rt]))]
    (is (= :rejected (get-in rejected [:execution :status])))
    (is (= :xtdb/assert-failed (get-in rejected [:execution :error/code])))
    (is (false? (get-in rejected [:comparison :checks :tx-ledger-commit-status])))
    (is (true? (get-in rejected [:comparison :checks :recorded-submission])))
    (is (false? (:transaction-ledger-matches? rejected)))
    (is (false? (get-in rejected [:comparison :corresponds?])))))

(deftest absent-or-invalid-physical-tx-key-fails-after-one-submit-test
  (doseq [[label break-key] [["missing" (constantly nil)]
                              ["malformed" (constantly {:tx-id "not-an-id"})]
                              ["negative" (fn [key] (assoc key :tx-id -1))]
                              ["missing-time" (fn [key] (dissoc key :system-time))]]]
    (testing label
      (let [req (request (str "ledger-key-" label))
            rt [:requests (:xt/id req)]
            submissions (atom 0)
            real-execute xt/execute-tx
            result (try
                     (with-redefs [xt/execute-tx
                                   (fn [& args]
                                     (let [key (apply real-execute args)]
                                       (when (= 3 (count args))
                                         (swap! submissions inc))
                                       (if (= 3 (count args))
                                         (break-key key)
                                         key)))]
                       (collect-ledger! (plan [(command/create :requests req version)]) [rt]))
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
        (is (= ::collector/missing-tx-key (:error/type (ex-data result))))
        (is (= 1 @submissions) "A committed operation is not retried after missing evidence.")
        (is (= req (physical-document :requests (:xt/id req))))))))

(deftest ledger-query-throws-after-commit-without-retry-test
  (let [req (request "ledger-query-error")
        rt [:requests (:xt/id req)]
        submissions (atom 0)
        ledger-calls (atom 0)
        failure (ex-info "unavailable ledger" {:error/type ::ledger-query-unavailable})
        real-query xt/q
        real-execute xt/execute-tx
        caught (try
                 (with-redefs
                   [xt/q (fn [& args]
                           (if (and (vector? (second args))
                                    (= ledger-sql (first (second args))))
                             (do (swap! ledger-calls inc) (throw failure))
                             (apply real-query args)))
                    xt/execute-tx (fn [& args]
                                    (when (= 3 (count args))
                                      (swap! submissions inc))
                                    (apply real-execute args))]
                   (collect-ledger! (plan [(command/create :requests req version)]) [rt]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (identical? failure caught))
    (is (= 1 @submissions))
    (is (= 1 @ledger-calls))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest ledger-input-shape-is-identical-to-original-collector-test
  (let [req (request "ledger-input")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        base {:node (node) :plan action :targets [rt]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :query) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (is (= ::collector/invalid-input
             (error-type #(collector/observe-and-compare-ledger!
                           (assoc base :claimed-tx-key {:tx-id 0})))))
      (is (= ::collector/invalid-input
             (error-type #(collector/observe-and-compare-ledger!
                           (assoc base :execution {:status :committed})))))
      (is (= ::collector/untracked-dependencies
             (error-type #(collector/observe-and-compare-ledger!
                           (assoc base :targets [[:memberships "missing"]])))))
      (is (= [] @calls)))))


;; v1012 ledger bracketing: one pinned BEFORE snapshot provides the maximum
;; already-visible transaction ID, and a distinct pinned AFTER snapshot
;; provides the specific submitted transaction ledger entry. Neither is a
;; transaction-internal read snapshot or a proof of intervening-writer absence.

(def ^:private before-watermark-sql
  "SELECT _id FROM xt.txs ORDER BY _id DESC LIMIT 1")

(defn- collect-bracketed! [a-plan targets]
  (collector/observe-and-compare-bracketed!
   {:node (node) :plan a-plan :targets targets}))

(defn- check-bracketed-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (true? (get-in report [:comparison :checks
                            :executed-tx-after-before-watermark])))
  (is (true? (:transaction-ledger-matches? report)))
  (is (= :direct-controlled-xtdb-ledger-bracketed-observation
         (:evidence-kind report)))
  (is (= :gesso.model.tx/direct-xtdb-ledger-bracketed-step-v1
         (get-in report [:collection-scope :collector])))
  (is (integer? (:physical-tx-id report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (= #{:executed-tx-after-before-watermark}
         (set (filter #{:executed-tx-after-before-watermark}
                      (keys (get-in report [:comparison :checks]))))))
  (is (not-any? #(contains? report %)
                [:node :plan :tx-key :snapshot-token :system-time
                 :recorded-tx-ops :observed-before :observed-after
                 :reference-before :before-ledger-watermark]))
  report)

(deftest bracketed-scope-reports-its-assumptions-and-non-proofs-test
  (let [{:keys [collector collected assumed not-established]}
        collector/bracketed-ledger-collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-ledger-bracketed-step-v1 collector))
    (doseq [fact [:pinned-before-ledger-high-watermark
                 :executed-tx-id-greater-than-before-watermark
                 :xt-txs-record-from-pinned-post-observation]]
      (is (contains? collected fact)))
    (doseq [assumption [:monotonic-xtdb-transaction-ids
                        :trustworthy-xtdb-tx-key-and-system-ledger
                        :exclusive-control-of-tracked-documents-during-collection]]
      (is (contains? assumed assumption)))
    (doseq [open [:no-intervening-external-transactions
                 :transaction-internal-snapshot-equals-observed-prestate
                 :absence-of-external-writers
                 :authenticated-physical-submission-receipt
                 :general-xtdb-step-refinement]]
      (is (contains? not-established open))
      (is (not (contains? collected open))))))

(deftest bracketed-commit-and-stale-rejection-correspond-test
  (let [req (seed! :requests (request "bracket-claim"))
        member (seed! :memberships (membership "bracket-member"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "bracket-helper")]
                     (command/guard :memberships member version))
        commit (check-bracketed-report!
                (collect-bracketed! action [rt mt]) :committed)
        reject (check-bracketed-report!
                (collect-bracketed! action [rt mt]) :rejected)]
    (is (< (:physical-tx-id commit) (:physical-tx-id reject)))
    (is (= :xtdb/assert-failed (get-in reject [:execution :error/code])))
    (is (= :command-version
           (get-in reject [:comparison :failed-precondition-kind])))
    (is (= "bracket-helper" (:owner (physical-document :requests (:xt/id req)))))))

(deftest bracketed-create-and-duplicate-physical-ids-test
  (let [req (request "bracket-create")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        commit (check-bracketed-report! (collect-bracketed! action [rt]) :committed)
        reject (check-bracketed-report! (collect-bracketed! action [rt]) :rejected)]
    (is (< (:physical-tx-id commit) (:physical-tx-id reject)))
    (is (= :document-absent
           (get-in reject [:comparison :failed-precondition-kind])))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest bracketed-ledger-watermark-precedes-submission-and-shares-before-basis-test
  (let [req (seed! :requests (request "bracket-read-basis"))
        rt [:requests (:xt/id req)]
        events (atom [])
        real-query xt/q
        real-execute xt/execute-tx
        report (with-redefs
                 [xt/q (fn [& args]
                         (when (= 3 (count args))
                           (let [query (second args)]
                             (swap! events conj
                                    {:kind (cond
                                             (= query "SHOW SNAPSHOT_TOKEN") :basis
                                             (= query before-watermark-sql) :watermark
                                             (and (vector? query) (= (first query) ledger-sql)) :after-ledger
                                             :else :document)
                                     :basis (nth args 2)})))
                         (apply real-query args))
                  xt/execute-tx (fn [& args]
                                  (when (= 3 (count args))
                                    (swap! events conj {:kind :submit}))
                                  (apply real-execute args))]
                 (collect-bracketed! (plan [(claim req "bracket-basis")]) [rt]))
        phases (mapv :kind @events)
        before-read (first (filter #(= :document (:kind %)) @events))
        watermark (first (filter #(= :watermark (:kind %)) @events))
        document-reads (filter #(= :document (:kind %)) @events)
        after-read (second document-reads)
        after-ledger (first (filter #(= :after-ledger (:kind %)) @events))]
    (check-bracketed-report! report :committed)
    (is (= [:basis :document :watermark :submit :basis :document :after-ledger]
           phases))
    (is (= (:basis before-read) (:basis watermark)))
    (is (= (:basis after-read) (:basis after-ledger)))
    (is (not= (:basis watermark) (:basis after-ledger)))
    (is (= #{:snapshot-token :current-time}
           (set (keys (:basis watermark)))))
    (is (string? (get-in watermark [:basis :snapshot-token])))))

(deftest empty-before-ledger-history-uses-explicit-negative-one-watermark-test
  (let [req (request "bracket-empty-ledger")
        rt [:requests (:xt/id req)]
        real-query xt/q
        seen (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (if (= before-watermark-sql (second args))
                           (do (swap! seen inc) [])
                           (apply real-query args)))]
                 (collect-bracketed!
                  (plan [(command/create :requests req version)]) [rt]))]
    (check-bracketed-report! report :committed)
    (is (= 1 @seen))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest invalid-before-ledger-watermark-fails-before-submission-test
  (let [req (request "bracket-malformed")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        invalid-rows [nil {} [nil] [{}] [{:xt/id -1}]
                      [{:xt/id "8"}] [{:xt/id 1.5}]
                      [{:xt/id 0} {:xt/id 1}]]
        real-query xt/q]
    (doseq [rows invalid-rows]
      (testing (pr-str rows)
        (let [submissions (atom 0)]
          (with-redefs
            [xt/q (fn [& args]
                    (if (= before-watermark-sql (second args))
                      rows
                      (apply real-query args)))
             xt/execute-tx (fn [& _]
                             (swap! submissions inc)
                             (throw (ex-info "should not submit" {})))]
            (is (= ::collector/invalid-before-ledger-watermark
                   (error-type #(collect-bracketed! action [rt]))))
            (is (zero? @submissions))))))
    (is (nil? (physical-document :requests (:xt/id req))))))

(deftest unavailable-before-ledger-query-does-not-submit-test
  (let [req (request "bracket-query-unavailable")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        failure (ex-info "No ledger available" {:error/type ::watermark-query-failed})
        original-query xt/q
        submissions (atom 0)
        caught (try
                 (with-redefs
                   [xt/q (fn [& args]
                           (if (= before-watermark-sql (second args))
                             (throw failure)
                             (apply original-query args)))
                    xt/execute-tx (fn [& _]
                                    (swap! submissions inc)
                                    (throw (ex-info "unexpected submit" {})))]
                   (collect-bracketed! action [rt]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (identical? failure caught))
    (is (zero? @submissions))
    (is (nil? (physical-document :requests (:xt/id req))))))

(deftest falsely-newer-before-watermark-rejects-correspondence-not-physical-commit-test
  (let [req (seed! :requests (request "bracket-false-watermark"))
        rt [:requests (:xt/id req)]
        action (plan [(claim req "physical-commit-still-occurs")])
        real-query xt/q
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [rows (apply real-query args)]
                           (if (= before-watermark-sql (second args))
                             [{:xt/id (+ 1000000000 (or (:xt/id (first rows)) 0))}]
                             rows)))]
                 (collect-bracketed! action [rt]))
        checks (get-in report [:comparison :checks])]
    (is (= :committed (get-in report [:execution :status])))
    (is (false? (:executed-tx-after-before-watermark checks)))
    (is (true? (:transaction-ledger-matches? report)))
    (is (every? true? (vals (select-keys checks ledger-checks))))
    (is (true? (:poststate checks)))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (= "physical-commit-still-occurs"
           (:owner (physical-document :requests (:xt/id req)))))
    (is (false? (:certifies-physical-execution? report)))))

(deftest bracket-does-not-prove-absence-of-intervening-writers-test
  (let [req (seed! :requests (request "bracket-intervening-request"))
        outsider (membership "bracket-intervening-outsider")
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-execute xt/execute-tx
        injected (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [rows (apply real-query args)]
                           (when (and (= before-watermark-sql (second args))
                                      (zero? @injected))
                             ;; The watermark was already observed. An
                             ;; unrelated writer commits before our submit.
                             (swap! injected inc)
                             (real-execute (node)
                                           [[:put-docs :memberships outsider]]))
                           rows))]
                 (collect-bracketed!
                  (plan [(claim req "bracket-intervening-helper")]) [rt]))]
    (check-bracketed-report! report :committed)
    (is (= 1 @injected))
    (is (= outsider (physical-document :memberships (:xt/id outsider))))
    (is (contains? (get-in report [:collection-scope :not-established])
                   :no-intervening-external-transactions))))

(deftest bracketed-input-shape-rejects-client-claimed-evidence-before-io-test
  (let [req (request "bracket-input")
        rt [:requests (:xt/id req)]
        base {:node (node)
              :plan (plan [(command/create :requests req version)])
              :targets [rt]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :query) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (doseq [extra [:before-ledger-watermark :tx-key :recorded-tx-ops
                     :observed-before :execution]]
        (is (= ::collector/invalid-input
               (error-type #(collector/observe-and-compare-bracketed!
                             (assoc base extra :untrusted)))))
        (is (empty? @calls)))
      (is (= ::collector/untracked-dependencies
             (error-type #(collector/observe-and-compare-bracketed!
                           (assoc base :targets [[:memberships "other"]])))))
      (is (empty? @calls)))))


;; v1015: Window checks are intentionally STRONGER than the ID bracket, but
;; remain observations rather than a production execution certificate.
(defn- collect-window! [a-plan targets]
  (collector/observe-and-compare-ledger-window!
   {:node (node) :plan a-plan :targets targets}))

(def ^:private window-check-keys
  #{:executed-tx-after-before-watermark
    :executed-tx-immediate-successor
    :executed-tx-equals-after-watermark})

(defn- check-window-report! [report expected-status]
  (is (= expected-status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (every? true? (vals (select-keys (get-in report [:comparison :checks])
                                      window-check-keys))))
  (is (= :direct-controlled-xtdb-ledger-window-observation
         (:evidence-kind report)))
  (is (= :gesso.model.tx/direct-xtdb-ledger-window-step-v1
         (get-in report [:collection-scope :collector])))
  (is (true? (:transaction-ledger-matches? report)))
  (is (integer? (:physical-tx-id report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (not-any? #(contains? report %)
                [:node :plan :tx-key :snapshot-token :system-time
                 :recorded-tx-ops :observed-before :observed-after
                 :reference-before :before-ledger-watermark
                 :after-ledger-watermark]))
  report)

(deftest ledger-window-scope-does-not-assert-refinement-or-isolation-test
  (let [{:keys [collector collected assumed not-established]}
        collector/ledger-window-collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-ledger-window-step-v1 collector))
    (doseq [fact [:pinned-before-ledger-high-watermark
                 :pinned-after-ledger-high-watermark
                 :executed-tx-immediate-successor-of-before-watermark
                 :executed-tx-equals-after-ledger-watermark]]
      (is (contains? collected fact)))
    (doseq [premise [:monotonic-xtdb-transaction-ids
                    :unique-monotonic-xtdb-transaction-ids
                    :ledger-high-watermarks-include-all-visible-tx-ids
                    :exclusive-control-of-tracked-documents-during-collection]]
      (is (contains? assumed premise)))
    (doseq [unproved [:transaction-internal-snapshot-equals-observed-prestate
                     :no-writer-outside-pinned-observation-window
                     :no-in-flight-or-future-external-commits
                     :transaction-snapshot-attestation
                     :authenticated-physical-submission-receipt
                     :general-xtdb-step-refinement]]
      (is (contains? not-established unproved))
      (is (not (contains? collected unproved))))))

(deftest ledger-window-commit-and-rejected-retry-correspond-test
  (let [req (seed! :requests (request "window-retry"))
        rt [:requests (:xt/id req)]
        action (plan [(claim req "window-helper")])
        commit (check-window-report! (collect-window! action [rt]) :committed)
        reject (check-window-report! (collect-window! action [rt]) :rejected)]
    (is (= :xtdb/assert-failed (get-in reject [:execution :error/code])))
    (is (< (:physical-tx-id commit) (:physical-tx-id reject)))
    (is (= "window-helper" (:owner (physical-document :requests (:xt/id req)))))))

(deftest ledger-window-duplicate-create-rejection-still-has-ledger-entry-test
  (let [req (request "window-duplicate")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        first-result (check-window-report! (collect-window! action [rt]) :committed)
        second-result (check-window-report! (collect-window! action [rt]) :rejected)]
    (is (< (:physical-tx-id first-result) (:physical-tx-id second-result)))
    (is (= :document-absent
           (get-in second-result [:comparison :failed-precondition-kind])))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest ledger-window-high-watermarks-share-pinned-document-bases-test
  (let [req (seed! :requests (request "window-bases"))
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-execute xt/execute-tx
        events (atom [])
        report (with-redefs
                 [xt/q (fn [& args]
                         (when (= 3 (count args))
                           (let [query (second args)]
                             (swap! events conj
                                    {:kind (cond
                                             (= query "SHOW SNAPSHOT_TOKEN") :basis
                                             (= before-watermark-sql query) :watermark
                                             (and (vector? query) (= ledger-sql (first query))) :ledger
                                             :else :document)
                                     :basis (nth args 2)})))
                         (apply real-query args))
                  xt/execute-tx (fn [& args]
                                  (when (= 2 (count args))
                                    (swap! events conj {:kind :submit}))
                                  (apply real-execute args))]
                 (collect-window! (plan [(claim req "window-bases-helper")]) [rt]))
        [before-basis before-doc before-watermark submit
         after-basis after-doc ledger after-watermark] @events]
    (check-window-report! report :committed)
    (is (= [:basis :document :watermark :submit
            :basis :document :ledger :watermark]
           (mapv :kind @events)))
    (is (= :basis (:kind before-basis)))
    (is (= :basis (:kind after-basis)))
    (is (= (:basis before-doc) (:basis before-watermark)))
    (is (= (:basis after-doc) (:basis ledger) (:basis after-watermark)))
    (is (not= (:basis before-doc) (:basis after-doc)))
    (is (= #{:snapshot-token :current-time} (set (keys (:basis before-doc)))))
    (is (string? (get-in after-watermark [:basis :snapshot-token])))
    (is (nil? (:basis submit)))))

(deftest ledger-window-bad-before-watermark-fails-before-execution-test
  (let [req (request "window-bad-before")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        real-query xt/q
        invalid-rows [nil {} [nil] [{}] [{:xt/id -1}]
                      [{:xt/id "1"}] [{:xt/id 1.2}]
                      [{:xt/id 0} {:xt/id 1}]]]
    (doseq [rows invalid-rows]
      (testing (pr-str rows)
        (let [submissions (atom 0)]
          (with-redefs
            [xt/q (fn [& args]
                    (if (= before-watermark-sql (second args))
                      rows
                      (apply real-query args)))
             xt/execute-tx (fn [& _]
                             (swap! submissions inc)
                             (throw (ex-info "unexpected submission" {})))]
            (is (= ::collector/invalid-before-ledger-watermark
                   (error-type #(collect-window! action [rt]))))
            (is (zero? @submissions))))))
    (is (nil? (physical-document :requests (:xt/id req))))))

(deftest ledger-window-bad-after-watermark-fails-after-single-commit-test
  (let [req (request "window-bad-after")
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-submit xt/execute-tx
        watermark-count (atom 0)
        submits (atom 0)
        caught (try
                 (with-redefs
                   [xt/q (fn [& args]
                           (if (= before-watermark-sql (second args))
                             (if (= 2 (swap! watermark-count inc))
                               [{:xt/id "not-a-number"}]
                               (apply real-query args))
                             (apply real-query args)))
                    xt/execute-tx (fn [& args]
                                    (when (= 2 (count args)) (swap! submits inc))
                                    (apply real-submit args))]
                   (collect-window! (plan [(command/create :requests req version)]) [rt]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= ::collector/invalid-after-ledger-watermark
           (:error/type (ex-data caught))))
    (is (= 2 @watermark-count))
    (is (= 1 @submits) "Missing post-commit evidence must not resubmit.")
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest ledger-window-falsified-before-id-gap-breaks-correspondence-test
  (let [req (seed! :requests (request "window-false-before"))
        rt [:requests (:xt/id req)]
        original-query xt/q
        watermarks (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [rows (apply original-query args)]
                           (if (= before-watermark-sql (second args))
                             (if (= 1 (swap! watermarks inc))
                               ;; Fabricate only the PREDECESSOR: the actual
                               ;; after-ledger query must remain independent.
                               [{:xt/id (- (or (:xt/id (first rows)) 0) 1)}]
                               rows)
                             rows)))]
                 (collect-window! (plan [(claim req "window-false-before-helper")]) [rt]))
        checks (get-in report [:comparison :checks])]
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (:executed-tx-after-before-watermark checks)))
    (is (false? (:executed-tx-immediate-successor checks)))
    (is (true? (:executed-tx-equals-after-watermark checks)))
    (is (true? (:transaction-ledger-matches? report)))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (= "window-false-before-helper"
           (:owner (physical-document :requests (:xt/id req)))))))

(deftest ledger-window-falsified-after-watermark-breaks-correspondence-test
  (let [req (seed! :requests (request "window-false-after"))
        rt [:requests (:xt/id req)]
        real-query xt/q
        query-count (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (if (= before-watermark-sql (second args))
                           (let [i (swap! query-count inc)
                                 rows (apply real-query args)]
                             (if (= 2 i)
                               [{:xt/id (inc (:xt/id (first rows)))}]
                               rows))
                           (apply real-query args)))]
                 (collect-window! (plan [(claim req "window-false-after-helper")]) [rt]))
        checks (get-in report [:comparison :checks])]
    (is (= 2 @query-count))
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (:executed-tx-immediate-successor checks)))
    (is (false? (:executed-tx-equals-after-watermark checks)))
    (is (true? (:poststate checks)))
    (is (false? (get-in report [:comparison :corresponds?])))))

(deftest ledger-window-unrelated-writer-before-submit-is-detected-test
  (let [req (request "window-writer-before")
        unrelated (membership "window-unrelated-before")
        rt [:requests (:xt/id req)]
        real-submit xt/execute-tx
        injected (atom false)
        report (with-redefs
                 [xt/execute-tx
                  (fn [& args]
                    (when (and (= 2 (count args))
                               (compare-and-set! injected false true))
                      (real-submit (node) [[:put-docs :memberships unrelated]]))
                    (apply real-submit args))]
                 (collect-window! (plan [(command/create :requests req version)]) [rt]))
        checks (get-in report [:comparison :checks])]
    (is @injected)
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (:poststate checks)))
    (is (true? (:executed-tx-after-before-watermark checks)))
    (is (false? (:executed-tx-immediate-successor checks)))
    (is (true? (:executed-tx-equals-after-watermark checks)))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (= unrelated (physical-document :memberships (:xt/id unrelated))))))

(deftest ledger-window-unrelated-writer-after-submit-is-detected-test
  (let [req (request "window-writer-after")
        unrelated (membership "window-unrelated-after")
        rt [:requests (:xt/id req)]
        original-query xt/q
        real-submit xt/execute-tx
        seen-bases (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         ;; Only direct two-argument snapshot-token requests
                         ;; count. Internal XTDB queries may use a different
                         ;; arity and must not trigger the injection.
                         (when (and (= 2 (count args))
                                    (= "SHOW SNAPSHOT_TOKEN" (second args))
                                    (= 2 (swap! seen-bases inc)))
                           (real-submit (node) [[:put-docs :memberships unrelated]]))
                         (apply original-query args))]
                 (collect-window! (plan [(command/create :requests req version)]) [rt]))
        checks (get-in report [:comparison :checks])]
    (is (= 2 @seen-bases))
    (is (= :committed (get-in report [:execution :status])))
    (is (true? (:poststate checks)))
    (is (true? (:executed-tx-immediate-successor checks)))
    (is (false? (:executed-tx-equals-after-watermark checks)))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (= unrelated (physical-document :memberships (:xt/id unrelated))))))

(deftest ledger-window-input-shape-rejects-claimed-evidence-before-io-test
  (let [req (request "window-claimed-evidence")
        rt [:requests (:xt/id req)]
        base {:node (node) :plan (plan [(command/create :requests req version)])
              :targets [rt]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :read) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (doseq [field [:before-ledger-watermark :after-ledger-watermark
                     :tx-key :recorded-tx-ops :execution :observed-before]]
        (is (= ::collector/invalid-input
               (error-type #(collector/observe-and-compare-ledger-window!
                             (assoc base field :untrusted)))))
        (is (empty? @calls))))))

;; v1017: Repeated physical observations are checked only at the EXACT
;; originally acquired {:snapshot-token ... :current-time ...} pair. No
;; production receipt or transaction-internal read basis is inferred from a
;; passing replay, and none of these tests supplies production evidence.

(def ^:private repeat-check-keys
  #{:before-documents-repeat-at-pinned-basis
    :after-documents-repeat-at-pinned-basis
    :before-ledger-watermark-repeats-at-pinned-basis
    :after-ledger-watermark-repeats-at-pinned-basis})

(defn- collect-repeatable! [a-plan targets]
  (collector/observe-and-compare-repeatable-window!
   {:node (node) :plan a-plan :targets targets}))

(defn- check-repeatable-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (get-in report [:comparison :corresponds?]))
      (pr-str (get-in report [:comparison :checks])))
  (is (every? true? (vals (select-keys (get-in report [:comparison :checks])
                                      (into window-check-keys repeat-check-keys)))))
  (is (= :direct-controlled-xtdb-repeatable-ledger-window-observation
         (:evidence-kind report)))
  (is (= :gesso.model.tx/direct-xtdb-repeatable-ledger-window-v1
         (get-in report [:collection-scope :collector])))
  (is (true? (:transaction-ledger-matches? report)))
  (is (integer? (:physical-tx-id report)))
  (is (false? (:certifies-physical-execution? report)))
  (is (false? (get-in report [:comparison :certifies-physical-execution?])))
  (is (not-any? #(contains? report %)
                [:node :plan :tx-key :snapshot-token :system-time
                 :recorded-tx-ops :observed-before :observed-after
                 :reference-before :before-ledger-watermark
                 :after-ledger-watermark]))
  report)

(deftest repeatable-window-scope-explicitly-excludes-general-refinement-test
  (let [{:keys [collector collected assumed not-established]}
        collector/repeatable-ledger-window-collection-scope]
    (is (= :gesso.model.tx/direct-xtdb-repeatable-ledger-window-v1 collector))
    (doseq [fact [:before-document-replay-at-identical-basis
                 :after-document-replay-at-identical-basis
                 :before-ledger-watermark-replay-at-identical-basis
                 :after-ledger-watermark-replay-at-identical-basis]]
      (is (contains? collected fact)))
    (is (contains? assumed :exclusive-control-of-tracked-documents-during-collection))
    (doseq [not-proven [:transaction-internal-read-basis-attested
                       :all-possible-replays-are-repeatable
                       :concurrent-writers-are-excluded
                       :retroactive-basis-clock-rebinding-safe
                       :general-xtdb-step-refinement
                       :authenticated-physical-submission-receipt]]
      (is (contains? not-established not-proven))
      (is (not (contains? collected not-proven))))))

(deftest repeatable-window-commit-and-stale-retry-correspond-test
  (let [req (seed! :requests (request "repeatable-claim"))
        rt [:requests (:xt/id req)]
        action (plan [(claim req "repeatable-helper")])
        accepted (check-repeatable-report! (collect-repeatable! action [rt]) :committed)
        rejected (check-repeatable-report! (collect-repeatable! action [rt]) :rejected)]
    (is (< (:physical-tx-id accepted) (:physical-tx-id rejected)))
    (is (= :xtdb/assert-failed (get-in rejected [:execution :error/code])))
    (is (= :command-version
           (get-in rejected [:comparison :failed-precondition-kind])))
    (is (= "repeatable-helper" (:owner (physical-document :requests (:xt/id req)))))))

(deftest repeatable-window-create-and-duplicate-rejection-correspond-test
  (let [req (request "repeatable-create")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        accepted (check-repeatable-report! (collect-repeatable! action [rt]) :committed)
        rejected (check-repeatable-report! (collect-repeatable! action [rt]) :rejected)]
    (is (< (:physical-tx-id accepted) (:physical-tx-id rejected)))
    (is (= :document-absent
           (get-in rejected [:comparison :failed-precondition-kind])))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest repeatable-window-reuses-both-complete-phase-bases-test
  (let [req (request "repeatable-bases")
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-execute xt/execute-tx
        basis-calls (atom [])
        document-calls (atom [])
        watermark-calls (atom [])
        submit-count (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (when (= 2 (count args))
                           (when (= "SHOW SNAPSHOT_TOKEN" (second args))
                             (swap! basis-calls conj :captured)))
                         (when (= 3 (count args))
                           (let [q (second args)
                                 basis (nth args 2)]
                             (when (and (vector? q)
                                        (string? (first q))
                                        (.startsWith ^String (first q) "SELECT * FROM requests"))
                               (swap! document-calls conj basis))
                             (when (= before-watermark-sql q)
                               (swap! watermark-calls conj basis))))
                         (apply real-query args))
                  xt/execute-tx (fn [& args]
                                  (when (= 2 (count args))
                                    (swap! submit-count inc))
                                  (apply real-execute args))]
                 (collect-repeatable! (plan [(command/create :requests req version)]) [rt]))
        [before-doc after-doc before-repeat after-repeat] @document-calls
        [before-mark after-mark before-mark-repeat after-mark-repeat] @watermark-calls]
    (check-repeatable-report! report :committed)
    (is (= 2 (count @basis-calls)) "Replays must not acquire newer phase bases.")
    (is (= 1 @submit-count))
    (is (= 4 (count @document-calls)))
    (is (= 4 (count @watermark-calls)))
    (is (= before-doc before-repeat before-mark before-mark-repeat))
    (is (= after-doc after-repeat after-mark after-mark-repeat))
    (is (not= before-doc after-doc))
    (is (= #{:snapshot-token :current-time} (set (keys before-doc))))
    (is (= #{:snapshot-token :current-time} (set (keys after-doc))))
    (is (string? (:snapshot-token before-doc)))
    (is (string? (:snapshot-token after-doc)))
    (is (:current-time before-doc))
    (is (:current-time after-doc))))

(deftest repeatable-window-detects-falsified-document-replays-without-retry-test
  (doseq [[phase falsified-n] [[:before 3] [:after 4]]]
    (testing (name phase)
      (let [req (request (str "repeatable-forged-" (name phase)))
            rt [:requests (:xt/id req)]
            real-query xt/q
            real-execute xt/execute-tx
            reads (atom 0)
            submissions (atom 0)
            report (with-redefs
                     [xt/q (fn [& args]
                             (let [query (second args)
                                   sql (if (vector? query) (first query) query)]
                               (if (and (= 3 (count args))
                                        (string? sql)
                                        (.startsWith ^String sql "SELECT * FROM requests"))
                                 (if (= falsified-n (swap! reads inc))
                                   [{:xt/id (:xt/id req) :revision 999 :created-at "fabricated"}]
                                   (apply real-query args))
                                 (apply real-query args))))
                      xt/execute-tx (fn [& args]
                                      (when (= 2 (count args))
                                        (swap! submissions inc))
                                      (apply real-execute args))]
                     (collect-repeatable! (plan [(command/create :requests req version)]) [rt]))
            checks (get-in report [:comparison :checks])]
        (is (= 4 @reads))
        (is (= 1 @submissions))
        (is (= :committed (get-in report [:execution :status])))
        (is (= (not= phase :before)
               (:before-documents-repeat-at-pinned-basis checks)))
        (is (= (not= phase :after)
               (:after-documents-repeat-at-pinned-basis checks)))
        (is (every? true? (vals (select-keys checks window-check-keys))))
        (is (false? (get-in report [:comparison :corresponds?])))
        (is (false? (:certifies-physical-execution? report)))
        (is (= req (physical-document :requests (:xt/id req))))))))

(deftest repeatable-window-detects-falsified-ledger-replay-without-retry-test
  (doseq [[phase falsified-n check-key]
          [[:before 3 :before-ledger-watermark-repeats-at-pinned-basis]
           [:after 4 :after-ledger-watermark-repeats-at-pinned-basis]]]
    (testing (name phase)
      (let [req (request (str "repeatable-watermark-" (name phase)))
            rt [:requests (:xt/id req)]
            real-query xt/q
            real-execute xt/execute-tx
            calls (atom 0)
            submissions (atom 0)
            report (with-redefs
                     [xt/q (fn [& args]
                             (let [rows (apply real-query args)]
                               (if (= before-watermark-sql (second args))
                                 (if (= falsified-n (swap! calls inc))
                                   [{:xt/id (+ 2 (:xt/id (first rows)))}]
                                   rows)
                                 rows)))
                      xt/execute-tx (fn [& args]
                                      (when (= 2 (count args))
                                        (swap! submissions inc))
                                      (apply real-execute args))]
                     (collect-repeatable! (plan [(command/create :requests req version)]) [rt]))
            checks (get-in report [:comparison :checks])]
        (is (= 4 @calls))
        (is (= 1 @submissions))
        (is (true? (:before-documents-repeat-at-pinned-basis checks)))
        (is (true? (:after-documents-repeat-at-pinned-basis checks)))
        (is (false? (get checks check-key)))
        (is (true? (get checks (if (= phase :before)
                                 :after-ledger-watermark-repeats-at-pinned-basis
                                 :before-ledger-watermark-repeats-at-pinned-basis))))
        (is (false? (get-in report [:comparison :corresponds?])))
        (is (= req (physical-document :requests (:xt/id req))))))))

(deftest repeatable-window-malformed-before-ledger-replay-never-resubmits-test
  (let [req (request "repeatable-malformed-replay")
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-execute xt/execute-tx
        watermark-reads (atom 0)
        submissions (atom 0)
        thrown (try
                 (with-redefs
                   [xt/q (fn [& args]
                           (if (and (= before-watermark-sql (second args))
                                    (= 3 (swap! watermark-reads inc)))
                             [{:xt/id "not-an-id"}]
                             (apply real-query args)))
                    xt/execute-tx (fn [& args]
                                    (when (= 2 (count args))
                                      (swap! submissions inc))
                                    (apply real-execute args))]
                   (collect-repeatable! (plan [(command/create :requests req version)]) [rt]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= ::collector/invalid-before-ledger-watermark
           (:error/type (ex-data thrown))))
    (is (= 3 @watermark-reads))
    (is (= 1 @submissions))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest repeatable-window-rejects-caller-evidence-before-any-physical-io-test
  (let [req (request "repeatable-reject-evidence")
        base {:node (node)
              :plan (plan [(command/create :requests req version)])
              :targets [[:requests (:xt/id req)]]}
        calls (atom [])]
    (with-redefs [xt/q (fn [& _] (swap! calls conj :query) nil)
                  xt/execute-tx (fn [& _] (swap! calls conj :submit) nil)]
      (doseq [extra [:observed-before :observed-after :reference-before
                     :before-ledger-watermark :after-ledger-watermark
                     :tx-key :recorded-tx-ops :execution :snapshot-token
                     :current-time]]
        (is (= ::collector/invalid-input
               (error-type #(collector/observe-and-compare-repeatable-window!
                             (assoc base extra :forged)))))
        (is (empty? @calls))))))


;; v1028: conditional argument and controlled physical observation are obtained
;; within the SAME collector execution. These are assertions about one bounded
;; diagnostic, not a backend refinement theorem or a production receipt.

(defn- collect-composed! [a-plan targets]
  (collector/observe-and-derive-conditional!
   {:node (node) :plan a-plan :targets targets}))

(defn- check-composed-report! [report status]
  (is (= status (get-in report [:execution :status])))
  (is (true? (:conditional-argument-admissible? (:conditional-step report))))
  (is (= status (get-in report [:conditional-step :abstract-status])))
  (is (= :conditional-only
         (get-in report [:conditional-step :physical-refinement :status])))
  (is (true? (get-in report [:comparison :corresponds?])))
  (is (= :locally-valid-and-observationally-consistent
         (get-in report [:composed-assurance :status])))
  (is (true? (get-in report [:composed-assurance :local-argument-valid?])))
  (is (true? (get-in report [:composed-assurance :physical-observations-correspond?])))
  (is (true? (get-in report [:composed-assurance :joint-evidence-consistent?])))
  (is (= 0 (get-in report [:comparison :prestate-mismatch-count])))
  (is (= 0 (get-in report [:comparison :poststate-mismatch-count])))
  (is (not (contains? (:comparison report) :prestate-mismatches)))
  (is (not (contains? (:comparison report) :poststate-mismatches)))
  (is (false? (:certifies-physical-execution? report)))
  (doseq [flag [:certifies-physical-execution?
                :proves-internal-prestate?
                :proves-general-refinement?]]
    (is (false? (get-in report [:composed-assurance flag]))))
  report)

(deftest composed-conditional-scope-retains-all-backend-premises-test
  (let [{:keys [composition checked undischarged-premises not-established]}
        collector/composed-step-scope]
    (is (= :gesso.model.tx/collected-conditional-step-v1 composition))
    (is (contains? checked
                   :finite-prestate-derived-from-same-physical-before-observation))
    (is (contains? checked
                   :local-lowering-and-predicate-argument-valid-before-submission))
    (is (contains? checked
                   :separate-local-and-observed-correspondence-outcomes))
    (is (contains? undischarged-premises
                   :concrete-transaction-internal-prestate-relates-to-abstract-prestate))
    (doseq [unproved [:transaction-internal-prestate-attested
                     :universal-physical-to-abstract-refinement
                     :backend-premises-discharged-by-observational-agreement]]
      (is (contains? not-established unproved))
      (is (not (contains? checked unproved))))))

(deftest composed-create-and-rejected-duplicate-share-collector-state-test
  (let [req (request "composed-private-create")
        id (:xt/id req)
        action (plan [(command/create :requests req version)])
        rt [:requests id]
        committed (check-composed-report! (collect-composed! action [rt]) :committed)
        rejected (check-composed-report! (collect-composed! action [rt]) :rejected)]
    (is (= :document-absent
           (get-in rejected [:comparison :failed-precondition-kind])))
    (is (= 1 (get-in committed [:conditional-step :abstract-write-count])))
    (is (nil? (get-in committed [:conditional-step :first-failed-assertion-index])))
    (is (= 0 (get-in rejected [:conditional-step :first-failed-assertion-index])))
    (is (= :xtdb/assert-failed (get-in rejected [:execution :error/code])))
    (is (= req (physical-document :requests id)))
    (doseq [report [committed rejected]]
      (is (not (re-find #"composed-private-create" (pr-str report))))
      (is (contains? (get-in report [:composed-assurance :undischarged-premises])
                     :concrete-transaction-internal-prestate-relates-to-abstract-prestate)))))

(deftest composed-authoritative-version-and-dependency-rejections-test
  (let [req (seed! :requests (request "composed-claim"))
        member (seed! :memberships (membership "composed-membership"))
        rt [:requests (:xt/id req)]
        mt [:memberships (:xt/id member)]
        action (plan [(claim req "owner-composed")]
                     (command/guard :memberships member version))
        committed (check-composed-report! (collect-composed! action [rt mt]) :committed)
        stale (check-composed-report! (collect-composed! action [rt mt]) :rejected)]
    (is (= :command-version
           (get-in stale [:comparison :failed-precondition-kind])))
    (is (= "owner-composed" (:owner (physical-document :requests (:xt/id req)))))
    (is (= 2 (:collected-target-count committed)))
    (is (= 2 (:collected-target-count stale)))))

(deftest composed-invalid-local-argument-stops-before-submission-test
  (let [req (request "composed-local-blocked")
        rt [:requests (:xt/id req)]
        action (plan [(command/create :requests req version)])
        actual prestate/derive-conditional-step
        submitted (atom 0)
        thrown (try
                 (with-redefs
                   [prestate/derive-conditional-step
                    (fn [state p]
                      (assoc (actual state p)
                             :conditional-argument-admissible? false
                             :failed-local-checks [:deliberately-falsified-local-check]))
                    xt/execute-tx (fn [& _]
                                    (swap! submitted inc)
                                    (throw (ex-info "Unexpected submission" {})))]
                   (collect-composed! action [rt]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= ::collector/invalid-local-argument
           (:error/type (ex-data thrown))))
    (is (= [:deliberately-falsified-local-check]
           (get-in (ex-data thrown) [:error/details :failed-checks])))
    (is (= 0 @submitted))
    (is (nil? (physical-document :requests (:xt/id req))))))

(deftest composed-rejects-caller-fabricated-evidence-before-io-test
  (let [req (request "composed-injected-evidence")
        input {:node (node)
               :plan (plan [(command/create :requests req version)])
               :targets [[:requests (:xt/id req)]]}
        io (atom [])]
    (with-redefs
      [xt/q (fn [& _] (swap! io conj :query) nil)
       xt/execute-tx (fn [& _] (swap! io conj :execute) nil)]
      (doseq [extra [:reference-before :observed-before :observed-after
                     :conditional-step :execution :recorded-tx-ops
                     :snapshot-token :before-ledger-watermark]]
        (is (= ::collector/invalid-input
               (error-type #(collector/observe-and-derive-conditional!
                             (assoc input extra :forged)))))
        (is (empty? @io))))))

(deftest composed-falsified-replayed-document-suppresses-joint-evidence-test
  (doseq [[phase injection-read] [[:before 3] [:after 4]]]
    (testing (name phase)
      (let [req (request (str "composed-private-replay-" (name phase)))
            rt [:requests (:xt/id req)]
            actual-query xt/q
            actual-execute xt/execute-tx
            reads (atom 0)
            submits (atom 0)
            report (with-redefs
                     [xt/q (fn [& args]
                             (let [q (second args)
                                   sql (if (vector? q) (first q) q)]
                               (if (and (= 3 (count args))
                                        (string? sql)
                                        (.startsWith ^String sql "SELECT * FROM requests"))
                                 (if (= injection-read (swap! reads inc))
                                   [{:xt/id (:xt/id req) :revision 999
                                     :created-at "secret-fabricated-observation"}]
                                   (apply actual-query args))
                                 (apply actual-query args))))
                      xt/execute-tx (fn [& args]
                                      (when (= 2 (count args))
                                        (swap! submits inc))
                                      (apply actual-execute args))]
                     (collect-composed!
                      (plan [(command/create :requests req version)]) [rt]))
            joint (:composed-assurance report)]
        (is (= 4 @reads))
        (is (= 1 @submits))
        (is (= :committed (get-in report [:execution :status])))
        (is (true? (:conditional-argument-admissible? (:conditional-step report))))
        (is (true? (:local-argument-valid? joint)))
        (is (false? (get-in report [:comparison :corresponds?])))
        (is (false? (:physical-observations-correspond? joint)))
        (is (false? (:joint-evidence-consistent? joint)))
        (is (= :observation-mismatch (:status joint)))
        (is (not (contains? (:comparison report) :prestate-mismatches)))
        (is (not (contains? (:comparison report) :poststate-mismatches)))
        (is (not (re-find #"composed-private-replay|secret-fabricated-observation"
                          (pr-str report))))
        (is (= req (physical-document :requests (:xt/id req))))))))

(deftest composed-falsified-ledger-replay-suppresses-joint-evidence-test
  (let [req (request "composed-private-watermark")
        rt [:requests (:xt/id req)]
        real-query xt/q
        real-execute xt/execute-tx
        watermark-reads (atom 0)
        submissions (atom 0)
        report (with-redefs
                 [xt/q (fn [& args]
                         (let [rows (apply real-query args)]
                           (if (= before-watermark-sql (second args))
                             (if (= 4 (swap! watermark-reads inc))
                               [{:xt/id (+ 2 (:xt/id (first rows)))}]
                               rows)
                             rows)))
                  xt/execute-tx (fn [& args]
                                  (when (= 2 (count args))
                                    (swap! submissions inc))
                                  (apply real-execute args))]
                 (collect-composed! (plan [(command/create :requests req version)]) [rt]))]
    (is (= 4 @watermark-reads))
    (is (= 1 @submissions))
    (is (true? (:conditional-argument-admissible? (:conditional-step report))))
    (is (= :committed (get-in report [:execution :status])))
    (is (false? (get-in report [:comparison :corresponds?])))
    (is (false? (get-in report [:composed-assurance :joint-evidence-consistent?])))
    (is (= :observation-mismatch (get-in report [:composed-assurance :status])))
    (is (false? (get-in report [:comparison :checks
                                :after-ledger-watermark-repeats-at-pinned-basis])))
    (is (= req (physical-document :requests (:xt/id req))))))

(deftest composed-unknown-outcome-is-never-reclassified-or-retried-test
  (let [req (request "composed-unknown")
        actual-execute xt/execute-tx
        submits (atom 0)
        thrown (try
                 (with-redefs
                   [xt/execute-tx (fn [& args]
                                    (if (= 2 (count args))
                                      (do (swap! submits inc)
                                          (throw (ex-info "Simulated indeterminate transport" 
                                                          {:error/type :indeterminate})))
                                      (apply actual-execute args)))]
                   (collect-composed!
                    (plan [(command/create :requests req version)])
                    [[:requests (:xt/id req)]]))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= 1 @submits))
    (is (= :indeterminate (:error/type (ex-data thrown))))
    (is (nil? (physical-document :requests (:xt/id req))))))

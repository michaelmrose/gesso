(ns gesso.assurance.example.physical-single-node-outcome-correspondence
  "v1499: Observe ONE real v1494 operation and compare its reported XTDB
   transaction outcome to a fresh external physical readback and the ORIGINAL
   typed reservation operation/invariants. The physical->abstract translation
   remains an unproved application-owned mapping.

   The operation is invoked exactly once, without diagnostic retry. Exceptions
   propagate unchanged. All reads use the application's caller-supplied node;
   neither the returned transaction key nor an agreeing poststate attests that
   this attempted operation caused the write. Concurrent writers, the node's
   identity, transaction-internal prestate, and the execute! callback's
   provenance are NOT authenticated. Never use this diagnostic to certify a
   commit, authorize a browser settlement, or declare physical refinement."
  (:require [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.typed-presence-semantics :as typed]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

(def report-type
  :gesso.assurance.example.physical-single-node-outcome-correspondence/bounded-attempt)
(def report-version 1)

(def ^:private exact-document-keys
  #{:xt/id :revision :created-at :updated-at :status :claims :audit-tag})
(def ^:private expected-writers
  #{:initializer/bootstrap :operation/reserve :operation/release})
(def ^:private open-obligations
  #{:actual-execute-callback-and-submission-not-source-authenticated
    :reported-transaction-key-and-commit-not-independently-authenticated
    :physical-read-node-and-read-timing-not-authenticated
    :concurrent-external-writer-interference-not-excluded
    :physical-to-abstract-mapping-not-independently-proved
    :issued-certified-executor-and-installed-route-not-established
    :complete-writer-coverage-not-established
    :transaction-internal-prestate-not-authenticated
    :universal-physical-refinement-not-established})

;; Capture the source seams; ordinary redefinition must not turn this report
;; into a source-backed claim about unrelated computations. This is not a JVM
;; integrity or trusted-deployment guarantee.
(def ^:private original-catalog pilot/reservation-catalog)
(def ^:private original-projection physical/physical->abstract)
(def ^:private original-query xt/q)
(def ^:private original-evaluate typed/evaluate-expression!)

(defn- reject! [reason]
  (throw (ex-info "Invalid physical operation outcome observation."
                  {:physical-single-node-outcome-correspondence/rejection true
                   :reason reason})))

(defn- check-seams! []
  (when-not (and (identical? original-catalog
                             (var-get #'pilot/reservation-catalog))
                 (identical? original-projection
                             (var-get #'physical/physical->abstract))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-evaluate
                             (var-get #'typed/evaluate-expression!)))
    (reject! :substituted-source-or-physical-read-seam)))

(defn- checked-source! [app operation-id]
  (check-seams!)
  (let [catalog (original-catalog)
        entries (:operations catalog)
        operation (first (filter #(= operation-id (:id %)) entries))]
    (when-not (and (map? app)
                   (some? (:node app))
                   (identical? (:node app)
                               (get-in app [:biff-context :biff.xtdb/node]))
                   (string? (:slot-id app))
                   (re-matches #"[A-Za-z0-9_-]{1,128}" (:slot-id app))
                   (fn? (:execute! app))
                   (= catalog (:catalog app))
                   (= :slot/reservation-lifecycle (:id catalog))
                   (= physical/operation-ids (:declared-operation-ids catalog))
                   (= physical/operation-ids (set (map :id entries)))
                   (= 2 (count entries))
                   (contains? physical/operation-ids operation-id)
                   (map? operation)
                   (= operation-id
                      (get-in operation [:construction :subject :id]))
                   (= physical/operation-ids
                      (get-in app [:report :operation-ids]))
                   (= expected-writers
                      (get-in app [:report :declared-example-writer-paths]))
                   (= :not-established
                      (get-in app [:report :observed-writer-completeness]))
                   (= :not-issued (get-in app [:report :certificate]))
                   (= :none (get-in app [:report :authority])))
      (reject! :invalid-application-operation-or-source))
    (let [construction (:construction operation)
          replacements (:replacements construction)
          state-fields (set (keys (:state-types construction)))]
      (when-not (and (= #{:slot/status :slot/active-claims :slot/audit-tag}
                        state-fields)
                     (= #{:slot/status} (get-in construction [:guard :readers]))
                     (= #{:slot/status :slot/active-claims}
                        (:writes construction))
                     (= #{:slot/audit-tag} (:frame construction))
                     (= #{:slot/status :slot/active-claims}
                        (set (keys replacements)))
                     (every? (fn [[_ expr]]
                               (and (vector? expr) (= 2 (count expr))
                                    (= :literal (first expr))))
                             replacements)
                     (= #{:slot/status :slot/active-claims}
                        (into #{} (mapcat (fn [[_ rule]] (:readers rule))
                                          (:invariants catalog)))))
        (reject! :unsupported-typed-operation))
      {:catalog catalog :construction construction})))

(defn- physical-row! [app]
  ;; No caller-supplied :read! or :abstract! hook is used. Nevertheless the
  ;; node is supplied by app, so read-origin authenticity remains open.
  (let [rows (original-query (:node app)
                             ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ? LIMIT 2"
                              (:slot-id app)])]
    (when-not (and (sequential? rows) (= 1 (count rows))
                   (map? (first rows)))
      (reject! :missing-or-ambiguous-physical-readback))
    (let [row (first rows)]
      (when-not (and (= exact-document-keys (set (keys row)))
                     (= (:slot-id app) (:xt/id row))
                     (integer? (:revision row))
                     (<= 0 (:revision row))
                     (string? (:created-at row))
                     (string? (:updated-at row)))
        (reject! :unsupported-physical-readback))
      row)))

(defn- rule-holds? [rule state types]
  (when-not (and (map? rule) (set? (:readers rule))
                 (every? (set (keys types)) (:readers rule)))
    (reject! :unsupported-declared-rule))
  (let [result (original-evaluate (:expression rule)
                                  (select-keys types (:readers rule))
                                  (select-keys state (:readers rule)))]
    (when-not (and (= :not-proved (:proof-status result))
                   (= :not-admitted (:admission result))
                   (boolean? (:value result)))
      (reject! :invalid-typed-rule-evaluation))
    (:value result)))

(defn- valid-invariants! [catalog types state]
  (when-not (and (= #{:slot/status-domain :slot/nonnegative-claims
                      :slot/at-most-one-claim}
                    (set (keys (:invariants catalog))))
                 (every? #(rule-holds? % state types)
                         (vals (:invariants catalog))))
    (reject! :observed-state-violates-declared-invariant)))

(defn- expected-after! [{:keys [catalog construction]} before]
  (let [types (:state-types construction)
        state (original-projection before)
        _ (valid-invariants! catalog types state)
        _ (when-not (rule-holds? (:guard construction) state types)
            (reject! :abstract-operation-guard-does-not-hold))
        expected-state (reduce-kv
                        (fn [s field [_ value]] (assoc s field value))
                        state (:replacements construction))
        _ (valid-invariants! catalog types expected-state)
        _ (when-not (= (:slot/audit-tag state)
                       (:slot/audit-tag expected-state))
            (reject! :abstract-operation-frame-changed))
        expected-row (-> before
                         (assoc :status (name (:slot/status expected-state))
                                :claims (:slot/active-claims expected-state)
                                :revision (inc (:revision before))
                                :updated-at (str "v1494-revision-"
                                                 (inc (:revision before)))))]
    {:abstract expected-state :physical expected-row}))

(defn observe-operation!
  "Observe one :slot/reserve or :slot/release on the real v1494 fixture.

   The original abstract catalog and its typed guard/invariants determine
   the expected transition. Read the node before, invoke :execute! ONCE, and
   read the node again. A matching reported-committed outcome is only a
   bounded agreement of a caller-origin result and an external readback.
   Noncommitted returns never count as success. Exceptions propagate without
   retries or a false reconciliation. No sensitive physical rows are returned."
  [app operation-id]
  (let [source (checked-source! app operation-id)
        before (physical-row! app)
        expected (expected-after! source before)
        outcome ((:execute! app) operation-id)
        after (physical-row! app)
        actual-abstract (original-projection after)
        source-types (get-in source [:construction :state-types])
        _ (valid-invariants! (:catalog source) source-types actual-abstract)
        physical-match? (= (:physical expected) after)
        abstract-match? (= (:abstract expected) actual-abstract)
        reported-committed? (= :committed (:commit/status outcome))
        tx-key? (instance? TransactionKey (:tx-result outcome))
        agreement? (and reported-committed? tx-key?
                        physical-match? abstract-match?)]
    {:type report-type
     :version report-version
     :catalog-id :slot/reservation-lifecycle
     :operation-id operation-id
     :reported-commit-status (if reported-committed?
                               :committed :nondefinitive)
     :public-transaction-key-shape? tx-key?
     :observed-before-revision (:revision before)
     :observed-after-revision (:revision after)
     :abstract-guard-checked? true
     :declared-invariants-evaluated? true
     :reported-outcome-and-observed-poststate-agree? agreement?
     :physical-poststate-matches-declared-transition? physical-match?
     :abstract-poststate-matches-declared-transition? abstract-match?
     :status (if agreement?
               :bounded-reported-commit-and-external-poststate-agreement
               :outcome-or-poststate-correspondence-not-established)
     :scope :one-caller-node-one-attempt-two-external-reads
     :checking :original-typed-operation-and-independent-physical-readback
     :proof-status :not-proved
     :physical-refinement :not-established
     :authenticated-commit :not-established
     :complete-writer-coverage :not-established
     :open-obligations open-obligations
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(ns gesso.model.command-resolution-test
  "Permanent bounded regressions for the v789 durable command-resolution
   construction. These tests distinguish three separate obligations:

   * Pure admission constructs a closed command binding and adds one reserved
     record CREATE to the SAME normalized Gesso model transaction plan.
   * Reconciliation may disclose an outcome only after trusted authorization
     and exact matching of the complete command binding; absence is unknown.
   * Embedded XTDB executes the resulting plan atomically for the exercised
     records and guarded domain mutations, including duplicate and stale
     transaction rejection.

   This is execution evidence for the admitted examples, not a proof of
   HumanHelp authorization, comprehensive dependency guards, record
   immutability against other writers, uncertain-delivery liveness, or
   universal XTDB linearizability. The disclosure predicate in these tests
   represents a trusted application decision; this namespace does not infer
   such permission from a principal's tagged data representation."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private domain-version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private initial-time "t0")
(def ^:private commit-time "t1")
(def ^:private allowed (fn [_ _] true))
(def ^:private denied (fn [_ _] false))

(defn- unique-id [prefix]
  (str prefix "-" (random-uuid)))

(defn- binding-for
  ([command-id request-id]
   (binding-for command-id request-id "helper-a"))
  ([command-id request-id principal]
   {:command-id (identity/command-id command-id)
    :principal (identity/principal principal)
    :operation :request/claim
    :arguments {:request-id request-id}}))

(defn- request-document [request-id]
  {:xt/id request-id
   :revision 0
   :created-at initial-time
   :updated-at initial-time
   :owner "unclaimed"})

(defn- membership-document [member-id]
  {:xt/id member-id
   :revision 0
   :created-at initial-time
   :updated-at initial-time
   :active true})

(defn- updated [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at commit-time)))

(defn- claim-command [before owner]
  (command/update-command :requests :claim before
                          (updated before {:owner owner})
                          domain-version))

(defn- revoke-command [before]
  (command/update-command :memberships :revoke before
                          (updated before {:active false})
                          domain-version))

(defn- domain-plan
  ([request]
   (domain-plan request nil))
  ([request membership]
   {:commands [(claim-command request "helper-a")]
    :guards (if membership
              [(command/guard :memberships membership domain-version)]
              [])
    :emit false}))

(defn- incorporation
  ([binding request]
   (incorporation binding request nil))
  ([binding request membership]
   (resolution/incorporation-plan
    {:binding binding
     :outcome {:status :claimed :owner "helper-a"}
     :now commit-time
     :plan (domain-plan request membership)})))

(defn- error-kind [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (is (= ::resolution/invalid-resolution (:error/type (ex-data error)))
          (str "Expected a typed command-resolution error, got " (ex-data error)))
      (:error/kind (ex-data error)))))

(defn- recorded-outcome []
  {:status :claimed :owner "helper-a"})

(defn- exemplar []
  (let [request (request-document (unique-id "request"))
        binding (binding-for (unique-id "command") (:xt/id request))]
    {:request request
     :binding binding
     :record (resolution/command-record binding (recorded-outcome) commit-time)}))

(deftest assurance-scope-does-not-overstate-verification-test
  (let [scope resolution/assurance-scope]
    (is (= :gesso.model.command-resolution/atomic-record-v1
           (:construction scope)))
    (is (contains? (:checked scope) :atomic-record-create-in-model-plan))
    (is (contains? (:checked scope) :exact-binding-comparison-before-replay))
    (doseq [assumption [:authoritative-read
                        :atomic-xtdb-assert-and-write
                        :correct-domain-plan-and-outcome
                        :trusted-authentication-and-disclosure-predicate]]
      (is (contains? (:assumed scope) assumption)))
    (doseq [unknown [:humanhelp-business-authorization
                     :complete-domain-transaction-guards
                     :record-immutability-after-incorporation
                     :durable-notification-debt
                     :unknown-delivery-liveness
                     :unbounded-concurrent-linearizability]]
      (is (contains? (:not-established scope) unknown)))))

(deftest canonical-closed-binding-and-command-row-id-test
  (let [{:keys [binding]} (exemplar)
        uuid (random-uuid)
        uuid-binding (assoc binding :command-id (identity/command-id uuid))]
    (is (= binding (resolution/require-binding binding)))
    (is (= (identity/raw-value (:command-id binding))
           (resolution/record-id binding)))
    (is (= uuid (resolution/record-id uuid-binding)))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (dissoc binding :principal)))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (assoc binding :extra true)))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (assoc binding :command-id "plain")))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (assoc binding :principal "plain")))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (assoc binding :operation "claim")))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding (assoc binding :arguments [])))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding
                         (assoc binding :command-id (identity/command-id "  "))))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding
                         (assoc binding :command-id
                                (assoc (:command-id binding) :extra :forged))))))
    (is (= :invalid-binding
           (error-kind #(resolution/require-binding
                         (assoc binding :principal
                                (assoc (:principal binding) :extra :forged))))))))

(deftest arguments-fail-closed-for-nonportable-nested-values-test
  (let [{:keys [binding]} (exemplar)]
    (doseq [arguments [{:x (Object.)}
                       {:outer {:inner #{1}}}
                       {:outer [1 {:inner '(2 3)}]}
                       {:x Double/NaN}
                       {:x Double/POSITIVE_INFINITY}
                       {:x java.time.Instant/EPOCH}]]
      (is (= :invalid-binding
             (error-kind #(resolution/require-binding
                           (assoc binding :arguments arguments))))
          (str "Rejected arguments: " (pr-str arguments))))
    (let [nested {:nested [:a 1 {:uuid (random-uuid)}]}
          valid (assoc binding :arguments nested)]
      (is (= valid (resolution/require-binding valid))))))

(deftest command-record-shape-and-required-outcome-test
  (let [{:keys [binding record]} (exemplar)]
    (is (= record (resolution/require-record record)))
    (is (= (resolution/record-id binding) (:xt/id record)))
    (is (= 0 (:gesso.command/revision record)))
    (is (= :incorporated (:gesso.command/status record)))
    (is (= commit-time (:gesso.command/created-at record)
           (:gesso.command/updated-at record)))
    (is (= binding (:gesso.command/binding record)))
    (is (= (recorded-outcome) (:gesso.command/outcome record)))
    (doseq [outcome [nil {} [] {:x (Object.)}
                     {:x {:nested #{1}}} {:x Double/NEGATIVE_INFINITY}]]
      (is (= :invalid-outcome
             (error-kind #(resolution/command-record binding outcome commit-time)))))
    (is (= :missing-time
           (error-kind #(resolution/command-record binding (recorded-outcome) nil))))))

(deftest malformed-authoritative-record-fails-before-disclosure-test
  (let [{:keys [binding record]} (exemplar)
        mutations [(dissoc record :gesso.command/outcome)
                   (assoc record :extra :untrusted)
                   (assoc record :xt/id "different-command")
                   (assoc record :gesso.command/revision 1)
                   (assoc record :gesso.command/status :pending)
                   (assoc record :gesso.command/updated-at "different-time")
                   (assoc record :gesso.command/outcome {})
                   (assoc record :gesso.command/outcome {:x (Object.)})
                   (assoc record :gesso.command/binding
                          (assoc (:gesso.command/binding record) :operation "claim"))]]
    (doseq [bad mutations]
      (let [calls (atom 0)]
        (is (= :invalid-record
               (error-kind #(resolution/reconcile
                             binding bad
                             (fn [_ _] (swap! calls inc) true)))))
        (is (zero? @calls)
            "Malformed records must not be handed to a disclosure callback.")))))

(deftest atomic-plan-preserves-domain-plan-and-adds-one-record-create-test
  (let [{:keys [request binding]} (exemplar)
        membership (membership-document (unique-id "member"))
        domain (domain-plan request membership)
        combined (resolution/incorporation-plan
                  {:binding binding
                   :outcome (recorded-outcome)
                   :now commit-time
                   :plan domain})
        original (tx/normalize-plan domain)
        actual (:plan combined)
        last-command (last (:commands actual))]
    (is (= resolution/assurance-scope (:scope combined)))
    (is (= 1 (count (:commands original))))
    (is (= 2 (count (:commands actual))))
    (is (= (:commands original) (pop (:commands actual))))
    (is (= (:guards original) (:guards actual)))
    (is (= (:emit original) (:emit actual)))
    (is (= (:changes original) (:changes actual)))
    (is (= (resolution/record-id binding) (:model/id last-command)))
    (is (= resolution/command-entity-type (:model/entity-type last-command)))
    (is (= :create (:model/operation last-command)))
    (is (= (:record combined) (:model/after last-command)))
    (is (= 5 (count (:tx-ops (tx/prepare {} actual))))
        "One membership assertion, update assert/write, record absence assert/write.")))

(deftest incorporation-rejects-invalid-envelope-and-reserved-table-test
  (let [{:keys [request binding record]} (exemplar)
        base {:binding binding :outcome (recorded-outcome)
              :now commit-time :plan (domain-plan request)}]
    (is (= :invalid-incorporation
           (error-kind #(resolution/incorporation-plan (dissoc base :plan)))))
    (is (= :invalid-incorporation
           (error-kind #(resolution/incorporation-plan (assoc base :extra true)))))
    (doseq [operation [:create :claim]]
      (let [reserved (if (= operation :create)
                       (command/create :gesso_commands record resolution/record-version)
                       (command/update-command
                        :gesso_commands :claim record
                        (assoc record :gesso.command/revision 1
                               :gesso.command/updated-at "t2")
                        resolution/record-version))]
        (is (= :reserved-command-table
               (error-kind #(resolution/incorporation-plan
                             (assoc base :plan {:commands [reserved] :emit false})))))))))

(deftest reconcile-absence-is-unresolved-without-disclosure-or-retry-test
  (let [{:keys [binding]} (exemplar)
        calls (atom 0)
        result (resolution/reconcile
                binding nil (fn [_ _] (swap! calls inc) true))]
    (is (= {:status :unresolved :scope resolution/assurance-scope} result))
    (is (zero? @calls) "Absent authoritative record requires no disclosure call.")
    (is (not (contains? result :outcome)))
    (is (not (contains? result :retry?)))
    (is (not (contains? result :rejected?)))))

(deftest reconcile-withholds-information-unless-explicitly-permitted-test
  (let [{:keys [binding record]} (exemplar)
        alternate (assoc binding :arguments {:request-id "different"})
        calls (atom 0)
        decision (fn [supplied actual]
                   (swap! calls inc)
                   (is (= alternate supplied))
                   (is (= record actual))
                   false)
        unavailable (resolution/reconcile alternate record decision)]
    (is (= 1 @calls))
    (is (= {:status :unavailable :scope resolution/assurance-scope}
           unavailable))
    (is (not (contains? unavailable :outcome)))
    (is (not (contains? unavailable :binding)))
    (is (= :unavailable (:status (resolution/reconcile binding record denied))))
    (is (= :unavailable (:status (resolution/reconcile binding record
                                                          (fn [_ _] :truthy))))
        "Disclosure requires literal true, not a truthy keyword.")))

(deftest compatible-replay-and-incompatible-bindings-test
  (let [{:keys [binding record]} (exemplar)
        variant-bindings [(assoc binding :arguments {:request-id "different"})
                          (assoc binding :operation :request/cancel)
                          (assoc binding :principal (identity/principal "helper-b"))]]
    (is (= {:status :replay
            :outcome (recorded-outcome)
            :scope resolution/assurance-scope}
           (resolution/reconcile binding record allowed)))
    (doseq [changed variant-bindings]
      (let [result (resolution/reconcile changed record allowed)]
        (is (= :incompatible (:status result)))
        (is (not (contains? result :outcome)))
        (is (not (contains? result :binding)))))
    (is (= :missing-disclosure-check
           (error-kind #(resolution/reconcile binding record nil))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"disclosure unavailable"
                          (resolution/reconcile
                           binding record
                           (fn [_ _] (throw (ex-info "disclosure unavailable" {}))))))))

;; The database tests use real embedded XTDB, with unique record IDs and a
;; single node fixture. Reads after every transaction are authoritative; an
;; XTDB assert conflict is the only accepted classification of rejection.
(defonce ^:private !database (atom nil))

(defn- with-real-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !database db)
    (try (f)
         (finally (reset! !database nil)))))

(use-fixtures :once with-real-xtdb)

(defn- database []
  (or @!database
      (throw (ex-info "Real XTDB test fixture not running." {}))))

(defn- commit! [plan]
  (xt/execute-tx (database) (:tx-ops (tx/prepare {} plan))))

(defn- attempt-commit [plan]
  (try
    (commit! plan)
    :committed
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        :assert-failed
        (throw error)))))

(defn- read-command [command-id]
  (let [rows (xt/q (database)
                   ["SELECT * FROM gesso_commands WHERE _id = ?" command-id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Multiple records for one command ID." {:rows rows})))
    (first rows)))

(defn- read-request [request-id]
  (let [rows (xt/q (database)
                   ["SELECT _id, revision, created_at, updated_at, owner FROM requests WHERE _id = ?"
                    request-id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Multiple request rows for one ID." {:rows rows})))
    (first rows)))

(defn- read-membership [member-id]
  (let [rows (xt/q (database)
                   ["SELECT _id, revision, created_at, updated_at, active FROM memberships WHERE _id = ?"
                    member-id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Multiple membership rows for one ID." {:rows rows})))
    (first rows)))

(defn- seed! [& documents]
  (xt/execute-tx (database)
                 (mapv (fn [[table document]] [:put-docs table document]) documents)))

(deftest real-xtdb-incorporation-is-atomic-and-durable-test
  (let [{:keys [request binding]} (exemplar)
        combined (incorporation binding request)
        command-id (resolution/record-id binding)]
    (seed! [:requests request])
    (is (= :committed (attempt-commit (:plan combined))))
    (is (= "helper-a" (:owner (read-request (:xt/id request)))))
    (is (= (:record combined) (read-command command-id)))
    (is (= (recorded-outcome)
           (:outcome (resolution/reconcile binding (read-command command-id) allowed))))
    (is (= :assert-failed (attempt-commit (:plan combined)))
        "Repeating the prepared plan is not a second successful mutation.")
    (is (= (updated request {:owner "helper-a"})
           (read-request (:xt/id request))))
    (is (= (:record combined) (read-command command-id)))))

(deftest real-xtdb-domain-rejection-leaves-no-command-record-test
  (let [{:keys [request binding]} (exemplar)
        membership (membership-document (unique-id "member"))
        combined (incorporation binding request membership)
        revoke {:commands [(revoke-command membership)] :emit false}
        command-id (resolution/record-id binding)]
    (seed! [:requests request] [:memberships membership])
    (is (= :committed (attempt-commit revoke)))
    (is (= false (:active (read-membership (:xt/id membership)))))
    (is (= :assert-failed (attempt-commit (:plan combined))))
    (is (= request (read-request (:xt/id request))))
    (is (nil? (read-command command-id)))
    (is (= :unresolved
           (:status (resolution/reconcile binding (read-command command-id) allowed))))
    (is (not (contains? (resolution/reconcile binding nil allowed) :retry?)))))

(deftest duplicate-command-id-blocks-independent-domain-writes-test
  (let [command-id (unique-id "command")
        first-request (request-document (unique-id "request-one"))
        second-request (request-document (unique-id "request-two"))
        first-binding (binding-for command-id (:xt/id first-request))
        second-binding (binding-for command-id (:xt/id second-request))
        first-plan (incorporation first-binding first-request)
        second-plan (incorporation second-binding second-request)]
    (seed! [:requests first-request] [:requests second-request])
    (is (= :committed (attempt-commit (:plan first-plan))))
    (is (= :assert-failed (attempt-commit (:plan second-plan)))
        "Second domain write has no conflict of its own: record-id reuse must reject it.")
    (is (= (updated first-request {:owner "helper-a"})
           (read-request (:xt/id first-request))))
    (is (= second-request (read-request (:xt/id second-request)))
        "A command-record create conflict prevents unrelated domain writes atomically.")
    (is (= (:record first-plan) (read-command command-id)))
    (is (= :incompatible
           (:status (resolution/reconcile
                     second-binding (read-command command-id) allowed))))
    (is (= :unavailable
           (:status (resolution/reconcile
                     second-binding (read-command command-id) denied))))))

(deftest committed-record-requires-disclosure-before-replay-test
  (let [{:keys [request binding]} (exemplar)
        combined (incorporation binding request)
        command-id (resolution/record-id binding)
        alternate (assoc binding :principal (identity/principal "someone-else"))]
    (seed! [:requests request])
    (is (= :committed (attempt-commit (:plan combined))))
    (let [stored (read-command command-id)]
      (is (= :replay (:status (resolution/reconcile binding stored allowed))))
      (is (= :unavailable (:status (resolution/reconcile binding stored denied))))
      (is (= :unavailable (:status (resolution/reconcile alternate stored denied))))
      (is (= :incompatible (:status (resolution/reconcile alternate stored allowed))))
      (is (= (recorded-outcome)
             (:outcome (resolution/reconcile binding stored allowed)))))))

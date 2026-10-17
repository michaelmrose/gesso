(ns gesso.model.tx-model-xtdb-correspondence-test
  "Bounded, real-XTDB correspondence tests for gesso.model.tx-model.

   Exercises the *production* gesso.model.tx/prepare lowering through a live
   embedded XTDB node. Each serialized execution is compared with the pure
   reference machine's prediction of commit/rejection and the persisted
   document state. A rejection must be XTDB's typed :xtdb/assert-failed
   conflict; unrelated execution failures are rethrown, never counted as
   evidence of correspondence.

   These tests cover a deliberately small admitted subset: version-guarded
   :put-docs on :requests and :memberships, including document absence for
   creates, canonical field-count ASSERTs, and explicit representation
   boundaries. Their serialization is deliberate. They do not establish XTDB's
   isolation under truly concurrent submissions, business authorization,
   durable application-command replay, general SQL correspondence, or
   correctness of domain-specific guards. In particular, the missing-guard
   test demonstrates a policy dependency that this generic layer cannot
   infer. An explicit-nil test is a RED correspondence regression against
   the current finite model: XTDB removes nil-valued document fields rather
   than counting them as persisted fields. This namespace is NOT a
   certification or formal proof artifact."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private !node (atom nil))

(defn- node []
  (or @!node
      (throw (ex-info "Real XTDB fixture was not started." {}))))

(defn- with-real-xtdb [f]
  (with-open [database (xtn/start-node {})]
    (reset! !node database)
    (try
      (f)
      (finally (reset! !node nil)))))

(use-fixtures :once with-real-xtdb)

(defn- request-document [id]
  {:xt/id id
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(defn- updated [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(defn- claim [before owner]
  (command/update-command
   :requests :claim before (updated before {:owner owner}) version))

(defn- revoke [before]
  (command/update-command
   :memberships :revoke before (updated before {:active false}) version))

(defn- plan [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(defn- prepared-ops [a-plan]
  ;; Gesso's actual XTDB/HoneySQL lowering: no test-side SQL formatter.
  (:tx-ops (tx/prepare {} a-plan)))

(defn- fixture []
  (let [suffix (str (random-uuid))
        req (request-document (str "tx-model-request-" suffix))
        mem (membership (str "tx-model-member-" suffix))
        state {[:requests (:xt/id req)] req
               [:memberships (:xt/id mem)] mem}]
    ;; Seed through real XTDB; each scenario gets disjoint unique IDs.
    (xt/execute-tx (node) [[:put-docs :requests req]
                           [:put-docs :memberships mem]])
    {:request req
     :membership mem
     :state state}))

(defn- select-document [table id]
  ;; SELECT * preserves the observable top-level field set, including newly
  ;; added fields. A fixed projection would hide the very drift that the
  ;; field-count assertion is intended to detect. XTDB returns :xt/id for _id.
  (let [sql (case table
              :requests "SELECT * FROM requests WHERE _id = ?"
              :memberships "SELECT * FROM memberships WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "More than one document for the requested ID."
                      {:table table :id id :rows rows})))
    (first rows)))

(defn- persisted-state [targets]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [document (select-document table id)]
                  [target document])))
        targets))

(defn- execute-and-classify [ops]
  (try
    {:status :committed
     :tx-key (xt/execute-tx (node) ops)}
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        {:status :rejected
         :error/code :xtdb/assert-failed}
        (throw error)))))

(defn- compare-step!
  "Compare the abstract serial step to actual XTDB execution, then read the
   durable documents rather than trusting the in-memory expected state.
   `targets` includes absent write targets, to detect partial commits."
  [before a-plan targets]
  (let [prediction (model/step before a-plan)
        execution (execute-and-classify (prepared-ops a-plan))
        actual (persisted-state targets)]
    (is (= (:status prediction) (:status execution))
        (str "Reference/XTDB disagreement: " (pr-str execution)))
    (is (= (:after prediction) actual)
        (str "Persisted documents disagree with reference step: "
             (pr-str {:expected (:after prediction) :actual actual})))
    (when (= :rejected (:status prediction))
      (is (= :xtdb/assert-failed (:error/code execution))
          "Only a recognized XTDB ASSERT conflict counts as rejection."))
    {:prediction prediction
     :execution execution
     :actual actual}))

(deftest successful-guarded-claim-and-stale-retry-correspond-test
  (let [{:keys [request membership state]} (fixture)
        action (plan [(claim request "helper-a")]
                     (command/guard :memberships membership version))
        targets (keys state)
        first-step (compare-step! state action targets)
        second-step (compare-step! (:actual first-step) action targets)]
    (is (= :committed (get-in first-step [:prediction :status])))
    (is (= "helper-a"
           (get-in first-step [:actual [:requests (:xt/id request)] :owner])))
    (is (= :rejected (get-in second-step [:prediction :status])))
    (is (= (:actual first-step) (:actual second-step))
        "Repeating a prepared update must not apply it twice.")))

(deftest competing-prepared-claims-correspond-in-both-serial-orders-test
  (doseq [order [[0 1] [1 0]]]
    (let [{:keys [request membership state]} (fixture)
          actions [(plan [(claim request "helper-a")]
                         (command/guard :memberships membership version))
                   (plan [(claim request "helper-b")]
                         (command/guard :memberships membership version))]
          reference-path (first (filter #(= order (:order %))
                                        (:paths (model/explore-orders state actions))))
          actual-trace (loop [current state
                              remaining order
                              trace []]
                         (if-let [index (first remaining)]
                           (let [result (compare-step! current (nth actions index)
                                                       (keys state))]
                             (recur (:actual result)
                                    (rest remaining)
                                    (conj trace (get-in result [:execution :status]))))
                           trace))]
      (is (= (:final-state reference-path) (persisted-state (keys state))))
      (is (= (mapv :status (:trace reference-path)) actual-trace))
      (is (= [:committed :rejected] actual-trace))
      (is (= (if (= 0 (first order)) "helper-a" "helper-b")
             (:owner (select-document :requests (:xt/id request))))))))

(deftest stale-membership-guard-rejects-claim-test
  (let [{:keys [request membership state]} (fixture)
        claim-plan (plan [(claim request "helper-a")]
                         (command/guard :memberships membership version))
        revoke-plan (plan [(revoke membership)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        result (compare-step! after-revoke claim-plan (keys state))]
    (is (= false
           (get-in after-revoke [[:memberships (:xt/id membership)] :active])))
    (is (= :rejected (get-in result [:execution :status])))
    (is (= "unclaimed"
           (get-in result [:actual [:requests (:xt/id request)] :owner])))))

(deftest rejected-multi-write-is-atomic-in-real-xtdb-test
  (let [{:keys [request membership state]} (fixture)
        created (request-document (str "tx-model-created-" (random-uuid)))
        action (plan [(claim request "helper-a")
                      (command/create :requests created version)]
                     (command/guard :memberships membership version))
        revoke-plan (plan [(revoke membership)])
        all-targets (conj (vec (keys state)) [:requests (:xt/id created)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        result (compare-step! after-revoke action all-targets)]
    (is (= :rejected (get-in result [:execution :status])))
    (is (= "unclaimed" (:owner (select-document :requests (:xt/id request)))))
    (is (nil? (select-document :requests (:xt/id created)))
        "A failing guard must suppress every put-docs operation.")))

(deftest successful-multi-write-matches-reference-test
  (let [{:keys [request membership state]} (fixture)
        created (request-document (str "tx-model-created-" (random-uuid)))
        action (plan [(claim request "helper-a")
                      (command/create :requests created version)]
                     (command/guard :memberships membership version))
        result (compare-step! state action
                              (conj (vec (keys state)) [:requests (:xt/id created)]))]
    (is (= :committed (get-in result [:execution :status])))
    (is (= created (select-document :requests (:xt/id created))))
    (is (= "helper-a" (:owner (select-document :requests (:xt/id request)))))))

(deftest duplicate-create-is-rejected-without-overwrite-test
  (let [created (request-document (str "tx-model-create-" (random-uuid)))
        target [:requests (:xt/id created)]
        action (plan [(command/create :requests created version)])
        first-step (compare-step! {} action [target])
        retry (compare-step! (:actual first-step) action [target])]
    (is (= :committed (get-in first-step [:execution :status])))
    (is (= :rejected (get-in retry [:execution :status])))
    (is (= created (select-document :requests (:xt/id created))))))

(deftest omitting-revoked-membership-guard-permits-an-unsafe-domain-outcome-test
  (let [{:keys [request membership state]} (fixture)
        guarded (plan [(claim request "helper-a")]
                      (command/guard :memberships membership version))
        unguarded (plan [(claim request "helper-a")])
        revoke-plan (plan [(revoke membership)])
        after-revoke (:actual (compare-step! state revoke-plan (keys state)))
        guarded-prediction (model/step after-revoke guarded)
        unguarded-result (compare-step! after-revoke unguarded (keys state))]
    (is (= :rejected (:status guarded-prediction)))
    (is (= :committed (get-in unguarded-result [:execution :status])))
    (is (= false
           (get-in unguarded-result [:actual [:memberships (:xt/id membership)] :active])))
    (is (= "helper-a"
           (get-in unguarded-result [:actual [:requests (:xt/id request)] :owner])))
    (testing "This is a counterexample to an *undeclared policy*, not a pass for authorization."
      (is (= :committed (:status (model/step after-revoke unguarded)))))))


;; v999 — Cross-representation witness cases for the canonical field-count
;; assertion. All comparisons below use production tx/prepare and the actual
;; embedded XTDB transaction executor. They are not universal refinement
;; proofs. The explicit nil case is intentionally a failing correspondence
;; regression until the admitted finite state representation is corrected.

(defn- fresh-request [label]
  (request-document (str "v999-" label "-" (random-uuid))))

(defn- seed-request! [document]
  (xt/execute-tx (node) [[:put-docs :requests document]])
  document)

(defn- complete-guard [observed]
  (command/guard
   :requests observed
   (assoc version
          :compare-keys (->> (keys observed)
                             (remove #{:xt/id})
                             sort
                             vec))))

(defn- field-count-plan
  ([observed output]
   (field-count-plan observed output true))
  ([observed output compare-all-fields?]
   {:commands [(command/create :requests output version)]
    :guards [(if compare-all-fields?
               (complete-guard observed)
               (command/guard :requests observed version))]
    :assertions [(tx/assert-document-field-count
                  :requests (:xt/id observed) (count observed))]
    :emit false}))

(deftest v999-complete-count-matches-real-nested-document-test
  (let [observed (seed-request!
                  (assoc (fresh-request "complete")
                         :policy {:groups ["helpers" "staff"]
                                  :priority 3}))
        output (fresh-request "complete-out")
        target [:requests (:xt/id observed)]
        outcome (compare-step! {target observed}
                               (field-count-plan observed output)
                               [target [:requests (:xt/id output)]])]
    (is (= :committed (get-in outcome [:execution :status])))
    (is (= observed (select-document :requests (:xt/id observed))))
    (is (= output (select-document :requests (:xt/id output))))))

(deftest v999-late-top-level-field-rejects-and-preserves-write-set-test
  (let [observed (seed-request! (fresh-request "new-field"))
        output (fresh-request "new-field-out")
        action (field-count-plan observed output)
        changed (assoc observed :late-policy "introduced")
        target [:requests (:xt/id observed)]
        _ (seed-request! changed)
        result (compare-step! {target changed} action
                              [target [:requests (:xt/id output)]])]
    (is (= :rejected (get-in result [:execution :status])))
    (is (= :document-field-count
           (get-in result [:prediction :failed-precondition :kind])))
    (is (nil? (select-document :requests (:xt/id output))))
    (is (= changed (select-document :requests (:xt/id observed))))))

(deftest v999-removed-top-level-field-rejects-and-preserves-write-set-test
  (let [observed (seed-request!
                  (assoc (fresh-request "removed-field")
                         :policy "must-remain"))
        output (fresh-request "removed-field-out")
        action (field-count-plan observed output)
        changed (dissoc observed :policy)
        target [:requests (:xt/id observed)]
        _ (seed-request! changed)
        result (compare-step! {target changed} action
                              [target [:requests (:xt/id output)]])]
    (is (= :rejected (get-in result [:execution :status])))
    (is (= :document-field-count
           (get-in result [:prediction :failed-precondition :kind])))
    (is (nil? (select-document :requests (:xt/id output))))))

(deftest v999-same-field-count-but-changed-nested-value-rejects-test
  (let [observed (seed-request!
                  (assoc (fresh-request "nested-drift")
                         :policy {:groups ["helpers"] :priority 3}))
        output (fresh-request "nested-drift-out")
        action (field-count-plan observed output)
        changed (assoc-in observed [:policy :priority] 4)
        target [:requests (:xt/id observed)]
        _ (seed-request! changed)
        result (compare-step! {target changed} action
                              [target [:requests (:xt/id output)]])]
    (is (= (count observed) (count changed)))
    (is (= :rejected (get-in result [:execution :status])))
    (is (= :dependency-version
           (get-in result [:prediction :failed-precondition :kind])))
    (is (nil? (select-document :requests (:xt/id output))))))

(deftest v999-explicit-nil-is-not-a-persisted-field-count-regression-test
  (let [observed (seed-request!
                  (assoc (fresh-request "nil-field")
                         :optional-policy nil))
        output (fresh-request "nil-field-out")
        target [:requests (:xt/id observed)]
        physical (select-document :requests (:xt/id observed))
        action (field-count-plan observed output false)
        prediction (model/step {target observed} action)
        execution (execute-and-classify (prepared-ops action))]
    (testing "XTDB drops top-level nil when persisting document data"
      (is (contains? observed :optional-policy))
      (is (not (contains? physical :optional-policy)))
      (is (= (dec (count observed)) (count physical))))
    (testing "The same field-count ASSERT must agree with the finite model"
      (is (= :rejected (:status execution)))
      ;; Expected RED against v994: tx-model presently counts every map key.
      ;; A later fix must either reject non-representable nil-valued documents
      ;; with a typed diagnostic or give them a sound physical abstraction.
      (is (= (:status execution) (:status prediction))
          (str "Explicit nil/absence is not represented faithfully: "
               (pr-str {:physical-field-count (count physical)
                        :reference-field-count (count observed)
                        :physical-status (:status execution)
                        :reference-status (:status prediction)})))
      (is (nil? (select-document :requests (:xt/id output)))))))

(deftest v999-removed-document-fails-field-count-and-guard-atomically-test
  (let [observed (seed-request! (fresh-request "removed-document"))
        output (fresh-request "removed-document-out")
        action (field-count-plan observed output)
        target [:requests (:xt/id observed)]
        _ (xt/execute-tx (node) [[:delete-docs :requests (:xt/id observed)]])
        result (compare-step! {} action
                              [target [:requests (:xt/id output)]])]
    (is (= :rejected (get-in result [:execution :status])))
    (is (= :document-field-count
           (get-in result [:prediction :failed-precondition :kind])))
    (is (nil? (select-document :requests (:xt/id output))))
    (is (nil? (select-document :requests (:xt/id observed))))))

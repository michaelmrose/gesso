(ns gesso.model.tx-model-correspondence-test
  "Correspondence tests between the bounded gesso.model.tx-model machine and
   production gesso.model.tx/prepare lowering.

   A deliberately independent, *test-only* interpreter admits only the small
   SQL ASSERT/put-docs subset generated for this fixture by tx/prepare. It
   evaluates every assertion on the same pre-write document snapshot, then
   applies all writes or none. Unknown SQL, predicates, and write forms fail
   closed rather than being counted as evidence.

   This checks finite reference-vs-lowered-operation agreement. It does NOT
   execute XTDB, prove XTDB's isolation/atomicity, certify arbitrary HoneySQL,
   validate application policy, or establish application-command replay."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as model]))

(def version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def request
  {:xt/id "request-1"
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :owner "unclaimed"})

(def membership
  {:xt/id "membership-1"
   :revision 0
   :created-at "t0"
   :updated-at "t0"
   :active true})

(def initial-state
  {[:requests "request-1"] request
   [:memberships "membership-1"] membership})

(defn- updated
  [document changes]
  (-> document
      (merge changes)
      (update :revision inc)
      (assoc :updated-at "t1")))

(def claim-a
  (command/update-command
   :requests :claim request (updated request {:owner "helper-a"}) version))

(def claim-b
  (command/update-command
   :requests :claim request (updated request {:owner "helper-b"}) version))

(def revoke
  (command/update-command
   :memberships :revoke membership (updated membership {:active false}) version))

(def member-guard
  (command/guard :memberships membership version))

(defn- plan
  [commands & guards]
  {:commands (vec commands)
   :guards (vec guards)
   :emit false})

(def guarded-claim
  (plan [claim-a] member-guard))

(def unguarded-claim
  (plan [claim-a]))

(def revoke-plan
  (plan [revoke]))

;; This decoder is intentionally NOT a general SQL interpreter. It recognizes
;; only the two canonical query shapes required by this particular fixture.
;; All source SQL originates from production tx/prepare, not test-side reformatting.
(def ^:private assert-prefix
  #"ASSERT \? = \(SELECT COUNT\(\*\) FROM (requests|memberships) WHERE (.*)\)")

(def ^:private allowed-columns
  {"_id" :xt/id
   "revision" :revision
   "updated_at" :updated-at})

(defn- unsupported!
  [value]
  (throw
   (ex-info "Outside the v786 lowered-transaction correspondence subset."
            {:error/type ::unsupported-lowering
             :value value})))

(defn- column!
  [column]
  (or (get allowed-columns column)
      (unsupported! {:column column})))

(defn- decode-assert
  [[sql expected-count & values :as operation]]
  (when-not (and (string? sql)
                 (#{0 1} expected-count))
    (unsupported! operation))
  (let [[_ table clause] (or (re-matches assert-prefix sql)
                            (unsupported! operation))
        columns (cond
                  (= clause "_id = ?")
                  ["_id"]

                  (re-matches #"\(_id = \?\)( AND \([a-z_]+ = \?\))+" clause)
                  (let [found (mapv second
                                    (re-seq #"\(([a-z_]+) = \?\)" clause))]
                    (when-not (= (count found) (count (distinct found)))
                      (unsupported! operation))
                    found)

                  :else
                  (unsupported! operation))
        columns (mapv column! columns)]
    (when-not (and (= :xt/id (first columns))
                   (= (count columns) (count values)))
      (unsupported! operation))
    {:table (keyword table)
     :expected-count expected-count
     :checks (mapv vector columns values)}))

(defn- assert-count
  [state {:keys [table checks]}]
  (count
   (for [[[entity-type _] document] state
         :when (= entity-type table)
         :when (every? (fn [[field expected]]
                         (and (contains? document field)
                              (= expected (get document field))))
                       checks)]
     document)))

(defn- decode-write
  [op]
  (when-not (and (vector? op)
                 (= 3 (count op))
                 (= :put-docs (nth op 0))
                 (#{:requests :memberships} (nth op 1))
                 (map? (nth op 2))
                 (string? (:xt/id (nth op 2))))
    (unsupported! op))
  [(nth op 1) (:xt/id (nth op 2)) (nth op 2)])

(defn- lowered-step
  "Evaluate *formatted production operations*, not model/step's checks.

   Every SELECT count observes the same original document state. A rejected
   transaction has no writes; success applies the complete put-docs set."
  [state prepared-ops]
  (model/require-state state)
  (let [[assertions writes] (split-with #(and (vector? %)
                                               (string? (first %)))
                                         prepared-ops)
        assertions (mapv decode-assert assertions)
        writes (mapv decode-write writes)]
    (when (or (empty? assertions)
              (empty? writes)
              (not= (count writes)
                    (count (distinct (map #(subvec % 0 2) writes)))))
      (unsupported! prepared-ops))
    (let [passes? (every? (fn [assertion]
                            (= (:expected-count assertion)
                               (assert-count state assertion)))
                          assertions)]
      {:status (if passes? :committed :rejected)
       :after (if passes?
                (reduce (fn [current [entity-type id document]]
                          (assoc current [entity-type id] document))
                        state
                        writes)
                state)
       :assertions assertions
       :writes writes})))

(defn- compare-step
  [state plan]
  (let [abstract (model/step state plan)
        prepared (tx/prepare {} plan)
        lowered (lowered-step state (:tx-ops prepared))]
    {:abstract abstract
     :lowered lowered
     :prepared prepared
     :agreement? (= (select-keys abstract [:status :after])
                    (select-keys lowered [:status :after]))}))

(defn- run-order
  [initial plans order]
  (loop [state initial
         indices order
         trace []]
    (if-let [index (first indices)]
      (let [comparison (compare-step state (nth plans index))]
        (recur (get-in comparison [:lowered :after])
               (rest indices)
               (conj trace
                     {:plan-index index
                      :status (get-in comparison [:lowered :status])
                      :state (get-in comparison [:lowered :after])
                      :agreement? (:agreement? comparison)})))
      {:order order
       :trace trace
       :final-state state})))

(defn- compare-all-orders
  [initial plans]
  (let [reference (model/explore-orders initial plans)]
    (mapv (fn [path]
            {:reference path
             :lowered (run-order initial plans (:order path))})
          (:paths reference))))

(deftest preparation-actually-emits-guard-and-command-assertions-test
  (let [{:keys [tx-ops plan]} (tx/prepare {} guarded-claim)
        [member-check request-check write] tx-ops]
    (is (= 3 (count tx-ops)))
    (is (= [:dependency-version :command-version]
           (mapv :kind (:checks (model/step initial-state guarded-claim)))))
    (is (= {:table :memberships
            :expected-count 1
            :checks [[:xt/id "membership-1"]
                     [:revision 0]
                     [:updated-at "t0"]]}
           (decode-assert member-check)))
    (is (= {:table :requests
            :expected-count 1
            :checks [[:xt/id "request-1"]
                     [:revision 0]
                     [:updated-at "t0"]]}
           (decode-assert request-check)))
    (is (= [:requests "request-1" (command/after claim-a)]
           (decode-write write)))
    (is (= false (:emit plan)))))

(deftest claim-and-stale-claim-correspond-test
  (let [first-pass (compare-step initial-state guarded-claim)
        after (get-in first-pass [:lowered :after])
        stale (compare-step after guarded-claim)]
    (is (:agreement? first-pass))
    (is (= :committed (get-in first-pass [:lowered :status])))
    (is (= "helper-a" (get-in after [[:requests "request-1"] :owner])))
    (is (:agreement? stale))
    (is (= :rejected (get-in stale [:lowered :status])))
    (is (= after (get-in stale [:lowered :after])))))

(deftest competing-prepared-claims-correspond-in-every-order-test
  (doseq [{:keys [reference lowered]}
          (compare-all-orders initial-state [(plan [claim-a]) (plan [claim-b])])]
    (is (= (:order reference) (:order lowered)))
    (is (= (:final-state reference) (:final-state lowered)))
    (is (= (mapv :status (:trace reference))
           (mapv :status (:trace lowered))))
    (is (every? :agreement? (:trace lowered)))
    (is (= [:committed :rejected]
           (mapv :status (:trace lowered))))))

(deftest stale-guard-and-omitted-guard-correspond-in-both-orders-test
  (let [after-revocation (:after (model/step initial-state revoke-plan))
        guarded (compare-step after-revocation guarded-claim)
        unguarded (compare-step after-revocation unguarded-claim)]
    (is (:agreement? guarded))
    (is (= :rejected (get-in guarded [:lowered :status])))
    (is (:agreement? unguarded))
    (is (= :committed (get-in unguarded [:lowered :status])))
    (is (= "helper-a"
           (get-in unguarded [:lowered :after [:requests "request-1"] :owner]))))
  (doseq [plans [[guarded-claim revoke-plan]
                 [unguarded-claim revoke-plan]]
          {:keys [reference lowered]} (compare-all-orders initial-state plans)]
    (is (= (:final-state reference) (:final-state lowered)))
    (is (= (mapv :status (:trace reference))
           (mapv :status (:trace lowered))))
    (is (every? :agreement? (:trace lowered)))))

(deftest atomic-multi-write-and-stale-guard-correspond-test
  (let [new-record {:xt/id "request-2"
                    :revision 0
                    :created-at "t0"
                    :updated-at "t0"
                    :owner "unclaimed"}
        create (command/create :requests new-record version)
        multiple (plan [claim-a create] member-guard)
        success (compare-step initial-state multiple)
        after-revocation (:after (model/step initial-state revoke-plan))
        rejection (compare-step after-revocation multiple)]
    (is (:agreement? success))
    (is (= :committed (get-in success [:lowered :status])))
    (is (= new-record
           (get-in success [:lowered :after [:requests "request-2"]])))
    (is (= "helper-a"
           (get-in success [:lowered :after [:requests "request-1"] :owner])))
    (is (= 2 (count (get-in success [:lowered :writes]))))
    (is (:agreement? rejection))
    (is (= :rejected (get-in rejection [:lowered :status])))
    (is (= after-revocation (get-in rejection [:lowered :after])))
    (is (not (contains? (get-in rejection [:lowered :after])
                        [:requests "request-2"])))))

(deftest duplicate-create-corresponds-test
  (let [created {:xt/id "request-2"
                 :revision 0
                 :created-at "t0"
                 :updated-at "t0"
                 :owner "unclaimed"}
        create (plan [(command/create :requests created version)])]
    (doseq [{:keys [reference lowered]} (compare-all-orders {} [create create])]
      (is (= (:final-state reference) (:final-state lowered)))
      (is (= [:committed :rejected]
             (mapv :status (:trace lowered))))
      (is (every? :agreement? (:trace lowered))))))

(deftest omitted-guard-in-lowering-produces-detectable-disagreement-test
  (let [after-revocation (:after (model/step initial-state revoke-plan))
        {:keys [tx-ops]} (tx/prepare {} guarded-claim)
        reference (model/step after-revocation guarded-claim)
        correctly-lowered (lowered-step after-revocation tx-ops)
        ;; Deliberately remove the dependency assertion, leaving the command
        ;; version assertion and the put-docs operation intact.
        guard-omitted (lowered-step after-revocation (subvec tx-ops 1))]
    (is (= :rejected (:status reference)))
    (is (= :rejected (:status correctly-lowered)))
    (is (= :committed (:status guard-omitted)))
    (is (not= (select-keys reference [:status :after])
              (select-keys guard-omitted [:status :after])))
    (is (= "helper-a"
           (get-in guard-omitted [:after [:requests "request-1"] :owner])))))

(deftest missing-command-assertion-produces-detectable-replay-disagreement-test
  (let [after-claim (:after (model/step initial-state unguarded-claim))
        {:keys [tx-ops]} (tx/prepare {} unguarded-claim)
        reference (model/step after-claim unguarded-claim)]
    ;; Removing *all* preconditions is not an admitted lowering. The strict
    ;; interpreter must refuse it, rather than classify a blind put as valid.
    (is (= :rejected (:status reference)))
    (is (= ::unsupported-lowering
           (try
             (lowered-step after-claim (subvec tx-ops 1))
             nil
             (catch clojure.lang.ExceptionInfo ex
               (:error/type (ex-data ex))))))))

(deftest interpreter-rejects-unadmitted-sql-and-operations-test
  (let [unsupported (fn [ops]
                      (try
                        (lowered-step {} ops)
                        nil
                        (catch clojure.lang.ExceptionInfo ex
                          (:error/type (ex-data ex)))))]
    (testing "unknown assertions never count as corresponded"
      (is (= ::unsupported-lowering
             (unsupported [["ASSERT TRUE"] [:put-docs :requests request]])))
      (is (= ::unsupported-lowering
             (unsupported [["ASSERT ? = (SELECT COUNT(*) FROM requests WHERE owner = ?)"
                            1 "helper-a"]
                           [:put-docs :requests request]]))))
    (testing "unknown writes never count as corresponded"
      (is (= ::unsupported-lowering
             (unsupported [["ASSERT ? = (SELECT COUNT(*) FROM requests WHERE _id = ?)"
                            0 "request-1"]
                           [:delete-docs :requests "request-1"]]))))
    (testing "uninterpreted assertion plans remain inadmissible in reference model"
      (is (= ::model/unsupported-plan
             (try
               (model/admitted-plan
                {:commands [claim-a]
                 :assertions [(tx/assert-one :requests [:= :owner "unclaimed"])]
                 :emit false})
               nil
               (catch clojure.lang.ExceptionInfo ex
                 (:error/type (ex-data ex)))))))))

(ns gesso.model.resolution-evidence-test
  "Permanent finite witness-checker regressions for v803.

   These tests use *actual* gesso.model.command-resolution incorporation
   constructions. The pure cases exercise exhaustive serial-order checking,
   status/order alignment, completeness of target inspection and deliberately
   adversarial or malformed observations. Real embedded XTDB tests execute
   gesso.model.tx/prepare's output, then independently query durable domain
   and command-result records.

   A :consistent result establishes consistency ONLY relative to supplied
   observations and the finite serial reference. It is not authenticated
   observation provenance, a policy proof, a proof of actual database
   serializability, or a HumanHelp domain guarantee. In particular, the tests
   intentionally demonstrate that fabricated but internally consistent
   observations and under-guarded application plans remain outside scope."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-evidence :as evidence]
   [gesso.model.tx :as tx]
   [gesso.model.tx-model :as reference]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private now "2026-10-12T12:00:00Z")
(defonce ^:private !node (atom nil))

(defn- with-database [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-database)

(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB fixture not started." {}))))

(defn- request [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- fixture []
  (let [suffix (str (random-uuid))
        req (request (str "resolution-evidence-request-" suffix))
        member (membership (str "resolution-evidence-member-" suffix))]
    {:request req
     :membership member
     :initial {[:requests (:xt/id req)] req
               [:memberships (:xt/id member)] member}}))

(defn- after [before changes]
  (-> before (merge changes) (update :revision inc) (assoc :updated-at now)))

(defn- incorporation
  ([req member id owner]
   (incorporation req member id owner true))
  ([req member id owner guard?]
   (resolution/incorporation-plan
    {:binding {:command-id (identity/command-id id)
               :principal (identity/principal owner)
               :operation :request/claim
               :arguments {:request-id (:xt/id req)}}
     :outcome {:status :claimed :owner owner}
     :now now
     :plan {:commands [(command/update-command
                        :requests :claim req (after req {:owner owner}) version)]
            :guards (if guard?
                      [(command/guard :memberships member version)]
                      [])
            :emit false}})))

(defn- claims [{:keys [request membership]}]
  [(incorporation request membership
                  (str "cmd-a-" (:xt/id request)) "helper-a")
   (incorporation request membership
                  (str "cmd-b-" (:xt/id request)) "helper-b")])

(defn- targets [initial incorporations]
  (reduce (fn [acc {:keys [plan]}]
            (set/union
             acc
             (into #{} (map command/target) (:commands plan))
             (into #{} (map command/guard-target)
                   (tx/effective-guards (:commands plan) (:guards plan)))))
          (set (keys initial))
          incorporations))

(defn- reference-observations [initial incorporations order]
  (let [paths (:paths (reference/explore-orders
                       initial (mapv :plan incorporations)))
        path (first (filter #(= order (:order %)) paths))]
    (when-not path
      (throw (ex-info "Reference ordering missing." {:order order})))
    {:statuses (reduce (fn [v {:keys [plan-index status]}]
                         (assoc v plan-index status))
                       (vec (repeat (count incorporations) nil))
                       (:trace path))
     :state (:final-state path)
     :inspected-targets (targets initial incorporations)}))

(defn- input [initial incorporations observations]
  {:initial-state initial
   :incorporations incorporations
   :observations observations})

(defn- evidence-for [initial incorporations observations]
  (evidence/check-execution (input initial incorporations observations)))

(defn- error-kind [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (let [data (ex-data error)]
        (is (= ::evidence/invalid-evidence (:error/type data))
            (str "Expected a typed evidence error: " (pr-str data)))
        (:error/kind data)))))

(defn- prepared-pair []
  (let [{:keys [initial] :as f} (fixture)
        incorporations (claims f)]
    {:initial initial :incorporations incorporations}))

(deftest scope-explicitly-limits-what-consistency-means-test
  (let [{:keys [checked assumed not-established]} evidence/evidence-scope]
    (is (= :gesso.model.resolution-evidence/serial-observation-v1
           (:checker evidence/evidence-scope)))
    (doseq [property [:finite-serial-order-witness
                      :persisted-state-correspondence
                      :whole-transaction-status-correspondence
                      :complete-declared-target-inspection]]
      (is (contains? checked property)))
    (doseq [assumption [:independent-authoritative-observation
                        :consistent-post-execution-database-snapshot
                        :prepared-plans-are-the-actual-submitted-plans]]
      (is (contains? assumed assumption)))
    (doseq [limitation [:authenticated-observation-provenance
                        :policy-guard-completeness
                        :domain-invariant-correctness
                        :durable-replay-and-recovery-liveness
                        :unbounded-concurrent-linearizability
                        :formal-refinement-proof]]
      (is (contains? not-established limitation)))))

(deftest both-serial-winners-produce-checkable-witnesses-test
  (let [{:keys [initial incorporations]} (prepared-pair)]
    (doseq [order [[0 1] [1 0]]]
      (let [observations (reference-observations initial incorporations order)
            result (evidence-for initial incorporations observations)]
        (is (= :consistent (:status result)))
        (is (= :matched-serial-reference (:reason result)))
        (is (= [order] (:matching-orders result)))
        (is (= order (get-in result [:witness :order])))
        (is (= 2 (count (get-in result [:witness :trace]))))
        (is (nil? (:expected result)))
        (is (= (:inspected-targets observations) (:inspected-targets result)))
        (is (= evidence/evidence-scope (:scope result)))
        (is (= reference/reference-scope (:reference-scope result)))))))

(deftest statuses-correlate-with-plan-indices-not-commit-order-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        reverse-observation (reference-observations initial incorporations [1 0])
        result (evidence-for initial incorporations reverse-observation)]
    (is (= [:rejected :committed] (:statuses reverse-observation)))
    (is (= :consistent (:status result)))
    (is (= [[1 0]] (:matching-orders result)))
    (is (= [1 0] (get-in result [:witness :order])))))

(deftest impossible-status-combinations-produce-counterexamples-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])]
    (doseq [statuses [[:committed :committed] [:rejected :rejected]]]
      (let [result (evidence-for initial incorporations
                                 (assoc base :statuses statuses))]
        (is (= :counterexample (:status result)))
        (is (= :impossible-transaction-statuses (:reason result)))
        (is (nil? (:witness result)))
        (is (empty? (:matching-orders result)))
        (is (= 2 (count (:expected result))))))))

(deftest persisted-state-must-agree-even-when-statuses-match-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        command-target [:gesso_commands (:xt/id (:record (first incorporations)))]
        request-target (first (filter #(= :requests (first %)) (keys initial)))
        altered [(assoc base :state initial)
                 (update base :state dissoc command-target)
                 (assoc-in base [:state command-target :gesso.command/outcome]
                           {:status :forged})
                 (assoc-in base [:state request-target :owner] "invented")]]
    (doseq [obs altered]
      (let [result (evidence-for initial incorporations obs)]
        (is (= :counterexample (:status result)))
        (is (= :persisted-state-disagrees (:reason result)))
        (is (nil? (:witness result)))
        (is (seq (:expected result)))))))

(deftest inspection-requires-every-initial-guarded-and-written-target-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        absent-record [:gesso_commands (:xt/id (:record (second incorporations)))]
        membership-target (first (filter #(= :memberships (first %)) (keys initial)))]
    (is (not (contains? (:state base) absent-record)))
    (is (contains? (:inspected-targets base) absent-record))
    (doseq [missing [absent-record membership-target]]
      (is (= :incomplete-inspection
             (error-kind #(evidence-for initial incorporations
                                        (update base :inspected-targets disj missing))))))
    (is (= :incomplete-inspection
           (error-kind #(evidence-for initial incorporations
                                      (update base :inspected-targets conj [:other "extra"])))))))

(deftest uninspected-extra-current-document-is-rejected-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        extra [:requests "unexpected"]]
    (is (= :incomplete-inspection
           (error-kind #(evidence-for
                         initial incorporations
                         (assoc-in base [:state extra]
                                   (request "unexpected"))))))))

(deftest initial-and-observed-documents-must-have-coherent-identities-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        [target original] (first initial)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (evidence-for (assoc initial target (assoc original :xt/id "wrong"))
                               incorporations base)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (evidence-for initial incorporations
                               (assoc-in base [:state target :xt/id] "wrong"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (evidence-for initial incorporations
                               (assoc-in base [:state target] nil))))))

(deftest top-level-and-observation-maps-are-exactly-shaped-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        valid (input initial incorporations base)]
    (doseq [bad [(assoc valid :trusted true)
                 (dissoc valid :observations)]]
      (is (= :invalid-input
             (error-kind #(evidence/check-execution bad)))))
    (doseq [bad [(assoc base :attested true)
                 (dissoc base :inspected-targets)]]
      (is (= :invalid-observation
             (error-kind #(evidence-for initial incorporations bad)))))))

(deftest incorporation-maps-and-scope-are-closed-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        good (first incorporations)]
    (is (= :invalid-incorporation
           (error-kind #(evidence-for initial
                                      (assoc incorporations 0 (assoc good :certified true))
                                      base))))
    (is (= :invalid-incorporation
           (error-kind #(evidence-for initial
                                      (assoc incorporations 0 (dissoc good :record))
                                      base))))
    (is (= :invalid-incorporation-scope
           (error-kind #(evidence-for initial
                                      (assoc incorporations 0 (assoc good :scope {:forged true}))
                                      base))))))

(deftest durable-record-create-must-match-record-exactly-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        good (first incorporations)
        commands (get-in good [:plan :commands])
        row-index (first (keep-indexed
                          (fn [idx c]
                            (when (= :gesso_commands (first (command/target c))) idx))
                          commands))
        changed (assoc-in good
                          [:plan :commands row-index :model/after :gesso.command/outcome]
                          {:status :forged})
        removed (update-in good [:plan :commands]
                           (fn [cmds] (vec (remove #(= :gesso_commands
                                                       (first (command/target %))) cmds))))]
    (is (= :invalid-durable-incorporation
           (error-kind #(evidence-for initial (assoc incorporations 0 changed) base))))
    (is (= :invalid-durable-incorporation
           (error-kind #(evidence-for initial (assoc incorporations 0 removed) base))))))

(deftest invalid-status-types-and-cardinality-fail-before-model-exploration-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])]
    (doseq [bad [[:committed] [:committed :replayed]
                 [:committed :failed] '(:committed :rejected) nil]]
      (is (= :invalid-statuses
             (error-kind #(evidence-for initial incorporations
                                        (assoc base :statuses bad))))))))

(deftest invalid-number-or-shape-of-incorporations-fail-closed-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])]
    (doseq [bad [[] (vec (repeat 7 (first incorporations)))
                 (seq incorporations) nil]]
      (is (= :invalid-incorporations
             (error-kind #(evidence-for initial bad base)))))))

(deftest unsupported-raw-tx-assertions-are-not-assumed-to-pass-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        base (reference-observations initial incorporations [0 1])
        modified (assoc-in incorporations
                           [0 :plan :assertions]
                           ["ASSERT EXISTS (SELECT 1 FROM requests)"])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (evidence-for initial modified base)))))

(deftest correct-looking-observations-are-not-authenticated-test
  (let [{:keys [initial incorporations]} (prepared-pair)
        ;; No XTDB calls here: observations are *fabricated* directly from
        ;; the abstract machine. This MUST stay consistent, illustrating why
        ;; the checker cannot be treated as authentication or a certificate.
        invented (reference-observations initial incorporations [0 1])
        result (evidence-for initial incorporations invented)]
    (is (= :consistent (:status result)))
    (is (contains? (get-in result [:scope :not-established])
                   :authenticated-observation-provenance))))

(deftest absent-policy-guard-cannot-be-inferred-from-transaction-model-test
  (let [{:keys [initial request membership]} (fixture)
        revoked (after membership {:active false})
        after-revoke (assoc initial [:memberships (:xt/id membership)] revoked)
        unguarded (incorporation request membership
                                 (str "unguarded-" (:xt/id request)) "helper-a" false)
        incs [unguarded]
        obs (reference-observations after-revoke incs [0])
        result (evidence-for after-revoke incs obs)]
    (is (= false (:active revoked)))
    (is (= [:committed] (:statuses obs)))
    (is (= :consistent (:status result)))
    (is (= "helper-a"
           (:owner (get-in obs [:state [:requests (:xt/id request)]]))))
    (is (contains? (get-in result [:scope :not-established])
                   :policy-guard-completeness))))

(defn- row [table id]
  (let [sql (case table
              :requests "SELECT * FROM requests WHERE _id = ?"
              :memberships "SELECT * FROM memberships WHERE _id = ?"
              :gesso_commands "SELECT * FROM gesso_commands WHERE _id = ?")
        rows (xt/q (node) [sql id])]
    (when-not (<= (count rows) 1)
      (throw (ex-info "Unexpected duplicate XTDB documents." {:table table :id id})))
    (first rows)))

(defn- persisted [tracked]
  (into {}
        (keep (fn [[table id :as target]]
                (when-let [doc (row table id)]
                  [target doc])))
        tracked))

(defn- classify! [incp]
  (try
    (xt/execute-tx (node) (:tx-ops (tx/prepare {} (:plan incp))))
    :committed
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        :rejected
        (throw error)))))

(defn- seed! [{:keys [request membership]}]
  (xt/execute-tx (node) [[:put-docs :requests request]
                         [:put-docs :memberships membership]]))

(deftest actual-xtdb-competing-commands-correspond-in-both-orders-test
  (doseq [order [[0 1] [1 0]]]
    (let [{:keys [initial] :as f} (fixture)
          incs (claims f)
          tracked (targets initial incs)]
      (seed! f)
      (let [statuses (reduce (fn [v idx]
                               (assoc v idx (classify! (nth incs idx))))
                             [nil nil] order)
            observations {:statuses statuses
                          :state (persisted tracked)
                          :inspected-targets tracked}
            result (evidence-for initial incs observations)
            winner (first order)
            loser (second order)
            winner-target [:gesso_commands (:xt/id (:record (nth incs winner)))]
            loser-target [:gesso_commands (:xt/id (:record (nth incs loser)))]]
        (is (= :committed (nth statuses winner)))
        (is (= :rejected (nth statuses loser)))
        (is (= :consistent (:status result)))
        (is (= [order] (:matching-orders result)))
        (is (= (get-in observations [:state winner-target])
               (:record (nth incs winner))))
        (is (nil? (get-in observations [:state loser-target])))
        (is (= (if (= 0 winner) "helper-a" "helper-b")
               (:owner (get-in observations
                               [:state [:requests (:xt/id (:request f))]]))))))))

(deftest actual-xtdb-rejected-duplicate-leaves-no-second-record-test
  (let [{:keys [initial] :as f} (fixture)
        incp (first (claims f))
        tracked (targets initial [incp])]
    (seed! f)
    (is (= :committed (classify! incp)))
    (is (= :rejected (classify! incp)))
    ;; Only physical XTDB transactions are fed to the witness checker;
    ;; replay reconciliation belongs to a distinct semantic boundary.
    (let [observations {:statuses [:committed :rejected]
                        :state (persisted tracked)
                        :inspected-targets tracked}
          result (evidence-for initial [incp incp] observations)]
      (is (= :consistent (:status result)))
      (is (= [[0 1]] (:matching-orders result)))
      (is (= (:record incp)
             (get-in observations [:state [:gesso_commands (:xt/id (:record incp))]]))))))

(ns gesso.model.resolution-passive-observation-test
  "Permanent regressions for the v807 controlled, passive XTDB observer.

   Begin and completion must NEVER invoke or submit application transactions.
   The tests execute physical transactions independently through tx/prepare
   and xtdb.api/execute-tx, then provide their definitive outcomes. A passing
   finite witness DOES NOT authenticate those outcomes or the capture handle,
   establish snapshot isolation, prove that an application submitted the
   declared plan, or verify omitted business guards."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-passive-observation :as passive]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private now "2026-10-12T12:00:00Z")
(defonce ^:private !node (atom nil))

(defn- with-database [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-database)

(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB fixture is not running." {}))))

(defn- request [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- after [before changes]
  (-> before
      (merge changes)
      (update :revision clojure.core/inc)
      (assoc :updated-at now)))

(defn- fixture! []
  (let [suffix (str (random-uuid))
        req (request (str "passive-request-" suffix))
        member (membership (str "passive-member-" suffix))]
    (xt/execute-tx (node) [[:put-docs :requests req]
                           [:put-docs :memberships member]])
    {:request req :membership member}))

(defn- incorporation
  ([req member tag owner]
   (incorporation req member tag owner true))
  ([req member tag owner guard-membership?]
   (resolution/incorporation-plan
    {:binding {:command-id (identity/command-id
                            (str "passive-" tag "-" (random-uuid)))
               :principal (identity/principal owner)
               :operation :request/claim
               :arguments {:request-id (:xt/id req)}}
     :outcome {:status :claimed :owner owner}
     :now now
     :plan {:commands [(command/update-command
                        :requests :claim req (after req {:owner owner}) version)]
            :guards (if guard-membership?
                      [(command/guard :memberships member version)]
                      [])
            :emit false}})))

(defn- command-target [incorporation]
  [resolution/command-entity-type (get-in incorporation [:record :xt/id])])

(defn- document [table id]
  (first (xt/q (node)
               [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- capture! [incs]
  (passive/begin-observation! {:node (node) :incorporations incs}))

(defn- complete! [capture statuses]
  (passive/complete-observation! capture statuses))

(defn- submit! [incorporation]
  ;; This is test-owned physical execution, not a callback into the observer.
  (try
    (xt/execute-tx (node) (:tx-ops (tx/prepare {} (:plan incorporation))))
    :committed
    (catch Throwable error
      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data error)))
        :rejected
        (throw error)))))

(defn- error-kind [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (is (= ::passive/invalid-passive-observation
             (:error/type (ex-data error)))
          (str "Expected typed passive-observation error, got: "
               (pr-str (ex-data error))))
      (:error/kind (ex-data error)))))

(deftest scope-is-explicit-about-collected-versus-trusted-facts-test
  (let [{:keys [collector collected assumed not-established]}
        passive/observation-scope]
    (is (= :gesso.model.resolution-passive-observation/xtdb-before-after-v1
           collector))
    (doseq [property [:physical-initial-documents-from-xtdb
                      :physical-final-documents-from-xtdb
                      :guard-and-write-target-coverage
                      :finite-serial-reference-comparison
                      :no-transaction-submission-by-collector]]
      (is (contains? collected property)))
    (doseq [assumption [:trusted-physical-transaction-status-inputs
                        :supplied-plans-equal-executed-plans
                        :controlled-execution-without-unrelated-writes
                        :consistent-series-of-unpinned-xtdb-reads
                        :synchronous-physical-outcomes-settled-before-final-read
                        :supplied-node-is-the-relevant-authoritative-database]]
      (is (contains? assumed assumption)))
    (doseq [limitation [:authenticated-status-or-capture-provenance
                        :automatic-observation-of-application-transaction-statuses
                        :single-snapshot-cross-table-read
                        :application-execution-correspondence-without-trusted-inputs
                        :live-settlement-or-projection-correctness
                        :business-authorization-or-invariant-correctness
                        :policy-guard-completeness
                        :safe-retry-after-indeterminate-delivery
                        :unbounded-concurrency-or-linearizability
                        :formal-refinement-proof]]
      (is (contains? not-established limitation)))))

(deftest begin-captures-complete-authoritative-initial-state-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "capture" "helper-a")
        capture (capture! [inc])
        target (command-target inc)]
    (is (= passive/observation-scope (:scope capture)))
    (is (= [inc] (:incorporations capture)))
    (is (identical? (node) (:node capture)))
    (is (= #{[:requests (:xt/id request)]
             [:memberships (:xt/id membership)]
             target}
           (:inspected-targets capture)))
    (is (= request (get-in capture [:initial-state
                                   [:requests (:xt/id request)]])))
    (is (= membership (get-in capture [:initial-state
                                      [:memberships (:xt/id membership)]])))
    (is (not (contains? (:initial-state capture) target)))
    (is (nil? (document :gesso_commands (second target))))
    (is (= "unclaimed" (:owner (document :requests (:xt/id request)))))))

(deftest both-passive-phases-are-read-only-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "readonly" "helper-a")
        submissions (atom 0)
        result (with-redefs [xt/execute-tx
                             (fn [& _]
                               (swap! submissions clojure.core/inc)
                               (throw (ex-info "Observer attempted to submit." {})))]
                 (let [capture (capture! [inc])]
                   (complete! capture [:committed])))]
    (is (zero? @submissions))
    (is (= :counterexample (get-in result [:evidence :status])))
    (is (= "unclaimed" (:owner (document :requests (:xt/id request)))))
    (is (nil? (document :gesso_commands
                        (second (command-target inc)))))))

(deftest externally-committed-incorporation-corresponds-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "success" "helper-a")
        capture (capture! [inc])]
    (is (= :committed (submit! inc)))
    (let [result (complete! capture [:committed])
          state (get-in result [:observations :state])]
      (is (= passive/observation-scope (:collection result)))
      (is (= (:initial-state capture) (:initial-state result)))
      (is (= [:committed] (get-in result [:observations :statuses])))
      (is (= (:inspected-targets capture)
             (get-in result [:observations :inspected-targets])))
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= [[0]] (get-in result [:evidence :matching-orders])))
      (is (= (:record inc) (get state (command-target inc))))
      (is (= (:record inc)
             (document :gesso_commands (second (command-target inc)))))
      (is (= "helper-a" (get-in state
                                [[:requests (:xt/id request)] :owner])))
      (is (= 1 (:revision (document :requests (:xt/id request))))))))

(deftest competing-external-claims-match-either-serial-order-test
  (doseq [order [[0 1] [1 0]]]
    (let [{:keys [request membership]} (fixture!)
          incs [(incorporation request membership "a" "helper-a")
                (incorporation request membership "b" "helper-b")]
          capture (capture! incs)
          statuses (reduce (fn [acc index]
                             (assoc acc index (submit! (nth incs index))))
                           [nil nil] order)
          result (complete! capture statuses)
          winner (first order)
          loser (second order)]
      (is (= (assoc [:rejected :rejected] winner :committed) statuses))
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= [order] (get-in result [:evidence :matching-orders])))
      (is (= (:record (nth incs winner))
             (get-in result [:observations :state
                             (command-target (nth incs winner))])))
      (is (not (contains? (get-in result [:observations :state])
                          (command-target (nth incs loser)))))
      (is (= (str "helper-" (if (zero? winner) "a" "b"))
             (:owner (document :requests (:xt/id request))))))))

(deftest repeated-command-delivery-incorporates-only-once-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "duplicate" "helper-a")
        capture (capture! [inc inc])
        results [(submit! inc) (submit! inc)]
        outcome (complete! capture results)]
    (is (= [:committed :rejected] results))
    (is (= :consistent (get-in outcome [:evidence :status])))
    (is (= [[0 1]] (get-in outcome [:evidence :matching-orders])))
    (is (= 1 (:revision (document :requests (:xt/id request)))))
    (is (= (:record inc)
           (document :gesso_commands (second (command-target inc)))))))

(deftest independent-external-claims-both-commit-test
  (let [{req-a :request mem-a :membership} (fixture!)
        {req-b :request mem-b :membership} (fixture!)
        a (incorporation req-a mem-a "independent-a" "helper-a")
        b (incorporation req-b mem-b "independent-b" "helper-b")
        capture (capture! [a b])]
    (is (= :committed (submit! b)))
    (is (= :committed (submit! a)))
    (let [result (complete! capture [:committed :committed])]
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= #{[0 1] [1 0]}
             (set (get-in result [:evidence :matching-orders]))))
      (is (= "helper-a" (:owner (document :requests (:xt/id req-a)))))
      (is (= "helper-b" (:owner (document :requests (:xt/id req-b)))))
      (is (= (:record a)
             (document :gesso_commands (second (command-target a)))))
      (is (= (:record b)
             (document :gesso_commands (second (command-target b))))))))

(deftest stale-membership-guard-rejects-without-partial-record-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "revoked" "helper-a")]
    ;; Revoke *before* capturing. This is an input state of the reference
    ;; machine, not an unrelated write during the passive observation window.
    (xt/execute-tx (node) [[:put-docs :memberships
                            (after membership {:active false})]])
    (let [capture (capture! [inc])]
      (is (= false
             (get-in capture [:initial-state
                              [:memberships (:xt/id membership)] :active])))
      (is (= :rejected (submit! inc)))
      (let [result (complete! capture [:rejected])]
        (is (= :consistent (get-in result [:evidence :status])))
        (is (= [[0]] (get-in result [:evidence :matching-orders])))
        (is (= "unclaimed" (:owner (document :requests (:xt/id request)))))
        (is (nil? (document :gesso_commands
                            (second (command-target inc)))))))))

(deftest omitted-policy-guard-can-still-produce-consistent-witness-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "unguarded" "helper-a" false)]
    (xt/execute-tx (node) [[:put-docs :memberships
                            (after membership {:active false})]])
    (let [capture (capture! [inc])]
      (is (not (contains? (:inspected-targets capture)
                          [:memberships (:xt/id membership)])))
      (is (= :committed (submit! inc)))
      (let [result (complete! capture [:committed])]
        (is (= :consistent (get-in result [:evidence :status])))
        (is (= "helper-a" (:owner (document :requests (:xt/id request)))))
        (is (false? (:active (document :memberships (:xt/id membership)))))
        (is (contains? (get-in result [:collection :not-established])
                       :policy-guard-completeness))))))

(deftest missing-or-falsely-reported-physical-status-is-counterexample-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "false-report" "helper-a")
        capture (capture! [inc])]
    (is (= :counterexample
           (get-in (complete! capture [:committed]) [:evidence :status]))
        "A claimed commit without a physical write cannot match the model.")
    (is (= :counterexample
           (get-in (complete! capture [:rejected]) [:evidence :status]))
        "A claimed rejection of an otherwise admissible transaction cannot match the model.")
    (is (= :committed (submit! inc)))
    (let [result (complete! capture [:rejected])]
      (is (= :counterexample (get-in result [:evidence :status])))
      (is (nil? (get-in result [:evidence :witness])))
      (is (= :committed (first (get-in result [:evidence :expected 0 :statuses]))))
      (is (= (:record inc)
             (document :gesso_commands (second (command-target inc))))))))

(deftest incorrect-contender-status-vector-produces-counterexample-test
  (let [{:keys [request membership]} (fixture!)
        a (incorporation request membership "wrong-a" "helper-a")
        b (incorporation request membership "wrong-b" "helper-b")
        capture (capture! [a b])]
    (is (= :committed (submit! b)))
    (is (= :rejected (submit! a)))
    (is (= [[1 0]] (get-in (complete! capture [:rejected :committed])
                            [:evidence :matching-orders])))
    (doseq [statuses [[:committed :rejected]
                      [:committed :committed]
                      [:rejected :rejected]]]
      (let [result (complete! capture statuses)]
        (is (= :counterexample (get-in result [:evidence :status])))
        (is (nil? (get-in result [:evidence :witness])))))))

(deftest invalid-statuses-never-read-final-state-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "statuses" "helper-a")
        capture (capture! [inc])
        reads (atom 0)]
    (with-redefs [xt/q (fn [& _]
                         (swap! reads clojure.core/inc)
                         (throw (ex-info "Unexpected query" {})))]
      (doseq [statuses [nil [] [:unknown] [:unresolved] [:failed]
                        [:reconciled] [:already-incorporated] [:confirmed]
                        [true] '( :committed ) [:committed :rejected]]]
        (is (= :invalid-statuses
               (error-kind #(complete! capture statuses)))
            (str "Unexpected admission of physical status " (pr-str statuses)))))
    (is (zero? @reads))))

(deftest malformed-begin-requests-are-rejected-before-reads-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "malformed" "helper-a")
        valid {:node (node) :incorporations [inc]}
        bad-plan (update inc :plan assoc :assertions ["SELECT 1"])
        other-record (assoc-in inc [:record :gesso.command/outcome]
                               {:status :fabricated})
        variants [[nil :invalid-request]
                  [(assoc valid :extra true) :invalid-request]
                  [(dissoc valid :incorporations) :invalid-request]
                  [(assoc valid :node nil) :missing-node]
                  [(assoc valid :incorporations []) :invalid-incorporations]
                  [(assoc valid :incorporations (vec (repeat 7 inc)))
                   :invalid-incorporations]
                  [(assoc valid :incorporations (list inc))
                   :invalid-incorporations]
                  [(assoc valid :incorporations [(assoc inc :extra true)])
                   :invalid-incorporation]
                  [(assoc valid :incorporations [(dissoc inc :record)])
                   :invalid-incorporation]
                  [(assoc valid :incorporations [(assoc inc :scope {:fake true})])
                   :invalid-incorporation-scope]]
        reads (atom 0)]
    (with-redefs [xt/q (fn [& _]
                         (swap! reads clojure.core/inc)
                         (throw (ex-info "Unexpected query" {})))]
      (doseq [[input expected] variants]
        (is (= expected
               (error-kind #(passive/begin-observation! input)))
            (pr-str (dissoc (or input {}) :node))))
      ;; Errors from the nested admitted-plan / record validators also happen
      ;; before physical reads, even though they use their own typed errors.
      (is (thrown? clojure.lang.ExceptionInfo
                   (passive/begin-observation!
                    (assoc valid :incorporations [bad-plan]))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (passive/begin-observation!
                    (assoc valid :incorporations [other-record])))))
    (is (zero? @reads))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest hostile-table-identifier-is-rejected-before-query-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "sql-table" "helper-a")
        bad (update-in inc [:plan :guards]
                       conj (command/guard :not-safe/table
                                           {:xt/id "x" :revision 0
                                            :created-at "t0" :updated-at "t0"}
                                           version))
        reads (atom 0)]
    (with-redefs [xt/q (fn [& _] (swap! reads clojure.core/inc) [])]
      (is (= :unsupported-table
             (error-kind #(capture! [bad])))))
    (is (zero? @reads))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest malformed-captures-rejected-before-final-reads-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "capture-forgery" "helper-a")
        capture (capture! [inc])
        nonexistent [:requests "invented"]
        mutations [[(assoc capture :unexpected 1) :invalid-capture]
                   [(dissoc capture :scope) :invalid-capture]
                   [(assoc capture :scope {:fabricated true}) :invalid-capture]
                   [(assoc capture :kind :fake) :invalid-capture]
                   [(assoc capture :node nil) :invalid-capture]
                   [(assoc capture :incorporations []) :invalid-incorporations]
                   [(assoc capture :inspected-targets #{}) :capture-target-mismatch]
                   [(update capture :inspected-targets conj nonexistent)
                    :capture-target-mismatch]
                   [(assoc-in capture [:initial-state nonexistent]
                              (assoc request :xt/id "invented"))
                    :capture-state-mismatch]]
        reads (atom 0)]
    (with-redefs [xt/q (fn [& _]
                         (swap! reads clojure.core/inc)
                         (throw (ex-info "Unexpected query" {})))]
      (doseq [[candidate expected] mutations]
        (is (= expected (error-kind #(complete! candidate [:rejected])))
            (pr-str (dissoc candidate :node)))))
    (is (zero? @reads))))

(deftest query-failures-propagate-without-generated-evidence-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "query-failure" "helper-a")
        failed (ex-info "Authoritative read unavailable" {:cause :test/read-failed})]
    (is (identical? failed
                    (try
                      (with-redefs [xt/q (fn [& _] (throw failed))]
                        (capture! [inc]))
                      nil
                      (catch Throwable error error))))
    (let [capture (capture! [inc])]
      (is (identical? failed
                      (try
                        (with-redefs [xt/q (fn [& _] (throw failed))]
                          (complete! capture [:committed]))
                        nil
                        (catch Throwable error error))))
      (is (nil? (document :gesso_commands (second (command-target inc))))))))

(deftest duplicate-physical-rows-fail-closed-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "duplicate-rows" "helper-a")
        capture (capture! [inc])]
    (is (= :duplicate-document
           (with-redefs [xt/q (fn [& _] [request request])]
             (error-kind #(capture! [inc])))))
    (is (= :duplicate-document
           (with-redefs [xt/q (fn [& _] [request request])]
             (error-kind #(complete! capture [:committed])))))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest altered-final-read-cannot-silently-become-a-witness-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "altered-read" "helper-a")
        capture (capture! [inc])]
    (is (= :committed (submit! inc)))
    (let [original-q xt/q
          result (with-redefs [xt/q (fn [& args]
                                     (let [query (second args)
                                           rows (apply original-q args)]
                                       (if (str/includes? (first query)
                                                          "FROM requests")
                                         (mapv #(assoc % :owner "forged") rows)
                                         rows)))]
                   (complete! capture [:committed]))]
      (is (= :counterexample (get-in result [:evidence :status])))
      (is (= :persisted-state-disagrees
             (get-in result [:evidence :reason])))
      (is (nil? (get-in result [:evidence :witness])))
      (is (= "helper-a" (:owner (document :requests (:xt/id request))))))))

(deftest capture-can-be-copied-so-success-is-not-authenticated-test
  (let [{:keys [request membership]} (fixture!)
        inc (incorporation request membership "replay-copy" "helper-a")
        capture (capture! [inc])
        indistinguishable-copy (into {} capture)]
    (is (= capture indistinguishable-copy))
    (is (= :committed (submit! inc)))
    (is (= :consistent
           (get-in (complete! indistinguishable-copy [:committed])
                   [:evidence :status])))
    (is (contains? (:not-established passive/observation-scope)
                   :authenticated-status-or-capture-provenance))))

(ns gesso.model.resolution-observation-test
  "Permanent regressions for the v805 controlled-XTDB finite observation collector.

   These use real embedded XTDB for submitted/inspected document outcomes and
   narrowly scoped fault injection for admission, read integrity and unknown
   transaction errors. They do NOT establish authenticated observation origin,
   snapshot consistency between independent queries, business authorization,
   safe retry after indeterminate delivery or general concurrent linearizability.
   The collector itself submits writes: never use these tests against a live DB."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-observation :as observation]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(def ^:private document-version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private now "2026-10-12T12:00:00Z")
(defonce ^:private !node (atom nil))

(defn- with-embedded-xtdb [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-embedded-xtdb)

(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB is not started." {}))))

(defn- request [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :owner "unclaimed"})

(defn- membership [id]
  {:xt/id id :revision 0 :created-at "t0" :updated-at "t0"
   :active true})

(defn- after [before changes]
  (-> before
      (merge changes)
      (update :revision inc)
      (assoc :updated-at now)))

(defn- fixture []
  (let [suffix (str (random-uuid))
        req (request (str "collector-request-" suffix))
        mem (membership (str "collector-member-" suffix))]
    (xt/execute-tx (node) [[:put-docs :requests req]
                           [:put-docs :memberships mem]])
    {:request req :membership mem}))

(defn- incorporation
  ([req mem tag owner]
   (incorporation req mem tag owner true))
  ([req mem tag owner guarded?]
   (resolution/incorporation-plan
    {:binding {:command-id (identity/command-id (str "collector-" tag "-" (random-uuid)))
               :principal (identity/principal owner)
               :operation :request/claim
               :arguments {:request-id (:xt/id req)}}
     :outcome {:status :claimed :owner owner}
     :now now
     :plan {:commands [(command/update-command
                        :requests :claim req (after req {:owner owner}) document-version)]
            :guards (if guarded?
                      [(command/guard :memberships mem document-version)]
                      [])
            :emit false}})))

(defn- collect! [incs order]
  (observation/observe-and-check!
   {:node (node) :incorporations incs :submission-order order}))

(defn- command-target [inc]
  [resolution/command-entity-type (get-in inc [:record :xt/id])])

(defn- document [table id]
  (first (xt/q (node) [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- error-kind [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (is (= ::observation/invalid-observation-request (:error/type (ex-data e)))
          (str "Expected collector admission error, got " (pr-str (ex-data e))))
      (:error/kind (ex-data e)))))

(defn- base-input [incs order]
  {:node (node) :incorporations incs :submission-order order})

(deftest observation-scope-does-not-promise-more-than-collected-test
  (let [{:keys [collector collected assumed not-established]} observation/observation-scope]
    (is (= :gesso.model.resolution-observation/direct-xtdb-v1 collector))
    (doseq [k [:initial-documents-read-from-xtdb
               :transaction-submission-through-model-tx-prepare
               :physical-xtdb-commit-and-assertion-rejection-outcomes
               :final-documents-read-from-xtdb
               :checked-against-finite-serial-reference]]
      (is (contains? collected k)))
    (doseq [k [:exclusive-control-of-relevant-documents-during-collection
               :consistent-series-of-xtdb-reads
               :synchronous-execute-tx-outcome-is-definitive]]
      (is (contains? assumed k)))
    (doseq [k [:authenticated-observation-provenance
               :single-snapshot-cross-table-read
               :production-live-execution-correspondence
               :safe-retry-of-indeterminate-delivery
               :authorization-or-domain-invariant-correctness
               :unbounded-concurrency-or-linearizability
               :formal-refinement-proof]]
      (is (contains? not-established k)))))

(deftest successful-incorporation-reads-all-physical-records-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "success" "helper-a")
        result (collect! [inc] [0])
        state (get-in result [:observations :state])
        target (command-target inc)]
    (is (= observation/observation-scope (:collection result)))
    (is (= [0] (:submission-order result)))
    (is (= [:committed] (get-in result [:observations :statuses])))
    (is (= :consistent (get-in result [:evidence :status])))
    (is (= [[0]] (get-in result [:evidence :matching-orders])))
    (is (= request (get-in result [:initial-state [:requests (:xt/id request)]])))
    (is (not (contains? (:initial-state result) target)))
    (is (= (:record inc) (get state target)))
    (is (= (:record inc) (document :gesso_commands (second target))))
    (is (= "helper-a" (get-in state [[:requests (:xt/id request)] :owner])))
    (is (= #{[:requests (:xt/id request)]
             [:memberships (:xt/id membership)]
             target}
           (get-in result [:observations :inspected-targets])))))

(deftest competing-claims-respect-explicit-order-and-plan-indices-test
  (doseq [order [[0 1] [1 0]]]
    (let [{:keys [request membership]} (fixture)
          incs [(incorporation request membership "a" "helper-a")
                (incorporation request membership "b" "helper-b")]
          result (collect! incs order)
          winner (first order)
          loser (second order)
          state (get-in result [:observations :state])]
      (is (= (assoc [:rejected :rejected] winner :committed)
             (get-in result [:observations :statuses])))
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= [order] (get-in result [:evidence :matching-orders])))
      (is (= (:record (nth incs winner)) (get state (command-target (nth incs winner)))))
      (is (not (contains? state (command-target (nth incs loser)))))
      (is (contains? (get-in result [:observations :inspected-targets])
                     (command-target (nth incs loser))))
      (is (= (str "helper-" (if (= winner 0) "a" "b"))
             (:owner (document :requests (:xt/id request))))))))

(deftest duplicate-delivery-does-not-write-a-second-command-record-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "duplicate" "helper-a")
        result (collect! [inc inc] [0 1])]
    (is (= [:committed :rejected] (get-in result [:observations :statuses])))
    (is (= :consistent (get-in result [:evidence :status])))
    (is (= [[0 1]] (get-in result [:evidence :matching-orders])))
    (is (= 1 (:revision (document :requests (:xt/id request)))))
    (is (= (:record inc) (document :gesso_commands (second (command-target inc)))))))

(deftest independent-requests-both-commit-test
  (let [{req-a :request mem-a :membership} (fixture)
        {req-b :request mem-b :membership} (fixture)
        incs [(incorporation req-a mem-a "independent-a" "helper-a")
              (incorporation req-b mem-b "independent-b" "helper-b")]
        result (collect! incs [1 0])]
    (is (= [:committed :committed] (get-in result [:observations :statuses])))
    (is (= :consistent (get-in result [:evidence :status])))
    (is (= #{[0 1] [1 0]} (set (get-in result [:evidence :matching-orders]))))
    (is (= "helper-a" (:owner (document :requests (:xt/id req-a)))))
    (is (= "helper-b" (:owner (document :requests (:xt/id req-b)))))
    (doseq [inc incs]
      (is (= (:record inc) (document :gesso_commands (second (command-target inc))))))))

(deftest stale-membership-guard-rejects-without-command-record-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "stale" "helper-a")]
    (xt/execute-tx (node) [[:put-docs :memberships (after membership {:active false})]])
    (let [result (collect! [inc] [0])]
      (is (= [:rejected] (get-in result [:observations :statuses])))
      (is (= :consistent (get-in result [:evidence :status])))
      (is (nil? (document :gesso_commands (second (command-target inc)))))
      (is (= "unclaimed" (:owner (document :requests (:xt/id request)))))
      (is (= false (get-in result [:initial-state [:memberships (:xt/id membership)] :active]))))))

(deftest omitted-membership-guard-is-not-inferred-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "unguarded" "helper-a" false)]
    (xt/execute-tx (node) [[:put-docs :memberships (after membership {:active false})]])
    (let [result (collect! [inc] [0])]
      (is (= [:committed] (get-in result [:observations :statuses])))
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= "helper-a" (:owner (document :requests (:xt/id request)))))
      (is (= false (:active (document :memberships (:xt/id membership)))))
      (is (not (contains? (get-in result [:observations :inspected-targets])
                          [:memberships (:xt/id membership)]))
          "An omitted business dependency is not inferred by the collector."))))

(deftest malformed-inputs-fail-before-any-transaction-submission-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "bad-input" "helper-a")
        proper (base-input [inc] [0])
        variants [[(assoc proper :extra true) :invalid-input]
                  [(dissoc proper :node) :invalid-input]
                  [(assoc proper :node nil) :missing-node]
                  [(assoc proper :incorporations []) :invalid-incorporations]
                  [(assoc proper :incorporations (vec (repeat 7 inc))) :invalid-incorporations]
                  [(assoc proper :incorporations (list inc)) :invalid-incorporations]
                  [(assoc proper :submission-order []) :invalid-submission-order]
                  [(assoc proper :submission-order [1]) :invalid-submission-order]
                  [(assoc proper :submission-order [0 0]) :invalid-submission-order]
                  [(assoc proper :submission-order '(0)) :invalid-submission-order]
                  [(assoc proper :incorporations [(assoc inc :unexpected true)]) :invalid-incorporation]
                  [(assoc proper :incorporations [(dissoc inc :record)]) :invalid-incorporation]
                  [(assoc proper :incorporations [(assoc inc :scope {:forged true})]) :invalid-incorporation]
                  [(assoc proper :incorporations [(assoc-in inc [:record :gesso.command/outcome] {:forged true})])
                   :invalid-durable-incorporation]]
        attempted (atom 0)]
    (with-redefs [xt/execute-tx (fn [& _]
                                (swap! attempted clojure.core/inc)
                                (throw (ex-info "Unexpected transaction" {})))]
      (doseq [[input expected] variants]
        (is (= expected (error-kind #(observation/observe-and-check! input)))
            (str "Unexpected admission result for " (pr-str (dissoc input :node))))))
    (is (zero? @attempted))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest unsupported-table-identifiers-are-rejected-before-writing-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "unsafe-table" "helper-a")
        ; A valid-looking extra version guard is not safe to splice into SQL.
        bad (update-in inc [:plan :guards]
                       conj (command/guard :unsafe-table-name
                                           {:xt/id "some-id" :revision 0
                                            :created-at "t0" :updated-at "t0"}
                                           document-version))
        attempted (atom 0)]
    (with-redefs [xt/execute-tx (fn [& _] (swap! attempted clojure.core/inc) :unexpected)]
      (is (= :unsupported-table-name
             (error-kind #(collect! [bad] [0])))))
    (is (zero? @attempted))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest unknown-transaction-error-is-never-reclassified-as-rejection-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "lost-ack" "helper-a")
        calls (atom 0)
        error (ex-info "acknowledgment lost" {:cause :unknown-delivery})]
    (is (identical?
         error
         (try
           (with-redefs [xt/execute-tx (fn [& _] (swap! calls clojure.core/inc) (throw error))]
             (collect! [inc] [0]))
           nil
           (catch Throwable t t))))
    (is (= 1 @calls))
    (is (nil? (document :gesso_commands (second (command-target inc)))))))

(deftest partial-execution-followed-by-an-unknown-outcome-stops-immediately-test
  (let [{:keys [request membership]} (fixture)
        incs [(incorporation request membership "first" "helper-a")
              (incorporation request membership "second" "helper-b")]
        calls (atom 0)
        original xt/execute-tx
        error (ex-info "indeterminate second submit" {:cause :lost-ack})]
    (is (identical?
         error
         (try
           (with-redefs [xt/execute-tx
                         (fn [& args]
                           ;; xt/execute-tx's public 2-arity delegates to the
                           ;; public 3-arity; do not count that as another
                           ;; transaction or inject a failure mid-delegation.
                           (if (= 3 (count args))
                             (apply original args)
                             (if (= 1 (swap! calls clojure.core/inc))
                               (apply original args)
                               (throw error))))]
             (collect! incs [0 1]))
           nil
           (catch Throwable t t))))
    (is (= 2 @calls))
    (is (= "helper-a" (:owner (document :requests (:xt/id request)))))
    (is (= (:record (first incs))
           (document :gesso_commands (second (command-target (first incs))))))
    (is (nil? (document :gesso_commands (second (command-target (second incs))))))))

(deftest tampered-final-read-yields-counterexample-not-success-test
  (let [{:keys [request membership]} (fixture)
        inc (incorporation request membership "tampered" "helper-a")
        q-original xt/q
        reads (atom 0)
        result (with-redefs [xt/q (fn [& args]
                                     (let [n (swap! reads clojure.core/inc)
                                           query (second args)
                                           rows (apply q-original args)]
                                       (if (and (> n 3)
                                                (str/includes? (first query) "FROM requests"))
                                         (mapv #(assoc % :owner "fabricated-owner") rows)
                                         rows)))]
                 (collect! [inc] [0]))]
    (is (= [:committed] (get-in result [:observations :statuses])))
    (is (= :counterexample (get-in result [:evidence :status])))
    (is (= :persisted-state-disagrees (get-in result [:evidence :reason])))
    (is (nil? (get-in result [:evidence :witness])))
    (is (= "helper-a" (:owner (document :requests (:xt/id request))))
        "Only the test's collector reads were altered; XTDB stored the real value.")))

(deftest duplicate-database-rows-fail-before-submission-test
  (let [{:keys [request membership]} (fixture)
        incorporation (incorporation request membership "duplicate-rows" "helper-a")
        submits (atom 0)]
    (is (= :multiple-rows
           (with-redefs [xt/q (fn [& _] [request request])
                         xt/execute-tx (fn [& _]
                                         (swap! submits clojure.core/inc)
                                         :unexpected)]
             (error-kind #(collect! [incorporation] [0])))))
    (is (zero? @submits))
    (is (nil? (document :gesso_commands (second (command-target incorporation)))))))

(deftest unknown-authoritative-read-error-propagates-before-submission-test
  (let [{:keys [request membership]} (fixture)
        incorporation (incorporation request membership "read-error" "helper-a")
        submits (atom 0)
        read-error (ex-info "read unavailable" {:error/type :database/read-unavailable})]
    (is (identical? read-error
                    (try
                      (with-redefs [xt/q (fn [& _] (throw read-error))
                                    xt/execute-tx (fn [& _]
                                                    (swap! submits clojure.core/inc)
                                                    :unexpected)]
                        (collect! [incorporation] [0]))
                      nil
                      (catch Throwable error error))))
    (is (zero? @submits))
    (is (nil? (document :gesso_commands (second (command-target incorporation)))))))

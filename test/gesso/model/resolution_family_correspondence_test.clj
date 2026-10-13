(ns gesso.model.resolution-family-correspondence-test
  "Permanent regression coverage for v826's single-attempt comparison between
   a developer-declared finite operation family and the domain plan actually
   submitted through Gesso's resolved-operation/XTDB execution boundary.

   These are bounded diagnostic comparisons, not application certification:
   the family callbacks are trusted, detached observation maps can be forged,
   and the checked operation instance is not proved to cover all principals,
   arguments, reachable states, or registered application operations."
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.resolution-event-observation :as bridge]
   [gesso.model.resolved-operation :as resolved]
   [gesso.model.tx :as tx]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))

(defn- node [] @!node)

(defn- with-db [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-db)

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- read-row [table id]
  (first (xt/q (node) [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- fixture!
  ([] (fixture! {}))
  ([{:keys [domain-guard?]
     :or {domain-guard? true}}]
   (let [suffix (str (random-uuid))
         request {:xt/id (str "v827-request-" suffix)
                  :revision 0 :created-at "t0" :updated-at "t0"
                  :owner "unclaimed"}
         membership {:xt/id (str "v827-membership-" suffix)
                     :revision 0 :created-at "t0" :updated-at "t0"
                     :active true}
         binding {:command-id (identity/command-id (str "v827-command-" suffix))
                  :principal (identity/principal "helper-a")
                  :operation :request/claim
                  :arguments {:request-id (:xt/id request)}}
         claimed (assoc request :revision 1 :updated-at "t1" :owner "helper-a")
         outcome {:status :claimed :owner "helper-a"}
         built (atom 0)
         domain-plan {:commands [(command/update-command :requests :claim
                                                         request claimed version)]
                      :guards (if domain-guard?
                                [(command/guard :memberships membership version)]
                                [])
                      :emit false}
         operation (resolved/build-operation
                    {:operation :request/claim
                     :read-record! (fn [_ row-id] (read-row :gesso_commands row-id))
                     :may-disclose? (fn [& _] true)
                     :construct! (fn [_ _]
                                   (swap! built inc)
                                   {:now "t1" :outcome outcome
                                    :plan domain-plan})})]
     (xt/execute-tx (node) [[:put-docs :requests request]
                            [:put-docs :memberships membership]])
     {:request request :membership membership :claimed claimed
      :binding binding :outcome outcome :domain-plan domain-plan
      :operation operation :built built})))

(defn- session [example]
  (bridge/start-session! {:node (node) :binding (:binding example)}))

(defn- execute! [example capture]
  ((:operation example)
   {:biff.xtdb/node (node)
    resolved/observation-context-key (:observe! capture)}
   {:binding (:binding example) :mode :submit}))

(defn- observe! [example]
  (let [capture (session example)
        result (execute! example capture)]
    {:execution result
     :observation ((:complete! capture))}))

(defn- model-family
  ([example] (model-family example {}))
  ([example {:keys [guard? owner enabled? extra-guard]
             :or {guard? true owner "helper-a" enabled? true}}]
   {:enabled?
    (fn [state principal arguments]
      (and enabled?
           (= principal "helper-a")
           (= arguments (get-in example [:binding :arguments]))
           (= (:request example)
              (get state [:requests (get-in example [:request :xt/id])]))))
    :prepare
    (fn [state _principal _arguments]
      (let [request (get state [:requests (get-in example [:request :xt/id])])
            membership (get state [:memberships (get-in example [:membership :xt/id])])
            updated (assoc request :revision 1 :updated-at "t1" :owner owner)]
        {:commands [(command/update-command :requests :claim request updated version)]
         :guards (vec (concat (if guard?
                                [(command/guard :memberships membership version)]
                                [])
                              (if extra-guard [extra-guard] [])))
         :emit false}))}))

(defn- compare!
  ([example observation family]
   (compare! example observation family "helper-a"
             (get-in example [:binding :arguments])))
  ([example observation family principal arguments]
   (bridge/compare-family-preparation observation family principal arguments)))

(defn- error-kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error
         (:error/kind (ex-data error)))))

(deftest scope-does-not-exaggerate-one-execution-correspondence-test
  (let [{:keys [checked assumed not-established]} bridge/family-correspondence-scope]
    (is (contains? checked :actual-event-pair-submitted-plan))
    (is (contains? checked :physical-finite-execution-witness))
    (is (contains? checked :modeled-enablement-for-observed-state))
    (is (contains? checked :exact-modeled-domain-plan-and-guards))
    (is (contains? assumed :truthful-finite-family-callbacks))
    (is (contains? assumed :observed-initial-state-covers-model-dependencies))
    (is (contains? not-established :authenticity-of-detached-observation-maps))
    (is (contains? not-established :model-coverage-of-all-application-executors))
    (is (contains? not-established :correctness-of-business-policy-or-guards))
    (is (contains? not-established :unbounded-refinement-or-certification))))

(deftest acknowledged-xtdb-incorporation-matches-declared-family-test
  (let [example (fixture!)
        {:keys [execution observation]} (observe! example)
        response (compare! example observation (model-family example))]
    (is (= :incorporated (:status execution)))
    (is (= :consistent (get-in observation [:evidence :status])))
    (is (= :committed (get-in observation [:physical :status])))
    (is (= :matched (:status response)))
    (is (= :exact-observed-domain-plan-match (:reason response)))
    (is (true? (:matching-guards? response)))
    (is (true? (:matching-commands? response)))
    (is (= :committed (:physical-status response)))
    (is (= bridge/family-correspondence-scope (:scope response)))
    (is (= (:claimed example)
           (read-row :requests (get-in example [:request :xt/id]))))
    (is (= (resolution/command-record (:binding example)
                                      (:outcome example) "t1")
           (read-row :gesso_commands
                     (resolution/record-id (:binding example)))))))

(deftest tagged-principal-and-its-raw-value-are-valid-comparison-inputs-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        raw-family (model-family example)
        tagged-family (assoc raw-family :enabled?
                             (fn [_ principal arguments]
                               (and (= (get-in example [:binding :principal]) principal)
                                    (= (get-in example [:binding :arguments]) arguments))))]
    (is (= :matched (:status (compare! example observation raw-family))))
    (is (= :matched (:status
                     (compare! example observation tagged-family
                               (get-in example [:binding :principal])
                               (get-in example [:binding :arguments])))))))

(deftest omission-of-a-real-membership-guard-is-a-counterexample-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        response (compare! example observation (model-family example {:guard? false}))]
    (is (= :counterexample (:status response)))
    (is (= :modeled-and-submitted-plans-disagree (:reason response)))
    (is (false? (:matching-guards? response)))
    (is (true? (:matching-commands? response)))))

(deftest an-extra-modeled-guard-disagrees-with-actual-execution-test
  (let [example (fixture! {:domain-guard? false})
        observation (:observation (observe! example))
        additional (command/guard :requests (:request example) version)
        response (compare! example observation
                           (model-family example {:guard? false
                                                  :extra-guard additional}))]
    (is (= :matched-serial-reference (get-in observation [:evidence :reason])))
    (is (= :counterexample (:status response)))
    (is (false? (:matching-guards? response)))
    (is (true? (:matching-commands? response)))))

(deftest wrong-owner-is-a-domain-mutation-counterexample-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        response (compare! example observation
                           (model-family example {:owner "helper-b"}))]
    (is (= :counterexample (:status response)))
    (is (true? (:matching-guards? response)))
    (is (false? (:matching-commands? response)))))

(deftest extra-declared-guard-is-not-silently-dropped-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        extra (command/guard :requests (:request example) version)
        response (compare! example observation
                           (model-family example {:extra-guard extra}))]
    (is (= :counterexample (:status response)))
    (is (false? (:matching-guards? response)))
    (is (true? (:matching-commands? response)))))

(deftest disabled-declared-operation-cannot-match-physical-execution-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        prepared (atom 0)
        response (compare! example observation
                           {:enabled? (fn [& _] false)
                            :prepare (fn [& _] (swap! prepared inc)
                                       (throw (ex-info "should never prepare" {})))})]
    (is (= :counterexample (:status response)))
    (is (= :actual-execution-while-model-disabled (:reason response)))
    (is (zero? @prepared))
    (is (= bridge/family-correspondence-scope (:scope response)))))

(deftest non-boolean-enablement-fails-closed-test
  (let [example (fixture!)
        observation (:observation (observe! example))]
    (doseq [value [nil :yes 1 "true" []]]
      (is (= :non-boolean-enablement
             (error-kind #(compare! example observation
                                    {:enabled? (fn [& _] value)
                                     :prepare (:prepare (model-family example))})))))))

(deftest callback-shape-is-strictly-enforced-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        valid (model-family example)]
    (doseq [family [(assoc valid :enabled? {:callable :map})
                    (assoc valid :prepare :missing)
                    (assoc valid :prepare nil)]]
      (is (= :invalid-family-callback
             (error-kind #(compare! example observation family)))))
    (doseq [family [(dissoc valid :prepare)
                    (assoc valid :extra true)
                    (dissoc valid :enabled?)]]
      (is (= :invalid-comparison
             (error-kind #(compare! example observation family)))))))

(deftest comparison-requires-same-observed-principal-and-arguments-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        valid (model-family example)]
    (is (= :invalid-comparison
           (error-kind #(compare! example observation valid "helper-b"
                                  (get-in example [:binding :arguments])))))
    (is (= :invalid-comparison
           (error-kind #(compare! example observation valid "helper-a"
                                  {:request-id "other"}))))
    (is (= :invalid-comparison
           (error-kind #(compare! example observation valid "helper-a" nil))))))

(deftest physical-witness-must-be-consistent-and-exactly-from-bridge-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        valid (model-family example)]
    (doseq [altered [(dissoc observation :bridge-scope)
                     (assoc observation :bridge-scope {})
                     (assoc-in observation [:evidence :status] :counterexample)
                     (assoc-in observation [:evidence :reason] :different)
                     nil]]
      (is (= :invalid-comparison
             (error-kind #(compare! example altered valid)))))))

(deftest incorporation-scope-must-be-canonical-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        altered (assoc-in observation [:submitted-incorporation :scope] {})]
    (is (= :invalid-submitted-incorporation
           (error-kind #(compare! example altered (model-family example)))))))

(deftest mismatched-record-binding-is-rejected-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        altered (assoc-in observation
                          [:submitted-incorporation :record :gesso.command/binding :arguments]
                          {:request-id "other"})]
    (is (contains? #{:invalid-record :incorporation-binding-mismatch}
                   (error-kind #(compare! example altered (model-family example)))))))

(deftest missing-or-altered-durable-command-create-is-rejected-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        original (get-in observation [:submitted-incorporation :plan])
        original-commands (:commands original)
        domain-only (filterv #(not= resolution/command-entity-type
                                    (first (command/target %))) original-commands)
        modified [(assoc original :commands domain-only)
                  (assoc original :commands
                         (mapv (fn [cmd]
                                 (if (= resolution/command-entity-type
                                        (first (command/target cmd)))
                                   (update cmd :model/after
                                           assoc :gesso.command/outcome
                                           {:status :forged})
                                   cmd)) original-commands))]]
    (doseq [plan modified]
      (is (some? (error-kind #(compare! example
                                     (assoc-in observation
                                               [:submitted-incorporation :plan] plan)
                                     (model-family example))))))))

(deftest an-unsupported-declared-transaction-cannot-pass-correspondence-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        base-family (model-family example)
        family (assoc base-family
                      :prepare (fn [state principal arguments]
                                 (assoc ((:prepare base-family)
                                         state principal arguments)
                                        :assertions
                                        [(tx/assert-document-exists
                                          :requests
                                          (get-in example [:request :xt/id]))])))
        result (try (compare! example observation family)
                    nil
                    (catch clojure.lang.ExceptionInfo exception
                      (:error/type (ex-data exception))))]
    (is (= :gesso.model.tx-model/unsupported-plan result))))

(deftest declared-model-prepare-exceptions-are-not-converted-to-matches-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        family {:enabled? (fn [& _] true)
                :prepare (fn [& _] (throw (ex-info "model exploded"
                                                  {:reason :intended}))) }]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"model exploded"
                          (compare! example observation family)))))

(deftest stale-rejected-transaction-must-not-match-current-state-preparation-test
  (let [example (fixture!)
        membership (:membership example)
        revoked (assoc membership :revision 1 :updated-at "t1" :active false)]
    (xt/execute-tx (node)
                   (:tx-ops (tx/prepare {}
                                        {:commands [(command/update-command
                                                     :memberships :revoke
                                                     membership revoked version)]
                                         :emit false})))
    (let [{:keys [execution observation]} (observe! example)
          response (compare! example observation (model-family example))]
      (is (= :unresolved (:status execution)))
      (is (= :rejected (get-in observation [:physical :status])))
      (is (= :consistent (get-in observation [:evidence :status])))
      ;; Physical rejection is consistent with its prepared plan, but the
      ;; family's current-state builder uses the new membership revision.
      ;; These are distinct questions, and the latter must disagree.
      (is (= :counterexample (:status response)))
      (is (false? (:matching-guards? response)))
      (is (= :rejected (:physical-status response)))
      (is (= (:request example)
             (read-row :requests (get-in example [:request :xt/id]))))
      (is (nil? (read-row :gesso_commands
                          (resolution/record-id (:binding example))))))))

(deftest a-prepared-transaction-with-no-policy-guard-can-match-a-bad-model-test
  (let [example (fixture! {:domain-guard? false})
        {:keys [observation execution]} (observe! example)
        comparison (compare! example observation (model-family example {:guard? false}))]
    (is (= :incorporated (:status execution)))
    (is (= :matched (:status comparison)))
    (is (= :committed (:physical-status comparison)))
    ;; Matching the implementation is not establishing that Claim was authorized.
    (is (contains? (:not-established bridge/family-correspondence-scope)
                   :correctness-of-business-policy-or-guards))))

(deftest comparison-executes-no-additional-authoritative-write-test
  (let [example (fixture!)
        observation (:observation (observe! example))
        initial @(:built example)
        family (model-family example)]
    (dotimes [_ 3]
      (is (= :matched (:status (compare! example observation family)))))
    (is (= initial @(:built example)))
    (is (= (:claimed example)
           (read-row :requests (get-in example [:request :xt/id]))))))

(ns gesso.assurance.reference-claim-recovery-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.live.consistency.xtdb :as live-xtdb]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)))

(deftest prepared-competing-claim-aborts-and-recovers
  (let [root (Files/createTempDirectory "gesso-claim-recovery-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        source-id "retained-recovery"
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0" :request/location-id "l1"}
        after (assoc before :status :claimed :revision 1 :updated-at "t1")
        member (fn [helper] {:xt/id helper :active true :revision 0 :created-at "t0" :updated-at "t0"})
        assignment (fn [helper] {:xt/id "primary-r1" :request-id "r1" :helper helper :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"})
        version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
        binding (fn [helper id] {:command-id (identity/command-id id) :principal (identity/principal helper)
                                 :operation :request/claim :arguments {:request-id "r1"}})
        winner (binding "helper-1" "claim-1") loser (binding "helper-2" "claim-2")
        accepted (fn [helper] {:outcome :request/claimed :helper helper})
        refused {:outcome :request/not-claimable :reason :already-claimed}
        tables #{:requests :request_assignments :memberships :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        writes #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0] [:requests "r1" :updated-at "t0"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        refusal-guards #{[:requests "r1" :status :claimed] [:requests "r1" :revision 1] [:requests "r1" :updated-at "t1"]}
        snapshots (atom {}) attempts (atom []) admissions (atom 0) recovered (atom 0)
        actor #(identity/raw-value (:principal %))
        admit (induction/build-inductive-mapped-admission! (claim/physical-mapped-admission-source (claim/bound-inductive-source!)))
        catalog (induction/describe-mapped-admission admit)
        prepare! (fn [invocation branch]
                   (let [snapshot (claim/snapshot-at-frontier! (:ctx invocation))
                         id (claim/alternative-id (actor invocation) branch)]
                     (admit :prepare! id (select-keys invocation [:command-id :principal :operation :arguments]) snapshot {})
                     (swap! snapshots assoc id snapshot)))
        refusal-contract {:construction-contract {:required-guard-tables #{:requests} :allowed-guard-tables #{:requests}
                                                  :required-write-tables #{:gesso_commands} :allowed-write-tables #{:gesso_commands}}
                          :target-contract! (fn [& _] {:required-guard-targets #{[:requests "r1"]} :allowed-guard-targets #{[:requests "r1"]}
                                                       :required-write-targets #{} :allowed-write-targets #{}})
                          :predicate-contract! (fn [& _] {:required-guard-checks refusal-guards :allowed-guard-checks refusal-guards
                                                          :required-update-checks #{} :allowed-update-checks #{}})
                          :command-operation-contract! (fn [& _] {:required-command-operations #{} :allowed-command-operations #{}})
                          :post-state-contract! (fn [& _] {:required-post-state-values #{} :allowed-post-state-values #{}})
                          :outcome-contract! (fn [& _] {:expected-outcome refused})}
        opts {:operation :request/claim :submission-mode! (constantly :submit) :mapped-admission admit
              :single-command-per-principal? true :may-disclose? (constantly true)
              :construction-contract {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                                      :required-write-tables writes :allowed-write-tables writes}
              :target-contract! (fn [_ b] (let [g #{[:memberships (actor b)]}]
                                            {:required-guard-targets g :allowed-guard-targets g :required-write-targets targets :allowed-write-targets targets}))
              :predicate-contract! (fn [_ b] (let [h (actor b) g #{[:memberships h :active true] [:memberships h :revision 0] [:memberships h :updated-at "t0"]}]
                                               {:required-guard-checks g :allowed-guard-checks g :required-update-checks updates :allowed-update-checks updates}))
              :command-operation-contract! (fn [& _] {:required-command-operations operations :allowed-command-operations operations})
              :post-state-contract! (fn [_ b] (let [v (into #{} (mapcat (fn [[table row]] (map (fn [[k v]] [table (:xt/id row) k v]) row)))
                                                               [[:requests after] [:request_assignments (assignment (actor b))]])]
                                                {:required-post-state-values v :allowed-post-state-values v}))
              :outcome-contract! (fn [_ b] {:expected-outcome (accepted (actor b))})
              :project! (fn [& _] (throw (ex-info "Unexpected projection" {})))}
        construct (fn [invocation]
                    (let [helper (actor invocation)]
                      (prepare! invocation :accept)
                      {:spec {:purpose :gesso.live/refresh}
                       :model-plan {:guards [(command/guard :memberships (member helper) (assoc version :compare-keys [:revision :updated-at :active]))]
                                    :commands [(command/update-command :requests :claim before after (assoc version :compare-keys [:revision :updated-at :status]))
                                               (command/create :request_assignments (assignment helper) version)]}
                       :outcome (accepted helper) :now "t1"}))
        subjects {:request :request/location-id :request-assignment :request/location-id}
        invoke! (fn [node executor b]
                  (let [helper (actor b) id (resolution/record-id b)
                        prepare (with-meta (fn [& _] (throw (ex-info "Raw publication forbidden" {})))
                                  {::publication/source-id source-id ::publication/binding (constantly b)
                                   ::publication/topics #{:request :request-assignment} ::publication/require-derived-model? true
                                   ::publication/derive-model-fragment #( #'publication/derive-model-fragment % subjects {:requests #{:request} :request_assignments #{:request-assignment}})
                                   ::publication/prepare-model (fn [spec changes]
                                                                 (outbox/prepare! (assoc spec :source-id source-id :operation :request/claim :command-id id
                                                                                         :intent-ids (mapv #(outbox/derived-intent-id source-id :request/claim helper id %) (range (count changes))))
                                                                                  (mapv (fn [{:keys [topic] :as change}] {:topic topic :id (get change (subjects topic))}) changes)))})
                        cmd (assoc b :execution-id (identity/execution-id (str "e-" id)))]
                    (executor (assoc cmd :command cmd :ctx {:xtdb/connectable node publication/preparation-key prepare}))))
        cases {["helper-1" :request/claim {:request-id "r1"} (accepted "helper-1")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} (accepted "helper-2")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} refused] :rejected}
        check! (fn [node history]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       actual (:replay-inputs sample)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) actual))]
                   (is (= :observed (:status sample)))
                   (is (= history actual))
                   (is (= :predicted (:status prediction)))
                   (is (= [true true false true] (mapv :committed? (:history prediction))))
                   (is (= (:segments (nth (:history prediction) 1)) (:segments (nth (:history prediction) 2))))
                   (doseq [[idx b branch outcome now] [[1 winner :accept (accepted "helper-1") "t1"]
                                                       [2 loser :accept (accepted "helper-2") "t1"]
                                                       [3 loser :refuse refused "t2"]]]
                     (let [id (claim/alternative-id (actor b) branch) source (claim/source (actor b) branch)
                           {:keys [document-rows records]} (get @snapshots id) journal (vec (vals records))
                           prefix-count (count (frame/formatted-prestate-assertions! (plan/check-target-mapping! source claim/target-map) document-rows journal))
                           tx (nth history idx)
                           spec {:alternative id :inputs {} :effects (get claim/document-effects id) :binding b :outcome outcome :now now :outcome-cases cases}
                           context (assoc (select-keys catalog [:catalog-fingerprint :induction-fingerprint]) :selectors (get-in catalog [:input-derivations id]))
                           report (evidence/compare-local-processed-retained-mapped-executor! node tables source claim/target-map document-rows journal tx prefix-count spec context source-id
                                                                                              (:publication-derivations catalog) (:residual-assertion-derivations catalog))
                           abort! (fn [tx scope]
                                    (evidence/compare-local-processed-retained-mapped-executor-abort!
                                     node scope source claim/target-map document-rows journal tx prefix-count spec context source-id
                                     (:publication-derivations catalog) (:residual-assertion-derivations catalog)))
                           abort (abort! tx tables)
                           commit! (fn [tx scope]
                                     (evidence/compare-local-processed-retained-mapped-executor-commit!
                                      node scope source claim/target-map document-rows journal tx prefix-count spec context source-id
                                      (:publication-derivations catalog) (:residual-assertion-derivations catalog)))
                           committed (commit! tx tables)
                           abstract (evidence/compare-local-processed-retained-mapped-executor-abstract!
                                     node tables source claim/target-map document-rows journal tx prefix-count spec context source-id
                                     (:publication-derivations catalog) (:residual-assertion-derivations catalog))
                           physical (provider/compare-local-processed-retained-committed-transaction! node tables tx)]
                       (if (= idx 2)
                         (do (is (= :mismatch (:status report)) (pr-str report))
                             (is (= :selected-retained-prestate-mismatch (:reason report)))
                             (is (= :checked-exact-prefix (:status (lowering/check-prefix! source claim/target-map document-rows journal (subvec (:ops tx) 1 (inc prefix-count))))))
                             (is (<= 1 (:failed-assertion-index (nth (:history prediction) idx)) prefix-count))
                             (is (= :matched (:status (provider/compare-local-processed-retained-transaction! node tables tx))))
                             (is (= :mismatch (:status committed)))
                             (is (= :selected-retained-prestate-mismatch (:reason committed)))
                             (is (not (contains? committed :selected-transaction-commitment)))
                             (is (= :mismatch (:status abstract)) (pr-str abstract))
                             (is (= :selected-retained-prestate-mismatch (:reason abstract)))
                             (is (not (contains? abstract :abstract-transition)))
                             (is (= :mismatch (:status physical)))
                             (is (= :selected-retained-commitment-not-predicted (:reason physical)))
                             (is (not (contains? physical :selected-transaction-commitment)))
                             (is (= :matched (:status abort)) (pr-str abort))
                             (is (= :stale-at-retained-predecessor (:prepared-prestate abort)))
                             (is (= :aborted (:selected-transaction-commitment abort)))
                             (is (= :matched (:assertion-error abort)))
                             (is (= (:failed-assertion-index (nth (:history prediction) idx)) (:mapped-prefix-failure-index abort)))
                             (is (= :none-at-selected-abort (:durable-effects abort)))
                             (is (= :matched-source-relative-attempt (:candidate-envelope abort)))
                             (is (= :matched-uncommitted-record (:candidate-transition-evidence abort)))
                             (is (= :none (:authority abort)))
                             (is (= :not-issued (:certificate abort)))
                             (is (not (contains? abort :transition-evidence)))
                             (is (= :unsupported (:status (abort! tx (disj tables :gesso_live_outbox)))))
                             (let [query xt/q
                                   r (with-redefs [xt/q (fn [n sql & args]
                                                          (if (= ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2" (:tx-id tx)] sql)
                                                            [{:committed false :error (ex-info "Wrong index" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx 0})}]
                                                            (apply query n sql args)))]
                                       (abort! tx tables))]
                               (is (= :mismatch (:status r)))
                               (is (= :selected-retained-abort-error-mismatch (:reason r)))
                               (is (not (contains? r :candidate-envelope))))
                             (let [ops (:ops tx) altered (envelope/bind-transition-envelope!
                                                         (-> ops (assoc-in [(dec (count ops)) 2 :poststate-fingerprint] "bad")
                                                             (update-in [(dec (count ops)) 2] #(apply dissoc % envelope/binding-keys))))
                                   r (abort! (assoc tx :ops altered) tables)]
                               (is (= :mismatch (:status r)))
                               (is (= :transition-evidence-record-mismatch (:reason r)))
                               (is (not (contains? r :candidate-envelope)))))
                         (do (is (= :matched (:status report)) (pr-str report))
                             (is (= :matched-source-relative-record (:transition-evidence report)))
                             (is (= :not-asserted (:selected-transaction-commitment report)))
                             (is (= :matched (:status committed)) (pr-str committed))
                             (is (= :selected-mapped-retained-executor-commit-v1 (:mapped-retained-evidence-profile committed)))
                             (is (= :committed (:selected-transaction-commitment committed)))
                             (is (= :matched (:selected-commitment-outcome committed)))
                             (is (= :matched (:status abstract)) (pr-str abstract))
                             (is (= :selected-mapped-retained-executor-abstract-v1 (:mapped-retained-evidence-profile abstract)))
                             (is (= :committed (:selected-transaction-commitment abstract)))
                             (is (= :selected-retained-abstract-transition-v1 (:selected-abstract-transition-profile abstract)))
                             (is (= :matched-retained-projection (:abstract-transition abstract)))
                             (is (= :matched-retained-predecessor (:prepared-prestate abstract)))
                             (is (= :none (:authority abstract)))
                             (is (= :not-issued (:certificate abstract)))
                             (is (= :not-established (:provider-refinement abstract)))
                             (is (= :matched-source-relative-record (:transition-evidence committed)))
                             (is (= :matched-retained-predecessor (:prepared-prestate committed)))
                             (is (= :none (:authority committed)))
                             (is (= :not-issued (:certificate committed)))
                             (is (= :matched (:status physical)))
                             (is (= :committed (:selected-transaction-commitment physical)))
                             (is (= :unsupported (:status (commit! tx (disj tables :gesso_live_outbox)))))
                             (let [query xt/q
                                   sql ["SELECT committed, error FROM xt.txs FOR VALID_TIME ALL FOR SYSTEM_TIME ALL WHERE _id = ? LIMIT 2" (:tx-id tx)]]
                               (doseq [[rows status reason] [[[] :unavailable :invalid-retained-commitment-outcome]
                                                           [[{:committed false}] :mismatch :selected-retained-commitment-outcome-mismatch]
                                                           [[{:committed true :error (ex-info "Unexpected error" {})}] :mismatch :selected-retained-commitment-outcome-mismatch]]]
                                 (let [r (with-redefs [xt/q (fn [n q & args]
                                                            (if (= sql q) rows (apply query n q args)))]
                                           (commit! tx tables))]
                                   (is (= status (:status r)) (pr-str r))
                                   (is (= reason (:reason r)))
                                   (is (not (contains? r :selected-transaction-commitment)))
                                   (is (not (contains? r :transition-evidence))))))
                             (is (= :mismatch (:status abort)) (pr-str abort))
                             (is (= :selected-retained-prestate-not-stale (:reason abort)))
                             (is (not (contains? abort :candidate-envelope)))))))))]
    (try
      (let [history (with-open [node (xtn/start-node config)]
                      (xt/execute-tx node [[:put-docs :requests before] [:put-docs :memberships (member "helper-1") (member "helper-2")]])
                      (let [release (live-xtdb/register-submission-policy! node (fn [submission]
                                                                                (when (publication/claim-framed-submission! admit node submission)
                                                                                  (swap! admissions inc) true)))
                            execute live-xtdb/execute-tx-from!
                            winning (publication/build-executor (assoc opts :construct! construct))
                            losing (publication/build-executor
                                    (assoc opts :abort-contract refusal-contract
                                           :construct! (fn [invocation]
                                                         (let [candidate (construct invocation)
                                                               result (invoke! node winning winner)]
                                                           (is (= :incorporated (:status result)))
                                                           (is (= (accepted "helper-1") (:outcome result)))
                                                           candidate))
                                           :construct-after-abort! (fn [invocation]
                                                                     (swap! recovered inc)
                                                                     (prepare! invocation :refuse)
                                                                     {:spec nil :model-plan {:guards [(command/guard :requests after (assoc version :compare-keys [:revision :updated-at :status]))]}
                                                                      :outcome refused :now "t2"})))]
                        (try
                          (let [result (with-redefs [adapter/project-resolution! (fn [_ r _] r)
                                                     live-xtdb/execute-tx-from! (fn [ctx ops & [options]]
                                                                                 (try (let [receipt (execute ctx ops options)]
                                                                                        (swap! attempts conj {:ops ops :receipt receipt}) receipt)
                                                                                      (catch Exception e
                                                                                        (swap! attempts conj {:ops ops :error (:xtdb.error/code (ex-data e))})
                                                                                        (throw e))))]
                                         (invoke! node losing loser))
                                basis (live-xtdb/tx-result-basis (:tx-result (:receipt (peek @attempts))))]
                            (is (= :incorporated (:status result)))
                            (is (= refused (:outcome result)))
                            (is (= 3 @admissions))
                            (is (= 1 @recovered))
                            (is (= [nil :xtdb/assert-failed nil] (mapv :error @attempts)))
                            (is (= [after] (xt/q node "SELECT * FROM requests")))
                            (is (= [(assignment "helper-1")] (xt/q node "SELECT * FROM request_assignments")))
                            (is (= #{(resolution/command-record winner (accepted "helper-1") "t1") (resolution/command-record loser refused "t2")}
                                   (set (xt/q node "SELECT * FROM gesso_commands"))))
                            (is (= #{"claim-1" "claim-2"} (set (map :xt/id (xt/q node "SELECT * FROM gesso_transition_evidence")))))
                            (is (= #{"claim-1"} (set (map :gesso.live.outbox/command-id (xt/q node "SELECT * FROM gesso_live_outbox")))))
                            (.awaitSourceBlocking (db/primary-db node) (live-xtdb/basis-tx-id basis) (Duration/ofSeconds 30))
                            (let [history (:replay-inputs (coverage/observe-local-processed-retained-replay-inputs! node))]
                              (is (= 4 (count history)))
                              (is (= (mapv :ops @attempts) (mapv :ops (subvec history 1))))
                              (doseq [idx [1 3]]
                                (let [basis (live-xtdb/tx-result-basis (:tx-result (:receipt (nth @attempts (dec idx)))))]
                                  (is (= (select-keys (nth history idx) [:tx-id :system-time])
                                         {:tx-id (live-xtdb/basis-tx-id basis) :system-time (live-xtdb/basis-system-time basis)}))))
                              (check! node history)
                              history))
                          (finally (release)))))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (check! node history)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

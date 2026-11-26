(ns gesso.assurance.reference-installed-claim-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.live.consistency.xtdb :as live-xtdb]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.live.outbox :as outbox]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.model.command :as command]
            [gesso.model.inductive-plan :as induction]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration)))

(deftest issued-claim-executor-retained-transaction
  (let [root (Files/createTempDirectory "gesso-issued-claim-replay-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        source-id "retained-claim"
        helper "helper-1" alternative (claim/alternative-id helper :accept)
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0" :request/location-id "l1"}
        member {:xt/id helper :active true :revision 0 :created-at "t0" :updated-at "t0"}
        after (assoc before :status :claimed :revision 1 :updated-at "t1")
        assignment {:xt/id "primary-r1" :request-id "r1" :helper helper :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"}
        docs {:requests [before] :request_assignments [] :memberships [member (assoc member :xt/id "helper-2")]}
        version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
        binding {:command-id (identity/command-id "claim-1") :principal (identity/principal helper)
                 :operation :request/claim :arguments {:request-id "r1"}}
        cmd (assoc binding :execution-id (identity/execution-id "e1"))
        outcome {:outcome :request/claimed :helper helper}
        tables #{:requests :request_assignments :memberships :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        write-tables #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        guard-targets #{[:memberships helper]}
        guards #{[:memberships helper :active true] [:memberships helper :revision 0] [:memberships helper :updated-at "t0"]}
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0] [:requests "r1" :updated-at "t0"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        values (into #{} (mapcat (fn [[table row]] (map (fn [[k v]] [table (:xt/id row) k v]) row)))
                     [[:requests after] [:request_assignments assignment]])
        capture (atom nil) prepared (atom nil) claimed (atom 0)]
    (try
      (let [selected
            (with-open [node (xtn/start-node config)]
              (xt/execute-tx node [[:put-docs :requests before] (into [:put-docs :memberships] (:memberships docs))])
              (let [admission (induction/build-inductive-mapped-admission! (claim/physical-mapped-admission-source (claim/bound-inductive-source!)))
                    opts {:operation :request/claim :submission-mode! (constantly :submit) :mapped-admission admission
                          :single-command-per-principal? true :may-disclose? (constantly true)
                          :construction-contract {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                                                  :required-write-tables write-tables :allowed-write-tables write-tables}
                          :target-contract! (fn [& _] {:required-guard-targets guard-targets :allowed-guard-targets guard-targets
                                                      :required-write-targets targets :allowed-write-targets targets})
                          :predicate-contract! (fn [& _] {:required-guard-checks guards :allowed-guard-checks guards
                                                         :required-update-checks updates :allowed-update-checks updates})
                          :command-operation-contract! (fn [& _] {:required-command-operations operations :allowed-command-operations operations})
                          :post-state-contract! (fn [& _] {:required-post-state-values values :allowed-post-state-values values})
                          :outcome-contract! (fn [& _] {:expected-outcome outcome})
                          :construct! (fn [{:keys [ctx] :as invocation}]
                                        (let [snapshot (claim/snapshot-at-frontier! ctx)]
                                          (reset! prepared snapshot)
                                          (admission :prepare! alternative (select-keys invocation [:command-id :principal :operation :arguments]) snapshot {}))
                                        {:spec {:purpose :gesso.live/refresh}
                                         :model-plan {:guards [(command/guard :memberships member (assoc version :compare-keys [:revision :updated-at :active]))]
                                                      :commands [(command/update-command :requests :claim before after (assoc version :compare-keys [:revision :updated-at :status]))
                                                                 (command/create :request_assignments assignment version)]}
                                         :outcome outcome :now "t1"})
                          :project! (fn [& _] (throw (ex-info "Unexpected browser projection" {})))}
                    executor (publication/build-executor opts)
                    subjects {:request :request/location-id :request-assignment :request/location-id}
                    prepare (with-meta (fn [_ _] (throw (ex-info "Raw publication forbidden" {})))
                              {::publication/source-id source-id ::publication/binding (constantly binding)
                               ::publication/topics #{:request :request-assignment} ::publication/require-derived-model? true
                               ::publication/derive-model-fragment #( #'publication/derive-model-fragment % subjects {:requests #{:request} :request_assignments #{:request-assignment}})
                               ::publication/prepare-model (fn [spec changes]
                                                             (outbox/prepare! (assoc spec :source-id source-id :operation :request/claim :command-id "claim-1"
                                                                                     :intent-ids (mapv #(outbox/derived-intent-id source-id :request/claim helper "claim-1" %) (range (count changes))))
                                                                              (mapv (fn [{:keys [topic] :as change}] {:topic topic :id (get change (subjects topic))}) changes)))})
                    release (live-xtdb/register-submission-policy! node (fn [submission]
                              (when-let [_ (publication/claim-framed-submission! admission node submission)]
                                (swap! claimed inc) true)))
                    execute live-xtdb/execute-tx-from!]
                (try
                  (let [r (with-redefs [adapter/project-resolution! (fn [_ result _] result)
                                        live-xtdb/execute-tx-from! (fn [ctx ops & [options]]
                                                                    (let [receipt (execute ctx ops options)]
                                                                      (reset! capture {:ops ops :receipt receipt}) receipt))]
                            (executor (assoc cmd :command cmd :ctx {:xtdb/connectable node publication/preparation-key prepare})))
                        basis (live-xtdb/tx-result-basis (:tx-result (:receipt @capture)))
                        id (live-xtdb/basis-tx-id basis)
                        expected {:tx-id id :system-time (live-xtdb/basis-system-time basis) :ops (:ops @capture)}]
                    (is (= :incorporated (:status r)))
                    (is (= 1 @claimed))
                    (is (= :checked-commit-basis-projection (get-in r [:transaction :mapped-readback :physical-correspondence])))
                    (is (= (update-vals docs set) (update-vals (:document-rows @prepared) set)))
                    (is (= {} (:records @prepared)))
                    (is (envelope/bound-transition-envelope? (:ops expected)))
                    (.awaitSourceBlocking (db/primary-db node) id (Duration/ofSeconds 30))
                    (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                          report (provider/compare-local-processed-retained-transaction! node tables expected)
                          ;; Three required document rows each contribute two
                          ;; assertions, one nullable document and two empty slots.
                          prestate (provider/compare-local-processed-retained-mapped-evidence-absence!
                                    node tables (claim/source helper :accept) claim/target-map
                                    (:document-rows @prepared) [] expected 9 "claim-1")]
                      (is (= :observed (:status sample)) (pr-str (dissoc sample :replay-inputs)))
                      (is (= expected (peek (:replay-inputs sample))))
                      (is (= :matched (:status report)) (pr-str report))
                      (is (= :matched (:selected-transaction-binding report)))
                      (is (= :matched (:status prestate)) (pr-str prestate))
                      (is (= :checked-exact-prefix (:source-lowering prestate)))
                      (is (= :matched-retained-predecessor (:prepared-prestate prestate)))
                      (is (= :absent-at-retained-predecessor (:global-evidence-id prestate)))
                      (is (= :absent-at-retained-predecessor (:global-command-id prestate)))
                      (is (nil? (current/operation-tables ["ASSERT ? = (SELECT COUNT(*) FROM unrelated WHERE _id = ?)" 0 "r1"])))
                      (is (= :not-asserted (:selected-transaction-commitment report)))
                      (is (= :none (:authority report)))
                      (is (= :not-issued (:certificate report)))
                      (is (= :mismatch (:status (provider/compare-local-processed-retained-transaction! node tables (update expected :ops pop)))))
                      (is (= :unsupported (:status (provider/compare-local-processed-retained-transaction! node (disj tables :memberships) expected)))))
                    expected)
                  (finally (release)))))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id selected) (Duration/ofSeconds 30))
          (is (= :matched (:status (provider/compare-local-processed-retained-transaction! node tables selected))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

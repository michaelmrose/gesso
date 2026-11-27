(ns gesso.assurance.reference-claim-recovery-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-submission :as submission]
            [gesso.assurance.reference-issued-envelope-argument :as issued-envelope]
            [gesso.assurance.reference-physical-envelope-argument :as physical-envelope]
            [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.assurance.reference-xtdb-provider-contract :as provider-contract]
            [gesso.assurance.reference-mapped-observed-recovery :as observed-recovery]
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
            [gesso.live.outbox.writer-policy :as writer-policy]
            [gesso.live.outbox.publication-manifest :as manifest]
            [gesso.live.outbox.physical-correspondence :as correspondence]
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
        descriptor "[[[\"\" \"request\"] [\"request\" \"location-id\"] []] [[\"\" \"request-assignment\"] [\"request\" \"location-id\"] []]]"
        seed [[:put-docs :requests before] [:put-docs :memberships (member "helper-1") (member "helper-2")]]
        tables #{:gesso_initializations :gesso_publication_manifests :requests :request_assignments :memberships :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        writes #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0] [:requests "r1" :updated-at "t0"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        refusal-guards #{[:requests "r1" :status :claimed] [:requests "r1" :revision 1] [:requests "r1" :updated-at "t1"]}
        snapshots (atom {}) attempts (atom []) admissions (atom 0) recovered (atom 0)
        policy-events (atom [])
        claims (atom []) captured (atom nil) observation-handle (atom nil)
        actor #(identity/raw-value (:principal %))
        mapped-source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admit (induction/build-inductive-mapped-admission! mapped-source)
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
        outcome-declarations (into {} (for [helper (keys claim/helpers) branch [:accept :refuse]]
                                         [(claim/alternative-id helper branch)
                                          {:arguments {:request-id "r1"}
                                           :outcome (if (= :accept branch) (accepted helper) refused)}]))
        cases {["helper-1" :request/claim {:request-id "r1"} (accepted "helper-1")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} (accepted "helper-2")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} refused] :rejected
               ["helper-1" :request/claim {:request-id "r1"} refused] :rejected}
        recovery-steps (fn [history]
                         (mapv (fn [[idx b branch outcome now]]
                                 (let [id (claim/alternative-id (actor b) branch)
                                       source (claim/source (actor b) branch)
                                       {:keys [document-rows records]} (get @snapshots id)
                                       journal (vec (vals records))
                                       prefix-count (count (frame/formatted-prestate-assertions!
                                                            (plan/check-target-mapping! source claim/target-map)
                                                            document-rows journal))]
                                   {:original source :targets claim/target-map :docs document-rows :journal journal
                                    :expectation (nth history idx) :prefix-count prefix-count
                                    :spec {:alternative id :inputs {} :effects (get claim/document-effects id)
                                           :binding b :outcome outcome :now now :outcome-cases cases}
                                    :context (assoc (select-keys catalog [:catalog-fingerprint :induction-fingerprint])
                                                    :selectors (get-in catalog [:input-derivations id]))
                                    :source-id source-id :declarations (:publication-derivations catalog)
                                    :residual-declarations (:residual-assertion-derivations catalog)}))
                               [[2 winner :accept (accepted "helper-1") "t1"]
                                [3 loser :accept (accepted "helper-2") "t1"]
                                [4 loser :refuse refused "t2"]]))
        check! (fn [node history original-node?]
                 (let [sample (coverage/observe-local-processed-retained-replay-inputs! node)
                       actual (:replay-inputs sample)
                       prediction (temporal/replay (mapv #(dissoc % :tx-id) actual))
                       steps (recovery-steps history)
                       trajectory! #(evidence/compare-local-processed-retained-mapped-executor-outcome-recovery! node %1 mapped-source outcome-declarations %2)
                       issued! #(evidence/compare-local-processed-retained-issued-executor-recovery! node tables admit mapped-source outcome-declarations %)
                       combined! #(observed-recovery/compare-selected! node %1 admit mapped-source outcome-declarations %2 %3)
                       activated! #(observed-recovery/compare-activated! node %1 admit mapped-source outcome-declarations %2 %3)
                       trajectory (activated! tables steps @observation-handle)
                       submission! #(submission/compare-selected! admit steps %1 %2)
                       observed (submission! @claims @captured)
                       issued-observation (submission/compare-observed! admit steps @observation-handle)]
                   (is (= :matched (:status trajectory)) (pr-str trajectory))
                   (is (= observed-recovery/activated-profile (:activated-recovery-profile trajectory)))
                   (is (= :matched-local-activated-retained-root (:activation-root-binding trajectory)))
                   (is (= (:tx-id (first history)) (:manifest-tx-id trajectory)))
                   (is (= (:tx-id (second history)) (:initialization-tx-id trajectory)))
                   (doseq [[r reason] [[(activated! tables steps @captured) :unissued-framed-submission-observation]
                                       [(activated! (disj tables :gesso_initializations) steps @observation-handle) :incomplete-activation-root-scope]
                                       [(activated! tables (assoc-in steps [0 :source-id] "wrong") @observation-handle) :selected-activation-source-mismatch]
                                       [(activated! tables (assoc-in steps [0 :expectation :tx-id] -1) @observation-handle) :selected-activation-root-mismatch]]]
                     (is (#{:unsupported :mismatch} (:status r)) (pr-str r))
                     (is (= reason (:reason r)) (pr-str r))
                     (is (not-any? #(contains? r %) [:activation-root-binding :submission-recovery-linkage :issued-admission-binding])))
                   (is (= observed-recovery/profile (:observed-recovery-profile trajectory)))
                   (is (= :matched-local-observed-retained-steps (:submission-recovery-linkage trajectory)))
                   (is (= :matched-local-issued-policy-window (:observation-origin trajectory)))
                   (is (= :matched-selected-commit-identities (:returned-transaction-keys trajectory)))
                   (is (= :not-established (:observation-storage-identity trajectory)))
                   (is (= :not-established (:aborted-receipt-identity trajectory)))
                   (is (= :not-established (:submission-provenance trajectory)))
                   (doseq [[r reason] [[(combined! tables steps @captured) :unissued-framed-submission-observation]
                                       [(combined! tables (assoc-in steps [1 :expectation :ops] []) @observation-handle)
                                        :mapped-submission-envelope-mismatch]
                                       [(combined! tables (assoc-in steps [2 :expectation :tx-id] -1) @observation-handle)
                                        :mapped-submission-receipt-mismatch]
                                       [(combined! tables (assoc-in steps [0 :context :catalog-fingerprint] "wrong") @observation-handle)
                                        :issued-executor-recovery-declaration-mismatch]
                                       [(combined! (disj tables :memberships) steps @observation-handle)
                                        :incomplete-mapped-recovery-scope]]]
                     (is (#{:mismatch :unsupported} (:status r)) (pr-str r))
                     (is (= reason (:reason r)) (pr-str r))
                     (is (not-any? #(contains? r %) [:submission-recovery-linkage :observation-origin :framed-handoffs
                                                    :returned-transaction-keys :issued-admission-binding :closed-decoder-binding
                                                    :selected-tx-ids :recovery-linkage :outcome-derivations])))
                   (if original-node?
                     (let [check-envelope! #(issued-envelope/compare-recovery! node admit mapped-source outcome-declarations %1 %2)
                           result (check-envelope! steps @observation-handle)]
                       (is (= :conditional-issued-source-envelope-recovery (:status result)) (pr-str result))
                       (is (= 3 (count (:instances result))))
                       (is (every? #(= :matched-original-live-handles (:induction-fingerprint-provenance %)) (:instances result)))
                       (is (every? #(= :matched-consumed-candidate-witness (:preparation-correspondence %)) (:instances result)))
                       (is (every? #(= :matched-local-issued-policy-window (:consumption-correspondence %)) (:instances result)))
                       (is (= :not-established (:physical-prestate-provenance result) (:provider-refinement result)))
                       (is (= :not-performed (:proof-discharge result)))
                       (doseq [bad [(check-envelope! steps @captured)
                                    (check-envelope! (assoc-in steps [0 :context :induction-fingerprint] "foreign") @observation-handle)
                                    (check-envelope! (assoc-in steps [0 :docs :requests] []) @observation-handle)
                                    (check-envelope! (assoc-in steps [0 :expectation :ops] []) @observation-handle)]]
                         (is (= :not-established (:status bad)) (pr-str bad))
                         (is (not (contains? bad :instances))))
                       (with-redefs [publication/read-framed-submission-observation! (constantly {:claims @claims :capture @captured})]
                         (is (= :issued-envelope-boundary-replaced (:reason (check-envelope! steps @observation-handle))))))
                     (let [r (issued-envelope/compare-recovery! node admit mapped-source outcome-declarations steps @observation-handle)]
                       (is (= :unissued-envelope-origin (:reason r)))
                       (is (not (contains? r :instances)))))
                   (let [check-physical! #(physical-envelope/compare-recovery! node %1 admit mapped-source outcome-declarations %2 %3)
                         r (check-physical! tables steps @observation-handle)]
                     (if original-node?
                       (do
                         (is (= :conditional-physical-source-envelope-recovery (:status r)) (pr-str r))
                         (is (= [:matched-predecessor :matched-earlier-selected-image :matched-predecessor] (:preparation-images r)))
                         (is (= :distinct-from-stale-preparation (:aborted-prestate r)))
                         (is (= :matched-physical-stutter (:aborted-effect r)))
                         (is (= :matched-independent-retained-replay (:physical-rows r)))
                         (is (= [true false true] (mapv :committed? (:ledger r))))
                         (is (= [:committed-abstract-transition :aborted-stutter :committed-abstract-transition]
                                (mapv :relation (:abstract-steps r))))
                         (is (= [:exact-source-derived-puts :exact-row-stutter :exact-source-derived-puts]
                                (mapv :checked-row-effect (:abstract-steps r))))
                         (is (= (mapv #(get-in % [:spec :alternative]) steps)
                                (mapv #(get-in % [:identity :alternative-id]) (:abstract-steps r))))
                         (is (= (set (keys (get-in mapped-source [:inductive-source :catalog :invariants])))
                                (:invariant-ids r)))
                         (is (= :checked-both-evaluators-at-selected-endpoints (:invariant-correspondence r)))
                         (is (= :matched-both-coordinate-decoders (:projection-correspondence r)))
                         (is (= :matched-journal-only-mapped-effect (:refusal-effect r)))
                         (let [g (:general-argument r)
                               conditions (:general-conditions g)
                               writer (:related-writer-isolation-conditions g)
                               provider (set (keys (:related-provider-conditions g)))
                               other (:other-general-conditions g)]
                           (is (= :conditional-selected-general-argument-binding (:status g)))
                           (is (= 39 (count conditions)))
                           (is (= conditions (into writer (concat provider other))))
                           (is (= 39 (+ (count writer) (count provider) (count other))))
                           (is (every? (:remaining-conditions r) conditions))
                           (is (= (:original-open-obligations catalog) (:original-open-obligations g)))
                           (is (= :related-not-entailing (:condition-link-semantics g)))
                           (is (= :not-observed (:configured-trust-adoption g)))
                           (is (= :not-performed (:premise-acceptance g) (:proof-discharge g)))
                           (is (= :not-established (:universal-refinement g))))
                         (let [bad (with-redefs [provider-contract/derive! (constantly {})]
                                     (check-physical! tables steps @observation-handle))]
                           (is (= :physical-envelope-boundary-replaced (:reason bad)))
                           (is (not (contains? bad :general-argument))))
                         (doseq [bad [(with-redefs [refinement/check-step! (constantly {:status :conditional-step-correspondence})]
                                        (check-physical! tables steps @observation-handle))
                                      (with-redefs [invariant/check-projected-invariants! (constantly #{})]
                                        (check-physical! tables steps @observation-handle))]]
                           (is (= :physical-envelope-boundary-replaced (:reason bad)))
                           (is (not (contains? bad :abstract-steps))))
                         (is (= :not-established (:physical-preparation-provenance r) (:provider-refinement r)))
                         (doseq [bad [(check-physical! (disj tables :memberships) steps @observation-handle)
                                      (check-physical! tables (assoc-in steps [1 :expectation :tx-id] -1) @observation-handle)
                                      (with-redefs [xt/q (constantly [])]
                                        (check-physical! tables steps @observation-handle))]]
                           (is (= :not-established (:status bad)) (pr-str bad))
                           (is (not (contains? bad :preparation-images)))))
                       (is (= :not-established (:status r)))))
                   (is (= :matched (:status issued-observation)) (pr-str issued-observation))
                   (is (= submission/observed-profile (:observed-profile issued-observation)))
                   (is (= :matched-local-issued-policy-window (:observation-origin issued-observation)))
                   (is (= :not-established (:submission-provenance issued-observation)))
                   (is (= :not-established (:aborted-receipt-identity issued-observation)))
                   (doseq [[a h] [[admit (Object.)] [admit @captured] [(fn [& _] nil) @observation-handle]]]
                     (let [r (submission/compare-observed! a steps h)]
                       (is (= :unissued-framed-submission-observation (:reason r)))
                       (is (not (contains? r :observation-origin)))))
                   (is (= :not-observed (:command-origin-correspondence issued-observation)
                          (:command-origin-correspondence trajectory)))
                   (is (= [nil nil nil] (:selected-command-origins trajectory)))
                   (is (= :not-established (:command-authentication trajectory)))
                   (let [origins (mapv (fn [step]
                                         (let [b (get-in step [:spec :binding])]
                                           (assoc b :execution-id (identity/execution-id (str "e-" (resolution/record-id b)))
                                                  :origin :registered-server-invocation
                                                  :authentication :conditional-on-trusted-principal-resolver))) steps)
                         supplied (mapv #(assoc %1 :command-origin %2) @claims origins)
                         full (submission! supplied @captured)
                         partial (submission! (update supplied 0 dissoc :command-origin) @captured)]
                     (is (= :matched-supplied-command-bindings (:command-origin-correspondence full)))
                     (is (= :partial-supplied-command-bindings (:command-origin-correspondence partial)))
                     (is (nil? (first (:selected-command-origins partial))))
                     (is (= :same-observed-execution (:abort-recovery-execution full)))
                     (is (= (mapv #(get-in % [:expectation :tx-id]) steps)
                            (mapv :selected-tx-id (:selected-command-origins full))))
                     (is (= :distinct-observed-executions
                            (:abort-recovery-execution
                             (submission! (assoc-in supplied [2 :command-origin :execution-id]
                                                    (identity/execution-id "another-attempt")) @captured))))
                     (doseq [bad [(assoc-in supplied [0 :command-origin] nil)
                                  (assoc-in supplied [0 :command-origin :principal] (:principal loser))
                                  (assoc-in supplied [0 :command-origin :arguments] {})
                                  (assoc-in supplied [0 :command-origin :operation] :other/op)
                                  (assoc-in supplied [0 :command-origin :command-id] (:command-id loser))
                                  (assoc-in supplied [0 :command-origin :execution-id] (:command-id winner))
                                  (assoc-in supplied [0 :command-origin :authentication] :authenticated)]]
                       (let [r (submission! bad @captured)]
                         (is (= :mismatch (:status r)))
                         (is (= :mapped-command-origin-mismatch (:reason r)))
                         (is (not-any? #(contains? r %) [:command-origin-correspondence :selected-command-origins :framed-handoffs]))))
                     (with-redefs [publication/read-framed-submission-observation!
                                   (constantly {:claims supplied :capture @captured})]
                       (let [r (submission/compare-observed! admit steps @observation-handle)]
                         (is (= :command-origin-comparison-boundary-replaced (:reason r)))
                         (is (not (contains? r :selected-command-origins))))))
                   (is (= :matched (:status observed)) (pr-str observed))
                   (is (= :matched-observed-checked-candidates (:framed-handoffs observed)))
                   (is (= :matched-selected-retained-operations (:adapter-payloads observed)))
                   (is (= :matched-selected-commit-identities (:returned-transaction-keys observed)))
                   (is (= :observed-throw-without-transaction-key (:aborted-call observed)))
                   (is (= :not-established (:aborted-receipt-identity observed)))
                   (is (= :not-established (:submission-provenance observed)))
                   (is (= :none (:authority observed)))
                   (doseq [[r reason] [[(submission! (assoc-in @claims [1 :submission :tx-ops] []) @captured)
                                        :mapped-submission-envelope-mismatch]
                                       [(submission! (assoc-in @claims [2 :checked :identity :command-id] "other") @captured)
                                        :mapped-submission-handoff-mismatch]
                                       [(submission! @claims (assoc-in @captured [:events 5 :tx-result]
                                                                        (get-in @captured [:events 1 :tx-result])))
                                        :mapped-submission-receipt-mismatch]
                                       [(submission! @claims (assoc-in @captured [:events 3 :phase] :returned))
                                        :mapped-submission-abort-observation-mismatch]]]
                     (is (= :mismatch (:status r)) (pr-str r))
                     (is (= reason (:reason r)))
                     (is (not-any? #(contains? r %) [:framed-handoffs :adapter-payloads :returned-transaction-keys])))
                   (is (= :observed (:status sample)))
                   (is (= history actual))
                   (is (= :predicted (:status prediction)))
                   (is (= [true true true false true] (mapv :committed? (:history prediction))))
                   (is (= (:segments (nth (:history prediction) 2)) (:segments (nth (:history prediction) 3))))
                   (is (= :matched (:status trajectory)) (pr-str trajectory))
                   (is (= evidence/issued-executor-recovery-profile (:issued-executor-recovery-profile trajectory)))
                   (is (= :matched-issued-declarations (:issued-admission-binding trajectory)))
                   (is (= :matched-issued-closed-construction (:closed-decoder-binding trajectory)))
                   (is (= (:catalog-fingerprint catalog) (:issued-catalog-fingerprint trajectory)))
                   (is (= (:induction-fingerprint catalog) (:issued-induction-fingerprint trajectory)))
                   (doseq [bad [(assoc-in steps [0 :context :catalog-fingerprint] "wrong")
                                (assoc-in steps [1 :context :induction-fingerprint] "wrong")
                                (assoc-in steps [1 :context :selectors] {:fake/input [:principal]})
                                (assoc-in steps [0 :spec :effects] {})
                                (assoc-in steps [1 :declarations] {})
                                (assoc-in steps [2 :residual-declarations] {})]]
                     (let [r (issued! bad)]
                       (is (= :mismatch (:status r)) (pr-str r))
                       (is (= :issued-executor-recovery-declaration-mismatch (:reason r)))
                       (is (not-any? #(contains? r %) [:issued-admission-binding :closed-decoder-binding
                                                      :selected-tx-ids :input-derivations :outcome-derivations]))))
                   (is (= evidence/executor-recovery-profile (:executor-recovery-profile trajectory)))
                   (is (= evidence/executor-input-recovery-profile (:executor-input-recovery-profile trajectory)))
                   (is (= (vec (repeat 3 :matched-source-relative-selectors)) (:input-derivations trajectory)))
                   (is (= evidence/executor-outcome-recovery-profile (:executor-outcome-recovery-profile trajectory)))
                   (is (= (vec (repeat 3 :matched-declared-journal-assignment)) (:outcome-derivations trajectory)))
                   (is (= cases (:cases (outcome/derive-cases! mapped-source outcome-declarations))))
                   (is (= :not-established (:source-provenance trajectory)))
                   (is (= :not-established (:outcome-provenance trajectory)))
                   (is (= provider/mapped-recovery-profile (:mapped-recovery-profile trajectory)))
                   (is (= (mapv :tx-id (subvec history 2)) (:selected-tx-ids trajectory)))
                   (is (= [:committed :aborted :committed] (:selected-commitments trajectory)))
                   (is (= [:matched-retained-projection :not-committed :matched-retained-projection]
                          (:abstract-transitions trajectory)))
                   (is (= :matched-retained-journal-trajectory (:recovery-linkage trajectory)))
                   (is (= [:matched-source-relative-record :matched-uncommitted-record :matched-source-relative-record]
                          (:executor-envelopes trajectory)))
                   (is (= :none (:authority trajectory)))
                   (is (= :not-issued (:certificate trajectory)))
                   (is (= :not-established (:provider-refinement trajectory)))
                   (doseq [[bad reason] [[(trajectory! (disj tables :memberships) steps) :incomplete-mapped-recovery-scope]
                                         [(trajectory! tables [(nth steps 0) (nth steps 2) (nth steps 1)]) :nonadjacent-recovery-selection]
                                         [(trajectory! tables (assoc-in steps [1 :context :selectors] {:fake/input [:principal]})) :selected-input-selector-mismatch]
                                         [(trajectory! tables (update-in steps [1 :spec :outcome-cases] dissoc ["helper-1" :request/claim {:request-id "r1"} refused])) :selected-outcome-cases-mismatch]
                                         [(evidence/compare-local-processed-retained-mapped-executor-outcome-recovery!
                                           node tables mapped-source (dissoc outcome-declarations (claim/alternative-id "helper-1" :refuse)) steps)
                                          :incomplete-outcome-declarations]]]
                     (is (#{:mismatch :unsupported} (:status bad)) (pr-str bad))
                     (is (= reason (:reason bad)) (pr-str bad))
                     (is (not-any? #(contains? bad %) [:recovery-linkage :selected-tx-ids :selected-commitments
                                                      :abstract-transitions :executor-envelopes :input-derivations :outcome-derivations])))
                   (doseq [[i {:keys [original docs journal expectation prefix-count spec context]}] (map-indexed vector steps)]
                     (let [idx (+ 2 i) source original document-rows docs tx expectation
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
                       (if (= idx 3)
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
                      (let [writer (writer-policy/build-policy!
                                    node {:admission admit :initializer seed
                                          :observe! (fn [{:keys [kind] :as event}]
                                                      (swap! policy-events conj kind)
                                                      (when (= :mapped-operation kind) (swap! admissions inc)))}
                                    {:source-id source-id :worker-id "recovery" :descriptor descriptor})
                            release (live-xtdb/register-submission-policy! node writer)
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
                          (let [{:keys [observation capture]} (publication/capture-framed-submissions! admit node (constantly :not-activated))
                                r (observed-recovery/compare-activated! node tables admit mapped-source outcome-declarations [] observation)]
                            (is (some? observation))
                            (is (= :not-activated (:result capture)))
                            (is (= :missing-selected-activation-root (:reason r)))
                            (is (not (contains? r :activation-root-binding))))
                          (manifest/require-compatible! node source-id descriptor)
                          (writer-policy/activate-policy! writer)
                          (let [initial (correspondence/initialization-observation node admit)]
                            (is (= :matched (:manifest-envelope initial)))
                            (is (= (:catalog-fingerprint catalog) (:catalog-fingerprint initial))))
                          (let [result (with-redefs [adapter/project-resolution! (fn [_ r _] r)
                                                     live-xtdb/execute-tx-from! (fn [ctx ops & [options]]
                                                                                 (try (let [receipt (execute ctx ops options)]
                                                                                        (swap! attempts conj {:ops ops :receipt receipt}) receipt)
                                                                                      (catch Exception e
                                                                                        (swap! attempts conj {:ops ops :error (:xtdb.error/code (ex-data e))})
                                                                                        (throw e))))]
                                         (let [{:keys [observation capture]}
                                               (publication/capture-framed-submissions! admit node #(invoke! node losing loser))]
                                           (reset! captured capture)
                                           (reset! observation-handle observation)
                                           (reset! claims (:claims (publication/read-framed-submission-observation! admit observation)))
                                           (if (= :returned (:outcome capture))
                                             (:result capture)
                                             (throw (:error capture)))))
                                basis (live-xtdb/tx-result-basis (:tx-result (:receipt (peek @attempts))))]
                            (is (= :incorporated (:status result)))
                            (is (= refused (:outcome result)))
                            (is (= 3 @admissions))
                            (is (= 3 (count @claims)))
                            (is (every? #(not (contains? % :command-origin)) @claims))
                            (is (= [:manifest :initializer :mapped-operation :mapped-operation :mapped-operation] @policy-events))
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
                              (is (= 5 (count history)))
                              (is (= (mapv :ops @attempts) (mapv :ops (subvec history 2))))
                              (doseq [idx [2 4]]
                                (let [basis (live-xtdb/tx-result-basis (:tx-result (:receipt (nth @attempts (- idx 2)))))]
                                  (is (= (select-keys (nth history idx) [:tx-id :system-time])
                                         {:tx-id (live-xtdb/basis-tx-id basis) :system-time (live-xtdb/basis-system-time basis)}))))
                              (check! node history true)
                              history))
                          (finally (release)))))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (check! node history false)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

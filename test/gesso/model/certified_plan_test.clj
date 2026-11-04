(ns gesso.model.certified-plan-test
  "v1252: adversarial checks for the v1251 pre-submit certified plan gate.

   A checked candidate is only a COPYABLE abstract observation. A registered
   executor is an in-process function whose original proof, typed transition,
   callbacks, and physical plan are checked before a real submission attempt.
   These tests deliberately distinguish physical submission from permission,
   proof of invariants, transaction atomicity, and production correspondence.
   No XTDB process is started: the positive submit test replaces ONLY the
   final tx/transact! call after the original durable-command incorporation."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-operation :as certified]
            [gesso.model.certified-plan :as gate]
            [gesso.model.tx :as tx]))

(def ^:private operation-id :example/claim)
(def ^:private subject
  {:kind :operation :id operation-id
   :interface {:operation operation-id}})
(def ^:private construction
  {:subject subject
   :state-types
   {:request/status {:type :enum :name :request/status
                     :values #{:open :claimed}}
    :request/version {:type :integer}
    :request/score {:type :integer}
    :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/next-version {:type :integer}}
   :guard {:readers #{:request/status :request/score}
           :expression [:and
                        [:= [:read :request/status] [:literal :open]]
                        [:> [:read :request/score] [:literal 7]]]}
   :reads #{:request/status :request/score :input/next-version}
   :writes #{:request/status :request/version}
   :replacements {:request/status [:literal :claimed]
                  :request/version [:read :input/next-version]}
   :frame #{:request/score :request/note}})
(def ^:private field-map
  {:request/status :request/status
   :request/version :request/version
   :request/score :request/score
   :request/note :request/note})
(def ^:private before
  {:xt/id 10 :request/status :open :request/version 2 :request/score 12})
(def ^:private after
  {:xt/id 10 :request/status :claimed :request/version 3 :request/score 12})
(def ^:private inputs {:input/next-version 3})
(def ^:private command
  {:model/entity-type :requests
   :model/operation :claim
   :model/id 10
   :model/expected {:model/id 10 :model/checks [[:request/version 2]]}
   :model/before before
   :model/after after})
(def ^:private rejected-type :gesso.model.certified-plan/invalid-certified-plan)
(def ^:private abstraction-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private abstraction-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})

(defn- candidate
  ([] (candidate command))
  ([c]
   {:construction construction
    :field-map field-map
    :command-operation :claim
    :target [:requests 10]
    :current-document before
    :inputs inputs
    :plan {:commands [c] :emit false}}))

(defn- failure [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo ex (ex-data ex))))

(defn- reject! [thunk expected-type expected-reason]
  (let [data (failure thunk)]
    (is (map? data) (str "Expected typed rejection: " expected-reason))
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (= expected-reason (:reason data)) (pr-str data))
    (when (= expected-type rejected-type)
      (is (vector? (:at data)) (pr-str data)))
    data))

(defn- reject-plan! [reason thunk]
  (reject! thunk rejected-type reason))

(defn- assured-only-as-observation! [witness]
  (is (= gate/witness-type (:type witness)))
  (is (= gate/plan-version (:version witness)))
  (is (= operation-id (:operation witness)))
  (is (= [:requests 10] (:target witness)))
  (is (= :claim (:command-operation witness)))
  (is (= :pre-submit-candidate-checked (:checking witness)))
  (is (= :trusted-observation-only (:prestate-assurance witness)))
  (is (= :not-established (:atomicity-assurance witness)))
  (is (= :not-proved (:invariant-preservation witness)))
  (is (= :not-established (:physical-correspondence witness)))
  (is (false? (:transaction-submitted? witness)))
  (is (= :none (:authority witness)))
  (is (contains? (:open-obligations witness)
                 :prestate-to-commit-atomic-guard-enforcement))
  (is (contains? (:open-obligations witness)
                 :application-invariant-preservation))
  (is (not (contains? witness :valid?)))
  witness)

(def ^:private proof-fixture
  (delay
    (let [transition (-> construction
                         (dissoc :replacements :frame)
                         (assoc :assignments (:replacements construction)))
          proof {:transition transition
                 :steps [{:id :step/guard :rule :guard
                          :conclusion [:guard]}]
                 :goal [:guard]}
          graph {:roots #{operation-id}
                 :nodes [{:id operation-id :subject subject
                          :subject-handle (admission/admit-subject! subject)
                          :depends-on []}]}
          cert (certificate/admit-certificate!
                {:proof-kind :guarded-transition
                 :proof-source proof
                 :graph-handle (dependency/check-graph! graph)
                 :current-graph graph
                 :expected-open-premises #{abstraction-premise}
                 :expected-open-obligations abstraction-obligations})]
      {:graph graph :proof proof :certificate cert})))

(defn- environment []
  (let [{:keys [graph proof certificate]} @proof-fixture
        calls (atom {:acquire 0 :inputs 0 :target 0 :construct 0})
        proposed (atom command)
        ctor (fn [_ctx _binding]
               (swap! calls update :construct inc)
               {:now :clock
                :plan {:commands [@proposed] :emit false}
                :outcome {:status :claimed}})
        configuration {:operation operation-id
                       :read-record! (fn [_ _] nil)
                       :may-disclose? (fn [_ _ _] true)
                       :construct! ctor}
        source {:certified-source
                {:operation operation-id
                 :certificate certificate
                 :proof-source proof
                 :current-graph graph
                 :resolved-configuration configuration}
                :construction construction
                :field-map field-map
                :command-operation :claim
                :current-document! (fn [_ _]
                                     (swap! calls update :acquire inc)
                                     before)
                :transition-inputs! (fn [_ _]
                                      (swap! calls update :inputs inc)
                                      inputs)
                :target! (fn [_ _]
                           (swap! calls update :target inc)
                           [:requests 10])}]
    {:source source :calls calls :proposed proposed}))

(defn- invocation [mode]
  {:binding {:command-id (identity/identity :command "v1252-claim")
             :principal (identity/identity :principal :example/helper)
             :operation operation-id
             :arguments {}}
   :mode mode})

(deftest valid-candidate-has-typed-physical-footprint-and-no-authority
  (let [report (assured-only-as-observation! (gate/check-candidate! (candidate)))]
    (is (= :example/claim (:operation report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:transition-fingerprint report)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:subject-fingerprint report)))
    (is (= #{:typed-abstract-relation
             :exact-domain-command-and-target
             :complete-mapped-physical-document-fields
             :current-prestate-observation
             :closed-single-domain-write-before-submit}
           (:checked report)))
    (is (= report (gate/check-candidate! (candidate))))))

(deftest optional-presence-is-exactly-framed
  (assured-only-as-observation! (gate/check-candidate! (candidate)))
  (let [present (assoc before :request/note nil)
        after-present (assoc after :request/note nil)]
    (assured-only-as-observation!
     (gate/check-candidate!
      (assoc (candidate (assoc command :model/before present
                                     :model/after after-present))
             :current-document present)))
    (reject-plan! :frame-preservation-failed
                  #(gate/check-candidate!
                    (candidate (assoc command :model/after
                                      (assoc after :request/note nil)))))
    (reject-plan! :frame-preservation-failed
                  #(gate/check-candidate!
                    (assoc (candidate (assoc command
                                             :model/before present
                                             :model/after after))
                           :current-document present)))))

(deftest wrong-status-and-wrong-version-are-not-accepted
  (doseq [wrong [:open]]
    (reject-plan! :declared-write-mismatch
                  #(gate/check-candidate!
                    (candidate (assoc command :model/after
                                      (assoc after :request/status wrong))))))
  (doseq [wrong [2 4 999 -1]]
    (reject-plan! :declared-write-mismatch
                  #(gate/check-candidate!
                    (candidate (assoc command :model/after
                                      (assoc after :request/version wrong)))))))

(deftest bad-score-in-current-document-rejects-the-guard
  (doseq [score [7 6 0 -1]]
    (let [now (assoc before :request/score score)
          updated (assoc after :request/score score)]
      (reject-plan! :current-state-guard-failed
                    #(gate/check-candidate!
                      (assoc (candidate
                              (assoc command :model/before now
                                             :model/after updated))
                             :current-document now))))))

(deftest frame-score-writes-are-never-hidden-by-a-valid-claim
  (doseq [score [8 11 13 100]]
    (reject-plan! :frame-preservation-failed
                  #(gate/check-candidate!
                    (candidate (assoc command :model/after
                                      (assoc after :request/score score)))))))

(deftest type-invalid-status-and-score-must-be-typed-errors
  (doseq [[bad-state reason]
          [[(assoc after :request/status "claimed") :wrong-binding-type]
           [(assoc after :request/status :unknown) :enum-value-outside-domain]
           [(assoc after :request/status nil) :nil-for-nonnullable-binding]
           [(assoc after :request/score "12") :wrong-binding-type]
           [(assoc after :request/version "3") :wrong-binding-type]]]
    (reject! #(gate/check-candidate!
              (candidate (assoc command :model/after bad-state)))
             :gesso.assurance.rule-eval/invalid-rule-evaluation reason)))

(deftest extra-physical-fields-cannot-be-smuggled
  (doseq [document [(assoc before :request/secret :hidden)
                    (assoc after :request/secret :hidden)]]
    (let [bad (if (= :open (:request/status document))
                (assoc (candidate) :current-document document)
                (candidate (assoc command :model/after document)))]
      (reject-plan! :physical-field-footprint-mismatch
                    #(gate/check-candidate! bad))))
  (reject-plan! :physical-field-footprint-mismatch
                #(gate/check-candidate!
                  (candidate (assoc command :model/after
                                    (assoc after :request/new-field 3))))))

(deftest wrong-target-or-command-operation-is-not-accepted
  (reject-plan! :document-target-mismatch
                #(gate/check-candidate! (assoc (candidate) :target [:requests 9])))
  (reject-plan! :domain-command-mismatch
                #(gate/check-candidate!
                  (candidate (assoc command :model/operation :cancel))))
  (reject-plan! :domain-command-mismatch
                #(gate/check-candidate!
                  (candidate (assoc command :model/entity-type :other))))
  (let [data (failure #(gate/check-candidate!
                        (candidate (assoc command :model/before
                                          (assoc before :xt/id 9)))))]
    (is (= :gesso.model.command/invalid-command (:error/type data)) (pr-str data))))

(deftest missing-or-extra-write-is-rejected
  ;; Upstream canonical TX normalization fails earlier than the v1251
  ;; one-write gate for these two malformed transaction collections.
  (let [missing (failure #(gate/check-candidate!
                           (assoc-in (candidate) [:plan :commands] [])))
        repeated (failure #(gate/check-candidate!
                            (assoc-in (candidate) [:plan :commands]
                                      [command command])))]
    (is (= :gesso.model.tx/missing-commands (:error/type missing)) (pr-str missing))
    (is (= :gesso.model.tx/duplicate-command-targets
           (:error/type repeated)) (pr-str repeated)))
  ;; Two distinct valid targets get through normalizer but not the closed
  ;; single-domain-update gate.
  (reject-plan! :domain-write-count-mismatch
                #(gate/check-candidate!
                  (assoc-in (candidate) [:plan :commands]
                            [command
                             (-> command
                                 (assoc :model/id 11)
                                 (assoc-in [:model/expected :model/id] 11)
                                 (assoc-in [:model/before :xt/id] 11)
                                 (assoc-in [:model/after :xt/id] 11))]))))

(deftest stale-prestate-and-stale-expected-version-are-rejected
  (reject-plan! :stale-or-inconsistent-prestate
                #(gate/check-candidate!
                  (assoc (candidate) :current-document
                         (assoc before :request/version 1))))
  (reject-plan! :stale-update-check
                #(gate/check-candidate!
                  (candidate (assoc-in command [:model/expected :model/checks]
                                       [[:request/version 1]])))))

(deftest changed-to-claimed-current-state-rejects-claim
  (let [other (assoc before :request/status :claimed)]
    (reject-plan! :current-state-guard-failed
                  #(gate/check-candidate!
                    (assoc (candidate (assoc command :model/before other))
                           :current-document other)))))

(deftest unsupported-physical-effects-rejected
  ;; The canonical TX normalizer rejects a non-map :entry before v1251 can
  ;; reject executable effects. These are distinct typed failure boundaries.
  (doseq [extra [{:assertions [{:assert {:select [[1]]}}]}
                 {:tx-options {}}
                 {:entry-fn (fn [_] :bad)}]]
    (reject-plan! :unsupported-extra-effects
                  #(gate/check-candidate!
                    (update (candidate) :plan merge extra))))
  (let [data (failure #(gate/check-candidate!
                        (update (candidate) :plan assoc :entry :unexpected)))]
    (is (= :gesso.model.tx/invalid-entry (:error/type data)) (pr-str data))))

(deftest malformed-candidate-shapes-reject-as-expected
  (reject-plan! :expected-plain-map #(gate/check-candidate! []))
  (reject-plan! :wrong-keys #(gate/check-candidate! (assoc (candidate) :valid? true)))
  (reject-plan! :wrong-keys #(gate/check-candidate! (dissoc (candidate) :inputs)))
  (reject-plan! :metadata-not-allowed
                #(gate/check-candidate! (with-meta (candidate) {:proved? true})))
  (reject-plan! :expected-input-map
                #(gate/check-candidate! (assoc (candidate) :inputs [])))
  (reject-plan! :expected-field-map
                #(gate/check-candidate! (assoc (candidate) :field-map [])))
  (reject-plan! :expected-plan
                #(gate/check-candidate! (assoc (candidate) :plan []))))

(deftest physical-mapping-must-be-complete-and-injective
  (reject-plan! :incomplete-field-map
                #(gate/check-candidate!
                  (assoc (candidate) :field-map (dissoc field-map :request/note))))
  (reject-plan! :non-injective-or-invalid-field-map
                #(gate/check-candidate!
                  (assoc (candidate) :field-map
                         (assoc field-map :request/score :request/status))))
  (reject-plan! :non-injective-or-invalid-field-map
                #(gate/check-candidate!
                  (assoc (candidate) :field-map
                         (assoc field-map :request/score :xt/id)))))

(deftest mapped-field-names-can-differ-from-abstract-names
  (let [renamed {:request/status :physical/status
                 :request/version :physical/version
                 :request/score :physical/score
                 :request/note :physical/note}
        rewrite (fn [document]
                  (reduce-kv (fn [acc abstract physical]
                               (if (contains? document abstract)
                                 (-> acc (dissoc abstract)
                                     (assoc physical (get document abstract)))
                                 acc)) document renamed))
        b (rewrite before)
        a (rewrite after)
        c (-> command (assoc :model/before b :model/after a)
              (assoc-in [:model/expected :model/checks] [[:physical/version 2]]))
        witness (gate/check-candidate!
                 (-> (candidate c)
                     (assoc :current-document b :field-map renamed)))]
    (is (= gate/witness-type (:type witness)))
    (is (= :none (:authority witness)))))

(deftest source-is-rechecked-even-if-original-proof-report-is-copied
  (reject-plan! :wrong-keys
                #(gate/check-candidate!
                  (assoc (candidate) :certificate {:valid? true})))
  (reject-plan! :declared-write-mismatch
                #(gate/check-candidate!
                  (assoc-in (candidate)
                            [:construction :replacements :request/status]
                            [:literal :open]))))

(deftest genuine-registration-preserves-original-resolved-operation-contract
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))
        description (gate/describe-plan-operation registered)]
    (is (fn? registered))
    (is (gate/checked-plan-operation? registered))
    (is (certified/registered-operation? registered))
    (is (= gate/description-type (:type description)))
    (is (= operation-id (:operation description)))
    (is (= :registered-pre-submit-abstract-plan-gate (:checking description)))
    (is (= :none (:authority description)))
    (is (= :not-established (:atomicity-assurance description)))
    (is (= :not-proved (:invariant-preservation description)))
    (is (= :not-established (:physical-correspondence description)))
    (is (contains? (:open-obligations description)
                   :trusted-current-document-acquisition))
    (is (= :unresolved (:status (registered {} (invocation :reconcile)))))
    (is (= {:acquire 0 :inputs 0 :target 0 :construct 0} @(:calls env)))
    (is (identical? registered
                    (gate/require-current-plan-operation! registered
                                                          (:source env))))))

(deftest forged-or-copied-wrapper-is-not-registered
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))
        fake (with-meta (fn [& _] {:status :incorporated})
               (meta registered))]
    (is (false? (gate/checked-plan-operation? fake)))
    (reject-plan! :unregistered-plan-operation
                  #(gate/describe-plan-operation fake))
    (reject-plan! :unregistered-plan-operation
                  #(gate/require-current-plan-operation! fake (:source env)))
    (is (false? (gate/checked-plan-operation? nil)))))

(deftest stale-acquisition-callbacks-are-detected
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))]
    (doseq [k [:current-document! :transition-inputs! :target!]]
      (reject-plan! :stale-plan-source
                    #(gate/require-current-plan-operation!
                      registered
                      (assoc (:source env) k (fn [& args]
                                               (apply (get (:source env) k)
                                                      args))))))))

(deftest stale-typed-geometry-or-field-mapping-is-detected
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))]
    (reject-plan! :stale-plan-source
                  #(gate/require-current-plan-operation!
                    registered (assoc (:source env) :command-operation :cancel)))
    (reject-plan! :stale-plan-source
                  #(gate/require-current-plan-operation!
                    registered (assoc (:source env) :field-map
                                      (assoc field-map :request/score :physical/score))))
    (reject-plan! :stale-plan-source
                  #(gate/require-current-plan-operation!
                    registered (assoc-in (:source env)
                                         [:construction :replacements :request/status]
                                         [:literal :open])))))

(deftest proof-and-configuration-must-remain-current
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))
        current (:source env)]
    (is (identical? registered (gate/require-current-plan-operation! registered current)))
    (reject-plan! :stale-plan-source
                  #(gate/require-current-plan-operation!
                    registered (assoc-in current
                                         [:certified-source :proof-source :goal]
                                         [:true])))
    (reject-plan! :stale-resolved-configuration
                  #(gate/require-current-plan-operation!
                    registered (assoc-in current
                                         [:certified-source :resolved-configuration :construct!]
                                         (fn [_ _] {:now :other}))))))

(deftest registration-refuses-incorrect-proof-or-untrusted-construction
  (let [env (environment)
        source (:source env)]
    (reject-plan! :proof-transition-source-mismatch
                  #(gate/build-certified-plan-operation!
                    (assoc-in source [:construction :replacements :request/status]
                              [:literal :open])))
    (reject-plan! :wrong-keys
                  #(gate/build-certified-plan-operation! (assoc source :valid? true)))
    (reject-plan! :missing-trusted-acquisition-callback
                  #(gate/build-certified-plan-operation!
                    (assoc source :current-document! nil)))))

(deftest invalid-plan-is-rejected-before-durable-submit
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))
        submissions (atom [])]
    (with-redefs [tx/transact! (fn [& args]
                                 (swap! submissions conj args)
                                 (throw (ex-info "UNEXPECTED SUBMISSION" {})))]
      (doseq [[bad reason]
              [[(assoc command :model/after
                       (assoc after :request/status :open))
                :declared-write-mismatch]
               [(assoc command :model/after
                       (assoc after :request/score 99))
                :frame-preservation-failed]
               [(assoc command :model/after
                       (assoc after :request/hidden :secret))
                :physical-field-footprint-mismatch]]]
        (reset! (:proposed env) bad)
        (reject-plan! reason #(registered {} (invocation :submit)))))
    (is (empty? @submissions))
    (is (= {:acquire 3 :inputs 3 :target 3 :construct 3} @(:calls env)))))

(deftest accepted-plan-reaches-one-canonical-submission-with-command-record
  (let [env (environment)
        registered (gate/build-certified-plan-operation! (:source env))
        submissions (atom [])]
    (with-redefs [tx/transact! (fn [_ctx plan]
                                 (swap! submissions conj plan)
                                 {:commit/status :committed
                                  :tx-result :simulated-commit})]
      (let [result (registered {} (invocation :submit))]
        (is (= :incorporated (:status result)))
        (is (= {:status :claimed} (:outcome result)))
        (is (= 1 (count @submissions)))
        (let [submitted (first @submissions)
              commands (:commands submitted)]
          (is (= 2 (count commands)))
          (is (= #{[:requests 10] [:gesso_commands "v1252-claim"]}
                 (set (map (juxt :model/entity-type :model/id) commands))))
          (is (= command (first (filter #(= :requests (:model/entity-type %))
                                       commands))))
          (is (= false (:emit submitted))))))
    (is (= {:acquire 1 :inputs 1 :target 1 :construct 1} @(:calls env)))))

(deftest original-constructor-cannot-return-fabricated-approval
  (let [env (environment)
        bad-source (assoc-in (:source env)
                             [:certified-source :resolved-configuration :construct!]
                             (fn [_ _]
                               {:now :clock :plan (:plan (candidate))
                                :outcome {:status :claimed}
                                :valid? true}))
        registered (gate/build-certified-plan-operation! bad-source)]
    (reject-plan! :invalid-original-construction
                  #(registered {} (invocation :submit)))))

(deftest malformed-plan-source-is-not-implicitly-admitted
  (let [env (environment)
        src (:source env)]
    (reject-plan! :missing-trusted-constructor
                  #(gate/build-certified-plan-operation!
                    (assoc-in src [:certified-source :resolved-configuration :construct!]
                              nil)))
    (reject-plan! :invalid-command-operation
                  #(gate/build-certified-plan-operation!
                    (assoc src :command-operation :create)))
    (reject-plan! :proof-transition-source-mismatch
                  #(gate/build-certified-plan-operation!
                    (assoc-in src [:construction :subject :id] :example/cancel)))))

(deftest valid-score-and-optional-frame-matrix
  ;; Distinct legal scores and three optional-field shapes must not depend
  ;; on a special case in the pre-submit interpreter.
  (doseq [score [8 9 12 100 1000000]
          [had-note? note] [[false nil] [true nil] [true "kept"]]]
    (let [b (cond-> (assoc before :request/score score)
              had-note? (assoc :request/note note))
          a (cond-> (assoc after :request/score score)
              had-note? (assoc :request/note note))
          c (assoc command :model/before b :model/after a)
          checked (gate/check-candidate!
                   (assoc (candidate c) :current-document b))]
      (is (= gate/witness-type (:type checked)))
      (is (= :none (:authority checked))))))

(deftest frame-mutation-matrix-never-becomes-an-accepted-plan
  (doseq [bad-score [7 8 9 11 13 14 100 1000]
          :when (not= bad-score (:request/score after))]
    (reject-plan! :frame-preservation-failed
                  #(gate/check-candidate!
                    (candidate (assoc command :model/after
                                      (assoc after :request/score bad-score))))))
  (doseq [note [nil "written" "" 0 true]]
    (let [bad (assoc after :request/note note)
          reason (if (or (string? note) (nil? note))
                   :frame-preservation-failed nil)
          data (failure #(gate/check-candidate!
                          (candidate (assoc command :model/after bad))))]
      (is (map? data))
      (if reason
        (is (= reason (:reason data)) (pr-str data))
        (do
          (is (= :gesso.assurance.rule-eval/invalid-rule-evaluation
                 (:error/type data)) (pr-str data))
          (is (= :wrong-binding-type (:reason data)) (pr-str data)))))))

(deftest copied-abstract-witness-cannot-create-a-certified-executor
  (let [witness (gate/check-candidate! (candidate))
        fabricated (assoc witness :valid? true :authority :committed
                          :transaction-submitted? true)]
    (is (= :none (:authority witness)))
    (is (false? (gate/checked-plan-operation? witness)))
    (is (false? (gate/checked-plan-operation? fabricated)))
    (reject-plan! :unregistered-plan-operation
                  #(gate/describe-plan-operation fabricated))))

(deftest extra-metadata-on-runtime-observations-is-rejected
  (reject-plan! :metadata-not-allowed
                #(gate/check-candidate!
                  (assoc (candidate) :plan
                         (with-meta (:plan (candidate)) {:approved true}))))
  (reject-plan! :metadata-not-allowed
                #(gate/check-candidate!
                  (assoc (candidate) :current-document
                         (with-meta before {:authoritative true}))))
  (reject-plan! :metadata-not-allowed
                #(gate/check-candidate!
                  (assoc (candidate) :inputs
                         (with-meta inputs {:inferred true}))))
  (reject-plan! :metadata-not-allowed
                #(gate/check-candidate!
                  (assoc (candidate) :field-map
                         (with-meta field-map {:verified true})))))

(deftest incorrect-acquired-current-state-cannot-reach-physical-submit
  (let [env (environment)
        source (assoc (:source env) :current-document!
                      (fn [_ _] (assoc before :request/version 1)))
        registered (gate/build-certified-plan-operation! source)
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! submissions inc))]
      (reject-plan! :stale-or-inconsistent-prestate
                    #(registered {} (invocation :submit))))
    (is (zero? @submissions))))

(deftest incorrect-acquired-target-cannot-reach-physical-submit
  (let [env (environment)
        source (assoc (:source env) :target!
                      (fn [_ _] [:requests 11]))
        registered (gate/build-certified-plan-operation! source)
        submissions (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! submissions inc))]
      (reject-plan! :document-target-mismatch
                    #(registered {} (invocation :submit))))
    (is (zero? @submissions))))

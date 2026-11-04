(ns gesso.model.certified-operation-test
  "v1250: adversarial proof-linked authoritative executor registration.

   A live v1237 abstract certificate is necessary for this registration, but
   cannot be copied, replaced by its report, or promoted into proof of physical
   execution, authorization, atomicity, invariant preservation or route adoption.
   All negative checks demand a named typed failure; positive checks prove
   that registration actually produces a callable resolved-operation wrapper.
   No XTDB node or external side effects are required for this test namespace."
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-operation :as certified]
            [gesso.model.resolved-operation :as resolved]))

(def ^:private operation-id :example/claim)
(def ^:private raw-subject
  {:kind :operation :id operation-id
   :interface {:operation operation-id}})

(def ^:private original-derivation
  {:transition
   {:subject raw-subject
    :state-types {:request/status {:type :enum :name :request/status
                                   :values #{:open :claimed}}
                  :request/version {:type :integer}
                  :request/note {:type :string :optional? true :nullable? true}}
    :input-types {}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal :open]]}
    :reads #{:request/status}
    :writes #{:request/status}
    :assignments {:request/status [:literal :claimed]}}
   :steps [{:id :step/guard :rule :guard :conclusion [:guard]}
           {:id :step/status :rule :assignment :field :request/status
            :conclusion [:eq [:post :request/status] [:literal :claimed]]}
           {:id :step/summary :rule :conjunction-introduction
            :from [:step/guard :step/status]
            :conclusion [:and [:guard]
                         [:eq [:post :request/status] [:literal :claimed]]]}]
   :goal [:and [:guard]
          [:eq [:post :request/status] [:literal :claimed]]]})

(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private abstract-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})
(def ^:private registration-obligations
  #{:deployed-handler-and-route-registration
    :current-callback-authorization-and-purity
    :authoritative-plan-and-effect-correspondence
    :source-to-production-executor-correspondence
    :physical-transaction-and-recovery-evidence
    :application-invariant-preservation})
(def ^:private error-type
  :gesso.model.certified-operation/invalid-certified-operation)

(defn- rejection-data [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo ex
      (ex-data ex))))

(defn- rejects-as! [thunk expected-type expected-reason]
  (let [data (rejection-data thunk)]
    (is (= expected-type (:error/type data)) (pr-str data))
    (is (= expected-reason (:reason data)) (pr-str data))
    (is (vector? (:at data)) (pr-str data))
    data))

(defn- reject-registration! [thunk reason]
  (rejects-as! thunk error-type reason))

(defn- reject-resolved! [thunk kind]
  (let [data (rejection-data thunk)]
    (is (= :gesso.model.resolved-operation/invalid-operation
           (:error/type data)) (pr-str data))
    (is (= kind (:error/kind data)) (pr-str data))
    (is (map? (:error/details data)) (pr-str data))
    data))

(def ^:private proof-fixture
  ;; Deliberately share the immutable ORIGINAL proof and its live handle across
  ;; tests, but never a forged copyable description of them.
  (delay
    (let [graph {:roots #{operation-id}
                 :nodes [{:id operation-id :subject raw-subject
                          :subject-handle (admission/admit-subject! raw-subject)
                          :depends-on []}]}
          handle (dependency/check-graph! graph)
          cert (certificate/admit-certificate!
                {:proof-kind :guarded-transition
                 :proof-source original-derivation
                 :graph-handle handle
                 :current-graph graph
                 :expected-open-premises #{relation-premise}
                 :expected-open-obligations abstract-obligations})]
      {:graph graph :certificate cert})))

(defn- environment []
  (let [{:keys [graph certificate]} @proof-fixture
        calls (atom {:read 0 :disclose 0 :construct 0})
        configuration
        {:operation operation-id
         :read-record! (fn [_ctx _id]
                         (swap! calls update :read inc)
                         nil)
         :may-disclose? (fn [_ctx _binding _record]
                          (swap! calls update :disclose inc)
                          true)
         :construct! (fn [_ctx _binding]
                       (swap! calls update :construct inc)
                       (throw (ex-info "must never construct on reconcile" {})))}]
    {:calls calls
     :configuration configuration
     :source {:operation operation-id
              :certificate certificate
              :proof-source original-derivation
              :current-graph graph
              :resolved-configuration configuration}}))

(defn- register! [environment]
  (certified/build-certified-operation! (:source environment)))

(defn- current [environment]
  {:current-proof-source original-derivation
   :current-graph (:graph @proof-fixture)
   :current-configuration (:configuration environment)})

(defn- invocation [mode]
  {:binding {:command-id (identity/identity :command "v1250-test-command")
             :principal (identity/identity :principal :example/helper)
             :operation operation-id :arguments {}}
   :mode mode})

(deftest admitted-operation-is-a-real-callable
  (let [env (environment)
        registered (register! env)]
    (is (fn? registered))
    (is (true? (certified/registered-operation? registered)))
    (is (identical? registered
                    (certified/require-current-operation! registered
                                                          (current env))))
    (is (= operation-id (:gesso.model/operation-id (meta registered))))
    (is (= :durable-command-v1 (:gesso.model/resolution (meta registered))))
    (is (= :source-relative-abstract-proof-only
           (:gesso.model/proof-link-scope (meta registered))))
    (let [response (registered {} (invocation :reconcile))]
      (is (= :unresolved (:status response)))
      (is (= :gesso.model.resolved-operation/operation-v1
             (get-in response [:operation-scope :construction])))
      (is (= {:read 1 :disclose 0 :construct 0} @(:calls env))))))

(deftest report-is-a-narrow-copyable-description
  (let [registered (register! (environment))
        report (certified/describe-operation registered)
        original (certificate/describe-certificate (:certificate @proof-fixture))]
    (is (= certified/report-type (:type report)))
    (is (= certified/registration-version (:version report)))
    (is (= certified/registration-type (:registration report)))
    (is (= operation-id (:operation report)))
    (is (= (:fingerprint original) (:proof-fingerprint report)))
    (is (= (:source-fingerprint original) (:proof-source-fingerprint report)))
    (is (= (:subject-fingerprint original) (:proof-subject-fingerprint report)))
    (is (= (:dependency-graph-fingerprint original)
           (:dependency-graph-fingerprint report)))
    (is (= #{relation-premise} (:open-premises report)))
    (is (= (into abstract-obligations registration-obligations)
           (:open-obligations report)))
    (is (= #{:live-abstract-certificate-rechecked
             :exact-operation-subject-identity
             :resolved-executor-created-from-configuration
             :in-process-executor-identity-registered}
           (:checked report)))
    (doseq [[key expected]
            {:proof-scope :matching-abstract-transition-only
             :registration-scope :in-process-proof-linked-executor-only
             :assumption-discharge :not-performed
             :invariant-preservation :not-proved
             :business-correctness :not-proved
             :implementation-correspondence :not-established
             :runtime-effect-enforcement :not-established
             :atomicity-assurance :not-established
             :deployed-route-adoption :not-established
             :certificate :source-relative-abstract-proof-only
             :authority :none}]
      (is (= expected (get report key)) (name key)))
    (is (not (contains? report :valid?)))
    (is (not (contains? report :proved?)))
    (is (not (certified/registered-operation? report)))))

(deftest repeated-registrations-have-distinct-executor-identities
  (let [env (environment)
        a (register! env)
        b (register! env)]
    (is (not (identical? a b)))
    (is (certified/registered-operation? a))
    (is (certified/registered-operation? b))
    (is (= (certified/describe-operation a)
           (certified/describe-operation b)))))

(deftest raw-resolved-executor-is-not-certified
  (let [env (environment)
        raw (resolved/build-operation (:configuration env))]
    (is (fn? raw))
    (is (false? (certified/registered-operation? raw)))
    (reject-registration! #(certified/describe-operation raw)
                          :unregistered-executor)
    (reject-registration! #(certified/require-current-operation! raw
                                                                 (current env))
                          :unregistered-executor)))

(deftest metadata-and-report-copies-never-register-an-executor
  (let [env (environment)
        original (register! env)
        report (certified/describe-operation original)
        copied-fn (with-meta (fn [_ _] {:status :incorporated}) (meta original))
        altered-copy (with-meta original
                       (assoc (meta original) :gesso.model/proof-link-scope
                              :forged-metadata))]
    (doseq [candidate [report copied-fn altered-copy
                       {:executor original :valid? true}]]
      (is (false? (certified/registered-operation? candidate)))
      (reject-registration! #(certified/describe-operation candidate)
                            :unregistered-executor)
      (reject-registration! #(certified/require-current-operation! candidate
                                                                   (current env))
                            :unregistered-executor))
    (is (certified/registered-operation? original))))

(deftest forged-certificate-reports-and-new-unissued-handles-fail
  (let [{:keys [source]} (environment)
        cert (:certificate source)]
    (doseq [forgery [nil {}
                     (certificate/describe-certificate cert)
                     (assoc (certificate/describe-certificate cert) :valid? true)
                     (certificate/->CertificateHandle)]]
      (reject-registration!
       #(certified/build-certified-operation!
         (assoc source :certificate forgery))
       :unadmitted-proof-certificate))))

(deftest absent-contract-or-registration-inputs-rejected
  (let [{:keys [source]} (environment)]
    (doseq [key [:certificate :proof-source :current-graph
                 :resolved-configuration :operation]]
      (reject-registration!
       #(certified/build-certified-operation! (dissoc source key))
       :wrong-keys))
    (reject-registration!
     #(certified/build-certified-operation! (assoc source :executor (fn [_ _] {})))
     :wrong-keys)
    (reject-registration!
     #(certified/build-certified-operation! (assoc source :valid? true))
     :wrong-keys)
    (reject-registration!
     #(certified/build-certified-operation! (assoc source :certified true))
     :wrong-keys)))

(deftest identity-validation-rejects-unqualified-and-mismatched-operation
  (let [{:keys [source]} (environment)]
    (doseq [bad [:claim "example/claim" nil 23]]
      (reject-registration!
       #(certified/build-certified-operation! (assoc source :operation bad))
       :invalid-operation-id))
    (reject-registration!
     #(certified/build-certified-operation!
       (assoc-in source [:resolved-configuration :operation] :example/cancel))
     :executor-operation-mismatch)
    (reject-registration!
     #(certified/build-certified-operation!
       (-> source
           (assoc :operation :example/cancel)
           (assoc-in [:resolved-configuration :operation] :example/cancel)))
     :operation-subject-mismatch)))

(deftest build-requires-plain-closed-inputs
  (let [{:keys [source]} (environment)]
    (doseq [bad [nil [] '() "source" (atom source)]]
      (reject-registration!
       #(certified/build-certified-operation! bad)
       :expected-plain-map))
    (reject-registration!
     #(certified/build-certified-operation! (with-meta source {:approved true}))
     :metadata-not-allowed)
    (reject-registration!
     #(certified/build-certified-operation!
       (assoc source :resolved-configuration
              (with-meta (:resolved-configuration source) {:trusted true})))
     :metadata-not-allowed)
    (reject-registration!
     #(certified/build-certified-operation!
       (assoc source :resolved-configuration []))
     :expected-plain-map)))

(deftest underlying-resolved-operation-must-validate-its-callbacks
  (let [{:keys [source]} (environment)]
    (doseq [key [:read-record! :may-disclose? :construct!]]
      (reject-resolved!
       #(certified/build-certified-operation!
         (assoc-in source [:resolved-configuration key] nil))
       :missing-function))
    (reject-resolved!
     #(certified/build-certified-operation!
       (assoc-in source [:resolved-configuration :unlisted-flag] true))
     :invalid-configuration)))

(deftest original-proof-is-rechecked-at-registration
  (let [{:keys [source]} (environment)
        altered (assoc (:proof-source source) :goal [:guard])]
    (rejects-as!
     #(certified/build-certified-operation!
       (assoc source :proof-source altered))
     :gesso.assurance.proof.derivation/invalid-derivation
     :wrong-final-conclusion)))

(deftest altered-proof-subject-cannot-evade-a-live-certificate
  (let [env (environment)
        fake (assoc-in (:source env) [:proof-source :transition :subject :id]
                       :example/other)]
    (rejects-as!
     #(certified/build-certified-operation! fake)
     :gesso.assurance.proof.certificate/invalid-certificate
     :wrong-certificate-roots)))

(deftest require-current-accepts-equal-description-with-original-callbacks
  (let [env (environment)
        registered (register! env)
        config-copy (into {} (:configuration env))]
    (is (= (:configuration env) config-copy))
    (is (identical? registered
                    (certified/require-current-operation!
                     registered (assoc (current env)
                                       :current-configuration config-copy))))))

(deftest changed-callback-identity-is-stale-even-if-behavior-is-equivalent
  (let [env (environment)
        registered (register! env)
        old (get-in env [:configuration :construct!])
        changed (assoc (:configuration env)
                       :construct! (fn [ctx binding]
                                     (old ctx binding)))]
    (reject-registration!
     #(certified/require-current-operation!
       registered (assoc (current env) :current-configuration changed))
     :stale-executor-configuration)))

(deftest altered-config-shape-or-operation-is-stale
  (let [env (environment)
        registered (register! env)]
    (doseq [changed [(dissoc (:configuration env) :construct!)
                     (assoc (:configuration env) :operation :example/cancel)
                     (assoc (:configuration env) :injected true)
                     (with-meta (:configuration env) {:approved true})
                     []]]
      (reject-registration!
       #(certified/require-current-operation!
         registered (assoc (current env) :current-configuration changed))
       :stale-executor-configuration))))

(deftest future-proofing-optional-callbacks-have-identity-checks
  (let [env (environment)
        optional (fn [& _] {:required-guard-targets #{}
                            :allowed-guard-targets #{}
                            :required-write-targets #{}
                            :allowed-write-targets #{}})
        contract {:required-guard-tables #{} :allowed-guard-tables #{}
                  :required-write-tables #{:gesso_commands}
                  :allowed-write-tables #{:gesso_commands}}
        config (assoc (:configuration env)
                      :construction-contract contract
                      :target-contract! optional)
        certified-source (assoc (:source env) :resolved-configuration config)
        registered (certified/build-certified-operation! certified-source)]
    (is (certified/registered-operation? registered))
    (is (identical? registered
                    (certified/require-current-operation!
                     registered (assoc (current env) :current-configuration config))))
    (reject-registration!
     #(certified/require-current-operation!
       registered (assoc (current env) :current-configuration
                         (assoc config :target-contract! (fn [& args]
                                                           (apply optional args)))))
     :stale-executor-configuration)))

(deftest require-current-rejects-extra-and-missing-current-keys
  (let [env (environment)
        registered (register! env)]
    (doseq [current-source [(dissoc (current env) :current-graph)
                           (assoc (current env) :valid? true)
                           (assoc (current env) :authority :committed)]]
      (reject-registration!
       #(certified/require-current-operation! registered current-source)
       :wrong-keys))
    (reject-registration!
     #(certified/require-current-operation!
       registered (with-meta (current env) {:trusted true}))
     :metadata-not-allowed)
    (reject-registration!
     #(certified/require-current-operation! registered nil)
     :expected-plain-map)))

(deftest source-proofs-cannot-be-replaced-during-freshness-check
  (let [env (environment)
        registered (register! env)
        changed (assoc original-derivation :goal [:guard])]
    (rejects-as!
     #(certified/require-current-operation!
       registered (assoc (current env) :current-proof-source changed))
     :gesso.assurance.proof.derivation/invalid-derivation
     :wrong-final-conclusion)))

(deftest dependency-graph-changes-cannot-be-hidden-by-a-cached-report
  (let [env (environment)
        registered (register! env)
        graph (:graph @proof-fixture)
        changed (assoc graph :roots #{:example/other})]
    (is (string? (:dependency-graph-fingerprint
                  (certified/describe-operation registered))))
    (let [error (rejection-data
                 #(certified/require-current-operation!
                   registered (assoc (current env) :current-graph changed)))]
      (is (= :gesso.assurance.dependency/invalid-dependency-graph
             (:error/type error)) (pr-str error))
      (is (= :missing-root (:reason error)) (pr-str error)))))

(deftest forged-source-and-executor-metadata-cannot-create-a-registration
  (let [env (environment)
        registered (register! env)
        report (certified/describe-operation registered)
        copyable (assoc report :checked true :authority :full)
        forged (with-meta (fn [_ _] {:status :incorporated})
                 (assoc (meta registered)
                        :gesso.model/proof-link-scope :source-relative-abstract-proof-only))]
    (is (false? (certified/registered-operation? forged)))
    (reject-registration! #(certified/describe-operation forged)
                          :unregistered-executor)
    (reject-registration!
     #(certified/build-certified-operation!
       (assoc (:source env) :certificate copyable))
     :unadmitted-proof-certificate)))

(deftest bad-invocation-still-rejected-by-original-executor
  (let [env (environment)
        registered (register! env)]
    (doseq [bad [{:binding nil :mode :reconcile}
                 {:mode :reconcile}
                 {:binding (:binding (invocation :reconcile)) :mode :unknown}]]
      (let [data (rejection-data #(registered {} bad))]
        (is (contains? #{:gesso.model.resolved-operation/invalid-operation
                         :gesso.model.command-resolution/invalid-resolution}
                       (:error/type data)) (pr-str data))
        (is (keyword? (:error/kind data)) (pr-str data))))
    (is (= {:read 0 :disclose 0 :construct 0} @(:calls env)))))

(deftest registered-reconcile-never-creates-authoritative-state
  (let [env (environment)
        registered (register! env)]
    (doseq [_ (range 3)]
      (is (= :unresolved
             (:status (registered {} (invocation :reconcile))))) )
    (is (= {:read 3 :disclose 0 :construct 0} @(:calls env)))
    (is (= :not-established
           (:runtime-effect-enforcement (certified/describe-operation registered))))))

(deftest syntactically-valid-but-different-proof-cannot-reuse-certificate
  (let [env (environment)
        registered (register! env)
        renamed (walk/postwalk (fn [value]
                                 (if (= :step/status value)
                                   :step/reidentified
                                   value))
                               original-derivation)
        report (derivation/check-transition-derivation! renamed)]
    ;; Prove this is a genuinely valid new derivation, not a malformed source.
    (is (= derivation/report-type (:type report)))
    (is (not= renamed original-derivation))
    (rejects-as!
     #(certified/require-current-operation!
       registered (assoc (current env) :current-proof-source renamed))
     :gesso.assurance.proof.certificate/invalid-certificate
     :stale-certificate)
    (rejects-as!
     #(certified/build-certified-operation!
       (assoc (:source env) :proof-source renamed))
     :gesso.assurance.proof.certificate/invalid-certificate
     :stale-certificate)))

(deftest actual-propositional-certificate-cannot-register-as-transition-proof
  ;; This genuinely creates a live certificate of the WRONG proof kind,
  ;; rather than passing a forged or malformed certificate.
  (let [proposition-subject
        {:kind :rule :id :example/tautology
         :interface {:theorem :example/reflexive}}
        proof {:subject proposition-subject
               :variables {} :premises {}
               :steps [{:id :step/reflex :rule :implication-reflexivity
                        :conclusion [:implies [:true] [:true]]}]
               :goal [:implies [:true] [:true]]}
        graph {:roots #{:example/tautology}
               :nodes [{:id :example/tautology :subject proposition-subject
                        :subject-handle (admission/admit-subject! proposition-subject)
                        :depends-on []}]}
        issued (certificate/admit-certificate!
                {:proof-kind :propositional
                 :proof-source proof
                 :graph-handle (dependency/check-graph! graph)
                 :current-graph graph
                 :expected-open-premises #{}
                 :expected-open-obligations
                 #{:implementation-correspondence
                   :current-runtime-authorization
                   :checker-meta-soundness}})
        source (:source (environment))]
    (is (certificate/checked-certificate? issued))
    (is (= :propositional
           (:proof-kind (certificate/describe-certificate issued))))
    (reject-registration!
     #(certified/build-certified-operation!
       (assoc source :certificate issued
                     :proof-source proof
                     :current-graph graph))
     :operation-subject-mismatch)))

(deftest swapped-command-binding-cannot-act-through-registered-executor
  (let [env (environment)
        registered (register! env)
        wrong (assoc-in (invocation :reconcile) [:binding :operation]
                        :example/cancel)]
    (reject-resolved! #(registered {} wrong) :operation-mismatch)
    (is (= {:read 0 :disclose 0 :construct 0} @(:calls env)))))

(deftest every-trusted-callback-identity-is-part-of-current-registration
  (let [env (environment)
        registered (register! env)]
    (doseq [key [:read-record! :may-disclose? :construct!]]
      (let [original (get (:configuration env) key)
            replaced (assoc (:configuration env) key
                            (fn [& args] (apply original args)))]
        ;; Even if the callback computes the same answer, it is not the
        ;; callable object that was bound to the admitted executor.
        (reject-registration!
         #(certified/require-current-operation!
           registered (assoc (current env) :current-configuration replaced))
         :stale-executor-configuration)))
    (is (identical? registered
                    (certified/require-current-operation! registered
                                                          (current env))))))

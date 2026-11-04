(ns gesso.assurance.certified-claim-fixture
  "v1255: a reusable, Gesso-local, real-shaped Claim assurance fixture.

   This is deliberately NOT the HumanHelp production model. Its single request
   has an owner, a lifecycle status and an optimistic revision. Claim is
   permitted by the ABSTRACT transition only from :open with no owner, with
   a positive opaque authorization decision. The sole permitted write changes
   status to :claimed, sets one owner and advances the revision. Organization
   and description remain in the explicit frame.

   The opaque authorization decision comes from a supplied Clojure callback.
   Its input/output shapes are checked by v1227. The callback's correctness,
   purity, freshness and principal provenance are NOT proved. The supplied
   current-document callback is also trusted, and may run more than once in a
   submission. Only XTDB atomic concurrency checks can make the one-winner
   relation authoritative under racing requests; this fixture does not.

   The dependency DAG links the operation to the authorization contract and
   the complete *declared* invariant set with exact subject fingerprints.
   A live certificate checks only the conditional guard derivation. The v1241
   initial/preservation obligations remain open, as do production/refinement,
   physical commit and broader application invariant-coverage obligations.

   Public entry points:
     (describe-claim!)
     (new-claim-environment! {:request-id 17 :actor :helper/alice
                              :organization :organization/one
                              :read-current! fn :authorization! fn
                              :read-record! fn :may-disclose? fn})
     (candidate! document actor organization authorization-decision)

   No test or report may promote this fixture into a production proof."
  (:require [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.opaque :as opaque]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as transition]
            [gesso.model.certified-plan :as certified-plan]))

(def claim-operation :example/claim)
(def authorization-id :example/claim-authorization)
(def invariant-id :example/claim-invariants)
(def physical-table :requests)
(def command-operation :claim)

(def claim-subject
  {:kind :operation :id claim-operation
   :interface {:operation claim-operation
               :authorization authorization-id
               :invariant-set invariant-id
               :version 1}})

(def authorization-contract
  {:id authorization-id
   :implementation :opaque-clojure
   :implementation-boundary :example/authorize-claim
   :input {:rule {:readers #{:auth/request-id :auth/actor :auth/organization}
                  :expression
                  [:and [:> [:read :auth/request-id] [:literal 0]]
                   [:not= [:read :auth/actor] [:literal :identity/anonymous]]
                   [:not= [:read :auth/organization]
                    [:literal :organization/none]]]}
           :reader-types {:auth/request-id {:type :integer}
                          :auth/actor {:type :keyword}
                          :auth/organization {:type :keyword}}}
   :output {:rule {:readers #{:auth/decision}
                   :expression
                   [:in [:read :auth/decision] [:literal #{:permit :deny}]]}
            :reader-types
            {:auth/decision {:type :enum :name :example/authorization-decision
                             :values #{:permit :deny}}}}
   :effects {:mode :unknown :external-effects #{}}
   :termination {:mode :unknown :timeout-ms nil}})

(def construction
  {:subject claim-subject
   :state-types
   {:request/status {:type :enum :name :example/request-status
                     :values #{:open :claimed}}
    :request/owner {:type :keyword :nullable? true}
    :request/revision {:type :integer}
    :request/organization {:type :keyword}
    :request/description {:type :string}}
   :input-types
   {:input/authorized {:type :boolean}
    :input/actor {:type :keyword}
    :input/organization {:type :keyword}
    :input/next-revision {:type :integer}}
   :guard
   {:readers #{:request/status :request/owner :request/organization
               :input/authorized :input/organization
               :request/revision :input/next-revision}
    :expression
    [:and [:= [:read :request/status] [:literal :open]]
     [:= [:read :request/owner] [:literal nil]]
     [:= [:read :request/organization] [:read :input/organization]]
     [:= [:read :input/authorized] [:literal true]]
     [:>= [:read :request/revision] [:literal 0]]
     [:> [:read :input/next-revision] [:read :request/revision]]]}
   :reads #{:request/status :request/owner :request/organization
            :request/revision
            :input/authorized :input/organization :input/actor
            :input/next-revision}
   :writes #{:request/status :request/owner :request/revision}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]
                  :request/revision [:read :input/next-revision]}
   :frame #{:request/organization :request/description}})

(def field-map
  {:request/status :request/status
   :request/owner :request/owner
   :request/revision :request/revision
   :request/organization :request/organization
   :request/description :request/description})

(def initial-condition
  {:readers #{:request/status :request/owner :request/revision}
   :expression
   [:and [:= [:read :request/status] [:literal :open]]
    [:= [:read :request/owner] [:literal nil]]
    [:>= [:read :request/revision] [:literal 0]]]})

(def invariants
  {:claim/owner-matches-lifecycle
   {:readers #{:request/status :request/owner}
    :expression
    [:and
     [:implies [:= [:read :request/status] [:literal :open]]
      [:= [:read :request/owner] [:literal nil]]]
     [:implies [:= [:read :request/status] [:literal :claimed]]
      [:not= [:read :request/owner] [:literal nil]]]]}
   :claim/nonnegative-revision
   {:readers #{:request/revision}
    :expression [:>= [:read :request/revision] [:literal 0]]}})

(def obligation-source
  {:construction construction
   :initial-condition initial-condition
   :invariants invariants})

(def ^:private conditional-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private conditional-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})

(def ^:private external-obligations
  #{:opaque-authorization-policy-correctness
    :opaque-authorization-callback-provenance-and-purity
    :trusted-principal-and-organization-binding
    :all-authoritative-claim-paths-use-this-operation
    :all-other-owner-writers-respect-the-invariants
    :every-real-initial-state-satisfies-declared-invariants
    :single-commit-atomic-status-and-revision-guard
    :current-acquisition-to-commit-correspondence
    :checked-proof-does-not-establish-invariant-preservation
    :proof-checker-meta-soundness
    :physical-xtdb-execution-correspondence})

(defn- invalid! [reason details]
  (throw (ex-info "Invalid certified Claim fixture."
                  (merge {:error/type ::invalid-fixture :reason reason} details))))

(defn- authorization-input [request-id actor organization]
  {:auth/request-id request-id
   :auth/actor actor
   :auth/organization organization})

(defn authorization-inputs!
  "Validate an application-supplied opaque decision, then bind the checked
   result to the boolean abstract guard input. No policy correctness follows."
  [request-id actor organization decision]
  (opaque/validate-input! authorization-contract
                          (authorization-input request-id actor organization))
  (opaque/validate-result! authorization-contract decision)
  {:input/authorized (= :permit (:auth/decision decision))
   :input/actor actor
   :input/organization organization})

(defn- inputs-for! [document actor organization decision]
  (let [request-id (:xt/id document)
        provided (authorization-inputs! request-id actor organization decision)
        revision (:request/revision document)]
    (when-not (and (integer? revision) (not (neg? revision)))
      (invalid! :invalid-current-revision {:request-id request-id}))
    (assoc provided :input/next-revision (inc' revision))))

(defn claim-command!
  "Describe a single version-checked canonical domain update. This is an
   UNCOMMITTED command. The status/revision assertions must be checked
   atomically by the physical transaction path, not merely on acquisition."
  [document actor]
  (let [request-id (:xt/id document)
        revision (:request/revision document)]
    (when-not (and (integer? request-id) (pos? request-id)
                   (integer? revision) (not (neg? revision)))
      (invalid! :invalid-current-document-identity
                {:request-id request-id}))
    {:model/entity-type physical-table
     :model/operation command-operation
     :model/id request-id
     :model/expected
     {:model/id request-id
      :model/checks [[:request/revision revision]
                     [:request/status :open]]}
     :model/before document
     :model/after (assoc document
                         :request/status :claimed
                         :request/owner actor
                         :request/revision (inc' revision))}))

(defn candidate!
  "Build an exact v1251 candidate from one document and a supplied opaque
   decision. The candidate is not a certificate and is not submission authority."
  [document actor organization decision]
  (let [inputs (inputs-for! document actor organization decision)]
    {:construction construction
     :field-map field-map
     :command-operation command-operation
     :target [physical-table (:xt/id document)]
     :current-document document
     :inputs inputs
     :plan {:commands [(claim-command! document actor)] :emit false}}))

(defn check-candidate!
  "Check the concrete candidate through the ORIGINAL v1251 pre-submit gate."
  [document actor organization decision]
  (certified-plan/check-candidate!
   (candidate! document actor organization decision)))

(defn- dependency-source!
  []
  (let [auth-subject
        {:kind :operation :id authorization-id
         :interface {:contract authorization-contract}}
        rules-subject
        {:kind :rule :id invariant-id
         :interface {:initial-condition initial-condition
                     :invariants invariants}}
        auth-fingerprint (:fingerprint (subject/describe-subject auth-subject))
        rules-fingerprint (:fingerprint (subject/describe-subject rules-subject))]
    {:roots #{claim-operation}
     :nodes
     [{:id authorization-id :subject auth-subject
       :subject-handle (admission/admit-subject! auth-subject)
       :depends-on []}
      {:id invariant-id :subject rules-subject
       :subject-handle (admission/admit-subject! rules-subject)
       :depends-on []}
      {:id claim-operation :subject claim-subject
       :subject-handle (admission/admit-subject! claim-subject)
       :depends-on [{:id authorization-id :fingerprint auth-fingerprint}
                    {:id invariant-id :fingerprint rules-fingerprint}]}]}))

(defn- proof-source! []
  {:transition (:transition (transition/build-guarded-replacement! construction))
   :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
   :goal [:guard]})

(defn describe-claim!
  "Validate and describe the original rules and exact dependencies. All
   universal invariant-preservation, callback and commit obligations remain
   open, even though the guarded abstract relation can be checked."
  []
  (let [auth (opaque/describe-contract authorization-contract)
        obligations (obligation/derive-obligations! obligation-source)
        graph (dependency-source!)
        graph-handle (dependency/check-graph! graph)
        graph-report (dependency/describe-graph graph-handle)
        built (transition/build-guarded-replacement! construction)]
    {:operation claim-operation
     :authorization-contract auth
     :source-declarations {:construction construction
                           :obligation-source obligation-source}
     :obligations obligations
     :dependency-graph graph
     :dependency-handle graph-handle
     :dependency-description graph-report
     :transition-fingerprint (:transition-fingerprint built)
     :open-obligations
     (into (into external-obligations (:open-obligations auth))
           (:open-obligation-ids obligations))
     :invariant-preservation :not-proved
     :one-winner-physical-enforcement :not-established
     :business-authorization-correctness :not-proved
     :evidence-class :source-relative-abstract-constraints-only
     :production-authority :none}))

(defn new-claim-environment!
  "Build one proof-linked, real v1251 plan executor around caller-supplied
   trusted callbacks. :authorization! is called once on the :submit path by
   the input callback, and its result is checked as an opaque decision.
   :read-current! can run multiple times; no cross-read atomicity is claimed.

   This is a TEST FIXTURE; its callable is not a proven authorization boundary
   and directly calling it outside a checked route remains a deployment gap.
   Returned :source and :executor can be reused by v1256 physical XTDB tests."
  [{:keys [request-id actor organization read-current! authorization!
           read-record! may-disclose?] :as options}]
  (when-not (= #{:request-id :actor :organization :read-current!
                :authorization! :read-record! :may-disclose?}
               (set (keys options)))
    (invalid! :wrong-environment-options {:actual (set (keys options))}))
  (when-not (and (integer? request-id) (pos? request-id)
                 (keyword? actor) (keyword? organization)
                 (every? fn? [read-current! authorization!
                              read-record! may-disclose?]))
    (invalid! :invalid-environment-options {}))
  (let [description (describe-claim!)
        graph (:dependency-graph description)
        graph-handle (:dependency-handle description)
        proof (proof-source!)
        certificate
        (certificate/admit-certificate!
         {:proof-kind :guarded-transition
          :proof-source proof
          :graph-handle graph-handle
          :current-graph graph
          :expected-open-premises #{conditional-premise}
          :expected-open-obligations conditional-obligations})
        config
        {:operation claim-operation
         :read-record! read-record!
         :may-disclose? may-disclose?
         :construct!
         (fn [ctx binding]
           (let [document (read-current! ctx binding)]
             {:now (or (:biff.fx/now ctx) (java.time.Instant/now))
              :plan {:commands [(claim-command! document actor)]
                     :emit false}
              :outcome {:status :claimed :request-id request-id}}))}
        source
        {:certified-source
         {:operation claim-operation
          :certificate certificate
          :proof-source proof
          :current-graph graph
          :resolved-configuration config}
         :construction construction
         :field-map field-map
         :command-operation command-operation
         :current-document! read-current!
         :transition-inputs!
         (fn [ctx binding]
           (let [document (read-current! ctx binding)
                 decision (authorization! ctx binding)]
             (inputs-for! document actor organization decision)))
         :target! (fn [_ctx _binding] [physical-table request-id])}
        executor (certified-plan/build-certified-plan-operation! source)]
    {:source source
     :executor executor
     :certificate certificate
     :proof-source proof
     :dependency-graph graph
     :dependency-handle graph-handle
     :declarations description
     :operation claim-operation
     :request-id request-id
     :actor actor
     :organization organization
     :open-obligations (into (:open-obligations description)
                             conditional-obligations)
     :physical-one-winner-proof :not-established
     :authorization-correctness :not-proved
     :deployment-adoption :not-established}))

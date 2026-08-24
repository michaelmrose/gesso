(ns gesso.choreo.proof
  "Small machine-checkable proof obligations for the current Gesso Choreo core.

   This namespace deliberately grows through narrow named properties instead of
   pretending that the current verifier already proves full projection
   refinement.

   The current structural properties are:

     :projection-boundary-preservation-v1
     :projection-successor-preservation-v1
     :projection-completion-preservation-v1
     :projection-observable-origin-preservation-v1
     :projection-runtime-origin-preservation-v1

   For one exact verified finite choreography, every *reachable* semantic
   boundary owned by a role must retain the boundary information that local
   execution needs after projection:

     :local
       owner, action, required keys, and closed output keys

     :authoritative
       owner, public semantic operation, required keys, and closed output keys

     :branch
       owner, selector key, and the complete set of concrete case values

     :await
       owner, environment-event set, and declared semantic event-data contracts;
       every declared authoritative-observation event additionally retains the
       authority, observation identity, basis key, and semantic field contract

     :communicate
       the sender projection retains the exact route/message contract and the
       receiver projection contains an exact receive alternative for that same
       route/message contract

   Raw successor state ids are intentionally NOT required to remain equal.
   Projection is allowed to skip foreign-local work and synthesize
   receive/completion states; preserving raw global successor ids would therefore
   be the wrong theorem.

   :projection-successor-preservation-v1 instead checks every emitted local
   transition against project/semantic-continuation for the corresponding semantic
   successor. That includes initial continuations, direct :next transitions,
   branch cases, communication sender/receiver continuations, and await-event
   continuations.

   :projection-completion-preservation-v1 checks the deliberately narrower
   terminal fact that projection can preserve: on every reachable control-flow
   path into a global :return, the nearest compiler-recorded continuation for
   every projected role is already canonical local completion. A role may
   legitimately complete before the global return itself, so a direct terminal
   continuation is not required. The authored global outcome is retained only as
   diagnostic context because role-local completion intentionally does not encode
   it.

   :projection-observable-origin-preservation-v1 checks the first converse
   safety fact required by projection refinement. Every emitted projected runtime
   boundary that can directly produce a distributed observation has exact authored
   semantic justification through CompilerProjection v3 reverse provenance:

     :authoritative
       must originate at one exact authored global :authoritative state owned by
       the same role with the same public operation and closed output-key contract;

     :receive
       every emitted receive alternative must originate from one or more exact
       authored global :communicate states addressed to that role with the same
       route and message contract. Intentional many-to-one receive collapse is
       retained explicitly rather than reconstructed heuristically.

   This is still a finite compiler-origin property, not the universal projected-step
   simulation theorem. In particular it does not by itself prove that a dynamically
   delivered envelope was emitted by the corresponding sender occurrence; that
   requires realization/transport execution invariants in addition to compiler
   origin preservation.

   :projection-runtime-origin-preservation-v1 extends that converse origin
   check across every emitted runtime state, including distributed-hidden and
   stuttering boundaries. Authored local/authoritative/branch/await/send states
   must match exact reachable global origins; synthetic receives must retain exact
   reachable communication origins; synthetic completion must retain a reachable
   semantic source and canonical role-local completion. This closes the static
   origin side of a later step-simulation relation but does not itself quantify
   over dynamic executions.

   check-projection-structure now emits ProjectionStructuralCertificate v3,
   composing all four finite structural results into one theorem-facing
   certificate. ProjectionStructuralCertificate v1 and v2 remain recognizable
   as historical artifacts; their meanings and closed shapes are not
   retroactively changed. The current certificate also binds
   the composition to the current verifier, ExecutablePlan, CompilerProjection,
   global semantics, and distributed-observation alphabet. It deliberately records
   dynamic projected-step simulation, terminal-outcome preservation,
   :trace-refinement, and :projection-refinement as nonclaims.

   These structural properties still do not prove trace/refinement preservation,
   knowledge derivations, authority authenticity, observation freshness, basis
   ordering, liveness, or browser realization. An authoritative-observation
   obligation proves only that the knowledge-producing observation contract
   required by the global choreography survives projection into executable data.

   Authoritative basis advancement is reported separately from proved structural
   obligations. For every authoritative-observation scope the result states:

     - a :runtime-enforced obligation requiring an explicit, exactly scoped
       :advances witness before an existing authoritative basis may move; and
     - a :trusted assumption that the truth of that authority-specific ordering
       judgment is supplied by the trusted authority/adapter rather than derived
       by this checker.

   These runtime/trusted entries are deliberately siblings of :obligations. They
   do not contribute to :valid? and are not counted as structural proof
   obligations. The checker therefore cannot accidentally upgrade runtime basis
   enforcement into a proof of freshness, ordering, or authority authenticity.

   Each structural checker remains exhaustive over its named finite property in
   the exact authored program/compiler output supplied to it. The result says
   exactly that and no more.

   Concrete behavioral witness checking lives separately in
   gesso.choreo.correspondence. This namespace owns only structural/compiler
   proof obligations so there is no second distributed execution engine hidden
   inside the proof layer.

   Proof/checker artifacts live on the compiler/test side. Nothing in this
   namespace is required by the portable production machine or ExecutablePlan
   runtime semantics. ExecutablePlan intentionally contains compact runtime
   locators only. Exact authored-semantic-state -> runtime-location provenance
   comes from project/CompilerProjection and remains compiler-only; this checker
   verifies the boundary at that exact recorded location instead of attempting
   to rediscover semantic identity by matching executable boundary shapes."
  (:require
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.choreo.semantics :as semantics]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def proof-version 2)

(def result-type
  :gesso.choreo.proof/result)

(def obligation-type
  :gesso.choreo.proof/obligation)

(def projection-boundary-property
  :projection-boundary-preservation-v1)

(def projection-successor-property
  :projection-successor-preservation-v1)

(def projection-completion-property
  :projection-completion-preservation-v1)

(def projection-observable-origin-property
  :projection-observable-origin-preservation-v1)

(def projection-runtime-origin-property
  :projection-runtime-origin-preservation-v1)

(def structural-properties
  #{projection-boundary-property
    projection-successor-property
    projection-completion-property
    projection-observable-origin-property
    projection-runtime-origin-property})

(def projection-structural-certificate-v1-properties
  #{projection-boundary-property
    projection-successor-property})

(def projection-structural-certificate-v2-properties
  #{projection-boundary-property
    projection-successor-property
    projection-completion-property})

(def projection-structural-certificate-properties
  #{projection-boundary-property
    projection-successor-property
    projection-completion-property
    projection-observable-origin-property})

(def result-classification
  :exhaustive-finite-structural-check)

(def successor-result-classification
  :exhaustive-finite-successor-check)

(def completion-result-classification
  :exhaustive-finite-completion-check)

(def observable-origin-result-classification
  :exhaustive-finite-observable-origin-check)

(def runtime-origin-result-classification
  :exhaustive-finite-runtime-origin-check)

(def projection-structural-certificate-type
  :gesso.choreo.proof/projection-structural-certificate)

(def projection-structural-certificate-v1-version
  1)

(def projection-structural-certificate-v2-version
  2)

(def projection-structural-certificate-version
  3)

(def projection-structural-certificate-classification
  :composed-finite-structural-certificate)

(def projection-structural-certificate-v1-nonclaims
  #{:trace-refinement
    :projection-refinement})

(def projection-structural-certificate-v2-nonclaims
  #{:terminal-outcome-preservation
    :trace-refinement
    :projection-refinement})

(def projection-refinement-nonclaims
  #{:projected-step-simulation
    :terminal-outcome-preservation
    :trace-refinement
    :projection-refinement})

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- proof-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.proof/error
      :error/kind kind}
     data))))


(def checker-option-keys
  #{:verification-options})

(defn- ensure-checker-options!
  [options]
  (when (and (some? options)
             (not (map? options)))
    (proof-error
     :invalid-options
     "Proof checker options must be a map."
     {:options options}))

  (let [options'
        (or options {})

        unknown
        (set/difference
         (set (keys options'))
         checker-option-keys)]

    (when (seq unknown)
      (proof-error
       :unknown-option-keys
       "Proof checker options contain unsupported keys."
       {:unknown-option-keys unknown
        :allowed-option-keys checker-option-keys}))

    options'))

;; -----------------------------------------------------------------------------
;; Normalized boundary descriptions
;; -----------------------------------------------------------------------------

(defn- action-contract
  [state]
  {:requires
   (or (:requires state) #{})

   :outputs
   (or (:outputs state) #{})})

(defn- communication-contract
  [state]
  {:required
   (or (:required state) #{})

   :optional
   (or (:optional state) #{})

   :correlation
   (or (:correlation state) #{})

   :open-payload?
   (true? (:open-payload? state))})

(defn- communication-route
  [state]
  (cond->
   {:from (:from state)
    :to (:to state)
    :event (:event state)}

    (contains? state :via)
    (assoc :via (:via state))))

(defn- sender-boundary
  [state]
  (merge
   {:op :send
    :to (:to state)
    :event (:event state)}
   (when (contains? state :via)
     {:via (:via state)})
   (communication-contract state)))

(defn- projected-sender-boundary
  [state]
  (when state
    (merge
     {:op (:op state)
      :to (:to state)
      :event (:event state)}
     (when (contains? state :via)
       {:via (:via state)})
     (communication-contract state))))

(defn- receive-alternative-boundary
  [receiver-role alternative]
  (merge
   {:from (:from alternative)
    :to receiver-role
    :event (:event alternative)}
   (when (contains? alternative :via)
     {:via (:via alternative)})
   (communication-contract alternative)))

(defn- projected-owner-boundary
  [global-state projected-state]
  (when projected-state
    (case (:op global-state)
      :local
      (merge
       {:op (:op projected-state)
        :action (:action projected-state)}
       (action-contract projected-state))

      :authoritative
      (merge
       {:op (:op projected-state)
        :operation (:operation projected-state)}
       (action-contract projected-state))

      :branch
      {:op (:op projected-state)
       :on (:on projected-state)
       :case-values
       (set
        (keys
         (:cases projected-state)))}

      :await
      {:op (:op projected-state)
       :events
       (set
        (keys
         (:events projected-state)))
       :event-contracts
       (or (:event-contracts projected-state) {})}

      nil)))

(defn- expected-owner-boundary
  [global-state]
  (case (:op global-state)
    :local
    (merge
     {:op :local
      :action (:action global-state)}
     (action-contract global-state))

    :authoritative
    (merge
     {:op :authoritative
      :operation (:operation global-state)}
     (action-contract global-state))

    :branch
    {:op :branch
     :on (:on global-state)
     :case-values
     (set
      (keys
       (:cases global-state)))}

    :await
    {:op :await
     :events
     (set
      (keys
       (:events global-state)))
     :event-contracts
     (or (:event-contracts global-state) {})}

    nil))


;; -----------------------------------------------------------------------------
;; Authoritative-observation boundary descriptions
;; -----------------------------------------------------------------------------

(defn- semantic-event-keys
  [event-contract]
  (set/union
   (or (:required event-contract) #{})
   (or (:optional event-contract) #{})))

(defn- authoritative-observation-boundary
  [event event-contract]
  (when-let [observation
             (:authoritative-observation event-contract)]
    {:event event
     :authority (:authority observation)
     :observation (:observation observation)
     :basis-key (:basis-key observation)
     :required (or (:required event-contract) #{})
     :optional (or (:optional event-contract) #{})
     :semantic-keys (semantic-event-keys event-contract)
     :open-data? (true? (:open-data? event-contract))}))

(defn- authoritative-observation-events
  [state]
  (->> (:event-contracts state)
       (keep
        (fn [[event event-contract]]
          (when (:authoritative-observation event-contract)
            event)))
       (sort-by pr-str)
       vec))

;; -----------------------------------------------------------------------------
;; Obligation construction
;; -----------------------------------------------------------------------------

(defn- obligation
  [{:keys [id property state role endpoint expected actual valid?]
    :as data}]
  (merge
   {:gesso.choreo.proof/type obligation-type
    :id id
    :property property
    :state state
    :role role
    :expected expected
    :actual actual
    :valid? (true? valid?)}
   (when (some? endpoint)
     {:endpoint endpoint})
   (dissoc data
           :id
           :property
           :state
           :role
           :endpoint
           :expected
           :actual
           :valid?)))

(defn- executable-plan
  [compiled]
  (:executable-plan
   (project/ensure-compiler-projection compiled)))

(defn- semantic-endpoint-locations
  [compiled state-id endpoint]
  (if-not compiled
    []
    (->> (project/semantic-locations compiled state-id)
         (filterv #(= endpoint (:endpoint %))))))

(defn- owner-locations
  [compiled state-id global-state]
  (if-not compiled
    []
    (let [plan (executable-plan compiled)]
      (mapv
       (fn [{:keys [runtime-locator] :as location}]
         (let [projected-state (get-in plan [:states runtime-locator])]
           (assoc location
                  :boundary
                  (projected-owner-boundary global-state projected-state))))
       (semantic-endpoint-locations compiled state-id :owner)))))

(defn- owner-obligation
  [compiled-by-role state-id global-state]
  (let [role (:role global-state)
        compiled (get compiled-by-role role)
        expected (expected-owner-boundary global-state)
        locations (owner-locations compiled state-id global-state)
        valid? (and (seq locations)
                    (every? #(= expected (:boundary %)) locations))
        actual (:boundary (first locations))]
    (obligation
     {:id [:projection-boundary state-id :owner]
      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :owner
      :expected expected
      :actual actual
      :runtime-locator (:runtime-locator (first locations))
      :valid? valid?
      :reason
      (cond
        (nil? compiled) :missing-role-plan
        (empty? locations) :missing-owner-boundary
        valid? :preserved
        :else :owner-boundary-mismatch)})))

(defn- sender-locations
  [compiled state-id]
  (if-not compiled
    []
    (let [plan (executable-plan compiled)]
      (mapv
       (fn [{:keys [runtime-locator] :as location}]
         (assoc location
                :boundary
                (projected-sender-boundary
                 (get-in plan [:states runtime-locator]))))
       (semantic-endpoint-locations compiled state-id :sender)))))

(defn- communication-sender-obligation
  [compiled-by-role state-id global-state]
  (let [role (:from global-state)
        compiled (get compiled-by-role role)
        expected (sender-boundary global-state)
        locations (sender-locations compiled state-id)
        valid? (and (seq locations)
                    (every? #(= expected (:boundary %)) locations))
        actual (:boundary (first locations))]
    (obligation
     {:id [:projection-boundary state-id :sender]
      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :sender
      :expected expected
      :actual actual
      :runtime-locator (:runtime-locator (first locations))
      :valid? valid?
      :reason
      (cond
        (nil? compiled) :missing-role-plan
        (empty? locations) :missing-sender-boundary
        valid? :preserved
        :else :sender-boundary-mismatch)})))

(defn- receiver-locations
  [compiled state-id receiver-role]
  (if-not compiled
    []
    (let [plan (executable-plan compiled)]
      (mapv
       (fn [{:keys [runtime-locator alternative-index] :as location}]
         (let [alternative
               (get-in plan
                       [:states runtime-locator :alternatives alternative-index])]
           (assoc location
                  ;; Retained in proof output for diagnostic compatibility. The
                  ;; executable state identity is now the compact runtime locator.
                  :projected-state runtime-locator
                  :alternative
                  (receive-alternative-boundary receiver-role alternative))))
       (semantic-endpoint-locations compiled state-id :receiver)))))

(defn- communication-receiver-obligation
  [compiled-by-role state-id global-state]
  (let [role (:to global-state)
        compiled (get compiled-by-role role)
        expected (merge (communication-route global-state)
                        (communication-contract global-state))
        locations (receiver-locations compiled state-id role)
        valid? (and (seq locations)
                    (every? #(= expected (:alternative %)) locations))]
    (obligation
     {:id [:projection-boundary state-id :receiver]
      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :receiver
      :expected expected
      :actual {:matching-alternatives locations}
      :valid? valid?
      :reason
      (cond
        (nil? compiled) :missing-role-plan
        (empty? locations) :missing-receive-alternative
        valid? :preserved
        :else :receive-alternative-mismatch)})))

(defn- authoritative-observation-obligation
  [compiled-by-role state-id global-state event]
  (let [role (:role global-state)
        compiled (get compiled-by-role role)
        owner-locations' (owner-locations compiled state-id global-state)
        owner-location (first owner-locations')
        runtime-locator (:runtime-locator owner-location)
        plan (when compiled (executable-plan compiled))
        projected-state (when (and plan (some? runtime-locator))
                          (get-in plan [:states runtime-locator]))
        expected (authoritative-observation-boundary
                  event
                  (get-in global-state [:event-contracts event]))
        actual (when projected-state
                 (authoritative-observation-boundary
                  event
                  (get-in projected-state [:event-contracts event])))
        valid? (and (seq owner-locations')
                    (every?
                     (fn [{:keys [runtime-locator]}]
                       (= expected
                          (authoritative-observation-boundary
                           event
                           (get-in plan
                                   [:states runtime-locator
                                    :event-contracts event]))))
                     owner-locations'))]
    (obligation
     {:id [:projection-boundary state-id :authoritative-observation event]
      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :authoritative-observation
      :expected expected
      :actual actual
      :runtime-locator runtime-locator
      :valid? valid?
      :reason
      (cond
        (nil? compiled) :missing-role-plan
        (empty? owner-locations') :missing-owner-boundary
        valid? :preserved
        :else :missing-authoritative-observation-boundary)})))

(defn- authoritative-observation-obligations
  [compiled-by-role state-id global-state]
  (mapv
   (fn [event]
     (authoritative-observation-obligation
      compiled-by-role state-id global-state event))
   (authoritative-observation-events global-state)))

(defn- state-obligations
  [compiled-by-role state-id global-state]
  (case (:op global-state)
    :local
    [(owner-obligation compiled-by-role state-id global-state)]

    :authoritative
    [(owner-obligation compiled-by-role state-id global-state)]

    :branch
    [(owner-obligation compiled-by-role state-id global-state)]

    :communicate
    [(communication-sender-obligation compiled-by-role state-id global-state)
     (communication-receiver-obligation compiled-by-role state-id global-state)]

    :await
    (into
     [(owner-obligation compiled-by-role state-id global-state)]
     (authoritative-observation-obligations
      compiled-by-role state-id global-state))

    :return
    []

    []))

(defn- projection-boundary-obligations
  [verified compiled-by-role]
  (let [choreography (:choreography verified)
        states (:states choreography)
        reachable (get-in verified
                          [:verification :analysis :reachable-state-ids])]
    (->> reachable
         (sort-by pr-str)
         (mapcat
          (fn [state-id]
            (state-obligations
             compiled-by-role
             state-id
             (get states state-id))))
         vec)))


;; -----------------------------------------------------------------------------
;; Exact projected-successor preservation
;; -----------------------------------------------------------------------------

(defn- continuation-locator
  [compiled semantic-state]
  (when compiled
    (project/semantic-continuation
     compiled
     semantic-state)))

(defn- source-runtime-locators
  [locations]
  (mapv :runtime-locator locations))

(defn- projected-next-targets
  [compiled locations]
  (if-not compiled
    []
    (let [plan (executable-plan compiled)]
      (mapv
       (fn [{:keys [runtime-locator]}]
         (get-in plan
                 [:states runtime-locator :next]))
       locations))))

(defn- direct-successor-obligation
  [compiled-by-role
   state-id
   role
   endpoint
   semantic-target
   locations]
  (let [compiled
        (get compiled-by-role role)

        expected-runtime-target
        (continuation-locator
         compiled
         semantic-target)

        actual-runtime-targets
        (projected-next-targets
         compiled
         locations)

        valid?
        (and
         compiled
         (seq locations)
         (some? expected-runtime-target)
         (seq actual-runtime-targets)
         (every?
          #(= expected-runtime-target %)
          actual-runtime-targets))]

    (obligation
     {:id
      [:projection-successor state-id endpoint]

      :property
      projection-successor-property

      :state
      state-id

      :role
      role

      :endpoint
      endpoint

      :expected
      {:semantic-target semantic-target
       :runtime-target expected-runtime-target}

      :actual
      {:source-runtime-locators
       (source-runtime-locators locations)

       :runtime-targets
       actual-runtime-targets}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (empty? locations)
        :missing-source-boundary

        (nil? expected-runtime-target)
        :missing-semantic-continuation

        (empty? actual-runtime-targets)
        :missing-runtime-successor

        valid?
        :preserved

        :else
        :successor-mismatch)})))

(defn- owner-direct-successor-obligation
  [compiled-by-role state-id global-state]
  (let [role
        (:role global-state)

        compiled
        (get compiled-by-role role)

        locations
        (owner-locations
         compiled
         state-id
         global-state)]

    (direct-successor-obligation
     compiled-by-role
     state-id
     role
     :owner
     (:next global-state)
     locations)))

(defn- communication-sender-successor-obligation
  [compiled-by-role state-id global-state]
  (let [role
        (:from global-state)

        compiled
        (get compiled-by-role role)

        locations
        (sender-locations
         compiled
         state-id)]

    (direct-successor-obligation
     compiled-by-role
     state-id
     role
     :sender
     (:next global-state)
     locations)))

(defn- receiver-runtime-targets
  [compiled locations]
  (if-not compiled
    []
    (let [plan
          (executable-plan compiled)]

      (mapv
       (fn [{:keys [runtime-locator
                    alternative-index]}]
         (get-in
          plan
          [:states
           runtime-locator
           :alternatives
           alternative-index
           :next]))
       locations))))

(defn- communication-receiver-successor-obligation
  [compiled-by-role state-id global-state]
  (let [role
        (:to global-state)

        compiled
        (get compiled-by-role role)

        locations
        (receiver-locations
         compiled
         state-id
         role)

        semantic-target
        (:next global-state)

        expected-runtime-target
        (continuation-locator
         compiled
         semantic-target)

        actual-runtime-targets
        (receiver-runtime-targets
         compiled
         locations)

        valid?
        (and
         compiled
         (seq locations)
         (some? expected-runtime-target)
         (seq actual-runtime-targets)
         (every?
          #(= expected-runtime-target %)
          actual-runtime-targets))]

    (obligation
     {:id
      [:projection-successor state-id :receiver]

      :property
      projection-successor-property

      :state
      state-id

      :role
      role

      :endpoint
      :receiver

      :expected
      {:semantic-target semantic-target
       :runtime-target expected-runtime-target}

      :actual
      {:source-locations
       (mapv
        #(select-keys
          %
          [:runtime-locator
           :alternative-index])
        locations)

       :runtime-targets
       actual-runtime-targets}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (empty? locations)
        :missing-receive-alternative

        (nil? expected-runtime-target)
        :missing-semantic-continuation

        (empty? actual-runtime-targets)
        :missing-runtime-successor

        valid?
        :preserved

        :else
        :successor-mismatch)})))

(defn- projected-case-targets
  [compiled locations]
  (if-not compiled
    []
    (let [plan
          (executable-plan compiled)]

      (mapv
       (fn [{:keys [runtime-locator]}]
         {:runtime-locator
          runtime-locator

          :targets
          (get-in
           plan
           [:states runtime-locator :cases])})
       locations))))

(defn- branch-successor-obligation
  [compiled-by-role state-id global-state]
  (let [role
        (:role global-state)

        compiled
        (get compiled-by-role role)

        locations
        (owner-locations
         compiled
         state-id
         global-state)

        expected-targets
        (into {}
              (map
               (fn [[value semantic-target]]
                 [value
                  {:semantic-target
                   semantic-target

                   :runtime-target
                   (continuation-locator
                    compiled
                    semantic-target)}]))
              (:cases global-state))

        expected-runtime-cases
        (into {}
              (map
               (fn [[value
                     {:keys [runtime-target]}]]
                 [value runtime-target]))
              expected-targets)

        actual
        (projected-case-targets
         compiled
         locations)

        valid?
        (and
         compiled
         (seq locations)
         (every?
          some?
          (vals expected-runtime-cases))
         (every?
          #(= expected-runtime-cases
              (:targets %))
          actual))]

    (obligation
     {:id
      [:projection-successor state-id :owner]

      :property
      projection-successor-property

      :state
      state-id

      :role
      role

      :endpoint
      :owner

      :expected
      {:cases expected-targets}

      :actual
      {:projected-cases actual}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (empty? locations)
        :missing-owner-boundary

        (some nil?
              (vals expected-runtime-cases))
        :missing-semantic-continuation

        valid?
        :preserved

        :else
        :successor-mismatch)})))

(defn- projected-event-targets
  [compiled locations]
  (if-not compiled
    []
    (let [plan
          (executable-plan compiled)]

      (mapv
       (fn [{:keys [runtime-locator]}]
         {:runtime-locator
          runtime-locator

          :targets
          (get-in
           plan
           [:states runtime-locator :events])})
       locations))))

(defn- await-successor-obligation
  [compiled-by-role state-id global-state]
  (let [role
        (:role global-state)

        compiled
        (get compiled-by-role role)

        locations
        (owner-locations
         compiled
         state-id
         global-state)

        expected-targets
        (into {}
              (map
               (fn [[event semantic-target]]
                 [event
                  {:semantic-target
                   semantic-target

                   :runtime-target
                   (continuation-locator
                    compiled
                    semantic-target)}]))
              (:events global-state))

        expected-runtime-events
        (into {}
              (map
               (fn [[event
                     {:keys [runtime-target]}]]
                 [event runtime-target]))
              expected-targets)

        actual
        (projected-event-targets
         compiled
         locations)

        valid?
        (and
         compiled
         (seq locations)
         (every?
          some?
          (vals expected-runtime-events))
         (every?
          #(= expected-runtime-events
              (:targets %))
          actual))]

    (obligation
     {:id
      [:projection-successor state-id :owner]

      :property
      projection-successor-property

      :state
      state-id

      :role
      role

      :endpoint
      :owner

      :expected
      {:events expected-targets}

      :actual
      {:projected-events actual}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (empty? locations)
        :missing-owner-boundary

        (some nil?
              (vals expected-runtime-events))
        :missing-semantic-continuation

        valid?
        :preserved

        :else
        :successor-mismatch)})))

(defn- state-successor-obligations
  [compiled-by-role state-id global-state]
  (case (:op global-state)
    :local
    [(owner-direct-successor-obligation
      compiled-by-role
      state-id
      global-state)]

    :authoritative
    [(owner-direct-successor-obligation
      compiled-by-role
      state-id
      global-state)]

    :branch
    [(branch-successor-obligation
      compiled-by-role
      state-id
      global-state)]

    :communicate
    [(communication-sender-successor-obligation
      compiled-by-role
      state-id
      global-state)

     (communication-receiver-successor-obligation
      compiled-by-role
      state-id
      global-state)]

    :await
    [(await-successor-obligation
      compiled-by-role
      state-id
      global-state)]

    :return
    []

    []))

(defn- initial-successor-obligation
  [compiled-by-role initial-state role]
  (let [compiled
        (get compiled-by-role role)

        plan
        (when compiled
          (executable-plan compiled))

        expected-runtime-target
        (continuation-locator
         compiled
         initial-state)

        actual-runtime-target
        (:initial plan)

        valid?
        (and
         compiled
         (some? expected-runtime-target)
         (= expected-runtime-target
            actual-runtime-target))]

    (obligation
     {:id
      [:projection-successor :initial role]

      :property
      projection-successor-property

      :state
      initial-state

      :role
      role

      :endpoint
      :initial

      :expected
      {:semantic-target initial-state
       :runtime-target expected-runtime-target}

      :actual
      {:runtime-target actual-runtime-target}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (nil? expected-runtime-target)
        :missing-semantic-continuation

        valid?
        :preserved

        :else
        :successor-mismatch)})))

(defn- projection-successor-obligations
  [verified compiled-by-role]
  (let [choreography
        (:choreography verified)

        states
        (:states choreography)

        initial
        (:initial choreography)

        roles
        (get-in verified
                [:verification :analysis :roles])

        reachable
        (get-in verified
                [:verification :analysis :reachable-state-ids])]

    (vec
     (concat
      (map
       #(initial-successor-obligation
         compiled-by-role
         initial
         %)
       (sort-by pr-str roles))

      (mapcat
       (fn [state-id]
         (state-successor-obligations
          compiled-by-role
          state-id
          (get states state-id)))
       (sort-by pr-str reachable))))))

;; -----------------------------------------------------------------------------
;; Exact projected-completion preservation
;; -----------------------------------------------------------------------------

(defn- reachable-return-state-ids
  [verified]
  (let [states
        (get-in verified [:choreography :states])

        reachable
        (get-in verified
                [:verification :analysis :reachable-state-ids])]
    (->> reachable
         (filter
          (fn [state-id]
            (= :return
               (:op (get states state-id)))))
         (sort-by pr-str)
         vec)))

(defn- projected-completion-state
  [compiled runtime-locator]
  (when (and compiled
             (some? runtime-locator))
    (get-in (executable-plan compiled)
            [:states runtime-locator])))

(defn- completion-frontier
  [verified compiled terminal-state-id]
  (let [predecessors
        (get-in verified
                [:verification :analysis :predecessors])

        reachable
        (set
         (get-in verified
                 [:verification :analysis :reachable-state-ids]))]

    (loop [pending
           [terminal-state-id]

           seen
           #{}

           frontier
           []

           uncovered
           []]

      (if-let [state-id (first pending)]
        (if (contains? seen state-id)
          (recur
           (subvec pending 1)
           seen
           frontier
           uncovered)

          (let [seen'
                (conj seen state-id)

                runtime-locator
                (continuation-locator
                 compiled
                 state-id)]

            (if (some? runtime-locator)
              (recur
               (subvec pending 1)
               seen'
               (conj
                frontier
                {:semantic-state state-id
                 :runtime-locator runtime-locator
                 :local-state
                 (projected-completion-state
                  compiled
                  runtime-locator)})
               uncovered)

              (let [parents
                    (->> (get predecessors state-id #{})
                         (filter reachable)
                         (remove seen')
                         (sort-by pr-str)
                         vec)]

                (if (seq parents)
                  (recur
                   (into (subvec pending 1) parents)
                   seen'
                   frontier
                   uncovered)

                  (recur
                   (subvec pending 1)
                   seen'
                   frontier
                   (conj uncovered state-id)))))))

        {:frontier
         (->> frontier
              (sort-by
               (juxt
                (comp pr-str :semantic-state)
                :runtime-locator))
              vec)

         :uncovered-entry-states
         (->> uncovered
              (sort-by pr-str)
              vec)}))))

(defn- completion-obligation
  [verified compiled-by-role states state-id role]
  (let [compiled
        (get compiled-by-role role)

        global-state
        (get states state-id)

        {:keys [frontier uncovered-entry-states]}
        (if compiled
          (completion-frontier
           verified
           compiled
           state-id)
          {:frontier []
           :uncovered-entry-states []})

        expected-local-completion
        {:op :return
         :outcome project/complete-outcome}

        valid-frontier?
        (and
         (seq frontier)
         (every?
          #(= expected-local-completion
              (:local-state %))
          frontier))

        valid?
        (and
         compiled
         (empty? uncovered-entry-states)
         valid-frontier?)]

    (obligation
     {:id
      [:projection-completion state-id role]

      :property
      projection-completion-property

      :state
      state-id

      :role
      role

      :endpoint
      :completion

      :expected
      {:global-outcome
       (:outcome global-state)

       :local-completion
       expected-local-completion

       :coverage
       :every-reachable-control-flow-path-to-terminal}

      :actual
      {:frontier
       frontier

       :uncovered-entry-states
       uncovered-entry-states}

      :valid?
      valid?

      :reason
      (cond
        (nil? compiled)
        :missing-role-plan

        (seq uncovered-entry-states)
        :uncovered-terminal-path

        (empty? frontier)
        :missing-completion-frontier

        valid-frontier?
        :preserved

        :else
        :frontier-not-role-local-completion)})))

(defn- projection-completion-obligations
  [verified compiled-by-role]
  (let [states
        (get-in verified [:choreography :states])

        roles
        (->> (keys compiled-by-role)
             (sort-by pr-str)
             vec)]
    (->> (reachable-return-state-ids verified)
         (mapcat
          (fn [state-id]
            (map
             (fn [role]
               (completion-obligation
                verified
                compiled-by-role
                states
                state-id
                role))
             roles)))
         vec)))

;; -----------------------------------------------------------------------------
;; Runtime obligations and trusted assumptions
;; -----------------------------------------------------------------------------

(def authoritative-basis-progression-property
  :authoritative-basis-progression)

(def authoritative-basis-ordering-property
  :authoritative-basis-ordering)

(defn- authoritative-observation-scopes
  [verified]
  (let [choreography (:choreography verified)
        states (:states choreography)
        reachable (get-in verified
                          [:verification
                           :analysis
                           :reachable-state-ids])]
    (->> reachable
         (mapcat
          (fn [state-id]
            (let [state (get states state-id)]
              (when (= :await (:op state))
                (map
                 (fn [event]
                   (let [descriptor
                         (get-in state
                                 [:event-contracts
                                  event
                                  :authoritative-observation])]
                     {:state state-id
                      :role (:role state)
                      :event event
                      :authority (:authority descriptor)
                      :observation (:observation descriptor)
                      :basis-key (:basis-key descriptor)}))
                 (authoritative-observation-events state))))))
         (sort-by
          (fn [{:keys [authority observation state event]}]
            [(pr-str authority)
             (pr-str observation)
             (pr-str state)
             (pr-str event)]))
         vec)))

(defn- progression-runtime-obligation
  [{:keys [state role event authority observation basis-key]}]
  {:id
   [:authoritative-basis-progression
    state
    event]

   :property authoritative-basis-progression-property
   :classification :runtime-enforced
   :state state
   :role role
   :event event
   :authority authority
   :observation observation
   :basis-key basis-key
   :when :advancing-distinct-existing-authoritative-basis
   :requires
   #{:explicit-progression-witness
     :exact-authority-match
     :exact-observation-match
     :exact-from-basis-match
     :exact-to-basis-match
     :advances-relation}})

(defn- progression-trusted-assumption
  [{:keys [state role event authority observation basis-key]}]
  {:id
   [:authoritative-basis-ordering
    state
    event]

   :property authoritative-basis-ordering-property
   :classification :trusted
   :state state
   :role role
   :event event
   :authority authority
   :observation observation
   :basis-key basis-key
   :assumption
   :advances-witness-truth-is-supplied-by-trusted-authority})

(defn- authoritative-basis-runtime-obligations
  [verified]
  (mapv progression-runtime-obligation
        (authoritative-observation-scopes verified)))

(defn- authoritative-basis-trusted-assumptions
  [verified]
  (mapv progression-trusted-assumption
        (authoritative-observation-scopes verified)))

;; -----------------------------------------------------------------------------
;; Converse observable-origin obligations
;; -----------------------------------------------------------------------------

(def projected-observable-runtime-ops
  "Projected runtime operations that can directly create one canonical
   distributed observation in the current independent-realization semantics.

   :authoritative becomes observable when the trusted authoritative boundary
   completes. :receive becomes observable when one genuinely emitted participant
   message is consumed. :send remains physical/distributed-hidden until receiver
   consumption; local/environment/branch/return boundaries do not themselves
   create projected distributed observations."
  #{:authoritative
    :receive})

(defn- authoritative-origin-obligation
  [states reachable role runtime-locator runtime-state origin]
  (let [semantic-state
        (:semantic-state origin)

        global-state
        (get states semantic-state)

        expected
        (when global-state
          {:kind :authored-authoritative
           :semantic-state semantic-state
           :role (:role global-state)
           :operation (:operation global-state)
           :outputs (or (:outputs global-state) #{})})

        actual
        {:kind (:kind origin)
         :semantic-state semantic-state
         :role role
         :operation (:operation runtime-state)
         :outputs (or (:outputs runtime-state) #{})}

        valid?
        (and
         (= :authored-boundary (:kind origin))
         (map? global-state)
         (contains? reachable semantic-state)
         (= :authoritative (:op global-state))
         (= role (:role global-state))
         (= expected
            (assoc actual :kind :authored-authoritative)))]

    (obligation
     {:id
      [:projection-observable-origin role runtime-locator :authoritative]

      :property
      projection-observable-origin-property

      :state
      semantic-state

      :role
      role

      :endpoint
      :authoritative

      :runtime-locator
      runtime-locator

      :expected
      expected

      :actual
      actual

      :valid?
      valid?

      :reason
      (cond
        (not= :authored-boundary (:kind origin))
        :observable-runtime-origin-kind-mismatch

        (nil? global-state)
        :missing-authored-semantic-state

        (not (contains? reachable semantic-state))
        :unreachable-authored-semantic-state

        (not= :authoritative (:op global-state))
        :authored-semantic-op-mismatch

        (not= role (:role global-state))
        :authored-semantic-role-mismatch

        valid?
        :authored-observable-origin-preserved

        :else
        :authored-authoritative-contract-mismatch)})))

(defn- receive-origin-obligation
  [states reachable role runtime-locator runtime-state origin alternative-index semantic-state]
  (let [global-state
        (get states semantic-state)

        alternative
        (get (:alternatives runtime-state)
             alternative-index)

        expected
        (when global-state
          (merge
           {:kind :authored-communication
            :semantic-state semantic-state
            :from (:from global-state)
            :to (:to global-state)
            :event (:event global-state)}
           (when (contains? global-state :via)
             {:via (:via global-state)})
           (communication-contract global-state)))

        actual
        (when alternative
          (merge
           {:kind :authored-communication
            :semantic-state semantic-state
            :from (:from alternative)
            :to role
            :event (:event alternative)}
           (when (contains? alternative :via)
             {:via (:via alternative)})
           (communication-contract alternative)))

        valid?
        (and
         (= :synthetic-receive (:kind origin))
         (map? global-state)
         (contains? reachable semantic-state)
         (= :communicate (:op global-state))
         (= role (:to global-state))
         (some? alternative)
         (= expected actual))]

    (obligation
     {:id
      [:projection-observable-origin
       role
       runtime-locator
       :receive
       alternative-index
       semantic-state]

      :property
      projection-observable-origin-property

      :state
      semantic-state

      :role
      role

      :endpoint
      :receiver

      :runtime-locator
      runtime-locator

      :alternative-index
      alternative-index

      :expected
      expected

      :actual
      actual

      :valid?
      valid?

      :reason
      (cond
        (not= :synthetic-receive (:kind origin))
        :observable-runtime-origin-kind-mismatch

        (nil? global-state)
        :missing-authored-semantic-state

        (not (contains? reachable semantic-state))
        :unreachable-authored-semantic-state

        (not= :communicate (:op global-state))
        :authored-semantic-op-mismatch

        (not= role (:to global-state))
        :authored-semantic-role-mismatch

        (nil? alternative)
        :missing-receive-alternative

        valid?
        :authored-observable-origin-preserved

        :else
        :authored-communication-contract-mismatch)})))

(defn- receive-origin-obligations
  [states reachable role runtime-locator runtime-state origin]
  (if-not (= :synthetic-receive (:kind origin))
    [(obligation
      {:id
       [:projection-observable-origin role runtime-locator :receive]
       :property projection-observable-origin-property
       :state (:source-semantic-state origin)
       :role role
       :endpoint :receiver
       :runtime-locator runtime-locator
       :expected {:origin-kind :synthetic-receive}
       :actual {:origin-kind (:kind origin)}
       :valid? false
       :reason :observable-runtime-origin-kind-mismatch})]

    (let [alternative-sources
          (:alternative-semantic-states origin)

          alternatives
          (:alternatives runtime-state)]
      (if-not (= (count alternatives)
                 (count alternative-sources))
        [(obligation
          {:id
           [:projection-observable-origin role runtime-locator :receive]
           :property projection-observable-origin-property
           :state (:source-semantic-state origin)
           :role role
           :endpoint :receiver
           :runtime-locator runtime-locator
           :expected {:alternative-count (count alternatives)}
           :actual {:origin-alternative-count (count alternative-sources)}
           :valid? false
           :reason :receive-origin-alternative-count-mismatch})]

        (vec
         (mapcat
          (fn [alternative-index semantic-states]
            (map
             (fn [semantic-state]
               (receive-origin-obligation
                states
                reachable
                role
                runtime-locator
                runtime-state
                origin
                alternative-index
                semantic-state))
             semantic-states))
          (range (count alternative-sources))
          alternative-sources))))))

(defn- role-observable-origin-obligations
  [states reachable role compiled]
  (let [plan
        (executable-plan compiled)]
    (vec
     (mapcat
      (fn [[runtime-locator runtime-state]]
        (let [origin
              (project/runtime-origin compiled runtime-locator)]
          (case (:op runtime-state)
            :authoritative
            [(authoritative-origin-obligation
              states
              reachable
              role
              runtime-locator
              runtime-state
              origin)]

            :receive
            (receive-origin-obligations
             states
             reachable
             role
             runtime-locator
             runtime-state
             origin)

            ;; Every other current runtime op is outside the projected
            ;; distributed-observable alphabet and therefore creates no
            ;; observable-origin obligation here.
            [])))
      (sort-by key (:states plan))))))

(defn- projection-observable-origin-obligations
  [verified compiled-by-role]
  (let [states
        (get-in verified [:choreography :states])

        roles
        (get-in verified [:verification :analysis :roles])

        reachable
        (get-in verified [:verification :analysis :reachable-state-ids])]
    (vec
     (mapcat
      (fn [role]
        (if-let [compiled (get compiled-by-role role)]
          (role-observable-origin-obligations
           states
           reachable
           role
           compiled)
          [(obligation
            {:id [:projection-observable-origin role :missing-plan]
             :property projection-observable-origin-property
             :state nil
             :role role
             :endpoint :observable-origin
             :expected {:role-plan true}
             :actual {:role-plan false}
             :valid? false
             :reason :missing-role-plan})]))
      (sort-by pr-str roles)))))


;; -----------------------------------------------------------------------------
;; Converse runtime-origin obligations
;; -----------------------------------------------------------------------------

(def projected-runtime-ops
  "All runtime operations emitted by the current portable projector.

   This set is proof-facing vocabulary, not an executable-plan schema. It is
   deliberately exhaustive so a newly emitted runtime op cannot silently escape
   converse semantic-origin checking."
  #{:local
    :authoritative
    :branch
    :await
    :send
    :receive
    :return})

(defn- authored-runtime-owner
  [global-state]
  (case (:op global-state)
    :communicate (:from global-state)
    :return nil
    (:role global-state)))

(defn- expected-authored-runtime-boundary
  [global-state]
  (case (:op global-state)
    (:local :authoritative :branch :await)
    (expected-owner-boundary global-state)

    :communicate
    (sender-boundary global-state)

    nil))

(defn- actual-authored-runtime-boundary
  [global-state runtime-state]
  (case (:op global-state)
    (:local :authoritative :branch :await)
    (projected-owner-boundary
     global-state
     runtime-state)

    :communicate
    (projected-sender-boundary
     runtime-state)

    nil))

(defn- authored-runtime-origin-obligation
  [states reachable role runtime-locator runtime-state origin]
  (let [semantic-state
        (:semantic-state origin)

        global-state
        (get states semantic-state)

        expected-owner
        (when global-state
          (authored-runtime-owner global-state))

        expected-boundary
        (when global-state
          (expected-authored-runtime-boundary
           global-state))

        actual-boundary
        (when global-state
          (actual-authored-runtime-boundary
           global-state
           runtime-state))

        valid?
        (and
         (= :authored-boundary
            (:kind origin))
         (map? global-state)
         (contains? reachable semantic-state)
         (some? expected-owner)
         (= role expected-owner)
         (some? expected-boundary)
         (= expected-boundary
            actual-boundary))]

    (obligation
     {:id
      [:projection-runtime-origin
       role
       runtime-locator
       :authored-boundary]

      :property
      projection-runtime-origin-property

      :state
      semantic-state

      :role
      role

      :endpoint
      :runtime-state

      :runtime-locator
      runtime-locator

      :expected
      {:origin-kind :authored-boundary
       :semantic-state semantic-state
       :owner expected-owner
       :boundary expected-boundary}

      :actual
      {:origin-kind (:kind origin)
       :semantic-state semantic-state
       :owner role
       :boundary actual-boundary}

      :valid?
      valid?

      :reason
      (cond
        (not= :authored-boundary
              (:kind origin))
        :runtime-origin-kind-mismatch

        (nil? global-state)
        :missing-authored-semantic-state

        (not (contains? reachable semantic-state))
        :unreachable-authored-semantic-state

        (nil? expected-owner)
        :authored-semantic-state-has-no-runtime-owner

        (not= role expected-owner)
        :authored-semantic-role-mismatch

        (nil? expected-boundary)
        :unsupported-authored-runtime-origin

        (= expected-boundary actual-boundary)
        :authored-runtime-origin-preserved

        :else
        :authored-runtime-contract-mismatch)})))

(defn- receive-origin-detail
  [states reachable role alternative semantic-state]
  (let [global-state
        (get states semantic-state)

        expected
        (when global-state
          (merge
           (communication-route global-state)
           (communication-contract global-state)))

        actual
        (merge
         (receive-alternative-boundary
          role
          alternative))]

    {:semantic-state semantic-state
     :expected expected
     :actual actual
     :valid?
     (and
      (map? global-state)
      (contains? reachable semantic-state)
      (= :communicate (:op global-state))
      (= role (:to global-state))
      (= expected actual))
     :reason
     (cond
       (nil? global-state)
       :missing-authored-semantic-state

       (not (contains? reachable semantic-state))
       :unreachable-authored-semantic-state

       (not= :communicate (:op global-state))
       :authored-semantic-op-mismatch

       (not= role (:to global-state))
       :authored-semantic-role-mismatch

       (= expected actual)
       :authored-communication-origin-preserved

       :else
       :authored-communication-contract-mismatch)}))

(defn- semantic-successors
  [state]
  (case (:op state)
    :local [(:next state)]
    :authoritative [(:next state)]
    :communicate [(:next state)]
    :branch (->> (:cases state) vals (sort-by pr-str) vec)
    :await (->> (:events state) vals (sort-by pr-str) vec)
    :return []
    []))

(defn- role-participates-in-global-state?
  [role state]
  (case (:op state)
    (:local :authoritative :branch :await)
    (= role (:role state))

    :communicate
    (or (= role (:from state))
        (= role (:to state)))

    :return
    false

    false))

(defn- role-frontier-analysis
  "Compute the first global states at which role can next participate when
   entering semantic-state, independently of compiler provenance.

   Foreign states are traversed through all semantic successors. Once a state
   involving role is reached that path stops at the frontier. The result is
   finite because verified choreography control flow has finitely many state
   identities; revisiting an already explored foreign state adds no new first
   frontier."
  [states semantic-state role]
  (loop [pending [semantic-state]
         seen #{}
         frontier []]
    (if-let [state-id (first pending)]
      (if (contains? seen state-id)
        (recur (subvec pending 1)
               seen
               frontier)
        (let [seen' (conj seen state-id)
              state (get states state-id)]
          (cond
            (nil? state)
            (recur
             (subvec pending 1)
             seen'
             (conj frontier
                   {:semantic-state state-id
                    :kind :missing-state}))

            (role-participates-in-global-state?
             role
             state)
            (recur
             (subvec pending 1)
             seen'
             (conj
              frontier
              {:semantic-state state-id
               :kind
               (if (= :communicate (:op state))
                 (cond
                   (= role (:to state)) :receiver
                   (= role (:from state)) :sender
                   :else :communication)
                 :owner)
               :op (:op state)}))

            (= :return (:op state))
            (recur
             (subvec pending 1)
             seen'
             (conj
              frontier
              {:semantic-state state-id
               :kind :terminal
               :op :return}))

            :else
            (recur
             (into
              (subvec pending 1)
              (remove seen'
                      (semantic-successors state)))
             seen'
             frontier))))
      {:frontier
       (->> frontier
            distinct
            (sort-by
             (juxt
              (comp pr-str :semantic-state)
              (comp pr-str :kind)))
            vec)
       :visited-state-ids
       (->> seen (sort-by pr-str) vec)})))

(defn- role-free-suffix-analysis
  "Analyze the global suffix reachable from semantic-state for one role.

   Projection may synthesize role-local completion at semantic-state only when
   that role never participates in any state reachable from that entry. This is
   a finite control-flow fact; it does not depend on concrete payload/value
   domains."
  [states semantic-state role]
  (loop [pending [semantic-state]
         seen #{}
         participating []]
    (if-let [state-id (first pending)]
      (if (contains? seen state-id)
        (recur (subvec pending 1)
               seen
               participating)
        (let [seen' (conj seen state-id)
              state (get states state-id)
              participating'
              (if (and state
                       (role-participates-in-global-state?
                        role
                        state))
                (conj participating state-id)
                participating)
              successors
              (if state
                (semantic-successors state)
                [])]
          (recur
           (into (subvec pending 1)
                 (remove seen' successors))
           seen'
           participating')))
      {:visited-state-ids
       (->> seen (sort-by pr-str) vec)
       :participating-state-ids
       (->> participating distinct (sort-by pr-str) vec)
       :role-free?
       (empty? participating)})))

(defn- synthetic-receive-runtime-origin-obligation
  [states reachable role runtime-locator runtime-state origin]
  (let [source
        (:source-semantic-state origin)

        source-state
        (get states source)

        alternatives
        (:alternatives runtime-state)

        alternative-sources
        (:alternative-semantic-states origin)

        detail-groups
        (if (= (count alternatives)
               (count alternative-sources))
          (mapv
           (fn [alternative semantic-states]
             (mapv
              #(receive-origin-detail
                states
                reachable
                role
                alternative
                %)
              semantic-states))
           alternatives
           alternative-sources)
          [])

        details
        (vec
         (mapcat identity detail-groups))

        all-details-valid?
        (and
         (seq details)
         (every? :valid? details))

        frontier-analysis
        (when source-state
          (role-frontier-analysis
           states
           source
           role))

        frontier
        (:frontier frontier-analysis)

        frontier-receiver-states
        (->> frontier
             (filter #(= :receiver (:kind %)))
             (map :semantic-state)
             set)

        frontier-only-receives?
        (and
         (seq frontier)
         (every?
          #(= :receiver (:kind %))
          frontier))

        provenance-receiver-states
        (->> alternative-sources
             (mapcat identity)
             set)

        exact-frontier-coverage?
        (= frontier-receiver-states
           provenance-receiver-states)

        valid?
        (and
         (= :synthetic-receive
            (:kind origin))
         (= :receive (:op runtime-state))
         (map? source-state)
         (contains? reachable source)
         (= (count alternatives)
            (count alternative-sources))
         all-details-valid?
         frontier-only-receives?
         exact-frontier-coverage?)]

    (obligation
     {:id
      [:projection-runtime-origin
       role
       runtime-locator
       :synthetic-receive]

      :property
      projection-runtime-origin-property

      :state
      source

      :role
      role

      :endpoint
      :runtime-state

      :runtime-locator
      runtime-locator

      :expected
      {:origin-kind :synthetic-receive
       :source-semantic-state source
       :alternative-count (count alternatives)
       :all-alternatives-have-authored-communication-origins true
       :first-role-frontier
       {:only-receives? true
        :semantic-states provenance-receiver-states}}

      :actual
      {:origin-kind (:kind origin)
       :source-semantic-state source
       :alternative-count (count alternative-sources)
       :details details
       :frontier-analysis frontier-analysis}

      :valid?
      valid?

      :reason
      (cond
        (not= :synthetic-receive
              (:kind origin))
        :runtime-origin-kind-mismatch

        (not= :receive (:op runtime-state))
        :runtime-op-origin-mismatch

        (nil? source-state)
        :missing-receive-source-semantic-state

        (not (contains? reachable source))
        :unreachable-receive-source-semantic-state

        (not= (count alternatives)
              (count alternative-sources))
        :receive-origin-alternative-count-mismatch

        (empty? details)
        :missing-authored-communication-origins

        (not all-details-valid?)
        (:reason
         (first
          (remove :valid? details)))

        (not frontier-only-receives?)
        :receive-origin-source-frontier-mismatch

        (not exact-frontier-coverage?)
        :receive-origin-frontier-coverage-mismatch

        :else
        :synthetic-receive-origin-preserved)})))

(defn- synthetic-completion-runtime-origin-obligation
  [verified states reachable role compiled runtime-locator runtime-state origin]
  (let [source
        (:source-semantic-state origin)

        source-state
        (get states source)

        expected-local-completion
        {:op :return
         :outcome project/complete-outcome}

        continuation
        (project/semantic-continuation
         compiled
         source)

        actual-local-completion
        (select-keys
         runtime-state
         [:op :outcome])

        suffix-analysis
        (when source-state
          (role-free-suffix-analysis
           states
           source
           role))

        role-free-suffix?
        (true?
         (:role-free? suffix-analysis))

        valid?
        (and
         (= :synthetic-completion
            (:kind origin))
         (map? source-state)
         (contains? reachable source)
         (= runtime-locator continuation)
         (= expected-local-completion
            actual-local-completion)
         role-free-suffix?)]

    (obligation
     {:id
      [:projection-runtime-origin
       role
       runtime-locator
       :synthetic-completion]

      :property
      projection-runtime-origin-property

      :state
      source

      :role
      role

      :endpoint
      :runtime-state

      :runtime-locator
      runtime-locator

      :expected
      {:origin-kind :synthetic-completion
       :source-semantic-state source
       :runtime-locator runtime-locator
       :local-completion expected-local-completion
       :role-free-global-suffix true}

      :actual
      {:origin-kind (:kind origin)
       :source-semantic-state source
       :runtime-locator continuation
       :local-completion actual-local-completion
       :suffix-analysis suffix-analysis}

      :valid?
      valid?

      :reason
      (cond
        (not= :synthetic-completion
              (:kind origin))
        :runtime-origin-kind-mismatch

        (nil? source-state)
        :missing-completion-source-semantic-state

        (not (contains? reachable source))
        :unreachable-completion-source-semantic-state

        (not= runtime-locator continuation)
        :completion-origin-continuation-mismatch

        (not= expected-local-completion
              actual-local-completion)
        :noncanonical-local-completion

        (not role-free-suffix?)
        :completion-origin-role-still-participates

        :else
        :synthetic-completion-origin-preserved)})))

(defn- runtime-origin-obligation
  [verified states reachable role compiled runtime-locator]
  (let [runtime-state
        (get-in compiled
                [:executable-plan
                 :states
                 runtime-locator])

        origin
        (project/runtime-origin
         compiled
         runtime-locator)]

    (case (:kind origin)
      :authored-boundary
      (authored-runtime-origin-obligation
       states
       reachable
       role
       runtime-locator
       runtime-state
       origin)

      :synthetic-receive
      (synthetic-receive-runtime-origin-obligation
       states
       reachable
       role
       runtime-locator
       runtime-state
       origin)

      :synthetic-completion
      (synthetic-completion-runtime-origin-obligation
       verified
       states
       reachable
       role
       compiled
       runtime-locator
       runtime-state
       origin)

      (obligation
       {:id
        [:projection-runtime-origin
         role
         runtime-locator
         :unknown]

        :property
        projection-runtime-origin-property

        :state
        nil

        :role
        role

        :endpoint
        :runtime-state

        :runtime-locator
        runtime-locator

        :expected
        {:origin-kind
         #{:authored-boundary
           :synthetic-receive
           :synthetic-completion}}

        :actual
        {:origin origin
         :runtime-state runtime-state}

        :valid?
        false

        :reason
        :unknown-runtime-origin-kind}))))

(defn- role-runtime-origin-obligations
  [verified states reachable role compiled]
  (let [runtime-states
        (get-in compiled
                [:executable-plan :states])]
    (mapv
     #(runtime-origin-obligation
       verified
       states
       reachable
       role
       compiled
       %)
     (sort
      (keys runtime-states)))))

(defn- projection-runtime-origin-obligations
  [verified compiled-by-role]
  (let [states
        (get-in verified
                [:choreography :states])

        reachable
        (get-in verified
                [:verification
                 :analysis
                 :reachable-state-ids])

        roles
        (get-in verified
                [:verification
                 :analysis
                 :roles])]

    (vec
     (mapcat
      (fn [role]
        (if-let [compiled
                 (get compiled-by-role role)]
          (role-runtime-origin-obligations
           verified
           states
           reachable
           role
           compiled)

          [(obligation
            {:id
             [:projection-runtime-origin
              role
              :missing-plan]

             :property
             projection-runtime-origin-property

             :state
             nil

             :role
             role

             :endpoint
             :runtime-state

             :expected
             {:role-plan true}

             :actual
             {:role-plan false}

             :valid?
             false

             :reason
             :missing-role-plan})]))
      (sort-by pr-str roles)))))

;; -----------------------------------------------------------------------------
;; Results
;; -----------------------------------------------------------------------------

(defn result?
  "True when value is a structural result emitted by this proof namespace/version."
  [value]
  (and
   (map? value)
   (= result-type
      (:gesso.choreo/type value))
   (= proof-version
      (:gesso.choreo/version value))
   (contains?
    structural-properties
    (:property value))
   (boolean?
    (:valid? value))
   (vector?
    (:obligations value))
   (vector?
    (:runtime-obligations value))
   (vector?
    (:trusted-assumptions value))
   (vector?
    (:failures value))))

(defn valid?
  "True exactly when a proof/check result is valid."
  [result]
  (and
   (result? result)
   (true?
    (:valid? result))))

(defn failures
  "Return failed obligations/counterexamples from a result."
  [result]
  (when-not (result? result)
    (proof-error
     :invalid-result
     "Expected a Gesso Choreo proof result."
     {:value result}))
  (:failures result))

(defn first-counterexample
  "Return the first deterministic failed obligation, or nil when valid."
  [result]
  (first
   (failures result)))

(defn- successful-result
  [verified _compiled-by-role obligations runtime-obligations trusted-assumptions]
  (let [failures'
        (vec
         (remove :valid?
                 obligations))

        verification
        (:verification verified)]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version

     :property
     projection-boundary-property

     :classification
     result-classification

     :valid?
     (empty? failures')

     :scope
     {:kind :exact-finite-choreography-structure
      :reachable-state-count
      (count
       (get-in verification
               [:analysis
                :reachable-state-ids]))
      :role-count
      (count
       (get-in verification
               [:analysis
                :roles]))
      :obligation-count
      (count obligations)}

     :verification-version
     (:gesso.choreo/version verification)

     :executable-plan-version
     project/executable-plan-version

     :verification-options
     (:options verification)

     :obligations
     obligations

     :runtime-obligations
     runtime-obligations

     :trusted-assumptions
     trusted-assumptions

     :failures
     failures'

     :counterexample
     (first failures')}))


(defn- successful-successor-result
  [verified obligations]
  (let [failures'
        (vec
         (remove :valid?
                 obligations))

        verification
        (:verification verified)]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version

     :property
     projection-successor-property

     :classification
     successor-result-classification

     :valid?
     (empty? failures')

     :scope
     {:kind :exact-finite-choreography-transition-structure
      :reachable-state-count
      (count
       (get-in verification
               [:analysis
                :reachable-state-ids]))
      :role-count
      (count
       (get-in verification
               [:analysis
                :roles]))
      :obligation-count
      (count obligations)}

     :verification-version
     (:gesso.choreo/version verification)

     :executable-plan-version
     project/executable-plan-version

     :verification-options
     (:options verification)

     :obligations
     obligations

     :runtime-obligations
     []

     :trusted-assumptions
     []

     :failures
     failures'

     :counterexample
     (first failures')}))

(defn- successful-completion-result
  [verified obligations]
  (let [failures'
        (vec
         (remove :valid?
                 obligations))

        verification
        (:verification verified)]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version

     :property
     projection-completion-property

     :classification
     completion-result-classification

     :valid?
     (empty? failures')

     :scope
     {:kind :exact-finite-choreography-completion-structure
      :reachable-state-count
      (count
       (get-in verification
               [:analysis
                :reachable-state-ids]))
      :reachable-return-count
      (count
       (reachable-return-state-ids verified))
      :role-count
      (count
       (get-in verification
               [:analysis
                :roles]))
      :obligation-count
      (count obligations)}

     :verification-version
     (:gesso.choreo/version verification)

     :executable-plan-version
     project/executable-plan-version

     :verification-options
     (:options verification)

     :obligations
     obligations

     :runtime-obligations
     []

     :trusted-assumptions
     []

     :failures
     failures'

     :counterexample
     (first failures')}))

(defn- successful-observable-origin-result
  [verified obligations]
  (let [failures'
        (vec
         (remove :valid?
                 obligations))

        verification
        (:verification verified)]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version

     :property
     projection-observable-origin-property

     :classification
     observable-origin-result-classification

     :valid?
     (empty? failures')

     :scope
     {:kind :exact-finite-projected-observable-origin-structure
      :reachable-state-count
      (count
       (get-in verification
               [:analysis
                :reachable-state-ids]))
      :role-count
      (count
       (get-in verification
               [:analysis
                :roles]))
      :projected-observable-runtime-ops
      projected-observable-runtime-ops
      :obligation-count
      (count obligations)}

     :verification-version
     (:gesso.choreo/version verification)

     :executable-plan-version
     project/executable-plan-version

     :compiler-projection-version
     project/compiler-projection-version

     :verification-options
     (:options verification)

     :obligations
     obligations

     :runtime-obligations
     []

     :trusted-assumptions
     []

     :failures
     failures'

     :counterexample
     (first failures')}))


(defn- successful-runtime-origin-result
  [verified obligations]
  (let [failures'
        (vec
         (remove :valid?
                 obligations))

        verification
        (:verification verified)]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version

     :property
     projection-runtime-origin-property

     :classification
     runtime-origin-result-classification

     :valid?
     (empty? failures')

     :scope
     {:kind :exact-finite-projected-runtime-origin-structure
      :reachable-state-count
      (count
       (get-in verification
               [:analysis
                :reachable-state-ids]))
      :role-count
      (count
       (get-in verification
               [:analysis
                :roles]))
      :projected-runtime-ops
      projected-runtime-ops
      :obligation-count
      (count obligations)}

     :verification-version
     (:gesso.choreo/version verification)

     :executable-plan-version
     project/executable-plan-version

     :compiler-projection-version
     project/compiler-projection-version

     :verification-options
     (:options verification)

     :obligations
     obligations

     :runtime-obligations
     []

     :trusted-assumptions
     []

     :failures
     failures'

     :counterexample
     (first failures')}))

(defn- runtime-origin-construction-failure-result
  [phase ex]
  (let [counterexample
        {:kind :construction-failure
         :phase phase
         :message
         (or (ex-message ex)
             (str ex))
         :data
         (ex-data ex)}]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version
     :property projection-runtime-origin-property
     :classification runtime-origin-result-classification
     :valid? false
     :scope
     {:kind :not-checked
      :reason :construction-failure}
     :obligations []
     :runtime-obligations []
     :trusted-assumptions []
     :failures [counterexample]
     :counterexample counterexample}))

(defn- observable-origin-construction-failure-result
  [phase ex]
  (let [counterexample
        {:kind :construction-failure
         :phase phase
         :message
         (or (ex-message ex)
             (str ex))
         :data
         (ex-data ex)}]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version
     :property projection-observable-origin-property
     :classification observable-origin-result-classification
     :valid? false
     :scope
     {:kind :not-checked
      :reason :construction-failure}
     :obligations []
     :runtime-obligations []
     :trusted-assumptions []
     :failures [counterexample]
     :counterexample counterexample}))

(defn- completion-construction-failure-result
  [phase ex]
  (let [counterexample
        {:kind :construction-failure
         :phase phase
         :message
         (or (ex-message ex)
             (str ex))
         :data
         (ex-data ex)}]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version
     :property projection-completion-property
     :classification completion-result-classification
     :valid? false
     :scope
     {:kind :not-checked
      :reason :construction-failure}
     :obligations []
     :runtime-obligations []
     :trusted-assumptions []
     :failures [counterexample]
     :counterexample counterexample}))

(defn- successor-construction-failure-result
  [phase ex]
  (let [counterexample
        {:kind :construction-failure
         :phase phase
         :message
         (or (ex-message ex)
             (str ex))
         :data
         (ex-data ex)}]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version
     :property projection-successor-property
     :classification successor-result-classification
     :valid? false
     :scope
     {:kind :not-checked
      :reason :construction-failure}
     :obligations []
     :runtime-obligations []
     :trusted-assumptions []
     :failures [counterexample]
     :counterexample counterexample}))

(defn- construction-failure-result
  [phase ex]
  (let [counterexample
        {:kind :construction-failure
         :phase phase
         :message
         (or (ex-message ex)
             (str ex))
         :data
         (ex-data ex)}]

    {:gesso.choreo/type result-type
     :gesso.choreo/version proof-version
     :property projection-boundary-property
     :classification result-classification
     :valid? false
     :scope
     {:kind :not-checked
      :reason :construction-failure}
     :obligations []
     :runtime-obligations []
     :trusted-assumptions []
     :failures [counterexample]
     :counterexample counterexample}))

(defn check-projection-boundaries
  "Exhaustively check projection-boundary preservation for one exact finite
   choreography.

   The one-argument form accepts choreography data or an existing successful
   verification artifact.

   The two-argument form accepts verifier options for plain choreography data:

     {:verification-options
      {:entry-value-keys ...
       :entry-knowledge ...}}

   Passing verification options with an already-produced verification artifact
   retains verify/ensure-verified's normal rejection semantics.

   This function is non-throwing for verification/projection/check failures and
   returns a deterministic counterexample result instead. Programmer misuse of
   the returned result APIs may still throw.

   A valid result proves only the named finite structural property for this
   exact choreography/compiler output. It is not the later global-to-local trace
   refinement theorem."
  ([choreography-or-verified]
   (check-projection-boundaries
    choreography-or-verified
    nil))
  ([choreography-or-verified
    {:keys [verification-options]
     :as options}]
   (ensure-checker-options!
    options)

   (let [verified-result
         (try
           {:ok
            (if (some? verification-options)
              (verify/ensure-verified
               choreography-or-verified
               verification-options)
              (verify/ensure-verified
               choreography-or-verified))}
           (catch #?(:clj Throwable
                     :cljs :default) ex
             {:error ex}))]

     (if-let [ex (:error verified-result)]
       (construction-failure-result
        :verification
        ex)

       (let [verified
             (:ok verified-result)

             projection-result
             (try
               {:ok
                (project/compile-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (construction-failure-result
            :projection
            ex)

           (let [compiled-by-role
                 (:ok projection-result)

                 obligations
                 (projection-boundary-obligations
                  verified
                  compiled-by-role)

                 runtime-obligations
                 (authoritative-basis-runtime-obligations
                  verified)

                 trusted-assumptions
                 (authoritative-basis-trusted-assumptions
                  verified)]

             (successful-result
              verified
              compiled-by-role
              obligations
              runtime-obligations
              trusted-assumptions))))))))


(defn check-projection-successors
  "Exhaustively check exact projected-successor preservation for one verified
   finite choreography/compiler output.

   This property is intentionally narrower than the eventual global-to-local
   trace/refinement theorem. It checks that each role-local executable
   transition emitted for a reachable semantic boundary targets the exact
   compiler-recorded continuation for the corresponding semantic successor.

   The check covers:

     - every role's initial executable continuation;
     - :local and :authoritative :next targets;
     - every :branch case target;
     - :communicate sender :next and receiver-alternative :next targets;
     - every :await event target.

   Projection may legitimately skip foreign/unobservable semantic work. The
   expected target is therefore project/semantic-continuation for the semantic
   successor, not the raw global successor id.

   A valid result proves only this exact finite structural successor property.
   It does not quantify over all machine executions and does not claim
   :trace-refinement or :projection-refinement."
  ([choreography-or-verified]
   (check-projection-successors
    choreography-or-verified
    nil))
  ([choreography-or-verified
    {:keys [verification-options]
     :as options}]
   (ensure-checker-options!
    options)

   (let [verified-result
         (try
           {:ok
            (if (some? verification-options)
              (verify/ensure-verified
               choreography-or-verified
               verification-options)
              (verify/ensure-verified
               choreography-or-verified))}
           (catch #?(:clj Throwable
                     :cljs :default) ex
             {:error ex}))]

     (if-let [ex (:error verified-result)]
       (successor-construction-failure-result
        :verification
        ex)

       (let [verified
             (:ok verified-result)

             projection-result
             (try
               {:ok
                (project/compile-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (successor-construction-failure-result
            :projection
            ex)

           (successful-successor-result
            verified
            (projection-successor-obligations
             verified
             (:ok projection-result)))))))))


(defn check-projection-runtime-origins
  "Exhaustively check reverse semantic-origin justification for every emitted
   projected runtime state.

   This extends observable-origin preservation across distributed-hidden and
   stuttering runtime states. Authored local/authoritative/branch/await/send
   states must match exact reachable global authored boundaries. Synthetic
   receives must retain exact reachable authored communication origins for every
   alternative, including intentional many-to-one collapse. Synthetic completion
   must retain a reachable semantic source and the canonical role-local
   completion continuation.

   This is still a finite compiler-origin property. It closes the static origin
   premise needed by a later dynamic projected-step simulation relation; it does
   not itself quantify over arbitrary runtime executions, message schedules, or
   traces."
  ([choreography-or-verified]
   (check-projection-runtime-origins
    choreography-or-verified
    nil))
  ([choreography-or-verified
    {:keys [verification-options]
     :as options}]
   (ensure-checker-options!
    options)

   (let [verified-result
         (try
           {:ok
            (if (some? verification-options)
              (verify/ensure-verified
               choreography-or-verified
               verification-options)
              (verify/ensure-verified
               choreography-or-verified))}
           (catch #?(:clj Throwable
                     :cljs :default) ex
             {:error ex}))]

     (if-let [ex (:error verified-result)]
       (runtime-origin-construction-failure-result
        :verification
        ex)

       (let [verified
             (:ok verified-result)

             projection-result
             (try
               {:ok
                (project/compile-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (runtime-origin-construction-failure-result
            :projection
            ex)

           (successful-runtime-origin-result
            verified
            (projection-runtime-origin-obligations
             verified
             (:ok projection-result)))))))))

(defn check-projection-runtime-origins!
  "Check all projected runtime-origin preservation obligations or throw with the
   complete result."
  ([choreography-or-verified]
   (check-projection-runtime-origins!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [result
         (check-projection-runtime-origins
          choreography-or-verified
          options)]
     (if (valid? result)
       result
       (proof-error
        :projection-runtime-origin-proof-failed
        "Gesso Choreo projected runtime-origin preservation check failed."
        {:result result})))))

(defn check-projection-observable-origins
  "Exhaustively check reverse semantic-origin coverage for every projected
   runtime boundary that can directly create a distributed observation.

   The current projected distributed-observable runtime operations are
   :authoritative and :receive. For every emitted instance this checker follows
   CompilerProjection v3 runtime-origin provenance back into the exact verified
   global choreography and requires:

     - projected :authoritative -> one exact authored global :authoritative
       boundary owned by the same role with the same operation/output contract;

     - every projected :receive alternative -> one or more exact authored global
       :communicate states addressed to that role with the same route/message
       contract.

   This is the first converse structural property needed by projection
   refinement. It rules out compiler-emitted observable boundaries with no
   authored semantic justification, including forged reverse provenance that is
   internally self-consistent but points outside or at the wrong global state.

   It deliberately does NOT yet prove the universal projected-step simulation
   theorem. Dynamic claims such as 'the delivered envelope was produced by the
   corresponding sender occurrence' additionally depend on realization/transport
   invariants (including one semantic delivery per root send occurrence)."
  ([choreography-or-verified]
   (check-projection-observable-origins
    choreography-or-verified
    nil))
  ([choreography-or-verified
    {:keys [verification-options]
     :as options}]
   (ensure-checker-options!
    options)

   (let [verified-result
         (try
           {:ok
            (if (some? verification-options)
              (verify/ensure-verified
               choreography-or-verified
               verification-options)
              (verify/ensure-verified
               choreography-or-verified))}
           (catch #?(:clj Throwable
                     :cljs :default) ex
             {:error ex}))]

     (if-let [ex (:error verified-result)]
       (observable-origin-construction-failure-result
        :verification
        ex)

       (let [verified
             (:ok verified-result)

             projection-result
             (try
               {:ok
                (project/compile-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (observable-origin-construction-failure-result
            :projection
            ex)

           (successful-observable-origin-result
            verified
            (projection-observable-origin-obligations
             verified
             (:ok projection-result)))))))))

(defn check-projection-observable-origins!
  "Check projected observable-origin preservation or throw with the complete result."
  ([choreography-or-verified]
   (check-projection-observable-origins!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [result
         (check-projection-observable-origins
          choreography-or-verified
          options)]
     (if (valid? result)
       result
       (proof-error
        :projection-observable-origin-proof-failed
        "Gesso Choreo projected observable-origin preservation check failed."
        {:result result})))))

(defn check-projection-completions
  "Check exact finite preservation of global terminal completion into projected
   role-local completion.

   For every reachable global :return state and every projected role, every
   incoming reachable control-flow path must cross a nearest compiler-recorded
   continuation that is already the canonical local completion state
   {:op :return :outcome project/complete-outcome}. A role may therefore finish
   before the global return rather than having a direct continuation entry on the
   terminal state itself.

   This deliberately does NOT prove authored global terminal-outcome
   compatibility. Projected role-local :return states encode only that a role has
   no further protocol work; they do not carry the global outcome. The global
   outcome is retained in each obligation only as diagnostic context."
  ([choreography-or-verified]
   (check-projection-completions
    choreography-or-verified
    nil))
  ([choreography-or-verified
    {:keys [verification-options]
     :as options}]
   (ensure-checker-options! options)

   (let [verified-result
         (try
           {:ok
            (if (some? verification-options)
              (verify/ensure-verified
               choreography-or-verified
               verification-options)
              (verify/ensure-verified
               choreography-or-verified))}
           (catch #?(:clj Throwable
                     :cljs :default) ex
             {:error ex}))]

     (if-let [ex (:error verified-result)]
       (completion-construction-failure-result
        :verification
        ex)

       (let [verified
             (:ok verified-result)

             projection-result
             (try
               {:ok
                (project/compile-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (completion-construction-failure-result
            :projection
            ex)

           (successful-completion-result
            verified
            (projection-completion-obligations
             verified
             (:ok projection-result)))))))))

(defn check-projection-completions!
  "Check projected-completion preservation or throw with the complete result."
  ([choreography-or-verified]
   (check-projection-completions!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [result
         (check-projection-completions
          choreography-or-verified
          options)]
     (if (valid? result)
       result
       (proof-error
        :projection-completion-proof-failed
        "Gesso Choreo projection-completion preservation check failed."
        {:result result})))))

(defn check-projection-successors!
  "Check projected-successor preservation or throw with the complete result."
  ([choreography-or-verified]
   (check-projection-successors!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [result
         (check-projection-successors
          choreography-or-verified
          options)]
     (if (valid? result)
       result
       (proof-error
        :projection-successor-proof-failed
        "Gesso Choreo projection-successor preservation check failed."
        {:result result})))))

(defn check-projection-boundaries!
  "Check projection-boundary preservation or throw with the complete result."
  ([choreography-or-verified]
   (check-projection-boundaries!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [result
         (check-projection-boundaries
          choreography-or-verified
          options)]
     (if (valid? result)
       result
       (proof-error
        :projection-boundary-proof-failed
        "Gesso Choreo projection-boundary preservation check failed."
        {:result result})))))

(def ^:private projection-structural-certificate-v1-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :classification
    :valid?
    :properties
    :verification-version
    :executable-plan-version
    :compiler-projection-version
    :semantics-version
    :distributed-observation-relation
    :boundary-proof
    :successor-proof
    :runtime-obligations
    :trusted-assumptions
    :failures
    :nonclaims})

(def ^:private projection-structural-certificate-v2-keys
  (conj
   projection-structural-certificate-v1-keys
   :completion-proof))

(def ^:private projection-structural-certificate-keys
  (conj
   projection-structural-certificate-v2-keys
   :observable-origin-proof))

(defn- certificate-contract-versions-valid?
  [value]
  (and
   (= verify/verification-version
      (:verification-version value))
   (= project/executable-plan-version
      (:executable-plan-version value))
   (= project/compiler-projection-version
      (:compiler-projection-version value))
   (= semantics/semantics-version
      (:semantics-version value))
   (= {:observable-kinds semantics/distributed-observation-kinds
       :hidden-kinds semantics/distributed-hidden-kinds}
      (:distributed-observation-relation value))))

(defn- projection-structural-certificate-v1?
  [value]
  (and
   (= projection-structural-certificate-v1-keys
      (set (keys value)))
   (= projection-structural-certificate-v1-version
      (:gesso.choreo/version value))
   (= projection-structural-certificate-v1-properties
      (:properties value))
   (certificate-contract-versions-valid? value)
   (result? (:boundary-proof value))
   (= projection-boundary-property
      (get-in value [:boundary-proof :property]))
   (result? (:successor-proof value))
   (= projection-successor-property
      (get-in value [:successor-proof :property]))
   (vector? (:runtime-obligations value))
   (vector? (:trusted-assumptions value))
   (vector? (:failures value))
   (= projection-structural-certificate-v1-nonclaims
      (:nonclaims value))
   (= (:valid? value)
      (and
       (valid? (:boundary-proof value))
       (valid? (:successor-proof value))
       (empty? (:failures value))))))

(defn- projection-structural-certificate-v2?
  [value]
  (and
   (= projection-structural-certificate-v2-keys
      (set (keys value)))
   (= projection-structural-certificate-v2-version
      (:gesso.choreo/version value))
   (= projection-structural-certificate-v2-properties
      (:properties value))
   (certificate-contract-versions-valid? value)
   (result? (:boundary-proof value))
   (= projection-boundary-property
      (get-in value [:boundary-proof :property]))
   (result? (:successor-proof value))
   (= projection-successor-property
      (get-in value [:successor-proof :property]))
   (result? (:completion-proof value))
   (= projection-completion-property
      (get-in value [:completion-proof :property]))
   (vector? (:runtime-obligations value))
   (vector? (:trusted-assumptions value))
   (vector? (:failures value))
   (= projection-structural-certificate-v2-nonclaims
      (:nonclaims value))
   (= (:valid? value)
      (and
       (valid? (:boundary-proof value))
       (valid? (:successor-proof value))
       (valid? (:completion-proof value))
       (empty? (:failures value))))))

(defn- projection-structural-certificate-v3?
  [value]
  (and
   (= projection-structural-certificate-keys
      (set (keys value)))
   (= projection-structural-certificate-version
      (:gesso.choreo/version value))
   (= projection-structural-certificate-properties
      (:properties value))
   (certificate-contract-versions-valid? value)
   (result? (:boundary-proof value))
   (= projection-boundary-property
      (get-in value [:boundary-proof :property]))
   (result? (:successor-proof value))
   (= projection-successor-property
      (get-in value [:successor-proof :property]))
   (result? (:completion-proof value))
   (= projection-completion-property
      (get-in value [:completion-proof :property]))
   (result? (:observable-origin-proof value))
   (= projection-observable-origin-property
      (get-in value [:observable-origin-proof :property]))
   (vector? (:runtime-obligations value))
   (vector? (:trusted-assumptions value))
   (vector? (:failures value))
   (= projection-refinement-nonclaims
      (:nonclaims value))
   (= (:valid? value)
      (and
       (valid? (:boundary-proof value))
       (valid? (:successor-proof value))
       (valid? (:completion-proof value))
       (valid? (:observable-origin-proof value))
       (empty? (:failures value))))))

(defn projection-structural-certificate?
  "True when value is a closed projection structural certificate emitted by
   this proof namespace.

   Version 1 is the historical boundary+successor certificate.

   Version 2 is the historical boundary+successor+completion certificate.

   Version 3 is the current certificate. It additionally composes projected
   observable-origin preservation and binds all four finite structural checks to
   the current verifier/compiler/global-semantics versions and distributed
   observation alphabet.

   Historical versions remain recognizable with exactly their original shapes
   and nonclaims. No version is itself the dynamic projected-step simulation or
   trace/projection-refinement theorem."
  [value]
  (and
   (map? value)
   (= projection-structural-certificate-type
      (:gesso.choreo/type value))
   (= projection-structural-certificate-classification
      (:classification value))
   (boolean? (:valid? value))
   (case (:gesso.choreo/version value)
     1 (projection-structural-certificate-v1? value)
     2 (projection-structural-certificate-v2? value)
     3 (projection-structural-certificate-v3? value)
     false)))

(defn structural-certificate-valid?
  "True exactly when certificate is a recognized valid projection structural
   certificate, including historical v1/v2 and current v3 artifacts."
  [certificate]
  (and
   (projection-structural-certificate? certificate)
   (true? (:valid? certificate))))

(defn- property-failures
  [property result]
  (map
   (fn [failure]
     {:property property
      :counterexample failure})
   (:failures result)))

(defn- structural-certificate-failures
  [boundary-proof successor-proof completion-proof observable-origin-proof]
  (vec
   (concat
    (property-failures
     projection-boundary-property
     boundary-proof)
    (property-failures
     projection-successor-property
     successor-proof)
    (property-failures
     projection-completion-property
     completion-proof)
    (property-failures
     projection-observable-origin-property
     observable-origin-proof))))

(defn- combined-runtime-obligations
  [& results]
  (vec
   (distinct
    (mapcat :runtime-obligations results))))

(defn- combined-trusted-assumptions
  [& results]
  (vec
   (distinct
    (mapcat :trusted-assumptions results))))

(defn check-projection-structure
  "Compose the currently proved finite projection-structure properties into the
   current theorem-facing ProjectionStructuralCertificate v3.

   Version 3 establishes exactly:

     - projection boundary preservation;
     - projection successor preservation;
     - projection completion preservation; and
     - projected observable-origin preservation.

   Historical v1 and v2 certificates remain recognizable but are never
   rewritten or reinterpreted as having proved later properties.

   The current certificate records the exact verifier/compiler/global-semantics
   versions and distributed observation alphabet against which later refinement
   work must be stated. Runtime authoritative-basis obligations and trusted
   assumptions remain explicit siblings rather than being promoted into proved
   facts.

   Observable-origin preservation proves that every compiler-emitted projected
   boundary capable of directly creating a distributed observation has exact
   authored semantic justification. It does NOT yet prove that arbitrary dynamic
   projected executions are simulated by legal global executions.

   The certificate therefore deliberately records :projected-step-simulation,
   terminal-outcome preservation, :trace-refinement, and :projection-refinement
   as nonclaims.

   Completion preservation proves only that every role has reached canonical
   local completion by each reachable global terminal frontier; role-local
   completion intentionally does not encode the authored global terminal outcome.

   This certificate still does not quantify over arbitrary projected executions
   and therefore is not the central v4.5 projection/refinement theorem."
  ([choreography-or-verified]
   (check-projection-structure choreography-or-verified nil))
  ([choreography-or-verified options]
   (ensure-checker-options! options)
   (let [boundary-proof
         (check-projection-boundaries
          choreography-or-verified
          options)

         successor-proof
         (check-projection-successors
          choreography-or-verified
          options)

         completion-proof
         (check-projection-completions
          choreography-or-verified
          options)

         observable-origin-proof
         (check-projection-observable-origins
          choreography-or-verified
          options)

         failures'
         (structural-certificate-failures
          boundary-proof
          successor-proof
          completion-proof
          observable-origin-proof)

         valid?'
         (and
          (valid? boundary-proof)
          (valid? successor-proof)
          (valid? completion-proof)
          (valid? observable-origin-proof)
          (empty? failures'))]

     {:gesso.choreo/type
      projection-structural-certificate-type

      :gesso.choreo/version
      projection-structural-certificate-version

      :classification
      projection-structural-certificate-classification

      :valid?
      valid?'

      :properties
      projection-structural-certificate-properties

      :verification-version
      verify/verification-version

      :executable-plan-version
      project/executable-plan-version

      :compiler-projection-version
      project/compiler-projection-version

      :semantics-version
      semantics/semantics-version

      :distributed-observation-relation
      {:observable-kinds semantics/distributed-observation-kinds
       :hidden-kinds semantics/distributed-hidden-kinds}

      :boundary-proof
      boundary-proof

      :successor-proof
      successor-proof

      :completion-proof
      completion-proof

      :observable-origin-proof
      observable-origin-proof

      :runtime-obligations
      (combined-runtime-obligations
       boundary-proof
       successor-proof
       completion-proof
       observable-origin-proof)

      :trusted-assumptions
      (combined-trusted-assumptions
       boundary-proof
       successor-proof
       completion-proof
       observable-origin-proof)

      :failures
      failures'

      :nonclaims
      projection-refinement-nonclaims})))

(defn check-projection-structure!
  "Return a valid projection structural certificate or throw with the complete
   certificate when either composed structural property fails."
  ([choreography-or-verified]
   (check-projection-structure!
    choreography-or-verified
    nil))
  ([choreography-or-verified options]
   (let [certificate
         (check-projection-structure
          choreography-or-verified
          options)]
     (if (structural-certificate-valid? certificate)
       certificate
       (proof-error
        :projection-structural-certificate-failed
        "Gesso Choreo projection structural certificate contains failed obligations."
        {:certificate certificate})))))

(defn explain-structural-certificate
  "Return a compact stable explanation of one projection structural certificate.

   Historical v1 explanations retain their original shape.
   Historical v2 explanations additionally report completion-obligation count.
   Current v3 explanations additionally report observable-origin-obligation
   count."
  [certificate]
  (when-not (projection-structural-certificate? certificate)
    (proof-error
     :invalid-projection-structural-certificate
     "Expected a Gesso Choreo projection structural certificate."
     {:value certificate}))

  (let [base
        {:classification
         (:classification certificate)

         :valid?
         (:valid? certificate)

         :properties
         (:properties certificate)

         :verification-version
         (:verification-version certificate)

         :executable-plan-version
         (:executable-plan-version certificate)

         :compiler-projection-version
         (:compiler-projection-version certificate)

         :semantics-version
         (:semantics-version certificate)

         :distributed-observation-relation
         (:distributed-observation-relation certificate)

         :boundary-obligation-count
         (count
          (get-in certificate
                  [:boundary-proof :obligations]))

         :successor-obligation-count
         (count
          (get-in certificate
                  [:successor-proof :obligations]))

         :runtime-obligation-count
         (count (:runtime-obligations certificate))

         :trusted-assumption-count
         (count (:trusted-assumptions certificate))

         :failure-count
         (count (:failures certificate))

         :nonclaims
         (:nonclaims certificate)}

        with-completion
        (if (<= projection-structural-certificate-v2-version
                (:gesso.choreo/version certificate))
          (assoc
           base
           :completion-obligation-count
           (count
            (get-in certificate
                    [:completion-proof :obligations])))
          base)]

    (if (= projection-structural-certificate-version
           (:gesso.choreo/version certificate))
      (assoc
       with-completion
       :observable-origin-obligation-count
       (count
        (get-in certificate
                [:observable-origin-proof :obligations])))
      with-completion)))

(defn explain
  "Return a compact stable summary of a structural proof result."
  [result]
  (when-not (result? result)
    (proof-error
     :invalid-result
     "Expected a Gesso Choreo proof result."
     {:value result}))

  {:property
   (:property result)

   :classification
   (:classification result)

   :valid?
   (:valid? result)

   :scope
   (:scope result)

   :failure-count
   (count
    (:failures result))

   :runtime-obligation-count
   (count
    (:runtime-obligations result))

   :trusted-assumption-count
   (count
    (:trusted-assumptions result))

   :counterexample
   (:counterexample result)})


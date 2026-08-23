(ns gesso.choreo.proof
  "Small machine-checkable proof obligations for the current Gesso Choreo core.

   This namespace deliberately begins with one narrow property instead of
   pretending that the current verifier already proves full projection
   refinement.

   The first checked property is:

     :projection-boundary-preservation-v1

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
       owner, environment-event set, and declared semantic event-data contracts

     :communicate
       the sender projection retains the exact route/message contract and the
       receiver projection contains an exact receive alternative for that same
       route/message contract

   Successor state ids are intentionally NOT required to remain equal. Projection
   is allowed to skip foreign-local work and synthesize receive/completion states;
   preserving raw global successor ids would therefore be the wrong theorem.

   Likewise this property does not yet prove trace/refinement preservation,
   knowledge derivations, authority safety, liveness, or browser realization.
   It is an exhaustive structural check over every reachable boundary in the
   exact finite authored program supplied to it. The result says exactly that
   and no more.

   Concrete behavioral witness checking lives separately in
   gesso.choreo.correspondence. This namespace owns only structural/compiler
   proof obligations so there is no second distributed execution engine hidden
   inside the proof layer.

   Proof/checker artifacts live on the compiler/test side. Nothing in this
   namespace is required by the portable production machine or ExecutablePlan
   runtime semantics. ExecutablePlan intentionally contains compact runtime
   locators only; this checker locates preserved boundaries structurally and
   keeps authored semantic state identities only in proof obligations."
  (:require
   [gesso.choreo.core :as choreo]
   [gesso.choreo.project :as project]
   [gesso.choreo.verify :as verify]))

;; -----------------------------------------------------------------------------
;; Identity
;; -----------------------------------------------------------------------------

(def proof-version 1)

(def result-type
  :gesso.choreo.proof/result)

(def obligation-type
  :gesso.choreo.proof/obligation)

(def projection-boundary-property
  :projection-boundary-preservation-v1)

(def result-classification
  :exhaustive-finite-structural-check)

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

(defn- matching-owner-locations
  [plan global-state expected]
  (if-not plan
    []
    (vec
     (for [[runtime-locator projected-state]
           (:states plan)

           :let [actual
                 (projected-owner-boundary
                  global-state
                  projected-state)]

           :when (= expected actual)]
       {:runtime-locator runtime-locator
        :boundary actual}))))

(defn- owner-obligation
  [plans state-id global-state]
  (let [role
        (:role global-state)

        plan
        (get plans role)

        expected
        (expected-owner-boundary global-state)

        matches
        (matching-owner-locations
         plan
         global-state
         expected)

        actual
        (:boundary (first matches))]

    (obligation
     {:id
      [:projection-boundary
       state-id
       :owner]

      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :owner
      :expected expected
      :actual actual
      :runtime-locator
      (:runtime-locator (first matches))
      :valid? (boolean (seq matches))
      :reason
      (cond
        (nil? plan)
        :missing-role-plan

        (seq matches)
        :preserved

        :else
        :missing-owner-boundary)})))

(defn- matching-sender-locations
  [plan expected]
  (if-not plan
    []
    (vec
     (for [[runtime-locator projected-state]
           (:states plan)

           :let [actual
                 (projected-sender-boundary
                  projected-state)]

           :when (= expected actual)]
       {:runtime-locator runtime-locator
        :boundary actual}))))

(defn- communication-sender-obligation
  [plans state-id global-state]
  (let [role
        (:from global-state)

        plan
        (get plans role)

        expected
        (sender-boundary global-state)

        matches
        (matching-sender-locations
         plan
         expected)

        actual
        (:boundary (first matches))]

    (obligation
     {:id
      [:projection-boundary
       state-id
       :sender]

      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :sender
      :expected expected
      :actual actual
      :runtime-locator
      (:runtime-locator (first matches))
      :valid? (boolean (seq matches))
      :reason
      (cond
        (nil? plan)
        :missing-role-plan

        (seq matches)
        :preserved

        :else
        :missing-sender-boundary)})))

(defn- receive-alternative-locations
  [plan receiver-role expected]
  (vec
   (for [[projected-state-id projected-state]
         (:states plan)

         :when
         (= :receive
            (:op projected-state))

         [alternative-index alternative]
         (map-indexed
          vector
          (:alternatives projected-state))

         :let
         [actual
          (receive-alternative-boundary
           receiver-role
           alternative)]

         :when
         (= expected actual)]

     {:runtime-locator projected-state-id
      :alternative-index alternative-index
      :alternative actual})))

(defn- communication-receiver-obligation
  [plans state-id global-state]
  (let [role
        (:to global-state)

        plan
        (get plans role)

        expected
        (merge
         (communication-route global-state)
         (communication-contract global-state))

        matches
        (if plan
          (receive-alternative-locations
           plan
           role
           expected)
          [])]

    (obligation
     {:id
      [:projection-boundary
       state-id
       :receiver]

      :property projection-boundary-property
      :state state-id
      :role role
      :endpoint :receiver
      :expected expected
      :actual
      {:matching-alternatives matches}
      :valid? (boolean (seq matches))
      :reason
      (cond
        (nil? plan)
        :missing-role-plan

        (seq matches)
        :preserved

        :else
        :missing-receive-alternative)})))

(defn- state-obligations
  [plans state-id global-state]
  (case (:op global-state)
    :local
    [(owner-obligation
      plans
      state-id
      global-state)]

    :authoritative
    [(owner-obligation
      plans
      state-id
      global-state)]

    :branch
    [(owner-obligation
      plans
      state-id
      global-state)]

    :communicate
    [(communication-sender-obligation
      plans
      state-id
      global-state)

     (communication-receiver-obligation
      plans
      state-id
      global-state)]

    :await
    [(owner-obligation
      plans
      state-id
      global-state)]

    ;; Global :return has no role-owned boundary contract. Local completion is
    ;; intentionally weaker than assertion of the global terminal outcome, so
    ;; terminal/refinement obligations belong to a later property.
    :return
    []

    []))

(defn- projection-boundary-obligations
  [verified plans]
  (let [choreography
        (:choreography verified)

        states
        (:states choreography)

        reachable
        (get-in verified
                [:verification
                 :analysis
                 :reachable-state-ids])]

    (->> reachable
         (sort-by pr-str)
         (mapcat
          (fn [state-id]
            (state-obligations
             plans
             state-id
             (get states state-id))))
         vec)))

;; -----------------------------------------------------------------------------
;; Results
;; -----------------------------------------------------------------------------

(defn result?
  "True when value is a result emitted by this proof namespace/version."
  [value]
  (and
   (map? value)
   (= result-type
      (:gesso.choreo/type value))
   (= proof-version
      (:gesso.choreo/version value))
   (= projection-boundary-property
      (:property value))
   (boolean?
    (:valid? value))
   (vector?
    (:obligations value))
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
  [verified plans obligations]
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

     :failures
     failures'

     :counterexample
     (first failures')}))

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
   (when (and (some? options)
              (not (map? options)))
     (proof-error
      :invalid-options
      "Proof checker options must be a map."
      {:options options}))

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
                (project/project-all
                 verified)}
               (catch #?(:clj Throwable
                         :cljs :default) ex
                 {:error ex}))]

         (if-let [ex (:error projection-result)]
           (construction-failure-result
            :projection
            ex)

           (let [plans
                 (:ok projection-result)

                 obligations
                 (projection-boundary-obligations
                  verified
                  plans)]

             (successful-result
              verified
              plans
              obligations))))))))

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

(defn explain
  "Return a compact stable summary of a projection-boundary proof result."
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

   :counterexample
   (:counterexample result)})


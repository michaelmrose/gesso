(ns gesso.live.optimistic.choreo
  "Portable protocol-v3 choreography helpers for optimistic Gesso operations.

   This namespace deliberately does not own DOM targets, structural snapshots,
   browser timers, HTMX lifecycle, continuity generations, or physical cleanup.
   Those are browser-adapter/shell concerns.

   It also deliberately does not define one universal authoritative operation.
   V4.5 requires an authoritative choreography state to name the public model
   semantic operation it realizes (for example :request/claim), rather than an
   implementation placeholder such as :optimistic/execute-command.  Callers
   therefore build an operation-specific choreography with command-choreography.

   The direct command choreography models one short execution:

     browser derives explicit provisional knowledge
       -> browser communicates semantic command
       -> trusted authority performs the public model operation
       -> authority communicates a typed protocol-v3 settlement
       -> browser resolves its provisional trajectory
       -> terminal generic settlement resolution

   A lost direct settlement does not require a suspended server machine.  A
   later authoritative reread may instead resolve the provisional trajectory
   through a separate short supersession-recovery choreography.  This follows
   v4.5's rule that durable waiting lives in model authority, not in arbitrary
   in-process choreography continuations.

   Shape validation here is not authorization.  The trusted authoritative
   adapter must authenticate the principal, select/authorize the configured
   public model operation, reread/revalidate authority, perform any atomic model
   transition, and construct the settlement."
  (:require
   [clojure.set :as set]
   [gesso.choreo.core :as choreo]
   [gesso.live.optimistic.protocol :as protocol]
   #?(:clj [gesso.choreo.project :as project])
   #?(:clj [gesso.choreo.verify :as verify])))

;; =============================================================================
;; Stable semantic vocabulary
;; =============================================================================

(def default-browser-role :browser)
(def default-authority-role :authority)

(def command-event protocol/command-event)
(def settlement-event protocol/settlement-event)

(def derive-provisional-action
  :gesso.live.optimistic/derive-provisional)

(def resolve-settlement-action
  :gesso.live.optimistic/resolve-settlement)

(def resolve-supersession-action
  :gesso.live.optimistic/resolve-supersession)

(def provisional-value-key
  :gesso.live.optimistic/provisional)

(def settlement-value-key
  :gesso.live.optimistic/settlement)

(def resolution-value-key
  :gesso.live.optimistic/resolution)

(def reread-authoritative-key
  :gesso.live.optimistic/authoritative)

(def reread-basis-key
  :gesso.live.optimistic/authoritative-basis)

(def default-authoritative-observed-event
  :gesso.live.optimistic/authoritative-observed)

(def semantic-command-required-keys
  "Semantic command facts required by an optimistic choreography execution.

   Protocol-v3 permits a non-optimistic command to omit :observed-basis, but an
   optimistic projection may not.  Wire protocol version is transport metadata,
   not a Choreo semantic fact."
  (-> protocol/command-required-keys
      (disj protocol/protocol-version-key)
      (conj protocol/observed-basis-key)))

(def semantic-command-optional-keys
  "Optional semantic command facts that may cross to the authority role."
  (-> protocol/command-optional-keys
      (disj protocol/observed-basis-key)))

(def command-correlation-keys
  #{protocol/command-id-key
    protocol/execution-id-key})

(def settlement-message-required-keys
  "The authority sends the two top-level correlation identities plus one closed
   protocol-v3 settlement value.  The settlement value itself is validated by
   gesso.live.optimistic.protocol; duplicating the two identity facts at the
   Choreo message boundary lets projected machines enforce correlation without
   inspecting nested maps."
  #{protocol/command-id-key
    protocol/execution-id-key
    settlement-value-key})

(def direct-terminal-resolutions
  protocol/settlement-resolutions)

;; =============================================================================
;; Errors / configuration
;; =============================================================================

(defn- choreo-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.optimistic.choreo/error
      :error/kind kind}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (choreo-error
     :invalid-shape
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- require-keyword!
  [label value]
  (when-not (keyword? value)
    (choreo-error
     :invalid-keyword
     (str label " must be a keyword.")
     {:label label
      :value value}))
  value)

(defn- require-closed-options!
  [label options allowed]
  (let [options' (require-map! label (or options {}))
        unknown (set/difference (set (keys options')) allowed)]
    (when (seq unknown)
      (choreo-error
       :unknown-option
       (str label " contains unknown options.")
       {:label label
        :unknown unknown
        :allowed allowed}))
    options'))

(def ^:private command-option-keys
  #{:name
    :browser-role
    :authority-role
    :operation
    :derive-provisional-action
    :resolve-settlement-action})

(defn- normalize-command-options
  [options]
  (let [options'
        (require-closed-options!
         "Optimistic command choreography options"
         options
         command-option-keys)

        {:keys [name
                browser-role
                authority-role
                operation
                derive-provisional-action
                resolve-settlement-action]
         :or {browser-role default-browser-role
              authority-role default-authority-role
              derive-provisional-action derive-provisional-action
              resolve-settlement-action resolve-settlement-action}}
        options']

    (when (= browser-role authority-role)
      (choreo-error
       :same-role
       "Optimistic browser and authority roles must be distinct."
       {:browser-role browser-role
        :authority-role authority-role}))

    {:name
     (require-keyword! "Optimistic choreography :name" name)

     :browser-role
     (require-keyword! "Optimistic choreography :browser-role" browser-role)

     :authority-role
     (require-keyword! "Optimistic choreography :authority-role" authority-role)

     :operation
     (require-keyword! "Optimistic choreography :operation" operation)

     :derive-provisional-action
     (require-keyword!
      "Optimistic choreography :derive-provisional-action"
      derive-provisional-action)

     :resolve-settlement-action
     (require-keyword!
      "Optimistic choreography :resolve-settlement-action"
      resolve-settlement-action)}))

;; =============================================================================
;; Protocol/runtime value helpers
;; =============================================================================

(defn command-values
  "Validate one protocol-v3 optimistic command and return exactly the semantic
   facts consumed by command-choreography.

   Unlike protocol/command, this helper requires :observed-basis because a
   command entering an optimistic trajectory must justify its provisional
   projection from known authority."
  [command-envelope]
  (let [command'
        (protocol/command
         (dissoc command-envelope protocol/protocol-version-key))]
    (when-not (contains? command' protocol/observed-basis-key)
      (choreo-error
       :missing-observed-basis
       "An optimistic choreography command requires :observed-basis."
       {:command command'}))
    (select-keys
     command'
     (set/union
      semantic-command-required-keys
      semantic-command-optional-keys))))

(defn require-operation
  "Validate command-envelope and require that its semantic :operation matches
   the public authoritative operation configured for this choreography.

   This is a correlation check, not authorization.  The trusted server must
   still select and authorize the operation independently of browser claims."
  [operation command-envelope]
  (let [operation' (require-keyword! "Public authoritative operation" operation)
        command' (command-values command-envelope)]
    (when-not (= (protocol/qualified-name operation')
                 (protocol/qualified-name
                  (get command' protocol/operation-key)))
      (choreo-error
       :operation-mismatch
       "Optimistic command operation does not match this choreography's public authoritative operation."
       {:expected-operation operation'
        :expected-wire-name (protocol/qualified-name operation')
        :actual-operation (get command' protocol/operation-key)}))
    command'))

(defn provisional-value
  "Validate command/provisional correlation and return the closed provisional
   value stored as one browser-local semantic fact.

   Choreo still requires the command's individual identity/basis facts before
   the derive-provisional action may run.  The nested provisional envelope is a
   convenient portable value for the later resolution action, not a substitute
   for those explicit knowledge requirements."
  [command-envelope provisional-envelope]
  (:provisional
   (protocol/command-provisional-pair
    command-envelope
    provisional-envelope)))

(defn settlement-value
  "Validate and return one closed protocol-v3 settlement value."
  [settlement-envelope]
  (protocol/settlement
   (dissoc settlement-envelope protocol/protocol-version-key)))

(defn settlement-message-values
  "Validate one settlement and expose the exact semantic message payload used
   by the authority projection.

   The top-level identity copies are deliberate: projected-machine correlation
   remains explicit rather than requiring Choreo to understand nested protocol
   envelope structure."
  [settlement-envelope]
  (let [settlement' (settlement-value settlement-envelope)]
    {protocol/command-id-key
     (get settlement' protocol/command-id-key)

     protocol/execution-id-key
     (get settlement' protocol/execution-id-key)

     settlement-value-key
     settlement'}))

(defn settlement-resolution
  "Validate a provisional/settlement pair and return the generic direct
   settlement resolution for the browser-local resolve action."
  [provisional-envelope settlement-envelope]
  (let [provisional'
        (protocol/provisional
         (dissoc provisional-envelope
                 protocol/protocol-version-key
                 protocol/authority-key))

        settlement'
        (settlement-value settlement-envelope)]
    (doseq [key [protocol/command-id-key
                 protocol/execution-id-key]]
      (when-not (= (get provisional' key)
                   (get settlement' key))
        (choreo-error
         :settlement-correlation-mismatch
         "Optimistic settlement does not correlate with the provisional trajectory."
         {:key key
          :provisional (get provisional' key)
          :settlement (get settlement' key)})))
    (get settlement' protocol/resolution-key)))

;; =============================================================================
;; Direct command choreography
;; =============================================================================

(defn command-choreography
  "Build one operation-specific direct optimistic command choreography.

   Required options:

     :name
       Semantic choreography name.

     :operation
       Public authoritative model operation, for example :request/claim.  This
       becomes the actual Choreo :authoritative operation identity.

   Optional role/action names exist for embedding into applications with a more
   specific vocabulary; defaults remain generic and portable.

   The browser must start with semantic-command-required-keys as input knowledge
   (plus any optional command fields it intends to send).  Its first local
   action must return exactly {provisional-value-key <protocol provisional>}.

   The trusted authority action receives the communicated command facts and must
   return exactly {settlement-value-key <protocol settlement>}.  The browser's
   final local action validates provisional/settlement correlation and returns
   exactly {resolution-value-key <generic resolution>}.

   No DOM/snapshot/timer/continuity resource appears in this graph."
  [options]
  (let [{:keys [name
                browser-role
                authority-role
                operation
                derive-provisional-action
                resolve-settlement-action]}
        (normalize-command-options options)

        terminal-state
        (fn [resolution]
          (keyword
           "gesso.live.optimistic.terminal"
           (clojure.core/name resolution)))

        resolution-cases
        (into {}
              (map (fn [resolution]
                     [resolution (terminal-state resolution)]))
              (sort-by clojure.core/name direct-terminal-resolutions))

        terminal-states
        (into {}
              (map (fn [resolution]
                     [(terminal-state resolution)
                      (choreo/return resolution)]))
              (sort-by clojure.core/name direct-terminal-resolutions))]

    (choreo/->choreography
     {:name name
      :initial :gesso.live.optimistic/derive-provisional
      :states
      (merge
       {:gesso.live.optimistic/derive-provisional
        (choreo/local
         browser-role
         derive-provisional-action
         :gesso.live.optimistic/send-command
         {:requires semantic-command-required-keys
          :outputs #{provisional-value-key}})

        :gesso.live.optimistic/send-command
        (choreo/communicate
         browser-role
         authority-role
         command-event
         :gesso.live.optimistic/execute-authoritative
         {:via :http
          :required semantic-command-required-keys
          :optional semantic-command-optional-keys
          :correlation command-correlation-keys})

        :gesso.live.optimistic/execute-authoritative
        (choreo/authoritative
         authority-role
         operation
         :gesso.live.optimistic/send-settlement
         {:requires semantic-command-required-keys
          :outputs #{settlement-value-key}})

        :gesso.live.optimistic/send-settlement
        (choreo/communicate
         authority-role
         browser-role
         settlement-event
         :gesso.live.optimistic/resolve-settlement
         {:via :http
          :required settlement-message-required-keys
          :correlation command-correlation-keys})

        :gesso.live.optimistic/resolve-settlement
        (choreo/local
         browser-role
         resolve-settlement-action
         :gesso.live.optimistic/branch-resolution
         {:requires #{provisional-value-key
                      settlement-value-key}
          :outputs #{resolution-value-key}})

        :gesso.live.optimistic/branch-resolution
        (choreo/branch
         browser-role
         resolution-value-key
         resolution-cases)}
       terminal-states)})))

(defn command-entry-knowledge
  "Return the precise verifier entry-knowledge assumptions for a direct command
   choreography built with options.  Only the browser initially knows the
   semantic command facts; the authority learns them through communication."
  [options]
  (let [{:keys [browser-role]}
        (normalize-command-options options)]
    {browser-role semantic-command-required-keys}))

#?(:clj
   (defn verified-command
     "Build and verify one operation-specific direct optimistic choreography."
     [options]
     (verify/verify!
      (command-choreography options)
      {:entry-knowledge
       (command-entry-knowledge options)})))

#?(:clj
   (defn command-plans
     "Return role -> canonical ExecutablePlan for one verified direct optimistic
      choreography."
     [options]
     (project/project-all
      (verified-command options))))

#?(:clj
   (defn command-plan
     "Return one canonical role-local ExecutablePlan for a direct optimistic
      choreography."
     [options role]
     (let [role' (require-keyword! "Projected optimistic role" role)
           plans (command-plans options)]
       (or (get plans role')
           (choreo-error
            :unknown-role
            "Requested role is not present in optimistic choreography."
            {:role role'
             :roles (set (keys plans))})))))

;; =============================================================================
;; Authoritative-reread supersession recovery
;; =============================================================================

(def ^:private supersession-option-keys
  #{:name
    :browser-role
    :event
    :authority
    :observation
    :resolve-supersession-action})

(defn- normalize-supersession-options
  [options]
  (let [options'
        (require-closed-options!
         "Optimistic supersession choreography options"
         options
         supersession-option-keys)

        {:keys [name
                browser-role
                event
                authority
                observation
                resolve-supersession-action]
         :or {browser-role default-browser-role
              event default-authoritative-observed-event
              resolve-supersession-action resolve-supersession-action}}
        options']
    {:name (require-keyword! "Supersession choreography :name" name)
     :browser-role (require-keyword! "Supersession choreography :browser-role" browser-role)
     :event (require-keyword! "Supersession choreography :event" event)
     :authority (require-keyword! "Supersession choreography :authority" authority)
     :observation (require-keyword! "Supersession choreography :observation" observation)
     :resolve-supersession-action
     (require-keyword!
      "Supersession choreography :resolve-supersession-action"
      resolve-supersession-action)}))

(defn supersession-choreography
  "Build one short browser recovery choreography for authoritative reread.

   This choreography is intentionally separate from command-choreography.  A
   browser that lost the direct settlement may later obtain current authority
   through Live/refetch and resolve the old provisional trajectory without a
   durable suspended server execution.

   The trusted browser adapter supplies event data:

     reread-authoritative-key  closed protocol-v3 authoritative observation
     reread-basis-key          the same opaque authoritative basis

   Choreo records those declared fields with authoritative-observation
   provenance under the configured authority/observation scope.  The adapter
   must validate that the nested protocol observation carries the same basis;
   merely dispatching a browser event cannot manufacture authority."
  [options]
  (let [{:keys [name
                browser-role
                event
                authority
                observation
                resolve-supersession-action]}
        (normalize-supersession-options options)]
    (choreo/->choreography
     {:name name
      :initial :gesso.live.optimistic/await-authoritative-reread
      :states
      {:gesso.live.optimistic/await-authoritative-reread
       (choreo/await
        browser-role
        {event :gesso.live.optimistic/resolve-supersession}
        {:event-contracts
         {event
          {:required #{reread-authoritative-key
                       reread-basis-key}
           :authoritative-observation
           {:authority authority
            :observation observation
            :basis-key reread-basis-key}}}})

       :gesso.live.optimistic/resolve-supersession
       (choreo/local
        browser-role
        resolve-supersession-action
        :gesso.live.optimistic/branch-supersession
        {:requires #{provisional-value-key
                     reread-authoritative-key
                     reread-basis-key}
         :outputs #{resolution-value-key}})

       :gesso.live.optimistic/branch-supersession
       (choreo/branch
        browser-role
        resolution-value-key
        {:superseded :gesso.live.optimistic/return-superseded})

       :gesso.live.optimistic/return-superseded
       (choreo/return :superseded)}})))

(defn supersession-entry-knowledge
  "Return verifier entry knowledge for supersession recovery.  The browser
   starts with the existing provisional value; the authoritative reread facts
   are acquired only through the declared trusted observation event."
  [options]
  (let [{:keys [browser-role]}
        (normalize-supersession-options options)]
    {browser-role #{provisional-value-key}}))

(defn authoritative-reread-data
  "Validate one protocol-v3 authoritative observation and return the exact
   semantic event data expected by supersession-choreography.

   The basis is duplicated at the top level solely because Choreo's
   authoritative-observation proof machinery needs an explicit semantic
   :basis-key.  This helper guarantees the duplicate equals the nested protocol
   observation basis."
  [authoritative-observation]
  (let [authoritative'
        (protocol/authoritative
         (dissoc authoritative-observation protocol/authority-key))]
    {reread-authoritative-key authoritative'
     reread-basis-key (get authoritative' protocol/basis-key)}))

#?(:clj
   (defn verified-supersession
     "Build and verify one authoritative-reread supersession recovery."
     [options]
     (verify/verify!
      (supersession-choreography options)
      {:entry-knowledge
       (supersession-entry-knowledge options)})))

#?(:clj
   (defn supersession-plan
     "Return the browser ExecutablePlan for one verified supersession recovery."
     [options]
     (let [{:keys [browser-role]}
           (normalize-supersession-options options)]
       (project/project
        (verified-supersession options)
        browser-role))))

;; =============================================================================
;; Diagnostics
;; =============================================================================

#?(:clj
   (defn explain-command
     "Return compact compiler-facing diagnostics for a direct optimistic command
      choreography without placing diagnostics in runtime semantics."
     [options]
     (let [verified (verified-command options)
           plans (project/project-all verified)]
       {:choreography (choreo/explain (:choreography verified))
        :verification (verify/explain verified)
        :plans (into {}
                     (map (fn [[role plan]]
                            [role (project/explain plan)]))
                     plans)})))

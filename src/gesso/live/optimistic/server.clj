(ns gesso.live.optimistic.server
  "JVM/server support for Gesso Live optimistic commands.

   This namespace owns the server side of the Live optimistic feature:
   - optimistic descriptor construction and browser-facing Hiccup attrs
   - authoritative canonical marking
   - optimistic execution id extraction from Ring context
   - semantic settlement construction and rendering
   - execution of the projected server choreography endpoint
   - the adapter boundary to ordinary Biff FX machines

   Choreography owns distributed protocol order. Biff FX owns application-local
   command execution. This namespace connects those two without teaching either
   subsystem about the other.

   The browser/server protocol uses an opaque wire scope for correlation. Public
   rendering/settlement APIs continue to accept semantic application scopes;
   this namespace performs wire encoding exactly at the protocol boundary.

   It deliberately does not own:
   - application/domain transition policy
   - browser DOM or continuity behavior
   - XTDB transaction policy
   - consistency-token header transport
   - generic choreography execution semantics."
  (:require
   [clojure.string :as str]
   [gesso.choreo.machine :as machine]
   [gesso.live.htmx :as htmx]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol])
  (:import
   [java.util UUID]))

;; -----------------------------------------------------------------------------
;; Public identities
;; -----------------------------------------------------------------------------

(def protocol-version protocol/version)

(def descriptor-type
  :gesso.live.optimistic/descriptor)

(def settlement-type
  :gesso.live.optimistic/settlement)

(def prepared-server-send-type
  :gesso.live.optimistic.server/prepared-send)

(def default-template-prefix
  "gesso-optimistic-template-")

(def default-sync-strategy
  "drop")

(def default-projection-mode
  :provisional)

(def projection-modes
  protocol/projection-modes)

(def settlement-outcomes
  protocol/settlement-outcomes)

(def execution-request-header
  protocol/execution-header-name)

;; Public aliases keep JVM callers from spelling protocol attrs themselves while
;; the actual vocabulary remains owned by optimistic.protocol.
(def protocol-attr protocol/protocol-attr)
(def transition-attr protocol/transition-attr)
(def template-attr protocol/template-attr)
(def target-attr protocol/target-attr)
(def scope-attr protocol/scope-attr)
(def base-revision-attr protocol/base-revision-attr)
(def revision-attr protocol/revision-attr)
(def pending-label-attr protocol/pending-label-attr)
(def projection-mode-attr protocol/projection-mode-attr)
(def settlement-attr protocol/settlement-attr)
(def execution-attr protocol/execution-attr)
(def outcome-attr protocol/outcome-attr)
(def command-applied-attr protocol/command-applied-attr)
(def reason-attr protocol/reason-attr)
(def canonical-attr protocol/canonical-attr)

;; -----------------------------------------------------------------------------
;; Validation helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [x]
  (and (string? x)
       (str/blank? x)))

(defn- present?
  [x]
  (not (or (nil? x)
           (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex (str "gesso.live optimistic config requires " k ".")
         {k value})))
  value)

(defn- require-non-blank-string!
  [k value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (ex (str "gesso.live optimistic " k
              " must be a non-blank string.")
         {k value})))
  value)

(defn- require-map!
  [k value]
  (when-not (map? value)
    (throw
     (ex (str "gesso.live optimistic " k " must be a map.")
         {k value})))
  value)

(defn- require-callable!
  [k value]
  (when-not (fn? value)
    (throw
     (ex (str "gesso.live optimistic " k " must be callable.")
         {k value})))
  value)

(defn- normalize-semantic-scope
  "Validate semantic scope without converting stored server-side values to wire
   representation."
  [scope]
  (protocol/wire-scope scope)
  scope)

(defn- normalize-transition
  [transition]
  (protocol/normalize-name :transition transition))

(defn- normalize-pending-label
  [value]
  (when (some? value)
    (require-non-blank-string! :pending-label value)))

(defn- normalize-sync
  [sync]
  (cond
    (or (nil? sync)
        (false? sync))
    nil

    (and (string? sync)
         (not (str/blank? sync)))
    sync

    :else
    (throw
     (ex "gesso.live optimistic :sync must be nil, false, or a non-blank string."
         {:sync sync}))))

(defn- hiccup-tag?
  [x]
  (or (keyword? x)
      (symbol? x)
      (string? x)))

(defn- fragment-tag?
  [tag]
  (contains? #{:<> '<> "<>"} tag))

(defn- single-root-hiccup?
  [content]
  (and (vector? content)
       (hiccup-tag? (first content))
       (not (fragment-tag? (first content)))))

(defn- require-single-root!
  [k content]
  (when-not (single-root-hiccup? content)
    (throw
     (ex (str "gesso.live optimistic " k
              " must be one rooted Hiccup element and may not be a fragment or sequence.")
         {k content})))
  content)

(defn- remove-protocol-attrs
  [attrs]
  (apply dissoc attrs protocol/reserved-attrs))

(defn- hiccup-parts
  "Return [tag attrs children] for one rooted Hiccup element."
  [node]
  (require-single-root! :node node)
  (let [[tag second-item & rest-items] node]
    (if (map? second-item)
      [tag second-item rest-items]
      [tag
       {}
       (if (nil? second-item)
         rest-items
         (cons second-item rest-items))])))

;; -----------------------------------------------------------------------------
;; Optimistic descriptor construction
;; -----------------------------------------------------------------------------

(defn new-template-name
  "Return a fresh browser-safe optimistic template name."
  []
  (str default-template-prefix (UUID/randomUUID)))

(defn target-sync
  "Derive the default target-scoped HTMX single-flight synchronization value."
  ([target]
   (target-sync target default-sync-strategy))
  ([target strategy]
   (let [target' (->> target
                      (require-non-blank-string! :target)
                      htmx/normalize-target)
         strategy' (protocol/normalize-name :strategy strategy)]
     (str target' ":" strategy'))))

(defn ->optimistic
  "Prepare one optimistic transition descriptor.

   Required:
     :transition  semantic transition identifier
     :scope       stable semantic application scope
     :target      selector/id for the existing target element
     :content     exactly one rooted optimistic Hiccup projection

   Optional:
     :base-revision
     :projection-mode  :pending, :provisional, or :full
     :template-name
     :pending-label
     :sync             nil/false disables suggested hx-sync
     :attrs
     :template-attrs

   Framework protocol attrs always override caller-supplied conflicts."
  [{:keys [transition
           target
           scope
           base-revision
           content
           projection-mode
           template-name
           pending-label
           attrs
           template-attrs]
    :as opts}]
  (let [transition' (normalize-transition transition)
        target' (->> target
                     (require-non-blank-string! :target)
                     htmx/normalize-target)
        scope' (normalize-semantic-scope scope)
        base-revision' (protocol/normalize-revision
                        :base-revision
                        base-revision)
        content' (->> content
                      (require-present! :content)
                      (require-single-root! :content))
        projection-mode' (protocol/normalize-projection-mode
                          projection-mode)
        template-name' (or (protocol/normalize-optional-name
                            :template-name
                            template-name)
                           (new-template-name))
        pending-label' (normalize-pending-label pending-label)
        attrs' (require-map! :attrs (or attrs {}))
        template-attrs' (require-map! :template-attrs
                                     (or template-attrs {}))
        sync' (if (contains? opts :sync)
                (normalize-sync (:sync opts))
                (target-sync target'))]
    {:gesso.live.optimistic/type descriptor-type
     :protocol-version protocol-version
     :transition transition'
     :scope scope'
     :base-revision base-revision'
     :target target'
     :content content'
     :projection-mode projection-mode'
     :template-name template-name'
     :pending-label pending-label'
     :sync sync'
     :attrs attrs'
     :template-attrs template-attrs'}))

(defn optimistic?
  [x]
  (and (map? x)
       (= descriptor-type
          (:gesso.live.optimistic/type x))
       (= protocol-version
          (:protocol-version x))))

(defn ensure-optimistic
  "Return a prepared descriptor unchanged, or normalize raw opts."
  [optimistic]
  (if (optimistic? optimistic)
    optimistic
    (->optimistic optimistic)))

(defn- require-descriptor!
  [optimistic]
  (when-not (optimistic? optimistic)
    (throw
     (ex (str "gesso.live optimistic markup helpers require a prepared "
              "descriptor. Call ->optimistic once, or use render-parts with raw opts.")
         {:optimistic optimistic})))
  optimistic)

;; -----------------------------------------------------------------------------
;; Canonical authority
;; -----------------------------------------------------------------------------

(defn canonical-attrs
  "Return attrs explicitly marking authoritative server-rendered content.

   :scope is required; :revision is optional. A matching scope by itself never
   implies canonical authority."
  [{:keys [scope revision]}]
  (let [scope' (normalize-semantic-scope scope)
        revision' (protocol/normalize-revision :revision revision)]
    (htmx/clean-attrs
     {protocol-attr protocol-version
      scope-attr (protocol/wire-scope scope')
      revision-attr (protocol/revision->wire revision')
      canonical-attr "true"})))

(defn canonical
  "Mark one rooted Hiccup element authoritative for semantic scope/revision."
  [opts node]
  (let [node' (require-single-root! :canonical-content node)
        [tag attrs children] (hiccup-parts node')
        attrs' (merge (remove-protocol-attrs attrs)
                      (canonical-attrs opts))]
    (into [tag attrs'] children)))

;; -----------------------------------------------------------------------------
;; Browser-facing optimistic markup
;; -----------------------------------------------------------------------------

(defn source-attrs
  "Build optimistic protocol attrs for the actual HTMX request owner."
  [optimistic]
  (let [{:keys [transition
                template-name
                target
                scope
                base-revision
                pending-label
                projection-mode
                attrs]}
        (require-descriptor! optimistic)
        attrs' (remove-protocol-attrs attrs)]
    (htmx/merge-attrs
     attrs'
     {protocol-attr protocol-version
      transition-attr transition
      template-attr template-name
      target-attr target
      scope-attr (protocol/wire-scope scope)
      projection-mode-attr
      (protocol/projection-mode->wire projection-mode)}
     (when (some? base-revision)
       {base-revision-attr
        (protocol/revision->wire base-revision)})
     (when pending-label
       {pending-label-attr pending-label}))))

(defn template
  "Render the hidden one-root optimistic projection template.

   Projection markup carries identity/scope diagnostics but never canonical
   authority."
  [optimistic]
  (let [{:keys [transition
                template-name
                scope
                projection-mode
                content
                template-attrs]}
        (require-descriptor! optimistic)
        template-attrs' (remove-protocol-attrs template-attrs)]
    [:template
     (htmx/clean-attrs
      (merge
       template-attrs'
       {protocol-attr protocol-version
        transition-attr transition
        template-attr template-name
        scope-attr (protocol/wire-scope scope)
        projection-mode-attr
        (protocol/projection-mode->wire projection-mode)}))
     content]))

(defn render-parts
  "Return the prepared pieces consumed by higher-level HTMX UI helpers."
  [optimistic]
  (let [optimistic' (ensure-optimistic optimistic)]
    {:optimistic optimistic'
     :source-attrs (source-attrs optimistic')
     :template (template optimistic')
     :sync (:sync optimistic')}))

;; -----------------------------------------------------------------------------
;; Request execution identity
;; -----------------------------------------------------------------------------

(defn request-execution-id
  "Return the browser-generated optimistic execution id, when present.

   Normal Ring request header keys are lower-case. Explicit context injection is
   retained for tests and custom adapters."
  [ctx]
  (or (:gesso.live.optimistic/execution-id ctx)
      (get-in ctx [:headers execution-request-header])
      (get-in ctx [:headers "Gesso-Optimistic-Execution"])
      (get-in ctx [:request :headers execution-request-header])
      (get-in ctx [:request :headers "Gesso-Optimistic-Execution"])))

(defn require-request-execution-id
  "Return request-execution-id or throw when an optimistic command lacks one."
  [ctx]
  (require-non-blank-string!
   :execution-id
   (request-execution-id ctx)))

;; -----------------------------------------------------------------------------
;; Semantic settlement
;; -----------------------------------------------------------------------------

(defn ->settlement
  "Prepare one semantic optimistic settlement.

   Required:
     :execution-id
     :scope          semantic application scope
     :outcome        :confirmed, :reconciled, :rejected, or :failed

   Optional:
     :revision
     :reason
     :consistency-token

   :command-applied? is derived from outcome and may not be supplied.
   :transition is intentionally absent from settlement correlation."
  [{:keys [execution-id
           scope
           outcome
           revision
           reason
           consistency-token]
    :as opts}]
  (when (contains? opts :transition)
    (throw
     (ex "gesso.live optimistic settlement does not accept redundant :transition."
         {:transition (:transition opts)})))
  (when (contains? opts :command-applied?)
    (throw
     (ex (str "gesso.live optimistic settlement does not accept "
              ":command-applied?; it is derived from :outcome.")
         {:command-applied? (:command-applied? opts)
          :outcome outcome})))
  (let [execution-id' (require-non-blank-string!
                       :execution-id
                       execution-id)
        scope' (normalize-semantic-scope scope)
        outcome' (protocol/normalize-settlement-outcome outcome)
        revision' (protocol/normalize-revision :revision revision)
        reason' (protocol/normalize-optional-name :reason reason)]
    {:gesso.live.optimistic/type settlement-type
     :protocol-version protocol-version
     :execution-id execution-id'
     :scope scope'
     :outcome outcome'
     :revision revision'
     :command-applied?
     (protocol/command-applied-for-outcome? outcome')
     :reason reason'
     :consistency-token consistency-token}))

(defn settlement?
  [x]
  (and (map? x)
       (= settlement-type
          (:gesso.live.optimistic/type x))
       (= protocol-version
          (:protocol-version x))))

(defn ensure-settlement
  "Return a prepared settlement unchanged, or normalize raw opts."
  [settlement]
  (if (settlement? settlement)
    settlement
    (->settlement settlement)))

(defn settlement-for-request
  "Prepare settlement using the browser execution id carried by ctx."
  [ctx opts]
  (->settlement
   (assoc opts
          :execution-id
          (or (:execution-id opts)
              (request-execution-id ctx)))))

(defn settlement-marker
  "Render the inert browser-readable settlement marker.

   Consistency tokens are intentionally not serialized into this marker; their
   HTTP transport belongs to gesso.live.token."
  [settlement]
  (let [{:keys [execution-id
                scope
                outcome
                revision
                command-applied?
                reason]}
        (ensure-settlement settlement)]
    [:template
     (htmx/clean-attrs
      {protocol-attr protocol-version
       settlement-attr "true"
       execution-attr execution-id
       scope-attr (protocol/wire-scope scope)
       outcome-attr (protocol/settlement-outcome->wire outcome)
       revision-attr (protocol/revision->wire revision)
       command-applied-attr
       (protocol/command-applied->wire command-applied?)
       reason-attr reason})]))

(defn with-settlement
  "Return settlement marker + one authoritative canonical root + extra nodes.

   The canonical root derives its semantic scope/revision from the same prepared
   settlement, so marker and canonical authority cannot disagree."
  [settlement canonical-node & nodes]
  (let [settlement' (ensure-settlement settlement)
        canonical-node'
        (canonical
         {:scope (:scope settlement')
          :revision (:revision settlement')}
         (->> canonical-node
              (require-present! :canonical-content)
              (require-single-root! :canonical-content)))]
    (into
     [:div {:style {:display "contents"}}]
     (concat
      [(settlement-marker settlement')
       canonical-node']
      (remove nil? nodes)))))

;; -----------------------------------------------------------------------------
;; Projected server choreography adapter
;; -----------------------------------------------------------------------------

(defn command-payload
  "Construct the protocol command payload for a server request.

   Public :scope is semantic application data. The returned :scope is the same
   opaque wire identity the browser sends and uses for correlation.

   execution-id defaults from ctx's optimistic request header. transition and
   scope must be supplied by the authoritative route/application operation; the
   server never trusts DOM metadata that is not actually present in the request."
  [ctx {:keys [execution-id
               transition
               scope
               base-revision
               consistency-token]}]
  (let [execution-id' (require-non-blank-string!
                       :execution-id
                       (or execution-id
                           (request-execution-id ctx)))
        transition' (normalize-transition transition)
        semantic-scope (normalize-semantic-scope scope)
        base-revision' (protocol/normalize-revision
                        :base-revision
                        base-revision)]
    (cond->
        {protocol/execution-id-key execution-id'
         protocol/transition-key transition'
         protocol/scope-key (protocol/wire-scope semantic-scope)}
      (some? base-revision')
      (assoc protocol/base-revision-key base-revision')

      (some? consistency-token)
      (assoc protocol/consistency-token-key consistency-token))))

(defn- command-envelope
  [payload]
  (machine/message
   {:from optimistic-choreo/browser-role
    :to optimistic-choreo/server-role
    :event protocol/command-event
    :via :http}
   payload))

(defn begin-command
  "Start the projected server endpoint and deliver one HTTP command into it.

   Returns a choreography execution waiting on the built-in server FX machine.
   The server context is seeded only with the known correlation identity before
   the incoming command is matched."
  [ctx command]
  (let [payload (command-payload ctx command)
        execution-id (get payload protocol/execution-id-key)
        seed-context (select-keys payload
                                  protocol/command-correlation-keys)
        execution0 (machine/start
                    optimistic-choreo/server-plan
                    {:execution-id execution-id
                     :context seed-context})
        execution1 (machine/resume
                    execution0
                    (command-envelope payload))
        action (machine/pending-action execution1)]
    (when-not (machine/waiting-fx? execution1)
      (throw
       (ex "Projected optimistic server endpoint did not reach its FX boundary."
           {:execution execution1})))
    (when-not (= optimistic-choreo/server-execute-machine
                 (:machine action))
      (throw
       (ex "Projected optimistic server endpoint requested an unexpected FX machine."
           {:expected optimistic-choreo/server-execute-machine
            :action action})))
    execution1))

(defn command-context
  "Return the accumulated protocol context for a server execution waiting on FX."
  [execution]
  (when-not (machine/waiting-fx? execution)
    (throw
     (ex "Optimistic server execution is not waiting for command FX."
         {:execution execution})))
  (machine/execution-context execution))

(defn biff-fx-context
  "Merge ordinary server ctx with protocol context for the application Biff FX
   machine.

   The received command is additionally available at
   :gesso.live.optimistic/command. Protocol keys also remain directly available
   in the merged map for state functions that need them."
  [ctx execution]
  (let [protocol-context (command-context execution)]
    (merge ctx
           protocol-context
           {:gesso.live.optimistic/command
            (:command protocol-context)
            :gesso.live.optimistic/execution-id
            (:execution-id execution)})))

(defn run-biff-fx
  "Run the supplied ordinary Biff FX machine at the server choreography FX
   boundary.

   fx-machine is expected to be the callable produced by com.biffweb.fx/machine
   or com.biffweb.fx/defmachine. Its terminal value is returned unchanged."
  [ctx execution fx-machine]
  (require-callable! :fx-machine fx-machine)
  (fx-machine (biff-fx-context ctx execution)))

(defn- settlement-protocol-result
  [execution settlement canonical-node]
  (let [settlement' (ensure-settlement settlement)
        execution-id (:execution-id execution)
        protocol-context (machine/execution-context execution)
        command-wire-scope (get protocol-context protocol/scope-key)
        settlement-wire-scope (protocol/wire-scope (:scope settlement'))]
    (when-not (= execution-id (:execution-id settlement'))
      (throw
       (ex "Optimistic settlement execution id does not match the server choreography execution."
           {:execution-id execution-id
            :settlement-execution-id (:execution-id settlement')})))
    (when-not (= command-wire-scope settlement-wire-scope)
      (throw
       (ex "Optimistic settlement scope does not match the received command scope."
           {:command-scope command-wire-scope
            :settlement-scope settlement-wire-scope})))
    (require-single-root! :canonical-content canonical-node)
    (cond->
        {protocol/execution-id-key execution-id
         protocol/scope-key command-wire-scope
         protocol/outcome-key (:outcome settlement')
         protocol/command-applied-key (:command-applied? settlement')
         protocol/canonical-key canonical-node}
      (some? (:revision settlement'))
      (assoc protocol/revision-key (:revision settlement'))

      (some? (:reason settlement'))
      (assoc protocol/reason-key (:reason settlement'))

      (some? (:consistency-token settlement'))
      (assoc protocol/consistency-token-key
             (:consistency-token settlement')))))

(defn prepare-settlement-send
  "Complete the server FX boundary with one authoritative settlement.

   Returns a prepared value containing:
     :execution   choreography execution waiting at its server->browser send
     :action      exact projected send action/payload
     :settlement  normalized semantic settlement
     :canonical   raw authoritative Hiccup root

   The send payload is checked against the semantic settlement so execution-id,
   scope, outcome, command-applied?, revision, reason, canonical content, and
   optional consistency token all come from one construction path."
  [execution settlement canonical-node]
  (when-not (machine/waiting-fx? execution)
    (throw
     (ex "Optimistic server execution is not waiting for FX completion."
         {:execution execution})))
  (let [settlement' (ensure-settlement settlement)
        protocol-result (settlement-protocol-result
                         execution
                         settlement'
                         canonical-node)
        execution' (machine/complete-fx execution protocol-result)
        action (machine/pending-action execution')]
    (when-not (machine/waiting-send? execution')
      (throw
       (ex "Projected optimistic server endpoint did not reach its settlement send boundary."
           {:execution execution'})))
    (when-not (and (= :send (:kind action))
                   (= optimistic-choreo/browser-role (:to action))
                   (= protocol/settlement-event (:event action))
                   (= :http (:via action)))
      (throw
       (ex "Projected optimistic server endpoint produced an unexpected settlement send."
           {:action action})))
    {:gesso.live.optimistic.server/type prepared-server-send-type
     :execution execution'
     :action action
     :settlement settlement'
     :canonical canonical-node}))

(defn prepared-server-send?
  [x]
  (and (map? x)
       (= prepared-server-send-type
          (:gesso.live.optimistic.server/type x))
       (machine/waiting-send? (:execution x))))

(defn complete-settlement-send
  "Mark the projected server send boundary complete.

   Call this when the adapter considers the HTTP settlement response handed off.
   The built-in server projection should then be terminal."
  [prepared]
  (when-not (prepared-server-send? prepared)
    (throw
     (ex "Expected a prepared optimistic server settlement send."
         {:prepared prepared})))
  (let [execution' (machine/complete-send (:execution prepared))]
    (when-not (machine/completed? execution')
      (throw
       (ex "Optimistic server endpoint did not terminate after settlement send."
           {:execution execution'})))
    (assoc prepared :execution execution')))

(defn prepared-response-hiccup
  "Render marker + canonical root for a prepared server send.

   Optional additional nodes are appended after the authoritative root."
  [prepared & nodes]
  (when-not (prepared-server-send? prepared)
    (throw
     (ex "Expected a prepared optimistic server settlement send."
         {:prepared prepared})))
  (apply with-settlement
         (:settlement prepared)
         (:canonical prepared)
         nodes))

(defn run-command
  "Run one optimistic command through the projected server endpoint and Biff FX.

   Arguments:
     ctx      ordinary Ring/Biff context
     command  map accepted by command-payload
     opts
       :fx-machine  required Biff FX machine function
       :settle   required (fn [biff-fx-ctx fx-result] ...)

   settle must return a map containing:
     :outcome    semantic settlement outcome
     :canonical  exactly one rooted authoritative Hiccup element

   It may also return:
     :revision
     :reason
     :consistency-token

   The settlement's execution-id and semantic scope are derived from the command
   boundary, not trusted from settle. The returned value is a prepared server
   send; callers can render it with prepared-response-hiccup and then call
   complete-settlement-send when the response has been handed off."
  [ctx command {:keys [fx-machine settle]}]
  (require-callable! :fx-machine fx-machine)
  (require-callable! :settle settle)
  (let [execution (begin-command ctx command)
        fx-ctx (biff-fx-context ctx execution)
        fx-result (fx-machine fx-ctx)
        settlement-result (settle fx-ctx fx-result)
        _ (require-map! :settle-result settlement-result)
        canonical-node (require-present!
                        :canonical
                        (:canonical settlement-result))
        semantic-scope (normalize-semantic-scope (:scope command))
        settlement (->settlement
                    (merge
                     (select-keys settlement-result
                                  [:outcome
                                   :revision
                                   :reason
                                   :consistency-token])
                     {:execution-id (:execution-id execution)
                      :scope semantic-scope}))]
    (prepare-settlement-send
     execution
     settlement
     canonical-node)))

(ns gesso.live.browser.optimistic-htmx
  "Protocol-v3 HTMX transport bridge for browser optimism.

   This namespace connects four already-owned boundaries without becoming a
   second semantic runtime:

     server-rendered optimistic action annotation
       -> documented HTMX lifecycle observed through browser.core
       -> browser.optimistic / portable Choreo execution
       -> the same HTMX request carries the protocol-v3 command

   It deliberately does not install document listeners. browser.core remains the
   one owner of HTMX/document listener registration and calls this bridge through
   its event-observer seam.

   It deliberately does not own optimistic target/timeout/settlement semantics.
   Those remain in browser.adapter and browser.optimistic. The mutable values in
   this namespace are only ephemeral physical HTMX correlations: source element,
   XHR, and the adapter-issued execution reference associated with one request.
   None enters AdapterState or portable machine knowledge.

   HTMX integration order is intentional:

     htmx:configRequest
       - parse the server-rendered action annotation;
       - allocate command-id + execution-id;
       - construct/validate the protocol-v3 command;
       - inject the encoded command into HTMX's already-collected parameters.

     htmx:beforeRequest
       - start browser.optimistic while the HTMX request is still cancellable;
       - require the projected execution to reach its :transport/send boundary
         synchronously. This means the HTMX bridge currently requires a
         synchronous provisional semantic derivation even though the generic
         browser.optimistic runtime can support an asynchronous projector;
       - leave the Choreo transport effect pending on a Promise.

     htmx:beforeSend
       - resolve that Promise. This is the physical send handoff: HTMX owns the
         actual XMLHttpRequest from here onward.

     htmx:afterRequest / failure events
       - correlate by XHR, not mutable DOM position;
       - deliver a trusted decoded settlement when the response carries one;
       - otherwise leave the execution pending for timeout or later Live
         authoritative supersession;
       - classify transport/network uncertainty through the adapter-owned
         optimistic retirement path without manufacturing a :failed settlement.

   The default source annotation and command transport use EDN strings because
   the semantic protocol already requires portable Clojure/ClojureScript data.
   Applications may replace action parsing, command encoding, plan lookup, and
   settlement extraction explicitly. Shape decoding never grants authority: the
   server must still authenticate, resolve a trusted operation, re-authorize,
   reread authority, and invoke the public model operation."
  (:require
   [cljs.reader :as reader]
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.choreo.identity :as identity]
   [gesso.live.browser.choreo :as browser-choreo]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.optimistic.protocol :as protocol]))

;; =============================================================================
;; Public identity / transport vocabulary
;; =============================================================================

(def runtime-version 1)

(def runtime-type
  :gesso.live.browser.optimistic-htmx/runtime)

(def action-attribute
  "data-gesso-live-optimistic")

(def settlement-attribute
  "data-gesso-live-optimistic-settlement")

(def command-parameter
  "__gesso_live_optimistic_command")

(def observer-id
  :gesso.live.browser.optimistic-htmx/observer)

(def observed-events
  ["htmx:configRequest"
   "htmx:beforeRequest"
   "htmx:beforeSend"
   "htmx:afterRequest"
   "htmx:responseError"
   "htmx:sendError"
   "htmx:timeout"
   "htmx:abort"])

(def action-required-keys
  #{:operation
    :arguments
    :observed-basis})

(def action-optional-keys
  #{:scope
    :fact-versions
    :target-id
    :plan-key
    :rollback-eligible?
    :timeout-ms
    :replace-owner?
    :replace-execution?})

(def option-keys
  #{:action-attribute
    :command-parameter
    :read-action
    :encode-command
    :plan-for
    :settlement-from-event
    :command-id-fn
    :execution-id-fn
    :on-error})

;; =============================================================================
;; Errors / small validation helpers
;; =============================================================================

(defn- bridge-error
  ([kind message data]
   (bridge-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.optimistic-htmx/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (bridge-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-callable!
  [label value]
  (when-not (fn? value)
    (throw
     (bridge-error
      :invalid-callable
      (str label " must be callable.")
      {:label label
       :value value})))
  value)

(defn- require-optional-callable!
  [label value]
  (when (some? value)
    (require-callable! label value))
  value)

(defn- require-nonblank-string!
  [label value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (bridge-error
      :invalid-string
      (str label " must be a non-blank string.")
      {:label label
       :value value})))
  value)

(defn- check-option-keys!
  [options]
  (let [unknown (seq (remove option-keys (keys options)))]
    (when unknown
      (throw
       (bridge-error
        :unknown-options
        "Optimistic HTMX bridge options contain unsupported keys."
        {:unknown-keys (set unknown)
         :allowed-keys option-keys}))))
  options)

(defn- check-action-keys!
  [action]
  (require-map! "Optimistic HTMX action" action)
  (let [keys' (set (keys action))
        missing (set/difference action-required-keys keys')
        allowed (set/union action-required-keys action-optional-keys)
        unknown (set/difference keys' allowed)]
    (when (seq missing)
      (throw
       (bridge-error
        :missing-action-fields
        "Optimistic HTMX action is missing required fields."
        {:missing missing
         :required action-required-keys
         :action action})))
    (when (seq unknown)
      (throw
       (bridge-error
        :unknown-action-fields
        "Optimistic HTMX action contains unsupported fields."
        {:unknown unknown
         :allowed allowed
         :action action}))))
  action)

(defn- require-runtime!
  [runtime]
  (when-not
   (and (map? runtime)
        (= runtime-type
           (:gesso.live.browser.optimistic-htmx/type runtime))
        (= runtime-version
           (:gesso.live.browser.optimistic-htmx/version runtime))
        (core/core? (:core runtime))
        (optimistic/runtime? (:optimistic runtime))
        (identical?
         (core/shell-runtime (:core runtime))
         (optimistic/shell-runtime (:optimistic runtime))))
    (throw
     (bridge-error
      :invalid-runtime
      "Expected a Gesso Live optimistic HTMX bridge runtime."
      {:value runtime})))
  runtime)

;; =============================================================================
;; Portable EDN wire helpers
;; =============================================================================

(defn encode-command-edn
  "Encode one validated protocol-v3 command as portable EDN request data."
  [command]
  (pr-str
   (protocol/command->wire command)))

(defn decode-command-edn
  "Decode one EDN request parameter into a validated runtime command.

   This is shape/correlation decoding only. It does not authenticate or
   authorize the command."
  [encoded]
  (require-nonblank-string! "Encoded optimistic command" encoded)
  (protocol/wire->command
   (reader/read-string encoded)))

(defn encode-settlement-edn
  "Encode one trusted settlement as EDN suitable for an inert HTML marker."
  [settlement]
  (pr-str
   (protocol/settlement->wire settlement)))

(defn decode-settlement-edn
  "Decode one settlement-marker EDN value into a runtime settlement."
  [encoded]
  (require-nonblank-string! "Encoded optimistic settlement" encoded)
  (protocol/wire->settlement
   (reader/read-string encoded)))

;; =============================================================================
;; Browser event / host helpers
;; =============================================================================

(defn- event-source
  [event]
  (core/detail-field event "elt"))

(defn- source-with-action
  [runtime event]
  (let [attribute (:action-attribute runtime)
        source (event-source event)]
    (or
     (when (and source
                (dom/has-attr? source attribute))
       source)
     (dom/closest-with-attr source attribute))))

(defn- event-target-id
  [event]
  (let [target (core/detail-field event "target")
        id (when target (dom/attr target "id"))]
    (when (and (string? id)
               (not (str/blank? id)))
      id)))

(defn- event-xhr
  [event]
  (core/xhr-from-event event))

(defn- weak-get
  [weak-map key]
  (when (and weak-map key)
    (.get weak-map key)))

(defn- weak-set!
  [weak-map key value]
  (when (and weak-map key)
    (.set weak-map key value))
  value)

(defn- weak-delete!
  [weak-map key]
  (when (and weak-map key)
    (.delete weak-map key))
  true)

(defn- prevent-request!
  [event]
  (core/prevent-event! event))

(defn- abort-xhr!
  [xhr]
  (when xhr
    (let [abort (.-abort xhr)]
      (when (fn? abort)
        (.call abort xhr))))
  true)

(defn- set-parameter!
  [parameters name value]
  (when-not parameters
    (throw
     (bridge-error
      :missing-parameters
      "htmx:configRequest did not expose a parameters object."
      {:parameter name})))
  (let [set-fn (.-set parameters)]
    (if (fn? set-fn)
      (.call set-fn parameters name value)
      (aset parameters name value)))
  parameters)

(defn- config-parameters
  [event]
  (core/detail-field event "parameters"))

(defn- response-text
  [event]
  (let [xhr (event-xhr event)
        text (when xhr (.-responseText xhr))]
    (when (string? text)
      text)))

(defn- error-name
  [error]
  (or (.-name error)
      "Error"))

(defn- error-message
  [error]
  (or (.-message error)
      (str error)))

(defn- report-error!
  [runtime phase error data]
  (when-let [observer (:on-error runtime)]
    (try
      (observer
       (merge
        {:phase phase
         :error-name (error-name error)
         :message (error-message error)}
        data))
      (catch :default _
        nil)))
  nil)

;; =============================================================================
;; Source action / command preparation
;; =============================================================================

(defn- default-read-action
  [runtime source]
  (let [encoded (dom/attr source (:action-attribute runtime))]
    (when (and (string? encoded)
               (not (str/blank? encoded)))
      (reader/read-string encoded))))

(defn- normalize-action
  [action]
  (let [action (check-action-keys! action)]
    (when (contains? action :target-id)
      (require-nonblank-string!
       "Optimistic action :target-id"
       (:target-id action)))
    action))

(defn- default-command-id
  []
  (identity/command-id
   (random-uuid)))

(defn- default-execution-id
  []
  (identity/execution-id
   (random-uuid)))

(defn- command-from-action
  [runtime action]
  ((fn [command]
     (protocol/command command))
   (cond->
    {:command-id ((:command-id-fn runtime))
     :execution-id ((:execution-id-fn runtime))
     :operation (:operation action)
     :arguments (:arguments action)
     :observed-basis (:observed-basis action)}
     (contains? action :scope)
     (assoc :scope (:scope action))

     (contains? action :fact-versions)
     (assoc :fact-versions (:fact-versions action)))))

(defn- optimistic-start-options
  [preflight]
  (let [action (:action preflight)]
    (cond->
     {:plan (:plan preflight)
      :command (:command preflight)
      :target-id (:target-id preflight)}
      (contains? action :rollback-eligible?)
      (assoc :rollback-eligible? (:rollback-eligible? action))

      (contains? action :timeout-ms)
      (assoc :timeout-ms (:timeout-ms action))

      (contains? action :replace-owner?)
      (assoc :replace-owner? (:replace-owner? action))

      (contains? action :replace-execution?)
      (assoc :replace-execution? (:replace-execution? action)))))

(defn- preflight-for
  [runtime source event]
  (let [action
        (normalize-action
         ((:read-action runtime) runtime source))
        command (command-from-action runtime action)
        target-id (or (:target-id action)
                      (event-target-id event))
        _ (require-nonblank-string!
           "Optimistic HTMX target id"
           target-id)
        plan
        ((:plan-for runtime)
         {:source source
          :action action
          :plan-key (or (:plan-key action)
                        (:operation action))
          :command command
          :target-id target-id})
        encoded-command ((:encode-command runtime) command)]
    (when (nil? plan)
      (throw
       (bridge-error
        :missing-plan
        "Optimistic HTMX plan resolver returned nil."
        {:operation (:operation action)
         :plan-key (or (:plan-key action)
                       (:operation action))})))
    (require-nonblank-string!
     "Encoded optimistic HTMX command"
     encoded-command)
    {:source source
     :action action
     :command command
     :plan plan
     :target-id target-id
     :encoded-command encoded-command}))

;; =============================================================================
;; Deferred transport handoff
;; =============================================================================

(defn- deferred
  []
  (let [resolve* (atom nil)
        reject* (atom nil)
        settled? (atom false)
        promise
        (js/Promise.
         (fn [resolve reject]
           (reset! resolve* resolve)
           (reset! reject* reject)))]
    {:promise promise
     :settled? settled?
     :resolve!
     (fn [value]
       (when (compare-and-set! settled? false true)
         (@resolve* value)))
     :reject!
     (fn [error]
       (when (compare-and-set! settled? false true)
         (@reject* error)))}))

(defn- record-execution-id
  [record]
  (get-in record [:command protocol/execution-id-key]))

(defn- record-command-id
  [record]
  (get-in record [:command protocol/command-id-key]))

(defn- exact-active-record?
  [runtime record]
  (identical?
   record
   (get @(:active-by-execution runtime)
        (record-execution-id record))))

(defn- remove-active-record!
  [runtime record]
  (let [execution-id (record-execution-id record)]
    (swap! (:active-by-execution runtime)
           (fn [active]
             (if (identical? record (get active execution-id))
               (dissoc active execution-id)
               active))))
  (when-let [xhr @(:xhr record)]
    (when (identical? record (weak-get (:requests-by-xhr runtime) xhr))
      (weak-delete! (:requests-by-xhr runtime) xhr)))
  true)

(defn- record-error-data
  [record]
  (cond->
   {:execution-id
    (protocol/execution-id->wire
     (record-execution-id record))
    :command-id
    (protocol/command-id->wire
     (record-command-id record))}
    (:target-id record)
    (assoc :target-id (:target-id record))))

(defn- cancel-record!
  [runtime record reason]
  (when (compare-and-set! (:finished? record) false true)
    (when-let [transport @(:transport record)]
      ((:reject! transport)
       (bridge-error
        :transport-cancelled
        "Optimistic HTMX transport was cancelled before completion."
        {:reason reason})))
    (when-let [execution-ref @(:execution-ref record)]
      (try
        (optimistic/retire!
         (:optimistic runtime)
         execution-ref
         reason)
        (catch :default error
          (report-error!
           runtime
           :retire-after-cancel
           error
           (record-error-data record)))))
    (abort-xhr! @(:xhr record))
    (remove-active-record! runtime record))
  true)

;; =============================================================================
;; Choreo send/transport realization
;; =============================================================================

(defn- optimistic-send-action?
  [ctx]
  (= protocol/command-event
     (get-in ctx [browser-choreo/action-key :event])))

(defn- optimistic-command-message?
  [ctx]
  (= protocol/command-event
     (get-in ctx [browser-choreo/message-key :event])))

(defn- call-previous!
  [kind previous ctx]
  (if previous
    (previous ctx)
    (throw
     (bridge-error
      :unhandled-choreo-transport
      "No previous browser Choreo handler exists for this non-optimistic send."
      {:kind kind
       :execution-id (get ctx browser-choreo/execution-id-key)}))))

(defn- optimistic-send-payload
  [runtime ctx]
  (let [execution-id (get ctx browser-choreo/execution-id-key)
        record (get @(:active-by-execution runtime) execution-id)]
    (when-not record
      (throw
       (bridge-error
        :missing-request-correlation
        "Optimistic Choreo send has no active HTMX request correlation."
        {:execution-id execution-id})))
    (let [action (get ctx browser-choreo/action-key)
          payload-keys
          (set/union
           (or (:required action) #{})
           (or (:optional action) #{}))
          command-values
          (dissoc (:command record)
                  protocol/protocol-version-key)]
      (select-keys command-values payload-keys))))

(defn- verify-command-message!
  [record message]
  (when-not (= :http (:via message))
    (throw
     (bridge-error
      :wrong-transport
      "Optimistic command choreography must use the HTTP transport."
      {:via (:via message)})))
  (let [expected
        (dissoc (:command record)
                protocol/protocol-version-key)]
    (when-not (= expected (:payload message))
      (throw
       (bridge-error
        :command-message-mismatch
        "Projected optimistic command message differs from the command already attached to the HTMX request."
        {:expected expected
         :actual (:payload message)}))))
  message)

(defn- optimistic-transport-send
  [runtime ctx]
  (let [execution-id (get ctx browser-choreo/execution-id-key)
        record (get @(:active-by-execution runtime) execution-id)]
    (when-not record
      (throw
       (bridge-error
        :missing-request-correlation
        "Optimistic HTTP transport has no active HTMX request correlation."
        {:execution-id execution-id})))
    (verify-command-message!
     record
     (get ctx browser-choreo/message-key))
    (when @(:transport record)
      (throw
       (bridge-error
        :duplicate-transport-send
        "Optimistic HTMX request received more than one command transport send."
        {:execution-id execution-id})))
    (let [transport (deferred)]
      (reset! (:transport record) transport)
      {:completion (:promise transport)
       :cancel!
       (fn []
         (cancel-record!
          runtime
          record
          :transport-cancelled))})))

(defn- send-payload-wrapper
  [runtime previous]
  (fn [ctx]
    (if (optimistic-send-action? ctx)
      (optimistic-send-payload runtime ctx)
      (call-previous! :send-payload previous ctx))))

(defn- transport-wrapper
  [runtime previous]
  (fn [ctx]
    (if (optimistic-command-message? ctx)
      (optimistic-transport-send runtime ctx)
      (call-previous! :transport-send previous ctx))))

;; =============================================================================
;; Settlement extraction
;; =============================================================================

(defn- response-markers
  [runtime event]
  (let [text (response-text event)
        document (:document runtime)]
    (if (or (nil? text)
            (str/blank? text)
            (nil? document))
      []
      (let [template (.createElement document "template")
            selector (str "[" settlement-attribute "]")]
        (set! (.-innerHTML template) text)
        (let [content (.-content template)
              nodes (.querySelectorAll content selector)]
          (mapv
           #(dom/attr % settlement-attribute)
           (array-seq nodes)))))))

(defn- default-settlement-from-event
  [runtime event record]
  (let [decoded
        (mapv decode-settlement-edn
              (response-markers runtime event))
        command-id (record-command-id record)
        execution-id (record-execution-id record)
        matching
        (filterv
         (fn [settlement]
           (and (= command-id
                   (get settlement protocol/command-id-key))
                (= execution-id
                   (get settlement protocol/execution-id-key))))
         decoded)]
    (case (count matching)
      0 nil
      1 (first matching)
      (throw
       (bridge-error
        :duplicate-settlement
        "HTMX response contains multiple settlements for one optimistic execution."
        {:count (count matching)
         :execution-id (protocol/execution-id->wire execution-id)
         :command-id (protocol/command-id->wire command-id)})))))

;; =============================================================================
;; HTMX lifecycle observers
;; =============================================================================

(defn on-config-request!
  [runtime event]
  (let [runtime (require-runtime! runtime)
        source (source-with-action runtime event)]
    (when source
      (let [existing (weak-get (:preflights-by-source runtime) source)]
        (if (and existing
                 (nil? (:error existing)))
          (try
            (set-parameter!
             (config-parameters event)
             (:command-parameter runtime)
             (:encoded-command existing))
            (catch :default error
              (weak-set!
               (:preflights-by-source runtime)
               source
               {:source source
                :error error})
              (report-error!
               runtime
               :config-request
               error
               {})))
          (try
            (let [preflight (preflight-for runtime source event)]
              (set-parameter!
               (config-parameters event)
               (:command-parameter runtime)
               (:encoded-command preflight))
              (weak-set!
               (:preflights-by-source runtime)
               source
               preflight))
            (catch :default error
              ;; configRequest itself is not used as a cancellation boundary.
              ;; Preserve the failure so the documented cancellable
              ;; beforeRequest hook can fail the physical request closed.
              (weak-set!
               (:preflights-by-source runtime)
               source
               {:source source
                :error error})
              (report-error!
               runtime
               :config-request
               error
               {})))))))
  true)

(defn- preflight-source
  [runtime event]
  (let [elt (event-source event)]
    (cond
      (weak-get (:preflights-by-source runtime) elt)
      elt

      :else
      (source-with-action runtime event))))

(defn- fail-before-request!
  [runtime event source error]
  (prevent-request! event)
  (when source
    (weak-delete! (:preflights-by-source runtime) source))
  (report-error!
   runtime
   :before-request
   error
   {})
  true)

(defn on-before-request!
  [runtime event]
  (let [runtime (require-runtime! runtime)
        source (preflight-source runtime event)]
    (when source
      (let [preflight (weak-get (:preflights-by-source runtime) source)]
        (cond
          (nil? preflight)
          (fail-before-request!
           runtime event source
           (bridge-error
            :missing-preflight
            "Optimistic HTMX request reached beforeRequest without configRequest correlation."
            {}))

          (:error preflight)
          (fail-before-request!
           runtime event source (:error preflight))

          :else
          (let [xhr (event-xhr event)]
            (if-not xhr
              (fail-before-request!
               runtime event source
               (bridge-error
                :missing-xhr
                "Optimistic HTMX beforeRequest did not expose an XHR."
                {}))
              (let [record
                    (assoc preflight
                           :xhr (atom xhr)
                           :execution-ref (atom nil)
                           :transport (atom nil)
                           :handed-off? (atom false)
                           :finished? (atom false))
                    execution-id (record-execution-id record)]
                (swap! (:active-by-execution runtime) assoc execution-id record)
                (weak-set! (:requests-by-xhr runtime) xhr record)
                (try
                  (let [result
                        (optimistic/start!
                         (:optimistic runtime)
                         (optimistic-start-options preflight))
                        execution-ref (:execution-ref result)
                        transport @(:transport record)]
                    (reset! (:execution-ref record) execution-ref)
                    (weak-delete! (:preflights-by-source runtime) source)
                    (if (and execution-ref transport)
                      true
                      (do
                        ;; An asynchronous semantic projector has not yet
                        ;; reached the transport boundary. HTMX beforeRequest
                        ;; cannot wait for that Promise without reimplementing
                        ;; HTMX request issuance, so fail this attempt closed and
                        ;; leave generic async projection support to non-HTMX
                        ;; transports.
                        (prevent-request! event)
                        (when execution-ref
                          (optimistic/retire!
                           (:optimistic runtime)
                           execution-ref
                           :htmx-command-not-ready))
                        (remove-active-record! runtime record)
                        (report-error!
                         runtime
                         :before-request
                         (bridge-error
                          :command-not-ready
                          "Optimistic execution did not reach HTTP transport synchronously before HTMX request issuance."
                          {})
                         (record-error-data record))
                        true)))
                  (catch :default error
                    (prevent-request! event)
                    (weak-delete! (:preflights-by-source runtime) source)
                    (remove-active-record! runtime record)
                    (report-error!
                     runtime
                     :before-request
                     error
                     (record-error-data record))
                    true))))))))
  true))

(defn on-before-send!
  [runtime event]
  (let [runtime (require-runtime! runtime)
        xhr (event-xhr event)
        record (weak-get (:requests-by-xhr runtime) xhr)]
    (when record
      (if-let [transport @(:transport record)]
        (when (compare-and-set! (:handed-off? record) false true)
          ((:resolve! transport)
           :htmx-before-send))
        (do
          ;; beforeSend is no longer cancellable. Abort the physical XHR if
          ;; correlation somehow vanished rather than letting an unmodelled
          ;; command escape.
          (abort-xhr! xhr)
          (report-error!
           runtime
           :before-send
           (bridge-error
            :missing-transport
            "Optimistic HTMX request reached beforeSend without a pending Choreo transport."
            {})
           (record-error-data record))))))
  true)

(defn- retire-network-failure!
  [runtime record reason]
  (when (compare-and-set! (:finished? record) false true)
    (let [transport @(:transport record)]
      (if (and transport
               (not @(:handed-off? record)))
        ((:reject! transport)
         (bridge-error
          :transport-failed
          "Optimistic HTMX transport failed before send handoff."
          {:reason reason}))
        (when-let [execution-ref @(:execution-ref record)]
          (try
            (optimistic/retire!
             (:optimistic runtime)
             execution-ref
             :optimistic-network-failed)
            (catch :default error
              (report-error!
               runtime
               :network-failure-retire
               error
               (record-error-data record)))))))
    (remove-active-record! runtime record))
  true)

(defn on-request-failed!
  [runtime event]
  (let [runtime (require-runtime! runtime)
        xhr (event-xhr event)
        record (weak-get (:requests-by-xhr runtime) xhr)]
    (when record
      (retire-network-failure!
       runtime record (core/failure-reason event))))
  true)

(defn- incompatible-protocol-error?
  [error]
  (let [data (ex-data error)]
    (and (= :gesso.live.optimistic.protocol/error
            (:error/type data))
         (= :unsupported-version
            (:error/kind data))
         (= :incompatible
            (get-in data [:protocol/version-status :status])))))

(defn- retire-incompatible-protocol!
  [runtime record error]
  (report-error!
   runtime
   :settlement-protocol-incompatible
   error
   (merge
    (record-error-data record)
    {:protocol/version-status
     (:protocol/version-status (ex-data error))}))
  (when-let [execution-ref @(:execution-ref record)]
    (try
      ;; The adapter owns the semantic recovery policy.  This bridge only
      ;; classifies the trusted response boundary and supplies the distinct
      ;; terminal reason.  In particular, do not fabricate a :failed
      ;; settlement merely because browser/server wire versions disagree.
      (optimistic/retire!
       (:optimistic runtime)
       execution-ref
       :optimistic-incompatible-protocol)
      (catch :default retire-error
        ;; Semantic retirement is attempted before the physical HTTP
        ;; correlation is forgotten.  Any failure here is diagnostic; this
        ;; bridge must still release its host-object correlation below rather
        ;; than leaving an XHR record capable of replaying the stale response.
        (report-error!
         runtime
         :incompatible-protocol-retire
         retire-error
         (record-error-data record)))))
  true)

(defn- settle-successful-request!
  [runtime event record]
  (try
    (when-let [settlement
               ((:settlement-from-event runtime)
                runtime event record)]
      (when-let [execution-ref @(:execution-ref record)]
        (optimistic/settle!
         (:optimistic runtime)
         execution-ref
         settlement)))
    (catch :default error
      (if (incompatible-protocol-error? error)
        (retire-incompatible-protocol! runtime record error)
        ;; A malformed or conflicting response cannot manufacture a semantic
        ;; outcome. Leave the optimistic execution alive so timeout or a trusted
        ;; authoritative reread can recover it, and surface the physical protocol
        ;; failure diagnostically.
        (report-error!
         runtime
         :settlement-response
         error
         (record-error-data record)))))
  true)

(defn on-after-request!
  [runtime event]
  (let [runtime (require-runtime! runtime)
        xhr (event-xhr event)
        record (weak-get (:requests-by-xhr runtime) xhr)]
    (when record
      (if (core/request-successful? event)
        (when (compare-and-set! (:finished? record) false true)
          (settle-successful-request! runtime event record)
          (remove-active-record! runtime record))
        (retire-network-failure!
         runtime record (core/failure-reason event)))))
  true)

;; =============================================================================
;; Construction / detachment
;; =============================================================================

(defn runtime?
  [value]
  (try
    (require-runtime! value)
    true
    (catch :default _
      false)))

(defn core-runtime
  [runtime]
  (:core (require-runtime! runtime)))

(defn optimistic-runtime
  [runtime]
  (:optimistic (require-runtime! runtime)))

(defn choreo-runtime
  [runtime]
  (optimistic/choreo-runtime
   (optimistic-runtime runtime)))

(defn- event-handler
  [runtime event-name]
  (case event-name
    "htmx:configRequest" #(on-config-request! runtime %)
    "htmx:beforeRequest" #(on-before-request! runtime %)
    "htmx:beforeSend" #(on-before-send! runtime %)
    "htmx:afterRequest" #(on-after-request! runtime %)
    "htmx:responseError" #(on-request-failed! runtime %)
    "htmx:sendError" #(on-request-failed! runtime %)
    "htmx:timeout" #(on-request-failed! runtime %)
    "htmx:abort" #(on-request-failed! runtime %)))

(defn- observer-handler-still-owned?
  [core-runtime event-name handler]
  (identical?
   handler
   (get-in @(:event-observers core-runtime)
           [event-name observer-id])))

(defn create
  "Attach the protocol-v3 optimistic HTTP bridge to one browser Core runtime and
   one browser.optimistic runtime sharing the exact same shell.

   Required option:

     :plan-for
       (fn [{:keys [source action plan-key command target-id]}] plan)
       Returns the preverified browser ExecutablePlan for this bound operation.
       Verification/projection does not run in this browser namespace.

   Optional options:

     :read-action
       Reads one server-rendered action descriptor from the source element.
       Default reads EDN from data-gesso-live-optimistic.

     :encode-command
       Runtime command -> nonblank request string. Default is protocol-v3 wire
       EDN. The value is inserted into __gesso_live_optimistic_command during
       htmx:configRequest.

     :settlement-from-event
       (fn [bridge event physical-record] settlement-or-nil). Default parses
       data-gesso-live-optimistic-settlement markers from xhr.responseText.

     :command-id-fn / :execution-id-fn
       Zero-arity typed Choreo identity constructors. Defaults use random UUIDs.

     :on-error
       Read-only diagnostic callback receiving host-resource-free maps.

   This bridge wraps any existing generic browser.choreo send/transport handlers
   and delegates non-optimistic sends to them. detach! restores those handlers
   only if the bridge still owns the exact wrapper functions."
  ([core-runtime optimistic-runtime options]
   (core/require-core! core-runtime)
   (optimistic/require-runtime! optimistic-runtime)
   (when-not
    (identical?
     (core/shell-runtime core-runtime)
     (optimistic/shell-runtime optimistic-runtime))
     (throw
      (bridge-error
       :different-shell
       "Optimistic HTMX bridge requires Core and optimism to share one exact browser shell."
       {})))
   (let [options (check-option-keys! (or options {}))
         plan-for (require-callable!
                   "Optimistic HTMX :plan-for"
                   (:plan-for options))
         read-action (or (:read-action options) default-read-action)
         encode-command (or (:encode-command options) encode-command-edn)
         command-id-fn (or (:command-id-fn options) default-command-id)
         execution-id-fn (or (:execution-id-fn options) default-execution-id)
         on-error (:on-error options)
         _ (require-callable! "Optimistic HTMX :read-action" read-action)
         _ (require-callable! "Optimistic HTMX :encode-command" encode-command)
         _ (require-callable! "Optimistic HTMX :command-id-fn" command-id-fn)
         _ (require-callable! "Optimistic HTMX :execution-id-fn" execution-id-fn)
         _ (require-optional-callable! "Optimistic HTMX :on-error" on-error)
         choreo-runtime (optimistic/choreo-runtime optimistic-runtime)
         previous-send-payload (browser-choreo/send-payload-handler choreo-runtime)
         previous-transport (browser-choreo/transport-handler choreo-runtime)
         runtime
         {:gesso.live.browser.optimistic-htmx/type runtime-type
          :gesso.live.browser.optimistic-htmx/version runtime-version
          :core core-runtime
          :optimistic optimistic-runtime
          :choreo choreo-runtime
          :document (:document core-runtime)
          :action-attribute (or (:action-attribute options) action-attribute)
          :command-parameter (or (:command-parameter options) command-parameter)
          :read-action read-action
          :encode-command encode-command
          :plan-for plan-for
          :settlement-from-event nil
          :command-id-fn command-id-fn
          :execution-id-fn execution-id-fn
          :on-error on-error
          :preflights-by-source (js/WeakMap.)
          :requests-by-xhr (js/WeakMap.)
          :active-by-execution (atom {})
          :previous-send-payload previous-send-payload
          :previous-transport-send previous-transport
          :send-payload-wrapper (atom nil)
          :transport-wrapper (atom nil)
          :observer-handlers (atom {})
          :attached? (atom true)}
         settlement-from-event
         (or (:settlement-from-event options)
             default-settlement-from-event)
         runtime (assoc runtime :settlement-from-event settlement-from-event)
         send-wrapper (send-payload-wrapper runtime previous-send-payload)
         transport-wrapper* (transport-wrapper runtime previous-transport)
         observer-handlers
         (into {}
               (map (fn [event-name]
                      [event-name (event-handler runtime event-name)]))
               observed-events)]
     (reset! (:send-payload-wrapper runtime) send-wrapper)
     (reset! (:transport-wrapper runtime) transport-wrapper*)
     (reset! (:observer-handlers runtime) observer-handlers)
     (browser-choreo/set-send-payload-handler! choreo-runtime send-wrapper)
     (browser-choreo/set-transport-handler! choreo-runtime transport-wrapper*)
     (doseq [[event-name handler] observer-handlers]
       (core/register-event-observer!
        core-runtime event-name observer-id handler))
     runtime)))

(defn detach!
  "Detach only bridge-owned observers and Choreo transport wrappers.

   The enclosing composed runtime must retire semantic executions before calling
   this function. Physical correlations are then forgotten; foreign handlers
   that replaced bridge-owned slots are preserved."
  [runtime]
  (let [runtime (require-runtime! runtime)]
    (when (compare-and-set! (:attached? runtime) true false)
      (let [core-runtime (:core runtime)
            choreo-runtime (:choreo runtime)]
        (doseq [[event-name handler] @(:observer-handlers runtime)]
          (when (observer-handler-still-owned?
                 core-runtime event-name handler)
            (core/unregister-event-observer!
             core-runtime event-name observer-id)))
        (when (identical?
               @(:send-payload-wrapper runtime)
               (browser-choreo/send-payload-handler choreo-runtime))
          (browser-choreo/set-send-payload-handler!
           choreo-runtime
           (:previous-send-payload runtime)))
        (when (identical?
               @(:transport-wrapper runtime)
               (browser-choreo/transport-handler choreo-runtime))
          (browser-choreo/set-transport-handler!
           choreo-runtime
           (:previous-transport-send runtime)))
        (reset! (:observer-handlers runtime) {})
        (reset! (:active-by-execution runtime) {})))
    :detached))

(defn diagnostics
  "Return host-resource-free bridge diagnostics.

   DOM source elements, XHR objects, Promise resolvers, and callbacks are never
   returned."
  [runtime]
  (let [runtime (require-runtime! runtime)]
    {:gesso.live.browser.optimistic-htmx/type runtime-type
     :gesso.live.browser.optimistic-htmx/version runtime-version
     :attached? @(:attached? runtime)
     :action-attribute (:action-attribute runtime)
     :command-parameter (:command-parameter runtime)
     :observed-events (set (keys @(:observer-handlers runtime)))
     :active-executions
     (into #{}
           (map protocol/execution-id->wire)
           (keys @(:active-by-execution runtime)))}))

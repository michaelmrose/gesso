(ns gesso.live.optimistic.resolution-assurance
  "Process-local, controlled diagnostic evidence for an actually registered
   resolved-operation Live executor. This is NOT application certification.

   run-and-check! starts exactly one trusted Live command boundary, requires
   the precise registered :execute! function to have the private construction
   identity issued by gesso.live.optimistic.resolved-operation/build-executor,
   installs the resolved-operation transaction observer for that invocation,
   calls the actual executor and Live settlement preparation, then verifies the
   observed physical transaction against the bounded serial reference model
   using independent XTDB reads.

   The same session also captures the actual context and canonical binding at
   the resolved-operation construction call site. Only a complete, exact-match
   construction event paired with the definitive physical attempt can yield
   an evidence handle. Sensitive inputs stay private to the handle registry;
   the ordinary explanatory report never contains them.

   Only a successful, definitive single-attempt diagnostic run yields an opaque
   process-local evidence handle. inspect-evidence accepts only that exact
   handle, not an application-supplied report or copied function metadata.
   inspect-evidence-for-executor additionally requires the SAME executor object
   that actually ran. inspect-construction-input-for-executor requires that same
   identity and exposes sensitive original inputs only to a trusted local caller. Application reporting can use that check against the
   executor recovered from a freshly checked application assembly; reports
   must never infer this identity link from a matching operation keyword.
   These functions return explanatory maps, not transferable certificates.

   The diagnostic observer never submits or retries commands. The executor
   does submit through its existing ordinary path. If the operation or Live
   settlement throws, its exception propagates; if evidence collection fails
   after execution, that diagnostic failure must NOT be interpreted as a failed
   authoritative commit. Applications must reconcile against durable state.

   No claim is made about authentication of XTDB history, correctness of the
   application's :construct! or :project! callbacks, completeness of policy
   guards, pinned multi-table snapshots, concurrent unrelated writers, or
   universal refinement. Do not attach these process-local handles to normal
   application startup certificates or browser-visible settlement payloads."
  (:require
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command-resolution :as command-resolution]
   [gesso.model.resolution-event-observation :as events]
   [gesso.model.resolved-operation :as resolved])
  (:import
   (java.lang.ref WeakReference)
   (java.util WeakHashMap)))

(def request-keys #{:server :ctx :command})

(def assurance-scope
  {:construction :gesso.live.optimistic.resolution-assurance/diagnostic-v1
   :checked #{:actual-registered-executor-identity
              :server-derived-principal-and-command-binding
              :single-live-command-boundary
              :one-real-executor-invocation
              :physical-attempt-event-pair
              :independent-bounded-xtdb-state-correspondence
              :no-evidence-on-unknown-outcome-or-replay
              :opaque-process-local-evidence-handle
              :exact-executed-adapter-identity-bound-to-evidence-handle
              :construction-input-captured-at-real-call-site
              :construction-input-correlated-with-definitive-attempt
              :sensitive-input-excluded-from-detached-report}
   :assumed #{:trusted-server-and-request-context
              :trusted-v798-runtime-construction-registry
              :actual-v942-construction-input-hook
              :actual-v809-transaction-event-hook
              :truthful-xtdb-node-and-queries
              :no-unrelated-writes-during-observation
              :consistent-unpinned-xtdb-reads
              :trusted-domain-construct-and-project-callbacks}
   :not-established #{:independently-authenticated-construction-input-values
                      :complete-model-input-or-policy-dependencies
                      :authenticated-xtdb-transaction-history
                      :durable-or-portable-certificate
                      :domain-authorization-or-invariant-correctness
                      :complete-business-guard-dependencies
                      :universal-linearizability
                      :formal-refinement-proof
                      :whole-application-certification}})

(def ^:private ^WeakHashMap registered-evidence (WeakHashMap.))

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-resolution-assurance
                   :error/kind kind
                   :error/details details})))

(defn- record-evidence! [executor operation result construction-input]
  ;; Weak references to both the handle and executor avoid retaining hot-reload
  ;; generations. The executor is NOT recovered from function metadata: it is
  ;; the exact object invoked through the real Live boundary.
  (let [handle (Object.)]
    (locking registered-evidence
      (.put registered-evidence handle
            {:identity (WeakReference. handle)
             :executor-reference (WeakReference. executor)
             :operation operation
             :result result
             ;; SENSITIVE: never include this witness inside :result, because
             ;; inspect-evidence deliberately returns a detachable report.
             :construction-input construction-input}))
    handle))

(defn- evidence-entry [handle]
  (when (some? handle)
    (locking registered-evidence
      (let [{:keys [identity] :as entry} (.get registered-evidence handle)]
        (when (and identity (identical? handle (.get ^WeakReference identity)))
          entry)))))

(defn inspect-evidence
  "Inspect a handle issued by a completed run-and-check! in this JVM/namespace
   instance. Returns nil for fabricated, expired or foreign handles. Returned
   maps are explanatory DATA, not portable certificates or trusted evidence
   when detached from the opaque handle and this live registry."
  [handle]
  (:result (evidence-entry handle)))

(defn inspect-evidence-for-executor
  "Return bounded execution evidence ONLY when the supplied executor is the
   exact adapter object that produced this handle. No application-reported
   operation ID, copied metadata, new wrapper or separate adapter for the same
   operation can substitute for the original executor.

   The caller must obtain `executor` from the CURRENT checked application
   assembly before using this result in application assurance. This function
   does not itself establish registry currentness, test freshness, policy
   validity, a truthful projector, or universal execution correctness. If an
   executor has been replaced, the old handle is no evidence about the new
   registration. Returns nil if the link cannot be established."
  [handle executor]
  (when (fn? executor)
    (when-let [{:keys [executor-reference operation result]} (evidence-entry handle)]
      (let [current-construction (adapter/construction-evidence executor)]
        (when (and executor-reference
                   (identical? executor (.get ^WeakReference executor-reference))
                   (= :bounded-execution-correspondence (:status result))
                   (= operation (get-in result [:binding :operation]))
                   (= operation (:operation current-construction))
                   (= :constructed-by-live-resolved-operation-adapter
                      (:status current-construction))
                   (= current-construction (:adapter-construction result)))
          result)))))

(defn inspect-construction-input-for-executor
  "Return the SENSITIVE construction-input witness only for a current live
   registry handle and its EXACT executed adapter object. The caller must
   separately establish that the executor is still registered in the current
   checked application assembly. No detached report, copied metadata or other
   executor can retrieve the inputs.

   Returns {:status :executor-bound-construction-input
            :context original-trusted-server-context
            :binding canonical-command-binding
            :input-origin :trusted-local-call-site-capture} or nil.
   The context includes original application-private fields and the XTDB node,
   but excludes both temporary diagnostic hooks. Do not serialize, log,
   expose to the browser, or treat these values as an independently
   authenticated statement about all production authorization dependencies.
   This is a process-local witness for optional model-input comparison, not a
   formal proof or transferable certificate."
  [handle executor]
  (when (inspect-evidence-for-executor handle executor)
    (when-let [{:keys [context binding]} (:construction-input (evidence-entry handle))]
      {:status :executor-bound-construction-input
       :context context
       :binding binding
       :input-origin :trusted-local-call-site-capture
       :application-certification :not-established})))

(defn- construction-input-witness!
  [captured instrumented-context original-context binding]
  (let [observations @captured]
    ;; A real initial construction emits exactly one event. No event means
    ;; replay/reconciliation or a failed observer, not evidence of equality.
    (when-not (= 1 (count observations))
      (fail! :construction-input-incomplete
             "Construction input evidence is absent or ambiguous." {}))
    (let [event (first observations)]
      (when-not (and (map? event)
                     (= #{:scope :phase :operation :context :binding}
                        (set (keys event)))
                     (= resolved/construction-input-observation-scope
                        (:scope event))
                     (= :construction-input (:phase event))
                     (= (:operation binding) (:operation event))
                     (= binding (:binding event))
                     ;; A matching *copy* of a context cannot silently claim
                     ;; to be the actual map passed into construct!.
                     (identical? instrumented-context (:context event))
                     (= original-context
                        (dissoc (:context event)
                                resolved/observation-context-key
                                resolved/construction-input-context-key)))
        (fail! :construction-input-mismatch
               "Construction input evidence does not match the trusted Live invocation."
               {}))
      {:context original-context :binding binding})))

(defn run-and-check!
  "Run one fresh trusted Live command and check its definitive physical attempt.

   Request: {:server prepared-optimistic-server :ctx trusted-ring-context
             :command already-decoded-protocol-v3-command}.
   :ctx MUST carry the actual :biff.xtdb/node and must not have either
   diagnostic observation hook. The captured context is accessible only via
   inspect-construction-input-for-executor with the exact executor identity.
   Registration must contain the exact v798 adapter executor, not copied metadata or a wrapper. The principal and
   operation binding are derived from server/begin-command rather than from
   caller-provided assurance claims.

   On success returns {:prepared-send real-Live-prepared-send
                       :evidence opaque-process-local-handle}.
   Use inspect-evidence on the handle to see finite witness details.

   If the executor fails, the ORIGINAL exception propagates, even if a commit
   actually happened. If Live settlement succeeds but evidence is incomplete
   or inconsistent, a diagnostic exception propagates; this does not roll back
   any commit and is not a business rejection. No handle is issued on failure."
  [request]
  (when-not (and (map? request)
                 (= request-keys (set (keys request))))
    (fail! :invalid-request "Expected a closed diagnostic execution request."
           {:expected request-keys}))
  (let [{:keys [server ctx command]} request]
    (when-not (and (server/server? server) (map? ctx))
      (fail! :invalid-server-context
             "A checked Live server and trusted context are required." {}))
    (when (or (nil? (:biff.xtdb/node ctx))
              (contains? ctx resolved/observation-context-key)
              (contains? ctx resolved/construction-input-context-key))
      (fail! :invalid-observation-context
             "Require an authoritative node and exclusive diagnostic hook ownership."
             {}))
    (let [boundary (server/begin-command server ctx command)
          execution-context (server/operation-context boundary)
          executor (get-in boundary [:operation-entry :execute!])
          operation (:operation execution-context)
          provenance (adapter/construction-evidence executor)]
      (when-not (and (= :constructed-by-live-resolved-operation-adapter
                        (:status provenance))
                     (= operation (:operation provenance))
                     (= (:construction adapter/assurance-scope)
                        (:construction provenance)))
        (fail! :unrecognized-registered-adapter
               "The exact registered executor is not a checked resolved-operation adapter."
               {:operation operation}))
      (let [binding (command-resolution/require-binding
                     {:command-id (:command-id execution-context)
                      :principal (:principal execution-context)
                      :operation operation
                      :arguments (:arguments execution-context)})
            observation (events/start-session!
                         {:node (:biff.xtdb/node ctx) :binding binding})
            captured-inputs (atom [])
            instrumented-context
            (assoc ctx
                   resolved/observation-context-key (:observe! observation)
                   resolved/construction-input-context-key
                   (fn [event]
                     ;; Bound memory for incorrect or hostile duplicate event
                     ;; delivery; duplicate evidence must fail closed.
                     (swap! captured-inputs
                            (fn [events]
                              (if (< (count events) 2)
                                (conj events event)
                                events)))))
            instrumented (assoc execution-context :ctx instrumented-context)
            ;; Same trusted execution and settlement functions as server/run-command.
            ;; A thrown exception is never translated to a settled rejection.
            result (executor instrumented)
            prepared-send (server/prepare-settlement-send boundary result)
            checked ((:complete! observation))]
        (when-not (= :consistent (get-in checked [:evidence :status]))
          (fail! :execution-counterexample
                 "Real observed execution does not correspond to the finite reference machine."
                 {:reason (get-in checked [:evidence :reason])}))
        (let [input-witness (construction-input-witness!
                             captured-inputs instrumented-context ctx binding)]
          {:prepared-send prepared-send
           :evidence (record-evidence!
                      executor operation
                      {:status :bounded-execution-correspondence
                       :scope assurance-scope
                       :adapter-construction provenance
                       :binding binding
                       :construction-input
                       {:status :captured-and-correlated
                        :context-values :withheld-from-report
                        :binding :matched-canonical-live-command
                        :model-input-coverage :not-established}
                       :observation checked}
                      input-witness)})))))

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

   Only a successful, definitive single-attempt diagnostic run yields an opaque
   process-local evidence handle. inspect-evidence accepts only that exact
   handle, not an application-supplied report or copied function metadata.
   inspect-evidence-for-executor additionally requires the SAME executor object
   that actually ran. Application reporting can use that check against the
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
              :exact-executed-adapter-identity-bound-to-evidence-handle}
   :assumed #{:trusted-server-and-request-context
              :trusted-v798-runtime-construction-registry
              :actual-v809-transaction-event-hook
              :truthful-xtdb-node-and-queries
              :no-unrelated-writes-during-observation
              :consistent-unpinned-xtdb-reads
              :trusted-domain-construct-and-project-callbacks}
   :not-established #{:authenticated-xtdb-transaction-history
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

(defn- record-evidence! [executor operation result]
  ;; Weak references to both the handle and executor avoid retaining hot-reload
  ;; generations. The executor is NOT recovered from function metadata: it is
  ;; the exact object invoked through the real Live boundary.
  (let [handle (Object.)]
    (locking registered-evidence
      (.put registered-evidence handle
            {:identity (WeakReference. handle)
             :executor-reference (WeakReference. executor)
             :operation operation
             :result result}))
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

(defn run-and-check!
  "Run one fresh trusted Live command and check its definitive physical attempt.

   Request: {:server prepared-optimistic-server :ctx trusted-ring-context
             :command already-decoded-protocol-v3-command}.
   :ctx MUST carry the actual :biff.xtdb/node and must not have an existing
   transaction observation hook. Registration must contain the exact v798
   adapter executor, not copied metadata or a wrapper. The principal and
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
              (contains? ctx resolved/observation-context-key))
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
            instrumented (assoc execution-context :ctx
                                (assoc ctx resolved/observation-context-key
                                       (:observe! observation)))
            ;; Same trusted execution and settlement functions as server/run-command.
            ;; A thrown exception is never translated to a settled rejection.
            result (executor instrumented)
            prepared-send (server/prepare-settlement-send boundary result)
            checked ((:complete! observation))]
        (when-not (= :consistent (get-in checked [:evidence :status]))
          (fail! :execution-counterexample
                 "Real observed execution does not correspond to the finite reference machine."
                 {:reason (get-in checked [:evidence :reason])}))
        {:prepared-send prepared-send
         :evidence (record-evidence!
                    executor operation
                    {:status :bounded-execution-correspondence
                     :scope assurance-scope
                     :adapter-construction provenance
                     :binding binding
                     :observation checked})}))))

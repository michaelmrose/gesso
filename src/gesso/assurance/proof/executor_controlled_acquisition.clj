(ns gesso.assurance.proof.executor-controlled-acquisition
  "v1382: A single-call, executor-owned observation session for a canonical
   Gesso resolved-operation submission. This is a safer *acquisition wiring*
   boundary than the v1373 public (:observe! session) interface: this entry
   point constructs the observer, installs it only for its own call, executes
   the ORIGINAL resolved-operation builder, and completes the session itself.

   Applications do not receive :observe!, :complete!, or any observer-bearing
   context in their registered callbacks. An already-injected observation key
   is refused rather than reused. The controller never retries or changes the
   authoritative resolved-operation result, and an incomplete observer pair
   remains missing evidence, not a business failure or proof of noncommit.

   Limitations are deliberate. Configuration/callbacks and the invocation
   context are supplied by trusted application code; this does NOT prove the
   checked writer catalog identifies the deployed handler, that callbacks are
   pure or nonmalicious, or that tx/transact! reached XTDB. The underlying
   observation hook is still process-local and can be bypassed by other code.
   Clojure functions and source maps are not unforgeable capabilities. This
   diagnostic issues NO authenticated provenance, admission, or certificate.

   The returned :resolution and :acquisition-report can contain sensitive
   command and plan data. Keep them in a trusted server-side diagnostic scope;
   never return this map as an HTTP response or log it wholesale."
  (:require [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acquisition]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]))

(def boundary-type
  :gesso.assurance.proof.executor-controlled-acquisition/one-invocation)
(def boundary-version 1)

(def ^:private source-keys
  #{:writer-source :operation-id :binding :configuration})
(def ^:private application-context-keys
  #{resolved/observation-context-key
    resolved/construction-input-context-key})
(def ^:private callback-keys
  #{:read-record! :may-disclose? :construct!
    :target-contract! :predicate-contract!
    :command-operation-contract! :post-state-contract!
    :outcome-contract!})
(def ^:private original-build resolved/build-operation)
(def ^:private original-acquire acquisition/start-acquisition!)
(def ^:private required-open
  #{:typed-input-binding
    :transaction-internal-prestate
    :guard-authorization
    :assignment-write-footprint
    :frame-and-unmodeled-fields
    :serial-concurrency-validation
    :committed-outcome-incorporation
    :writer-provenance-and-deployment
    :universal-execution-refinement})

(defn- reject! [reason at]
  ;; No contexts, plans, principals, callbacks, or receipts in exceptions.
  (throw (ex-info "Invalid executor-controlled acquisition request."
                  {:executor-controlled-acquisition/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x allowed at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= allowed (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- original-functions! []
  (when-not (and (identical? original-build
                             (var-get #'resolved/build-operation))
                 (identical? original-acquire
                             (var-get #'acquisition/start-acquisition!)))
    (reject! :substituted-entry-point [])))

(defn- checked-context! [ctx]
  (when-not (and (plain-map? ctx) (nil? (meta ctx)))
    (reject! :invalid-invocation-context [:context]))
  (when (some #(contains? ctx %) application-context-keys)
    (reject! :preexisting-observation-hook [:context]))
  ctx)

(defn- isolated-configuration [configuration]
  ;; Build a *new* canonical executor whose application callbacks never see
  ;; the controller's observation hook. All optional contract callbacks are
  ;; included: the trusted server configuration remains responsible for their
  ;; correctness and for every other application side effect.
  (when-not (and (plain-map? configuration) (nil? (meta configuration)))
    (reject! :invalid-configuration [:configuration]))
  (reduce (fn [config callback-key]
            (if (contains? config callback-key)
              (let [callback (get config callback-key)]
                (when-not (fn? callback)
                  (reject! :invalid-callback [:configuration callback-key]))
                (assoc config callback-key
                       (fn [ctx & args]
                         (apply callback
                                (if (map? ctx)
                                  (apply dissoc ctx application-context-keys)
                                  ctx)
                                args))))
              config))
          configuration callback-keys))

(defn- completed-diagnostic [session]
  (try
    {:capture-status :complete-paired-hook-observation
     :capture-reason nil
     :acquisition-report ((:complete! session))}
    (catch clojure.lang.ExceptionInfo exception
      ;; A hook failure is not permitted to change the result of a completed
      ;; mutation. Only the redacted reason travels with this diagnostic.
      {:capture-status :no-complete-evidence
       :capture-reason (or (:reason (ex-data exception)) :acquisition-rejected)
       :acquisition-report nil})))

(defn run-controlled-submit!
  "Execute exactly ONE initial :submit using an observation callback created
   and retained within this call. No external observe!/complete! callback is
   accepted, and all application callbacks receive the original hook-free ctx.

   source = {:writer-source <original checked writer-source>
             :operation-id <qualified operation keyword>
             :binding <canonical Gesso command binding>
             :configuration <trusted resolved/build-operation configuration>}

   The returned :resolution is the ORIGINAL authoritative application result.
   When a prior durable record exists, or when a transaction outcome is
   indeterminate, :acquisition-report is nil and :capture-status explains the
   absence of completed evidence. Exceptions thrown by actual application or
   transaction execution propagate unchanged; this wrapper does not turn them
   into diagnostic success or retry the command.

   Even a captured committed outcome is NOT an authenticated XTDB receipt:
   other writer paths, same-JVM code substitution, physical transaction
   internals, deployed handler identity, and universal refinement remain open."
  [source ctx]
  (exact! source source-keys [])
  (original-functions!)
  (checked-context! ctx)
  (let [{:keys [writer-source operation-id binding configuration]} source]
    (resolution/require-binding binding)
    (when-not (and (qualified-keyword? operation-id)
                   (= operation-id (:operation binding))
                   (= operation-id (:operation configuration)))
      (reject! :operation-source-mismatch [:operation-id]))
    (let [;; Source review occurs before ANY executable application callback.
          session (original-acquire {:writer-source writer-source
                                     :operation-id operation-id
                                     :binding binding})
          ;; Only this function has access to the actual observer closure.
          executor (original-build (isolated-configuration configuration))
          controlled-ctx (assoc ctx resolved/observation-context-key
                                (:observe! session))
          resolution-result (executor controlled-ctx {:binding binding
                                                     :mode :submit})
          capture (completed-diagnostic session)]
      (merge
       {:type boundary-type
        :version boundary-version
        :checking :source-rechecked-single-resolved-executor-invocation
        :operation-id operation-id
        :resolution resolution-result
        :attempt-scope :one-invocation-no-automatic-retry
        :injection-origin :controller-created-per-invocation
        :caller-supplied-observer-accepted? false
        :application-callbacks-received-observer? false
        :executor-call-site-observed? (boolean (:acquisition-report capture))
        :observer-origin-authenticated? false
        :physical-submission-authenticated? false
        :issued-writer-instance-identified? :not-established
        :deployed-handler-identity :not-established
        :other-physical-writers-excluded? :not-established
        :transaction-internal-prestate :not-established
        :universal-execution-refinement :not-established
        :physical-refinement-obligations
        (mapv (fn [id] {:id id :status :open}) (sort required-open))
        :proof-status :executor-controlled-hook-diagnostic-only
        :admission :not-admitted
        :certificate :not-issued
        :transaction-authority :none
        :authority :none}
       capture))))

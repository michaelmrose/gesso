(ns gesso.assurance.proof.executor-controlled-acquisition
  "v1382/v1384: A single-call, executor-owned observation session for a canonical
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
            [gesso.assurance.proof.typed-value-semantics :as typed]
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

(def ^:private typed-source-keys (conj source-keys :input-paths))
(def ^:private input-event-keys
  #{:scope :phase :operation :context :binding})
(def ^:private original-typed-observe typed/observe-state!)
(def ^:private missing-path-value (Object.))
(def ^:private max-input-path-depth 8)

(defn- typed-input-declaration! [writer-source operation-id]
  ;; The ORIGINAL catalog has just been rechecked by start-acquisition!.
  ;; This lookup never derives types from the plan or a caller-supplied report.
  (let [operations (get-in writer-source [:induction-catalog :operations])
        matches (filterv #(= operation-id (:id %)) operations)]
    (when-not (= 1 (count matches))
      (reject! :missing-admitted-input-declaration [:writer-source]))
    (let [types (get-in (first matches) [:declaration :input-types])]
      (when-not (plain-map? types)
        (reject! :invalid-admitted-input-types [:writer-source]))
      types)))

(defn- checked-paths! [paths types]
  (when-not (and (plain-map? paths) (nil? (meta paths))
                 (= (set (keys types)) (set (keys paths))))
    (reject! :incomplete-input-projection [:input-paths]))
  (doseq [[field path] paths]
    (when-not (and (instance? clojure.lang.APersistentVector path)
                   (nil? (meta path))
                   (<= 2 (count path) max-input-path-depth)
                   (#{:context :binding} (first path))
                   (every? #(or (keyword? %)
                                (and (integer? %) (<= 0 % 256)))
                           (rest path))
                   (not-any? application-context-keys (rest path)))
      (reject! :invalid-input-path [:input-paths field])))
  paths)

(defn- lookup-projected [root path]
  (reduce (fn [value part]
            (if (identical? missing-path-value value)
              value
              (cond
                (plain-map? value)
                (get value part missing-path-value)
                (and (vector? value) (integer? part)
                     (<= 0 part) (< part (count value)))
                (nth value part)
                :else missing-path-value)))
          root (rest path)))

(defn- project-inputs [paths context binding]
  (reduce-kv
   (fn [acc field path]
     (let [root (if (= :context (first path)) context binding)
           value (lookup-projected root path)]
       ;; Absent input fields stay absent. The typed semantics distinguishes
       ;; absence from present nil, false and any supported scalar.
       (if (identical? missing-path-value value)
         acc (assoc acc field value))))
   {} paths))

(defn- checked-typed-inputs! [writer-source operation-id paths ctx binding]
  (when-not (identical? original-typed-observe
                        (var-get #'typed/observe-state!))
    (reject! :substituted-typed-input-interpreter []))
  (let [types (typed-input-declaration! writer-source operation-id)
        paths (checked-paths! paths types)
        inputs (project-inputs paths ctx binding)]
    ;; Validate before any executable callback, using the independent S1
    ;; value semantics. The original input values are NEVER put in the report.
    (try
      (original-typed-observe types inputs)
      (catch clojure.lang.ExceptionInfo _
        (reject! :ill-typed-or-missing-input [:input-paths])))
    {:types types :paths paths :projected inputs}))

(defn- construction-input-matches?
  [event original-ctx binding operation-id {:keys [types paths projected]}]
  (and (plain-map? event)
       (nil? (meta event))
       (= input-event-keys (set (keys event)))
       (= resolved/construction-input-observation-scope (:scope event))
       (= :construction-input (:phase event))
       (= operation-id (:operation event))
       (= binding (:binding event))
       (plain-map? (:context event))
       (contains? (:context event) resolved/observation-context-key)
       (contains? (:context event) resolved/construction-input-context-key)
       (let [visible (apply dissoc (:context event) application-context-keys)]
         (and (= original-ctx visible)
              (= projected (project-inputs paths visible (:binding event)))
              (try
                (= projected
                   (do (original-typed-observe types
                                               (project-inputs paths visible binding))
                       (project-inputs paths visible binding)))
                (catch Throwable _ false))))))

(defn- new-input-observer [ctx binding operation-id typed-inputs]
  ;; A single construction-site event is captured. Swallowed hook failures
  ;; remain visible as an INVALID diagnostic, never as a positive witness.
  (let [state (atom {:events 0 :status :missing})]
    {:state state
     :observe! (fn [event]
                 (locking state
                   (let [already (:events @state)
                         valid? (try
                                  (construction-input-matches?
                                   event ctx binding operation-id typed-inputs)
                                  (catch Throwable _ false))]
                     (reset! state
                             {:events (inc already)
                              :status (if (and (zero? already) valid?)
                                        :matched :invalid)}))))}))

(defn- run-core! [source ctx typed-paths]
  (original-functions!)
  (checked-context! ctx)
  (let [{:keys [writer-source operation-id binding configuration]} source]
    (resolution/require-binding binding)
    (when-not (and (qualified-keyword? operation-id)
                   (= operation-id (:operation binding))
                   (= operation-id (:operation configuration)))
      (reject! :operation-source-mismatch [:operation-id]))
    (let [;; Recheck the ORIGINAL writer source before any application callback.
          session (original-acquire {:writer-source writer-source
                                     :operation-id operation-id
                                     :binding binding})
          typed-inputs (when (some? typed-paths)
                         (checked-typed-inputs!
                          writer-source operation-id typed-paths ctx binding))
          input-observer (when typed-inputs
                           (new-input-observer ctx binding operation-id typed-inputs))
          executor (original-build (isolated-configuration configuration))
          controlled-ctx (cond-> (assoc ctx resolved/observation-context-key
                                      (:observe! session))
                           input-observer
                           (assoc resolved/construction-input-context-key
                                  (:observe! input-observer)))
          resolution-result (executor controlled-ctx {:binding binding
                                                     :mode :submit})
          capture (completed-diagnostic session)
          input-capture (when input-observer @(:state input-observer))]
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
       capture
       (when input-observer
         {:typed-input-observation
          {:checking :original-admitted-input-types-and-construction-call-site
           :declared-input-field-ids (set (keys (:types typed-inputs)))
           :projected-field-count (count (:projected typed-inputs))
           :construction-input-events (:events input-capture)
           :construction-site-correlation (:status input-capture)
           :typed-projection-source :caller-supplied-closed-paths
           :typed-input-value-provenance :trusted-invocation-not-authenticated
           :executed-request-to-typed-input-proof :not-established
           :physical-refinement-obligation :typed-input-binding
           :physical-refinement-status :open
           :scope :one-process-local-construction-hook-only
           :certificate :not-issued
           :authority :none}})))))

(defn run-controlled-submit!
  "v1382: Execute one initial submit with an executor-owned transaction
   observer. Return the original resolution and bounded, noncertifying
   acquisition report. No automatic retries, callbacks never see hooks."
  [source ctx]
  (exact! source source-keys [])
  (run-core! source ctx nil))

(defn run-controlled-typed-submit!
  "v1384: Execute one controlled submission with an ADDITIONAL private input
   observer at the original :construct! call site. Exactly one additional
   source key is required: :input-paths maps each ORIGINAL declared abstract
   input reader to a bounded path rooted at :context or :binding, e.g.
   {:input/actor [:context :app/actor]}. Types are drawn from the freshly
   checked original induction catalog, NEVER from caller-supplied type maps.

   Inputs are independently type-checked before application callbacks and
   compared at the actual construction hook. The complete original context,
   typed values, and hook closures remain private. A replay or a swallowed
   observation failure cannot produce :matched. Even :matched is ONLY a
   process-local diagnostic; the path mapping is application-supplied and the
   live request, authentication, deployed handler and physical transaction
   are not independently authenticated. All nine obligations stay OPEN."
  [source ctx]
  (exact! source typed-source-keys [])
  ;; Unlike the ordinary v1382 call, nil cannot disable the typed check.
  (when (nil? (:input-paths source))
    (reject! :incomplete-input-projection [:input-paths]))
  (run-core! (dissoc source :input-paths) ctx (:input-paths source)))

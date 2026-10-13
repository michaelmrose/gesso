(ns gesso.live.optimistic.resolved-operation
  "Explicit trusted adapter from a v792 durable resolved operation into the
   one-argument :execute! contract of gesso.live.optimistic.server/operation.

   Example (the application still owns authentication and business policy):

     (build-executor
      {:operation :request/claim
       :resolved-operation (model-resolved/build-operation {...})
       :submission-mode! (fn [operation-context] :submit-or-:reconcile)
       :project! (fn [operation-context resolution-result]
                   {:resolution :confirmed
                    :authoritative (fresh-authoritative-observation! ...)
                    :outcome :request/claimed})})

   :submission-mode! must return :submit or :reconcile based on TRUSTED server
   evidence, never a browser-provided mode, command ID, or client retry flag.
   The adapter cannot prove that choice correct: a repeated :submit before an
   uncertain first commit is reconciled may be inappropriate. A :reconcile with
   no durable record deliberately remains unresolved rather than initiating
   a transaction.

   :project! must independently obtain truthful authoritative state; recorded
   outcomes are application-owned maps, NOT automatically Live :outcome keys or
   authoritative browser projections. Successful :incorporated/:replay results
   cannot become a :failed settlement, even if publication/projection later
   throws. An error must propagate without inventing a semantic rejection.

   The exact function object returned by build-executor is remembered in a
   private weak registry. construction-evidence can therefore distinguish a
   registered adapter object from copied function metadata, provided the
   running namespace instance and its private registry are trusted. This is
   NOT proof of the underlying model operation or application policy.

   This adapter DOES NOT make function metadata into authenticated proof,
   establish the completeness of domain guards, authenticate principals, make
   notifications durable, or prove whole-application settlement correspondence.
   The checked application still must declare required capabilities and any
   settlement/progression contracts through the ordinary optimistic registry."
  (:require
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command-resolution :as command-resolution]
   [gesso.model.resolved-operation :as resolved-operation])
  (:import
   (java.lang.ref WeakReference)
   (java.util WeakHashMap)))

(def configuration-keys
  #{:operation :resolved-operation :submission-mode! :project!})

(def projected-result-keys
  #{:resolution :authoritative :outcome :reason :progression
    :progression-advances})

(def ^:private incorporated-resolutions
  #{:confirmed :reconciled :rejected})

(def ^:private replay-resolutions
  #{:already-incorporated :reconciled :rejected})

(def assurance-scope
  {:construction :gesso.live.optimistic.resolved-operation/adapter-v1
   :checked #{:registered-operation-binding
              :typed-principal-and-command-correlation
              :closed-execution-mode
              :no-settlement-on-unresolved-command
              :no-failed-resolution-after-incorporation
              :closed-projected-settlement-shape
              :committed-result-provenance}
   :assumed #{:trusted-optimistic-operation-context
              :trusted-submission-mode-selection
              :v792-resolved-operation-implementation
              :authoritative-record-reads-and-disclosure
              :trusted-authoritative-projection
              :correct-domain-guards-and-outcome
              :truthful-progression-evidence}
   :not-established #{:authenticated-construction-provenance
                      :humanhelp-authorization
                      :complete-domain-invariants
                      :lost-acknowledgment-liveness
                      :durable-notification-debt
                      :whole-application-correspondence
                      :universal-linearizability}})

(defn- fail! [kind message detail]
  (throw (ex-info message
                  {:error/type ::invalid-adapter
                   :error/kind kind
                   :error/details detail})))

(defn- require-configuration [config]
  (when-not (and (map? config)
                 (= configuration-keys (set (keys config))))
    (fail! :invalid-configuration
           "Expected the closed durable optimistic adapter configuration."
           {:expected configuration-keys}))
  (let [{:keys [operation resolved-operation submission-mode! project!]} config
        metadata (when (instance? clojure.lang.IMeta resolved-operation)
                   (meta resolved-operation))]
    (when-not (qualified-keyword? operation)
      (fail! :invalid-operation "Operation must be a qualified keyword."
             {:operation operation}))
    (when-not (and (fn? resolved-operation)
                   (= operation (:gesso.model/operation-id metadata))
                   (= :durable-command-v1 (:gesso.model/resolution metadata))
                   (= resolved-operation/assurance-scope
                      (:gesso.model/assurance-scope metadata)))
      (fail! :invalid-resolved-operation
             "Expected a compatible v792 resolved-operation function. Function metadata is not proof of origin."
             {:operation operation}))
    (when-not (fn? submission-mode!)
      (fail! :invalid-mode-selector
             "The trusted submission-mode selector must be a function." {}))
    (when-not (fn? project!)
      (fail! :invalid-projector
             "The authoritative result projector must be a function." {})))
  config)

(defn- require-context [operation context]
  (when-not (and (map? context)
                 (every? #(contains? context %)
                         [:ctx :principal :command-id :execution-id
                          :command :operation :arguments])
                 (map? (:command context))
                 (= operation (:operation context))
                 (= operation (:operation (:command context)))
                 (= (:command-id context) (:command-id (:command context)))
                 (= (:execution-id context) (:execution-id (:command context)))
                 (= (:arguments context) (:arguments (:command context)))
                 (identity/execution-id? (:execution-id context)))
    (fail! :invalid-operation-context
           "Expected a correlated trusted optimistic operation context."
           {:expected-operation operation}))
  ;; Require a persistable canonical binding, not just loosely tagged IDs.
  (command-resolution/require-binding
   {:command-id (:command-id context)
    :principal (:principal context)
    :operation operation
    :arguments (:arguments context)}))

(defn- require-mode [mode]
  (when-not (contains? resolved-operation/modes mode)
    (fail! :invalid-submission-mode
           "Only server-authorized :submit or :reconcile is accepted."
           {:mode mode}))
  mode)

(defn- require-projection [context status result]
  (when-not (and (map? result)
                 (contains? result :resolution)
                 (every? projected-result-keys (keys result)))
    (fail! :invalid-projection
           "Projector must return a closed Live operation-result map."
           {:status status}))
  (let [allowed (case status
                  :incorporated incorporated-resolutions
                  :replay replay-resolutions)]
    (when-not (contains? allowed (:resolution result))
      (fail! :invalid-resolution
             "A durable incorporation or replay cannot be reported as a failed or unrelated Live resolution."
             {:status status :resolution (:resolution result)
              :allowed allowed})))
  ;; Enforce the browser wire's portable settlement structure NOW, rather than
  ;; waiting for the caller to discover it after other application side effects.
  (protocol/settlement
   (merge (select-keys result #{:resolution :authoritative :outcome :reason})
          {:command-id (:command-id context)
           :execution-id (:execution-id context)}))
  ;; :progression and :progression-advances remain trusted application evidence;
  ;; the ordinary server/settlement-contract verifies their declared relation.
  (assoc result :commit/status server/committed-status))

(def ^:private ^WeakHashMap constructed-executors
  ;; Weak keys avoid retaining every executor ever built during hot reload.
  ;; The value must not retain its key strongly; even its identity witness is
  ;; weak. Accesses are synchronized because builders and preflight may run on
  ;; different threads. Construction-time registration is not a domain effect.
  (WeakHashMap.))

(defn construction-evidence
  "Return bounded, runtime-held construction evidence only for the EXACT
   function object returned by this namespace's build-executor.

   Copied metadata, with-meta copies, and unrelated wrappers are not accepted.
   A WeakHashMap alone compares via equals; an explicit identical? check on
   a weak reference prevents a function with a custom equals implementation
   from impersonating a different registered function.

   This attests the local adapter builder and its operation parameter, not
   the origin of the supplied v792 callback, semantic correctness, durable
   incorporation, authorization, or any externally verifiable certificate.
   After a process or namespace reload the evidence may be absent. Do not
   transform absence into a claim of malformed application semantics."
  [executor]
  (when (fn? executor)
    (locking constructed-executors
      (let [{:keys [executor-reference evidence]}
            (.get constructed-executors executor)]
        (when (and executor-reference
                   (identical? executor (.get ^WeakReference executor-reference)))
          evidence)))))

(defn- register-executor!
  [executor operation]
  (locking constructed-executors
    (.put constructed-executors
          executor
          {:executor-reference (WeakReference. executor)
           :evidence
           {:status :constructed-by-live-resolved-operation-adapter
            :evidence-source :private-runtime-construction-registry
            :operation operation
            :construction (:construction assurance-scope)
            :adapter-assurance-scope assurance-scope
            :model-operation-origin :unverified-function-metadata
            :domain-invariants :not-established
            :application-execution-correspondence :not-established}}))
  executor)

(defn build-executor
  "Create an ordinary optimistic.server :execute! function, with untrusted
   informational metadata plus independently queryable local provenance.

   construction-evidence recognizes the exact returned function identity;
   metadata alone cannot spoof that evidence. This authenticates neither the
   supplied v792 resolved-operation callback nor any business policy.

   This is an application-owned trusted adapter. Its mode callback cannot be
   selected by the browser; it must use independent server-side evidence to
   decide whether a fresh submission is authorized. The projector is called
   ONLY on :incorporated or :replay, and must supply authoritative current
   state for affirmative settlement resolutions. Its exceptions propagate.

   :unresolved, :unavailable and :incompatible abort settlement preparation
   with a typed exception. No recorded binding or outcome is disclosed through
   those errors. Applications must implement an explicit recovery/rejection
   protocol rather than translating them to :failed after an unknown commit."
  [config]
  (let [{:keys [operation resolved-operation submission-mode! project!]}
        (require-configuration config)
        executor
        (with-meta
          (fn execute-resolved! [context]
            (let [binding (require-context operation context)
                  mode (require-mode (submission-mode! context))
                  result (resolved-operation (:ctx context)
                                             {:binding binding :mode mode})
                  status (:status result)]
              (if (contains? #{:incorporated :replay} status)
                (require-projection context status (project! context result))
                (fail! :resolution-not-settleable
                       "Authoritative command resolution is not settleable; reconciliation or trusted recovery is required."
                       {:status status}))))
          ;; This metadata is advisory and can be copied. It is NOT the
          ;; identity witness stored by the private construction registry.
          {:gesso.model/operation-id operation
           :gesso.model/resolution :durable-command-v1
           :gesso.model/assurance-scope resolved-operation/assurance-scope
           :gesso.live.optimistic.resolved-operation/assurance-scope assurance-scope})]
    (register-executor! executor operation)))

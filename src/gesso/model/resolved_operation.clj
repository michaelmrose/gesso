(ns gesso.model.resolved-operation
  "Experimental application-facing composition of authoritative Gesso model
   operations with durable command resolution.

   Applications supply ordinary, explicit Clojure functions:

     (build-operation
      {:operation :request/claim
       :read-record! (fn [ctx command-row-id] ...) ; AUTHORITATIVE read
       :may-disclose? (fn [ctx binding record] ...) ; trusted permission
       :construct! (fn [ctx binding]                  ; trusted, PURE plan
                     {:now now :plan plan :outcome outcome})})

   The result is a callable `(fn [ctx {:binding ... :mode ...}])`. Its modes
   are server-owned decisions, NEVER browser-provided authorization:

   * :reconcile: only read a command record and reconcile. An absent record
     remains :unresolved. No domain planning or mutation happens.
   * :submit: reconcile any existing record first. If none is visible, ask the
     trusted application to construct the domain transaction and append a
     command-result creation to the SAME atomic gesso.model.tx plan; then
     commit through gesso.model.tx/transact!. If the commit is ambiguous or
     throws, reread the authoritative record. Never automatically reexecute.

   :submit must be used only from a trusted initial-submission boundary. An
   unknown delivery is not permission to submit again; callers must use
   :reconcile and an explicit higher-level recovery protocol. This layer
   cannot prove that :construct! correctly authenticates/authorizes the
   principal, reads every relevant dependency, expresses sufficient atomic
   guards, avoids effects before commit, or records a truthful outcome.

   After a successful transaction commit, Gesso Live publication and any
   read-your-writes context remain owned by gesso.model.tx. This wrapper does
   not turn its returned outcome into an authoritative browser projection or
   promise durable notification delivery. It is NOT a proof certificate and
   does not register an application operation with Choreo/Live preflight."
  (:require
   [gesso.live.core :as live]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.tx :as tx]))

(def configuration-keys
  #{:operation :read-record! :may-disclose? :construct!})

(def invocation-keys
  #{:binding :mode})

(def construction-keys
  #{:now :plan :outcome})

(def modes #{:submit :reconcile})

(def assurance-scope
  {:construction :gesso.model.resolved-operation/operation-v1
   :checked #{:closed-operation-configuration
              :binding-operation-match
              :reconcile-before-domain-construction
              :no-write-during-reconcile
              :atomic-command-record-plan-construction
              :no-automatic-resubmission-after-uncertain-commit}
   :assumed #{:trusted-server-owned-invocation-mode
              :authoritative-command-record-read
              :trusted-principal-authentication
              :correct-domain-construction-and-atomic-guards
              :trusted-record-disclosure
              :canonical-model-transaction-commit}
   :not-established #{:humanhelp-authorization
                      :complete-domain-invariants
                      :record-immutability-against-other-writers
                      :typed-business-rejection-on-guard-failure
                      :commit-uncertainty-liveness
                      :durable-notification-debt
                      :whole-application-choreo-preflight
                      :universal-linearizability}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-operation
                   :error/kind kind
                   :error/details details})))


;; An *optional*, trusted-context observation hook. This is deliberately not
;; part of the browser command or the operation configuration: the server may
;; install it for controlled diagnostics and disable it in normal operation.
;; No sink, including a sink managed by Gesso, is a transaction oracle.
(def observation-context-key ::observe-transaction!)

(def execution-observation-scope
  {:capture :gesso.model.resolved-operation/transaction-attempt-v1
   :observed #{:submitted-incorporation-plan-at-operation-boundary
               :command-record-and-binding-correlation
               :acknowledged-transaction-result
               :explicit-xtdb-assertion-rejection
               :classified-post-commit-delivery-failure
               :indeterminate-transaction-result}
   :assumed #{:trusted-server-injection-of-observer
              :observer-receives-events-without-loss-or-alteration
              :gesso-model-tx-transact-is-the-real-commit-boundary}
   :not-established #{:observer-event-authentication
                      :physical-transaction-id-for-every-attempt
                      :observer-event-durability
                      :automatic-before-after-xtdb-snapshot
                      :proof-that-tx-was-physically-submitted
                      :complete-application-authorization
                      :policy-guard-completeness
                      :universal-linearizability}})

(defn- observer-for! [ctx]
  (when (and (map? ctx) (contains? ctx observation-context-key))
    (let [observer (get ctx observation-context-key)]
      (when-not (fn? observer)
        (fail! :invalid-observer
               "Transaction observation hook must be a trusted function."
               {:context-key observation-context-key}))
      observer)))

(defn- publish-observation! [observer event]
  ;; The observer must never become part of the authoritative transaction
  ;; boundary. In particular, a callback error *after* commit cannot turn a
  ;; completed mutation into a reported failure. Missing events therefore
  ;; mean NO evidence; consumers must verify complete paired observations.
  (when observer
    (try
      (observer (assoc event :scope execution-observation-scope))
      (catch Throwable _
        nil))))

(defn- physical-result [transaction error]
  (cond
    (and (nil? error) (= :committed (:commit/status transaction)))
    {:physical/status :committed
     :physical/source :gesso-model-transaction-ack
     :physical/receipt (:tx-result transaction)}

    (and error (live/post-commit-delivery-failure? error))
    {:physical/status :committed
     :physical/source :gesso-live-classified-post-commit-failure
     :physical/receipt (:tx-result (ex-data error))}

    (and error (= :xtdb/assert-failed
                  (:xtdb.error/code (ex-data error))))
    {:physical/status :rejected
     :physical/source :xtdb-explicit-assertion-failure}

    :else
    {:physical/status :indeterminate
     :physical/source (if error :unclassified-transaction-exception
                          :unrecognized-transaction-response)}))

(defn- transact-observed! [ctx incorporated binding observer]
  (let [attempt-id (random-uuid)
        base {:attempt-id attempt-id
              :operation (:operation binding)
              :binding binding
              :record (:record incorporated)
              :plan (:plan incorporated)}]
    ;; This event says what THIS wrapper handed to tx/transact!, not what
    ;; XTDB necessarily received. In particular, there is no fabricated
    ;; physical tx-id when a rejection has no receipt.
    (publish-observation! observer (assoc base :phase :prepared))
    (let [executed (try
                     {:transaction (tx/transact! ctx (:plan incorporated))}
                     (catch Exception exception
                       {:error exception}))
          result (physical-result (:transaction executed) (:error executed))]
      (publish-observation! observer
                            (merge base {:phase :physical-outcome} result))
      executed)))

(defn- exact-map! [value expected-keys kind]
  (when-not (and (map? value) (= expected-keys (set (keys value))))
    (fail! kind "Expected an exactly shaped resolved-operation map."
           {:expected expected-keys :received value}))
  value)

(defn- require-configuration [configuration]
  (exact-map! configuration configuration-keys :invalid-configuration)
  (let [{:keys [operation read-record! may-disclose? construct!]} configuration]
    (when-not (qualified-keyword? operation)
      (fail! :invalid-operation-id
             "A resolved operation must have a qualified semantic operation keyword."
             {:operation operation}))
    (doseq [[name f] [[:read-record! read-record!]
                      [:may-disclose? may-disclose?]
                      [:construct! construct!]]]
      (when-not (fn? f)
        (fail! :missing-function
               "A resolved operation requires all three trusted callbacks."
               {:callback name}))))
  configuration)

(defn- require-invocation [operation input]
  (exact-map! input invocation-keys :invalid-invocation)
  (let [{:keys [binding mode]} input]
    (resolution/require-binding binding)
    (when-not (= operation (:operation binding))
      (fail! :operation-mismatch
             "Command binding does not identify this admitted operation."
             {:expected operation :binding-operation (:operation binding)}))
    (when-not (contains? modes mode)
      (fail! :invalid-mode
             "Resolution mode must be an explicit, trusted :submit or :reconcile."
             {:mode mode})))
  input)

(defn- disclose [may-disclose? ctx binding]
  (fn [_binding record]
    (true? (may-disclose? ctx binding record))))

(defn- authoritative-resolution [read-record! may-disclose? ctx binding]
  (let [record (read-record! ctx (resolution/record-id binding))]
    (resolution/reconcile binding record
                          (disclose may-disclose? ctx binding))))

(defn- construction! [construct! ctx binding]
  (let [value (construct! ctx binding)]
    (exact-map! value construction-keys :invalid-construction)
    value))

(defn- returned [result]
  ;; The current operation is not itself an assurance certificate. Preserve
  ;; reconciliation's scope rather than replacing it with a stronger claim.
  (assoc result :operation-scope assurance-scope))

(defn- after-commit-error [read-record! may-disclose? ctx binding]
  ;; Reconciliation is the ONLY way to turn an ambiguous commit exception into
  ;; replay. An absent command record is not a definitive business rejection.
  (returned (authoritative-resolution read-record! may-disclose? ctx binding)))

(defn build-operation
  "Build one callable server-side authoritative-operation wrapper.

   Call it with [ctx {:binding typed-command-binding :mode :submit|:reconcile}].
   Both mode selection and the principal identity must originate from trusted
   server machinery, not unvalidated request parameters.

   For :reconcile, return a result from gesso.model.command-resolution/reconcile.
   For :submit, reconcile existing records first. Only if no record exists,
   obtain a trusted pure domain construction and commit its incorporation plan
   through gesso.model.tx/transact!. A positively acknowledged commit returns
   {:status :incorporated :outcome ... :transaction ...}. A thrown commit or a
   non-committed response is resolved using a fresh authoritative record read;
   if no record is visible its status stays :unresolved. No automatic retry.

   The trusted :construct! callback is responsible for correct authorization,
   dependency guards and truthful effects, and must never execute a domain
   mutation or external effect while constructing the plan. The application
   must register the command-result schema/table and use an authoritative,
   read-after-commit-correct :read-record! implementation."
  [configuration]
  (let [{:keys [operation read-record! may-disclose? construct!]}
        (require-configuration configuration)]
    (with-meta
      (fn resolved-operation [ctx input]
        (let [{:keys [binding mode]} (require-invocation operation input)
              prior (authoritative-resolution read-record! may-disclose?
                                              ctx binding)]
          (cond
            ;; Never plan a domain mutation when a durable record exists,
            ;; even if the supplied binding is incompatible.
            (not= :unresolved (:status prior))
            (returned prior)

            (= :reconcile mode)
            (returned prior)

            :else
            (let [observer (observer-for! ctx)
                  {:keys [now plan outcome]} (construction! construct! ctx binding)
                  incorporated (resolution/incorporation-plan
                                {:binding binding
                                 :now now
                                 :plan plan
                                 :outcome outcome})
                  {:keys [transaction error]}
                  (transact-observed! ctx incorporated binding observer)]
              (if (and (nil? error)
                       (= :committed (:commit/status transaction)))
                (returned {:status :incorporated
                           :outcome outcome
                           :transaction transaction
                           :scope resolution/assurance-scope})
                ;; A physical assertion rejection, a delivery failure after
                ;; commit, and an indeterminate exception all require the
                ;; same *authoritative* reconciliation before returning an
                ;; application-level resolution. They are NOT interchangeable
                ;; physical results. An authoritative read error propagates.
                (after-commit-error read-record! may-disclose? ctx binding))))))
      {:gesso.model/operation-id operation
       :gesso.model/resolution :durable-command-v1
       :gesso.model/assurance-scope assurance-scope})))

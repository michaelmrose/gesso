(ns gesso.model.resolved-family
  "Construct a durable production operation and an admitted finite family from
   ONE application-owned domain construction callback.

   Ordinary applications traditionally author resolved-operation/:construct!
   (ctx, binding) and separately author operation-family/:prepare (state,
   principal, arguments). That duplication can make a finite model agree with
   itself while disagreeing with the actual production business construction.

   build-pair forwards the EXACT supplied :construct! function to
   resolved-operation/build-operation and invokes that SAME function in the
   family's :prepare closure. It does not translate domain commands, infer
   guards, invent policy, or create a second interpreter.

   The application STILL owns :enabled? and :model-input!: the latter maps a
   selected finite prestate, principal and arguments to a trusted CONTEXT and
   a canonical command BINDING for the existing construction callback. Every
   finite construction is validated against its declared principal/arguments
   and the same durable incorporation constraints as production before its
   domain plan is handed to the existing formal-family checker.

   This pairing removes independently authored preparation BUSINESS LOGIC;
   it does NOT prove that the model-input mapping is faithful, that the
   production context is authoritative, that the callback is pure or total,
   that enablement expresses complete authorization, or that the resulting
   resolved operation is currently registered as the actual Live executor.
   Neither construction nor a later TLC verdict certifies an application.
   The returned functions and data may be sensitive; keep them server-local."
  (:require
   [gesso.choreo.identity :as identity]
   [gesso.model.command-resolution :as resolution]
   [gesso.model.operation-family :as family]
   [gesso.model.resolved-operation :as resolved]
   [gesso.model.tx-model :as reference]))

(def configuration-keys
  #{:operation :read-record! :may-disclose? :construct!
    :principals :arguments :enabled? :model-input!})

(def model-input-keys #{:context :binding})

(def assurance-scope
  {:construction :gesso.model.resolved-family/shared-construction-v1
   :checked #{:same-construct-function-used-in-production-and-family
              :closed-production-and-finite-family-configuration
              :canonical-finite-command-binding
              :finite-binding-operation-principal-argument-correspondence
              :complete-durable-incorporation-construction-validation
              :source-derived-finite-domain-plan-admission}
   :assumed #{:truthful-and-complete-finite-choice-domains
              :trusted-state-to-production-context-mapping
              :pure-deterministic-total-domain-construction
              :truthful-state-dependent-enablement-and-authorization
              :correct-registered-production-executor}
   :not-established #{:identity-of-model-context-and-production-context
                      :complete-production-principals-and-arguments
                      :completeness-of-authored-policy-and-guards
                      :purity-of-clojure-callbacks
                      :current-live-executor-registration
                      :all-reachable-states-and-repreparation
                      :actual-xtdb-execution-refinement
                      :browser-server-correspondence
                      :application-certification}})

(defn- fail! [kind]
  ;; A failed callback may contain a private state, command ID or principal.
  ;; No application values, exceptions or arbitrary callback ex-data are
  ;; attached to the exception crossing this construction boundary.
  (throw (ex-info
          "The shared durable-operation family cannot be admitted."
          {:error/type ::invalid-resolved-family :error/kind kind})))

(defn- call-model! [kind f & arguments]
  (try
    (apply f arguments)
    (catch Exception _
      (fail! kind))))

(defn- matching-principal? [declared bound]
  (and (identity/principal? bound)
       (or (= declared bound)
           (= declared (identity/raw-value bound)))))

(defn- finite-plan! [operation model-input! construct! state principal arguments]
  (let [input (call-model! :model-input-exception
                           model-input! state principal arguments)]
    (when-not (and (map? input)
                   (= model-input-keys (set (keys input)))
                   (map? (:context input)))
      (fail! :invalid-model-input))
    (let [{:keys [context binding]} input]
      (try
        (resolution/require-binding binding)
        (catch Exception _
          (fail! :invalid-model-binding)))
      (when-not (and (= operation (:operation binding))
                     (= arguments (:arguments binding))
                     (matching-principal? principal (:principal binding)))
        (fail! :model-binding-choice-mismatch))
      (let [built (call-model! :domain-construction-exception
                               construct! context binding)]
        (when-not (and (map? built)
                       (= resolved/construction-keys (set (keys built))))
          (fail! :invalid-domain-construction))
        ;; Production attaches the durable command result record to this
        ;; EXACT domain construction. Enforce its existing structural limits
        ;; and reserved-table prohibition without invoking any transaction.
        (try
          (resolution/incorporation-plan
           {:binding binding :now (:now built) :outcome (:outcome built)
            :plan (:plan built)})
          (catch Exception _
            (fail! :invalid-durable-incorporation)))
        ;; Admit the ORIGINAL application domain plan. Do not silently project
        ;; away guards, commands or unsupported transaction entries.
        (try
          (reference/admitted-plan (:plan built))
          (catch Exception _
            (fail! :unsupported-formal-domain-plan)))))))

(defn build-pair
  "Build {:resolved-operation ..., :family ..., :scope ...} from one :construct!.

   The returned :resolved-operation is the ordinary callable production
   durable-command operation and must be explicitly supplied to the ordinary
   Live adapter and registered by the application. :family is a standard
   operation-family declaration suitable for formal-family/prepared-competition
   or state-dependent invariant checking. Both use the SAME lexical :construct!
   object, never a separately authored :prepare implementation.

   :model-input! receives [state principal arguments] and returns exactly
   {:context trusted-model-context :binding canonical-command-binding}.
   It is trusted application code and may NOT be assumed to reproduce the
   actual production context. In the finite family, the callback and its
   output are local diagnostics; Gesso invokes no XTDB transaction here."
  [configuration]
  (when-not (and (map? configuration)
                 (= configuration-keys (set (keys configuration)))
                 (qualified-keyword? (:operation configuration))
                 (fn? (:enabled? configuration))
                 (fn? (:model-input! configuration))
                 (fn? (:construct! configuration)))
    (fail! :invalid-configuration))
  (let [{:keys [operation principals arguments enabled? model-input! construct!]}
        configuration
        production (try
                     (resolved/build-operation
                      (select-keys configuration resolved/configuration-keys))
                     (catch Exception _ (fail! :invalid-production-operation)))
        operation-family {:id operation
                          :principals principals
                          :arguments arguments
                          :enabled? enabled?
                          :prepare (fn [state principal args]
                                     (finite-plan! operation model-input!
                                                   construct! state principal args))}]
    ;; Validate the entire finite family NOW, before returning either surface.
    ;; In particular, do not return a production function paired with a
    ;; truncated or duplicate choice declaration.
    (try
      (family/expand-families [operation-family])
      (catch Exception _
        (fail! :invalid-finite-domain)))
    {:kind :gesso.model.resolved-family/shared-construction-v1
     :operation operation
     :resolved-operation production
     :family operation-family
     :scope assurance-scope
     :application-certification :not-established}))

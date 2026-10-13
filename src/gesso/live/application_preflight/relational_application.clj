(ns gesso.live.application-preflight.relational-application
  "Opt-in, fail-closed application construction/startup with a bounded,
   source-interpreted relational-rule gate.

   This entry point accepts the SAME application options as
   application-preflight/require-application-assembly!, plus ordinary Clojure
   data for finite operation families, initial modeled state, relational
   declarations, and exploration bounds. The canonical ApplicationAssembly is
   built first; the current registered operation IDs must then be covered
   exactly, and Gesso must interpret and check every declared relationship over
   the entire bounded, state-dependent exploration. Only a passing model can
   reach the canonical Biff startup path.

   This is an EXPLICIT alternative startup API. It does not mutate Gesso's
   canonical ApplicationAssembly shape, issue a business-policy certificate,
   or pretend that calls to biff.core/start / ordinary Gesso startup cannot
   bypass this optional gate. The result reports finite-model evidence only;
   state, families, argument domains, relationships, and business callback
   semantics remain application supplied. The model is not the database and
   checked families need not agree with production executors. Actual Live and
   XTDB correspondence remains a separate diagnostic obligation.

   Startup reuses the SAME assembly that passed the finite rule gate and
   delegates to the canonical Gesso/Biff lifecycle without bypassing the
   physical browser-artifact or rendered-response checks. A later artifact
   change is still detected by the canonical lifecycle recheck."
  (:require
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]))

(def assurance-scope
  {:checker :gesso.live.application-preflight.relational-application/explicit-startup-finite-v1
   :checked #{:canonical-application-assembly-required
              :fresh-source-derived-operation-registry-coverage
              :source-interpreted-relational-rule-admission
              :bounded-state-dependent-operation-exploration
              :nonpassing-finite-model-prevents-canonical-startup
              :same-checked-assembly-used-for-canonical-startup}
   :assumed #{:opt-in-use-of-this-startup-entry-point
              :truthful-pure-total-operation-family-callbacks
              :representative-complete-modeled-initial-state
              :representative-complete-principal-and-argument-domains
              :complete-and-truthful-declared-business-relationships
              :serial-atomic-commit-reference-semantics}
   :not-established #{:enforcement-on-bypassed-or-direct-biff-startup
                      :source-derived-completeness-of-business-rules
                      :source-derived-completeness-of-policy-dependencies
                      :correspondence-of-family-callbacks-to-live-executors
                      :actual-database-enforcement-of-relations
                      :unbounded-invariant-preservation
                      :business-authorization-of-real-principals
                      :correctness-of-browser-projections
                      :whole-application-certification}})

(def ^:private model-keys
  #{:initial-state :families :declarations :options})

(defn- fail! [kind details]
  (throw (ex-info
          "Relational application construction requires a complete, passing finite model."
          {:error/type ::invalid-relational-application
           :error/kind kind
           :error/details details})))

(defn- validate-model! [model]
  (when-not (and (map? model)
                 (= model-keys (set (keys model))))
    (fail! :invalid-model-options
           {:required-model-keys model-keys
            :provided-model-keys (when (map? model)
                                   (set (keys model)))}))
  model)

(defn- counterexample-summary [verdict]
  (mapv
   (fn [violation]
     (let [path (mapv (fn [step choice]
                        {:operation-id (:family-id choice)
                         :model-instance-id (:operation-id step)
                         :principal-choice-index (:principal-index choice)
                         :argument-choice-index (:argument-index choice)
                         :status (:status step)})
                      (:trace violation)
                      (:family-trace violation))]
       {:rule-id (:rule-id violation)
        :at (:at violation)
        :depth (:depth violation)
        :operation-id (:operation-id (peek path))
        :model-instance-id (:operation-id violation)
        :status (:status violation)
        :transition-path path}))
   (:violations verdict)))

(defn require-relational-application!
  "Build the canonical whole-application assembly and REQUIRE a passing
   bounded relational-rule check in one explicit construction call.

   (require-relational-application!
     {:operation-acquisition-assembly ... :browser-artifact-path ...}
     {:initial-state {[:requests id] request ...}
      :families [{:id :request/claim :principals [...] :arguments [...]
                  :enabled? (fn [state principal arguments] ...)
                  :prepare (fn [state principal arguments] ...)} ...]
      :declarations [{:id ... :source-kind ... :reference-attr ...
                      :target-kind ... :target-where ...}]
      :options {:max-depth 3 :max-transitions 300}})

   All finite model inputs are required. The closed option vocabulary avoids
   accidental omission/misspelling of a source of assurance. The underlying
   validators enforce the six-instance and exploration budgets and reject
   unsupported transaction features and opaque :holds? rule callbacks.

   Returns {:application-assembly <current Gesso assembly>
            :finite-relational-verdict <bounded model verdict>
            :assurance-scope <explicit limits>}.
   If any modeled relation fails, throws with a minimal, non-state-bearing
   counterexample path rather than returning a usable application product.
   Exceptions, stale artifacts, invalid family coverage, and budget exhaustion
   also prevent construction. This is NOT an application proof certificate."
  [application-options model]
  (let [{:keys [initial-state families declarations options]}
        (validate-model! model)
        assembly (application/require-application-assembly! application-options)
        verdict (relations/check-registered-relations
                 assembly initial-state families declarations options)]
    (when-not (= :holds-in-explored-model (:status verdict))
      (fail! :finite-relational-invariant-violation
             {:status (:status verdict)
              :counterexamples (counterexample-summary verdict)
              :rule-ids (get-in verdict [:relational-coverage :rule-ids])}))
    {:application-assembly assembly
     :finite-relational-verdict verdict
     :assurance-scope assurance-scope}))

(defn start-biff-relational-application!
  "Require a passing finite relational application BEFORE starting Biff.

   Equivalent canonical lifecycle shapes:
     (start-biff-relational-application!
       application-options finite-model modules-var start-order)
     (start-biff-relational-application!
       application-options finite-model initial-system modules-var start-order)

   The application assembly is obtained and admitted by
   require-relational-application!, not passed in as a caller-authored report.
   On success, delegate to application/start-biff-application! using EXACTLY
   that assembly. No Biff start occurs if the finite check returns a
   counterexample or fails admission. The existing Gesso/Biff module-order
   requirements, HTML response enforcement, and remaining escape hatches
   are unchanged. This explicit gate does NOT apply to other startup paths."
  ([application-options model modules-var start-order]
   (let [admission (require-relational-application!
                    application-options model)]
     (application/start-biff-application!
      (:application-assembly admission) modules-var start-order)))
  ([application-options model initial-system modules-var start-order]
   (let [admission (require-relational-application!
                    application-options model)]
     (application/start-biff-application!
      (:application-assembly admission)
      initial-system modules-var start-order))))

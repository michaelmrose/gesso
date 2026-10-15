(ns gesso.model.formal-family
  "Bridge application-owned finite operation-family declarations to Gesso's
   admitted, source-derived prepared-transaction formal machine.

   Unlike a formal subject reconstructed from one observed transaction, this
   bridge constructs ONE selected simultaneous competition from the entire
   declared principal/argument cross product at a caller-supplied prestate.
   It expands gesso.model.operation-family and uses its original Clojure
   callbacks, then admits each plan through gesso.model.tx-model. There is
   no second transaction interpreter, no new application DSL, and no Recife
   dependency in this namespace.

   For this deliberately strict first bridge EVERY declared choice must be
   enabled in the selected initial state. If any is disabled the call fails:
   discarding it might conceal a choice enabled after a different operation.
   This is still a PREPARED-plan machine: callbacks are evaluated exactly
   once per choice at the selected prestate and are NOT rebuilt at successor
   states. For changing enablement or state-dependent reconstruction, use the
   separate reachable checker; this bridge makes no such claim.

   Declarations and supplied state are not authenticated against the actual
   application executor. In particular, the bridge proves neither production
   callback fidelity, domain authorization, declaration completeness, nor
   actual XTDB refinement. Its result is INTERNAL potentially sensitive
   diagnostic data, never a public certification report."
  (:require [gesso.model.formal-transition :as formal]
            [gesso.model.operation-family :as family]
            [gesso.model.tx-model :as reference]))

(def assurance-scope
  {:construction :gesso.model.formal-family/declared-prepared-competition-v1
   :checked #{:full-declared-finite-choice-expansion
              :exactly-one-evaluation-per-declared-choice
              :no-silently-disabled-declared-choice
              :canonical-admission-of-every-prepared-plan
              :source-derived-serial-competition-subject}
   :assumed #{:trusted-complete-application-family-declarations
              :trusted-complete-selected-prestate
              :pure-deterministic-side-effect-free-application-callbacks
              :all-declared-choices-enabled-at-selected-prestate
              :serialized-atomic-reference-transactions}
   :not-established #{:registry-or-executor-provenance
                      :complete-production-operation-and-input-enumeration
                      :complete-application-state-universe
                      :correctness-of-authored-business-policy
                      :state-dependent-repreparation-after-first-attempt
                      :authorization-or-principal-authentication
                      :actual-xtdb-implementation-refinement
                      :server-browser-composition
                      :model-checker-verdict
                      :application-certification}})

(defn- fail! [kind message instance-id cause]
  (throw (ex-info message
                  (cond-> {:error/type ::invalid-formal-family
                           :error/kind kind}
                    instance-id (assoc :instance-id instance-id))
                  cause)))

(defn- invoke! [instance-id kind f state]
  (try
    (f state)
    (catch Exception cause
      ;; Caller code may contain private principal, argument, or document
      ;; details. Never embed arbitrary callback exceptions into public reports;
      ;; this diagnostic retains the cause only for local JVM debugging.
      (fail! kind "A declared application family callback failed."
             instance-id cause))))

(defn prepared-competition
  "Construct one admitted finite prepared subject from ALL declared choices.

   `families` is the SAME ordinary finite-family construction accepted by
   operation-family/expand-families. Every declared principal/argument choice
   is checked for enablement and prepared once against `initial-documents`.
   All must return exactly true and nonempty supported transaction plans.
   If a choice is disabled, invalid, or cannot be admitted, NO partial formal
   subject is returned. The choice order is the family's expansion order.

   The :subject is suitable for formal-check/check-prepared!, which explores
   all serial orders of these already prepared plans. :choices preserves the
   unredacted instance-to-declaration correspondence for LOCAL diagnostics.
   Never treat :choices as an authenticated executor registry or proof of
   complete application behavior. No domain callbacks run in TLC."
  [initial-documents families]
  (let [initial (reference/require-state initial-documents)
        {:keys [operations choices]} (family/expand-families families)
        _ (when-not (= (count operations) (count choices))
            (fail! :invalid-expansion
                   "Family expansion did not account for every choice."
                   nil nil))
        plans (mapv
               (fn [operation choice]
                 (let [instance-id (:instance-id choice)
                       enabled (invoke! instance-id :enablement-exception
                                        (:enabled? operation) initial)]
                   (when-not (or (true? enabled) (false? enabled))
                     (fail! :non-boolean-enablement
                            "Declared choice enablement must be true or false."
                            instance-id nil))
                   (when-not enabled
                     (fail! :disabled-declared-choice
                            "Cannot silently omit a declared choice from the prepared formal subject."
                            instance-id nil))
                   (let [plan (invoke! instance-id :preparation-exception
                                       (:prepare operation) initial)]
                     (when-not (and (map? plan)
                                    (vector? (:commands plan))
                                    (seq (:commands plan)))
                       (fail! :empty-or-invalid-plan
                              "Every enabled choice must prepare a nonempty transaction plan."
                              instance-id nil))
                     ;; Deliberately let tx-model reject SQL assertions,
                     ;; custom entries and transaction options: no omissions.
                     (reference/admitted-plan plan))))
               operations choices)
        subject (formal/prepared-subject initial plans)]
    {:kind :gesso.model.formal-family/declared-prepared-competition-v1
     :scope assurance-scope
     :choice-count (count choices)
     :choices choices
     :subject subject}))

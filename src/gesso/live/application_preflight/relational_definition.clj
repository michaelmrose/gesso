(ns gesso.live.application-preflight.relational-definition
  "One source-owned, opt-in construction for a checked Gesso application,
   its bounded relational model, and executor-bound execution comparisons.

   construct! accepts ONE closed declaration containing canonical application
   options plus the finite relational model. It invokes the v834 startup gate's
   admission and the v832 registry-bound comparator against the SAME actual
   ApplicationAssembly and supplied families. The result is an opaque,
   process-local handle, not a caller-editable or durable certificate.

   start-biff! uses precisely the assembly admitted by construct!; compare!
   accepts a genuine v815 executor-bound physical evidence handle and does not
   accept new families or rules. The comparator rechecks application/current
   executor identity before checking an observed attempt.

   THIS DOES NOT make arbitrary Clojure callbacks analyzable, establish
   completeness of finite domains or business policies, ensure the modeled
   starting state matches the database, or make the opt-in construction part of
   other Biff startup paths. Relational model callbacks remain trusted; even
   identical model/implementation behavior may reflect the same bad policy."
  (:require
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]))

(def assurance-scope
  {:checker :gesso.live.application-preflight.relational-definition/one-source-v1
   :checked #{:closed-single-source-declaration
              :same-admitted-application-assembly-for-startup-and-comparison
              :registered-operation-family-coverage-on-admission
              :bounded-source-interpreted-relational-invariants-on-admission
              :frozen-comparator-callback-and-rule-selection
              :executor-identity-recheck-on-observed-comparison
              :no-caller-supplied-replacement-model-at-comparison}
   :assumed #{:explicit-opt-in-to-this-construction-and-startup
              :truthful-pure-total-family-callbacks
              :representative-finite-modeled-state-principals-and-arguments
              :complete-truthful-relational-policy-declarations
              :trusted-executor-bound-physical-evidence-source}
   :not-established #{:enforcement-on-other-application-startup-paths
                      :production-origin-of-modeled-family-callbacks
                      :business-policy-completeness-or-correctness
                      :equality-of-modeled-and-observed-initial-states
                      :all-executions-correspond-to-model
                      :unbounded-invariant-preservation
                      :actual-xtdb-enforcement-of-business-invariants
                      :correctness-of-projection-callbacks
                      :whole-application-certification}})

(def ^:private definition-keys #{:application-options :relational-model})

;; Weak keys avoid keeping a discarded development-generation alive. No stored
;; value may reference the handle; the identity-bearing Object itself is the
;; unforgeable process-local capability, not a public map or copied metadata.
(def ^:private constructions (java.util.WeakHashMap.))

(defn- fail! [kind details]
  (throw (ex-info
          "Relational definition requires a genuine, current, admitted construction."
          {:error/type ::invalid-relational-definition
           :error/kind kind
           :error/details details})))

(defn- construction! [handle]
  (let [entry (locking constructions (.get constructions handle))]
    (when-not entry
      (fail! :unknown-construction {}))
    entry))

(defn- current-construction! [handle]
  (let [{:keys [assembly] :as entry} (construction! handle)]
    (when-not (application/application-assembly? assembly)
      (fail! :stale-application-assembly {}))
    entry))

(defn construct!
  "Admit a single declaration:

     {:application-options <canonical ApplicationAssembly options>
      :relational-model {:initial-state ... :families ...
                         :declarations ... :options ...}}

   Validate the canonical assembly, exactly cover the registered operations,
   exhaustively check declared finite relations, and bind the admitted callbacks
   to the actual registered executors for subsequent diagnostic comparisons.

   Returns an opaque PROCESS-LOCAL handle. No raw state, callbacks, or command
   bindings can be recovered through inspection. This function does not start
   Biff or execute an XTDB transaction. A new definition requires fresh
   construction, rather than editing or replacing a report in place."
  [definition]
  (when-not (and (map? definition)
                 (= definition-keys (set (keys definition))))
    (fail! :invalid-definition
           {:required-keys definition-keys
            :provided-keys (when (map? definition)
                             (set (keys definition)))}))
  (let [{:keys [application-options relational-model]} definition
        {:keys [application-assembly finite-relational-verdict]}
        (relational-app/require-relational-application!
         application-options relational-model)
        {:keys [initial-state families declarations options]} relational-model
        ;; Constructed from the *same* assembly and declarations. This repeats
        ;; the finite check (and thus assumes deterministic pure callbacks),
        ;; but also creates the bound physical-comparison capability before
        ;; admitting the product; a failure cannot leave a usable handle.
        comparator (relations/build-comparator
                    application-assembly initial-state families declarations
                    options)
        rule-ids (get-in finite-relational-verdict
                         [:relational-coverage :rule-ids])
        operation-ids (get-in finite-relational-verdict
                              [:registry-coverage :registered-operation-ids])
        handle (Object.)]
    (locking constructions
      (.put constructions handle
            {:assembly application-assembly
             :comparator comparator
             :rule-ids rule-ids
             :operation-ids operation-ids}))
    handle))

(defn inspect
  "Return only a redacted, scoped summary of a genuine, current construction.
   This is NOT a proof certificate or a representation of the modeled state."
  [handle]
  (let [{:keys [rule-ids operation-ids]} (current-construction! handle)]
    {:status :admitted-finite-relational-construction
     :registered-operation-ids operation-ids
     :relational-rule-ids rule-ids
     :assurance-scope assurance-scope}))

(defn compare!
  "Check one genuine executor-bound execution handle against the admitted
   finite family callback, without accepting replacement source declarations.
   The comparison remains a bounded, diagnostic result, not universal proof."
  [handle execution-evidence-handle]
  (let [{:keys [comparator]} (current-construction! handle)]
    (comparator execution-evidence-handle)))

(defn start-biff!
  "Start Biff through the canonical Gesso lifecycle with EXACTLY the checked
   ApplicationAssembly retained by this relational construction. Caller does
   not supply a new assembly or model. Artifact currentness is rechecked by
   current-construction! and the canonical startup path."
  ([handle modules-var start-order]
   (let [{:keys [assembly]} (current-construction! handle)]
     (application/start-biff-application! assembly modules-var start-order)))
  ([handle initial-system modules-var start-order]
   (let [{:keys [assembly]} (current-construction! handle)]
     (application/start-biff-application!
      assembly initial-system modules-var start-order))))

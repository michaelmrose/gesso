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
   accept new families or rules. explain joins the admitted declared rules to
   the fresh canonical semantic audit without inventing policy completeness.
   The comparator rechecks application/current executor identity before
   checking an observed attempt. Its explanation reports the exact admitted
   finite exploration bounds, visitation counts, and per-rule source matching
   across visited states, never modeled states. An optional, source-owned
   :require-exercised-rule-ids gate rejects an admitted model if any named
   rule never matches a source in the explored states; it does not establish
   domain completeness or guarantee those rules are exercised in production.

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
              :no-caller-supplied-replacement-model-at-comparison
              :source-owned-relational-rules-in-current-semantic-explanation
              :exact-admitted-finite-exploration-scope-in-explanation
              :source-matching-coverage-over-visited-model-states
              :optional-source-owned-rule-exercise-admission-gate}
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
                      :unmodeled-state-and-transition-completeness
                      :actual-xtdb-enforcement-of-business-invariants
                      :correctness-of-projection-callbacks
                      :whole-application-certification
                      :rule-exercise-does-not-establish-all-predicate-branches}})

(def ^:private definition-keys #{:application-options :relational-model})
(def ^:private optional-definition-key :require-exercised-rule-ids)

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

(defn- finite-exploration-summary
  "Redact the already admitted finite verdict into a *descriptive*,
   non-authorizing exploration boundary. NEVER return :family-expansion,
   modeled states, prepared plans, callbacks, or untrusted verdict objects.
   Reusing the recorded check prevents explanation from reexecuting builders.
   A depth-limited search is complete only *through* that depth, not beyond."
  [finite-relational-verdict]
  (let [{:keys [kind max-depth max-transitions reached-depth
                attempted-transitions visited-state-occurrences
                frontier-exhausted?]} (:exploration finite-relational-verdict)]
    {:kind kind
     :status :fully-explored-within-declared-bounds
     :max-depth max-depth
     :max-transitions max-transitions
     :reached-depth reached-depth
     :attempted-transitions attempted-transitions
     :visited-state-occurrences visited-state-occurrences
     :frontier-exhausted? frontier-exhausted?
     :depth-limited? (and (= max-depth reached-depth)
                          (not frontier-exhausted?))
     :modeled-operation-instance-count
     (get-in finite-relational-verdict
             [:registry-coverage :expanded-instance-count])
     :domain-completeness :not-established
     :production-correspondence :not-established}))

(defn- require-exercised-rules!
  "Reject an explicit, source-owned coverage obligation when its declared
   rules never have matching source documents in the actual explored states.
   This is a bounded nonvacuity gate, NOT completeness of predicates, input
   domains, operation families, or the real database. Never leak documents."
  [required-ids declarations relational-coverage]
  (let [declared (set (map :id declarations))]
    (when-not (every? declared required-ids)
      (fail! :unknown-required-rule-ids
             {:unknown-rule-ids (->> required-ids
                                     (remove declared)
                                     (sort-by pr-str)
                                     vec)}))
    (let [reachable (:reachable-state-rule-coverage relational-coverage)
          reached-by-id (into {} (map (juxt :rule-id identity)) reachable)]
      (when-not (and (= declared (set (keys reached-by-id)))
                     (= (count declared) (count reachable)))
        (fail! :incomplete-relational-coverage
               {:declared-rule-ids (mapv :id declarations)}))
      (let [unexercised (->> declarations
                             (map :id)
                             (filter required-ids)
                             (remove #(true? (:ever-matched-source-in-explored-states?
                                              (get reached-by-id %))))
                             vec)]
        (when (seq unexercised)
          (fail! :required-relational-rules-unexercised
                 {:unexercised-rule-ids unexercised
                  :scope :declared-bounded-explored-states}))))))

(defn construct!
  "Admit a single declaration:

     {:application-options <canonical ApplicationAssembly options>
      :relational-model {:initial-state ... :families ...
                         :declarations ... :options ...}
      :require-exercised-rule-ids #{:request/owner-membership-active}}

   Validate the canonical assembly, exactly cover the registered operations,
   exhaustively check declared finite relations, optionally require selected
   rules to match a source in the explored model, and bind the admitted callbacks
   to the actual registered executors for subsequent diagnostic comparisons.

   Returns an opaque PROCESS-LOCAL handle. No raw state, callbacks, or command
   bindings can be recovered through inspection. This function does not start
   Biff or execute an XTDB transaction. A new definition requires fresh
   construction, rather than editing or replacing a report in place."
  [definition]
  (when-not (and (map? definition)
                 (= definition-keys
                    (disj (set (keys definition)) optional-definition-key)))
    (fail! :invalid-definition
           {:required-keys definition-keys
            :optional-key optional-definition-key
            :provided-keys (when (map? definition)
                             (set (keys definition)))}))
  (let [required (get definition optional-definition-key #{})]
    (when-not (and (set? required)
                   (every? qualified-keyword? required))
      (fail! :invalid-required-rule-ids
             {:required-form :set-of-qualified-rule-ids})))
  (let [{:keys [application-options relational-model
                require-exercised-rule-ids]} definition
        required (get definition optional-definition-key #{})
        {:keys [application-assembly finite-relational-verdict]}
        (relational-app/require-relational-application!
         application-options relational-model)
        {:keys [initial-state families declarations options]} relational-model
        relational-coverage (:relational-coverage finite-relational-verdict)
        ;; Check source-owned exercise obligations BEFORE building the second
        ;; comparator. A failing obligation can never produce an opaque handle.
        _ (require-exercised-rules! required declarations relational-coverage)
        ;; Constructed from the *same* assembly and declarations. This repeats
        ;; the finite check (and thus assumes deterministic pure callbacks),
        ;; but also creates the bound physical-comparison capability before
        ;; admitting the product; a failure cannot leave a usable handle.
        comparator (relations/build-comparator
                    application-assembly initial-state families declarations
                    options)
        rule-ids (:rule-ids relational-coverage)
        operation-ids (get-in finite-relational-verdict
                              [:registry-coverage :registered-operation-ids])
        handle (Object.)]
    (locking constructions
      (.put constructions handle
            {:assembly application-assembly
             :comparator comparator
             :rule-ids rule-ids
             :operation-ids operation-ids
             :declarations declarations
             :required-exercised-rule-ids
             (->> declarations (map :id) (filter required) vec)
             :initial-rule-coverage (:initial-state-rule-coverage
                                     relational-coverage)
             :reachable-rule-coverage (:reachable-state-rule-coverage
                                       relational-coverage)
             :finite-exploration
             (finite-exploration-summary finite-relational-verdict)}))
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

(defn explain
  "Explain this current relational construction with its source-owned business
   rule declarations inside the canonical application semantic audit.

   The ordinary ApplicationAssembly's semantic audit cannot infer arbitrary
   Clojure business policy. This *explicit*, opt-in construction possesses
   additional validated finite relational declarations, so it can report
   precisely those rules, their initial modeled coverage, source matching
   across actually visited states, and their bounded model-checking evidence. The assembly's opaque operation-model premises,
   unknown policy inventory, unmodeled rules, and open obligations remain.

   A forged explanation carries no admission authority. Only the opaque handle
   can retrieve this view, and every call checks the same physical artifact
   and current application assembly used for startup and comparison. The
   admitted exploration depth, strict budget, visits, frontier limit, and
   per-rule matching counts are visible without reevaluating any callback. No application
   callbacks, modeled documents, or executor handles are exposed."
  [handle]
  (let [{:keys [assembly declarations initial-rule-coverage
                reachable-rule-coverage operation-ids finite-exploration
                required-exercised-rule-ids]}
        (current-construction! handle)
        ;; application/explain derives a fresh, current semantic audit from the
        ;; actual admitted assembly; never trust a stored or caller-authored
        ;; successful application-report map.
        explanation (application/explain assembly)
        initial-by-id (into {} (map (juxt :rule-id identity))
                            initial-rule-coverage)
        reached-by-id (into {} (map (juxt :rule-id identity))
                            reachable-rule-coverage)
        ;; Both fact sets originate from the already admitted finite verdict.
        ;; Retain the same source-order IDs so a rule cannot silently acquire
        ;; another rule's observation if the producer changes its shape.
        ids (mapv :id declarations)
        _ (when-not (and (= (set ids) (set (keys initial-by-id)))
                         (= (set ids) (set (keys reached-by-id)))
                         (= (count ids) (count initial-rule-coverage)
                            (count reachable-rule-coverage)))
            (fail! :incomplete-relational-coverage
                   {:declared-rule-ids ids}))
        rules (mapv (fn [declaration]
                      (let [id (:id declaration)]
                        (assoc declaration
                               :initial-model-coverage (get initial-by-id id))))
                    declarations)
        rule-coverage
        (mapv (fn [{:keys [id initial-model-coverage]}]
                {:subject [:relational-rule id :finite-invariant-preservation]
                 :class :bounded-model-checked
                 :evidence :admitted-source-interpreted-relational-model
                 :scope :declared-finite-operation-family-reachable-states
                 :initially-exercised?
                 (:initially-exercised? initial-model-coverage)
                 :ever-matched-source-in-explored-states?
                 (:ever-matched-source-in-explored-states?
                  (get reached-by-id id))
                 :not-established
                 #{:unbounded-invariant-preservation
                   :production-implementation-correspondence
                   :business-policy-completeness}})
              rules)
        catalog {:status :partial-source-owned-relational-catalog
                 :declared-count (count rules)
                 :total-business-rule-count :unknown
                 :completeness :not-established
                 :rules rules
                 ;; Additive, ID-keyed observations preserve the existing
                 ;; declaration-plus-initial-coverage shape of :rules.
                 :reachable-state-rule-coverage reachable-rule-coverage}]
    (-> explanation
        (assoc-in [:semantic-audit :business-rule-catalog] catalog)
        (assoc-in [:semantic-audit :relational-assurance]
                  {:status :holds-in-explored-model
                   :source :current-relational-definition
                   :registered-operation-ids operation-ids
                   :relational-rule-ids (mapv :id declarations)
                   :finite-exploration finite-exploration
                   :required-exercised-rule-ids required-exercised-rule-ids
                   :rule-exercise-gate
                   (if (seq required-exercised-rule-ids)
                     :required-rules-matched-in-explored-model
                     :not-requested)
                   :initially-unexercised-rule-ids
                   (mapv :rule-id
                         (remove :initially-exercised? initial-rule-coverage))
                   :became-exercised-during-exploration-rule-ids
                   (mapv :id
                         (filter (fn [{:keys [id initial-model-coverage]}]
                                   (and (not (:initially-exercised?
                                              initial-model-coverage))
                                        (:ever-matched-source-in-explored-states?
                                         (get reached-by-id id))))
                                 rules))
                   :never-matched-in-explored-states-rule-ids
                   (mapv :rule-id
                         (remove :ever-matched-source-in-explored-states?
                                 reachable-rule-coverage))
                   :assurance-scope assurance-scope})
        (update-in [:semantic-audit :assurance-coverage]
                   into rule-coverage))))

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

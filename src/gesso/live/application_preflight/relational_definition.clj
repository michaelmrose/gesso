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
   The finite verdict also retains source-ordered, redacted operation-choice
   exercise counts tied to the admitted registry and expanded family choices.
   An optional :require-attempted-operation-ids gate rejects registered
   operations with no enabled attempts inside the explored finite model;
   rejected transactions count as attempts, but disabled operations do not.
   A separate optional :require-operation-outcomes map requires selected
   registered operations to exercise committed and/or rejected outcomes in
   the finite model; exercising one outcome never implies the other.
   The admitted v856 transition prefix tree is joined back to registered
   operation names, checked against per-instance outcomes, and explained as
   redacted, non-authorizing bounded MODEL transitions, not production traces.
   A genuine executor-bound comparison also identifies explored MODEL tree nodes
   with the same admitted choice and definitive physical outcome, if any. It
   reconstructs each candidate's redacted MODEL prefix through the admitted
   tree, so a candidate can be understood in its modeled predecessor context.
   These prefixes are NOT observed physical histories, and do not establish
   that a modeled predecessor equals the observed XTDB starting state.

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
              :optional-source-owned-rule-exercise-admission-gate
              :source-owned-modeled-operation-choice-exercise-reporting
              :optional-source-owned-operation-attempt-admission-gate
              :optional-source-owned-operation-outcome-admission-gate
              :source-bound-bounded-transition-tree-in-explanation
              :transition-tree-cross-checked-against-exercise-counts
              :executor-bound-comparison-linked-to-bounded-model-choice-outcomes
              :matched-model-transition-prefix-witnesses}
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
                      :rule-exercise-does-not-establish-all-predicate-branches
                      :modeled-operation-exercise-does-not-establish-production-execution
                      :operation-attempts-do-not-establish-successful-commits
                      :modeled-operation-outcomes-do-not-establish-production-outcomes
                      :modeled-transition-tree-is-not-production-execution
                      :equal-concrete-and-modeled-states-for-linked-nodes
                      :physical-execution-trace-refines-modeled-tree
                      :modeled-prefix-witnesses-are-not-physical-history
                      :complete-real-application-state-machine}})

(def ^:private definition-keys #{:application-options :relational-model})
(def ^:private optional-definition-keys
  #{:require-exercised-rule-ids :require-attempted-operation-ids
    :require-operation-outcomes})

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

(def ^:private operation-count-keys
  [:enablement-checks :disabled-checks :enabled-attempts
   :committed-attempts :rejected-attempts])

(defn- admitted-operation-exercise!
  "Summarize the v848 reachable check's ACTUAL recorded instance counts.
   Join only via the exact synthetic IDs in the already admitted expansion.
   The expansion contains principals and arguments; neither leaves this
   boundary. Fail rather than publish plausible but incomplete telemetry."
  [verdict operation-ids]
  (let [choices (get-in verdict [:family-expansion :instances])
        instances (:operation-exercise verdict)
        expected (get-in verdict [:registry-coverage :expanded-instance-count])
        attempted (get-in verdict [:exploration :attempted-transitions])
        family-ids (mapv :family-id choices)
        valid-counts?
        (fn [entry]
          (and (every? (fn [key]
                         (let [value (get entry key)]
                           (and (integer? value) (<= 0 value))))
                       operation-count-keys)
               (= (:enablement-checks entry)
                  (+ (:disabled-checks entry) (:enabled-attempts entry)))
               (= (:enabled-attempts entry)
                  (+ (:committed-attempts entry) (:rejected-attempts entry)))))]
    (when-not
        (and (vector? choices)
             (vector? instances)
             (pos-int? expected)
             (= expected (count choices) (count instances))
             (= (mapv :instance-id choices)
                (mapv :operation-id instances))
             (every? qualified-keyword? family-ids)
             (= (set operation-ids) (set family-ids))
             (= (count family-ids)
                (count (distinct (map :instance-id choices))))
             (every? valid-counts? instances)
             (integer? attempted)
             (<= 0 attempted)
             (= attempted (reduce + (map :enabled-attempts instances))))
      (fail! :incomplete-modeled-operation-exercise
             {:expected-model-instance-count expected
              :registered-operation-ids operation-ids}))
    (let [safe-instances
          (mapv (fn [choice counts]
                  (merge
                   {:model-instance-id (:instance-id choice)
                    :registered-operation-id (:family-id choice)
                    :principal-choice-index (:principal-index choice)
                    :argument-choice-index (:argument-index choice)}
                   (select-keys counts operation-count-keys)
                   {:ever-attempted-in-explored-states?
                    (pos? (:enabled-attempts counts))}))
                choices instances)
          families
          (mapv (fn [id]
                  (let [matching (filterv #(= id (:registered-operation-id %))
                                          safe-instances)
                        totals (into {}
                                     (map (fn [key]
                                            [key (reduce + (map key matching))]))
                                     operation-count-keys)]
                    (assoc totals
                           :registered-operation-id id
                           :modeled-instance-count (count matching)
                           :ever-attempted-in-explored-states?
                           (pos? (:enabled-attempts totals)))))
                operation-ids)]
      {:status :observed-in-declared-bounded-model
       :scope :declared-finite-operation-family-reachable-states
       :registered-operation-exercise families
       :modeled-instance-exercise safe-instances
       :never-attempted-registered-operation-ids
       (mapv :registered-operation-id
             (remove :ever-attempted-in-explored-states? families))
       :never-attempted-modeled-instance-ids
       (mapv :model-instance-id
             (remove :ever-attempted-in-explored-states? safe-instances))
       :not-established
       #{:completeness-of-real-principals-or-arguments
         :faithfulness-of-family-callbacks-to-registered-executors
         :exercise-in-production
         :unbounded-operation-coverage}})))

(defn- admitted-transition-tree!
  "Join the v856 checker-owned transition prefix tree to admitted, redacted
   instance identities. Cross-check every node and outcome with the v848
   exercise counts before publishing an inert application explanation.

   The checker may retain concrete state and callback-bearing diagnostic
   traces elsewhere in its verdict. Nothing from those traces or from the
   family expansion is returned here. Repeated states are distinct nodes."
  [verdict operation-exercise]
  (let [tree (:transition-tree verdict)
        edges (:edges tree)
        instances (:modeled-instance-exercise operation-exercise)
        by-id (into {} (map (juxt :model-instance-id identity)) instances)
        {:keys [attempted-transitions visited-state-occurrences
                max-depth]} (:exploration verdict)]
    (when-not
        (and (map? tree)
             (= :bounded-reachable-transition-prefix-tree (:kind tree))
             (= 0 (:root-node-id tree))
             (= :occurrences-not-deduplicated (:state-identity tree))
             (= :not-established (:production-correspondence tree))
             (vector? edges)
             (integer? attempted-transitions)
             (<= 0 attempted-transitions)
             (= attempted-transitions (count edges))
             (= (inc attempted-transitions)
                (:node-count tree) visited-state-occurrences)
             (pos-int? max-depth)
             (= (count by-id) (count instances)))
      (fail! :incomplete-modeled-transition-tree
             {:reason :invalid-tree-boundary}))
    (let [{:keys [depths counts safe-edges]}
          (reduce
           (fn [{:keys [depths] :as acc} [index edge]]
             (let [{:keys [node-id parent-node-id depth operation-id status]} edge
                   parent-depth (get depths parent-node-id)
                   instance (get by-id operation-id)]
               (when-not
                   (and (map? edge)
                        (= #{:node-id :parent-node-id :depth
                             :operation-id :status} (set (keys edge)))
                        (= (inc index) node-id)
                        (integer? parent-node-id)
                        (< -1 parent-node-id node-id)
                        (integer? parent-depth)
                        (= (inc parent-depth) depth)
                        (<= depth max-depth)
                        (#{:committed :rejected} status)
                        instance)
                 (fail! :incomplete-modeled-transition-tree
                        {:reason :invalid-transition
                         :transition-index index}))
               (-> acc
                   (assoc-in [:depths node-id] depth)
                   (update-in [:counts operation-id status] (fnil inc 0))
                   (update :safe-edges conj
                           {:node-id node-id
                            :parent-node-id parent-node-id
                            :depth depth
                            :model-instance-id operation-id
                            :registered-operation-id
                            (:registered-operation-id instance)
                            :status status}))))
           {:depths {0 0} :counts {} :safe-edges []}
           (map-indexed vector edges))]
      (when-not
          (every?
           (fn [{:keys [model-instance-id committed-attempts
                        rejected-attempts]}]
             (= {:committed committed-attempts
                 :rejected rejected-attempts}
                (merge {:committed 0 :rejected 0}
                       (get counts model-instance-id))))
           instances)
        (fail! :incomplete-modeled-transition-tree
               {:reason :outcomes-disagree-with-exercise}))
      {:kind :bounded-reachable-transition-prefix-tree
       :status :observed-in-declared-bounded-model
       :scope :declared-finite-operation-family-reachable-states
       :root-node-id 0
       :node-count (:node-count tree)
       :edges safe-edges
       :state-identity :occurrences-not-deduplicated
       :domain-completeness :not-established
       :production-correspondence :not-established
       :not-established
       #{:actual-executor-traces
         :complete-real-application-state-machine
         :unbounded-invariant-preservation
         :production-implementation-correspondence}})))

(defn- require-attempted-operations!
  "Reject explicit operation nonvacuity requirements unless each specified
   REGISTERED operation had an enabled modeled attempt in explored states.
   Counting a rejected attempt is intentional: this is an attempt gate, not
   successful-commit coverage, complete branch coverage, or evidence that
   a registered production executor actually ran. The caller must first
   validate the v848 counters against the admitted expanded model choices."
  [required-ids registered-ids operation-exercise]
  (let [known (set registered-ids)]
    (when-not (every? known required-ids)
      (fail! :unknown-required-operation-ids
             {:unknown-operation-ids (->> required-ids
                                          (remove known)
                                          (sort-by pr-str)
                                          vec)}))
    (let [families (:registered-operation-exercise operation-exercise)
          by-id (into {} (map (juxt :registered-operation-id identity)) families)]
      (when-not (and (= known (set (keys by-id)))
                     (= (count registered-ids) (count families)))
        (fail! :incomplete-modeled-operation-exercise
               {:registered-operation-ids registered-ids}))
      (let [unattempted (->> registered-ids
                             (filter required-ids)
                             (remove #(pos? (get-in by-id [% :enabled-attempts] 0)))
                             vec)]
        (when (seq unattempted)
          (fail! :required-operations-unattempted
                 {:unattempted-operation-ids unattempted
                  :scope :declared-bounded-explored-states}))))))

(def ^:private admitted-operation-outcomes [:committed :rejected])
(def ^:private outcome-counter-keys
  {:committed :committed-attempts
   :rejected :rejected-attempts})

(defn- require-operation-outcomes!
  "Demand actual finite-check observations of each explicitly selected outcome.
   Validate operation identity against the same admitted registry and read ONLY
   its already verified v848 per-operation counters. A rejected transaction
   demonstrates a modeled rejected branch, NOT a production rejection, and
   none of these observations prove exhaustive branching or fairness."
  [requirements registered-ids operation-exercise]
  (let [known (set registered-ids)
        unknown (->> (keys requirements) (remove known) (sort-by pr-str) vec)]
    (when (seq unknown)
      (fail! :unknown-required-operation-outcome-ids
             {:unknown-operation-ids unknown}))
    (let [families (:registered-operation-exercise operation-exercise)
          by-id (into {} (map (juxt :registered-operation-id identity)) families)]
      (when-not (and (= known (set (keys by-id)))
                     (= (count registered-ids) (count families)))
        (fail! :incomplete-modeled-operation-exercise
               {:registered-operation-ids registered-ids}))
      (let [missing
            (->> registered-ids
                 (keep (fn [id]
                         (when-let [required (get requirements id)]
                           (let [unseen (->> admitted-operation-outcomes
                                             (filter required)
                                             (remove (fn [outcome]
                                                       (pos? (get-in by-id
                                                                     [id (outcome-counter-keys outcome)]
                                                                     0))))
                                             vec)]
                             (when (seq unseen)
                               {:operation-id id :unobserved-outcomes unseen})))))
                 vec)]
        (when (seq missing)
          (fail! :required-operation-outcomes-unobserved
                 {:unobserved-operation-outcomes missing
                  :scope :declared-bounded-explored-states}))))))

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
      :require-exercised-rule-ids #{:request/owner-membership-active}
      :require-attempted-operation-ids #{:request/claim}
      :require-operation-outcomes {:request/claim #{:committed :rejected}}}

   Validate the canonical assembly, exactly cover the registered operations,
   exhaustively check declared finite relations, optionally require selected
   rules to match a source, operations to receive enabled attempts, and
   selected operation outcomes to occur within the explored model; bind the
   admitted callbacks to the actual
   registered executors for subsequent diagnostic comparisons. No exercise
   gate establishes real-world coverage or production executor invocation.
   Outcome requirements distinguish modeled commits from guard rejections,
   neither of which establishes a corresponding production execution.

   Returns an opaque PROCESS-LOCAL handle. No raw state, callbacks, or command
   bindings can be recovered through inspection. This function does not start
   Biff or execute an XTDB transaction. A new definition requires fresh
   construction, rather than editing or replacing a report in place."
  [definition]
  (when-not (and (map? definition)
                 (= definition-keys
                    (apply disj (set (keys definition)) optional-definition-keys)))
    (fail! :invalid-definition
           {:required-keys definition-keys
            :optional-keys optional-definition-keys
            :provided-keys (when (map? definition)
                             (set (keys definition)))}))
  (let [required-rules (get definition :require-exercised-rule-ids #{})
        required-operations (get definition :require-attempted-operation-ids #{})
        required-outcomes (get definition :require-operation-outcomes {})]
    (when-not (and (map? required-outcomes)
                   (every? qualified-keyword? (keys required-outcomes))
                   (every? (fn [requested]
                             (and (set? requested)
                                  (seq requested)
                                  (every? (set admitted-operation-outcomes)
                                          requested)))
                           (vals required-outcomes)))
      (fail! :invalid-required-operation-outcomes
             {:required-form :map-of-qualified-operation-ids-to-nonempty-outcome-sets
              :allowed-outcomes admitted-operation-outcomes}))
    (when-not (and (set? required-rules)
                   (every? qualified-keyword? required-rules))
      (fail! :invalid-required-rule-ids
             {:required-form :set-of-qualified-rule-ids}))
    (when-not (and (set? required-operations)
                   (every? qualified-keyword? required-operations))
      (fail! :invalid-required-operation-ids
             {:required-form :set-of-qualified-operation-ids})))
  (let [{:keys [application-options relational-model]} definition
        required-rules (get definition :require-exercised-rule-ids #{})
        required-operations (get definition :require-attempted-operation-ids #{})
        required-outcomes (get definition :require-operation-outcomes {})
        {:keys [application-assembly finite-relational-verdict]}
        (relational-app/require-relational-application!
         application-options relational-model)
        {:keys [initial-state families declarations options]} relational-model
        relational-coverage (:relational-coverage finite-relational-verdict)
        ;; Check source-owned exercise obligations BEFORE building the second
        ;; comparator. A failing obligation can never produce an opaque handle.
        _ (require-exercised-rules! required-rules declarations relational-coverage)
        rule-ids (:rule-ids relational-coverage)
        operation-ids (get-in finite-relational-verdict
                              [:registry-coverage :registered-operation-ids])
        ;; Invalid or missing v848 coverage fails before comparator creation.
        operation-exercise (admitted-operation-exercise!
                            finite-relational-verdict operation-ids)
        transition-tree (admitted-transition-tree!
                         finite-relational-verdict operation-exercise)
        _ (require-attempted-operations! required-operations
                                         operation-ids operation-exercise)
        _ (require-operation-outcomes! required-outcomes
                                       operation-ids operation-exercise)
        ;; Constructed from the *same* assembly and declarations. This repeats
        ;; the finite check (and thus assumes deterministic pure callbacks),
        ;; but also creates the bound physical-comparison capability before
        ;; admitting the product; a failure cannot leave a usable handle.
        comparator (relations/build-comparator
                    application-assembly initial-state families declarations
                    options)
        handle (Object.)]
    (locking constructions
      (.put constructions handle
            {:assembly application-assembly
             :comparator comparator
             :rule-ids rule-ids
             :operation-ids operation-ids
             :operation-exercise operation-exercise
             :transition-tree transition-tree
             :declarations declarations
             :required-exercised-rule-ids
             (->> declarations (map :id) (filter required-rules) vec)
             :required-attempted-operation-ids
             (->> operation-ids (filter required-operations) vec)
             :required-operation-outcomes
             (into (array-map)
                   (keep (fn [id]
                           (when-let [requested (get required-outcomes id)]
                             [id (->> admitted-operation-outcomes
                                      (filter requested)
                                      vec)])))
                   operation-ids)
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
   across actually visited states, and their bounded model-checking evidence.
   The assembly's opaque operation-model premises, unknown policy inventory,
   unmodeled rules, and open obligations remain.

   A forged explanation carries no admission authority. Only the opaque handle
   can retrieve this view, and every call checks the same physical artifact
   and current application assembly used for startup and comparison. The
   admitted exploration depth, strict budget, visits, frontier limit, and
   per-rule matching counts and per-choice modeled-operation exercise are
   visible without reevaluating callbacks. The operation-attempt gate records
   requested IDs in registered order and counts rejected attempts as attempts.
   The independent outcome gate reports required committed/rejected branches
   by registered operation and never treats an attempt as proof of a commit.
   The model transition tree is cross-checked with recorded outcome counts;
   it exposes synthetic instance and registered operation IDs, not state or
   production traces. Principal and argument values, callbacks, modeled
   documents, and executor handles are never exposed."
  [handle]
  (let [{:keys [assembly declarations initial-rule-coverage
                reachable-rule-coverage operation-ids finite-exploration
                operation-exercise transition-tree required-exercised-rule-ids
                required-attempted-operation-ids required-operation-outcomes]}
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
                   :modeled-operation-exercise operation-exercise
                   :modeled-transition-tree transition-tree
                   :required-attempted-operation-ids
                   required-attempted-operation-ids
                   :required-operation-outcomes required-operation-outcomes
                   :operation-outcome-gate
                   (if (seq required-operation-outcomes)
                     :required-outcomes-observed-in-explored-model
                     :not-requested)
                   :operation-attempt-gate
                   (if (seq required-attempted-operation-ids)
                     :required-operations-attempted-in-explored-model
                     :not-requested)
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

(defn- modeled-prefix-witnesses!
  "Recover the MODEL-ONLY path from root to each matching node in the admitted
   bounded occurrence tree. The validated tree's parent IDs point strictly to
   earlier nodes. Only the already redacted operation IDs, outcome and depth
   are emitted; no concrete state, inputs, guards, or callbacks are consulted.
   This is a possible modeled predecessor history, not observed execution."
  [transition-tree node-ids]
  (let [root (:root-node-id transition-tree)
        by-node (into {} (map (juxt :node-id identity))
                      (:edges transition-tree))]
    (mapv
     (fn [node-id]
       (loop [cursor node-id
              prefix ()]
         (if (= root cursor)
           {:matched-node-id node-id
            :steps (vec prefix)}
           (let [edge (get by-node cursor)]
             (when-not edge
               (fail! :incomplete-modeled-transition-tree
                      {:reason :missing-model-prefix-node
                       :matched-node-id node-id}))
             (recur (:parent-node-id edge)
                    (conj prefix (select-keys
                                  edge
                                  [:node-id :parent-node-id :depth
                                   :model-instance-id
                                   :registered-operation-id :status])))))))
     node-ids)))

(defn- modeled-transition-context!
  "From an already executor-bound comparison, find only the recorded finite
   tree occurrences with the same synthetic model-choice ID and physical
   outcome. Node IDs are diagnostic locators into explain's admitted model
   tree, NOT a claim about the XTDB state preceding the physical attempt.

   A matching preparation plan does not establish state correspondence; no
   transition candidate (including when the comparator found a counterexample)
   can change the underlying comparison status. Missing modeled occurrences
   are likewise not grounds to reject an otherwise valid physical attempt.
   Returned prefixes describe only modeled predecessor steps, never physical
   state, execution history, or a refinement proof."
  [operation-exercise transition-tree comparison]
  (let [instance-id (:family-instance-id comparison)
        operation-id (:operation comparison)
        physical-status (:physical-status comparison)
        instances (:modeled-instance-exercise operation-exercise)
        instance (first (filter #(= instance-id (:model-instance-id %)) instances))]
    (when-not (and instance
                   (= operation-id (:registered-operation-id instance))
                   (or (nil? physical-status)
                       (#{:committed :rejected} physical-status)))
      (fail! :invalid-execution-transition-identity
             {:registered-operation-id operation-id
              :physical-outcome-available? (some? physical-status)}))
    (let [candidates
          (if (nil? physical-status)
            []
            (->> (:edges transition-tree)
                 (filter (fn [edge]
                           (and (= instance-id (:model-instance-id edge))
                                (= physical-status (:status edge)))))
                 (mapv :node-id)))]
      {:status (cond
                 (nil? physical-status) :physical-outcome-not-reported
                 (seq candidates) :same-choice-and-outcome-explored
                 :else :same-choice-and-outcome-not-explored)
       :modeled-transition-node-ids candidates
       :modeled-prefix-witnesses
       (modeled-prefix-witnesses! transition-tree candidates)
       :prefix-evidence :modeled-occurrence-tree-only
       :matching-dimensions
       (if (nil? physical-status) #{:admitted-model-choice}
           #{:admitted-model-choice :physical-outcome})
       :scope :declared-finite-operation-family-reachable-states
       :modeled-state-equals-observed-state? :not-established
       :production-trace-refinement :not-established
       :not-established
       #{:equality-of-modeled-and-observed-pretransition-state
         :stateful-refinement-of-real-execution
         :production-sequence-or-concurrency-correspondence
         :model-prefix-is-not-observed-physical-history
         :unbounded-model-coverage}})))

(defn compare!
  "Check one genuine executor-bound execution handle against the admitted
   finite family callback, without accepting replacement source declarations.
   Its :modeled-transition-context only LOCATES finite model tree occurrences
   with the same choice and observed outcome; each matching node now carries
   an inert, redacted prefix of MODEL-only predecessors. No prefix establishes
   equality of modeled and XTDB states or changes the comparator's verdict.
   Neither a matching node nor a missing node proves trace correspondence."
  [handle execution-evidence-handle]
  (let [{:keys [comparator operation-exercise transition-tree]}
        (current-construction! handle)
        comparison (comparator execution-evidence-handle)]
    (assoc comparison :modeled-transition-context
           (modeled-transition-context!
            operation-exercise transition-tree comparison))))

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

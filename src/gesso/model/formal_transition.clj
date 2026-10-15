(ns gesso.model.formal-transition
  "A finite, source-derived *prepared-transaction* transition subject.

   This is the common semantic input for model-checker lowering; it is not a
   second transaction interpreter, a proof term, or an application language.
   Each enabled action invokes gesso.model.tx-model/step with an already
   admitted Gesso transaction plan. The source of commit/reject behavior is
   therefore the existing reference semantics, not a copied set of rules.

   The caller supplies a finite prestate and a complete *selected* set of
   already prepared plans. Gesso does not infer omitted writers, eligibility,
   domain policy, or a realistic workload. Every plan is attempted exactly
   once, in all orderings permitted by the checker. No retry, network, or
   external database actions are implied. A model-checking backend must still
   check independent properties of the resulting reachable states and report
   its bounds; the existence of this subject establishes no theorem."
  (:require [gesso.model.tx-model :as reference]))

(def assurance-scope
  {:subject :gesso.model.formal-transition/prepared-serial-v1
   :derived-from :gesso.model.tx/serial-commit-reference-v1
   :checked #{:finite-action-enumeration
              :canonical-plan-admission
              :actual-reference-step-execution
              :single-attempt-per-selected-plan}
   :assumed #{:chosen-state-and-prepared-plans-cover-the-modeled-case
              :serialized-atomic-commit}
   :not-established #{:model-checker-result
                      :completeness-of-enumerated-operations
                      :application-policy-or-authorization
                      :real-xtdb-correspondence
                      :durable-command-identity-and-replay
                      :browser-or-network-behavior
                      :unbounded-refinement}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-formal-transition
                   :error/kind kind
                   :error/details details})))

(defn prepared-subject
  "Construct a finite formal transition subject from existing Gesso plans.

   `plans` must be a vector of 1..6 prepared transaction plans in the subset
   admitted by tx-model. The restriction is deliberate and identical to
   tx-model/explore-orders. Unsupported assertions/entry hooks/tx-options
   fail through tx-model/admitted-plan, rather than being silently erased.

   The result is data (including the admitted plans), not a Recife program.
   Its action vocabulary is [::attempt index], indexed in the supplied order.
   State keys are namespace-qualified for Recife-compatible globals. Backend
   bookkeeping keys are preserved but never interpreted as transaction state.
   This finite vocabulary may be lowered to TLC by a separate test/backend."
  [initial-documents plans]
  (let [initial-documents (reference/require-state initial-documents)]
    (when-not (and (vector? plans) (<= 1 (count plans) 6))
      (fail! :invalid-plans
             "Expected 1..6 already prepared Gesso transaction plans."
             {:count (when (sequential? plans) (count plans))
              :maximum 6}))
    (let [admitted (mapv reference/admitted-plan plans)
          indexes (vec (range (count admitted)))]
      {:kind :gesso.model.formal-transition/prepared-serial-v1
       :scope assurance-scope
       :reference-scope reference/reference-scope
       :plans admitted
       :actions (mapv (fn [index] [::attempt index]) indexes)
       :initial-state {::documents initial-documents
                       ::pending indexes
                       ::observations []}})))

(defn initial-state
  "The initially reachable state of this derived subject."
  [subject]
  (:initial-state subject))

(defn- require-state! [subject state]
  (when-not (and (= :gesso.model.formal-transition/prepared-serial-v1
                    (:kind subject))
                 (= assurance-scope (:scope subject))
                 (vector? (:plans subject))
                 (map? state)
                 (every? #(contains? state %)
                         [::documents ::pending ::observations])
                 (vector? (::pending state))
                 (vector? (::observations state)))
    (fail! :invalid-state
           "Formal transition subject or machine state has an invalid shape."
           {:state state}))
  ;; Recife injects :pc, :self and :recife.core/procs into process states.
  ;; Validate the Gesso-owned fields without rejecting or interpreting these
  ;; backend-owned keys.
  (reference/require-state (::documents state))
  (let [indices (set (range (count (:plans subject))))
        pending (::pending state)
        attempted (mapv :plan-index (::observations state))]
    (when-not (and (= (count pending) (count (distinct pending)))
                   (every? indices pending)
                   (= (vec (sort pending)) pending)
                   (= (count attempted) (count (distinct attempted)))
                   (every? indices attempted)
                   (= indices (into (set pending) attempted)))
      (fail! :invalid-state
             "Pending and already observed plan indices must partition the subject."
             {:pending pending :attempted attempted})))
  state)

(defn enabled-actions
  "Enumerate the currently enabled *abstract* attempt actions.
   This is intentionally not a business-policy or user-authorization guard."
  [subject state]
  (require-state! subject state)
  (mapv (fn [index] [::attempt index]) (::pending state)))

(defn transition
  "Apply an enabled action by calling tx-model/step, not reimplementing it.

   The resulting state records exact reference status and before/after
   documents for independent model-checker invariants and trace replay.
   Invalid, duplicate, or unknown actions fail closed. No hidden stuttering
   step is inserted: terminal states deliberately have no enabled actions."
  [subject state action]
  (require-state! subject state)
  (when-not (some #(= action %) (enabled-actions subject state))
    (fail! :unavailable-action
           "Only a pending action supplied by this subject may execute."
           {:action action :enabled (enabled-actions subject state)}))
  (let [[_ index] action
        result (reference/step (::documents state) (nth (:plans subject) index))]
    (-> state
        (assoc ::documents (:after result))
        (update ::pending (fn [pending]
                           (filterv #(not= index %) pending)))
        (update ::observations conj
                {:plan-index index
                 :status (:status result)
                 :before (:before result)
                 :after (:after result)
                 :failed-precondition (:failed-precondition result)
                 :checks (:checks result)}))))

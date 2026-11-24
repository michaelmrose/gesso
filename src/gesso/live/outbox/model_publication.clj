(ns gesso.live.outbox.model-publication
  "Selected durable subject-to-model correspondence. No arbitrary change
   payload, predicate evaluation at replay, or implicit identity coercion."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.model :as model]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.live.optimistic.server :as server]))

(defn- reject! [reason data]
  (throw (ex-info "Durable model publication not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-model-publication
                         :reason reason))))

(defn- portable-subject? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

(def preparation-key :gesso.live.outbox.model-publication/prepare!)

(defn prepare!
  "Prepare an atomic outbox contribution using the installed command context.
   Supply only :intent-ids and :purpose; changes carry their model identity
   fields. The trusted executor still owns the domain transaction and must
   prefix guard-ops before writes. Not a sandbox for arbitrary writers."
  [ctx spec changes]
  (if-let [prepare (get ctx preparation-key)]
    (prepare spec changes)
    (reject! :missing-command-preparation {})))

(defn profile!
  "Compile an explicit topic -> subject field declaration against the exact
   admitted Live graph. Every graph topic must have at least one unconditional
   target; all targets for a topic must use its one declared identity field.
   The durable :id is a trusted writer assertion of that field, not proof
   of the domain write. Reject unsupported graphs before provider startup.
   Unknown topics and inconsistent supplied fields fail closed on expansion;
   the ordinary Live runtime retains the physical lease progression. Return
   both replay rules and a command-bound producer for the same mapping."
  [assembly options]
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-assembly {}))
  (when-not (and (map? options) (= #{:subject-keys} (set (keys options)))
                 (map? (:subject-keys options)))
    (reject! :invalid-subject-mapping {}))
  (let [compiled (get-in assembly [:operation-acquisition-assembly
                                   :acquisition-assembly :live-app])
        graph (:graph compiled)
        subjects (:subject-keys options)
        operations (get-in assembly [:operation-acquisition-assembly
                                     :execution-assembly :server :operations])]
    (when-not (and (seq graph) (= (set (keys graph)) (set (keys subjects))))
      (reject! :incomplete-topic-mapping {:expected (set (keys graph))
                                         :actual (set (keys subjects))}))
    (doseq [[topic targets] graph]
      (let [field (get subjects topic)]
        (when-not (and (keyword? field)
                       (not (#{:topic :event/type :change/topic :change/kind :progression} field))
                       (seq targets)
                       (every? #(and (= field (:id-key %)) (not (contains? % :when))) targets))
          (reject! :unsupported-subject-graph {:topic topic :field field
                                               :target-fields (set (map :id-key targets))}))))
    ;; A catch-all prevents the Live runtime's unmatched :keep default from
    ;; silently publishing a raw outbox topic instead of a model invalidation.
    {:rules
     [{:when (fn [_ _] true)
      :expand (fn [ctx {:keys [topic id] :as change}]
                (let [field (get subjects topic)]
                  (when-not field
                    (reject! :unknown-change-topic {:topic topic}))
                  (when-not (portable-subject? id)
                    (reject! :invalid-subject {:topic topic}))
                  (when (and (contains? change field) (not= id (get change field)))
                    (reject! :conflicting-subject-field {:topic topic :field field}))
                  (model/expand-change compiled ctx (assoc change field id))))}]
     :bind-command
     (fn [source-id {:keys [operation command-id]}]
       (let [entry (get operations operation)
             topics (when entry (server/operation-published-change-topics entry))]
         (fn [spec changes]
           (when-not (and (map? spec) (= #{:intent-ids :purpose} (set (keys spec))))
             (reject! :invalid-command-intent-spec {}))
           (when-not (and (vector? changes) (seq changes))
             (reject! :missing-model-changes {}))
           (when-not (set? topics)
             (reject! :undeclared-operation-publication {:operation operation}))
           (outbox/prepare!
            (assoc spec :source-id source-id :operation operation
                        :command-id (identity/raw-value command-id))
            (mapv (fn [{:keys [topic] :as change}]
                    (when-not (and (map? change) (contains? topics topic))
                      (reject! :undeclared-operation-topic {:operation operation :topic topic}))
                    (let [field (get subjects topic)
                          id (get change field)]
                      (when-not field
                        (reject! :unknown-change-topic {:topic topic}))
                      (when-not (portable-subject? id)
                        (reject! :invalid-subject {:topic topic :field field}))
                      (when (and (contains? change :id) (not= id (:id change)))
                        (reject! :conflicting-subject-field {:topic topic :field field}))
                      (assoc (select-keys change [:topic :change/kind]) :id id)))
                  changes)))))}))

(defn rules! [assembly options]
  (:rules (profile! assembly options)))

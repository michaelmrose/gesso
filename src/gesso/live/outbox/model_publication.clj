(ns gesso.live.outbox.model-publication
  "Selected durable subject-to-model correspondence. No arbitrary change
   payload, predicate evaluation at replay, or implicit identity coercion."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.model :as model]))

(defn- reject! [reason data]
  (throw (ex-info "Durable model publication not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-model-publication
                         :reason reason))))

(defn- portable-subject? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

(defn rules!
  "Compile an explicit topic -> subject field declaration against the exact
   admitted Live graph. Every graph topic must have at least one unconditional
   target; all targets for a topic must use its one declared identity field.
   The durable :id is a trusted writer assertion of that field, not proof
   of the domain write. Reject unsupported graphs before provider startup.
   Unknown topics and inconsistent supplied fields fail closed on expansion;
   the ordinary Live runtime retains the physical lease progression."
  [assembly options]
  (when-not (application/application-assembly? assembly)
    (reject! :invalid-assembly {}))
  (when-not (and (map? options) (= #{:subject-keys} (set (keys options)))
                 (map? (:subject-keys options)))
    (reject! :invalid-subject-mapping {}))
  (let [compiled (get-in assembly [:operation-acquisition-assembly
                                   :acquisition-assembly :live-app])
        graph (:graph compiled)
        subjects (:subject-keys options)]
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
    [{:when (fn [_ _] true)
      :expand (fn [ctx {:keys [topic id] :as change}]
                (let [field (get subjects topic)]
                  (when-not field
                    (reject! :unknown-change-topic {:topic topic}))
                  (when-not (portable-subject? id)
                    (reject! :invalid-subject {:topic topic}))
                  (when (and (contains? change field) (not= id (get change field)))
                    (reject! :conflicting-subject-field {:topic topic :field field}))
                  (model/expand-change compiled ctx (assoc change field id))))}]))

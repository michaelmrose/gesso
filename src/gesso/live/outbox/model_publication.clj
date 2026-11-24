(ns gesso.live.outbox.model-publication
  "Selected durable subject-to-model correspondence. No arbitrary change
   payload, predicate evaluation at replay, or implicit identity coercion."
  (:require [clojure.string :as str]
            [gesso.live.application-preflight :as application]
            [gesso.live.model :as model]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.optimistic.server :as server])
  (:import [java.io ByteArrayOutputStream DataOutputStream]
           [java.nio ByteBuffer]
           [java.security MessageDigest]
           [java.util UUID]))

(defn- reject! [reason data]
  (throw (ex-info "Durable model publication not admitted."
                  (assoc data :error/type :gesso.live.outbox/invalid-model-publication
                         :reason reason))))

(defn- portable-subject? [id]
  (or (and (string? id) (not (str/blank? id)))
      (keyword? id) (integer? id) (uuid? id)))

(defn- identity-parts [value]
  (cond
    (string? value) ["string" value]
    (keyword? value) ["keyword" (if (namespace value) "qualified" "unqualified")
                      (or (namespace value) "") (name value)]
    (integer? value) ["integer" (str value)]
    (uuid? value) ["uuid" (str value)]
    :else (reject! :unsupported-derived-identity {})))

(defn- derived-intent-id [source operation principal command-id slot]
  ;; Frozen v1 encoding: count each typed component's UTF-16 code units,
  ;; followed by those units in network order. No printer or locale settings
  ;; can affect the identity. SHA-256 truncated into an RFC 9562 UUIDv8.
  (let [buffer (ByteArrayOutputStream.)
        fields (into ["gesso.live.outbox/command-intent-v1"]
                     (mapcat identity-parts)
                     [source operation principal command-id slot])]
    (with-open [out (DataOutputStream. buffer)]
      (doseq [^String field fields]
        (.writeInt out (.length field))
        (.writeChars out field)))
    (let [digest (ByteBuffer/wrap (.digest (MessageDigest/getInstance "SHA-256")
                                          (.toByteArray buffer)))
          high (bit-or (bit-and (.getLong digest) -61441) 32768)
          low (bit-or (bit-and (.getLong digest) 4611686018427387903) Long/MIN_VALUE)]
      (UUID. high low))))

(def preparation-key :gesso.live.outbox.model-publication/prepare!)

(defn prepare!
  "Prepare an atomic outbox contribution using the installed command context.
   Supply :purpose alone for derived retry-stable UUIDs, or :intent-ids
   and :purpose for the existing explicit mode. Derived mode assumes one
   nonempty publication batch per source/operation/principal/command; slot
   zero fences retries even when their proposed changes differ. Do not mix
   explicit and derived modes for one command. Changes carry their model
   identity fields. The trusted executor still owns the domain transaction
   and must prefix guard-ops before writes. Not a sandbox for arbitrary writers."
  [ctx spec changes]
  (if-let [prepare (get ctx preparation-key)]
    (prepare spec changes)
    (reject! :missing-command-preparation {})))

(defn transact!
  "Prepare and execute one command-bound durable publication transaction.
   Put every intent absence guard before the trusted domain transaction
   operations and append the intent write. Return the original XTDB receipt
   (including progression) and prepared :outbox/intents; do not emit or
   acknowledge publication here. Original provider exceptions, including
   committed post-execute-result failures, propagate unchanged.

   Domain ops are trusted, including any domain ASSERTs; this does not
   validate their semantics or prohibit arbitrary raw writes to the outbox.
   Derived mode retains IDs across replacement executions; explicit mode
   requires the caller to retain them. Neither mode reconstructs settlements."
  [ctx spec changes domain-ops]
  (when-not (and (vector? domain-ops) (seq domain-ops))
    (reject! :invalid-domain-ops {}))
  (let [{:keys [intents guard-ops tx-op]} (prepare! ctx spec changes)
        ops (into (into guard-ops domain-ops) [tx-op])
        receipt (xtdb/execute-tx-from! ctx ops)]
    (assoc receipt :outbox/intents intents)))

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
     (fn [source-id {:keys [operation command-id principal]}]
       (let [entry (get operations operation)
             topics (when entry (server/operation-published-change-topics entry))]
         (fn [spec changes]
           (when-not (and (map? spec) (#{#{:intent-ids :purpose} #{:purpose}} (set (keys spec))))
             (reject! :invalid-command-intent-spec {}))
           (when-not (and (vector? changes) (seq changes))
             (reject! :missing-model-changes {}))
           (when-not (set? topics)
             (reject! :undeclared-operation-publication {:operation operation}))
           (let [spec (if (contains? spec :intent-ids)
                        spec
                        (do
                          (when-not (and (identity/principal? principal)
                                         (identity/command-id? command-id)
                                         (portable-subject? (identity/raw-value principal))
                                         (portable-subject? (identity/raw-value command-id)))
                            (reject! :invalid-derived-command-identity {}))
                          (assoc spec :intent-ids
                                 (mapv #(derived-intent-id source-id operation
                                                           (identity/raw-value principal)
                                                           (identity/raw-value command-id) %)
                                       (range (count changes))))))]
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
                    changes))))))}))

(defn rules! [assembly options]
  (:rules (profile! assembly options)))

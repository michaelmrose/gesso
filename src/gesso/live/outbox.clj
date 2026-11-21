(ns gesso.live.outbox
  "Pre-commit construction of minimal, durable Live publication intent.

   This constructs one XTDB :put-docs operation to be submitted in the SAME
   transaction as an authoritative domain mutation. It does not publish or
   acknowledge any notice. A future recovery worker must establish the actual
   commit's provider basis, claim and retry pending intents, and acknowledge
   distribution at a declared boundary. An intent's declared source and scope
   are trusted caller claims, not independent admission/source proofs.

   The caller supplies stable, independently issued UUID intent identities.
   No post-commit callback, recipient list, private domain snapshot, or
   speculative progression token is persisted here. The XTDB transaction
   establishes its provider basis only when it executes."
  (:require [clojure.string :as str]))

(def intent-table :gesso_live_outbox)
(def intent-version 1)
(def intent-type :gesso.live.outbox/publication-intent)

(def ^:private allowed-spec-keys
  #{:intent-ids :source-id :operation :purpose :command-id})

(defn- reject!
  [reason data]
  (throw (ex-info "Invalid Gesso Live publication intent."
                  (assoc data :error/type :gesso.live.outbox/invalid-intent
                         :reason reason))))

(defn- portable-id?
  [x]
  (or (string? x) (keyword? x) (uuid? x) (integer? x)))

(defn prepare!
  "Validate a caller-supplied publication declaration and primary changes,
   then return {:intents [...] :tx-op [:put-docs ...]}.

   All checks happen before the authoritative transaction. :intent-ids must
   provide a distinct UUID per change; retaining them across an uncertain
   retry is the caller's responsibility. :source-id identifies the admitted
   source *as asserted by the trusted caller* (not independently authenticated).
   :operation and :purpose must be qualified keywords. :command-id is optional.

   Only topic/id/change-kind and explicitly declared operation/command metadata
   are persisted. Arbitrary app change payloads, untrusted :progression, and
   private domain data are deliberately excluded. A physical absence/uniqueness
   assertion for an outbox ID is NOT yet installed by this constructor, so it
   does not independently prevent an existing ID from being overwritten."
  [spec changes]
  (when-not (map? spec)
    (reject! :invalid-spec {:value spec}))
  (when-let [unknown (seq (remove allowed-spec-keys (keys spec)))]
    (reject! :unknown-fields {:fields (vec unknown)}))
  (when-not (and (string? (:source-id spec))
                 (not (str/blank? (:source-id spec))))
    (reject! :invalid-source {:value (:source-id spec)}))
  (when-not (qualified-keyword? (:operation spec))
    (reject! :invalid-operation {:value (:operation spec)}))
  (when-not (qualified-keyword? (:purpose spec))
    (reject! :invalid-purpose {:value (:purpose spec)}))
  (when (and (contains? spec :command-id)
             (not (portable-id? (:command-id spec))))
    (reject! :invalid-command-id {:value (:command-id spec)}))
  (when-not (and (vector? changes) (seq changes))
    (reject! :missing-changes {:changes changes}))
  (let [ids (:intent-ids spec)]
    (when-not (and (vector? ids)
                   (= (count ids) (count changes))
                   (every? uuid? ids)
                   (= (count ids) (count (distinct ids)))
                   (not-any? #{(:command-id spec)} ids))
      (reject! :invalid-intent-identities
               {:identities ids :change-count (count changes)}))
    (let [intents
          (mapv
           (fn [intent-id change]
             (when-not (and (map? change)
                            (keyword? (:topic change))
                            (portable-id? (:id change))
                            (or (nil? (:change/kind change))
                                (keyword? (:change/kind change))))
               (reject! :invalid-change {:change change}))
             (cond->
              {:xt/id intent-id
               :gesso.live.outbox/type intent-type
               :gesso.live.outbox/version intent-version
               :gesso.live.outbox/status :pending
               :gesso.live.outbox/source-id (:source-id spec)
               :gesso.live.outbox/operation (:operation spec)
               :gesso.live.outbox/purpose (:purpose spec)
               :gesso.live.outbox/topic (:topic change)
               :gesso.live.outbox/subject-id (:id change)}
               (:change/kind change)
               (assoc :gesso.live.outbox/change-kind (:change/kind change))
               (contains? spec :command-id)
               (assoc :gesso.live.outbox/command-id (:command-id spec))))
           ids changes)]
      {:intents intents
       :tx-op (into [:put-docs intent-table] intents)})))

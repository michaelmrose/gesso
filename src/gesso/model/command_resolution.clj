(ns gesso.model.command-resolution
  "Experimental durable-incorporation construction for gesso.model.

   A server-authenticated application supplies one typed command binding, an
   already-authorized and domain-validated transaction plan, and the outcome
   it wishes to record. This namespace adds ONE canonical command-result
   document creation to that SAME atomic gesso.model.tx plan. A concurrent
   attempt using the same command ID is guarded by the usual create/absence
   precondition. The command-result record can later be reconciled with a
   retry's complete binding without executing the domain operation again.

   A command ID is NOT authority. This layer cannot validate a principal's
   authentication, permission to invoke an operation, completeness of its
   domain guards, accuracy of a supplied outcome, or permission to disclose a
   recorded result. Reconciliation therefore requires an explicit trusted
   disclosure predicate. Missing records and uncertain delivery are never
   reported as proof of business rejection or as permission to retry without
   authoritative rereads.

   This is a transaction-plan construction, NOT an executor or a certificate.
   It assumes the application uses the canonical gesso.model.tx commit path
   and that the command record is retrieved from authoritative storage.
   Further testing is required for real XTDB persistence, serialization, and
   contention with an actual application command."
  (:require
   [clojure.string :as str]
   [gesso.choreo.identity :as identity]
   [gesso.model.command :as command]
   [gesso.model.tx :as tx]))

(def command-entity-type
  "The table for durable application command outcomes. Applications must
   register a compatible schema and use this table consistently."
  :gesso_commands)

(def record-version
  {:revision-key :gesso.command/revision
   :created-at-key :gesso.command/created-at
   :updated-at-key :gesso.command/updated-at})

(def binding-keys
  #{:command-id :principal :operation :arguments})

(def incorporation-keys
  #{:binding :outcome :now :plan})

(def record-keys
  #{:xt/id
    :gesso.command/revision
    :gesso.command/created-at
    :gesso.command/updated-at
    :gesso.command/binding
    :gesso.command/status
    :gesso.command/outcome})

(def assurance-scope
  {:construction :gesso.model.command-resolution/atomic-record-v1
   :checked #{:closed-typed-command-binding
              :atomic-record-create-in-model-plan
              :duplicate-command-id-create-conflict
              :exact-binding-comparison-before-replay}
   :assumed #{:authoritative-read
              :atomic-xtdb-assert-and-write
              :correct-domain-plan-and-outcome
              :trusted-authentication-and-disclosure-predicate}
   :not-established #{:humanhelp-business-authorization
                      :complete-domain-transaction-guards
                      :record-immutability-after-incorporation
                      :durable-notification-debt
                      :unknown-delivery-liveness
                      :end-to-end-xtdb-correspondence
                      :unbounded-concurrent-linearizability}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-resolution
                   :error/kind kind
                   :error/details details})))

(defn- require-keys! [allowed value kind]
  (when-not (and (map? value) (= allowed (set (keys value))))
    (fail! kind "Expected an exactly shaped command-resolution map."
           {:expected allowed :received value}))
  value)

(defn- portable-data?
  "Deliberately conservative nested data subset. Does not promise that every
   downstream schema/codec will retain the same representation."
  [value]
  (cond
    (or (nil? value) (boolean? value) (string? value) (keyword? value)
        (uuid? value) (integer? value)) true
    (and (number? value) (not (Double/isNaN (double value)))
         (not (Double/isInfinite (double value)))) true
    (vector? value) (every? portable-data? value)
    (map? value) (and (every? portable-data? (keys value))
                      (every? portable-data? (vals value)))
    :else false))

(defn- canonical-identity?
  "Require the exact identity constructor shape, not merely identity?'s
   permissive structural recognition of maps with additional fields."
  [expected-kind value]
  (and (= expected-kind (identity/kind value))
       (= value (identity/identity expected-kind (identity/raw-value value)))
       (portable-data? value)))

(defn require-binding
  "Require one *structurally* typed semantic command binding. Principal must
   originate at a trusted server boundary; a tagged map does not authenticate
   it. A command ID must carry a string or UUID usable as the command row ID."
  [binding]
  (require-keys! binding-keys binding :invalid-binding)
  (let [{:keys [command-id principal operation arguments]} binding
        raw-id (when (identity/command-id? command-id)
                 (identity/raw-value command-id))]
    (when-not (and (or (uuid? raw-id)
                       (and (string? raw-id) (not (str/blank? raw-id))))
                   (canonical-identity? :command command-id)
                   (canonical-identity? :principal principal)
                   (keyword? operation)
                   (map? arguments)
                   (portable-data? arguments))
      (fail! :invalid-binding
             "Command binding must use a persistable command ID, typed principal, operation and portable arguments."
             {:binding binding})))
  binding)

(defn record-id
  "The stable XTDB _id assigned to this semantic command within the command
   table. The ID alone must not be treated as proof of authority."
  [binding]
  (identity/raw-value (:command-id (require-binding binding))))

(defn command-record
  "Construct a version-zero persisted *incorporation* record. The supplied
   outcome is application-owned but must be a portable non-empty map; `now`
   is a trusted non-nil timestamp in the application's chosen storage format."
  [binding outcome now]
  (require-binding binding)
  (when-not (and (map? outcome) (seq outcome) (portable-data? outcome))
    (fail! :invalid-outcome "Recorded command outcome must be a nonempty portable map."
           {:outcome outcome}))
  (when (nil? now)
    (fail! :missing-time "A trusted creation timestamp is required." {}))
  {:xt/id (record-id binding)
   :gesso.command/revision 0
   :gesso.command/created-at now
   :gesso.command/updated-at now
   :gesso.command/binding binding
   :gesso.command/status :incorporated
   :gesso.command/outcome outcome})

(defn require-record
  "Reject malformed or inconsistently identified authoritative records.
   This checks record structure, not the trustworthiness of its origin."
  [record]
  (when-not (and (map? record)
                 (= record-keys (set (keys record)))
                 (= :incorporated (:gesso.command/status record))
                 (= 0 (:gesso.command/revision record))
                 (= (:gesso.command/created-at record)
                    (:gesso.command/updated-at record))
                 (command/versioned-document? record record-version)
                 (map? (:gesso.command/outcome record))
                 (seq (:gesso.command/outcome record))
                 (portable-data? (:gesso.command/outcome record))
                 (try
                   (= (:xt/id record)
                      (record-id (:gesso.command/binding record)))
                   (catch clojure.lang.ExceptionInfo _ false)))
    (fail! :invalid-record
           "Authoritative command record has an invalid shape or identity binding."
           {:record record}))
  record)

(defn incorporation-plan
  "Add a durable command outcome to the *same* atomic domain transaction.

   Input is exactly {:binding ..., :outcome ..., :now ..., :plan ...}. Domain
   commands and guards remain unchanged, including SQL assertions; nothing is
   inferred or dropped. Domain plans cannot themselves write to the reserved
   command-result table. The resulting plan is normalized with tx/normalize-plan
   so it can be passed to the ordinary tx/prepare / tx/transact! path.

   A replay must NOT call this function and execute again: first perform an
   authoritative lookup and call reconcile. If a fresh commit has an ambiguous
   result, reread the command record before considering any new attempt.

   The function deliberately does not manufacture Live changes for the record.
   The domain plan must already satisfy its own publish/emit contract."
  [input]
  (require-keys! incorporation-keys input :invalid-incorporation)
  (let [{:keys [binding outcome now plan]} input
        record (command-record binding outcome now)
        normalized (tx/normalize-plan plan)
        _ (when (some #(= command-entity-type (:model/entity-type %))
                      (:commands normalized))
            (fail! :reserved-command-table
                   "Domain plans must not write to the reserved command-result table."
                   {:table command-entity-type}))
        plan' (tx/normalize-plan
               (update normalized :commands
                       conj (command/create command-entity-type record record-version)))]
    {:record record
     :plan plan'
     :scope assurance-scope}))

(defn reconcile
  "Inspect one *authoritatively read* command record with an independently
   trusted disclosure predicate `(fn [binding record] ...)`.

   For absent record return :unresolved, NOT a rejection or permission to
   reexecute. A present record that cannot be disclosed returns :unavailable,
   and never reveals the recorded binding or outcome. A permitted, incompatible
   binding returns :incompatible; only a permitted exact match returns :replay
   and its recorded outcome. Predicates that throw propagate their exception.

   This is a pure decision helper, not the authoritative lookup, authentication
   or retry driver. Do not expose :unresolved as a public existence oracle."
  [binding record may-disclose?]
  (require-binding binding)
  (when-not (ifn? may-disclose?)
    (fail! :missing-disclosure-check
           "Reconciliation requires a trusted disclosure predicate."
           {}))
  (if (nil? record)
    {:status :unresolved :scope assurance-scope}
    (let [record (require-record record)]
      (cond
        (not (true? (may-disclose? binding record)))
        {:status :unavailable :scope assurance-scope}

        (not= binding (:gesso.command/binding record))
        {:status :incompatible :scope assurance-scope}

        :else
        {:status :replay
         :outcome (:gesso.command/outcome record)
         :scope assurance-scope}))))

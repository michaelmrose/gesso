(ns gesso.model.certified-plan
  "Pre-submit, source-bound PLAN gating for one proof-linked Gesso operation (v1251).

   build-certified-plan-operation! accepts the ORIGINAL v1249 registration source,
   the ORIGINAL v1239 guarded-replacement construction, and trusted application
   callbacks that acquire a document, derive abstract input values, and derive
   the one authorized document target. The original :construct! callback is
   wrapped *inside* the canonical resolved-operation execution path. A bad
   candidate is rejected BEFORE tx/transact! can be invoked.

   The supported physical projection is deliberately narrow: ONE versioned
   domain update document whose fields, except :xt/id, are completely and
   injectively mapped from the abstract typed state. A second domain write,
   undeclared field, opaque SQL ASSERT or executable transaction option is
   rejected. The durable command record is added later by resolved-operation,
   independently of this domain plan gate.

   We check a trusted acquired pre-state against the command's :model/before,
   the abstract guard and complete typed assignment/frame relation against
   the ACTUAL :model/after, and command/target identity against a trusted
   target declaration. A caller's :valid? report or candidate :after is NEVER
   evidence; the write is extracted from the canonical model command itself.

   IMPORTANT: acquisition is an application callback, not a verified XTDB
   transaction snapshot. This is an observation gate, not a proof that the
   current state remains current until commit or that predicates/authorization
   and field absence are atomically enforced. Those assumptions, alongside
   proof-kernel soundness and production correspondence, remain OPEN. The
   caller also remains responsible for invoking the exact issued executor at
   the installed server route; v1253 will inspect that assembly boundary.

   This module intentionally does NOT accept create/delete, arbitrary effect
   callbacks, unrestricted document fields, or a caller-supplied executor."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.transition :as construction]
            [gesso.model.certified-operation :as certified]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx])
  (:import (java.util WeakHashMap)))

(def plan-version 1)
(def description-type :gesso.model.certified-plan/copyable-checked-plan-description)
(def witness-type :gesso.model.certified-plan/checked-pre-submit-candidate)
(def ^:private source-keys
  #{:certified-source :construction :field-map :command-operation
    :current-document! :transition-inputs! :target!})
(def ^:private candidate-keys
  #{:construction :field-map :command-operation :target
    :current-document :inputs :plan})
(def ^:private open-obligations
  #{:trusted-current-document-acquisition
    :trusted-invocation-inputs-and-target
    :prestate-to-commit-atomic-guard-enforcement
    :concurrent-field-absence-and-frame-protection
    :callback-purity-and-current-authorization
    :poststate-to-physical-xtdb-correspondence
    :deployed-route-and-handler-adoption
    :application-invariant-preservation})
(def ^:private ^WeakHashMap registrations (WeakHashMap.))

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso certified plan or candidate."
                  (merge {:error/type ::invalid-certified-plan
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value required at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= required actual)
      (reject! :wrong-keys at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual required)})))
  value)

(defn- qualified-operation? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- checked-mapping! [field-map state-fields]
  (when-not (plain-map? field-map)
    (reject! :expected-field-map [:field-map] {}))
  (metadata-free! field-map [:field-map])
  (when-not (= state-fields (set (keys field-map)))
    (reject! :incomplete-field-map [:field-map]
             {:missing (set/difference state-fields (set (keys field-map)))
              :unexpected (set/difference (set (keys field-map)) state-fields)}))
  (when-not (and (every? qualified-operation? (vals field-map))
                 (not-any? #{:xt/id} (vals field-map))
                 (= (count field-map) (count (set (vals field-map)))))
    (reject! :non-injective-or-invalid-field-map [:field-map] {}))
  field-map)

(defn- checked-construction! [original field-map]
  (let [built (construction/build-guarded-replacement! original)]
    (checked-mapping! field-map (:state-projection built))
    built))

(defn- checked-target! [target]
  (when-not (and (vector? target) (= 2 (count target))
                 (keyword? (first target)) (some? (second target)))
    (reject! :invalid-domain-target [:target] {}))
  (metadata-free! target [:target])
  target)

(defn- checked-document! [document target field-map state-types at]
  (when-not (plain-map? document)
    (reject! :expected-complete-document at {}))
  (metadata-free! document at)
  (let [allowed (conj (set (vals field-map)) :xt/id)
        required (conj (into #{} (keep (fn [[state-field document-field]]
                                           (when-not (:optional? (get state-types state-field))
                                             document-field)))
                             field-map)
                       :xt/id)
        actual (set (keys document))]
    (when-not (and (set/subset? required actual)
                   (set/subset? actual allowed))
      (reject! :physical-field-footprint-mismatch at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual allowed)}))
    (when-not (= (second target) (:xt/id document))
      (reject! :document-target-mismatch at {})))
  document)

(defn- project-document [document field-map]
  ;; select-keys preserves absence versus presence of nil, which matters for
  ;; optional fields in the v1233 interpreter.
  (reduce-kv (fn [result state-field document-field]
               (if (contains? document document-field)
                 (assoc result state-field (get document document-field))
                 result))
             {} field-map))

(defn check-candidate!
  "Check ORIGINAL abstract construction against a canonical candidate plan.
   Returned COPYABLE evidence is a scoped observation, never commit authority.
   Only the exact later wrapped :construct! uses this to block submission."
  [candidate]
  (exact-map! candidate candidate-keys [])
  (let [{:keys [construction field-map command-operation target
                current-document inputs plan]} candidate
        built (checked-construction! construction field-map)
        target (checked-target! target)]
    (when-not (keyword? command-operation)
      (reject! :invalid-command-operation [:command-operation] {}))
    (when-not (plain-map? inputs)
      (reject! :expected-input-map [:inputs] {}))
    (metadata-free! inputs [:inputs])
    (checked-document! current-document target field-map
                       (:state-types construction) [:current-document])
    (when-not (plain-map? plan)
      (reject! :expected-plan [:plan] {}))
    (metadata-free! plan [:plan])
    ;; Use the same normalization that the canonical executor uses later.
    (let [normalized (tx/normalize-plan plan)
          commands (:commands normalized)]
      (when (or (seq (:assertions normalized))
                (contains? plan :entry)
                (contains? plan :entry-fn)
                (contains? plan :tx-options))
        (reject! :unsupported-extra-effects [:plan] {}))
      (when-not (= 1 (count commands))
        (reject! :domain-write-count-mismatch [:plan :commands]
                 {:actual-count (count commands)}))
      (let [domain-command (command/require-command (first commands))]
        (when-not (and (command/update? domain-command)
                       (= target (command/target domain-command))
                       (= command-operation (command/operation domain-command)))
          (reject! :domain-command-mismatch [:plan :commands] {}))
        (let [actual-before (command/before domain-command)
              actual-after (command/after domain-command)]
          (checked-document! actual-before target field-map
                             (:state-types construction)
                             [:plan :commands 0 :model/before])
          (checked-document! actual-after target field-map
                             (:state-types construction)
                             [:plan :commands 0 :model/after])
          (when-not (= current-document actual-before)
            (reject! :stale-or-inconsistent-prestate [:plan :commands 0] {}))
          ;; A canonical command expected-version MUST describe the state it
          ;; claims to update. This still does not prove complete CAS coverage.
          (doseq [[field expected]
                  (get-in domain-command [:model/expected :model/checks])]
            (when-not (and (contains? current-document field)
                           (= expected (get current-document field)))
              (reject! :stale-update-check
                       [:plan :commands 0 :model/expected] {})))
          (let [observation
                (abstract/observe-transition!
                 (:transition built)
                 (project-document actual-before field-map)
                 inputs
                 (project-document actual-after field-map))]
            (when-not (:guard-holds? observation)
              (reject! :current-state-guard-failed [:current-document] {}))
            (when-not (:assignments-hold? observation)
              (reject! :declared-write-mismatch [:plan :commands 0 :model/after] {}))
            (when-not (:frame-holds? observation)
              (reject! :frame-preservation-failed
                       [:plan :commands 0 :model/after] {}))
            (when-not (:matches? observation)
              (reject! :abstract-transition-mismatch [:plan] {}))
            {:type witness-type
             :version plan-version
             :operation (get-in construction [:subject :id])
             :target target
             :command-operation command-operation
             :transition-fingerprint (:transition-fingerprint built)
             :subject-fingerprint (:subject-fingerprint built)
             :checked #{:typed-abstract-relation
                       :exact-domain-command-and-target
                       :complete-mapped-physical-document-fields
                       :current-prestate-observation
                       :closed-single-domain-write-before-submit}
             :open-obligations open-obligations
             :checking :pre-submit-candidate-checked
             :prestate-assurance :trusted-observation-only
             :atomicity-assurance :not-established
             :invariant-preservation :not-proved
             :physical-correspondence :not-established
             :transaction-submitted? false
             :authority :none}))))))

(defn- require-source! [source]
  (exact-map! source source-keys [])
  (let [{:keys [certified-source construction field-map command-operation
                current-document! transition-inputs! target!]} source]
    (when-not (and (plain-map? certified-source)
                   (plain-map? (:resolved-configuration certified-source)))
      (reject! :invalid-certified-source [:certified-source] {}))
    (when-not (every? fn? [current-document! transition-inputs! target!])
      (reject! :missing-trusted-acquisition-callback [:callbacks] {}))
    (when-not (fn? (get-in certified-source [:resolved-configuration :construct!]))
      (reject! :missing-trusted-constructor
               [:certified-source :resolved-configuration :construct!] {}))
    (when-not (and (keyword? command-operation)
                   (not= :create command-operation))
      (reject! :invalid-command-operation [:command-operation] {}))
    (let [built (checked-construction! construction field-map)]
      (when-not (= (:transition built)
                   (get-in certified-source [:proof-source :transition]))
        (reject! :proof-transition-source-mismatch [:construction] {}))
      (when-not (= (get-in construction [:subject :id])
                   (:operation certified-source))
        (reject! :subject-operation-mismatch [:construction :subject] {}))
      built)))

(defn- source-match? [original current]
  (and (plain-map? current)
       (nil? (meta current))
       (= source-keys (set (keys current)))
       (= (dissoc original :current-document! :transition-inputs! :target!
                  :certified-source)
          (dissoc current :current-document! :transition-inputs! :target!
                  :certified-source))
       (every? (fn [key]
                 (identical? (get original key) (get current key)))
               [:current-document! :transition-inputs! :target!])
       (identical? (get-in original [:certified-source :certificate])
                   (get-in current [:certified-source :certificate]))
       (= (dissoc (:certified-source original) :resolved-configuration)
          (dissoc (:certified-source current) :resolved-configuration))))

(defn- entry [registered]
  (when (fn? registered)
    (locking registrations (.get registrations registered))))

(defn checked-plan-operation?
  "Only the exact original function built by this module qualifies."
  [registered]
  (boolean (entry registered)))

(defn describe-plan-operation [registered]
  (let [record (or (entry registered)
                   (reject! :unregistered-plan-operation [:operation] {}))
        parent (certified/describe-operation registered)]
    {:type description-type
     :version plan-version
     :operation (:operation parent)
     :proof-fingerprint (:proof-fingerprint parent)
     :transition-fingerprint (:transition-fingerprint record)
     :state-fields (:state-fields record)
     :field-map (:field-map record)
     :command-operation (:command-operation record)
     :checking :registered-pre-submit-abstract-plan-gate
     :open-premises (:open-premises parent)
     :open-obligations (set/union (:open-obligations parent)
                                  open-obligations)
     :source-relative-proof :checked-conditional-abstract-derivation
     :prestate-assurance :trusted-observation-only
     :atomicity-assurance :not-established
     :invariant-preservation :not-proved
     :physical-correspondence :not-established
     :deployed-route-adoption :not-established
     :authority :none}))

(defn require-current-plan-operation!
  "Require the same issued function and fresh assembly-owned original source.
   The caller is responsible for obtaining CURRENT source from actual assembly;
   re-presenting old source cannot detect an independently swapped route."
  [registered current-source]
  (let [record (or (entry registered)
                   (reject! :unregistered-plan-operation [:operation] {}))
        old-source (:source record)]
    (when-not (source-match? old-source current-source)
      (reject! :stale-plan-source [:current-source] {}))
    (require-source! current-source)
    (let [current-config (get-in current-source
                                 [:certified-source :resolved-configuration])]
      (when-not (and (= (set (keys current-config))
                        (set (keys (:original-configuration record))))
                     (every? (fn [[key value]]
                               (if (fn? value)
                                 (identical? value (get current-config key))
                                 (= value (get current-config key))))
                             (:original-configuration record)))
        (reject! :stale-resolved-configuration [:current-source] {})))
    (certified/require-current-operation!
     registered
     {:current-proof-source
      (get-in current-source [:certified-source :proof-source])
      :current-graph (get-in current-source [:certified-source :current-graph])
      :current-configuration (:guarded-configuration record)})
    registered))

(defn build-certified-plan-operation!
  "Issue a real canonical resolved-operation executor with a pre-submit gate.
   The guarded :construct! runs ONLY on the :submit path, after the original
   durable-command reconciliation. All failed candidate checks throw BEFORE
   resolved-operation's incorporation and tx/transact! call."
  [source]
  (let [{:keys [certified-source construction field-map command-operation
                current-document! transition-inputs! target!]} source
        built (require-source! source)
        original-config (:resolved-configuration certified-source)
        original-construct! (:construct! original-config)
        guarded-construct!
        (fn [ctx binding]
          (let [document (current-document! ctx binding)
                inputs (transition-inputs! ctx binding)
                target (target! ctx binding)
                candidate (original-construct! ctx binding)]
            (when-not (and (plain-map? candidate)
                           (= #{:now :plan :outcome} (set (keys candidate))))
              (reject! :invalid-original-construction [:construct!] {}))
            (check-candidate!
             {:construction construction
              :field-map field-map
              :command-operation command-operation
              :target target
              :current-document document
              :inputs inputs
              :plan (:plan candidate)})
            candidate))
        guarded-config (assoc original-config :construct! guarded-construct!)
        registered (certified/build-certified-operation!
                    (assoc certified-source
                           :resolved-configuration guarded-config))
        registration {:source source
                      :original-configuration original-config
                      :guarded-configuration guarded-config
                      :transition-fingerprint (:transition-fingerprint built)
                      :state-fields (:state-projection built)
                      :field-map field-map
                      :command-operation command-operation}]
    (locking registrations (.put registrations registered registration))
    registered))

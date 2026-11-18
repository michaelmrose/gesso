(ns gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial
  "v1775: Bind the v1774 typed full-atomic canonical execution trial to ONE
   selected construction in a checked ORIGINAL v1241 operation-set catalog.

   Unlike v1774, this trial does NOT accept a separate transition declaration,
   invariant inventory, or read-premise list. It rederives the original v1239
   transition and its complete read-premise inventory from the same catalog,
   before and after the one physical trial. Source-relative fingerprints
   detect an operation/rule change, but neither a caller-supplied catalog nor
   a source-derived check proves actual deployed policy provenance, certified
   executor adoption, authorization, or full writer coverage.

   The only physical call is the original v1774 trial's canonical writer.
   Its one-shot and no-retry boundaries are preserved; no capability escapes."
  (:require [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.transition :as construction]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial
             :as canonical])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial/catalog-bound)
(def report-version 1)

(def ^:private original-catalog-check catalog/check-operation-set!)
(def ^:private original-constructor construction/build-guarded-replacement!)
(def ^:private original-canonical-issuer
  canonical/issue-typed-full-atomic-canonical-trial!)

(defn- reject! [reason]
  (throw (ex-info "Original catalog-bound typed canonical trial not established."
                  {:catalog-bound-atomic-canonical-trial/rejection true
                   :reason reason})))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentHashMap v))
       (nil? (meta v))))

(defn- pinned! []
  (when-not (and (identical? original-catalog-check
                             (var-get #'catalog/check-operation-set!))
                 (identical? original-constructor
                             (var-get #'construction/build-guarded-replacement!))
                 (identical? original-canonical-issuer
                             (var-get #'canonical/issue-typed-full-atomic-canonical-trial!)))
    (reject! :substituted-catalog-or-trial-boundary)))

(defn- selected-catalog-source! [source operation]
  (pinned!)
  (when-not (and (plain-map? source)
                 (= #{:catalog :physical-field-map} (set (keys source)))
                 (plain-map? (:catalog source))
                 (plain-map? (:physical-field-map source))
                 (qualified-keyword? operation))
    (reject! :invalid-original-catalog-source))
  (let [source-catalog (:catalog source)
        checked (try (original-catalog-check source-catalog)
                     (catch Exception _ (reject! :invalid-original-operation-set)))
        entries (filterv #(= operation (:id %)) (:operations source-catalog))]
    (when-not (and (= catalog/report-type (:type checked))
                   (= catalog/operation-set-version (:version checked))
                   (= :checked (:exact-declared-operation-coverage checked))
                   (= :closed-declared-abstract-operation-set-only
                      (:scope checked))
                   (= :not-issued (:certificate checked))
                   (= :none (:authority checked))
                   (contains? (:declared-operation-ids checked) operation)
                   (= 1 (count entries))
                   (= (set (keys (:invariants source-catalog)))
                      (:invariant-ids checked)))
      (reject! :catalog-selected-operation-or-rules-not-established))
    (let [original (:construction (first entries))
          built (try (original-constructor original)
                     (catch Exception _ (reject! :original-construction-not-established)))
          declared (:transition built)
          selected-report (first (filter #(= operation (:operation-id %))
                                         (:operation-results checked)))
          invariants (:invariants source-catalog)
          read-targets (into (sorted-map)
                             (for [[target [kind reader]] (:assignments declared)
                                   :when (= kind :read)]
                               [target reader]))
          premises (vec
                    (for [[target reader] read-targets
                          [invariant-id predicate] (sort-by key invariants)
                          :when (= #{target} (:readers predicate))]
                      {:operation-id operation
                       :invariant-id invariant-id
                       :field target
                       :reader reader
                       :reader-origin
                       (if (contains? (:input-types declared) reader)
                         :input :prestate)}))
          field-map (:physical-field-map source)]
      (when-not (and (= construction/construction-type (:type built))
                     (= construction/transition-version (:version built))
                     (= (:transition-fingerprint built)
                        (:transition-fingerprint selected-report))
                     (= (:subject-fingerprint built)
                        (:subject-fingerprint selected-report))
                     (= operation (get-in declared [:subject :id]))
                     (seq read-targets)
                     (= (set (keys read-targets)) (set (map :field premises)))
                     (= (count premises) (count (set premises)))
                     (= (set (keys (:state-types declared)))
                        (set (keys field-map)))
                     (= (count (vals field-map))
                        (count (set (vals field-map)))))
        (reject! :selected-original-declaration-or-premises-diverged))
      (pinned!)
      {:declaration declared
       :field-map field-map
       :premises {:invariants invariants :read-premises premises}
       :catalog-fingerprint (:fingerprint checked)
       :transition-fingerprint (:transition-fingerprint built)})))

(defn- report [operation status]
  {:type report-type
   :version report-version
   :operation-id operation
   :status status
   :declaration-source :original-checked-catalog-construction
   :invariant-inventory :same-original-checked-catalog
   :read-premises :derived-from-original-invariant-rules
   :physical-field-projection :source-bound-declared-bijection
   :actual-canonical-execution
   (if (= status :bounded-catalog-bound-atomic-acknowledgment)
     :single-original-model-transaction-acknowledged
     :not-established)
   :catalog-provenance-in-deployed-application :not-established
   :authentic-business-policy :not-established
   :certified-executor-adoption :not-established
   :complete-deployed-writer-coverage :not-established
   :typed-complete-cas-gateway-adoption :not-established
   :authenticated-xtdb-receipt :not-established
   :actual-live-delivery :not-established
   :retry-authority :none
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})

(defn issue-catalog-bound-canonical-trial!
  "Take ONE original checked operation-set source with an explicitly bound
   physical field projection, a selected operation and invocation inputs.
   Derive the abstract transition and invariant/read-premise inventory from
   that original catalog; do not accept replacement declarations or proof
   contexts. The underlying v1774 guarded canonical trial remains one-use.

   Copyable catalog data alone cannot authenticate deployed application source
   or prove a state-machine theorem. This is source-relative and unadmitted."
  [source operation inputs]
  (when-not (plain-map? inputs)
    (reject! :invalid-typed-invocation-inputs))
  (let [initial (selected-catalog-source! source operation)
        underlying (original-canonical-issuer
                    operation (:declaration initial) inputs
                    (:field-map initial) (:premises initial))
        attempt (AtomicBoolean. false)
        status (atom :not-attempted)
        recheck! (fn []
                   (when-not (= initial (selected-catalog-source! source operation))
                     (reject! :original-catalog-source-changed))
                   (let [s ((:summary! underlying))]
                     (when-not (and (= canonical/report-type (:type s))
                                    (= canonical/report-version (:version s))
                                    (= operation (:operation-id s))
                                    (= :not-issued (:certificate s))
                                    (= :none (:authority s)))
                       (reject! :invalid-original-canonical-trial)))
                   true)
        summary! (fn [] (recheck!) (report operation @status))
        observe! (fn [invoke!]
                   (when-not (.compareAndSet attempt false true)
                     (reject! :catalog-bound-trial-already-attempted))
                   (reset! status :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (fn? invoke!)
                     (reject! :invalid-original-canonical-invocation))
                   (let [observed ((:observe-once! underlying) invoke!)
                         raw-status (:status observed)]
                     (recheck!)
                     (when-not (and (= canonical/report-type (:type observed))
                                    (= canonical/report-version (:version observed))
                                    (= operation (:operation-id observed))
                                    (= :not-issued (:certificate observed))
                                    (= :none (:authority observed)))
                       (reject! :invalid-underlying-canonical-observation))
                     (let [outcome
                           (case raw-status
                             :bounded-typed-full-atomic-canonical-acknowledgment
                             :bounded-catalog-bound-atomic-acknowledgment
                             :typed-full-atomic-canonical-submission-rejected
                             :catalog-bound-canonical-rejected
                             :no-canonical-submission-observed
                             :no-canonical-submission-observed
                             :typed-preparation-not-established-before-submit
                             :catalog-bound-typed-check-not-established
                             :not-qualified-or-uncertain)]
                       (reset! status outcome)
                       (report operation outcome))))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :observe-once! observe!}))

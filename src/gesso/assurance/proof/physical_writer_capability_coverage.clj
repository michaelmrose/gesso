(ns gesso.assurance.proof.physical-writer-capability-coverage
  "v1523: Join process-local, host-observed writer registrations with HOST-
   DECLARED target/mutation capabilities and a rechecked finite XTDB protection
   domain. This is a diagnostic of POTENTIAL physical write access, not a
   capability system or a claim that declarations restrict deployed writers.

   Source:
   {:writer-inventory-source <v1489 original writer source>
    :protected-domain-source <v1412 source>
    :writer-capabilities
    {<registered path-id>
     {:writer <identical callable from the observed registration>
      :target-scope :enumerated|:unrestricted|:unknown
      :document-targets #{[table id] ...}
      :table-targets #{table ...}
      :mutation-forms :unknown|#{:put-docs :patch-docs :delete-docs
                                  :erase-docs :sql-dml :other}
      :restriction :host-declared|:none|:unknown}}}}

   :enumerated only describes the supplied scopes. A table scope can reach
   documents NOT named by the finite protected-domain declaration. :put-docs
   always denotes full-document replacement, not a known-key delta. The
   checker never invokes writer functions or XTDB and does not authenticate
   supplied capability declarations, registration completeness, sink binding,
   deployment, or physical refinement. Returned reports omit writer functions,
   document identifiers, table names and protected field names."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-writer-inventory :as inventory]
            [gesso.assurance.proof.protected-write-domain :as domain])
  (:import [java.security SecureRandom]
           [java.util Base64]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(def report-type
  :gesso.assurance.proof.physical-writer-capability-coverage/host-declared-access)
(def report-version 1)

(def ^:private source-keys
  #{:writer-inventory-source :protected-domain-source :writer-capabilities})
(def ^:private capability-keys
  #{:writer :target-scope :document-targets :table-targets :mutation-forms
    :restriction})
(def ^:private mutation-kinds
  #{:put-docs :patch-docs :delete-docs :erase-docs :sql-dml :other})
(def ^:private original-inventory inventory/describe-physical-writer-inventory!)
(def ^:private original-domain domain/derive-protected-write-domain!)
(def ^:private original-domain-check
  domain/require-current-protected-write-domain!)
(def ^:private max-paths 512)
(def ^:private max-targets 256)
(def ^:private binding-key
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    bytes))

(defn- reject! [reason at]
  (throw (ex-info "Invalid physical writer-capability coverage source."
                  {:physical-writer-capability-coverage/rejection true
                   :reason reason :at at})))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentHashMap v))
       (nil? (meta v))))

(defn- plain-set? [v]
  (and (instance? clojure.lang.PersistentHashSet v)
       (nil? (meta v))))

(defn- exact! [v fields at]
  (when-not (and (plain-map? v) (= fields (set (keys v))))
    (reject! :invalid-source-shape at))
  v)

(defn- target? [v]
  (and (vector? v) (nil? (meta v)) (= 2 (count v))
       (keyword? (first v))
       (some? (second v))
       (not (coll? (second v)))
       (not (fn? (second v)))))

(defn- seams! []
  (when-not (and (identical? original-inventory
                             (var-get #'inventory/describe-physical-writer-inventory!))
                 (identical? original-domain
                             (var-get #'domain/derive-protected-write-domain!))
                 (identical? original-domain-check
                             (var-get #'domain/require-current-protected-write-domain!)))
    (reject! :substituted-source-checker [])))

(defn- inventory! [source]
  (let [r (try (original-inventory source)
               (catch Exception _
                 (reject! :invalid-writer-inventory-source
                          [:writer-inventory-source])))]
    (when-not (and (= inventory/inventory-type (:type r))
                   (= inventory/inventory-version (:version r))
                   (= :not-established (:complete-physical-writer-coverage r))
                   (= :not-established (:writer-function-and-physical-sink-correspondence r))
                   (= :not-issued (:certificate r))
                   (= :not-admitted (:admission r))
                   (= :none (:authority r)))
      (reject! :unexpected-writer-inventory-contract
               [:writer-inventory-source]))
    r))

(defn- protected! [source]
  (let [r (try (let [r (original-domain source)]
                 (original-domain-check source r))
               (catch Exception _
                 (reject! :invalid-protected-domain-source
                          [:protected-domain-source])))]
    (when-not (and (= domain/domain-type (:type r))
                   (= domain/domain-version (:version r))
                   (= :declared-finite-domain-only (:proof-status r))
                   (= :not-established (:writer-path-closure r))
                   (= :not-issued (:certificate r))
                   (= :not-admitted (:admission r))
                   (= :none (:authority r)))
      (reject! :unexpected-protected-domain-contract
               [:protected-domain-source]))
    r))

(defn- registrations! [source report]
  ;; A SECOND survey ties descriptors to actual current callable identities.
  ;; The first survey is independently rechecked by the v1489 source checker.
  ;; No writer callable is invoked. Even stable registrations are not a proof
  ;; that other registrations/processes do not exist.
  (let [paths (try ((:enumerate-write-paths! source))
                   (catch Exception _
                     (reject! :failed-fresh-writer-survey
                              [:writer-inventory-source])))
        _ (when-not (plain-map? paths)
            (reject! :changed-or-invalid-observed-writer-survey
                     [:writer-inventory-source]))
        rows (mapv (fn [[path-id entry]]
                     {:path-id path-id
                      :kind (:kind entry)
                      :claimed-operation-id (:operation-id entry)
                      :declared-issued-operation?
                      (and (= :issued-operation (:kind entry))
                           (contains? (:independent-operation-ids report)
                                      (:operation-id entry)))
                      :physical-sink-binding :not-established})
                   (sort-by (comp str key) paths))]
    (when-not (and (plain-map? paths)
                   (<= (count paths) max-paths)
                   (= (:writer-rows report) rows)
                   (every? (fn [[_ entry]]
                             (and (plain-map? entry)
                                  (= #{:kind :operation-id :writer}
                                     (set (keys entry)))
                                  (fn? (:writer entry))))
                           paths))
      (reject! :changed-or-invalid-observed-writer-survey
               [:writer-inventory-source]))
    paths))

(defn- capability! [path-id observed supplied]
  (exact! supplied capability-keys [:writer-capabilities path-id])
  (when-not (identical? (:writer observed) (:writer supplied))
    (reject! :writer-callable-identity-mismatch
             [:writer-capabilities path-id :writer]))
  (let [{:keys [target-scope document-targets table-targets mutation-forms
                restriction]} supplied]
    (when-not (and (#{:enumerated :unrestricted :unknown} target-scope)
                   (plain-set? document-targets)
                   (plain-set? table-targets)
                   (<= (count document-targets) max-targets)
                   (<= (count table-targets) max-targets)
                   (every? target? document-targets)
                   (every? keyword? table-targets)
                   (or (= :enumerated target-scope)
                       (and (empty? document-targets)
                            (empty? table-targets)))
                   (or (= :unknown mutation-forms)
                       (and (plain-set? mutation-forms)
                            (set/subset? mutation-forms mutation-kinds)))
                   (#{:host-declared :none :unknown} restriction))
      (reject! :invalid-or-ambiguous-capability
               [:writer-capabilities path-id]))
    supplied))

(defn- secret-binding [source paths]
  ;; Unlike redacted counts, this changes when a declared target or callable
  ;; identity changes. It is an in-process HMAC, NOT a deployment signature.
  (let [callables (mapv (fn [[id entry]]
                          [id (System/identityHashCode (:writer entry))])
                        (sort-by (comp str key) paths))
        mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. binding-key "HmacSHA256"))
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
                     (.doFinal mac (.getBytes (pr-str [source callables])
                                              "UTF-8")))))

(defn- row [i [_ observed] capability protected-by-target declared-operations]
  (let [{:keys [target-scope document-targets table-targets mutation-forms
                restriction]} capability
        declared-targets (set (keys protected-by-target))
        enumerated? (= :enumerated target-scope)
        overlapping (if enumerated?
                      (set (filter (fn [[table :as target]]
                                     (or (contains? document-targets target)
                                         (contains? table-targets table)))
                                   declared-targets))
                      declared-targets)
        partial (set (filter #(not= :all (get protected-by-target %))
                             overlapping))
        outside? (or (not enumerated?)
                     (seq (set/difference document-targets declared-targets))
                     ;; Every table scope can address unnamed documents,
                     ;; including in a table that has one protected document.
                     (seq table-targets))
        potential-mutation? (or (= :unknown mutation-forms)
                                (seq mutation-forms))
        possible? (and potential-mutation? (seq overlapping))]
    {:index i
     :kind (:kind observed)
     :declared-issued-operation?
     (and (= :issued-operation (:kind observed))
          (contains? declared-operations (:operation-id observed)))
     :target-scope target-scope
     :restriction-declaration restriction
     :mutation-form-count (if (= :unknown mutation-forms)
                            :unknown (count mutation-forms))
     :whole-document-replacement-advertised?
     (or (= :unknown mutation-forms)
         (contains? mutation-forms :put-docs))
     :unrestricted-or-unknown-mutations? (= :unknown mutation-forms)
     :noncanonical-mutation-advertised?
     (boolean (or (= :unknown mutation-forms)
                  (seq (disj mutation-forms :put-docs))))
     :potentially-accessible-protected-document-count (count overlapping)
     :potentially-accessible-partially-protected-document-count (count partial)
     :potential-protected-mutation? (boolean possible?)
     :possible-unlisted-document-access? (boolean outside?)
     :actual-target-restriction-enforced? :not-established
     :physical-sink-binding :not-established}))

(def ^:private permanent-obligations
  #{:enumerator-and-capability-declarations-exhaustive-and-accurate
    :callable-to-physical-sink-binding
    :all-processes-and-external-writers-mediated
    :host-target-and-mutation-restrictions-enforced
    :unlisted-documents-and-tables-covered
    :actual-submitted-transaction-vector-binding
    :transaction-internal-prestate-and-poststate-causality
    :physical-refinement-and-certification})

(defn inspect-writer-capabilities!
  "Join the v1489 registered callable survey with host-supplied descriptions
   of where each callable MAY write, and the v1412 caller-declared finite
   protection domain. Requires precisely one capability entry per surveyed
   registration and identity-equal callables. No writer or XTDB is invoked.

   A finite descriptor with no listed protected targets is only an assertion
   about a host registry; it DOES NOT prove the writer lacks raw XTDB access.
   A clean report never grants authority or complete writer coverage."
  [source]
  (exact! source source-keys [])
  (seams!)
  (let [observed (inventory! (:writer-inventory-source source))
        protected (protected! (:protected-domain-source source))
        paths (registrations! (:writer-inventory-source source) observed)
        paths-again (registrations! (:writer-inventory-source source) observed)
        _ (when-not (every? (fn [[id entry]]
                              (identical? (:writer entry)
                                          (:writer (get paths-again id))))
                            paths)
            (reject! :unstable-registered-writer-identity
                     [:writer-inventory-source]))
        caps (:writer-capabilities source)]
    (when-not (and (plain-map? caps)
                   (= (set (keys caps)) (set (keys paths))))
      (reject! :missing-or-extra-writer-capability
               [:writer-capabilities]))
    (doseq [[id entry] paths]
      (capability! id entry (get caps id)))
    (let [protect (into {}
                        (map (juxt :target :fields))
                        (:protected-documents protected))
          rows (mapv (fn [i [id observed-entry]]
                       (row i [id observed-entry] (get caps id) protect
                            (:independent-operation-ids observed)))
                     (range) (sort-by (comp str key) paths))
          potential (count (filter :potential-protected-mutation? rows))
          outside (count (filter :possible-unlisted-document-access? rows))
          nonissued (count (remove #(= :issued-operation (:kind %)) rows))
          unbounded (count (filter #(or (= :unknown (:target-scope %))
                                        (= :unrestricted (:target-scope %))
                                        (:unrestricted-or-unknown-mutations? %))
                                   rows))
          partial (count (filter #(and (:potential-protected-mutation? %)
                                      (:whole-document-replacement-advertised? %)
                                      (pos? (:potentially-accessible-partially-protected-document-count %)))
                                 rows))
          noncanonical (count (filter :noncanonical-mutation-advertised? rows))
          unclaimed (count (filter #(not= :host-declared (:restriction-declaration %))
                                   rows))
          status (cond
                   (or (seq (:missing-observed-issued-operation-ids observed))
                       (seq (:foreign-observed-issued-operation-ids observed)))
                   :open-observed-issued-registration-mismatch
                   (pos? unbounded) :open-unbounded-or-unknown-writer-capabilities
                   (pos? nonissued) :open-nonissued-physical-write-paths
                   (pos? noncanonical) :open-noncanonical-physical-mutation-capabilities
                   (pos? unclaimed) :open-undeclared-capability-restrictions
                   (pos? partial) :open-partially-protected-replacement-access
                   (pos? outside) :open-outside-finite-protection-domain
                   :else :conditional-host-declared-capability-protection-join)]
      {:type report-type
       :version report-version
       :checking :fresh-host-writer-registrations-and-declared-xtdb-protection
       :scope :only-process-local-survey-and-caller-declared-capability-scopes
       :process-local-source-binding (secret-binding source paths)
       :source-binding-authenticates-execution? false
       :status status
       :upstream-writer-status (:status observed)
       :declared-protection-status (:status protected)
       :observed-writer-count (count paths)
       :observed-nonissued-writer-count nonissued
       :potential-protected-mutation-path-count potential
       :possible-unlisted-document-access-path-count outside
       :unbounded-or-unknown-capability-path-count unbounded
       :partially-protected-replacement-path-count partial
       :noncanonical-mutation-path-count noncanonical
       :undeclared-restriction-path-count unclaimed
       :declared-protected-document-count (count protect)
       :writer-rows rows
       :open-obligations (set/union permanent-obligations
                                    (:open-obligations observed)
                                    (when (pos? potential)
                                      #{:potential-protected-mutations-require-plan-and-executor-binding})
                                    (when (pos? nonissued)
                                      #{:nonissued-write-paths-require-mediation})
                                    (when (pos? noncanonical)
                                      #{:noncanonical-write-forms-need-separate-admission})
                                    (when (pos? unclaimed)
                                      #{:host-capability-restrictions-undeclared})
                                    (when (pos? partial)
                                      #{:whole-document-writes-may-escape-field-protection})
                                    (when (pos? outside)
                                      #{:writer-capabilities-may-reach-undeclared-documents}))
       :capability-restrictions-authenticated? false
       :host-writer-enumerator-exhaustiveness :not-established
       :complete-physical-writer-coverage :not-established
       :physical-write-footprint-completeness :not-established
       :physical-execution-binding :not-established
       :universal-refinement :not-established
       :proof-status :host-advertised-conditional-diagnostic-only
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))

(defn require-current-writer-capabilities!
  "Rederive original sources and identities; reject altered and stale reports.
   Neither process-local source bindings nor repeated surveys authenticate a
   deployed capability restriction or prevent a hidden external writer."
  [source report]
  (let [fresh (inspect-writer-capabilities! source)]
    (when-not (and (plain-map? report) (= fresh report))
      (reject! :stale-or-substituted-capability-report [:report]))
    fresh))

(defn redacted-summary
  "Recheck and return only counts, statuses, and stated open obligations."
  [source report]
  (let [fresh (require-current-writer-capabilities! source report)]
    (dissoc fresh :writer-rows :process-local-source-binding)))

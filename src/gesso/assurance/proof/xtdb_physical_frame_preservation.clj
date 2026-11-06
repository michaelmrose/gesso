(ns gesso.assurance.proof.xtdb-physical-frame-preservation
  "v1519: Compare two supplied v1492 external XTDB domain acquisitions against
   the SAME source-derived v1491 physical projection and v1517 whole-vector
   write-footprint declaration. Only the explicitly framed abstract fields
   and enumerated frame-only protected documents are compared.

   For a frame-only whole document, compare existence and every top-level
   field, including field presence and nested values; for a partially
   protected frame-only document, compare existence and precisely the named
   top-level fields. For a written document compare only mapped abstract
   fields appearing in the original construction's :frame declaration.

   Both acquisition reports are CALLER SUPPLIED. A source-matching, internally
   consistent pair does not authenticate either read, their chronological
   order, the executing transaction, its internal prestate, deployment or any
   other writer. This checker performs NO XTDB reads, submissions, callbacks
   or retries. It issues no certificate, admission or execution authority.

   The input and stored :source contain sensitive rows, document identifiers
   and snapshot tokens. Only redacted-summary is suitable for diagnostics."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.physical-domain-acquisition :as acquisition]
            [gesso.assurance.proof.xtdb-write-footprint-accounting :as footprint]
            [gesso.model.tx :as tx])
  (:import [java.time Instant]))

(def report-type
  :gesso.assurance.proof.xtdb-physical-frame-preservation/external-pair)
(def report-version 1)

(def ^:private source-keys
  #{:projection-source :pre-acquisition :post-acquisition})
(def ^:private acquisition-keys
  #{:type :version :checking :scope :source :declared-projection-fingerprint
    :external-read-basis :observed-rows :abstract-physical-values
    :observed-protected-target-count :observed-mapped-field-count
    :observed-absent-unmapped-target-count :observed-unprotected-field-count
    :unmapped-protected-targets :status :read-node-provenance
    :read-basis-authenticity :physical-value-and-type-translation
    :transaction-internal-prestate :complete-writer-coverage
    :physical-frame-preservation :universal-physical-refinement
    :proof-status :certificate :admission :transaction-authority
    :retry-authority :settlement-authority :authority})
(def ^:private row-keys
  #{:target :document :mapped-values :mapped-field-count
    :unprotected-observed-field-count :protection-mode
    :observed-document-status})
(def ^:private original-projection projection/describe-physical-domain-projection!)
(def ^:private original-footprint footprint/inspect-footprint!)
(def ^:private original-prepare tx/prepare)
(def ^:private original-acquisition-summary acquisition/redacted-summary)
(def ^:private max-targets 128)
(def ^:private max-fields 256)

(defn- reject! [reason at]
  (throw (ex-info "Unsupported external XTDB physical-frame comparison."
                  {:xtdb-physical-frame-preservation/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- exact! [x expected at]
  (when-not (and (plain-map? x) (= expected (set (keys x))))
    (reject! :invalid-map-shape at))
  x)

(defn- seams! []
  (when-not (and (identical? original-projection
                             (var-get #'projection/describe-physical-domain-projection!))
                 (identical? original-footprint
                             (var-get #'footprint/inspect-footprint!))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-acquisition-summary
                             (var-get #'acquisition/redacted-summary)))
    (reject! :substituted-source-or-acquisition-checker [])))

(defn- checked-declarations! [source]
  (let [p (try (original-projection source)
               (catch Exception _
                 (reject! :invalid-physical-projection [:projection-source])))
        domain (:protected-domain-source source)
        prepared (try (original-prepare nil (:plan domain))
                      (catch Exception _
                        (reject! :unsupported-protected-transaction [:projection-source])))
        f (try (original-footprint
                {:plan (:plan domain)
                 :protected-documents (:protected-documents domain)
                 :candidate-tx-ops (:tx-ops prepared)})
               (catch Exception _
                 (reject! :unsupported-write-footprint [:projection-source])))]
    (when-not (and (= projection/projection-type (:type p))
                   (= projection/projection-version (:version p))
                   (= :declarations-only (:proof-status p))
                   (= :not-established (:physical-frame-preservation p))
                   (= footprint/report-type (:type f))
                   (= footprint/report-version (:version f))
                   (= :bounded-declared-transaction-vector-only (:proof-status f))
                   (:all-candidate-operations-accounted-for? f)
                   (:all-declared-write-targets-covered-in-finite-domain? f)
                   (= :not-issued (:certificate f))
                   (= :none (:authority f)))
      (reject! :unusable-source-declarations [:projection-source]))
    {:projection p :footprint f}))

(defn- validate-acquisition! [phase supplied source p]
  ;; v1492 reports have no issuance token and no recheck/readback method.
  ;; Check their structural consistency here; no authenticity is inferred.
  (exact! supplied acquisition-keys [phase])
  (when-not (and (= acquisition/acquisition-type (:type supplied))
                 (= acquisition/acquisition-version (:version supplied))
                 (= :fresh-v1491-projection-and-one-pinned-external-read
                    (:checking supplied))
                 (= :finite-declared-protected-documents-only (:scope supplied))
                 (= source (:source supplied))
                 (= (:obligation-fingerprint p)
                    (:declared-projection-fingerprint supplied))
                 (= :bounded-pinned-external-field-observation (:status supplied))
                 (= :external-read-diagnostic-only (:proof-status supplied))
                 (= :caller-supplied-unattested (:read-node-provenance supplied))
                 (= :not-established (:read-basis-authenticity supplied))
                 (= :not-established (:physical-value-and-type-translation supplied))
                 (= :not-authenticated (:transaction-internal-prestate supplied))
                 (= :not-established (:complete-writer-coverage supplied))
                 (= :not-established (:physical-frame-preservation supplied))
                 (= :not-established (:universal-physical-refinement supplied))
                 (= :not-issued (:certificate supplied))
                 (= :not-admitted (:admission supplied))
                 (= :none (:transaction-authority supplied))
                 (= :none (:retry-authority supplied))
                 (= :none (:settlement-authority supplied))
                 (= :none (:authority supplied)))
    (reject! :invalid-or-unbound-acquisition [phase]))
  (let [basis (exact! (:external-read-basis supplied)
                      #{:snapshot-token :current-time} [phase :external-read-basis])
        rows (:observed-rows supplied)
        entries (get-in source [:protected-domain-source :protected-documents])
        locations (:field-locations p)
        by-target (reduce-kv
                   (fn [m abstract-field {:keys [target physical-field]}]
                     (assoc-in m [target physical-field] abstract-field))
                   {} locations)]
    (when-not (and (string? (:snapshot-token basis))
                   (seq (:snapshot-token basis))
                   (instance? Instant (:current-time basis))
                   (plain-vector? rows)
                   (= (count rows) (count entries))
                   (<= 1 (count rows) max-targets))
      (reject! :invalid-or-incomplete-acquisition-rows [phase]))
    (let [checked
          (mapv
           (fn [i row {:keys [target fields]}]
             (exact! row row-keys [phase :observed-rows i])
             (let [doc (:document row)
                   field-map (get by-target target {})
                   observed (disj (set (keys doc)) :xt/id)
                   required (set (keys field-map))
                   protected (if (= fields :all)
                               observed
                               (set/intersection observed fields))
                   unprotected (if (= fields :all)
                                 #{}
                                 (set/difference observed fields))
                   mapped (into {} (map (fn [[field abstract]]
                                          [abstract (get doc field)])) field-map)]
               (when-not (and (= target (:target row))
                              (or (nil? doc)
                                  (and (plain-map? doc)
                                       (<= (count doc) max-fields)
                                       (every? keyword? (keys doc))
                                       (or (not (contains? doc :xt/id))
                                           (= (second target) (:xt/id doc)))))
                              (or (not (seq required)) (some? doc))
                              (empty? (set/difference required observed))
                              (empty? (set/difference protected required))
                              (= (if (= fields :all)
                                   :whole-document :declared-fields)
                                 (:protection-mode row))
                              (= (if doc :present :absent)
                                 (:observed-document-status row))
                              (= (count unprotected)
                                 (:unprotected-observed-field-count row))
                              (= (count required) (:mapped-field-count row))
                              (= mapped (:mapped-values row)))
                 (reject! :divergent-or-incomplete-acquisition-row
                          [phase :observed-rows i]))
               row))
           (range (count rows)) rows entries)
          values (reduce (fn [acc row] (merge acc (:mapped-values row)))
                         {} checked)]
      (when-not (and (= values (:abstract-physical-values supplied))
                     (= (count rows) (:observed-protected-target-count supplied))
                     (= (count values) (:observed-mapped-field-count supplied))
                     (= (count (filter #(= :absent (:observed-document-status %))
                                       (filter #(empty? (:mapped-values %)) rows)))
                        (:observed-absent-unmapped-target-count supplied))
                     (= (reduce + (map :unprotected-observed-field-count rows))
                        (:observed-unprotected-field-count supplied))
                     (= (:unmapped-protected-targets p)
                        (:unmapped-protected-targets supplied)))
        (reject! :inconsistent-acquisition-summary [phase]))
      rows)))

(defn- field-changed? [before after field]
  (or (not= (contains? before field) (contains? after field))
      (and (contains? before field)
           (not= (get before field) (get after field)))))

(defn- frame-only-changed? [fields before after]
  (or (not= (nil? before) (nil? after))
      (if (= fields :all)
        (not= (dissoc before :xt/id) (dissoc after :xt/id))
        (boolean (some #(field-changed? before after %) fields)))))

(defn compare-external-physical-frame!
  "Compare two supplied, internally checked v1492 acquisitions using the
   ORIGINAL v1491 declaration. The original typed :frame specifies which
   mapped abstract fields must be unchanged; v1412 supplies protected
   frame-only targets and their precise :all or top-level-field scope.

   This is an observational comparison, not an authenticated execution or
   chronological claim. A changed field produces a divergence report, not a
   transaction failure or an authorization to retry."
  [input]
  (exact! input source-keys [])
  (seams!)
  (let [{:keys [projection-source pre-acquisition post-acquisition]} input
        {:keys [projection footprint]} (checked-declarations! projection-source)
        before (validate-acquisition! :pre-acquisition pre-acquisition
                                      projection-source projection)
        after (validate-acquisition! :post-acquisition post-acquisition
                                     projection-source projection)
        fields (->> (get-in projection-source
                            [:obligation-source :construction :frame])
                    (sort-by pr-str) vec)
        positions (into {} (map-indexed (fn [i row] [(:target row) i]) before))
        locations (:field-locations projection)
        changed-fields (->> fields
                            (keep-indexed
                             (fn [i abstract-field]
                               (let [{:keys [target physical-field]} (get locations abstract-field)
                                     idx (get positions target)
                                     lhs (get-in before [idx :document])
                                     rhs (get-in after [idx :document])]
                                 (when (field-changed? lhs rhs physical-field)
                                   i))))
                            vec)
        frame-entries (->> (get-in projection-source
                                   [:protected-domain-source :protected-documents])
                           (map-indexed vector)
                           (filter (fn [[_ {:keys [target]}]]
                                     (contains? (:frame-only-targets projection) target)))
                           vec)
        changed-documents (->> frame-entries
                               (keep-indexed
                                (fn [i [index {:keys [fields]}]]
                                  (when (frame-only-changed? fields
                                           (get-in before [index :document])
                                           (get-in after [index :document]))
                                    i))) vec)
        total (+ (count fields) (count frame-entries))
        agreement? (and (pos? total)
                        (empty? changed-fields)
                        (empty? changed-documents))
        status (cond
                 (zero? total) :no-declared-frame-obligations
                 (seq changed-fields) :bounded-supplied-frame-diverges
                 (seq changed-documents) :bounded-supplied-frame-diverges
                 :else :bounded-supplied-frame-agrees)]
    {:type report-type
     :version report-version
     :checking :source-checked-typed-frame-and-protected-physical-observation-pair
     :scope :only-source-declared-finite-protected-frame
     :source input
     :status status
     :declared-typed-frame-field-count (count fields)
     :declared-frame-only-document-count (count frame-entries)
     :partially-protected-frame-only-document-count
     (count (filter (fn [[_ entry]] (not= :all (:fields entry)))
                    frame-entries))
     :unprotected-observed-field-count-before
     (:observed-unprotected-field-count pre-acquisition)
     :unprotected-observed-field-count-after
     (:observed-unprotected-field-count post-acquisition)
     :observed-protected-document-count (count before)
     :source-relative-write-operation-count (:candidate-operation-count footprint)
     :changed-typed-frame-field-indices changed-fields
     :changed-frame-only-document-indices changed-documents
     :finite-supplied-frame-agrees? agreement?
     :same-reported-snapshot-token?
     (= (get-in pre-acquisition [:external-read-basis :snapshot-token])
        (get-in post-acquisition [:external-read-basis :snapshot-token]))
     :external-observation-order :not-authenticated
     :pre-acquisition-origin :caller-supplied-unattested
     :post-acquisition-origin :caller-supplied-unattested
     :transaction-internal-prestate :not-authenticated
     :actual-submitted-operations :not-authenticated
     :complete-physical-writer-coverage :not-established
     :frame-outside-enumerated-domain :not-established
     :actual-physical-frame-preservation :not-established
     :universal-refinement :not-established
     :proof-status :bounded-supplied-external-frame-diagnostic-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :retry-authority :none
     :settlement-authority :none
     :authority :none}))

(defn require-current-external-physical-frame! [input report]
  "Recheck source, whole write footprint and supplied observations. This
   detects stale reports but does not authenticate their claimed XTDB origin."
  (let [current (compare-external-physical-frame! input)]
    (when-not (and (plain-map? report) (= report current))
      (reject! :stale-or-forged-frame-report [:report]))
    current))

(defn redacted-summary [report]
  "Counts, positional differences and explicit open guarantees only."
  (dissoc (require-current-external-physical-frame!
           (:source report) report) :source))

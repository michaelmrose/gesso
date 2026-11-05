(ns gesso.assurance.proof.independent-transition-semantics
  "Independent, purely ABSTRACT S4 guarded-transition semantics (v1351).

   Interpret the ORIGINAL v1233 transition declaration and caller-supplied
   before/input/after maps using only v1341 partial-state semantics and v1344
   branch-sensitive typed-guard semantics, not gesso.assurance.proof.transition,
   rule-normalize, rule-eval, subject identity, or the proof kernel.

   For a well-typed closed projection, the relation is:
     guard(before, inputs) AND
     (forall written f: after[f] exists and equals RHS(before, inputs)) AND
     (forall unwritten f: presence and value agree before/after).

   Every RHS reads the PRE-state or operation inputs; assignments are
   simultaneous, never sequential. An assigned optional target becomes present.
   The pre/post projection is closed and all three maps are fully validated.

   This is a diagnostic interpretation, not a source identity, a theorem, a
   proof certificate, an actual XTDB observation, or transaction authority.
   In particular no claim is made about unmodeled application fields, writer
   completeness, or the independent S1 interpreter's universal soundness."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.proof.typed-presence-semantics :as presence]))

(def semantics-version 1)
(def report-type
  :gesso.assurance.proof.independent-transition-semantics/observation)

(def ^:private source-keys
  #{:subject :state-types :input-types :guard :reads :writes :assignments})
(def ^:private subject-keys #{:kind :id :interface})
(def ^:private guard-keys #{:readers :expression})
(def ^:private max-total-readers 64)
(def ^:private max-data-depth 64)
(def ^:private max-data-nodes 8192)

(defn- reject! [reason at more]
  (throw (ex-info "Invalid independent guarded-transition semantics."
                  (merge {:independent-transition-semantics/rejection true
                          :reason reason :at at}
                         more))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- plain-vector? [x]
  (instance? clojure.lang.APersistentVector x))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {}))
  x)

(defn- qualified-field? [x]
  (and (keyword? x) (some? (namespace x))))

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

(defn- field-map! [value at]
  (when-not (plain-map? value)
    (reject! :expected-field-map at {}))
  (metadata-free! value at)
  (doseq [id (keys value)]
    (when-not (qualified-field? id)
      (reject! :unqualified-reader at {:reader id})))
  value)

(defn- field-set! [value at]
  (when-not (plain-set? value)
    (reject! :expected-field-set at {}))
  (metadata-free! value at)
  (doseq [id value]
    (when-not (qualified-field? id)
      (reject! :unqualified-reader at {:reader id})))
  value)

(defn- data-only!
  "Reject executable or metadata-bearing subject-interface structures.
   Identity hashing is deliberately NOT performed by this interpreter."
  [value at depth budget]
  (when (> depth max-data-depth)
    (reject! :subject-depth-limit at {}))
  (when (> (swap! budget inc) max-data-nodes)
    (reject! :subject-size-limit at {}))
  (metadata-free! value at)
  (cond
    (or (nil? value) (instance? Boolean value) (integer? value)
        (string? value) (keyword? value)) nil
    (plain-vector? value)
    (doseq [[i v] (map-indexed vector value)]
      (data-only! v (conj at i) (inc depth) budget))
    (plain-set? value)
    (doseq [v value]
      (data-only! v (conj at :member) (inc depth) budget))
    (plain-map? value)
    (doseq [[k v] value]
      (data-only! k (conj at :key) (inc depth) budget)
      (data-only! v (conj at k) (inc depth) budget))
    :else (reject! :unsupported-subject-value at
                   {:value-class (some-> value class str)})))

(defn- subject-shape! [subject]
  (exact-map! subject subject-keys [:subject])
  (when-not (and (= :operation (:kind subject))
                 (qualified-field? (:id subject))
                 (plain-map? (:interface subject))
                 (seq (:interface subject)))
    (reject! :invalid-subject [:subject] {}))
  (data-only! (:interface subject) [:subject :interface] 0 (atom 0)))

(defn- normalized-type [d]
  ;; Descriptor validation is supplied independently by v1341. Missing
  ;; optional/nullable flags have the same denotation as false.
  (assoc d :optional? (true? (:optional? d))
           :nullable? (true? (:nullable? d))))

(defn- compatible-copy? [source target]
  (let [s (normalized-type source)
        t (normalized-type target)]
    (and (not (:optional? s))
         (or (not (:nullable? s)) (:nullable? t))
         (= (:type s) (:type t))
         (or (not= :enum (:type s))
             (and (= (:name s) (:name t))
                  (= (:values s) (:values t)))))))

(defn- assignment! [target expression state-types all-types]
  (let [at [:assignments target]]
    (when-not (and (plain-vector? expression)
                   (= 2 (count expression)))
      (reject! :invalid-assignment at {}))
    (metadata-free! expression at)
    (let [[op operand] expression
          descriptor (get state-types target)]
      (case op
        :literal
        (do
          (when-not (values/value-in-domain? descriptor operand)
            (reject! :ill-typed-assignment-literal at {}))
          {:source-readers #{} :expression expression})

        :read
        (do
          (when-not (and (qualified-field? operand)
                         (contains? all-types operand))
            (reject! :undeclared-assignment-source at {:reader operand}))
          (when-not (compatible-copy? (get all-types operand) descriptor)
            (reject! :incompatible-assignment-copy at {:reader operand}))
          {:source-readers #{operand} :expression expression})

        (reject! :unsupported-assignment at {:operator op})))))

(defn- declaration-shape!
  [declaration]
  (exact-map! declaration source-keys [])
  (subject-shape! (:subject declaration))
  (let [{:keys [state-types input-types guard reads writes assignments]}
        declaration]
    (field-map! state-types [:state-types])
    (field-map! input-types [:input-types])
    (when (empty? state-types)
      (reject! :empty-state-projection [:state-types] {}))
    (let [state-ids (set (keys state-types))
          input-ids (set (keys input-types))
          all-ids (set/union state-ids input-ids)]
      (when (> (count all-ids) max-total-readers)
        (reject! :independent-reader-limit []
                 {:maximum max-total-readers :actual (count all-ids)}))
      (when (seq (set/intersection state-ids input-ids))
        (reject! :state-input-overlap [:input-types] {}))
      (exact-map! guard guard-keys [:guard])
      (field-set! (:readers guard) [:guard :readers])
      (field-set! reads [:reads])
      (field-set! writes [:writes])
      (field-map! assignments [:assignments])
      (when-not (set/subset? (:readers guard) all-ids)
        (reject! :undeclared-guard-reader [:guard] {}))
      (when-not (= writes (set (keys assignments)))
        (reject! :write-footprint-mismatch [:writes] {}))
      (when-not (set/subset? writes state-ids)
        (reject! :writes-outside-state [:writes] {}))
      (let [all-types (merge state-types input-types)
            assignment-reads
            (apply set/union #{}
                   (map :source-readers
                        (map (fn [[id expr]]
                               (assignment! id expr state-types all-types))
                             assignments)))
            expected-reads (set/union (:readers guard) assignment-reads)]
        (when-not (= expected-reads reads)
          (reject! :read-footprint-mismatch [:reads]
                   {:expected expected-reads :actual reads}))
        (when-not (set/subset? input-ids reads)
          (reject! :unused-input-declarations [:input-types] {}))
        {:state-ids state-ids
         :input-ids input-ids
         :all-types all-types
         :frame-fields (set/difference state-ids writes)}))))

(defn- same-field? [before after id]
  (and (= (contains? before id) (contains? after id))
       (or (not (contains? before id))
           (= (get before id) (get after id)))))

(defn- assigned-value [expression before-input]
  (let [[op v] expression]
    (case op
      :literal v
      :read (get before-input v))))

(defn observe-transition!
  "Independently interpret one well-typed guarded before/input/after witness.
   Source syntax, field footprints, pre/post presence and values, typed guard,
   RHS source typing, and simultaneous writes are all checked. A valid but
   nonmatching witness returns :matches? false, including a false guard.

   All arguments are caller-supplied, so matching is not authoritative and is
   not evidence that any persisted transaction realizes the abstract machine."
  [declaration before inputs after]
  (let [{:keys [state-ids input-ids all-types frame-fields]}
        (declaration-shape! declaration)
        {:keys [state-types input-types guard writes assignments]}
        declaration]
    (field-map! before [:before])
    (field-map! inputs [:inputs])
    (field-map! after [:after])
    (let [pre-observation (values/observe-state! state-types before)
          input-observation (values/observe-state! input-types inputs)
          post-observation (values/observe-state! state-types after)
          before-input (merge before inputs)
          guard-state (select-keys before-input (:readers guard))
          guard-types (select-keys all-types (:readers guard))
          guard-result (presence/evaluate-expression!
                        (:expression guard) guard-types guard-state)
          guard-holds? (:value guard-result)
          assignments-hold?
          (every? (fn [field]
                    (and (contains? after field)
                         (= (get after field)
                            (assigned-value (get assignments field)
                                            before-input))))
                  writes)
          frame-holds? (every? #(same-field? before after %) frame-fields)]
      {:type report-type
       :version semantics-version
       :source-declaration declaration
       :before before
       :inputs inputs
       :after after
       :state-fields state-ids
       :input-fields input-ids
       :frame-fields frame-fields
       :writes writes
       :guard-holds? guard-holds?
       :assignments-hold? (boolean assignments-hold?)
       :frame-holds? (boolean frame-holds?)
       :matches? (boolean (and guard-holds? assignments-hold? frame-holds?))
       :checking :independent-typed-guarded-relation
       :semantics :prestate-guard-simultaneous-writes-exact-frame
       :state-presence {:before (select-keys pre-observation
                                            [:present-readers :absent-readers])
                        :after (select-keys post-observation
                                            [:present-readers :absent-readers])}
       :input-presence (select-keys input-observation
                                    [:present-readers :absent-readers])
       :proof-status :not-proved
       :admission :not-admitted
       :runtime-enforcement :not-established
       :physical-observation :not-established
       :authority :none})))

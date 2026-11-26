(ns gesso.assurance.reference-mapped-writes
  "Forward derivation of a closed mapped put-only suffix. No provider poststate
   or candidate row enters the expected write calculation."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.mapped-prestate-semantics :as prestate]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-transaction :as current]
            [gesso.assurance.transition :as construction]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]))

(def profile :selected-mapped-retained-writes-v1)

(defn- reject! [reason]
  (throw (ex-info "Unsupported mapped write expectation." {::unsupported true :reason reason})))

(defn- target [[kind a b]]
  (if (= :journal-slot kind) [kind a b] [:document a b]))

(defn- row-key [[_ _ _ k]] (keyword (str/replace (name k) "_" "-")))

(defn- project! [coords predicates outcome-key]
  (into {} (map (fn [[field coord]]
                  (let [[mode row] (get predicates (target coord))]
                    [field (if (= :absent mode) nil
                               (if (= :journal-slot (first coord))
                                 (let [v (get-in row [:gesso.command/outcome outcome-key])]
                                   (when-not (keyword? v) (reject! :invalid-journal-outcome)) v)
                                 (let [k (row-key coord)]
                                   (when-not (contains? row k) (reject! :missing-mapped-field))
                                   (get row k))))]))) coords))

(defn derive-puts!
  "Derive expected complete rows from the original construction, closed
   prepared rows, supplied inputs/effects and explicit command record source.
   Only one inserted journal slot and default-valid-time document puts are
   supported. The outcome selector is a single supplied keyword, not an
   arbitrary callback. No authenticated inputs, effects or time provenance."
  [original targets docs journal {:keys [inputs effects binding outcome now outcome-key] :as spec}]
  (when-not (and (map? spec) (nil? (meta spec)) (map? inputs)
                 (= #{:inputs :effects :binding :outcome :now :outcome-key} (set (keys spec)))
                 (simple-keyword? outcome-key) (not-any? :optional? (vals (:state-types original))))
    (reject! :invalid-write-source))
  (let [mapping (plan/check-target-mapping! original targets)
        coords (:target-map mapping)
        predicates (:predicates (prestate/describe! original targets docs journal))
        before (project! coords predicates outcome-key)
        record (resolution/command-record binding outcome now)
        slot [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]
        written (:write-targets mapping)
        _ (when-not (and (= #{slot} (set (filter #(= :journal-slot (first %)) written)))
                         (= [:absent] (get predicates slot))
                         (not-any? #(= (:xt/id record) (:xt/id %)) journal))
            (reject! :invalid-journal-insertion))
        _ (frame/check-effects! mapping effects)
        env (merge before inputs)
        after (reduce-kv (fn [m field [op value]]
                           (assoc m field (case op :literal value :read (get env value))))
                         before (:replacements original))
        checked (independent/observe-transition! (:transition (construction/build-guarded-replacement! original))
                                                  before inputs after)
        _ (when-not (:matches? checked) (reject! :abstract-relation-mismatch))
        rows (reduce (fn [m [_ table id :as address]]
                       (let [[mode prior] (get predicates address)
                             fields (if (= :absent mode) coords (:write-coordinates mapping))
                             row (reduce-kv (fn [r field coord]
                                              (if (= address (target coord))
                                                (assoc r (row-key coord) (get after field)) r))
                                            (merge (or prior {:xt/id id}) (get effects address)) fields)]
                         (assoc m address row)))
                     {slot record} (filter #(= :document (first %)) written))
        projected (project! coords (reduce-kv #(assoc %1 %2 [:exact-row %3]) predicates rows) outcome-key)
        puts (mapv (fn [[address row]]
                     [:put-docs (if (= :journal-slot (first address)) :gesso_commands (second address)) row])
                   (sort-by (comp pr-str key) rows))]
    (when-not (and (= after projected) (set? (current/operation-sequence-tables puts)))
      (reject! :unsupported-derived-writes))
    puts))

(defn- rows-of! [puts]
  (when-not (and (vector? puts) (seq puts)
                 (every? #(and (vector? %) (= :put-docs (first %)) (simple-keyword? (second %))
                               (<= 3 (count %)) (every? map? (drop 2 %))) puts)
                 (set? (current/operation-sequence-tables puts)))
    (reject! :unsupported-write-suffix))
  (frequencies (mapcat (fn [[_ table & rows]] (map #(vector table %) rows)) puts)))

(defn compare-local-processed-retained-mapped-writes!
  "Compose exact complete put-only suffix correspondence with the existing
   same-transaction prefix and predecessor diagnostic. Compare row
   multiplicity, not physical batching; the retained expectation itself
   still binds exact operation order. A match does not assert commitment."
  [node tables original targets docs journal expectation prefix-count spec]
  (let [tag #(assoc % :mapped-retained-writes-profile profile)
        invalid #(tag {:status %1 :reason %2 :authority :none :certificate :not-issued
                       :provider-refinement :not-established})
        checked (try
                  (if (and (vector? (:ops expectation)) (integer? prefix-count)
                           (<= 1 prefix-count (dec (count (:ops expectation)))))
                    (if (= (rows-of! (derive-puts! original targets docs journal spec))
                           (rows-of! (subvec (:ops expectation) prefix-count))) :matched :mismatch)
                    :unsupported)
                  (catch clojure.lang.ExceptionInfo _ :unsupported))]
    (case checked
      :unsupported (invalid :unsupported :invalid-mapped-write-source-or-suffix)
      :mismatch (invalid :mismatch :selected-retained-write-suffix-mismatch)
      :matched (let [r (tag (provider/compare-local-processed-retained-mapped-prestate!
                             node tables original targets docs journal expectation prefix-count))]
                 (if (= :matched (:status r))
                   (assoc r :mapped-write-suffix :matched-source-derived-puts)
                   r)))))

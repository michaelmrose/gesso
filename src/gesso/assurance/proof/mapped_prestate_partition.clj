(ns gesso.assurance.proof.mapped-prestate-partition
  "Concrete decoded-map entry partitions for the encoded cardinality lemma.
   Child equality is checked on supplied values, not recursively proved.
   Local codes describe entries, not SQL values."
  (:require [gesso.assurance.proof.mapped-prestate-rules :as rules]))

(defn partition-map
  "Construct a known/residual partition without assuming equal cardinality.
   Assign a local integer to each actual unique entry, then order the
   encoded actual list with matching expected entries first."
  [expected actual]
  (if-not (and (map? expected) (map? actual)
               (every? keyword? (keys expected)) (every? keyword? (keys actual)))
    {:status :not-established :reason :unsupported-map-shape}
    (let [missing (into #{} (remove #(contains? actual %)) (keys expected))
          different (into #{} (filter #(and (contains? actual %)
                                            (not= (get expected %) (get actual %))))
                          (keys expected))]
      (if (or (seq missing) (seq different))
        {:status :not-established :reason :unmatched-child-values
         :missing-fields missing :different-fields different}
        (let [dictionary (mapv (fn [[k v]] [k v]) (sort-by (comp str key) actual))
              codes (into {} (map-indexed (fn [i [k _]] [k i]) dictionary))
              known (mapv codes (sort-by str (keys expected)))
              residual (into [] (keep-indexed (fn [i [k _]]
                                                (when-not (contains? expected k) i))) dictionary)
              ordered (into known residual)
              decode #(mapv dictionary %)]
          ;; Check the representation rather than assuming arbitrary map
          ;; implementations have faithful entry enumeration.
          (if-not (and (= (count actual) (count dictionary) (count codes) (count ordered))
                       (= (set ordered) (set (range (count dictionary))))
                       (= expected (into {} (decode known)))
                       (= actual (into {} (decode ordered))))
            {:status :not-established :reason :entry-encoding-mismatch}
            {:status :partition-constructed
             :dictionary dictionary :known known :residual residual :actual ordered
             :expected-cardinality (count expected) :actual-cardinality (count actual)
             :cardinality-premises-satisfied? (= (count expected) (count actual))}))))))

(defn- at-map [row path]
  (reduce (fn [v k] (when (and (map? v) (contains? v k)) (get v k))) row path))

(defn observe!
  "Re-derive map-size sites from original source and prefix. Construct one
   partition per site from supplied selected row bags, without requiring an
   exact-row match. This is instance evidence, not a map semantics proof."
  [original target-map docs journal prefix actual]
  (let [description (rules/derive! original target-map docs journal prefix)
        _ (rules/evaluate! original target-map docs journal prefix description actual)
        sites (vec (for [{:keys [target predicate rules]} (:groups description)
                         [kind path n] rules :when (= :map-size kind)
                         :let [bag (get actual target)]]
                     {:target target :path path :cardinality n
                      :partition (if (= 1 (count bag))
                                   (partition-map (at-map (second predicate) path)
                                                  (at-map (first bag) path))
                                   {:status :not-established :reason :non-singleton-row-bag})}))]
    {:type ::mapped-prestate-partition-observation
     :mapping-fingerprint (:mapping-fingerprint description)
     :sites sites
     :status (cond (empty? sites) :no-map-sites
                   (every? #(get-in % [:partition :cardinality-premises-satisfied?]) sites)
                   :all-map-cardinality-premises-observed
                   :else :map-cardinality-premises-open)
     :scope :supplied-decoded-map-entry-partitions
     :authority :none :certificate :not-issued :sql-refinement :not-established
     :open-obligations (into (:open-obligations description)
                             #{:general-decoded-map-encoding-refinement
                               :recursive-child-equality-correspondence
                               :kernel-checked-partition-instance-composition
                               :general-structural-soundness})}))

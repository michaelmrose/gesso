(ns gesso.assurance.proof.mapped-prestate-rules
  "Decoded-row interpretation of the restricted prestate assertion fragment.
   Selection, SQL/codec semantics and a common transaction read basis remain
   premises. This is an executable reference, not a provider proof."
  (:require [gesso.assurance.proof.mapped-prestate-lowering :as lowering]))

(defn- field-rules [path value]
  (if (and (map? value) (seq value))
    (into [[:map-size path (count value)]]
          (mapcat (fn [[k child]] (field-rules (conj path k) child)))
          (sort-by (comp str key) value))
    [[:equals path value]]))

(defn- predicate-rules [[kind row]]
  (case kind
    :absent [[:row-count 0]]
    :exact-row (into [[:row-count 1]] (field-rules [] row))))

(defn derive!
  "Bind abstract rules to the checked source-relative SQL prefix. Each
   target denotes its complete selected bag of decoded rows. Nonempty maps
   require exact visible cardinality plus constraints on every child; leaves
   (including empty maps and supported vectors) require exact equality."
  [original target-map docs journal prefix]
  (let [checked (lowering/check-prefix! original target-map docs journal prefix)]
    {:type ::mapped-prestate-rules :version 1
     :mapping-fingerprint (:mapping-fingerprint checked)
     :groups (mapv (fn [{:keys [target predicate assertions]}]
                     {:target target :predicate predicate :assertions assertions
                      :rules (predicate-rules predicate)})
                   (:groups checked))
     :scope :selected-decoded-row-bags :authority :none
     :sql-refinement :not-established
     :open-obligations (:open-obligations checked)}))

(defn- at-path [row path]
  ;; Preserve absence separately from nil; do not traverse lists or scalars
  ;; as if they were structs.
  (reduce (fn [found k]
            (when (and found (map? (:value found))
                       (contains? (:value found) k))
              {:value (get (:value found) k)}))
          {:value row} path))

(defn- holds? [rows [op a b]]
  (case op
    :row-count (= a (count rows))
    :map-size (let [found (at-path (first rows) a)
                    v (:value found)]
                (and (map? v) (= b (count v))))
    :equals (let [found (at-path (first rows) a)]
              (and (some? found) (= b (:value found))))))

(defn- exact? [rows [kind row]]
  (case kind
    :absent (empty? rows)
    :exact-row (and (= 1 (count rows)) (= row (first rows)))))

(defn evaluate!
  "Re-derive the rules before interpreting a supplied complete bag per
   target. Check the finite instance against the independent exact-row
   predicate. A result describes supplied observations only; it does not
   attest that SQL ran or that the bags were acquired faithfully."
  [original target-map docs journal prefix description actual]
  (when-not (= description (derive! original target-map docs journal prefix))
    (throw (ex-info "Changed prestate rule description."
                    {:error/type ::invalid-rules :reason :description-mismatch})))
  (when-not (and (map? actual)
                 (= (set (keys actual)) (set (map :target (:groups description))))
                 (every? (fn [rows] (and (vector? rows) (every? map? rows))) (vals actual)))
    (throw (ex-info "Expected a complete selected row bag per target."
                    {:error/type ::invalid-rules :reason :invalid-selected-bags})))
  (let [groups (mapv (fn [{:keys [target predicate rules]}]
                       (let [rows (get actual target)]
                         {:target target
                          :rules-pass? (every? #(holds? rows %) rules)
                          :exact-match? (boolean (exact? rows predicate))}))
                     (:groups description))
        pass? (every? :rules-pass? groups)
        match? (every? :exact-match? groups)]
    {:type ::mapped-prestate-rule-observation
     :groups groups :rules-pass? pass? :exact-match? match?
     :instance-correspondence? (every? #(= (:rules-pass? %) (:exact-match? %)) groups)
     :scope :supplied-selected-decoded-row-bags
     :authority :none :certificate :not-issued :sql-refinement :not-established
     :open-obligations (:open-obligations description)}))

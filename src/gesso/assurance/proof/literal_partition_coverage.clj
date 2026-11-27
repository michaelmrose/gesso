(ns gesso.assurance.proof.literal-partition-coverage
  "Check an abstract scalar partition's coverage and observation constancy.
   Uses closed integer intervals and equality classes, not sampled original
   values. The argument assumes discrete integer order and scalar equality;
   executable evaluator refinement remains open."
  (:require [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.typed-value-semantics :as values]))

(def profile :abstract-literal-partition-coverage-v1)
(def ^:private original-audit partition/audit-rule!)
(def ^:private original-domain? values/value-in-domain?)

(defn- reject! [reason]
  (throw (ex-info "Unqualified scalar coverage" {:reason reason})))

(defn- integer-cells [constants]
  (let [cuts (vec (sort constants))]
    (if (empty? cuts)
      [{:kind :integer-interval :lower nil :upper nil}]
      (into [{:kind :integer-interval :lower nil :upper (dec' (first cuts))}]
            (concat
              (mapcat (fn [i]
                        (let [a (cuts i) b (get cuts (inc i))]
                          (cond-> [{:kind :integer-interval :lower a :upper a}]
                            (and b (< (inc' a) b))
                            (conj {:kind :integer-interval :lower (inc' a) :upper (dec' b)}))))
                      (range (count cuts)))
              [{:kind :integer-interval :lower (inc' (peek cuts)) :upper nil}])))))

(defn- covers-integer-line? [cells]
  (and (seq cells) (nil? (:lower (first cells))) (nil? (:upper (peek cells)))
       (every? (fn [{:keys [lower upper]}]
                 (and (or (nil? lower) (integer? lower))
                      (or (nil? upper) (integer? upper))
                      (or (nil? lower) (nil? upper) (<= lower upper)))) cells)
       (every? (fn [[a b]]
                 (and (integer? (:upper a)) (integer? (:lower b))
                      (= (inc' (:upper a)) (:lower b))))
               (partition 2 1 cells))))

(defn- member? [{:keys [kind lower upper value excluded]} x]
  (case kind
    :nil (nil? x)
    :singleton (and (some? x) (= value x))
    :other (and (some? x) (not (contains? excluded x)))
    :integer-interval (and (integer? x)
                           (or (nil? lower) (<= lower x))
                           (or (nil? upper) (<= x upper)))))

(defn- observation! [{:keys [kind lower upper value excluded]} literal]
  (cond
    (= :nil kind) (if (nil? literal) :equal :different)
    (nil? literal) :different
    (= :singleton kind) (if (= value literal) :equal :different)
    (= :other kind) (if (contains? excluded literal) :different
                      (reject! :unclassified-equality-literal))
    (= :integer-interval kind)
    (cond
      (and upper (< upper literal)) :below
      (and lower (> lower literal)) :above
      (= lower literal upper) :equal
      :else (reject! :comparison-crosses-cell))
    :else (reject! :unsupported-cell)))

(defn- scalar-observation [kind x literal]
  (cond (= x literal) :equal
        (or (nil? x) (nil? literal)) :different
        (= :integer kind) (if (< x literal) :below :above)
        :else :different))

(defn- check-domain! [descriptor literals domain]
  (let [kind (:type descriptor) constants (disj literals nil)
        cells (case kind
                :integer (let [cs (integer-cells constants)]
                           (when-not (covers-integer-line? cs)
                             (reject! :integer-line-not-covered))
                           cs)
                :boolean (mapv #(hash-map :kind :singleton :value %) [false true])
                (:keyword :string) (conj (mapv #(hash-map :kind :singleton :value %)
                                              (sort-by str constants))
                                         {:kind :other :excluded constants})
                (reject! :unsupported-domain))
        cells (cond-> cells (:nullable? descriptor) (conj {:kind :nil}))]
    (when-not (and (not (:optional? descriptor)) (vector? domain)
                   (= (count cells) (count domain) (count (set domain)))
                   (every? #(original-domain? descriptor %) domain)
                   (every? #(original-domain? descriptor %) literals)
                   (every? (fn [x] (= 1 (count (filter #(member? % x) cells)))) domain))
      (reject! :invalid-representative-domain))
    (mapv (fn [cell]
            (let [matches (filterv #(member? cell %) domain)]
              (when-not (= 1 (count matches)) (reject! :cell-not-uniquely-represented))
              (let [rep (first matches)
                    observations (into {} (map (fn [l] [l (observation! cell l)])) literals)]
                (when-not (= observations (into {} (map (fn [l] [l (scalar-observation kind rep l)])) literals))
                  (reject! :representative-observation-mismatch))
                (assoc cell :representative rep :observations observations)))) cells)))

(defn check-coverage!
  "Rebuild the original source partition and check a finite cover of each
   abstract scalar domain. Integer endpoints and adjacency justify the cover
   under mathematical integer order; singleton/complement classes justify
   equality domains. Each entire cell fixes the original literal observations.
   No report supplied by a caller is trusted. No JVM semantics theorem follows."
  [rule state-types]
  (let [base {:profile profile :scope :original-abstract-scalar-domains-only
              :executable-evaluator-refinement :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (and (identical? original-audit partition/audit-rule!)
                     (identical? original-domain? values/value-in-domain?))
        (reject! :coverage-boundary-replaced))
      (let [{:keys [status reader-types comparison-literals representative-domains] :as report}
            (original-audit rule state-types)]
        (when-not (and (= :matched status) (= partition/profile (:profile report))
                       (= rule (:rule report)) (= reader-types (select-keys state-types (:readers rule)))
                       (= (:readers rule) (set (keys reader-types))
                          (set (keys comparison-literals)) (set (keys representative-domains))))
          (reject! :unqualified-partition))
        (assoc base :status :conditional-abstract-coverage :rule rule :reader-types reader-types
               :conditions #{:discrete-unbounded-integer-order :typed-scalar-equality}
               :cells (into {} (map (fn [[id descriptor]]
                                      [id (check-domain! descriptor (comparison-literals id)
                                                         (representative-domains id))])) reader-types)
               :conclusion :unique-represented-cell-with-constant-literal-observations))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-coverage))))))

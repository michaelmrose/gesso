(ns gesso.assurance.proof.guarded-substitution
  "Conditional preservation by guarded literal substitution. This restricted
   rule uses original typed obligation sources, not sampled domains. Each
   branch is a conjunction of exact field/literal equalities. Incompatible
   branches cannot satisfy the antecedent; every remaining branch must imply
   a whole postcondition branch after exact replacement and framing.
   Unsupported syntax, optional readers and over-budget expansion stay open.
   This is an unadmitted inference, not independent checker soundness."
  (:require [gesso.assurance.proof.obligation :as obligation]))

(def ^:private branch-limit 128)

(defn- total-field? [types field]
  (let [d (get types field)]
    (and (#{:boolean :keyword :string :integer} (:type d))
         (not (:optional? d)))))

(defn- combine [a b]
  (when (every? (fn [[k v]] (or (not (contains? a k)) (= (get a k) v))) b)
    (merge a b)))

(defn- conjunction [a b]
  (when (and (some? a) (some? b) (<= (* (count a) (count b)) branch-limit))
    (into [] (keep (fn [[x y]] (combine x y))) (for [x a y b] [x y]))))

(defn- branches [types expression]
  (let [[op a b] expression]
    (case op
      :literal (case a true [{}] false [] nil)
      := (let [[reader literal] (if (= :read (first a)) [a b] [b a])
               field (second reader)]
           (when (and (= :read (first reader)) (= :literal (first literal))
                      (total-field? types field))
             [{field (second literal)}]))
      :and (reduce (fn [acc e] (conjunction acc (branches types e))) [{}] (rest expression))
      :or (reduce (fn [acc e]
                    (let [next (branches types e)]
                      (when (and (some? acc) (some? next)
                                 (<= (+ (count acc) (count next)) branch-limit))
                        (into acc next)))) [] (rest expression))
      nil)))

(defn- implies-branch? [facts goal]
  (every? (fn [[k v]] (and (contains? facts k) (= (get facts k) v))) goal))

(defn- infer [construction invariant]
  (let [{:keys [state-types guard replacements frame]} construction
        goal (branches state-types (:expression invariant))
        guards (branches state-types (:expression guard))
        antecedent (conjunction goal guards)
        literal-writes? (every? (fn [[field expression]]
                                  (and (total-field? state-types field)
                                       (= :literal (first expression)))) replacements)]
    (if-not (and (some? goal) (seq antecedent) literal-writes?)
      {:status :open :reason :unsupported-or-empty-antecedent}
      (let [values (into {} (map (fn [[k expression]] [k (second expression)])) replacements)
            cases (mapv (fn [pre]
                          (let [post (merge (select-keys pre frame) values)]
                            {:pre-facts pre :post-facts post
                             :goal-branch (first (filter #(implies-branch? post %) goal))})) antecedent)
            success? (every? #(some? (:goal-branch %)) cases)]
        {:status (if success? :checked-conditional-inference :open)
         :reason (when-not success? :post-branch-not-established)
         :cases cases}))))

(defn check-guarded-preservation!
  "Recheck the original obligation source. Prove each supported invariant
   from itself and the guard; other pre-invariants are not needed or assumed.
   Empty antecedents stay open. An open branch is not a concrete counterexample."
  [source]
  (let [ledger (obligation/derive-obligations! source)
        outcomes (mapv (fn [[id invariant]]
                         (merge {:obligation-id [:preservation id] :invariant id}
                                (infer (:construction source) invariant))) (:invariants ledger))]
    {:obligation-fingerprint (:fingerprint ledger)
     :transition-fingerprint (:transition-fingerprint ledger)
     :preservation-outcomes outcomes
     :conditionally-checked-preservation-ids
     (into #{} (comp (filter #(= :checked-conditional-inference (:status %)))
                     (map :obligation-id)) outcomes)
     :original-open-obligation-ids (:open-obligation-ids ledger)
     :scope :declared-abstract-state-projection-only
     :checker-soundness :not-independently-proved
     :certificate :not-issued :authority :none}))

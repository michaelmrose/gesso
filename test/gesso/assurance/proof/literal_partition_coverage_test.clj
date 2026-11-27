(ns gesso.assurance.proof.literal-partition-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-partition-coverage :as coverage]
            [gesso.assurance.proof.literal-partition :as partition]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-claim-abstract-domains-covered
  (let [catalog (claim/catalog)]
    (doseq [[id rule] (assoc (:invariants catalog) :fixture/init (:initial-condition catalog))]
      (let [r (coverage/check-coverage! rule claim/state-types)
            partition (partition/audit-rule! rule claim/state-types)]
        (is (= :conditional-abstract-coverage (:status r)) (str id " " (pr-str r)))
        (is (= rule (:rule r)))
        (is (= (:readers rule) (set (keys (:cells r)))))
        (is (= (:representative-domains partition)
               (into {} (map (fn [[k cells]] [k (mapv :representative cells)])) (:cells r))))
        (is (= :not-established (:executable-evaluator-refinement r)))
        (is (= :not-performed (:proof-discharge r)))))))

(deftest integer-intervals-cover-adjacent-and-arbitrary-size-cutpoints
  (let [lo Long/MIN_VALUE hi Long/MAX_VALUE
        cuts [lo (inc' lo) 0 hi]
        rule {:readers #{:fixture/x}
              :expression (into [:or] (map #(vector :>= [:read :fixture/x] [:literal %])) cuts)}
        types {:fixture/x {:type :integer}}
        r (coverage/check-coverage! rule types)
        cells (get-in r [:cells :fixture/x])]
    (is (= :conditional-abstract-coverage (:status r)) (pr-str r))
    (is (= 8 (count cells)))
    (is (= (dec' lo) (:representative (first cells))))
    (is (= (inc' hi) (:representative (peek cells))))
    (doseq [x [(dec' (dec' lo)) lo (inc' lo) -1 0 1 hi (inc' (inc' hi))]]
      (let [matches (filterv #(#'coverage/member? % x) cells)]
        (is (= 1 (count matches)))
        (is (= (into {} (map (fn [c] [c (cond (< x c) :below (> x c) :above :else :equal)])) cuts)
               (:observations (first matches))))))))

(deftest equality-and-null-cells
  (doseq [[kind literal other] [[:keyword :fixture/a :fixture/b] [:string "a" "b"]
                                [:boolean true false] [:integer 0 1]]]
    (let [types {:fixture/x {:type kind :nullable? true}}
          rule {:readers #{:fixture/x} :expression [:or [:= [:read :fixture/x] [:literal literal]]
                                                        [:= [:read :fixture/x] [:literal nil]]]}
          r (coverage/check-coverage! rule types)
          cells (get-in r [:cells :fixture/x])]
      (is (= :conditional-abstract-coverage (:status r)) (pr-str r))
      (doseq [x [nil literal other]]
        (let [matches (filterv #(#'coverage/member? % x) cells)]
          (is (= 1 (count matches)))
          (is (= (if (= x literal) :equal (if (and (= kind :integer) (some? x)) :above :different))
                 (get-in (first matches) [:observations literal])))
          (is (= (if (nil? x) :equal :different) (get-in (first matches) [:observations nil])))))))
  (let [r (coverage/check-coverage! {:readers #{:fixture/x} :expression [:= [:read :fixture/x] [:literal nil]]}
                                   {:fixture/x {:type :integer :nullable? true}})]
    (is (= :conditional-abstract-coverage (:status r)))
    (is (= [:integer-interval :nil] (mapv :kind (get-in r [:cells :fixture/x]))))))

(deftest rejects-holes-overlap-and-unqualified-representatives
  (let [check #'coverage/check-domain!
        types {:type :integer}
        cells [{:kind :integer-interval :lower nil :upper -1}
               {:kind :integer-interval :lower 0 :upper 0}
               {:kind :integer-interval :lower 1 :upper nil}]]
    (is (#'coverage/covers-integer-line? cells))
    (is (not (#'coverage/covers-integer-line? (assoc-in cells [1 :lower] 1))))
    (is (not (#'coverage/covers-integer-line? (assoc-in cells [0 :upper] 0))))
    (is (not (#'coverage/covers-integer-line? (subvec cells 1))))
    (is (not (#'coverage/covers-integer-line? (pop cells))))
    (is (thrown? clojure.lang.ExceptionInfo (#'coverage/observation! {:kind :integer-interval :lower -1 :upper 1} 0)))
    (doseq [domain [[-1 1] [-1 0 0] [-1 -2 1] [-1 0 nil]]]
      (is (thrown? clojure.lang.ExceptionInfo (check types #{0} domain))))
    (let [rule {:readers #{:fixture/x} :expression [:= [:read :fixture/x] [:literal 0]]}
          run #(coverage/check-coverage! rule {:fixture/x types})]
      (doseq [v [#'partition/audit-rule! #'values/value-in-domain?]]
        (with-redefs-fn {v (constantly true)}
          #(is (= :coverage-boundary-replaced (:reason (run))))))
      (is (= :conditional-abstract-coverage (:status (run)))))))

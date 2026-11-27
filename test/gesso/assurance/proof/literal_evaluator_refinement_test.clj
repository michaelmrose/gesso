(ns gesso.assurance.proof.literal-evaluator-refinement-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.literal-evaluator-refinement :as refinement]
            [gesso.assurance.proof.literal-partition-reduction :as reduction]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(deftest original-claim-reviewed-source-refinement
  (let [catalog (claim/catalog)
        rules (assoc (:invariants catalog) :fixture/init (:initial-condition catalog))
        results (mapv #(refinement/derive! % claim/state-types) (vals rules))]
    (doseq [[rule r] (map vector (vals rules) results)]
      (is (= :conditional-source-refinement (:status r)) (pr-str r))
      (is (= rule (:rule r)))
      (is (= :reviewed-interpreter-truth-factors-through-abstract-cells (:conclusion r)))
      (is (= #{:production :independent :observation} (set (keys (:source-artifacts r)))))
      (doseq [side [:production :independent]]
        (is (= #{:literal :read :not :and :or :implies := :not= :< :<= :> :>=}
               (get-in r [:source-artifacts side :checked-operators]))))
      (is (contains? (:conditions r) :faithful-clojure-scalar-equality-and-integer-compare))
      (is (= :not-established (:loaded-code-correspondence r) (:universal-evaluator-correspondence r)))
      (is (= :not-performed (:proof-discharge r))))
    (is (= 819 (reduce + (map :cases-checked results))))))

(deftest rejects-source-and-clause-mutations
  (doseq [side [:production :independent]]
    (let [spec (get @#'refinement/reviewed-sources side)
          source (slurp (io/resource (:path spec)))
          check #'refinement/check-source!]
      (is (= (:sha256 spec) (:sha256 (check side spec source))))
      (doseq [[before after] [[":literal a" ":literal false"]
                              [":< (neg? (compare" ":< (pos? (compare"]
                              [(if (= :production side) "(recur (inc index))" "(recur (inc i))")
                               "false"]]]
        (let [mutated (str/replace source before after)]
          (is (not= source mutated))
          (is (= :unreviewed-evaluator-source
                 (try (check side spec mutated) (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
          ;; A changed fingerprint alone cannot make a changed clause match.
          (is (= :interpreter-clause-mismatch
                 (try (check side (assoc spec :sha256 (#'refinement/digest mutated)) mutated)
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest unsupported-source-and-substituted-boundaries
  (let [rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        types {:fixture/x {:type :integer}}
        run #(refinement/derive! rule types)]
    (doseq [v [#'reduction/derive! #'presence/evaluate-expression! #'production/evaluate-rule!]]
      (with-redefs-fn {v (constantly {})}
        #(is (= :source-refinement-boundary-replaced (:reason (run))))))
    (is (= :not-established (:status (refinement/derive! (assoc rule :expression [:= [:read :fixture/x] [:read :fixture/x]]) types))))
    (is (= :conditional-source-refinement (:status (run))))))

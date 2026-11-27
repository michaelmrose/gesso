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
      (is (= #{:production :independent :observation :production-syntax :production-typing}
             (set (keys (:source-artifacts r)))))
      (doseq [side [:production :independent]]
        (is (= #{:literal :read :not :and :or :implies := :not= :< :<= :> :>=}
               (get-in r [:source-artifacts side :checked-operators]))))
      (let [identity (:runtime-identity r)]
        (is (= refinement/runtime-profile (:profile identity)))
        (is (= :matched-captured-var-identities (:status identity)))
        (is (= :before-and-after-refinement (:checking identity)))
        (is (= #{:production :independent :observation :production-syntax :production-typing}
               (set (keys (:namespace-interns identity)))))
        (is (contains? (get-in identity [:namespace-interns :production]) 'interpret!))
        (is (contains? (get-in identity [:namespace-interns :independent]) 'interpretation!))
        (is (= :not-established (:source-to-loaded-code identity)
               (:transient-concurrent-substitution-exclusion identity))))
      (let [admission (:admission-source r)]
        (is (= refinement/admission-profile (:profile admission)))
        (is (= :conditional-source-handoff (:status admission)))
        (is (= :original-expression-and-present-scalar-bindings-preserved (:conclusion admission)))
        (is (= :not-established (:validator-acceptance-equivalence admission) (:loaded-code-correspondence admission)))
        (is (= #{'evaluate-rule! 'check-bindings!} (get-in admission [:checked-functions :production])))
        (is (= #{'check-rule!} (get-in admission [:checked-functions :production-syntax])))
        (is (= #{'check-rule-type!} (get-in admission [:checked-functions :production-typing])))
        (is (= #{'observe-state! 'observation} (get-in admission [:checked-functions :observation])))
        (is (= #{'evaluate-expression!} (get-in admission [:checked-functions :independent]))))
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

(deftest local-runtime-identity-rejects-hidden-var-substitutions
  (let [rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        types {:fixture/x {:type :integer}}
        run #(refinement/derive! rule types)
        targets {'gesso.assurance.rule-eval ['interpret! 'check-bindings! 'strict-boolean!]
                 'gesso.assurance.proof.typed-presence-semantics ['interpretation! 'type-expression!]
                 'gesso.assurance.proof.typed-value-semantics ['observation 'observe-state!]
                 'gesso.assurance.rule ['check-rule!]
                 'gesso.assurance.rule-type ['check-rule-type!]}]
    (doseq [[n names] targets name names]
      (let [v (ns-resolve n name) original (var-get v)]
        ;; Even a behavior-preserving wrapper changes the execution identity.
        (with-redefs-fn {v (fn [& args] (apply original args))}
          #(let [r (run)]
             (is (= :not-established (:status r)))
             (is (= :evaluator-runtime-identity-changed (:reason r)))
             (is (not (contains? r :runtime-identity)))))))
    (let [n (the-ns 'gesso.assurance.rule-eval) name 'unexpected-runtime-intern]
      (try
        (intern n name true)
        (is (= :evaluator-runtime-identity-changed (:reason (run))))
        (finally (ns-unmap n name))))
    (is (= :conditional-source-refinement (:status (run))))))

(deftest local-runtime-identity-is-rechecked-after-reduction
  (let [v (ns-resolve 'gesso.assurance.rule-eval 'interpret!)
        original (var-get v)
        derive @#'refinement/original-reduction
        source @#'refinement/reviewed-artifacts!
        rule {:readers #{:fixture/x} :expression [:>= [:read :fixture/x] [:literal 0]]}
        types {:fixture/x {:type :integer}}]
    (try
      ;; Change a hidden root after the initial identity check. The wrapper
      ;; delegates unchanged, so truth-table agreement alone cannot detect it.
      (with-redefs-fn {#'refinement/reviewed-artifacts!
                       (fn [] (let [result (source)]
                                (alter-var-root v (constantly (fn [& args] (apply original args))))
                                result))}
        #(let [r (refinement/derive! rule types)]
           (is (= :evaluator-runtime-identity-changed (:reason r)))
           (is (not (contains? r :runtime-identity)))))
      (finally (alter-var-root v (constantly original))))
    (is (identical? derive reduction/derive!))
    (is (= :conditional-source-refinement (:status (refinement/derive! rule types))))))

(deftest rejects-altered-admission-handoffs-even-with-candidate-digest
  (doseq [[side before after]
          [[:production-syntax ":expression (:expression declaration)" ":expression [:literal true]"]
           [:production-typing ":reader-types reader-types" ":reader-types {}"]
           [:production "(interpret! (:expression checked) bindings" "(interpret! (:expression checked) {}"]
           [:observation ":presence :present :kind kind :value value" ":presence :present :kind kind :value nil"]
           [:independent "(interpretation! expression (:fields observed)" "(interpretation! expression {}"]]]
    (let [spec (get @#'refinement/reviewed-sources side)
          source (slurp (io/resource (:path spec)))
          mutated (str/replace source before after)
          check #'refinement/check-source!]
      (is (seq (:checked-admission-handoffs (check side spec source))))
      (is (not= source mutated))
      (is (= :unreviewed-evaluator-source
             (try (check side spec mutated) (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (is (= :admission-handoff-mismatch
             (try (check side (assoc spec :sha256 (#'refinement/digest mutated)) mutated)
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

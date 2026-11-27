(ns gesso.assurance.proof.literal-evaluator-refinement
  "Conditional source-level refinement of the literal comparison fragment.
   Exact reviewed source, not an attestation of loaded JVM bytecode."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.proof.literal-partition-reduction :as reduction]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.rule-eval :as production])
  (:import [java.io PushbackReader StringReader]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def profile :conditional-literal-source-refinement-v1)
(def ^:private original-reduction reduction/derive!)
(def ^:private original-production production/evaluate-rule!)
(def ^:private original-independent presence/evaluate-expression!)

(def ^:private reviewed-sources
  {:production {:path "gesso/assurance/rule_eval.clj"
                :namespace 'gesso.assurance.rule-eval :interpreter 'interpret!
                :sha256 "6c7bb9a129409960faa7c74c2ddecab5d8904d8c9da4507a0cfd87354aebe455"}
   :independent {:path "gesso/assurance/proof/typed_presence_semantics.clj"
                 :namespace 'gesso.assurance.proof.typed-presence-semantics :interpreter 'interpretation!
                 :sha256 "8c8380d2fb359c828b4d3238cf8759ca5b040f22339929195ecde73c964e1428"}
   :observation {:path "gesso/assurance/proof/typed_value_semantics.clj"
                 :sha256 "70cdf9de02d52b9d1287462decc606ea811cb3f007054c0b829a209c5c84728d"}})

(defn- reject! [reason]
  (throw (ex-info "Unqualified literal evaluator source refinement" {:reason reason})))

(defn- digest [source]
  (.formatHex (HexFormat/of)
              (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes ^String source StandardCharsets/UTF_8))))

(defn- expected-clauses [side]
  (let [production? (= :production side)
        f (if production? 'interpret! 'interpretation!)
        expr (if production? 'expression 'expr)
        idx (if production? 'index 'i)
        call (fn [arg position]
               (cond-> (list f arg (if production? 'bindings 'fields) (list 'conj 'at position))
                 (not production?) (concat '(evaluated))))
        strict (fn [arg position]
                 (let [c (call arg position)]
                   (if production? (list 'strict-boolean! c (list 'conj 'at position)) c)))
        substitutions {'A (call 'a 1) 'B (call 'b 2)
                       'BA (strict 'a 1) 'BB (strict 'b 2)
                       'BI (strict (list 'nth expr idx) idx) 'INDEX idx 'EXPR expr}]
    (walk/postwalk-replace
     substitutions
     {:literal 'a
      :read (if production?
              '(if (contains? bindings a) (get bindings a)
                 (reject! :absent-reader-evaluated at {:reader a}))
              '(let [observation (get fields a)]
                 (if (= :present (:presence observation)) (:value observation)
                   (reject! :read-absent-reader at {:reader a}))))
      :not '(not BA)
      :and '(loop [INDEX 1] (if (= INDEX (count EXPR)) true (if BI (recur (inc INDEX)) false)))
      :or '(loop [INDEX 1] (if (= INDEX (count EXPR)) false (if BI true (recur (inc INDEX)))))
      :implies '(if BA BB true)
      := '(= A B) :not= '(not= A B)
      :< '(neg? (compare A B)) :<= '(not (pos? (compare A B)))
      :> '(pos? (compare A B)) :>= '(not (neg? (compare A B)))})))

(defn- check-clauses! [side forms interpreter]
  (let [production? (= :production side)
        matches (filter #(and (seq? %) (= 'defn- (first %)) (= interpreter (second %))) forms)
        [_ _ args & body] (first matches)
        shell (last body)
        [_ bindings dispatch] shell
        [_ discriminator & branches] dispatch
        entries (partition 2 (butlast branches))
        expected (expected-clauses side)]
    (when-not (and (= 1 (count matches))
                   (= args (if production? '[expression bindings at] '[expr fields at evaluated]))
                   (= (vec (butlast body)) (if production? [] '[(swap! evaluated conj at)]))
                   (= 3 (count shell)) (= 'let (first shell))
                   (= bindings (if production? '[[op a b] expression] '[[op a b] expr]))
                   (= 'case (first dispatch)) (= 'op discriminator)
                   (odd? (count branches)) (= (count entries) (count (set (map first entries))))
                   (= expected (select-keys (into {} (map vec) entries) (keys expected))))
      (reject! :interpreter-clause-mismatch))
    (set (keys expected))))

(defn- check-source! [side spec source]
  (when-not (= (:sha256 spec) (digest source)) (reject! :unreviewed-evaluator-source))
  (cond-> {:path (:path spec) :sha256 (:sha256 spec)}
    (:interpreter spec)
    (assoc :checked-operators
           (with-open [r (PushbackReader. (StringReader. source))]
             (binding [*read-eval* false *ns* (the-ns (:namespace spec))]
               (let [forms (loop [result []]
                             (let [form (read {:eof ::end} r)]
                               (if (= ::end form) result (recur (conj result form)))))]
                 (check-clauses! side forms (:interpreter spec))))))))

(defn- reviewed-artifacts! []
  (into {} (map (fn [[side spec]]
                  (let [resource (io/resource (:path spec))]
                    (when-not resource (reject! :evaluator-source-unavailable))
                    [side (check-source! side spec (slurp resource :encoding "UTF-8"))]))) reviewed-sources))

(defn derive!
  "Check the exact reviewed interpreter clauses and compose their source-level
   argument with the original rule's abstract reduction. The argument is
   restricted to successfully admitted required-reader literal expressions."
  [rule state-types]
  (let [base {:profile profile :scope :reviewed-source-admitted-literal-fragment-only
              :loaded-code-correspondence :not-established
              :universal-evaluator-correspondence :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (and (identical? original-reduction reduction/derive!)
                     (identical? original-production production/evaluate-rule!)
                     (identical? original-independent presence/evaluate-expression!))
        (reject! :source-refinement-boundary-replaced))
      (let [artifacts (reviewed-artifacts!)
            r (original-reduction rule state-types)]
        (when-not (and (= reduction/profile (:profile r)) (= :conditional-abstract-reduction (:status r))
                       (= rule (:rule r)) (= (select-keys state-types (:readers rule)) (:reader-types r)))
          (reject! :unqualified-abstract-reduction))
        (assoc base :status :conditional-source-refinement
               :rule rule :reader-types (:reader-types r) :source-artifacts artifacts
               :cases-checked (:cases-checked r)
               :conclusion :reviewed-interpreter-truth-factors-through-abstract-cells
               :argument [:required-read-and-literal-denotation :scalar-comparison-observations
                          :boolean-structural-induction :short-circuit-loop-induction]
               :conditions (into (:conditions r)
                                 #{:successful-admission-preserves-source-and-bindings
                                   :faithful-clojure-scalar-equality-and-integer-compare
                                   :standard-clojure-control-flow-and-termination
                                   :loaded-code-implements-reviewed-source-without-substitution})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-source-refinement))))))

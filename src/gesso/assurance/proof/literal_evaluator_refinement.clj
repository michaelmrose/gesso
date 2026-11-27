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
                 :namespace 'gesso.assurance.proof.typed-value-semantics
                 :sha256 "70cdf9de02d52b9d1287462decc606ea811cb3f007054c0b829a209c5c84728d"}
   :production-syntax {:path "gesso/assurance/rule.clj" :namespace 'gesso.assurance.rule
                       :sha256 "51f4a99ed469f46a5523a17bc0380c81ff0c9218caf1514ca8b9a8a462232738"}
   :production-typing {:path "gesso/assurance/rule_type.clj" :namespace 'gesso.assurance.rule-type
                       :sha256 "374f55133f4148e7cc4889334ef25b418f1d77cac86b25315156983e516d53e8"}})

(defn- reject! [reason]
  (throw (ex-info "Unqualified literal evaluator source refinement" {:reason reason})))

(def runtime-profile :literal-evaluator-local-runtime-identity-v1)

(def ^:private runtime-namespaces
  {:production 'gesso.assurance.rule-eval
   :independent 'gesso.assurance.proof.typed-presence-semantics
   :observation 'gesso.assurance.proof.typed-value-semantics
   :production-syntax 'gesso.assurance.rule
   :production-typing 'gesso.assurance.rule-type})

(def ^:private original-runtime
  ;; Capture only after the evaluator dependency namespaces have loaded.
  ;; Raw roots and Var identities are process-local objects, not portable
  ;; fingerprints. Include private interpreters, helpers and constants.
  (into {} (map (fn [[role sym]]
                  (let [n (the-ns sym)]
                    [role {:namespace n
                           :interns (into {} (map (fn [[name ^clojure.lang.Var v]]
                                                   (when-not (.hasRoot v)
                                                     (reject! :unbound-evaluator-var))
                                                   [name {:var v :root (.getRawRoot v)}]))
                                          (ns-interns n))}]))) runtime-namespaces))

(defn- runtime-identity! []
  (doseq [[role sym] runtime-namespaces]
    (let [{:keys [namespace interns]} (original-runtime role)
          current (find-ns sym)]
      (when-not (and (identical? namespace current)
                     (= (set (keys interns)) (set (keys (ns-interns current))))
                     (every? (fn [[name {:keys [^clojure.lang.Var var root]}]]
                               (and (identical? var (get (ns-interns current) name))
                                    (.hasRoot var)
                                    (identical? root (.getRawRoot var))
                                    (not (thread-bound? var))
                                    (identical? root (var-get var)))) interns))
        (reject! :evaluator-runtime-identity-changed))))
  {:profile runtime-profile
   :status :matched-captured-var-identities
   :scope :process-local-inspection-only
   :namespace-interns (into {} (map (fn [[role {:keys [interns]}]]
                                      [role (set (keys interns))])) original-runtime)
   :capture-origin :namespace-load-time
   :source-to-loaded-code :not-established
   :transient-concurrent-substitution-exclusion :not-established})

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

(def admission-profile :literal-admission-source-handoff-v1)

(def ^:private admission-handoffs
  ;; Paths address body forms after docstring and arguments. Whole-source
  ;; digests also pin validation code outside these exact handoff checks.
  {:production-syntax
   {'check-rule! ['[declaration] 4
                  {[3 1] '[readers (:readers declaration)]
                   [3 6 1] '[budget (atom 0) used (atom #{})]
                   [3 6 2] '(check-expression! (:expression declaration) [:expression] 0 budget readers used)
                   [3 6 3] '(when-not (= @used readers)
                              (invalid! :unused-reader-declaration
                                        {:at [:readers] :unused (set/difference readers @used)}))
                   [3 6 4 :expression] '(:expression declaration)
                   [3 6 4 :readers] 'readers}]}
   :production-typing
   {'check-rule-type! ['[declaration reader-types] 1
                       {[0 0] 'let
                        [0 1] '[syntax (rule/check-rule! declaration)
                                types (validate-reader-types! (:readers syntax) reader-types)
                                result (type-expression! (:expression syntax) [:expression] types
                                                         {:present #{} :nonnull #{}})]
                        [0 2] '(boolean! result [:expression])
                        [0 3 :expression] '(:expression syntax)
                        [0 3 :readers] '(:readers syntax)
                        [0 3 :reader-types] 'reader-types}]}
   :production
   {'check-bindings! ['[checked bindings] 4 {[3] 'bindings}]
    'evaluate-rule! ['[declaration reader-types bindings] 1
                     {[0 0] 'let
                      [0 1] '[checked (rule-type/check-rule-type! declaration reader-types)
                              _ (check-bindings! checked bindings)
                              value (strict-boolean! (interpret! (:expression checked) bindings [:expression])
                                                     [:expression])]
                      [0 2 :expression] '(:expression checked)
                      [0 2 :readers] '(:readers checked)
                      [0 2 :value] 'value}]}
   :observation
   {'observation ['[id descriptor state] 1
                  {[0] '(if-not (contains? state id)
                          (if (:optional? descriptor)
                            {:presence :absent}
                            (reject! :missing-required-reader [:state id] {:reader id}))
                          (let [value (get state id) kind (value-kind! value)]
                            (when-not (value-in-domain* descriptor value kind)
                              (reject! :ill-typed-present-value [:state id]
                                       {:reader id :declared-type (:type descriptor) :actual-kind kind}))
                            {:presence :present :kind kind :value value}))}]
    'observe-state! ['[reader-types state] 1
                     {[0 0] 'let
                      [0 1] '[types (types! reader-types) _ (exact-plain-map! state [:state])
                              provided (set (keys state)) unexpected (set/difference provided (set (keys types)))]
                      [0 2] '(when (seq unexpected) (reject! :undeclared-readers [:state] {:unexpected unexpected}))
                      [0 3 0] 'let
                      [0 3 1] '[fields (into {} (map (fn [[id descriptor]] [id (observation id descriptor state)])) types)]
                      [0 3 2 :fields] 'fields}]}
   :independent
   {'evaluate-expression! ['[expression reader-types state] 1
                           {[0 0] 'let
                            [0 1] '[observed (values/observe-state! reader-types state)
                                    used (atom #{}) budget (atom 0)
                                    checked-type (type-expression! expression reader-types {:present #{} :nonnull #{}}
                                                                   [:expression] 0 budget used)
                                    declared (set (keys reader-types))]
                            [0 2] '(nonnullable-boolean! checked-type [:expression])
                            [0 3] '(when-not (= @used declared)
                                      (reject! :reader-footprint-mismatch [:reader-types] {:unused (set/difference declared @used)}))
                            [0 4 0] 'let
                            [0 4 1] '[visited (atom []) evaluated (interpretation! expression (:fields observed) [:expression] visited)]
                            [0 4 2 :expression] 'expression
                            [0 4 2 :source-readers] 'declared
                            [0 4 2 :value] 'evaluated}]}})

(defn- form-at [form path]
  (reduce (fn [node k]
            (cond (and (integer? k) (sequential? node)) (nth node k ::missing)
                  (map? node) (get node k ::missing)
                  :else ::missing)) form path))

(defn- check-handoffs! [side forms]
  (let [expected (admission-handoffs side)]
    (when-not (seq expected) (reject! :unknown-admission-source))
    (doseq [[name [args body-count checks]] expected]
      (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %)) (= name (second %))) forms)
            declaration (drop 2 (first matches))
            [actual-args & body] (if (string? (first declaration)) (rest declaration) declaration)]
        (when-not (and (= 1 (count matches)) (= args actual-args)
                       (= body-count (count body))
                       (every? (fn [[path form]] (= form (form-at body path))) checks))
          (reject! :admission-handoff-mismatch))))
    (set (keys expected))))

(defn- check-source! [side spec source]
  (when-not (= (:sha256 spec) (digest source)) (reject! :unreviewed-evaluator-source))
  (with-open [r (PushbackReader. (StringReader. source))]
    (binding [*read-eval* false *ns* (the-ns (:namespace spec))]
      (let [forms (loop [result []]
                    (let [form (read {:eof ::end} r)]
                      (if (= ::end form) result (recur (conj result form)))))
            artifact (cond-> {:path (:path spec) :sha256 (:sha256 spec)}
                       (:interpreter spec) (assoc :checked-operators (check-clauses! side forms (:interpreter spec))))]
        (assoc artifact :checked-admission-handoffs (check-handoffs! side forms))))))

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
      (let [before (runtime-identity!)
            artifacts (reviewed-artifacts!)
            r (original-reduction rule state-types)
            after (runtime-identity!)]
        (when-not (= before after) (reject! :evaluator-runtime-identity-changed))
        (when-not (and (= reduction/profile (:profile r)) (= :conditional-abstract-reduction (:status r))
                       (= rule (:rule r)) (= (select-keys state-types (:readers rule)) (:reader-types r)))
          (reject! :unqualified-abstract-reduction))
        (assoc base :status :conditional-source-refinement
               :rule rule :reader-types (:reader-types r) :source-artifacts artifacts
               :runtime-identity (assoc after :checking :before-and-after-refinement)
               :admission-source {:profile admission-profile
                                  :status :conditional-source-handoff
                                  :scope :successfully-validated-required-reader-inputs-only
                                  :checked-functions (into {} (map (fn [[role artifact]]
                                                                   [role (:checked-admission-handoffs artifact)])) artifacts)
                                  :conclusion :original-expression-and-present-scalar-bindings-preserved
                                  :validator-acceptance-equivalence :not-established
                                  :loaded-code-correspondence :not-established}
               :cases-checked (:cases-checked r)
               :conclusion :reviewed-interpreter-truth-factors-through-abstract-cells
               :argument [:required-read-and-literal-denotation :scalar-comparison-observations
                          :boolean-structural-induction :short-circuit-loop-induction]
               :conditions (into (:conditions r)
                                 #{:successful-validation-of-original-required-reader-inputs
                                   :faithful-immutable-map-and-observation-semantics
                                   :faithful-clojure-scalar-equality-and-integer-compare
                                   :standard-clojure-control-flow-and-termination
                                   :loaded-code-implements-reviewed-source-without-substitution})))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-source-refinement))))))

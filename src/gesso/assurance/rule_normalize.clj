(ns gesso.assurance.rule-normalize
  "Conservative canonical identities for the *interpreted*, typed Gesso rule
   language (v1225). Rule normalization is DATA ONLY, not proof admission.

   (normalize-rule! declaration reader-types) always checks the ORIGINAL v1219
   syntax and the ORIGINAL v1221 nominal types before deriving an identity.
   It never accepts a cached report, fingerprint, or purported certificate.

   The only denotational equivalences currently normalized are:
   * integer literals are mathematical integers (7 and 7N normalize alike);
   * plain map/set insertion order is irrelevant;
   * omitted optional?/nullable? flags mean false, exactly as v1221 defines.

   Operator and argument order, including :and/:or, are PRESERVED: the v1223
   evaluator short-circuits left-to-right, and the type checker uses order-
   sensitive presence and non-nil refinements. Do not sort, flatten, fold,
   commute, or otherwise simplify predicates without a separate soundness
   argument for their type and observation semantics.

   Fingerprints include the rule language, nominal reader-type, interpreter,
   normalization, and canonical subject identity versions. The identity names
   only this *declarative rule under these semantics*, NOT the application,
   installed handler, runtime snapshot, arbitrary Clojure execution, or any
   checked theorem. It cannot be used to authorize a transaction."
  (:require [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.subject :as subject]))

(def normalization-version 1)
(def normalized-type :gesso.assurance.rule-normalize/unadmitted-rule)

(def ^:private semantic-rule-subject :gesso.assurance/rule-semantics-v1)

(defn- normalize-scalar
  [value]
  ;; v1219 only admits nil, booleans, integers, strings and keywords here.
  ;; bigint maps Java Integer/Long/BigInteger and Clojure BigInt values to the
  ;; same mathematical value, independent of how the EDN was printed.
  (if (integer? value) (bigint value) value))

(defn- normalize-literal
  [value]
  (if (set? value)
    (let [normalized (into #{} (map normalize-scalar) value)]
      ;; Never silently collapse set members if two distinct original members
      ;; have an identical normalized representation. Although v1223 uses
      ;; Clojure membership semantics, a collision should be re-examined at
      ;; the representation boundary, not treated as an established theorem.
      (when (not= (count normalized) (count value))
        (throw (ex-info "Ambiguous normalized Gesso rule literal set."
                        {:error/type ::normalized-literal-collision
                         :reason :integer-normalization-collision})))
      normalized)
    (normalize-scalar value)))

(defn- normalize-expression
  [expression]
  ;; The syntax checker has already established every expression is a closed
  ;; vector with one of the supported v1219 operators and valid arity.
  (case (first expression)
    :literal [:literal (normalize-literal (second expression))]
    :read [:read (second expression)]
    (mapv (fn [part index]
            (if (zero? index) part (normalize-expression part)))
          expression (range))))

(defn- normalize-descriptor
  [descriptor]
  (cond-> {:type (:type descriptor)
           :optional? (true? (:optional? descriptor))
           :nullable? (true? (:nullable? descriptor))}
    (= :enum (:type descriptor))
    (assoc :name (:name descriptor)
           :values (:values descriptor))))

(defn normalize-rule!
  "Revalidate and canonicalize an untrusted rule declaration and its exact
   nominal field-type footprint. Return copyable, UNADMITTED data containing a
   stable *structural interpretation identity*, not a semantic-equivalence
   proof or a handle. Both inputs must be source declarations, never reports.

   The identity is independent of map/set insertion order, EDN presentation,
   integer representation, and omitted default-false descriptor flags. It is
   sensitive to operator/argument order, rule contents, named enum domains,
   optional/nullable declarations, and checker/interpreter version changes.

   This procedure intentionally does not normalize logically equivalent but
   syntactically different expressions such as (not (not P)) and P; treating
   them as distinct is safe and conservatively invalidates cached evidence."
  [declaration reader-types]
  (let [checked (rule-type/check-rule-type! declaration reader-types)
        semantics {:normalization-version normalization-version
                   :syntax-version rule/rule-version
                   :type-version rule-type/type-version
                   :evaluator-version rule-eval/evaluator-version
                   :readers (:readers checked)
                   :reader-types (into {}
                                       (map (fn [[id descriptor]]
                                              [id (normalize-descriptor descriptor)]))
                                       (:reader-types checked))
                   :expression (normalize-expression (:expression checked))}
        identity (subject/describe-subject
                  {:kind :rule
                   :id semantic-rule-subject
                   :interface semantics})]
    {:type normalized-type
     :version normalization-version
     :canonical-format subject/canonical-format
     :fingerprint (:fingerprint identity)
     :semantics semantics
     :checking :syntax-and-types
     :normalization :conservative-interpretation
     :evaluation :not-performed
     :proof-status :not-proved
     :admission :not-admitted
     :runtime-enforcement :not-established
     :authority :none}))

(defn same-normalized-rule?
  "Whether two independently checked declarations have an identical v1225
   normalized-rule fingerprint. This checks structural identity within the
   explicitly defined normalization rules; it does not prove general logical
   equivalence, application binding, or deployment correspondence."
  [left-declaration left-types right-declaration right-types]
  (= (:fingerprint (normalize-rule! left-declaration left-types))
     (:fingerprint (normalize-rule! right-declaration right-types))))

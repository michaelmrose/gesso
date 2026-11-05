(ns gesso.assurance.proof.preservation-composition-soundness
  "Restricted, independent S4 conjunction-composition schema (v1359).

   Consider a source-declared, well-typed invariant I = C1 AND ... AND Cn.
   If every *independently well-typed* clause has a conditional preservation
   argument from ONE of the independent v1353 frame, v1355 literal, or v1357
   same-origin copy schemas, then, assuming those schemas and typed expression
   extensionality are sound:

      I(s) AND T(s,x,s') => each Ci(s') => I(s').

   Frame clauses use Ci(s) plus the exact frame; literal/copy clauses establish
   Ci(s') directly. All clauses must be covered; a single open clause makes
   the whole composition OPEN. No cross-clause guard refinement is invented:
   independently ill-typed fragments remain open even if the complete rule is
   well typed. Immediate top-level :and operands are the ONLY split points;
   a nested :and remains an intact clause so its own presence guards apply.

   All constructors are rerun against ORIGINAL declarations. Family reports
   are not accepted as user-supplied evidence. Public Var substitution cannot
   silently fabricate a supported clause: captured source-deriving functions
   remain the reference for comparison. A concrete witness is checked against
   the independent guarded relation and typed interpretation, not a physical
   transaction. This conditional schema issues NO certificate, operation
   admission, implementation proof, or XTDB authority. Universal correctness
   of S1 and family schemas remains open."
  (:require [clojure.set :as set]
            [gesso.assurance.rule :as rule]
            [gesso.assurance.rule-normalize :as typed-rule]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.typed-presence-semantics :as presence]
            [gesso.assurance.proof.frame-soundness :as frame]
            [gesso.assurance.proof.literal-assignment-soundness :as literal]
            [gesso.assurance.proof.field-copy-soundness :as copy]))

(def semantics-version 1)
(def report-type
  :gesso.assurance.proof.preservation-composition-soundness/conditional-lemma)
(def witness-type
  :gesso.assurance.proof.preservation-composition-soundness/witness-audit)

(def ^:private invariant-keys #{:id :readers :expression})
(def ^:private max-clauses 32)
(def ^:private family-order [:frame :literal :copy])
(def ^:private family-definitions
  {:frame {:captured frame/derive-frame-lemma!
           :public #'frame/derive-frame-lemma!
           :type frame/report-type
           :version frame/semantics-version
           :success :conditional-frame-rule-instance
           :claim :matching-transition-preserves-invariant-truth}
   :literal {:captured literal/derive-literal-lemma!
             :public #'literal/derive-literal-lemma!
             :type literal/report-type
             :version literal/semantics-version
             :success :conditional-literal-rule-instance
             :claim :matching-transition-establishes-post-invariant}
   :copy {:captured copy/derive-copy-lemma!
          :public #'copy/derive-copy-lemma!
          :type copy/report-type
          :version copy/semantics-version
          :success :conditional-same-origin-copy-rule
          :claim :matching-transition-establishes-post-equality}})
(def ^:private captured-observer independent/observe-transition!)

(defn- reject! [reason at details]
  (throw (ex-info "Invalid conditional preservation composition."
                  (merge {:preservation-composition-soundness/rejection true
                          :reason reason :at at} details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- exact-invariant! [invariant state-types]
  (when-not (and (plain-map? invariant)
                 (nil? (meta invariant))
                 (= invariant-keys (set (keys invariant)))
                 (qualified-id? (:id invariant)))
    (reject! :invalid-invariant-source [:invariant] {}))
  (let [source (select-keys invariant [:readers :expression])
        syntax (rule/check-rule! source)
        readers (:readers syntax)]
    (when-not (set/subset? readers (set (keys state-types)))
      (reject! :invariant-reads-outside-state [:invariant :readers]
               {:unknown (set/difference readers (set (keys state-types)))}))
    (let [admission (typed-rule/normalize-rule!
                     source (select-keys state-types readers))]
      (when-not (= readers (get-in admission [:semantics :readers]))
        (reject! :full-typed-reader-disagreement [:invariant] {}))
      admission)))

(defn- clauses! [expression]
  (let [parts (if (= :and (first expression))
                (subvec expression 1)
                [expression])]
    (when (> (count parts) max-clauses)
      (reject! :clause-limit [:invariant :expression]
               {:maximum max-clauses :actual (count parts)}))
    parts))

(defn- expression-readers
  "Reconstruct exact clause readers from a previously syntax-validated AST.
   A :literal never contributes readers; a nested :and stays intact."
  [expression]
  (let [[op & operands] expression]
    (case op
      :read #{(first operands)}
      :literal #{}
      (apply set/union #{} (map expression-readers operands)))))

(defn- fragment! [invariant expression]
  {:id (:id invariant)
   :readers (expression-readers expression)
   :expression expression})

(defn- typed-fragment-admission [fragment state-types]
  ;; A complete [:and [:present optional] [:> optional 0]] may type check,
  ;; while [:> optional 0] alone does not. That fragment is OPEN, never proved
  ;; by borrowing a guard that a different clause establishes.
  (try
    {:status :admitted
     :report (typed-rule/normalize-rule!
              (select-keys fragment [:readers :expression])
              (select-keys state-types (:readers fragment)))}
    (catch clojure.lang.ExceptionInfo e
      (if (contains? #{:gesso.assurance.rule-type/invalid-rule-type
                       :gesso.assurance.rule/invalid-rule}
                     (:error/type (ex-data e)))
        {:status :open-not-independently-typed
         :reason (:reason (ex-data e))}
        (throw e)))))

(defn- family-report! [family declaration fragment typed-fingerprint transition-fingerprint]
  (let [{:keys [captured public type version success claim]}
        (get family-definitions family)
        report (captured declaration fragment)]
    ;; If a public derivation Var was replaced, demand source-derived equality
    ;; rather than trusting a forged :status or a cached/copyable report.
    (when-not (identical? (var-get public) captured)
      (when-not (= report ((var-get public) declaration fragment))
        (reject! :substituted-family-report [:families family] {})))
    (when-not (and (= type (:type report))
                   (= version (:version report))
                   (= fragment (:source-invariant report))
                   (= (:id fragment) (:invariant-id report))
                   (= (:readers fragment) (:readers report))
                   (= typed-fingerprint (:typed-invariant-fingerprint report))
                   (= transition-fingerprint (:transition-fingerprint report))
                   (= :conditional-schema-only (:proof-status report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report))
                   (if (= success (:status report))
                     (= claim (:conditional-claim report))
                     (nil? (:conditional-claim report))))
      (reject! :family-report-contract-changed [:families family] {}))
    report))

(defn- clause-outcome! [declaration invariant description index expression]
  (let [fragment (fragment! invariant expression)
        admission (typed-fragment-admission
                   fragment (:state-types declaration))]
    (if (not= :admitted (:status admission))
      {:index index
       :expression expression
       :readers (:readers fragment)
       :supporting-family nil
       :status :open-not-independently-typed
       :reason (:reason admission)
       :tried-families []}
      (let [fingerprint (get-in admission [:report :fingerprint])]
        (loop [[family & rest-families] family-order
               attempts []]
          (if-not family
            {:index index
             :expression expression
             :readers (:readers fragment)
             :typed-clause-fingerprint fingerprint
             :supporting-family nil
             :status :open-no-supported-family
             :tried-families attempts}
            (let [r (family-report! family declaration fragment fingerprint
                                    (:fingerprint description))
                  attempt {:family family :status (:status r)}
                  all-attempts (conj attempts attempt)]
              (if (= (:status r) (get-in family-definitions [family :success]))
                {:index index
                 :expression expression
                 :readers (:readers fragment)
                 :typed-clause-fingerprint fingerprint
                 :supporting-family family
                 :schema (:schema r)
                 :status :conditionally-supported-clause
                 :tried-families all-attempts}
                (recur rest-families all-attempts)))))))))

(defn derive-composed-lemma!
  "Re-derive a restricted conjunctive preservation argument from ORIGINAL
   transition and invariant. ALL immediate clauses must be independently
   typed and supported. Partial coverage is reported OPEN, never certified."
  [declaration invariant]
  (let [description (transition/describe-transition! declaration)
        admission (exact-invariant! invariant (:state-types declaration))
        clauses (clauses! (:expression invariant))
        outcomes (mapv (fn [i expression]
                         (clause-outcome! declaration invariant description
                                          i expression))
                       (range (count clauses)) clauses)
        supported (into #{} (map :index)
                        (filter #(= :conditionally-supported-clause (:status %))
                                outcomes))
        all-indices (set (range (count clauses)))
        open (set/difference all-indices supported)
        eligible? (empty? open)]
    {:type report-type
     :version semantics-version
     :source-declaration declaration
     :source-invariant invariant
     :transition-fingerprint (:fingerprint description)
     :invariant-id (:id invariant)
     :typed-invariant-fingerprint (:fingerprint admission)
     :composition-shape (if (= :and (first (:expression invariant)))
                          :top-level-conjunction :single-clause)
     :clauses outcomes
     :conditionally-supported-clause-indices supported
     :open-clause-indices open
     :scope :complete-immediate-conjunctive-clauses-only
     :schema :conjunction-of-independently-justified-conditional-preservation
     :premises #{:typed-whole-invariant
                 :independently-typed-conjuncts
                 :all-clauses-covered
                 :family-conditional-soundness
                 :pure-typed-conjunction-extensionality
                 :exact-abstract-guarded-transition}
     :conditional-claim (when eligible?
                          :invariant-and-matching-transition-imply-post-invariant)
     :status (if eligible? :conditional-composed-rule-instance
                 :open-unproved-clause)
     :typed-expression-soundness :not-independently-proved
     :family-rule-soundness :not-independently-proved
     :transition-implementation-soundness :not-independently-proved
     :proof-status :conditional-schema-only
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))

(defn- observed-transition! [declaration before inputs after]
  (let [report (captured-observer declaration before inputs after)]
    (when-not (identical? (var-get #'independent/observe-transition!)
                           captured-observer)
      (when-not (= report (independent/observe-transition!
                           declaration before inputs after))
        (reject! :substituted-transition-observation [:witness] {})))
    (when-not (and (= independent/report-type (:type report))
                   (= independent/semantics-version (:version report))
                   (= declaration (:source-declaration report))
                   (= before (:before report))
                   (= inputs (:inputs report))
                   (= after (:after report))
                   (= :independent-typed-guarded-relation (:checking report))
                   (= :prestate-guard-simultaneous-writes-exact-frame
                      (:semantics report))
                   (= (:writes declaration) (:writes report))
                   (= (set/difference (set (keys (:state-types declaration)))
                                      (:writes declaration))
                      (:frame-fields report))
                   (every? #(instance? Boolean (get report %))
                           [:guard-holds? :assignments-hold?
                            :frame-holds? :matches?])
                   (= (:matches? report)
                      (and (:guard-holds? report)
                           (:assignments-hold? report)
                           (:frame-holds? report)))
                   (= :not-proved (:proof-status report))
                   (= :not-admitted (:admission report))
                   (= :none (:authority report)))
      (reject! :independent-observation-contract-changed [:witness] {}))
    report))

(defn audit-composed-witness!
  "Re-derive the full clause ledger and check one concrete abstract witness.
   A matching witness supporting every clause is NOT a universal proof."
  [declaration invariant before inputs after]
  (let [lemma (derive-composed-lemma! declaration invariant)
        observed (observed-transition! declaration before inputs after)
        readers (:readers invariant)
        types (select-keys (:state-types declaration) readers)
        expression (:expression invariant)
        pre (presence/evaluate-expression!
             expression types (select-keys before readers))
        post (presence/evaluate-expression!
              expression types (select-keys after readers))
        pre-value (:value pre)
        post-value (:value post)
        eligible? (= :conditional-composed-rule-instance (:status lemma))
        matches? (:matches? observed)]
    (when (and eligible? matches? pre-value (not post-value))
      (reject! :independent-composition-preservation-disagreement
               [:witness] {:invariant (:id invariant)}))
    {:type witness-type
     :version semantics-version
     :transition-fingerprint (:transition-fingerprint lemma)
     :invariant-id (:id invariant)
     :lemma-status (:status lemma)
     :conditionally-supported-clause-indices
     (:conditionally-supported-clause-indices lemma)
     :open-clause-indices (:open-clause-indices lemma)
     :matches-transition? matches?
     :before-invariant pre-value
     :after-invariant post-value
     :preservation-implication-holds? (or (not pre-value) post-value)
     :status (cond
               (not eligible?) :open-unproved-clause
               (not matches?) :nonmatching-transition
               :else :checked-conditional-composition-instance)
     :checking :independent-abstract-relation-and-typed-expression
     :proof-status :not-universally-proved
     :physical-observation :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

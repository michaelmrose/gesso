(ns gesso.assurance.proof.copy-preservation
  "A narrowly checked cross-field COPY preservation inference (v1265).

   Under the original v1233 abstract transition relation, an assigned field
   has an exact post-state origin: a typed literal, PRE-state field, or INPUT
   field. An unchanged, REQUIRED frame field has its own PRE-state origin.

   If two distinct, independently well-typed post-state readers have the SAME
   total origin, the existing v1235 derivation checker can establish their
   equality using two exact field equalities and one equality substitution.
   This justifies the typed post-invariant [:= [:read a] [:read b]], conditional on the matching abstract relation.
   A flat :and may combine such self-contained equalities with independently
   well-typed frame-only clauses. No conjunct is reordered or flattened.

   This intentionally does NOT infer arithmetic inequalities from equality,
   reason about different origins, use optional PRE-field equalities, infer
   whole-rule optional refinements, or prove that a supplied operation was
   actually executed. These reports are COPYABLE, UNADMITTED inference data.
   Original initial-state, invariant, implementation, checker soundness, and
   certificate obligations remain open."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.transition :as abstract-transition]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def copy-preservation-version 1)
(def report-type
  :gesso.assurance.proof.copy-preservation/unadmitted-copy-preservation)

(def ^:private identity-id :gesso.assurance.proof/copy-preservation-v1)
(def ^:private relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)
(def ^:private checker-obligations
  #{:certificate-admission :production-transition-correspondence
    :current-runtime-authorization :atomic-transaction-enforcement})
(def ^:private obligation-categories
  #{:initial-state :preservation :assumption :implementation})

(defn- fail! [reason details]
  (throw (ex-info "Invalid Gesso copy preservation inference."
                  (merge {:error/type ::invalid-copy-preservation
                          :reason reason}
                         details))))

(defn- expression-readers
  "Syntactic field readers of an expression checked by the source normalizer."
  [expression]
  (case (first expression)
    :read #{(second expression)}
    :literal #{}
    (reduce set/union #{} (map expression-readers (rest expression)))))

(defn- standalone-typed?
  "A clause depending on earlier :and refinements must remain OPEN. A
   successful but changed result contract is rejected, not trusted."
  [expression readers state-types]
  (try
    (let [types (select-keys state-types readers)
          checked (rule-type/check-rule-type!
                   {:readers readers :expression expression} types)]
      (when-not (and (= rule-type/checked-type (:type checked))
                     (= rule-type/type-version (:version checked))
                     (= readers (:readers checked))
                     (= expression (:expression checked))
                     (= types (:reader-types checked))
                     (= :boolean (:result-type checked))
                     (= :syntax-and-types (:checking checked))
                     (= :not-performed (:evaluation checked))
                     (= :not-proved (:proof-status checked))
                     (= :not-admitted (:admission checked))
                     (= :none (:authority checked)))
        (fail! :rule-type-checker-contract-changed {:expression expression}))
      true)
    (catch clojure.lang.ExceptionInfo e
      (if (= ::invalid-copy-preservation (:error/type (ex-data e)))
        (throw e)
        false))))

(defn- origin
  "The precise TOTAL value term available for a post-field. An optional
   FRAME field has no total pre-value and is deliberately unsupported."
  [{:keys [state-types input-types writes assignments frame-fields]} field]
  (cond
    (contains? frame-fields field)
    (when-not (true? (:optional? (get state-types field)))
      [:pre field])

    (contains? writes field)
    (let [[op source] (get assignments field)]
      (case op
        :literal [:literal source]
        :read (cond
                (contains? state-types source) [:pre source]
                (contains? input-types source) [:input source]
                :else nil)
        nil))

    :else nil))

(defn- field-step
  "An original assignment or REQUIRED frame equality. Both are checked by
   the independent v1235 rule table, not interpreted a second time here."
  [semantics field id]
  (let [term (origin semantics field)]
    (if (contains? (:frame-fields semantics) field)
      {:id id :rule :frame :field field
       :conclusion [:eq [:post field] term]}
      {:id id :rule :assignment :field field
       :conclusion [:eq [:post field] term]})))

(defn- checked-derivation!
  [transition description steps goal context]
  (let [result (derivation/check-transition-derivation!
                {:transition transition :steps steps :goal goal})]
    (when-not (and (= derivation/report-type (:type result))
                   (= derivation/derivation-version (:version result))
                   (= derivation/derivation-version (:checker-version result))
                   (= :checked-abstract-transition-derivation
                      (:evidence-class result))
                   (= (:fingerprint description) (:transition-fingerprint result))
                   (= (:subject-fingerprint description)
                      (:subject-fingerprint result))
                   (= goal (:goal result))
                   (= (count steps) (:rule-count result))
                   (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint result))
                   (= #{relation-premise} (:open-premises result))
                   (= checker-obligations (:missing-obligations result))
                   (= #{} (:trusted-assumptions result))
                   (= :matching-abstract-transition-only (:claim-scope result))
                   (= :pure-abstract-transition-only
                      (:implementation-boundary result))
                   (= :abstract-transition-derivation-checked (:checking result))
                   (= :conditional-abstract-inference (:proof-status result))
                   (= :conditional-on-matching-abstract-relation
                      (:conclusion-status result))
                   (= :not-performed (:assumption-discharge result))
                   (= :not-issued (:certificate result))
                   (= :not-admitted (:admission result))
                   (= :not-established (:runtime-enforcement result))
                   (= :not-established (:implementation-correspondence result))
                   (= :none (:authority result)))
      (fail! :derivation-checker-contract-changed {:context context}))
    {:conclusion goal
     :derivation-fingerprint (:fingerprint result)
     :scope :matching-abstract-transition-only}))

(defn- prove-copy-equality!
  "A post equality is established by the v1235 substitution rule, NOT by
   merely noticing equal source names. With E = (post b = origin) and
   P = (post a = origin), reverse-substitute E into P's right operand."
  [transition description a b]
  (let [semantics (:semantics description)
        a-step (field-step semantics a
                           :gesso.assurance.proof.copy-preservation/left)
        b-step (field-step semantics b
                           :gesso.assurance.proof.copy-preservation/right)
        goal [:eq [:post a] [:post b]]
        steps [a-step b-step
               {:id :gesso.assurance.proof.copy-preservation/equality
                :rule :substitution
                :from [:gesso.assurance.proof.copy-preservation/right
                       :gesso.assurance.proof.copy-preservation/left]
                :side :right :direction :reverse
                :conclusion goal}]]
    (checked-derivation! transition description steps goal [a b])))

(defn- prove-frame!
  [transition description field]
  (checked-derivation!
   transition description
   [{:id :gesso.assurance.proof.copy-preservation/frame
     :rule :frame :field field :conclusion [:same-field field]}]
   [:same-field field] field))

(defn- equality-readers
  "Match only exact two-reader equality. No arithmetic, implication,
   nested Boolean rewrite, or equality with a sampled input/literal."
  [expression]
  (when (and (vector? expression)
             (= 3 (count expression))
             (= := (first expression)))
    (let [[_ a b] expression]
      (when (and (vector? a) (= 2 (count a)) (= :read (first a))
                 (vector? b) (= 2 (count b)) (= :read (first b))
                 (not= (second a) (second b)))
        [(second a) (second b)]))))

(defn- classify-clause
  [expression semantics]
  (let [readers (expression-readers expression)
        state-types (:state-types semantics)
        typed? (and (seq readers)
                    (standalone-typed? expression readers state-types))
        [a b] (when typed? (equality-readers expression))
        origin-a (when a (origin semantics a))
        origin-b (when b (origin semantics b))
        frame? (and typed? (set/subset? readers (:frame-fields semantics)))
        copy-equality? (and typed? a b (some? origin-a) (= origin-a origin-b))]
    (cond
      frame? {:kind :frame :readers readers}
      copy-equality? {:kind :copy-equality :readers readers
                      :left a :right b :common-origin origin-a}
      :else {:kind :unsupported :readers readers
             :reason (if-not typed?
                       :not-independently-well-typed
                       :no-supported-copy-equality-proof)})))

(defn- classify-invariant
  [id source-rule semantics]
  (let [expression (:expression source-rule)
        clauses (if (= :and (first expression))
                  (subvec expression 1) [expression])
        decisions (mapv (fn [index expression]
                          (assoc (classify-clause expression semantics)
                                 :index index))
                        (range) clauses)
        checked? (every? #(not= :unsupported (:kind %)) decisions)
        copy? (some #(= :copy-equality (:kind %)) decisions)]
    {:invariant id
     :obligation-id [:preservation id]
     :readers (:readers source-rule)
     :clause-judgments decisions
     :inference (cond
                  (not checked?) :not-established-by-copy-equality
                  (and (= :and (first expression)) copy?)
                  :frame-and-copy-equality-conjunction
                  copy? :shared-origin-cross-field-equality
                  :else :frame-only-preservation)
     :status (if checked? :checked-conditional-inference :open)}))

(defn check-copy-preservation!
  "From ORIGINAL v1241 source, check a restricted preservation inference for
   copy-assigned equalities (possibly conjoined with independent frame-only
   clauses). Every checked cross-field equality has its own THREE-step v1235
   proof; frame-only clauses use exact frame derivations. Returns no certificate
   or physical authority, and discharges no v1241 obligations."
  [source]
  (let [obligations (obligation/derive-obligations! source)
        transition (:transition (construction/build-guarded-replacement!
                                 (:construction source)))
        description (abstract-transition/describe-transition! transition)
        semantics (:semantics description)
        {:keys [state-types writes assignments frame-fields]} semantics
        declared (:invariants obligations)
        preservation (get-in obligations [:obligations :preservation])]
    (when-not (and (= obligation-categories (set (keys (:obligations obligations))))
                   (= (:fingerprint description) (:transition-fingerprint obligations))
                   (= writes (set (keys assignments)))
                   (= (set (keys state-types)) (set/union writes frame-fields))
                   (= (set (keys declared)) (set (map :invariant preservation)))
                   (= (count declared) (count preservation))
                   (every? #(= :open (:status %)) preservation))
      (fail! :obligation-source-contract-changed {}))
    (let [outcomes (mapv (fn [[id _]]
                           (classify-invariant id (get-in source [:invariants id])
                                               semantics))
                         declared)
          established (into #{} (map :obligation-id)
                            (filter #(= :checked-conditional-inference (:status %))
                                    outcomes))
          open (into #{} (map :obligation-id)
                     (filter #(= :open (:status %)) outcomes))
          expected (set (map :id preservation))
          frame-needed (into (sorted-set)
                             (mapcat :readers)
                             (for [outcome outcomes
                                   :when (= :checked-conditional-inference (:status outcome))
                                   clause (:clause-judgments outcome)
                                   :when (= :frame (:kind clause))]
                               clause))
          equality-needed (for [outcome outcomes
                                :when (= :checked-conditional-inference (:status outcome))
                                clause (:clause-judgments outcome)
                                :when (= :copy-equality (:kind clause))]
                            [(:invariant outcome) clause])
          frame-proofs (into (sorted-map)
                             (map (fn [field]
                                    [field (prove-frame! transition description field)]))
                             frame-needed)
          equality-proofs (into (sorted-map)
                                (map (fn [[id {:keys [index left right]}]]
                                       [[id index]
                                        (prove-copy-equality! transition description
                                                              left right)]))
                                equality-needed)]
      (when-not (and (= expected (set/union established open))
                     (empty? (set/intersection established open)))
        (fail! :preservation-obligation-coverage-loss {}))
      (let [identity
            (subject/describe-subject
             {:kind :rule :id identity-id
              :interface
              {:version copy-preservation-version
               :obligation-fingerprint (:fingerprint obligations)
               :transition-fingerprint (:fingerprint description)
               :outcomes outcomes
               :frame-derivations
               (into (sorted-map)
                     (map (fn [[field proof]]
                            [field (:derivation-fingerprint proof)])) frame-proofs)
               :equality-derivations
               (into (sorted-map)
                     (map (fn [[key proof]]
                            [key (:derivation-fingerprint proof)])) equality-proofs)}})]
        {:type report-type
         :version copy-preservation-version
         :fingerprint (:fingerprint identity)
         :obligation-fingerprint (:fingerprint obligations)
         :transition-fingerprint (:fingerprint description)
         :subject-fingerprint (:subject-fingerprint description)
         :proof-rule :same-total-origin-substitution-and-frame
         :scope :declared-abstract-state-projection-only
         :claim :all-pre-invariants-and-matching-transition-imply-post-invariant
         :preservation-outcomes outcomes
         :frame-derivations frame-proofs
         :equality-derivations equality-proofs
         :conditionally-checked-preservation-ids established
         :unsupported-preservation-ids open
         :original-open-obligation-ids (:open-obligation-ids obligations)
         :checking :original-sources-independent-equality-substitution-and-frame
         :proof-status :restricted-conditional-preservation-checked
         :checker-soundness :not-independently-proved
         :open-meta-obligations
         #{:proof-checker-substitution-and-frame-extensionality-soundness
           :certificate-admission}
         :certificate :not-issued
         :admission :not-admitted
         :runtime-enforcement :not-established
         :implementation-correspondence :not-established
         :authority :none}))))

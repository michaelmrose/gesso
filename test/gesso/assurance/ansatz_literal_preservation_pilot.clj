(ns gesso.assurance.ansatz-literal-preservation-pilot
  "Opt-in v1744: source-derived signed-integer and finite-enum invariant
   preservation for ordinary, unannotated guarded replacement catalogs.

   Supported rules: required :integer state fields with typed signed
   [:>=/:<= [:read field] [:literal n]] bounds; or required, nonnullable
   nominal enum fields with a typed [:in [:read field] [:literal #{...}]]
   invariant. Enum encoding is reused from the shared finite-enum compiler. For every
   declared operation and EVERY declared invariant, the ORIGINAL Gesso
   guarded replacement constructor must establish either a typed literal
   assignment to that field or exact framing of that field. From that
   source, generate a quantified signed-Int preservation implication and
   kernel-check it with published Ansatz 0.2.617. Unsafe literal writes
   have no proof and fail closed. No invariant is dropped on unsupported
   shapes, and the full operation x invariant product is mandatory.

   This is a conditional proof about the abstract declaration's write/frame
   semantics, not a proof of guards, interpreter/translator correctness,
   actual initialization, writer completeness or XTDB execution. It emits
   no admission, certificate, or production authority. App authors require
   no Typed Clojure or Ansatz source code. Run with optional dependencies."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.ansatz-finite-enum-partition-pilot :as enum-pilot]
            [gesso.assurance.example.single-node-proving]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as guarded]))

(def ^:private original-proof-ns 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private required-catalog-keys
  #{:id :declared-operation-ids :invariants :operations})
(def ^:private allowed-construction-keys
  #{:subject :state-types :input-types :guard :reads :writes
    :replacements :frame})

(defn- reject! [reason details]
  (throw (ex-info "Unsupported generic Ansatz preservation source"
                  {:reason reason :details details})))

(defn- check-original-proof! [obligation]
  (let [v (ns-resolve original-proof-ns 'check-obligation!)]
    (when-not v (reject! :proof-checker-not-available {}))
    (@v obligation)))

(defn- qualified? [k] (and (keyword? k) (some? (namespace k))))

(defn- catalog! [catalog]
  (let [{:keys [id declared-operation-ids invariants operations]} catalog
        ids (mapv :id operations)]
    (when-not (and (map? catalog)
                   (= required-catalog-keys (set (keys catalog)))
                   (qualified? id)
                   (set? declared-operation-ids)
                   (seq declared-operation-ids)
                   (<= (count declared-operation-ids) 12)
                   (map? invariants) (seq invariants)
                   (<= (count invariants) 12)
                   (every? qualified? (keys invariants))
                   (vector? operations)
                   (= (count declared-operation-ids) (count ids))
                   (= declared-operation-ids (set ids))
                   (every? qualified? ids))
      (reject! :incomplete-catalog-coverage
               {:catalog-id id :declared declared-operation-ids
                :operations ids :invariants (keys invariants)}))
    {:fingerprint (:fingerprint
                   (subject/describe-subject
                    {:kind :rule
                     :id :gesso.assurance.ansatz-pilot/typed-preservation-catalog
                     :interface catalog}))
     :operation-ids ids
     :invariant-ids (vec (sort (keys invariants)))}))

(defn- invariant! [invariant-id rule]
  (let [{:keys [readers expression]} rule
        [op read literal] expression
        field (when (vector? read) (second read))
        value (when (vector? literal) (second literal))
        base? (and (= #{:readers :expression} (set (keys rule)))
                   (vector? expression) (= 3 (count expression))
                   (= [:read field] read)
                   (qualified? field) (= #{field} readers)
                   (vector? literal) (= 2 (count literal))
                   (= :literal (first literal)))]
    (when-not base?
      (reject! :unsupported-invariant {:invariant-id invariant-id :rule rule}))
    (case op
      (:>= :<=)
      (if (integer? value)
        {:kind :signed-int-bound :invariant-id invariant-id
         :field field :op op :bound value :rule rule}
        (reject! :unsupported-invariant {:invariant-id invariant-id :rule rule}))
      :in
      (if (and (set? value) (seq value) (every? keyword? value))
        {:kind :finite-enum-membership :invariant-id invariant-id
         :field field :members value :rule rule}
        (reject! :unsupported-invariant {:invariant-id invariant-id :rule rule}))
      (reject! :unsupported-invariant {:invariant-id invariant-id :rule rule}))))

(defn- state-field-type! [invariant construction]
  (let [{:keys [kind field]} invariant
        state-types (:state-types construction)
        field-type (get state-types field)]
    (when-not (and (contains? state-types field)
                   (case kind
                     :signed-int-bound (= {:type :integer} field-type)
                     :finite-enum-membership
                     (and (= :enum (:type field-type))
                          (not (true? (:optional? field-type)))
                          (not (true? (:nullable? field-type))))
                     false))
      (reject! :invariant-field-type-unsupported
               {:invariant-id (:invariant-id invariant)
                :field field :actual field-type}))
    ;; An original Gesso typed invariant is mandatory even if Ansatz can
    ;; elaborate some broader/weaker surrogate expression.
    (rule-type/check-rule-type! (:rule invariant)
                                (select-keys state-types #{field}))
    (when (= kind :finite-enum-membership)
      (let [encoding (enum-pilot/checked-enum-encoding! field field-type)]
        (when-not (set/subset? (:members invariant)
                               (set (:values encoding)))
          (reject! :invariant-enum-not-in-source-domain
                   {:invariant-id (:invariant-id invariant)}))
        encoding))))

(defn- disjunction [forms]
  (when-not (seq forms) (reject! :empty-invariant-enum {}))
  (reduce (fn [a b] (list 'Or a b)) (first forms) (rest forms)))

(defn- enum-membership [codes members var]
  (disjunction (mapv (fn [member] (list '= 'Int var (get codes member)))
                     (sort-by str members))))

(defn- enum-write-script [members write-value]
  ;; The membership proposition is a left-associated disjunction. Select
  ;; the exact literal's branch, then use the source-derived write equality.
  (let [ordered (vec (sort-by str members))
        index (.indexOf ordered write-value)
        size (count ordered)]
    (when (neg? index)
      (reject! :enum-write-outside-invariant
               {:write write-value :allowed members}))
    (vec (concat (repeat (- (dec size) index) '(left))
                 (when (pos? index) ['(right)])
                 ['(exact heffect)]))))

(defn- original-transition! [operation]
  (let [{:keys [id construction]} operation]
    (when-not (and (= #{:id :construction} (set (keys operation)))
                   (qualified? id) (map? construction)
                   (= allowed-construction-keys (set (keys construction)))
                   (= id (get-in construction [:subject :id]))
                   (= :operation (get-in construction [:subject :kind]))
                   (= {:operation id}
                      (get-in construction [:subject :interface])))
      (reject! :unsupported-operation-source {:operation operation}))
    (let [built (guarded/build-guarded-replacement! construction)
          original (:transition built)
          description (abstract/describe-transition! original)
          semantics (:semantics description)]
      (when-not (and (= (:replacements construction) (:assignments original))
                     (= (:replacements construction) (:assignments semantics))
                     (= (:fingerprint description) (:transition-fingerprint built))
                     (= (:reads construction) (:reads semantics))
                     (= (:writes construction) (:writes semantics))
                     (= (:frame construction) (:frame-fields semantics)))
        (reject! :original-transition-mismatch {:operation-id id}))
      {:id id :construction construction :built built
       :source original :semantics semantics
       :transition-fingerprint (:fingerprint description)})))

(defn- comparison [op value bound]
  (list (case op :<= '<= :>= '>=) 'Int value bound))

(defn- obligation! [identity invariant operation]
  (let [{:keys [invariant-id field kind op bound members]} invariant
        {:keys [id construction semantics transition-fingerprint]} operation
        written? (contains? (:writes semantics) field)
        framed? (contains? (:frame-fields semantics) field)
        assignment (get-in semantics [:assignments field])
        exact-source (get-in construction [:replacements field])
        encoding (state-field-type! invariant construction)]
    (when-not (not= written? framed?)
      (reject! :ambiguous-invariant-field-effect
               {:operation-id id :field field}))
    (when (and written?
               (not (and (= assignment exact-source)
                         (vector? assignment)
                         (= 2 (count assignment))
                         (= :literal (first assignment))
                         (case kind
                           :signed-int-bound (integer? (second assignment))
                           :finite-enum-membership
                           (contains? (:codes encoding) (second assignment))
                           false))))
      (reject! :unsupported-invariant-write
               {:operation-id id :field field :assignment assignment}))
    ;; Normalized original semantics require either post = declared literal
    ;; or post = pre. Neither fact is invented by the proof translator.
    (let [value (when written? (second assignment))
          enum? (= kind :finite-enum-membership)
          rhs (if written?
                (if enum? (get (:codes encoding) value) value)
                'pre)
          proposition (if enum?
                        (enum-membership (:codes encoding) members 'post)
                        (comparison op 'post bound))
          prior (if enum?
                  (enum-membership (:codes encoding) members 'pre)
                  (comparison op 'pre bound))
          proof-script (cond
                         (not enum?) '[(omega)]
                         framed? '[(rw [heffect]) (exact hpre)]
                         :else (enum-write-script members value))]
      {:obligation-kind (if enum? :quantified-conditional-enum-preservation
                            :quantified-conditional-integer-preservation)
       :catalog-fingerprint (:fingerprint identity)
       :operation-id id :invariant-id invariant-id
       :transition-fingerprint transition-fingerprint
       :effect (if written? :source-literal-write :source-framed-field)
       :field field :kind kind :bound bound :comparison op
       :enum-encoding (when enum? (:codes encoding))
       :declared-members (when enum? members)
       :write-value value
       :parameters ['pre ':- 'Int
                    'post ':- 'Int
                    'hpre ':- prior
                    'heffect ':- (list '= 'Int 'post rhs)]
       :goal-form proposition
       :proof-script proof-script})))

(defn- source-obligations! [catalog]
  (let [identity (catalog! catalog)
        invariants (mapv (fn [id]
                           (invariant! id (get-in catalog [:invariants id])))
                         (:invariant-ids identity))
        operations (mapv original-transition! (:operations catalog))
        _ (doseq [inv invariants]
            (let [types (mapv #(get-in % [:construction :state-types (:field inv)])
                              operations)]
              (when-not (apply = types)
                (reject! :inconsistent-invariant-field-types
                         {:field (:field inv) :observed types}))))
        actual (vec (for [operation operations invariant invariants]
                      (obligation! identity invariant operation)))
        pairs (mapv (juxt :operation-id :invariant-id) actual)
        expected (set (for [id (:operation-ids identity)
                            inv (:invariant-ids identity)] [id inv]))]
    (when-not (and (= expected (set pairs))
                   (= (count expected) (count pairs)))
      (reject! :incomplete-proof-product {:expected expected :actual pairs}))
    {:identity identity :obligations actual}))

(defn- check-catalog! [catalog]
  (let [{:keys [identity obligations]} (source-obligations! catalog)
        checked (mapv check-original-proof! obligations)]
    (when-not (and (= (count obligations) (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (= (:fingerprint identity)
                                     (:catalog-fingerprint description))
                                  (kernel/verifies? (ansatz/env) goal proof)
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies))))
                           checked))
      (reject! :unqualified-preservation-proof
               {:required (count obligations) :observed (count checked)}))
    {:catalog-fingerprint (:fingerprint identity)
     :kernel-checked-proofs checked
     :proof-count (count checked)
     :operation-count (count (:operation-ids identity))
     :invariant-count (count (:invariant-ids identity))
     :scope :source-derived-required-int-and-enum-preservation
     :initialization :not-proved
     :general-translation-correctness :not-proved
     :physical-correspondence :not-established
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- example-catalog [field low high values]
  (let [status :queue/status
        statuses #{:ready :busy :closed}
        state-types {status {:type :enum :name :queue/status :values statuses}
                     field {:type :integer}
                     :queue/audit {:type :keyword :nullable? true}}
        transition (fn [id from to & [write]]
                     (let [written (if (some? write) #{status field} #{status})
                           replacements (cond-> {status [:literal to]}
                                          (some? write) (assoc field [:literal write]))]
                       {:id id
                        :construction
                        {:subject {:kind :operation :id id
                                   :interface {:operation id}}
                         :state-types state-types :input-types {}
                         :guard {:readers #{status}
                                 :expression [:= [:read status] [:literal from]]}
                         :reads #{status}
                         :writes written :replacements replacements
                         :frame (set/difference (set (keys state-types)) written)}}))]
    {:id :queue/literal-preservation-example
     :declared-operation-ids #{:queue/start :queue/reset :queue/close}
     :invariants {:queue/lower {:readers #{field}
                                :expression [:>= [:read field] [:literal low]]}
                  :queue/upper {:readers #{field}
                                :expression [:<= [:read field] [:literal high]]}}
     :operations [(transition :queue/start :ready :busy (first values))
                  (transition :queue/reset :busy :ready (second values))
                  (transition :queue/close :ready :closed)]}))

(defn- mixed-catalog []
  (let [base (example-catalog :queue/count 0 4 [4 0])
        status-values #{:ready :busy :closed}
        hold {:id :queue/hold
              :construction
              {:subject {:kind :operation :id :queue/hold
                         :interface {:operation :queue/hold}}
               :state-types (get-in base [:operations 0 :construction :state-types])
               :input-types {}
               :guard {:readers #{:queue/status}
                       :expression [:= [:read :queue/status] [:literal :ready]]}
               :reads #{:queue/status}
               :writes #{:queue/count}
               :replacements {:queue/count [:literal 2]}
               :frame #{:queue/status :queue/audit}}}]
    (-> base
        (update :declared-operation-ids conj :queue/hold)
        (update :operations conj hold)
        (assoc-in [:invariants :queue/status-domain]
                  {:readers #{:queue/status}
                   :expression [:in [:read :queue/status]
                                [:literal status-values]]}))))

(deftest ordinary-mixed-catalog-proves-all-integer-and-enum-invariants
  (let [catalog (mixed-catalog)
        result (check-catalog! catalog)
        proofs (:kernel-checked-proofs result)
        enum-proofs (filter #(= :finite-enum-membership
                                (get-in % [:description :kind])) proofs)]
    (is (= 4 (:operation-count result)))
    (is (= 3 (:invariant-count result)))
    (is (= 12 (:proof-count result)))
    (is (= 4 (count enum-proofs)))
    (is (= #{:source-framed-field :source-literal-write}
           (set (map #(get-in % [:description :effect]) enum-proofs))))
    (is (= #{:ready :busy :closed}
           (set (keep #(get-in % [:description :write-value]) enum-proofs))))
    (is (= 12 (count (set (map (juxt (comp :operation-id :description)
                                    (comp :invariant-id :description)) proofs)))))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) proofs))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest proper-enum-subsets-and-singletons-are-proved-not-assumed
  (let [base (mixed-catalog)
        narrowed (-> base
                     (assoc-in [:invariants :queue/status-domain :expression 2 1]
                               #{:ready :busy})
                     (assoc-in [:operations 2 :construction :replacements
                                :queue/status] [:literal :busy]))
        singleton (-> narrowed
                      (assoc-in [:invariants :queue/status-domain :expression 2 1]
                                #{:busy})
                      (assoc-in [:operations 1 :construction :replacements
                                 :queue/status] [:literal :busy]))]
    (doseq [catalog [narrowed singleton]]
      (let [result (check-catalog! catalog)]
        (is (= 12 (:proof-count result)))
        (is (= 4 (count (filter #(= :finite-enum-membership
                                   (get-in % [:description :kind]))
                                (:kernel-checked-proofs result)))))))))

(deftest reservation-catalog-uses-the-same-mixed-preservation-compiler
  (let [catalog (dissoc (gesso.assurance.example.single-node-proving/reservation-catalog)
                        :initial-condition)
        result (check-catalog! catalog)]
    (is (= 6 (:proof-count result)))
    (is (= 2 (count (filter #(= :finite-enum-membership
                                (get-in % [:description :kind]))
                            (:kernel-checked-proofs result)))))))

(deftest mixed-enum-invariant-mutations-fail-closed
  (let [source (mixed-catalog)
        reason (fn [data]
                 (try (check-catalog! data) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))
        narrower (assoc-in source [:invariants :queue/status-domain
                                   :expression 2 1] #{:ready :busy})
        malformed (assoc-in source [:invariants :queue/status-domain
                                    :expression 0] :not-in)
        type-change (assoc-in source [:operations 3 :construction :state-types
                                      :queue/status :name] :other/identity)
        bad-write (assoc-in source [:operations 0 :construction :replacements
                                    :queue/status] [:literal :absent])
        extra (assoc-in source [:invariants :queue/new]
                        {:readers #{:queue/audit}
                         :expression [:in [:read :queue/audit]
                                      [:literal #{:audit/one}]]})]
    (is (thrown? clojure.lang.ExceptionInfo (check-catalog! narrower)))
    (is (= :unsupported-invariant (reason malformed)))
    (is (= :inconsistent-invariant-field-types (reason type-change)))
    (is (thrown? clojure.lang.ExceptionInfo (check-catalog! bad-write)))
    (is (= :invariant-field-type-unsupported (reason extra)))
    (is (= :incomplete-catalog-coverage
           (reason (update source :operations pop))))))

(deftest non-reservation-catalog-proves-all-written-and-framed-bounds
  (let [catalog (example-catalog :queue/count 0 4 [4 0])
        result (check-catalog! catalog)
        proofs (:kernel-checked-proofs result)]
    (is (= 3 (:operation-count result)))
    (is (= 2 (:invariant-count result)))
    (is (= 6 (:proof-count result)))
    (is (= 6 (count (set (map (juxt (comp :operation-id :description)
                                    (comp :invariant-id :description)) proofs)))))
    (is (= 2 (count (filter #(= :source-framed-field
                                (get-in % [:description :effect])) proofs))))
    (is (= 4 (count (filter #(= :source-literal-write
                                (get-in % [:description :effect])) proofs))))
    (is (every? #(kernel/verifies? (ansatz/env) (:goal %) (:proof %)) proofs))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:authority result)))))

(deftest signed-integer-bounds-are-not-reservation-specific
  (let [result (check-catalog! (example-catalog :queue/temperature -20 30 [-5 30]))]
    (is (= 6 (:proof-count result)))
    (is (= #{-5 30}
           (set (keep (comp :write-value :description)
                      (:kernel-checked-proofs result)))))))

(deftest unsafe-and-unsupported-source-fails-closed
  (let [source (example-catalog :queue/count 0 4 [4 0])
        reason (fn [modified]
                 (try (check-catalog! modified) nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))
        absent (update source :operations pop)
        unproved (assoc-in source [:operations 0 :construction
                                   :replacements :queue/count] [:literal 5])
        unsupported (assoc-in source [:invariants :queue/upper :expression 0] :=)
        extra (assoc-in source [:invariants :queue/new]
                         {:readers #{:queue/status}
                          :expression [:not [:in [:read :queue/status]
                                                 [:literal #{:ready :busy :closed}]]]})
        changed-type (assoc-in source [:operations 0 :construction
                                       :state-types :queue/count] {:type :number})
        read-value (assoc-in source [:operations 0 :construction
                                     :replacements :queue/count]
                             [:read :queue/count])]
    (is (= :incomplete-catalog-coverage (reason absent)))
    (is (= :unsupported-invariant (reason unsupported)))
    (is (= :unsupported-invariant (reason extra)))
    (is (= :unknown-reader-type (reason changed-type)))
    (is (= :unsupported-invariant-write
           (reason (-> read-value
                       (update-in [:operations 0 :construction :reads]
                                  conj :queue/count)))))
    (is (thrown? clojure.lang.ExceptionInfo (check-catalog! unproved)))
    ;; A source change that is still safe gets new source-bound proof identity.
    (let [before (:catalog-fingerprint (check-catalog! source))
          after (:catalog-fingerprint
                 (check-catalog! (assoc-in source
                                           [:operations 0 :construction
                                            :replacements :queue/count]
                                           [:literal 3])))]
      (is (not= before after)))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-literal-preservation-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

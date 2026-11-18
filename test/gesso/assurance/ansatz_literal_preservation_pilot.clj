(ns gesso.assurance.ansatz-literal-preservation-pilot
  "Opt-in v1743: source-derived integer-bound invariant preservation for
   ordinary, unannotated Gesso guarded replacement catalogs.

   Supported rules: a required :integer state field with a single typed
   [:>= [:read field] [:literal n]] or [:<= ...] invariant. For every
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
                     :id :gesso.assurance.ansatz-pilot/integer-preservation-catalog
                     :interface catalog}))
     :operation-ids ids
     :invariant-ids (vec (sort (keys invariants)))}))

(defn- invariant! [invariant-id rule]
  (let [{:keys [readers expression]} rule
        [op read literal] expression
        field (second read)
        bound (second literal)]
    (when-not (and (= #{:readers :expression} (set (keys rule)))
                   (vector? expression) (= 3 (count expression))
                   (#{:<= :>=} op)
                   (vector? read) (= [:read field] read)
                   (qualified? field)
                   (= #{field} readers)
                   (vector? literal) (= 2 (count literal))
                   (= :literal (first literal))
                   (integer? bound))
      (reject! :unsupported-invariant
               {:invariant-id invariant-id :rule rule}))
    {:invariant-id invariant-id :field field :op op :bound bound
     :rule rule}))

(defn- state-field-type! [invariant construction]
  (let [field (:field invariant)
        state-types (:state-types construction)
        field-type (get state-types field)]
    (when-not (and (contains? state-types field)
                   (= {:type :integer} field-type))
      (reject! :invariant-field-not-required-integer
               {:invariant-id (:invariant-id invariant)
                :field field :actual field-type}))
    ;; The ORIGINAL Gesso type checker must accept the actual source rule.
    ;; A separate translator is not allowed to make a malformed rule legal.
    (rule-type/check-rule-type! (:rule invariant)
                                (select-keys state-types #{field}))))

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
  (let [{:keys [invariant-id field op bound]} invariant
        {:keys [id construction semantics transition-fingerprint]} operation
        written? (contains? (:writes semantics) field)
        framed? (contains? (:frame-fields semantics) field)
        assignment (get-in semantics [:assignments field])
        exact-source (get-in construction [:replacements field])]
    (state-field-type! invariant construction)
    (when-not (not= written? framed?)
      (reject! :ambiguous-invariant-field-effect
               {:operation-id id :field field}))
    (when (and written?
               (not (and (= assignment exact-source)
                         (vector? assignment)
                         (= 2 (count assignment))
                         (= :literal (first assignment))
                         (integer? (second assignment)))))
      (reject! :unsupported-invariant-write
               {:operation-id id :field field :assignment assignment}))
    ;; Under the ORIGINAL abstract semantics, post is either precisely the
    ;; literal write or precisely the same prestate value through framing.
    ;; We do NOT derive that rule from a separately handwritten operation.
    (let [rhs (if written? (second assignment) 'pre)
          proposition (comparison op 'post bound)]
      {:obligation-kind :quantified-conditional-integer-preservation
       :catalog-fingerprint (:fingerprint identity)
       :operation-id id :invariant-id invariant-id
       :transition-fingerprint transition-fingerprint
       :effect (if written? :source-literal-write :source-framed-field)
       :field field :bound bound :comparison op
       :write-value (when written? rhs)
       :parameters ['pre ':- 'Int
                    'post ':- 'Int
                    'hpre ':- (comparison op 'pre bound)
                    'heffect ':- (list '= 'Int 'post rhs)]
       :goal-form proposition
       :proof-script '[(omega)]})))

(defn- source-obligations! [catalog]
  (let [identity (catalog! catalog)
        invariants (mapv (fn [id]
                           (invariant! id (get-in catalog [:invariants id])))
                         (:invariant-ids identity))
        operations (mapv original-transition! (:operations catalog))
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
     :scope :source-derived-required-integer-bounds
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
                          :expression [:in [:read :queue/status]
                                       [:literal #{:ready :busy :closed}]]})
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

(ns gesso.assurance.ansatz-reservation-pilot
  "Opt-in Ansatz 0.2.617, source-derived, signed-integer preservation pilot.

   From the ORIGINAL Gesso reservation catalog, construct quantified
   conditional preservation propositions for BOTH claim-count invariants
   across EVERY declared operation. For pre/post Int states, assume the
   original invariant at pre and the original literal replacement at post;
   prove the invariant at post. The proposition and proof are checked by
   Ansatz's kernel. Neither the goals nor the operations are authored in a
   separate downstream specification.

   Scope remains conditional: this does not prove that Gesso's transition
   interpreter implements the equality premise, that guards are enabled,
   that the initial state is valid, that the translation is generally sound,
   that all invariants are covered, or that any physical/installed execution
   refines the theorem. It does not audit axioms or issue authority-bearing
   evidence. This test deliberately checks only a closed known source shape.

   Invoke using the optional dependency alias:
     clojure -M:ansatz:test -m gesso.assurance.ansatz-reservation-pilot

   The filename intentionally does not end in _test.clj: the ordinary JVM
   suite must not acquire an implicit Ansatz dependency."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.subject :as subject]))

(def ^:private claim-field :slot/active-claims)
(def ^:private claim-invariant-ids
  #{:slot/nonnegative-claims :slot/at-most-one-claim})
(def ^:private expected-invariant-ids
  (conj claim-invariant-ids :slot/status-domain))
(def ^:private supported-construction-keys
  #{:subject :state-types :input-types :guard :reads :writes
    :replacements :frame})

(defn- reject! [reason data]
  (throw (ex-info "Unsupported Ansatz reservation pilot source"
                  (assoc data :error/type ::unsupported-source :reason reason))))

(defn- integer-literal! [form at]
  (when-not (and (vector? form) (= 2 (count form))
                 (= :literal (first form))
                 (integer? (second form)))
    (reject! :expected-integer-literal {:at at :expression form}))
  (second form))

(defn- claim-invariant! [catalog invariant-id]
  (let [invariant (get-in catalog [:invariants invariant-id])
        {:keys [readers expression]} invariant]
    (when-not (and (= #{:readers :expression} (set (keys invariant)))
                   (= #{claim-field} readers)
                   (vector? expression) (= 3 (count expression))
                   (#{:<= :>=} (first expression))
                   (= [:read claim-field] (second expression)))
      (reject! :unsupported-invariant
               {:invariant-id invariant-id :invariant invariant}))
    {:invariant-id invariant-id
     :comparison (first expression)
     :bound (integer-literal! (nth expression 2)
                              [:invariants invariant-id :expression])}))

(defn- replacement! [operation]
  (let [{:keys [id construction]} operation
        {:keys [subject state-types writes replacements frame]} construction]
    ;; Do not silently ignore a new writer or effect mechanism. This is
    ;; intentionally NOT a general interpreter for Gesso transition data.
    (when-not (and (= #{:id :construction} (set (keys operation)))
                   (keyword? id)
                   (= supported-construction-keys (set (keys construction)))
                   (= :operation (:kind subject))
                   (= id (:id subject))
                   (= {:operation id} (:interface subject))
                   (= {:type :integer} (get state-types claim-field))
                   (set? writes) (contains? writes claim-field)
                   (map? replacements)
                   (= writes (set (keys replacements)))
                   (set? frame) (not (contains? frame claim-field))
                   (empty? (set/intersection writes frame)))
      (reject! :unsupported-transition {:operation-id id
                                          :construction construction}))
    (integer-literal! (get replacements claim-field)
                      [:operations id :replacements claim-field])))

(defn- comparison-form [comparison value bound]
  ;; This pilot maps Gesso's :integer to Ansatz Int, not Nat: negative
  ;; assignments must remain representable and fail the lower-bound proof.
  (list (case comparison :<= '<= :>= '>=) 'Int value bound))

(defn- source-obligations! [catalog]
  (when-not (and (map? catalog)
                 (= #{:id :declared-operation-ids :initial-condition
                      :invariants :operations}
                    (set (keys catalog)))
                 (= :slot/reservation-lifecycle (:id catalog))
                 (vector? (:operations catalog))
                 (seq (:operations catalog)))
    (reject! :unsupported-catalog {:catalog-id (:id catalog)}))
  (let [ids (mapv :id (:operations catalog))
        declared (:declared-operation-ids catalog)
        invariants (:invariants catalog)]
    (when-not (and (set? declared)
                   (= (count ids) (count (set ids)))
                   (= declared (set ids)))
      (reject! :incomplete-operation-catalog
               {:declared declared :observed ids}))
    ;; Keep the known catalog CLOSED for this pilot. An added/removed
    ;; invariant must be reviewed rather than silently inheriting coverage.
    (when-not (= expected-invariant-ids (set (keys invariants)))
      (reject! :changed-invariant-catalog
               {:expected expected-invariant-ids
                :observed (set (keys invariants))})))
  (let [invariants (mapv (partial claim-invariant! catalog)
                         (sort claim-invariant-ids))]
    (vec
     (for [operation (:operations catalog)
           {:keys [invariant-id comparison bound]} invariants]
       (let [replacement (replacement! operation)
             id (:id operation)
             source-invariant (get-in catalog [:invariants invariant-id])
             exact-subject
             {:kind :operation
              :id id
              :interface
              {:catalog-id (:id catalog)
               :construction (:construction operation)
               :invariant-id invariant-id
               :invariant source-invariant}}]
         {:operation-id id
          :invariant-id invariant-id
          :source-fingerprint
          (:fingerprint (subject/describe-subject exact-subject))
          :replacement replacement
          :comparison comparison
          :bound bound
          ;; The proof is universally quantified over signed pre/post
          ;; values. We assume ONLY a pre-invariant and the literal-write
          ;; equality, then prove the post-invariant.
          :parameters
          ['pre ':- 'Int
           'post ':- 'Int
           'hpre ':- (comparison-form comparison 'pre bound)
           'hwrite ':- (list '= 'Int 'post replacement)]
          :goal-form (comparison-form comparison 'post bound)})))))

(defn- check-obligation! [{:keys [parameters goal-form] :as obligation}]
  (let [[goal proof] (binding [ansatz/*verbose* false]
                       (ansatz/prove-law parameters goal-form '[(omega)]))]
    (when-not (kernel/verifies? (ansatz/env) goal proof)
      (reject! :kernel-rejected
               {:operation-id (:operation-id obligation)
                :invariant-id (:invariant-id obligation)}))
    {:description (dissoc obligation :parameters :goal-form)
     :goal goal
     :proof proof}))

(defn- check-catalog! [catalog]
  (mapv check-obligation! (source-obligations! catalog)))

(deftest original-catalog-generates-quantified-preservation-proofs
  (let [catalog (example/reservation-catalog)
        results (check-catalog! catalog)
        subjects (set (map (juxt (comp :operation-id :description)
                                 (comp :invariant-id :description)) results))]
    (is (= #{[:slot/reserve :slot/at-most-one-claim]
             [:slot/reserve :slot/nonnegative-claims]
             [:slot/release :slot/at-most-one-claim]
             [:slot/release :slot/nonnegative-claims]}
           subjects))
    (is (= #{[1 :<= 1] [1 :>= 0] [0 :<= 1] [0 :>= 0]}
           (set (map (juxt (comp :replacement :description)
                           (comp :comparison :description)
                           (comp :bound :description)) results))))
    (is (every? (fn [{:keys [goal proof]}]
                  (kernel/verifies? (ansatz/env) goal proof)) results))
    (testing "an upper-bound proof is not a lower-bound proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (second results))
                                   (:proof (first results))))))
    (testing "an operation's proof is not another operation's proof"
      (is (false? (kernel/verifies? (ansatz/env)
                                   (:goal (first results))
                                   (:proof (nth results 2))))))
    (is (false? (kernel/verifies? (ansatz/env)
                                  (:goal (first results)) nil)))))

(defn- proof-fails? [obligation]
  ;; A malformed source is not evidence that Ansatz refuted a proposition.
  ;; Require successful source translation and an actual proof failure.
  (try (check-obligation! obligation) false
       (catch clojure.lang.ExceptionInfo ex
         (when (= ::unsupported-source (:error/type (ex-data ex)))
           (throw ex))
         true)))

(deftest mutated-source-cannot-silently-inherit-proofs
  (let [source (example/reservation-catalog)
        reserve-index (first (keep-indexed
                              (fn [i op] (when (= :slot/reserve (:id op)) i))
                              (:operations source)))
        replacement-path [:operations reserve-index :construction
                          :replacements claim-field]
        mutated-obligation
        (fn [replacement invariant-id]
          (let [source' (assoc-in source replacement-path
                                  [:literal replacement])]
            (first (filter #(and (= :slot/reserve (:operation-id %))
                                 (= invariant-id (:invariant-id %)))
                           (source-obligations! source')))))]
    (testing "a poststate of two translates but fails the original upper bound"
      (let [obligation (mutated-obligation 2 :slot/at-most-one-claim)]
        (is (= 2 (:replacement obligation)))
        (is (proof-fails? obligation))))
    (testing "a negative Int poststate translates but fails the lower bound"
      (let [obligation (mutated-obligation -1 :slot/nonnegative-claims)]
        (is (= -1 (:replacement obligation)))
        (is (proof-fails? obligation))))
    (testing "arbitrary expressions cannot masquerade as literal writes"
      (is (= :expected-integer-literal
             (-> (try (source-obligations!
                       (assoc-in source replacement-path [:read claim-field]))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "an omitted operation cannot silently be proved"
      (is (= :incomplete-operation-catalog
             (-> (try (source-obligations! (update source :operations pop))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "removed invariants cannot silently inherit coverage"
      (is (= :changed-invariant-catalog
             (-> (try (source-obligations!
                       (update source :invariants dissoc
                               :slot/nonnegative-claims))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "unsupported effects fail closed"
      (is (= :unsupported-transition
             (-> (try (source-obligations!
                       (assoc-in source [:operations reserve-index
                                         :construction :unchecked-effect] :unknown))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "a same-type guard edit changes subject identity"
      (let [mutant (assoc-in source [:operations reserve-index :construction
                                    :guard :expression] [:literal true])
            before (source-obligations! source)
            after (source-obligations! mutant)]
        (is (not= (:source-fingerprint (first before))
                  (:source-fingerprint (first after))))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false]
    (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-reservation-pilot)]
    (when (pos? (+ fail error))
      (System/exit 1))))

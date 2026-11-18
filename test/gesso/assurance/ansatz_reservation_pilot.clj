(ns gesso.assurance.ansatz-reservation-pilot
  "Opt-in Ansatz 0.2.617 feasibility gate for an ORIGINAL Gesso catalog.

   Proves literal poststate claim bounds derived from the unmodified
   reservation-catalog source; it does not substitute an independently
   handwritten example specification. Checks each proof term against the
   exact elaborated goal using Ansatz's strict kernel entry point.

   Scope is deliberately narrow: proving a replacement literal is <= the
   declared bound is one conditional ingredient of preservation. This gate
   does NOT prove the full Gesso transition interpreter, invariant initiation,
   guard/authority, original-to-Ansatz translation fidelity, physical XTDB
   refinement, application writer closure, or runtime admission. Returned
   descriptions are ordinary, forgeable data and confer NO authority.

   Invoke with the optional dependency alias:
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
(def ^:private invariant-id :slot/at-most-one-claim)
(def ^:private supported-construction-keys
  #{:subject :state-types :input-types :guard :reads :writes
    :replacements :frame})

(defn- reject! [reason data]
  (throw (ex-info "Unsupported Ansatz reservation pilot source"
                  (assoc data :error/type ::unsupported-source :reason reason))))

(defn- natural-literal! [form at]
  (when-not (and (vector? form) (= 2 (count form))
                 (= :literal (first form))
                 (integer? (second form))
                 (not (neg? (second form))))
    (reject! :expected-natural-literal {:at at :expression form}))
  (second form))

(defn- invariant-bound! [catalog]
  (let [invariant (get-in catalog [:invariants invariant-id])
        expr (:expression invariant)]
    (when-not (and (= #{:readers :expression} (set (keys invariant)))
                   (= #{claim-field} (:readers invariant))
                   (vector? expr) (= 3 (count expr))
                   (= :<= (first expr))
                   (= [:read claim-field] (second expr)))
      (reject! :unsupported-invariant {:invariant-id invariant-id
                                        :invariant invariant}))
    (natural-literal! (nth expr 2) :invariant-bound)))

(defn- replacement! [operation]
  (let [{:keys [id construction]} operation
        {:keys [subject state-types writes replacements frame]} construction]
    ;; Never ignore a future effect mechanism that would invalidate the
    ;; literal-poststate inference. This is NOT a general transition translator.
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
    (natural-literal! (get replacements claim-field)
                      [:operations id :replacements claim-field])))

(defn- source-obligations! [catalog]
  (when-not (and (map? catalog)
                 (= :slot/reservation-lifecycle (:id catalog))
                 (vector? (:operations catalog))
                 (seq (:operations catalog)))
    (reject! :unsupported-catalog {:catalog-id (:id catalog)}))
  (let [ids (mapv :id (:operations catalog))
        declared (:declared-operation-ids catalog)]
    (when-not (and (set? declared)
                   (= (count ids) (count (set ids)))
                   (= declared (set ids)))
      (reject! :incomplete-operation-catalog
               {:declared declared :observed ids})))
  (let [bound (invariant-bound! catalog)]
    (mapv (fn [operation]
            (let [replacement (replacement! operation)
                  id (:id operation)
                  exact-subject
                  {:kind :operation
                   :id id
                   :interface
                   {:catalog-id (:id catalog)
                    :construction (:construction operation)
                    :invariant-id invariant-id
                    :invariant (get-in catalog [:invariants invariant-id])}}]
              {:operation-id id
               :invariant-id invariant-id
               :source-fingerprint
               (:fingerprint (subject/describe-subject exact-subject))
               :replacement replacement
               :upper-bound bound
               ;; No handwritten proof-goal: it is assembled exclusively
               ;; from the source replacement and original invariant.
               :goal-form (list '<= replacement bound)}))
          (:operations catalog))))

(defn- check-obligation! [{:keys [goal-form] :as obligation}]
  (let [[goal proof] (binding [ansatz/*verbose* false]
                       (ansatz/prove-law [] goal-form '[(omega)]))]
    (when-not (kernel/verifies? (ansatz/env) goal proof)
      (reject! :kernel-rejected {:operation-id (:operation-id obligation)}))
    {:description (dissoc obligation :goal-form)
     :goal goal
     :proof proof}))

(defn- check-catalog! [catalog]
  (mapv check-obligation! (source-obligations! catalog)))

(deftest original-reservation-declarations-generate-two-real-proofs
  (let [source (example/reservation-catalog)
        results (check-catalog! source)]
    (is (= #{:slot/reserve :slot/release}
           (set (map (comp :operation-id :description) results))))
    (is (= #{[1 1] [0 1]}
           (set (map (juxt (comp :replacement :description)
                           (comp :upper-bound :description)) results))))
    (is (every? (fn [{:keys [goal proof]}]
                  (kernel/verifies? (ansatz/env) goal proof)) results))
    ;; A valid proof of one proposition is not proof of the other.
    (is (false? (kernel/verifies? (ansatz/env)
                                  (:goal (first results))
                                  (:proof (second results)))))
    (is (false? (kernel/verifies? (ansatz/env)
                                  (:goal (first results)) nil)))))

(deftest source-mutants-cannot-silently-inherit-the-proofs
  (let [source (example/reservation-catalog)
        reserve-index (first (keep-indexed
                              (fn [i op] (when (= :slot/reserve (:id op)) i))
                              (:operations source)))
        replacement-path [:operations reserve-index :construction
                          :replacements claim-field]]
    (testing "the replacement value is part of the proposition"
      (let [failure (try (check-catalog!
                          (assoc-in source replacement-path [:literal 2]))
                         nil
                         (catch clojure.lang.ExceptionInfo ex ex))]
        (is (instance? clojure.lang.ExceptionInfo failure))
        (is (re-find #"omega: omega failed"
                     (or (some-> failure ex-message) "")))))
    (testing "nonliteral assignments are outside the supported translator"
      (is (= :expected-natural-literal
             (-> (try (check-catalog! (assoc-in source replacement-path
                                                 [:read claim-field]))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "an omitted operation cannot be silently proved"
      (is (= :incomplete-operation-catalog
             (-> (try (check-catalog! (update source :operations pop))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "unrecognized effect mechanisms fail closed"
      (is (= :unsupported-transition
             (-> (try (check-catalog!
                       (assoc-in source [:operations reserve-index
                                         :construction :unchecked-effect] :unknown))
                      (catch clojure.lang.ExceptionInfo ex (ex-data ex)))
                 :reason))))
    (testing "a same-type guard edit changes the original subject identity"
      (let [mutant (assoc-in source [:operations reserve-index :construction
                                    :guard :expression]
                             [:literal true])
            before (source-obligations! source)
            after (source-obligations! mutant)]
        (is (not= (:source-fingerprint (nth before reserve-index))
                  (:source-fingerprint (nth after reserve-index))))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false]
    (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests 'gesso.assurance.ansatz-reservation-pilot)]
    (when (pos? (+ fail error))
      (System/exit 1))))

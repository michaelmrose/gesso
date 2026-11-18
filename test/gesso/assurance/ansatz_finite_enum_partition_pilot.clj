(ns gesso.assurance.ansatz-finite-enum-partition-pilot
  "Opt-in v1741: reusable finite-enum equality-guard proof compiler.

   Accepts ordinary, unannotated Gesso rule declarations with precisely one
   shared, required, nonnullable, nominal enum reader. The reader field,
   nominal descriptor, equality ASTs, and complete set of literal targets
   are checked with Gesso's ORIGINAL type checker. The enum itself provides
   the finite closed domain; its members are encoded injectively into Int.

   Ansatz checks a domain-coverage implication and EVERY pairwise
   guard-exclusion theorem. The actual Gesso evaluator is independently
   exercised on EVERY allowed member/guard combination. This is a narrow,
   reusable expression subset, not arbitrary application verification.

   This pilot does not prove a general Clojure-to-Ansatz translation, JVM
   evaluator correctness, runtime authorization, or physical XTDB behavior.
   It emits no admission/certificate/authority, and requires no downstream
   Typed Clojure or Ansatz annotations. Use the optional :ansatz:typed:test aliases."
  (:require [ansatz.core :as ansatz]
            [ansatz.kernel.env :as kernel]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-observation-gate-pilot]
            [gesso.assurance.ansatz-reservation-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-type :as rule-type]
            [gesso.assurance.subject :as subject]))

(def ^:private reservation-pilot 'gesso.assurance.ansatz-reservation-pilot)
(def ^:private gate-pilot 'gesso.assurance.ansatz-observation-gate-pilot)

(defn- pilot! [ns-sym name & args]
  (let [var (ns-resolve ns-sym name)]
    (when-not var
      (throw (ex-info "Expected checked Gesso pilot function not found"
                      {:reason :pilot-contract-changed :function name})))
    (apply @var args)))

(defn- reject! [reason data]
  (throw (ex-info "Unsupported finite-enum Ansatz guard portfolio"
                  {:error/type ::unsupported-guard-portfolio
                   :reason reason :data data})))

(defn- enum-domain! [sources]
  (when-not (and (vector? sources) (<= 2 (count sources) 8)
                 (every? map? sources))
    (reject! :invalid-portfolio-size {:count (count sources)}))
  (let [first-source (first sources)
        field (first (get-in first-source [:rule :readers]))
        desc (get-in first-source [:reader-types field])
        values (:values desc)]
    (when-not (and (keyword? field) (namespace field)
                   (= #{field} (get-in first-source [:rule :readers]))
                   (= #{field} (set (keys (:reader-types first-source))))
                   (= :enum (:type desc))
                   (keyword? (:name desc)) (namespace (:name desc))
                   (not (true? (:optional? desc)))
                   (not (true? (:nullable? desc)))
                   (set? values) (= (count sources) (count values)))
      (reject! :unsupported-enum-reader {:field field :descriptor desc}))
    {:field field :descriptor desc :values (vec (sort-by str values))
     :codes (zipmap (sort-by str values) (range))}))

(defn- source-guard! [{:keys [field descriptor codes]} source]
  (let [{:keys [id rule reader-types]} source
        expression (:expression rule)
        checked (rule-type/check-rule-type! rule reader-types)
        target (cond
                 (and (vector? expression) (= 3 (count expression))
                      (= := (first expression))
                      (= [:read field] (second expression))
                      (vector? (nth expression 2))
                      (= :literal (first (nth expression 2)))
                      (= 2 (count (nth expression 2))))
                 (second (nth expression 2))

                 (and (vector? expression) (= 3 (count expression))
                      (= := (first expression))
                      (= [:read field] (nth expression 2))
                      (vector? (second expression))
                      (= :literal (first (second expression)))
                      (= 2 (count (second expression))))
                 (second (second expression))
                 :else (reject! :unsupported-equality-ast {:id id :ast expression}))]
    (when-not (and (= #{:id :rule :reader-types} (set (keys source)))
                   (keyword? id) (namespace id)
                   (= #{:readers :expression} (set (keys rule)))
                   (= #{field} (:readers checked))
                   (= {field descriptor} reader-types)
                   (contains? codes target))
      (reject! :source-guard-mismatch {:source source :target target}))
    {:id id :source source :target target :code (get codes target)
     :formula (list '= 'Int 'status (get codes target))}))

(defn- disjunction [forms]
  (when-not (seq forms) (reject! :empty-domain {}))
  (reduce (fn [a b] (list 'Or a b)) (first forms) (rest forms)))

(defn- derive-portfolio! [sources]
  ;; Source closure is checked before any synthetic theorem is trusted. This
  ;; still does NOT prove the Gesso JVM evaluator implements its source.
  (pilot! gate-pilot 'current-rule-source-closure!)
  (let [{:keys [values codes] :as domain} (enum-domain! sources)
        parsed (mapv (partial source-guard! domain) sources)
        targets (mapv :target parsed)
        ids (mapv :id parsed)
        identity (:fingerprint (subject/describe-subject
                                {:kind :rule
                                 :id :gesso.assurance.ansatz-pilot/finite-enum-guards
                                 :interface {:source-guards sources}}))
        ;; Normalize the domain proposition in SOURCE guard order. The
        ;; source-side bijection check establishes this is exactly the enum
        ;; domain, independent of operation order. The kernel then checks
        ;; the implication for that exact proposition.
        membership (disjunction (mapv :formula parsed))]
    (when-not (and (= (count ids) (count (set ids)))
                   (= (set values) (set targets))
                   (= (count values) (count (set targets))))
      (reject! :incomplete-or-aliased-guard-portfolio
               {:ids ids :targets targets :domain values}))
    {:source-fingerprint identity :domain domain :guards parsed
     :domain-form membership
     :guard-disjunction (disjunction (mapv :formula parsed))}))

(defn- source-propositions! [{:keys [source-fingerprint guards
                                   domain-form guard-disjunction]}]
  (let [coverage {:obligation-kind :complete-enum-guard-coverage
                  :source-fingerprint source-fingerprint
                  :parameters ['status ':- 'Int 'hdomain ':- domain-form]
                  :goal-form guard-disjunction
                  ;; Complete bijective coverage is SOURCE-checked; this
                  ;; kernel step is a tautological implication, NOT an
                  ;; independent formal proof of source completeness.
                  :proof-script '[(exact hdomain)]}
        exclusions (vec (for [i (range (count guards))
                              j (range (inc i) (count guards))]
                          {:obligation-kind :pairwise-enum-guard-exclusion
                           :source-fingerprint source-fingerprint
                           :guard-ids [(:id (nth guards i))
                                       (:id (nth guards j))]
                           :parameters ['status ':- 'Int]
                           :goal-form (list 'Not
                                            (list 'And (:formula (nth guards i))
                                                  (:formula (nth guards j))))
                           :proof-script '[(omega)]}))]
    (into [coverage] exclusions)))

(defn- check-portfolio! [sources]
  (let [model (derive-portfolio! sources)
        propositions (source-propositions! model)
        checked (mapv #(pilot! reservation-pilot 'check-obligation! %)
                      propositions)]
    (when-not (and (= (inc (quot (* (count sources) (dec (count sources))) 2))
                      (count checked))
                   (every? (fn [{:keys [goal proof dependencies description]}]
                             (and (kernel/verifies? (ansatz/env) goal proof)
                                  (= (:source-fingerprint model)
                                     (:source-fingerprint description))
                                  (= :ansatz-reservation-pilot/lean-core-axioms-v1
                                     (:policy dependencies))
                                  (empty? (:non-safe dependencies))
                                  (empty? (:unresolved dependencies))))
                           checked))
      (reject! :unqualified-ansatz-portfolio {:checked (count checked)}))
    {:model model :checked checked :proposition-count (count checked)
     :proof-status :kernel-checked-conditional-finite-enum-partition
     :evaluator-correspondence :exhaustive-finite-domain-observations-only
     :translation-correspondence :not-universally-proved
     :physical-correspondence :not-established
     :certificate :not-issued :admission :not-admitted :authority :none}))

(defn- observe-all! [{:keys [domain guards]}]
  (let [{:keys [field values codes]} domain
        observations (vec
                      (for [member values
                            {:keys [id source target]} guards]
                        (let [result (rule-eval/evaluate-rule!
                                      (:rule source) (:reader-types source)
                                      {field member})
                              expected (= (get codes target) (get codes member))]
                          (when-not (and (= :syntax-and-types (:checking result))
                                         (= :pure-interpreted (:evaluation result))
                                         (= :none (:authority result))
                                         (= (:expression (:rule source))
                                            (:expression result))
                                         (instance? Boolean (:value result))
                                         (= expected (:value result)))
                            (reject! :typed-evaluator-disagrees
                                     {:member member :guard id :report result}))
                          {:member member :guard id :matches? expected})))]
    (when-not (every? (fn [member]
                        (= 1 (count (filter :matches?
                                            (filter #(= member (:member %))
                                                    observations)))))
                      values)
      (reject! :non-partition-runtime-observations observations))
    observations))

(defn- source-rules [field enum-name members]
  (let [desc {:type :enum :name enum-name :values (set members)}]
    (vec (for [member members]
           {:id (keyword "example.guard" (name member))
            :rule {:readers #{field}
                   :expression [:= [:read field] [:literal member]]}
            :reader-types {field desc}}))))

(defn- reservation-rules []
  (let [catalog (example/reservation-catalog)]
    (mapv (fn [{:keys [id construction]}]
            {:id id
             :rule (:guard construction)
             :reader-types (select-keys (:state-types construction)
                                        (:readers (:guard construction)))})
          (:operations catalog))))

(deftest original-reservation-and-larger-ordinary-gesso-enums
  (doseq [[label sources]
          [[:original-reservation (reservation-rules)]
           [:three-member (source-rules :queue/phase :queue/phase
                                        [:queue/open :queue/held :queue/closed])]
           [:four-member (source-rules :task/state :task/state
                                       [:task/new :task/claimed :task/done
                                        :task/cancelled])]]]
    (testing (str label)
      (let [result (check-portfolio! sources)
            reports (observe-all! (:model result))
            n (count sources)]
        (is (= (inc (quot (* n (dec n)) 2)) (:proposition-count result)))
        (is (= (* n n) (count reports)))
        (is (= n (count (filter :matches? reports))))
        (is (= :kernel-checked-conditional-finite-enum-partition
               (:proof-status result)))
        (is (= :not-issued (:certificate result)))
        (is (= :none (:authority result)))))))

(deftest source-mutations-and-unhandled-expressions-fail-closed
  (let [sources (source-rules :task/state :task/state
                              [:task/new :task/claimed :task/done])
        reason (fn [ss] (try (derive-portfolio! ss) nil
                             (catch clojure.lang.ExceptionInfo e
                               (:reason (ex-data e)))))
        duplicate (assoc-in sources [1 :rule :expression]
                            (get-in sources [0 :rule :expression]))]
    (is (= :incomplete-or-aliased-guard-portfolio (reason duplicate)))
    (is (= :unsupported-enum-reader (reason (pop sources))))
    (is (= :unsupported-equality-ast
           (reason (assoc-in sources [1 :rule :expression]
                             [:not= [:read :task/state] [:literal :task/claimed]]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (derive-portfolio!
                  (assoc-in sources [1 :rule :expression]
                            [:= [:literal :task/claimed] [:literal :task/new]]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (derive-portfolio!
                  (assoc-in sources [1 :reader-types :task/state :name]
                            :different/nominal-identity))))
    (is (= :unsupported-enum-reader
           (reason (assoc-in sources [0 :reader-types :task/state :nullable?]
                             true))))))

(deftest source-order-and-symmetric-equality-stay-source-bound
  (let [original (reservation-rules)
        reordered (vec (reverse original))
        swapped (assoc-in original [0 :rule :expression]
                          [:= [:literal :available] [:read :slot/status]])
        base (derive-portfolio! original)
        reverse-model (derive-portfolio! reordered)
        swapped-model (derive-portfolio! swapped)]
    (is (not= (:source-fingerprint base)
              (:source-fingerprint reverse-model)))
    (is (not= (:source-fingerprint base)
              (:source-fingerprint swapped-model)))
    (is (= 4 (count (observe-all! reverse-model))))
    (is (= 4 (count (observe-all! swapped-model))))
    ;; Coverage is always generated from the checked source order, rather
    ;; than silently assuming alphabetical or reservation-specific order.
    (is (= (:domain-form reverse-model)
           (:guard-disjunction reverse-model)))
    (is (= (:domain-form swapped-model)
           (:guard-disjunction swapped-model)))))

(deftest typed-evaluator-report-disagreement-is-not-a-proof
  (let [model (:model (check-portfolio!
                        (source-rules :task/state :task/state
                                      [:task/new :task/claimed :task/done])))
        real-evaluator rule-eval/evaluate-rule!]
    (is (= 9 (count (observe-all! model))))
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [& args] (assoc (apply real-evaluator args) :value true))]
      (is (= :typed-evaluator-disagrees
             (try (observe-all! model) nil
                  (catch clojure.lang.ExceptionInfo e
                    (:reason (ex-data e)))))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-finite-enum-partition-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

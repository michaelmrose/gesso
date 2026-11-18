(ns gesso.assurance.ansatz-mixed-observation-gate-pilot
  "Opt-in v1748: per-invocation checked abstract-observer correspondence for
   Gesso mixed integer/enum invariant catalogs.

   Kernel-check the source-derived v1747 mixed abstract-to-trace bridge, then
   build a TEST-ONLY observation boundary over the original typed transition
   declarations. For each invocation, independently validate the closed
   state projection; compute the exact source guard, literal assignments,
   and *all* framed state fields (including fields not tracked by invariants);
   check the original typed rule evaluator and BOTH Gesso abstract observers
   against those primitive facts; and require every accepted observation to
   satisfy the EXACT step proposition from the induction theorem.

   Scope is intentionally closed: no operation inputs, no optional state
   fields, a one-reader enum equality guard, literal assignments, required
   integer/enum fields and required nullable/nonnullable keyword fields.
   Unsupported constructions fail closed before issuing an observation.

   This is an operational condition for calls THROUGH THIS TEST BOUNDARY,
   NOT a universal proof that Clojure/JVM implements the symbolic interpreter.
   The source and rule evaluator are attested, not formally verified; the
   finite-enum encoding and actual XTDB/write correspondence remain open.
   Reports are copyable, nonauthoritative, and do not admit applications."
  (:require [ansatz.core :as ansatz]
            [clojure.set :as set]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-mixed-transition-bridge-pilot]
            [gesso.assurance.ansatz-literal-preservation-pilot]
            [gesso.assurance.ansatz-mixed-initialization-pilot]
            [gesso.assurance.ansatz-observation-gate-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.rule-eval :as rule-eval]))

(def ^:private bridge-ns 'gesso.assurance.ansatz-mixed-transition-bridge-pilot)
(def ^:private preservation-ns 'gesso.assurance.ansatz-literal-preservation-pilot)
(def ^:private initialization-ns 'gesso.assurance.ansatz-mixed-initialization-pilot)
(def ^:private original-gate-ns 'gesso.assurance.ansatz-observation-gate-pilot)
(def ^:private report-flags
  [:guard-holds? :assignments-hold? :frame-holds? :matches?])

(defn- fail! [reason details]
  (throw (ex-info "Unqualified mixed abstract observer correspondence"
                  {:reason reason :details details})))

(defn- pilot! [namespace function & args]
  (let [v (ns-resolve namespace function)]
    (when-not v
      (fail! :missing-pilot-contract {:namespace namespace :function function}))
    (apply @v args)))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- source-typed-value? [descriptor value]
  (let [{:keys [type nullable? optional?]} descriptor]
    (and (not (true? optional?))
         (if (nil? value)
           (true? nullable?)
           (case type
             :integer (integer? value)
             :enum (and (keyword? value) (contains? (:values descriptor) value))
             :keyword (keyword? value)
             false)))))

(defn- closed-projection! [state-types label state]
  ;; The pilot deliberately requires every state field to be present. A
  ;; general Gesso optional-field or input-aware interpreter is future work.
  (when-not (and (plain-map? state)
                 (nil? (meta state))
                 (= (set (keys state-types)) (set (keys state)))
                 (every? (fn [[field desc]]
                           (source-typed-value? desc (get state field)))
                         state-types))
    (fail! :invalid-closed-state {:side label :state state}))
  state)

(defn- checked-guard! [transition]
  (let [{:keys [state-types input-types guard assignments]} transition
        expression (:expression guard)
        [operator lhs rhs] expression
        read-form? (fn [x] (and (vector? x) (= 2 (count x))
                                (= :read (first x)) (keyword? (second x))))
        literal-form? (fn [x] (and (vector? x) (= 2 (count x))
                                   (= :literal (first x))))
        [field target] (cond
                         (and (read-form? lhs) (literal-form? rhs))
                         [(second lhs) (second rhs)]
                         (and (read-form? rhs) (literal-form? lhs))
                         [(second rhs) (second lhs)]
                         :else [nil nil])
        desc (get state-types field)]
    (when-not (and (= {} input-types)
                   (map? state-types) (seq state-types)
                   (every? (fn [[_ d]]
                             (and (not (true? (:optional? d)))
                                  (#{:integer :enum :keyword} (:type d))
                                  (or (= :keyword (:type d))
                                      (not (true? (:nullable? d))))
                                  (or (not= :enum (:type d))
                                      (and (keyword? (:name d))
                                           (set? (:values d))
                                           (every? keyword? (:values d))))))
                           state-types)
                   (vector? expression) (= 3 (count expression))
                   (= := operator)
                   (= #{field} (:readers guard))
                   (= :enum (:type desc))
                   (not (true? (:nullable? desc)))
                   (contains? (:values desc) target)
                   (every? (fn [[_ rhs]]
                             (and (vector? rhs)
                                  (= 2 (count rhs))
                                  (= :literal (first rhs))))
                           assignments))
      (fail! :unsupported-original-primitive-language
             {:guard guard :state-types state-types
              :input-types input-types :assignments assignments}))
    {:field field :target target}))

(defn- source-primitives! [transition guard before after]
  (let [state-types (:state-types transition)
        {:keys [field target]} guard
        _ (closed-projection! state-types :before before)
        _ (closed-projection! state-types :after after)
        guard-holds? (= (get before field) target)
        assignment-holds?
        (every? (fn [[id [_ value]]]
                  (= (get after id) value))
                (:assignments transition))
        frame-fields (set/difference
                      (set (keys state-types)) (set (keys (:assignments transition))))
        frame-holds?
        (every? (fn [id] (= (get before id) (get after id))) frame-fields)]
    {:guard-holds? (boolean guard-holds?)
     :assignments-hold? (boolean assignment-holds?)
     :frame-holds? (boolean frame-holds?)
     :matches? (boolean (and guard-holds? assignment-holds? frame-holds?))}))

(defn- checked-source-operations! [catalog model]
  (let [sources (mapv #(pilot! preservation-ns 'original-transition! %)
                      (:operations catalog))
        expected (mapv (juxt :operation-id :transition-fingerprint)
                       (:moves model))
        actual (mapv (juxt :id :transition-fingerprint) sources)
        guards (mapv #(checked-guard! (:source %)) sources)]
    (when-not (and (= expected actual)
                   (= (count sources) (count (:declared-operation-ids catalog)))
                   (= (set (:declared-operation-ids catalog))
                      (set (map :id sources))))
      (fail! :operation-transition-identity-mismatch
             {:expected expected :actual actual}))
    (into {} (map (fn [src guard] [(:id src) (assoc src :guard-spec guard)])
                  sources guards))))

(defn- require-observer-report! [source before after primary other]
  (let [transition (:source source)
        expected (source-primitives! transition (:guard-spec source) before after)
        flags (select-keys primary report-flags)
        other-flags (select-keys other report-flags)
        guard-rule (:guard transition)
        guard-types (select-keys (:state-types transition) (:readers guard-rule))
        guard-values (select-keys before (:readers guard-rule))
        evaluated (rule-eval/evaluate-rule! guard-rule guard-types guard-values)]
    (when-not (and (= :syntax-and-types (:checking evaluated))
                   (= :pure-interpreted (:evaluation evaluated))
                   (= :none (:authority evaluated))
                   (instance? Boolean (:value evaluated))
                   (= (:expression guard-rule) (:expression evaluated))
                   (= (:guard-holds? expected) (:value evaluated)))
      (fail! :original-typed-guard-disagreement
             {:operation (:id source) :observed evaluated
              :source-guard (:guard-holds? expected)}))
    (when-not (and (= abstract/observation-type (:type primary))
                   (= :abstract-relation-observed (:checking primary))
                   (= :none (:authority primary))
                   (= (:transition-fingerprint source)
                      (:transition-fingerprint primary))
                   (= independent/report-type (:type other))
                   (= :independent-typed-guarded-relation (:checking other))
                   (= :none (:authority other))
                   (= transition (:source-declaration other))
                   (= before (:before other))
                   (= after (:after other))
                   (= {} (:inputs other))
                   (every? boolean? (vals flags))
                   (every? boolean? (vals other-flags))
                   (= (set report-flags) (set (keys flags))
                      (set (keys other-flags)))
                   (= flags other-flags))
      (fail! :observer-report-disagreement
             {:operation (:id source) :primary flags :other other-flags}))
    (when-not (= expected flags other-flags)
      (fail! :source-primitives-disagree-with-observers
             {:operation (:id source) :source expected
              :primary flags :other other-flags}))
    expected))

(defn- audited-boundary! [catalog]
  ;; Kernel checks + source attestation occur BEFORE the observer closure can
  ;; be used. Unlike an authority token, the closure is test-local only.
  (let [source-attestation (pilot! original-gate-ns 'current-interpreter-source!)
        evaluator-attestation (pilot! original-gate-ns 'current-rule-source-closure!)
        bridge-report (pilot! bridge-ns 'check-bridge! catalog)
        {:keys [model]} (pilot! bridge-ns 'bridge-obligations! catalog)
        sources (checked-source-operations! catalog model)
        current-id (get-in model [:source :catalog-fingerprint])]
    (when-not (and (= :original-abstract-observer-source-forms-v1
                      (:policy source-attestation))
                   (= :original-rule-syntax-types-evaluator-source-v1
                      (:policy evaluator-attestation))
                   (= current-id (:catalog-fingerprint bridge-report))
                   (= (count sources) (:abstract-operation-count bridge-report))
                   (= (inc (count sources)) (:kernel-checked-bridge-count bridge-report)))
      (fail! :unqualified-kernel-or-source-boundary
             {:source-id current-id :bridge-id (:catalog-fingerprint bridge-report)}))
    {:checking :source-attested-kernel-backed-abstract-observer-pilot
     :catalog-fingerprint current-id
     :operation-count (count sources)
     :correspondence :per-invocation-checked-not-universally-proved
     :physical-correspondence :not-established
     :certificate :not-issued :admission :not-admitted :authority :none
     :observe!
     (fn [id before after]
       (let [source (get sources id)]
         (when-not source
           (fail! :undeclared-operation {:operation id}))
         ;; Reject invalid values EVEN if both observers are compromised.
         (closed-projection! (get-in source [:source :state-types]) :before before)
         (closed-projection! (get-in source [:source :state-types]) :after after)
         (let [transition (:source source)
               primary (abstract/observe-transition! transition before {} after)
               other (independent/observe-transition! transition before {} after)
               expected (require-observer-report! source before after primary other)
               matched? (:matches? expected)
               projected? (when matched?
                            (pilot! bridge-ns 'evaluate-projected! model before after))]
           (when (and matched? (not projected?))
             (fail! :accepted-observation-outside-proved-step
                    {:operation id :before before :after after}))
           {:operation-id id :matches? matched?
            :step-checked? (boolean matched?)
            :step-represented? (boolean projected?)
            :checking :exact-abstract-source-primitives
            :authority :none :admission :not-admitted
            :certificate :not-issued})))}))

(defn- initial-state [transition]
  ;; Test witness only: construct a typed closed projection satisfying the
  ;; source guard. NOT an installed database initial-state assertion.
  (let [types (:state-types transition)
        {:keys [field target]} (checked-guard! transition)]
    (into {} (for [[id descriptor] types]
               [id (if (= id field) target
                       (case (:type descriptor)
                         :integer 0
                         :enum (first (sort-by str (:values descriptor)))
                         :keyword (if (:nullable? descriptor)
                                    nil :audit/sample)))]))))

(defn- expected-post [transition before]
  (reduce (fn [state [field [_ literal]]]
            (assoc state field literal))
          before (:assignments transition)))

(defn- examples []
  [(pilot! initialization-ns 'initialized
           (pilot! preservation-ns 'mixed-catalog))
   (example/reservation-catalog)
   (pilot! initialization-ns 'initialized
           (pilot! preservation-ns 'example-catalog
                   :queue/temperature -20 30 [-5 30]))])

(deftest all-original-operations-have-checked-acceptance-and-rejection
  (doseq [catalog (examples)]
    (let [boundary (audited-boundary! catalog)
          observe! (:observe! boundary)
          sources (mapv #(pilot! preservation-ns 'original-transition! %)
                        (:operations catalog))]
      (is (= (count sources) (:operation-count boundary)))
      (is (= :not-established (:physical-correspondence boundary)))
      (is (= :none (:authority boundary)))
      (doseq [{:keys [id source]} sources]
        (let [pre (initial-state source)
              post (expected-post source pre)
              exact (observe! id pre post)
              changed-guard (assoc pre (:field (checked-guard! source))
                                   (first (remove #{(:target (checked-guard! source))}
                                                  (sort-by str (get-in source
                                                                       [:state-types
                                                                        (:field (checked-guard! source))
                                                                        :values])))))]
          (is (:matches? exact) (str "accepted original source " id))
          (is (:step-represented? exact))
          (is (false? (:matches? (observe! id changed-guard post))))
          (is (= :none (:authority exact))))))))

(deftest duplicate-false-observer-acceptance-cannot-inherit-weaker-step
  (let [catalog (first (examples))
        boundary (audited-boundary! catalog)
        observe! (:observe! boundary)
        source (:source (pilot! preservation-ns 'original-transition!
                                (first (:operations catalog))))
        pre (initial-state source)
        after (expected-post source pre)
        guard (:field (checked-guard! source))
        wrong-guard (assoc pre guard
                           (first (remove #{(get pre guard)}
                                          (sort-by str
                                                   (get-in source [:state-types guard :values])))))
        wrong-audit (assoc after :queue/audit :audit/changed)
        wrong-write (assoc after :queue/count 0)
        original-primary abstract/observe-transition!
        original-other independent/observe-transition!
        pretend-yes (fn [observer]
                      (fn [tr b in a]
                        (assoc (observer tr b in a)
                               :guard-holds? true :assignments-hold? true
                               :frame-holds? true :matches? true)))
        pretend-no (fn [observer]
                     (fn [tr b in a]
                       (assoc (observer tr b in a)
                              :guard-holds? false :assignments-hold? false
                              :frame-holds? false :matches? false)))
        reason (fn [before after]
                 (try (observe! :queue/start before after) nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))]
    (doseq [[label before result] [[:guard wrong-guard after]
                                   [:audit pre wrong-audit]
                                   [:assignment pre wrong-write]]]
      (is (false? (:matches? (observe! :queue/start before result)))
          (str label " is invalid original operation"))
      (with-redefs [abstract/observe-transition! (pretend-yes original-primary)
                   independent/observe-transition! (pretend-yes original-other)]
        (is (= :source-primitives-disagree-with-observers
               (reason before result))
            (str label " rejected despite colluding positive reports"))))
    (with-redefs [abstract/observe-transition! (pretend-no original-primary)
                 independent/observe-transition! (pretend-no original-other)]
      (is (= :source-primitives-disagree-with-observers (reason pre after))))
    (is (= :undeclared-operation
           (try (observe! :queue/not-declared pre after) nil
                (catch clojure.lang.ExceptionInfo e
                  (:reason (ex-data e))))))
    (is (= :invalid-closed-state (reason pre (assoc after :unused/key true))))))

(deftest original-typed-guard-disagreement-and-false-report-are-rejected
  (let [catalog (first (examples))
        boundary (audited-boundary! catalog)
        observe! (:observe! boundary)
        source (:source (pilot! preservation-ns 'original-transition!
                                (first (:operations catalog))))
        pre (initial-state source)
        after (expected-post source pre)
        evaluator rule-eval/evaluate-rule!
        reason (fn [] (try (observe! :queue/start pre after) nil
                           (catch clojure.lang.ExceptionInfo e
                             (:reason (ex-data e)))))]
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [& args] (assoc (apply evaluator args) :value false))]
      (is (contains? #{:observer-report-disagreement
                       :original-typed-guard-disagreement}
                     (reason))))
    (with-redefs [rule-eval/evaluate-rule!
                  (fn [& args] (assoc (apply evaluator args) :checking :unchecked))]
      (is (contains? #{:observer-report-disagreement
                       :original-typed-guard-disagreement}
                     (reason))))))

(deftest accepted-observations-still-require-the-checked-trace-step
  (let [catalog (first (examples))
        boundary (audited-boundary! catalog)
        source (:source (pilot! preservation-ns 'original-transition!
                                (first (:operations catalog))))
        pre (initial-state source)
        after (expected-post source pre)
        projected-var (ns-resolve bridge-ns 'evaluate-projected!)
        real-projection @projected-var]
    (is (:matches? ((:observe! boundary) :queue/start pre after)))
    (is (true? (real-projection
                (:model (pilot! bridge-ns 'bridge-obligations! catalog))
                pre after)))
    (with-redefs-fn {projected-var (fn [& _] false)}
      #(is (= :accepted-observation-outside-proved-step
              (try ((:observe! boundary) :queue/start pre after)
                   nil
                   (catch clojure.lang.ExceptionInfo ex
                     (:reason (ex-data ex)))))))))

(deftest invalid-typed-projections-rejected-before-observer-reports
  (let [catalog (first (examples))
        check! (:observe! (audited-boundary! catalog))
        source (:source (pilot! preservation-ns 'original-transition!
                                (first (:operations catalog))))
        pre (initial-state source)
        after (expected-post source pre)
        reason (fn [b a]
                 (try (check! :queue/start b a) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (doseq [bad [(assoc after :queue/count 1.25)
                 (assoc after :queue/status :not/declared)
                 (assoc after :queue/audit 42)
                 (dissoc after :queue/audit)
                 (assoc after :extra/field true)
                 (with-meta after {:unreviewed true})]]
      (is (= :invalid-closed-state (reason pre bad))))
    (is (= :invalid-closed-state
           (reason (assoc pre :queue/count 1.25) after)))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]}
        (run-tests 'gesso.assurance.ansatz-mixed-observation-gate-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

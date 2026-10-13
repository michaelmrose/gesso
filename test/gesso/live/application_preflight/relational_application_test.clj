(ns gesso.live.application-preflight.relational-application-test
  "Permanent v835 regressions for the opt-in relational application startup gate.

   A passing finite rule model is required BEFORE delegation to canonical
   Gesso/Biff startup. The same checked ApplicationAssembly must be used at
   that boundary, but the model, operation builders, and relational policy
   declarations are still supplied by the application. Neither these tests
   nor the explicit startup API establish policy completeness, physical XTDB
   enforcement, or protection against bypassing this opt-in startup path."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as sut]
   [gesso.model.command :as command]))

(def ^:private revision-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private target [:counters :one])
(def ^:private base-state
  {target {:xt/id :one
           :revision 0
           :created-at :t0
           :updated-at :t0
           :n 0
           :parent nil
           :active? true}})

(def ^:private active-parent-rule
  {:id :counter/active-parent
   :source-kind :counters
   :source-where {}
   :reference-attr :parent
   :allow-unset? true
   :target-kind :counters
   :target-where {:active? true}})

(defn- model-family
  ([id] (model-family id {}))
  ([id {:keys [calls wrong-parent? prepare-error?]
        :or {wrong-parent? false prepare-error? false}}]
   {:id id
    :principals [:helper/a]
    :arguments [{:delta 1}]
    :enabled? (fn [_ _ _]
                (when calls (swap! calls update :enabled (fnil inc 0)))
                true)
    :prepare (fn [state _ _]
               (when calls (swap! calls update :prepared (fnil inc 0)))
               (when prepare-error?
                 (throw (ex-info "A business callback refused preparation"
                                 {:error/kind :callback-refused})))
               (let [before (get state target)
                     after (cond-> (-> before
                                       (update :revision inc)
                                       (update :n inc)
                                       (assoc :updated-at :t1))
                             wrong-parent? (assoc :parent :not-present))]
                 {:commands [(command/update-command
                              :counters :increment before after revision-keys)]
                  :guards []
                  :emit false}))}))

(defn- complete-families
  ([] (complete-families {}))
  ([opts]
   [(model-family :request/claim opts)
    (model-family :request/reassign)
    (model-family :request/cancel)]))

(defn- finite-model
  ([] (finite-model {}))
  ([opts]
   {:initial-state base-state
    :families (complete-families opts)
    :declarations [active-parent-rule]
    :options {:max-depth 2 :max-transitions 50}}))

(defn- problem [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- kind [f]
  (:error/kind (problem f)))

(defn- with-app [f]
  (fixture/with-closed-application f))

(defn- redacted-violation [ex]
  (get-in ex [:error/details :counterexamples 0]))

(def real-modules [application/application-handler-module])

(deftest passing-construction-retains-canonical-assembly-and-scope
  (with-app
    (fn [{:keys [options]}]
      (let [admitted (sut/require-relational-application!
                      options (finite-model))
            finite (:finite-relational-verdict admitted)
            scope (:assurance-scope admitted)]
        (is (= #{:application-assembly :finite-relational-verdict
                 :assurance-scope}
               (set (keys admitted))))
        (is (application/application-assembly?
             (:application-assembly admitted)))
        (is (= :holds-in-explored-model (:status finite)))
        (is (= :exact-registered-operation-id-coverage
               (get-in finite [:registry-coverage :status])))
        (is (= [(:id active-parent-rule)]
               (get-in finite [:relational-coverage :rule-ids])))
        (is (contains? (:checked scope)
                       :same-checked-assembly-used-for-canonical-startup))
        (is (contains? (:not-established scope)
                       :unbounded-invariant-preservation))
        (is (contains? (:not-established scope)
                       :correspondence-of-family-callbacks-to-live-executors))
        (is (contains? (:not-established scope)
                       :enforcement-on-bypassed-or-direct-biff-startup))))))

(deftest valid-construction-actually-evaluates-model-operations
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom {})
            verdict (:finite-relational-verdict
                     (sut/require-relational-application!
                      options (finite-model {:calls calls})))]
        (is (= :holds-in-explored-model (:status verdict)))
        (is (pos? (:enabled @calls 0)))
        (is (pos? (:prepared @calls 0)))))))

(deftest absent-and-unknown-model-keys-fail-before-assembly
  (with-app
    (fn [{:keys [options]}]
      (let [builds (atom 0)]
        (with-redefs [application/require-application-assembly!
                      (fn [& _] (swap! builds inc) :must-not-build)]
          (doseq [bad [nil [] {}
                       (dissoc (finite-model) :initial-state)
                       (dissoc (finite-model) :families)
                       (dissoc (finite-model) :declarations)
                       (dissoc (finite-model) :options)
                       (assoc (finite-model) :proof-claimed? true)]]
            (is (= :invalid-model-options
                   (kind #(sut/require-relational-application! options bad)))))
          (is (zero? @builds)))))))

(deftest missing-registered-operation-cannot-enter-model
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom {})
            incomplete (-> (finite-model)
                           (assoc :families
                                  [(model-family :request/claim {:calls calls})]))]
        (is (= :operation-identity-coverage-mismatch
               (kind #(sut/require-relational-application! options incomplete))))
        (is (= {} @calls))))))

(deftest extra-operation-is-not-counted-as-registered
  (with-app
    (fn [{:keys [options]}]
      (is (= :operation-identity-coverage-mismatch
             (kind #(sut/require-relational-application!
                     options
                     (update (finite-model) :families
                             conj (model-family :request/not-registered)))))))))

(deftest duplicate-operation-identities-are-rejected
  (with-app
    (fn [{:keys [options]}]
      (is (= :duplicate-family
             (kind #(sut/require-relational-application!
                     options
                     (assoc (finite-model) :families
                            [(model-family :request/claim)
                             (model-family :request/claim)
                             (model-family :request/cancel)]))))))))

(deftest excessive-principal-domains-are-not-sampled
  (with-app
    (fn [{:keys [options]}]
      (is (= :instance-bound-exceeded
             (kind #(sut/require-relational-application!
                     options
                     (update (finite-model) :families
                             (fn [fs]
                               (mapv (fn [family] (assoc family :principals [:a :b :c])) fs))))))))))

(deftest empty-relations-rejected-rather-than-vacuous-admission
  (with-app
    (fn [{:keys [options]}]
      (is (= :invalid-declarations
             (kind #(sut/require-relational-application!
                     options (assoc (finite-model) :declarations []))))))))

(deftest opaque-predicate-is-not-admitted-as-a-relational-declaration
  (with-app
    (fn [{:keys [options]}]
      (is (= :invalid-declaration
             (kind #(sut/require-relational-application!
                     options
                     (assoc (finite-model) :declarations
                            [(assoc active-parent-rule
                                    :holds? (constantly true))]))))))))

(deftest invalid-initial-reference-cannot-be-admitted
  (with-app
    (fn [{:keys [options]}]
      (let [bad-state (assoc-in base-state [target :parent] :missing)
            error (problem #(sut/require-relational-application!
                            options (assoc (finite-model)
                                           :initial-state bad-state)))]
        (is (= :finite-relational-invariant-violation (:error/kind error)))
        (is (= :counterexample (get-in error [:error/details :status])))
        (is (= :counter/active-parent (get-in error
                                            [:error/details :counterexamples 0 :rule-id])))
        (is (empty? (:transition-path (redacted-violation error))))))))

(deftest invalid-transition-fails-with-family-aware-redacted-trace
  (with-app
    (fn [{:keys [options]}]
      (let [bad (finite-model {:wrong-parent? true})
            error (problem #(sut/require-relational-application! options bad))
            counterexample (redacted-violation error)
            step (first (:transition-path counterexample))]
        (is (= :finite-relational-invariant-violation (:error/kind error)))
        (is (= :counterexample (get-in error [:error/details :status])))
        (is (= :counter/active-parent (:rule-id counterexample)))
        (is (= :request/claim (:operation-id step)))
        (is (= 0 (:principal-choice-index step)))
        (is (= 0 (:argument-choice-index step)))
        (is (= :committed (:status step)))
        (is (not (contains? (:error/details error) :initial-state)))
        (is (not (contains? (:error/details error) :families)))
        (is (not (contains? (:error/details error) :declarations)))))))

(deftest sensitive-state-values-are-not-exposed-in-counterexample
  (with-app
    (fn [{:keys [options]}]
      (let [secret "sensitive-internal-value-do-not-report"
            bad-state (assoc-in base-state [target :parent] secret)
            err (problem #(sut/require-relational-application!
                           options (assoc (finite-model)
                                          :initial-state bad-state)))
            details (:error/details err)]
        (is (= :finite-relational-invariant-violation (:error/kind err)))
        (is (not (.contains (pr-str details) secret)))
        (is (not (.contains (pr-str details) ":commands")))))))

(deftest exploration-budget-exhaustion-fails-closed
  (with-app
    (fn [{:keys [options]}]
      (is (= :exploration-budget-exhausted
             (kind #(sut/require-relational-application!
                     options (assoc (finite-model)
                                    :options {:max-depth 2
                                              :max-transitions 1}))))))))

(deftest callback-failure-cannot-be-reported-as-passing-model
  (with-app
    (fn [{:keys [options]}]
      (let [error (problem #(sut/require-relational-application!
                            options (finite-model {:prepare-error? true})))]
        (is (some? error))
        (is (= :gesso.model.invariant-check/operation-evaluation-failed
               (:error/type error)))
        (is (= :prepare (:phase error)))))))

(deftest stale-browser-artifact-prevents-construction-before-callbacks
  (with-app
    (fn [{:keys [options artifact-path]}]
      (spit artifact-path "stale runtime after build receipt")
      (let [calls (atom {})
            error (problem #(sut/require-relational-application!
                            options (finite-model {:calls calls})))]
        (is (some? error))
        (is (= {} @calls))))))

(deftest four-argument-start-delegates-admitted-assembly-exactly-once
  (with-app
    (fn [{:keys [options]}]
      (let [created (atom nil)
            calls (atom [])
            original application/require-application-assembly!
            mods (Object.)
            order [application/application-handler-module-id]]
        (with-redefs [application/require-application-assembly!
                      (fn [opts]
                        (let [a (original opts)] (reset! created a) a))
                      application/start-biff-application!
                      (fn [& args] (swap! calls conj args) :started)]
          (is (= :started
                 (sut/start-biff-relational-application!
                  options (finite-model) mods order)))
          (is (= 1 (count @calls)))
          (is (identical? @created (first (first @calls))))
          (is (= [mods order] (vec (rest (first @calls))))))))))

(deftest five-argument-start-preserves-initial-system-and-order
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom [])
            initial-system {:biff/app :initial}
            mods (Object.)
            order [application/application-handler-module-id]]
        (with-redefs [application/start-biff-application!
                      (fn [& args] (swap! calls conj args) :started)]
          (is (= :started
                 (sut/start-biff-relational-application!
                  options (finite-model) initial-system mods order)))
          (is (= 1 (count @calls)))
          (is (application/application-assembly? (first (first @calls))))
          (is (identical? initial-system (second (first @calls))))
          (is (identical? mods (nth (first @calls) 2)))
          (is (= order (nth (first @calls) 3))))))))

(deftest rejected-models-never-invoke-startup-on-either-arity
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom 0)
            bad (finite-model {:wrong-parent? true})]
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! calls inc) :should-not-run)]
          (is (= :finite-relational-invariant-violation
                 (kind #(sut/start-biff-relational-application!
                         options bad nil []))))
          (is (= :finite-relational-invariant-violation
                 (kind #(sut/start-biff-relational-application!
                         options bad {} nil []))))
          (is (zero? @calls)))))))

(deftest invalid-registry-cannot-trigger-startup
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom 0)]
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! calls inc) :should-not-run)]
          (is (= :operation-identity-coverage-mismatch
                 (kind #(sut/start-biff-relational-application!
                         options
                         (assoc (finite-model)
                                :families [(model-family :request/claim)])
                         nil []))))
          (is (zero? @calls)))))))

(deftest stale-browser-artifact-cannot-trigger-startup
  (with-app
    (fn [{:keys [options artifact-path]}]
      (spit artifact-path "modified after expected artifact receipt")
      (let [calls (atom 0)]
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! calls inc) :should-not-run)]
          (is (some? (problem #(sut/start-biff-relational-application!
                                options (finite-model) nil []))))
          (is (zero? @calls)))))))

(deftest exhausted-budget-cannot-trigger-startup
  (with-app
    (fn [{:keys [options]}]
      (let [calls (atom 0)]
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! calls inc) :should-not-run)]
          (is (= :exploration-budget-exhausted
                 (kind #(sut/start-biff-relational-application!
                         options (assoc (finite-model)
                                        :options {:max-depth 2
                                                  :max-transitions 1})
                         nil []))))
          (is (zero? @calls)))))))

(deftest invalid-module-order-still-rejected-by-canonical-startup
  (with-app
    (fn [{:keys [options]}]
      (let [invalid-order [:other/unregistered
                           application/application-handler-module-id]]
        (is (some? (problem #(sut/start-biff-relational-application!
                             options (finite-model)
                             {:biff.ring/handler
                              (fn [_] (fixture/html-response [:p "test"]))}
                             (var real-modules)
                             invalid-order))))))))

(deftest real-biff-handler-serves-response-after-passing-gate
  (with-app
    (fn [{:keys [options]}]
      (let [handler (fn [_]
                      (fixture/html-response [:main [:p "relational gate admitted"]]))
            started (sut/start-biff-relational-application!
                     options (finite-model)
                     {:biff.ring/handler handler}
                     #'real-modules
                     [application/application-handler-module-id])
            response ((:biff.ring/handler started) {:uri "/"})]
        (is (fn? (:biff.ring/handler started)))
        (is (= 200 (:status response)))
        (is (re-find #"text/html"
                     (get-in response [:headers "content-type"])))
        (is (not (contains? started application/application-assembly-system-key)))))))

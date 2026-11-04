(ns gesso.assurance.proof.typed-atom-fuzz-test
  "v1323: permanent adversarial regression for v1322 deterministic generated
   typed-expression checking. These are ORIGINAL reader declarations and
   binding domains, not proof reports. The independent v1320 interpretation
   is compared with the production evaluator only on a bounded generated
   grammar. Even complete finite value domains do not prove the general
   language sound, certify the checker, or authorize a transaction.

   Negative tests distinguish invalid configuration, invalid generated source,
   per-expression/aggregate budget exhaustion, genuine semantic divergence,
   and a syntactically well-formed but forged production result. Shrinking
   must retain the same discrepancy on a single reproducible binding."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.typed-atom-fuzz :as fuzz]
            [gesso.assurance.proof.typed-atom-oracle :as independent]
            [gesso.assurance.rule-eval :as production]))

(def ^:private bool {:type :boolean})
(def ^:private status
  {:type :enum :name :request/status :values #{:open :claimed :done}})
(def ^:private score {:type :integer})
(def ^:private nullable-score
  {:type :integer :optional? true :nullable? true})

(defn- config
  ([types domains] (config types domains 64 17))
  ([types domains trials seed]
   {:reader-types types :binding-domains domains
    :seed seed :max-expressions trials
    :max-cases 512 :max-total-cases 16384}))

(defn- audit [c] (fuzz/audit-generated-atoms! c))

(defn- diagnostic! [r]
  (is (= fuzz/report-type (:type r)) (pr-str r))
  (is (= fuzz/fuzz-version (:version r)))
  (is (= :generated-typed-expression-independent-crosscheck (:checking r)))
  (is (= :generated-expressions-and-supplied-binding-domains-only
         (:scope r)))
  (is (= :not-proved (:proof-status r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :not-established (:independent-checker-soundness r)))
  (is (= :not-established (:atomic-semantics-universality r)))
  (is (= :not-established (:expression-language-exhaustiveness r)))
  (is (= :not-established (:jvm-cljs-equivalence r)))
  (is (= :not-established (:trusted-snapshot-provenance r)))
  (is (not (contains? r :certificate)))
  (is (not (contains? r :transaction-handle)))
  r)

(defn- agreement! [c domain-coverage]
  (let [r (diagnostic! (audit c))]
    (is (= :bounded-agreement (:status r)) (pr-str r))
    (is (= domain-coverage (:domain-coverage r)))
    (is (= :sampled-from-restricted-grammar (:expression-coverage r)))
    (is (pos? (:expressions-checked r)))
    (is (<= (:expressions-checked r) (:max-expressions c)))
    (is (<= (:cases-checked r) (:max-total-cases c)))
    (is (= (:cases-checked r) (+ (:true-cases r) (:false-cases r))))
    (is (<= (:expressions-checked r) (:expression-corpus-size r)))
    r))

(defn- failure! [c expected-status expected-reason]
  (let [r (diagnostic! (audit c))]
    (is (= expected-status (:status r)) (pr-str r))
    (is (= expected-reason (:reason r)) (pr-str r))
    (is (not= :bounded-agreement (:status r)))
    r))

(defn- contains-op? [expression target]
  (and (vector? expression)
       (or (= target (first expression))
           (boolean (some #(contains-op? % target) (rest expression))))))

(defn- shrunk-rechecks! [result]
  (let [{:keys [source comparison attempts accepted local-minimality]}
        (:shrunk result)
        rerun (independent/audit-typed-atom! source)]
    (is (= :divergence (:status result)) (pr-str result))
    (is (= :divergence (:status comparison)))
    (is (= comparison rerun))
    (is (= (:reason result) (:reason comparison)))
    (is (= 1 (:cases-checked comparison)))
    (is (= 1 (:cases-planned comparison)))
    (is (every? #(= 1 (count %)) (vals (:binding-domains source))))
    (is (<= 0 accepted attempts 64))
    (is (#{:budget-limited
           :no-smaller-defined-reduction-preserves-discrepancy}
         local-minimality))
    (is (= :none (:authority comparison)))
    source))

(deftest no-reader-corpus-is-finite-but-not-exhaustive-language-proof
  (doseq [seed [0 1 27 2147483647]]
    (let [c (config {} {} 8 seed)
          r (agreement! c :complete-declared-finite-values)]
      (is (= seed (:seed r)))
      (is (= (:expressions-checked r) (:cases-checked r)))
      (is (= r (audit c))))))

(deftest empty-universe-still-respects-the-expression-budget
  (let [c (config {} {} 1 0)
        r (agreement! c :complete-declared-finite-values)]
    (is (= 1 (:expressions-checked r)))
    (is (= 1 (:cases-checked r)))))

(deftest complete-boolean-value-domain
  (let [r (agreement! (config {:request/active bool}
                              {:request/active [false true]} 40 19)
                      :complete-declared-finite-values)]
    (is (= 40 (:expressions-checked r)))
    (is (<= 40 (:cases-checked r)))))

(deftest incomplete-boolean-domain-is-only-a-representative
  (agreement! (config {:request/active bool}
                      {:request/active [false]} 20 11)
              :representative-values))

(deftest complete-enum-domain-versus-partial-enum-domain
  (let [t {:request/status status}]
    (agreement! (config t {:request/status [:done :open :claimed]} 45 5)
                :complete-declared-finite-values)
    (agreement! (config t {:request/status [:done :open]} 45 5)
                :representative-values)))

(deftest boolean-enum-product-is-finite
  (let [c (config {:request/status status :request/active bool}
                  {:request/active [false true]
                   :request/status [:claimed :open :done]} 64 51)
        r (agreement! c :complete-declared-finite-values)]
    (is (= 64 (:expressions-checked r)))
    (is (= r (audit c)))))

(deftest nullable-boolean-completeness-requires-nil
  (let [t {:request/active {:type :boolean :nullable? true}}]
    (agreement! (config t {:request/active [true nil false]} 40 7)
                :complete-declared-finite-values)
    (agreement! (config t {:request/active [true false]} 40 7)
                :representative-values)))

(deftest optional-boolean-completeness-requires-absence
  (let [t {:request/active {:type :boolean :optional? true}}]
    (agreement! (config t {:request/active [independent/absent false true]}
                        40 42)
                :complete-declared-finite-values)
    (agreement! (config t {:request/active [false true]} 40 42)
                :representative-values)))

(deftest nullable-optional-boolean-covers-four-distinct-states
  (let [t {:request/active {:type :boolean :optional? true :nullable? true}}
        c (config t {:request/active [independent/absent nil false true]}
                  48 20)]
    (agreement! c :complete-declared-finite-values)
    (agreement! (assoc-in c [:binding-domains :request/active]
                          [independent/absent nil true])
                :representative-values)))

(deftest optional-absence-and-present-nil-are-distinct-runtime-bindings
  (let [c (config {:request/value {:type :boolean :optional? true
                                   :nullable? true}}
                  {:request/value [independent/absent nil true false]} 1 8)
        seen (atom [])
        original production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (swap! seen conj bindings)
                    (original rule types bindings))]
      (let [r (agreement! c :complete-declared-finite-values)]
        (is (= 1 (:expressions-checked r)))
        (is (= 4 (:cases-checked r)))
        (is (some #(not (contains? % :request/value)) @seen))
        (is (some #(and (contains? % :request/value)
                        (nil? (:request/value %))) @seen))
        (is (some #(= true (:request/value %)) @seen))
        (is (some #(= false (:request/value %)) @seen))))))

(deftest all-reader-presence-probes-are-mandatory-even-at-trial-limit
  (let [types {:request/z {:type :integer :optional? true}
               :request/a {:type :boolean :optional? true}
               :request/m {:type :string :optional? true}}
        domains {:request/z [independent/absent 1]
                 :request/a [independent/absent false true]
                 :request/m [independent/absent "x"]}
        seen (atom [])
        original production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule t bindings]
                    (swap! seen conj (:expression rule))
                    (original rule t bindings))]
      (let [r (agreement! (config types domains 3 72)
                          :representative-values)
            expressions (set @seen)]
        (is (= 3 (:expressions-checked r)))
        (is (= #{[:present [:read :request/a]]
                 [:present [:read :request/m]]
                 [:present [:read :request/z]]}
               expressions))
        (is (= 7 (:cases-checked r)))))))

(deftest generation-is-deterministic-under-map-insertion-order
  (let [a (array-map :request/z {:type :integer}
                     :request/a bool)
        b (array-map :request/a bool
                     :request/z {:type :integer})
        c (config a {:request/z [0 1 2] :request/a [true false]} 60 88)
        d (config b {:request/a [true false] :request/z [0 1 2]} 60 88)]
    (is (= (audit c) (audit d)))))

(deftest repeated-runs-preserve-seeded-results-and-do-not-mutate-config
  (let [c (config {:request/score score}
                  {:request/score [-1 0 1 7]} 52 512)
        original c
        a (agreement! c :representative-values)]
    (is (= a (audit c)))
    (is (= a (audit c)))
    (is (= original c))))

(deftest seed-matrix-produces-valid-typed-corpora
  (let [types {:request/status {:type :enum :name :request/status
                                :values #{:open :done}
                                :optional? true :nullable? true}
               :request/score nullable-score
               :request/active {:type :boolean :nullable? true}
               :request/title {:type :string :optional? true}}
        domains {:request/status [independent/absent nil :open :done]
                 :request/score [independent/absent nil -5 0 9]
                 :request/active [nil false true]
                 :request/title [independent/absent "" "hello"]}]
    (doseq [seed (range 25)]
      (let [c (config types domains 64 seed)
            r (audit c)]
        (is (= :bounded-agreement (:status r)) (pr-str [seed r]))
        (is (= 64 (:expressions-checked r)))
        (is (= :representative-values (:domain-coverage r)))
        (is (= :none (:authority r)))))))

(deftest huge-integers-and-small-samples-use-arbitrary-precision
  (agreement!
   (config {:request/score score}
           {:request/score [-99999999999999999999999999999N
                            0N 99999999999999999999999999999N]}
           60 13)
   :representative-values))

(deftest keyword-string-and-integer-products-are-samples
  (agreement!
   (config {:request/key {:type :keyword}
            :request/title {:type :string}
            :request/count {:type :integer}}
           {:request/key [:alpha :beta]
            :request/title ["" "hello"]
            :request/count [-1 0 1]}
           62 25)
   :representative-values))

(deftest nullable-numeric-short-circuit-never-evaluates-absent-read
  (let [r (agreement!
           (config {:request/score nullable-score}
                   {:request/score [independent/absent nil -1 0 1]}
                   80 77)
           :representative-values)]
    (is (= 80 (:expressions-checked r)))))

(deftest compound-disjoint-reader-operators-stay-well-typed
  (let [types {:request/a bool :request/b bool
               :request/c {:type :integer}}
        domains {:request/a [false true] :request/b [false true]
                 :request/c [-1 0 7]}
        seen (atom #{})
        original production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule t bindings]
                    (swap! seen conj (first (:expression rule)))
                    (original rule t bindings))]
      (agreement! (config types domains 256 17) :representative-values)
      (is (contains? @seen :and))
      (is (contains? @seen :or))
      (is (contains? @seen :implies)))))

(deftest invalid-config-keys-and-forged-credentials-fail-closed
  (let [c (config {} {} 4 1)]
    (doseq [field [:certificate :proof-status :authority
                   :admission :prior-report :issued-executor :runtime-enforcement]]
      (failure! (assoc c field :forged) :invalid-config :wrong-keys))
    (failure! (dissoc c :seed) :invalid-config :wrong-keys)
    (failure! (dissoc c :reader-types) :invalid-config :wrong-keys)))

(deftest invalid-top-level-shapes-and-metadata-fail-closed
  (let [c (config {} {})]
    (failure! nil :invalid-config :expected-unannotated-map)
    (failure! [] :invalid-config :expected-unannotated-map)
    (failure! (with-meta c {:proof :passed})
              :invalid-config :expected-unannotated-map)
    (failure! (into (sorted-map) c)
              :invalid-config :expected-unannotated-map)))

(deftest invalid-reader-and-domain-shapes-fail-closed
  (let [c (config {:request/active bool} {:request/active [false true]})]
    (failure! (assoc c :reader-types (sorted-map :request/active bool))
              :invalid-config :invalid-reader-types)
    (failure! (assoc c :binding-domains
                     (sorted-map :request/active [false true]))
              :invalid-config :invalid-binding-domains)
    (failure! (assoc c :binding-domains {})
              :invalid-config :invalid-binding-domains)
    (failure! (assoc c :reader-types (with-meta (:reader-types c) {:x 1}))
              :invalid-config :invalid-reader-types)
    (failure! (assoc c :reader-types
                     (zipmap (map #(keyword "request" (str "r" %)) (range 9))
                             (repeat bool)))
              :invalid-config :invalid-reader-types)))

(deftest seed-validity-and-upper-bound-are-enforced
  (let [c (config {} {})]
    (doseq [seed [-1 2147483648N 1.25 "1" nil true]]
      (failure! (assoc c :seed seed) :invalid-config :invalid-seed))
    (is (= :bounded-agreement
           (:status (audit (assoc c :seed 2147483647)))))))

(deftest trial-budgets-require-every-reader-and-a-finite-upper-limit
  (let [c (config {:request/a bool :request/b bool}
                  {:request/a [true false] :request/b [true false]} 2 5)]
    (doseq [n [0 1 257 1000000 -2 nil]]
      (failure! (assoc c :max-expressions n)
                :invalid-config :invalid-trial-budget))
    (is (= :bounded-agreement
           (:status (audit (assoc c :max-expressions 2)))))))

(deftest per-expression-budget-rejects-exhaustion
  (let [c (config {:request/active bool}
                  {:request/active [true false]} 20 5)]
    (let [r (failure! (assoc c :max-cases 1 :max-total-cases 2)
                      :budget-exceeded :per-expression-enumeration-limit)]
      (is (= 0 (:expressions-checked r)))
      (is (= 0 (:cases-checked r)))
      (is (= :budget-exceeded (:status (:oracle-result r)))))))

(deftest aggregate-budget-must-not-be-silently-truncated
  (let [c (config {:request/active bool}
                  {:request/active [true false]} 60 33)
        r (failure! (assoc c :max-cases 2 :max-total-cases 2)
                    :budget-exceeded :aggregate-enumeration-limit)]
    (is (= 1 (:expressions-checked r)))
    (is (= 2 (:cases-checked r)))
    (is (pos? (:expression-corpus-size r)))))

(deftest invalid-case-budgets-are-rejected-at-configuration-boundary
  (let [c (config {} {})]
    (doseq [n [0 -1 8193 nil 1.5]]
      (failure! (assoc c :max-cases n)
                :invalid-config :invalid-case-budget))
    (doseq [n [0 1 511 32769 nil 1.5]]
      (failure! (assoc c :max-total-cases n)
                :invalid-config :invalid-total-budget))))

(deftest invalid-domain-is-not-skipped-to-next-generated-expression
  (let [c (config {:request/flag bool} {:request/flag [false nil]} 40 3)
        r (failure! c :generation-error :generated-source-not-evaluable)]
    (is (= :invalid-source (get-in r [:oracle-result :status])))
    (is (= :ill-typed-domain-entry
           (get-in r [:oracle-result :reason])))
    (is (= 0 (:expressions-checked r)))))

(deftest absent-required-field-is-an-invalid-generated-source
  (let [c (config {:request/flag bool}
                  {:request/flag [independent/absent true]} 20 2)
        r (failure! c :generation-error :generated-source-not-evaluable)]
    (is (= :absent-required-field (get-in r [:oracle-result :reason])))))

(deftest invalid-enum-member-remains-a-generation-error
  (let [c (config {:request/status status}
                  {:request/status [:open :invalid]} 30 4)
        r (failure! c :generation-error :generated-source-not-evaluable)]
    (is (= :ill-typed-domain-entry (get-in r [:oracle-result :reason])))))

(deftest metadata-in-nested-source-is-not-silently-accepted
  (let [c (config {:request/active (with-meta bool {:checked true})}
                  {:request/active [true false]} 30 4)
        r (failure! c :generation-error :generated-source-not-evaluable)]
    (is (= :invalid-source (get-in r [:oracle-result :status])))
    (is (= :metadata-not-allowed (get-in r [:oracle-result :reason])))))

(deftest duplicate-and-empty-domains-are-invalid-generated-sources
  (let [c (config {:request/active bool} {:request/active [false true]})]
    (doseq [[values reason]
            [[[false false] :duplicate-domain-entry]
             [[] :invalid-domain]]]
      (let [r (failure! (assoc-in c [:binding-domains :request/active] values)
                        :generation-error :generated-source-not-evaluable)]
        (is (= reason (get-in r [:oracle-result :reason])))))))

(deftest an-injected-opposite-result-produces-repeatable-counterexample
  (let [c (config {:request/active bool}
                  {:request/active [false true]} 48 90)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (update (actual rule types bindings) :value not))]
      (let [r (diagnostic! (audit c))
            shrunk (shrunk-rechecks! r)]
        (is (= :different-truth-value (:reason r)))
        (is (= :different-truth-value
               (get-in r [:shrunk :comparison :reason])))
        (is (= :divergence
               (:status (audit (config {} {} 1 1)))))
        (is (map? shrunk))
        (is (= r (audit c)))))))

(deftest forced-true-production-evaluator-finds-false-expression
  (let [c (config {:request/score score} {:request/score [-1 0 1]} 60 97)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (assoc (actual rule types bindings) :value true))]
      (let [r (diagnostic! (audit c))]
        (is (= :divergence (:status r)))
        (is (= :different-truth-value (:reason r)))
        (is (= false (get-in r [:counterexample :independent-value])))
        (is (= true (get-in r [:counterexample :production-value])))
        (shrunk-rechecks! r)
        (is (= r (audit c)))))))

(deftest operator-specific-ordering-bug-is-retained-by-shrinking
  (let [c (config {:request/score score}
                  {:request/score [-5 0 7]} 220 22)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (let [r (actual rule types bindings)]
                      (if (contains-op? (:expression rule) :>)
                        (update r :value not) r)))]
      (let [r (diagnostic! (audit c))
            src (shrunk-rechecks! r)]
        (is (= :different-truth-value (:reason r)))
        (is (contains-op? (get-in src [:rule :expression]) :>))
        (is (= r (audit c)))))))

(deftest operator-specific-membership-bug-is-retained-by-shrinking
  (let [c (config {:request/status status}
                  {:request/status [:open :claimed :done]} 200 37)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (let [r (actual rule types bindings)]
                      (if (contains-op? (:expression rule) :in)
                        (update r :value not) r)))]
      (let [r (diagnostic! (audit c))
            src (shrunk-rechecks! r)]
        (is (= :different-truth-value (:reason r)))
        (is (contains-op? (get-in src [:rule :expression]) :in))
        (is (= r (audit c)))))))

(deftest counterexample-has-only-one-reproducible-binding
  (let [c (config {:request/active {:type :boolean :optional? true}}
                  {:request/active [independent/absent false true]} 30 44)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (let [r (actual rule types bindings)]
                      (if (contains-op? (:expression rule) :present)
                        (update r :value not)
                        r)))]
      (let [r (audit c)
            source (shrunk-rechecks! r)]
        (is (= 1 (count (get-in source [:binding-domains :request/active]))))
        (is (contains-op? (get-in source [:rule :expression]) :present))
        (is (= r (audit c)))))))

(deftest malformed-production-result-never-looks-like-agreement
  (let [c (config {} {} 2 5)]
    (with-redefs [production/evaluate-rule!
                  (fn [& _] {:value true :authority :transaction})]
      (let [r (failure! c :divergence :invalid-production-report)]
        (is (= :invalid-production-report
               (get-in r [:shrunk :comparison :reason])))
        (shrunk-rechecks! r)))))

(deftest substituted-production-evaluator-error-is-a-counterexample
  (let [c (config {:request/active bool}
                  {:request/active [false true]} 12 3)]
    (with-redefs [production/evaluate-rule!
                  (fn [& _] (throw (ex-info "fault injection" {:reason :injected}))) ]
      (let [r (failure! c :divergence :production-evaluator-rejected)]
        (is (= :injected (get-in r [:counterexample :production-error])))
        (let [src (shrunk-rechecks! r)]
          (is (= :injected (get-in (independent/audit-typed-atom! src)
                                  [:counterexample :production-error]))))))))

(deftest exact-production-report-with-forged-proof-status-is-divergent
  (let [c (config {} {} 2 8)
        actual production/evaluate-rule!]
    (with-redefs [production/evaluate-rule!
                  (fn [rule types bindings]
                    (assoc (actual rule types bindings) :proof-status :proved))]
      (let [r (failure! c :divergence :invalid-production-report)]
        (shrunk-rechecks! r)))))

(deftest generated-audit-is-not-a-proof-credential
  (let [c (config {:request/active bool}
                  {:request/active [false true]} 15 14)
        r (diagnostic! (audit c))]
    (is (= :bounded-agreement (:status r)))
    (is (= :none (:authority r)))
    (is (= :not-proved (:proof-status r)))
    (is (= :not-admitted (:admission r)))
    (is (not (contains? r :certificate)))
    (is (= :not-established (:expression-language-exhaustiveness r)))
    (is (= :not-established (:atomic-semantics-universality r)))))

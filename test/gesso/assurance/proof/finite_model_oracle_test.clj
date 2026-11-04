(ns gesso.assurance.proof.finite-model-oracle-test
  "v1310: permanent adversarial semantic tests for the INDEPENDENT v1309
   finite-universe oracle. These fixtures deliberately use original catalog
   data rather than induction, certificate, or typed-rule checker results.

   Passing tests do not establish metatheoretical checker soundness. Positive
   results are bounded by the explicitly supplied finite representatives;
   reports are freely copyable diagnostics and grant no transaction authority."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.finite-model-oracle :as oracle]))

(def ^:private status-type
  {:type :enum :name :request/status :values #{:open :claimed}})
(def ^:private state-types
  {:request/status status-type
   :request/owner {:type :keyword :optional? true :nullable? true}})
(def ^:private init
  {:readers #{:request/status}
   :expression [:= [:read :request/status] [:literal :open]]})
(def ^:private valid-status
  {:readers #{:request/status}
   :expression [:in [:read :request/status]
                [:literal #{:open :claimed}]]})

(defn- operation
  ([id from to] (operation id from to state-types))
  ([id from to types]
   {:id id
    :construction
    {:subject {:kind :operation :id id :interface {:operation id}}
     :state-types types :input-types {}
     :guard {:readers #{:request/status}
             :expression [:= [:read :request/status] [:literal from]]}
     :reads #{:request/status}
     :writes #{:request/status}
     :replacements {:request/status [:literal to]}
     :frame (disj (set (keys types)) :request/status)}}))

(def ^:private base-catalog
  {:id :request/oracle-regression
   :declared-operation-ids #{:request/claim :request/unclaim}
   :initial-condition init
   :invariants {:request/status-domain valid-status}
   :operations [(operation :request/claim :open :claimed)
                (operation :request/unclaim :claimed :open)]})
(def ^:private base-source
  {:catalog base-catalog
   :state-domains {:request/status [:open :claimed]
                   :request/owner [oracle/absent nil :person/alice]}
   :input-domains {:request/claim {} :request/unclaim {}}
   :max-cases 100})

(defn- outcome [source] (oracle/check-finite-model! source))
(defn- as-single [source op]
  (-> source
      (assoc-in [:catalog :operations] [op])
      (assoc-in [:catalog :declared-operation-ids] #{(:id op)})
      (assoc :input-domains {(:id op) {}})))
(defn- with-invariant [source id readers expression]
  (assoc-in source [:catalog :invariants id]
            {:readers readers :expression expression}))
(defn- expect-status [status source]
  (let [r (outcome source)]
    (is (= status (:status r)) (pr-str r))
    (is (= :none (:authority r)) (pr-str r))
    (is (= :not-admitted (:admission r)) (pr-str r))
    (is (= :not-issued (:certificate r)) (pr-str r))
    (is (= :not-established (:checker-soundness r)) (pr-str r))
    (is (= :not-established (:physical-correspondence r)) (pr-str r))
    (is (= :not-established (:universal-induction r)) (pr-str r))
    r))

(deftest bounded-two-operation-machine-counts-every-case
  (let [r (expect-status :bounded-no-counterexample base-source)]
    (is (= oracle/report-type (:type r)))
    (is (= oracle/oracle-version (:version r)))
    (is (= :independent-bounded-enumeration (:checking r)))
    (is (:exhaustive? r))
    (is (= 6 (get-in r [:universe :states])))
    (is (= {:request/claim 1 :request/unclaim 1}
           (get-in r [:universe :input-counts-by-operation])))
    (is (= 18 (get-in r [:universe :potential-cases])))
    (is (= {:initial-states-examined 6
            :initial-states-satisfying 3
            :transition-cases-examined 12
            :enabled-transitions-examined 6}
           (:examined r)))
    (is (false? (get-in r [:universe :all-type-domains-complete?])))))

(deftest complete-finite-enum-and-boolean-universe
  (let [t {:request/status status-type
           :request/flag {:type :boolean}}
        o (operation :request/claim :open :claimed t)
        s (-> (as-single base-source o)
              (assoc-in [:catalog :invariants]
                        {:request/status-domain valid-status})
              (assoc :state-domains {:request/status [:open :claimed]
                                     :request/flag [false true]}
                     :max-cases 8))
        r (expect-status :bounded-no-counterexample s)]
    (is (true? (get-in r [:universe :all-type-domains-complete?])))
    (is (= 4 (get-in r [:universe :states])))
    (is (= 8 (get-in r [:universe :potential-cases])))
    (is (= 2 (get-in r [:examined :initial-states-satisfying])))
    (is (= 2 (get-in r [:examined :enabled-transitions-examined])))))

(deftest nullable-and-optional-boolean-domains-can-be-complete
  (let [t {:request/status status-type
           :request/flag {:type :boolean :nullable? true :optional? true}}
        o (operation :request/claim :open :claimed t)
        s (-> (as-single base-source o)
              (assoc :state-domains {:request/status [:open :claimed]
                                     :request/flag [oracle/absent nil false true]}
                     :max-cases 16))
        r (expect-status :bounded-no-counterexample s)]
    (is (true? (get-in r [:universe :all-type-domains-complete?])))
    (is (= 8 (get-in r [:universe :states])))
    (is (= 4 (get-in r [:examined :initial-states-satisfying])))))

(deftest positive-result-on-representatives-is-not-universal
  (let [r (expect-status :bounded-no-counterexample base-source)]
    (is (false? (get-in r [:universe :all-type-domains-complete?])))
    (is (= :enumerated-declared-abstract-machine-only (:scope r)))
    (is (not (contains? r :proof-handle)))
    (is (not (contains? r :transaction-authority)))))

(deftest base-counterexample-is-concrete-and-reproducible
  (let [s (with-invariant base-source :request/must-be-claimed
                          #{:request/status}
                          [:= [:read :request/status] [:literal :claimed]])
        r (expect-status :counterexample s)
        c (:counterexample r)]
    (is (= :base (:kind c)))
    (is (= :request/must-be-claimed (:invariant-id c)))
    (is (= :open (get-in c [:state :request/status])))
    (is (= {:initial-condition true} (:antecedent c)))
    (is (= {:invariant false} (:consequent c)))
    (is (false? (:exhaustive? r)))
    (is (= c (:counterexample (outcome s))))
    (is (= (:examined r) (:examined (outcome s))))))

(deftest step-counterexample-identifies-operation-and-poststate
  (let [s (with-invariant base-source :request/always-open
                          #{:request/status}
                          [:= [:read :request/status] [:literal :open]])
        r (expect-status :counterexample s)
        c (:counterexample r)]
    (is (= :step (:kind c)))
    (is (= :request/claim (:operation-id c)))
    (is (= :request/always-open (:invariant-id c)))
    (is (= :open (get-in c [:prestate :request/status])))
    (is (= :claimed (get-in c [:poststate :request/status])))
    (is (= (get-in c [:prestate :request/owner])
           (get-in c [:poststate :request/owner])))
    (is (= {:all-pre-invariants true :guard true} (:antecedent c)))
    (is (= {:post-invariant false} (:consequent c)))))

(deftest guard-does-not-trigger-from-bad-prestates
  (let [s (-> (as-single base-source (operation :request/unclaim :claimed :open))
              (with-invariant :request/always-open #{:request/status}
                [:= [:read :request/status] [:literal :open]]))
        r (expect-status :bounded-no-counterexample s)]
    (is (= 3 (get-in r [:examined :enabled-transitions-examined])))
    (is (= 3 (get-in r [:examined :initial-states-satisfying])))))

(deftest missing-initial-state-does-not-become-vacuous-success
  (let [s (assoc-in base-source [:catalog :initial-condition]
                    {:readers #{} :expression [:literal false]})
        r (expect-status :unsupported-language s)]
    (is (= :no-initial-states-in-declared-universe (:reason r)))
    (is (false? (:exhaustive? r)))
    (is (= 0 (get-in r [:examined :initial-states-satisfying])))))

(deftest out-of-universe-poststate-is-inconclusive
  (let [s (-> base-source
              (assoc-in [:state-domains :request/status] [:open]))
        r (expect-status :unsupported-language s)]
    (is (= :poststate-outside-declared-universe (:reason r)))
    (is (= :out-of-universe (get-in r [:counterexample :kind])))
    (is (= :claimed (get-in r [:counterexample :poststate :request/status])))
    (is (false? (:exhaustive? r)))))

(deftest exact-case-budget-works-and-one-less-does-not
  (let [s (assoc base-source :max-cases 18)
        r (expect-status :bounded-no-counterexample s)
        smaller (expect-status :budget-exceeded (assoc s :max-cases 17))]
    (is (= 18 (get-in r [:universe :potential-cases])))
    (is (= 18 (get-in smaller [:universe :potential-cases])))
    (is (= :finite-case-budget (:reason smaller)))
    (is (false? (:exhaustive? smaller)))))

(deftest optional-absence-nil-and-value-are-distinct-states
  (let [s (with-invariant base-source :request/owner-present
                          #{:request/owner}
                          [:present [:read :request/owner]])
        r (expect-status :counterexample s)]
    (is (= :base (get-in r [:counterexample :kind])))
    (is (not (contains? (get-in r [:counterexample :state])
                        :request/owner)))
    (is (= 6 (get-in r [:universe :states])))))

(deftest guarded-presence-of-nullable-field-produces-a-witness
  (let [s (with-invariant base-source :request/owner-nonnull
                          #{:request/owner}
                          [:implies [:present [:read :request/owner]]
                           [:not= [:read :request/owner] [:literal nil]]])
        r (expect-status :counterexample s)]
    (is (= :base (get-in r [:counterexample :kind])))
    (is (contains? (get-in r [:counterexample :state]) :request/owner))
    (is (nil? (get-in r [:counterexample :state :request/owner])))))

(deftest optional-absence-preserved-by-framing
  (let [s (with-invariant base-source :request/claimed-must-have-owner
                          #{:request/status :request/owner}
                          [:implies [:= [:read :request/status] [:literal :claimed]]
                           [:present [:read :request/owner]]])
        r (expect-status :counterexample s)]
    (is (= :step (get-in r [:counterexample :kind])))
    (is (= :request/claim (get-in r [:counterexample :operation-id])))
    (is (not (contains? (get-in r [:counterexample :prestate])
                        :request/owner)))
    (is (not (contains? (get-in r [:counterexample :poststate])
                        :request/owner)))))

(deftest input-assignment-enumerates-every-actor
  (let [op (-> (operation :request/claim :open :claimed)
               (assoc-in [:construction :input-types]
                         {:input/actor {:type :keyword}})
               (assoc-in [:construction :reads]
                         #{:request/status :input/actor})
               (assoc-in [:construction :writes]
                         #{:request/status :request/owner})
               (assoc-in [:construction :replacements :request/owner]
                         [:read :input/actor])
               (assoc-in [:construction :frame] #{}))
        s (-> (as-single base-source op)
              (assoc :input-domains {:request/claim
                                     {:input/actor [:person/alice :person/bob]}}
                     :max-cases 24)
              (assoc-in [:state-domains :request/owner]
                        [oracle/absent nil :person/alice :person/bob]))
        r (expect-status :bounded-no-counterexample s)]
    (is (= 8 (get-in r [:universe :states])))
    (is (= {:request/claim 2}
           (get-in r [:universe :input-counts-by-operation])))
    (is (= 24 (get-in r [:universe :potential-cases])))
    (is (= 8 (get-in r [:examined :enabled-transitions-examined])))))

(deftest compound-booleans-short-circuit-and-set-membership
  (let [s (-> base-source
              (assoc-in [:catalog :initial-condition]
                        {:readers #{:request/status}
                         :expression [:and [:literal true]
                                      [:not [:literal false]]
                                      [:or [:literal false]
                                       [:= [:read :request/status]
                                        [:literal :open]]]]})
              (assoc-in [:catalog :operations 0 :construction :guard]
                        {:readers #{:request/status}
                         :expression [:and
                                      [:in [:read :request/status]
                                       [:literal #{:open}]]
                                      [:not [:literal false]]]}))
        r (expect-status :bounded-no-counterexample s)]
    (is (= 3 (get-in r [:examined :initial-states-satisfying])))
    (is (= 6 (get-in r [:examined :enabled-transitions-examined])))))

(deftest strict-ordering-and-nonnegative-base
  (let [t {:request/status status-type
           :request/version {:type :integer}}
        op (-> (operation :request/claim :open :claimed t)
               (assoc-in [:construction :writes]
                         #{:request/status :request/version})
               (assoc-in [:construction :replacements :request/version]
                         [:literal 1])
               (assoc-in [:construction :frame] #{}))
        s (-> (as-single base-source op)
              (assoc :state-domains {:request/status [:open :claimed]
                                     :request/version [0 1 2]}
                     :max-cases 20)
              (with-invariant :request/bounded-version #{:request/version}
                [:and [:>= [:read :request/version] [:literal 0]]
                 [:<= [:read :request/version] [:literal 2]]]))
        r (expect-status :bounded-no-counterexample s)]
    (is (= 6 (get-in r [:universe :states])))
    (is (= 3 (get-in r [:examined :enabled-transitions-examined])))))

(deftest numeric-step-violation-does-not-pretend-success
  (let [t {:request/status status-type
           :request/version {:type :integer}}
        op (-> (operation :request/claim :open :claimed t)
               (assoc-in [:construction :writes]
                         #{:request/status :request/version})
               (assoc-in [:construction :replacements :request/version]
                         [:literal 1])
               (assoc-in [:construction :frame] #{}))
        s (-> (as-single base-source op)
              (assoc :state-domains {:request/status [:open :claimed]
                                     :request/version [0 1]}
                     :max-cases 12)
              (assoc-in [:catalog :initial-condition]
                        {:readers #{:request/status :request/version}
                         :expression [:and
                                      [:= [:read :request/status] [:literal :open]]
                                      [:= [:read :request/version] [:literal 0]]]})
              (with-invariant :request/version-must-be-zero
                #{:request/version}
                [:= [:read :request/version] [:literal 0]]))
        r (expect-status :counterexample s)]
    (is (= :step (get-in r [:counterexample :kind])))
    (is (= 0 (get-in r [:counterexample :prestate :request/version])))
    (is (= 1 (get-in r [:counterexample :poststate :request/version])))))

(deftest four-operation-real-catalog-remains-bounded
  (require 'gesso.assurance.proof.induction-acceptance-test)
  (let [c (deref (ns-resolve 'gesso.assurance.proof.induction-acceptance-test
                             'catalog))
        s {:catalog c
           :state-domains {:request/status [:open :claimed :completed :cancelled]
                           :request/version [-1 0 1]
                           :request/priority [:low :normal :urgent]
                           :request/owner [nil :person/alice]}
           :input-domains (zipmap (:declared-operation-ids c) (repeat {}))
           :max-cases 2000}
        r (expect-status :bounded-no-counterexample s)]
    (is (= 72 (get-in r [:universe :states])))
    (is (= 360 (get-in r [:universe :potential-cases])))
    (is (= 72 (get-in r [:examined :initial-states-examined])))
    (is (= 288 (get-in r [:examined :transition-cases-examined])))))

(deftest malformed-source-and-domains-fail-closed
  (doseq [[reason s]
          [[:wrong-keys (assoc base-source :authority :yes)]
           [:expected-plain-map (into (sorted-map) base-source)]
           [:metadata-not-allowed (with-meta base-source {:trusted? true})]
           [:invalid-case-budget (assoc base-source :max-cases 0)]
           [:invalid-case-budget (assoc base-source :max-cases 100001)]
           [:wrong-keys (assoc base-source :input-domains {})]
           [:wrong-keys (assoc base-source :state-domains {})]
           [:invalid-domain-vector
            (assoc-in base-source [:state-domains :request/status] [])]
           [:duplicate-domain-value
            (assoc-in base-source [:state-domains :request/status]
                      [:open :claimed :open])]
           [:ill-typed-domain-value
            (assoc-in base-source [:state-domains :request/status]
                      [:open :person/alice])]
           [:absence-for-required-field
            (assoc-in base-source [:state-domains :request/status]
                      [oracle/absent :open :claimed])]]]
    (testing (str "Source failure " reason)
      (let [r (expect-status :invalid-source s)]
        (is (= reason (:reason r)) (pr-str r))
        (is (false? (:exhaustive? r)))))))

(deftest malformed-constructions-and-operation-coverage-fail-closed
  (doseq [[reason s]
          [[:operation-coverage-mismatch
            (assoc-in base-source [:catalog :declared-operation-ids]
                      #{:request/claim})]
           [:operation-subject-mismatch
            (assoc-in base-source [:catalog :operations 1 :id] :request/claim)]
           [:bad-transition-footprint
            (assoc-in base-source [:catalog :operations 0 :construction :frame]
                      #{})]
           [:bad-transition-footprint
            (assoc-in base-source [:catalog :operations 0 :construction :writes]
                      #{:request/status :request/owner})]
           [:read-footprint-mismatch
            (assoc-in base-source [:catalog :operations 0 :construction :reads]
                      #{})]
           [:ill-typed-assignment
            (assoc-in base-source [:catalog :operations 0 :construction
                                   :replacements :request/status] [:literal 7])]
           [:operation-subject-mismatch
            (assoc-in base-source [:catalog :operations 0 :construction
                                   :subject :id] :request/unclaim)]
           [:state-type-disagreement
            (assoc-in base-source [:catalog :operations 1 :construction
                                   :state-types :request/owner :type] :string)]]]
    (testing (str "Catalog failure " reason)
      (let [r (expect-status :invalid-source s)]
        (is (= reason (:reason r)) (pr-str r))))))

(deftest unsupported-language-cannot-be-accepted-by-accident
  (doseq [[reason s]
          [[:unsupported-expression-operator
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{} :expression [:arbitrary [:literal true]]})]
           [:incompatible-equality
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{}
                       :expression [:= [:literal :open] [:literal 3]]})]
           [:heterogeneous-set-type
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{}
                       :expression [:in [:literal :open]
                                    [:literal #{:open 3}]]})]
           [:unguarded-optional-read
            (with-invariant base-source :request/owner-unguarded
              #{:request/owner}
              [:= [:read :request/owner] [:literal :person/alice]])]
           [:enum-literal-outside-domain
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{:request/status}
                       :expression [:= [:read :request/status]
                                    [:literal :not-a-status]]})]
           [:incompatible-ordering
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{:request/status}
                       :expression [:< [:read :request/status]
                                    [:literal 2]]})]
           [:expression-arity
            (assoc-in base-source [:catalog :initial-condition]
                      {:readers #{} :expression [:= [:literal :open]]})]]]
    (testing (str "Unsupported semantics " reason)
      (let [r (expect-status (if (= reason :expression-arity)
                               :invalid-source :unsupported-language) s)]
        (is (= reason (:reason r)) (pr-str r))
        (is (false? (:exhaustive? r)))))))

(deftest no-implicit-closure-or-result-as-authority
  (let [source-report (outcome base-source)
        forged (assoc source-report :certificate :certified
                       :checker-soundness :proved)
        r (expect-status :invalid-source forged)]
    (is (= :wrong-keys (:reason r)))
    (is (= :none (:authority source-report)))
    (is (= :not-issued (:certificate source-report)))
    (is (= :not-established (:runtime-enforcement source-report)))))

(deftest optional-short-circuit-prevents-undefined-reads
  (let [s (with-invariant base-source :request/owner-not-null
                          #{:request/owner}
                          [:or [:not [:present [:read :request/owner]]]
                           [:not= [:read :request/owner] [:literal nil]]])
        r (expect-status :counterexample s)]
    (is (= :base (get-in r [:counterexample :kind])))
    (is (contains? (get-in r [:counterexample :state]) :request/owner))
    (is (nil? (get-in r [:counterexample :state :request/owner])))))

(deftest guarded-and-uses-presence-before-reading-optional
  (let [s (with-invariant base-source :request/owner-never-null
                          #{:request/owner}
                          [:not [:and
                                 [:present [:read :request/owner]]
                                 [:= [:read :request/owner] [:literal nil]]]])
        r (expect-status :counterexample s)]
    (is (= :base (get-in r [:counterexample :kind])))
    (is (contains? (get-in r [:counterexample :state]) :request/owner))
    (is (nil? (get-in r [:counterexample :state :request/owner])))))

(deftest nested-source-metadata-cannot-claim-trust
  (doseq [[reason s]
          [[:metadata-not-allowed
            (assoc-in base-source [:catalog :invariants :request/status-domain]
                      (with-meta valid-status {:issued-proof true}))]
           [:metadata-not-allowed
            (assoc-in base-source [:catalog :operations]
                      (with-meta (:operations base-catalog) {:trusted true}))]
           [:metadata-not-allowed
            (assoc-in base-source [:state-domains :request/owner]
                      (with-meta [oracle/absent nil :person/alice]
                        {:trusted true}))]
           [:metadata-not-allowed
            (assoc-in base-source [:catalog :operations 0 :construction
                                   :replacements]
                      (with-meta {:request/status [:literal :claimed]}
                        {:proved true}))]]]
    (let [r (expect-status :invalid-source s)]
      (is (= reason (:reason r)) (pr-str r)))))

(deftest original-catalog-closure-is-exact
  (doseq [[reason s]
          [[:operation-coverage-mismatch
            (assoc-in base-source [:catalog :operations 1]
                      (operation :request/claim :claimed :open))]
           [:operation-coverage-mismatch
            (assoc-in base-source [:catalog :declared-operation-ids]
                      #{:request/claim :request/unclaim :request/extra})]
           [:wrong-keys
            (assoc-in base-source [:catalog :operations 0 :construction]
                      (assoc (get-in base-source
                                     [:catalog :operations 0 :construction])
                             :authorization :asserted))]
           [:rule-reader-footprint-mismatch
            (assoc-in base-source [:catalog :operations 0 :construction
                                   :guard :readers] #{})]
           [:read-footprint-mismatch
            (assoc-in base-source [:catalog :operations 0 :construction :reads]
                      #{:request/status :request/owner})]]]
    (testing (str "Closure failure " reason)
      (let [r (expect-status :invalid-source s)]
        (is (= reason (:reason r)) (pr-str r))))))

(deftest input-types-and-domain-coverage-must-agree
  (let [op (-> (operation :request/claim :open :claimed)
               (assoc-in [:construction :input-types]
                         {:input/actor {:type :keyword}}))
        s (as-single base-source op)]
    (doseq [[reason changed]
            [[:wrong-keys s]
             [:ill-typed-domain-value
              (assoc s :input-domains {:request/claim {:input/actor [42]}})]
             [:duplicate-domain-value
              (assoc s :input-domains
                     {:request/claim {:input/actor [:person/alice :person/alice]}})]]]
      (testing (str "Input failure " reason)
        (let [r (expect-status :invalid-source changed)]
          (is (= reason (:reason r)) (pr-str r)))))))

(deftest unsupported-assignments-do-not-run
  (let [op (-> (operation :request/claim :open :claimed)
               (assoc-in [:construction :replacements :request/status]
                         [:custom-computation :unsafe]))
        r (expect-status :unsupported-language (as-single base-source op))]
    (is (= :unsupported-assignment (:reason r)))
    (is (false? (:exhaustive? r)))))

(deftest expression-depth-limit-is-not-a-success
  (let [expr (nth (iterate (fn [e] [:not e]) [:literal true]) 34)
        s (assoc-in base-source [:catalog :initial-condition]
                    {:readers #{} :expression expr})
        r (expect-status :unsupported-language s)]
    (is (= :expression-too-deep (:reason r)))
    (is (false? (:exhaustive? r)))))

(deftest adding-authority-to-a-copied-counterexample-does-not-admit-it
  (let [s (with-invariant base-source :request/must-be-claimed
                          #{:request/status}
                          [:= [:read :request/status] [:literal :claimed]])
        untrusted (outcome s)
        altered (-> untrusted
                    (assoc :certificate :issued)
                    (assoc :status :bounded-no-counterexample)
                    (assoc :authority :trusted))]
    (is (= :counterexample (:status untrusted)))
    (is (= :invalid-source (:status (outcome altered))))
    (is (= :wrong-keys (:reason (outcome altered))))))

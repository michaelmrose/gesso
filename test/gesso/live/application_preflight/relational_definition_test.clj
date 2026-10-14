(ns gesso.live.application-preflight.relational-definition-test
  "v845 regressions for source-owned relational construction and explanation.

   Preserve earlier admission, comparator, startup, and explanation tests.
   Verify that actual visited-state matching from v842 is retained by the
   handle-only semantic audit without reinterpreting policy declarations.
   Matching counts remain bounded diagnostic evidence, not policy completeness,
   unbounded proof, or all-executions production correspondence."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]))

(def ^:private version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private counter-key [:counters :one])
(def ^:private initial-state
  {counter-key {:xt/id :one
                :revision 0
                :created-at :t0
                :updated-at :t0
                :n 0
                :parent nil
                :active? true}})

(def ^:private parent-rule
  {:id :counter/parent-active
   :source-kind :counters
   :source-where {}
   :reference-attr :parent
   :allow-unset? true
   :target-kind :counters
   :target-where {:active? true}})

(defn- family
  ([id] (family id false))
  ([id invalid-parent?]
   {:id id
    :principals [:helper/a]
    :arguments [{:delta 1}]
    :enabled? (fn [_ _ _] true)
    :prepare (fn [state _ _]
               (let [before (get state counter-key)
                     after (cond-> (-> before
                                       (update :revision inc)
                                       (update :n inc)
                                       (assoc :updated-at :t1))
                             invalid-parent? (assoc :parent :missing))]
                 {:commands [(command/update-command
                              :counters :increment before after version-keys)]
                  :guards []
                  :emit false}))}))

(defn- relational-model
  ([] (relational-model false))
  ([invalid-parent?]
   {:initial-state initial-state
    :families [(family :request/claim invalid-parent?)
               (family :request/reassign)
               (family :request/cancel)]
    :declarations [parent-rule]
    :options {:max-depth 2 :max-transitions 50}}))

(defn- definition
  [application-options]
  {:application-options application-options
   :relational-model (relational-model)})

(defn- error-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo error
      (ex-data error))))

(defn- error-kind [f]
  (:error/kind (error-data f)))

(deftest admitted-construction-has-redacted-inspectable-scope
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (sut/inspect handle)]
        (is (not (map? handle)) "The authority is not a caller-editable map")
        (is (= #{:status :registered-operation-ids
                 :relational-rule-ids :assurance-scope}
               (set (keys report))))
        (is (= :admitted-finite-relational-construction (:status report)))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (:registered-operation-ids report))))
        (is (= [:counter/parent-active] (:relational-rule-ids report)))
        (is (= sut/assurance-scope (:assurance-scope report)))
        (is (contains? (get-in report [:assurance-scope :not-established])
                       :all-executions-correspond-to-model))
        (is (not (.contains (pr-str report) ":initial-state")))
        (is (not (.contains (pr-str report) ":prepare")))
        (is (= report (sut/inspect handle)))))))

(deftest edited-reports-and-recreated-objects-have-no-authority
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (sut/inspect handle)
            forged (assoc report :status :admitted-finite-relational-construction)]
        (doseq [candidate [nil (Object.) report forged {}]]
          (is (= :unknown-construction
                 (error-kind #(sut/inspect candidate))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! candidate nil)))))))))

(deftest declaration-must-be-closed-before-building-an-application
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)]
        (with-redefs [application/require-application-assembly!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Unexpected construction" {})))]
          (doseq [bad [nil {} []
                       {:application-options options}
                       {:relational-model (relational-model)}
                       (assoc (definition options) :forged-certificate true)]]
            (is (= :invalid-definition
                   (error-kind #(sut/construct! bad)))))
          (is (zero? @calls)))))))

(deftest model-mismatch-and-invalid-transitions-fail-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)]
        (is (= :operation-identity-coverage-mismatch
               (error-kind
                #(sut/construct!
                  (update-in base [:relational-model :families] pop)))))
        (is (= :invalid-declaration
               (error-kind
                #(sut/construct!
                  (assoc-in base [:relational-model :declarations 0 :holds?]
                            (constantly true))))))
        (is (= :exploration-budget-exhausted
               (error-kind
                #(sut/construct!
                  (assoc-in base [:relational-model :options :max-transitions]
                            1)))))
        (let [bad (assoc base :relational-model (relational-model true))
              error (error-data #(sut/construct! bad))]
          (is (= :finite-relational-invariant-violation (:error/kind error)))
          (is (= :counterexample (get-in error [:error/details :status])))
          (is (= :counter/parent-active
                 (get-in error [:error/details :counterexamples 0 :rule-id])))
          (is (not (contains? (:error/details error) :initial-state))))))))

(deftest comparison-refuses-unissued-physical-evidence
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))]
        (doseq [attempt [nil (Object.)
                         {:operation :request/claim
                          :principal :helper/a
                          :arguments {:delta 1}}]]
          (is (= :unrecognized-executor-bound-evidence
                 (error-kind #(sut/compare! handle attempt)))))))))

(deftest both-startup-arities-reuse-the-exact-admitted-assembly
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [actual-require application/require-application-assembly!
            checked (atom nil)
            started (atom [])
            initial-system {:fixture/system true}
            modules-var #'fixture/standard-operation-acquisition
            order [application/application-handler-module-id]]
        (with-redefs [application/require-application-assembly!
                      (fn [opts]
                        (let [assembly (actual-require opts)]
                          (reset! checked assembly)
                          assembly))
                      application/start-biff-application!
                      (fn [& args]
                        (swap! started conj args)
                        :started)]
          (let [handle (sut/construct! (definition options))]
            (is (application/application-assembly? @checked))
            (is (= :started (sut/start-biff! handle modules-var order)))
            (is (= :started (sut/start-biff!
                             handle initial-system modules-var order)))
            (is (= 2 (count @started)))
            (is (every? #(identical? @checked (first %)) @started))
            (is (= [@checked modules-var order] (first @started)))
            (is (= [@checked initial-system modules-var order]
                   (second @started)))))))))

(deftest stale-artifact-revokes-inspection-comparison-and-startup
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))
            starts (atom 0)]
        (is (= :admitted-finite-relational-construction
               (:status (sut/inspect handle))))
        (spit artifact-path "modified after construction\n")
        (with-redefs [application/start-biff-application!
                      (fn [& _] (swap! starts inc) :unexpected-start)]
          (is (= :stale-application-assembly
                 (error-kind #(sut/inspect handle))))
          (is (= :stale-application-assembly
                 (error-kind #(sut/compare! handle nil))))
          (is (= :stale-application-assembly
                 (error-kind #(sut/start-biff! handle nil []))))
          (is (zero? @starts)))))))


;; The following tests exercise the v838 explanation as a PUBLIC capability,
;; rather than calling semantic-audit implementation helpers or reconstructing
;; a success report from caller-editable metadata.

(def ^:private unexercised-parent-rule
  (assoc parent-rule
         :id :counter/parent-at-ninety-nine
         :source-where {:n 99}))

(defn- relational-audit
  [handle]
  (:semantic-audit (sut/explain handle)))

(deftest explanation-retains-source-owned-rules-without-policy-completeness
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            explained (sut/explain handle)
            audit (:semantic-audit explained)
            catalog (:business-rule-catalog audit)
            rule (first (:rules catalog))
            coverage (:assurance-coverage audit)
            relation-evidence (last coverage)]
        (is (= :application-runtime-backbone-preflight-closed
               (:guarantee explained)))
        (is (= :partial-source-owned-relational-catalog (:status catalog)))
        (is (= 1 (:declared-count catalog)))
        (is (= :unknown (:total-business-rule-count catalog)))
        (is (= :not-established (:completeness catalog)))
        (is (= [(assoc parent-rule :initial-model-coverage
                       {:rule-id :counter/parent-active
                        :source-kind :counters
                        :target-kind :counters
                        :initially-matching-source-count 1
                        :initially-exercised? true
                        :initially-holds? true})]
               (:rules catalog)))
        (is (= :counter/parent-active (:id rule)))
        (is (= :holds-in-explored-model
               (get-in audit [:relational-assurance :status])))
        (is (= :current-relational-definition
               (get-in audit [:relational-assurance :source])))
        (is (= #{:request/claim :request/reassign :request/cancel}
               (set (get-in audit [:relational-assurance
                                   :registered-operation-ids]))))
        (is (= [:counter/parent-active]
               (get-in audit [:relational-assurance :relational-rule-ids])))
        (is (= [] (get-in audit [:relational-assurance
                                :initially-unexercised-rule-ids])))
        (is (= sut/assurance-scope
               (get-in audit [:relational-assurance :assurance-scope])))
        (is (= [:relational-rule :counter/parent-active
                :finite-invariant-preservation]
               (:subject relation-evidence)))
        (is (= :bounded-model-checked (:class relation-evidence)))
        (is (= :admitted-source-interpreted-relational-model
               (:evidence relation-evidence)))
        (is (= :declared-finite-operation-family-reachable-states
               (:scope relation-evidence)))
        (is (true? (:initially-exercised? relation-evidence)))
        (is (= #{:unbounded-invariant-preservation
                 :production-implementation-correspondence
                 :business-policy-completeness}
               (:not-established relation-evidence)))
        (is (= 7 (count coverage)))
        (is (= explained (sut/explain handle)))))))

(deftest explanation-preserves-canonical-audit-unknowns-and-trusted-boundaries
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      ;; Capture the exact canonical explanation that v838 extends. Checking
      ;; source-identical fields avoids substituting a second, forged report.
      (let [original-explain application/explain
            canonical (atom nil)]
        (with-redefs [application/explain
                      (fn [assembly]
                        (let [result (original-explain assembly)]
                          (reset! canonical result)
                          result))]
          (let [handle (sut/construct! (definition options))
                explained (sut/explain handle)
                audit (:semantic-audit explained)
                base (:semantic-audit @canonical)
                adjusted (-> base
                             (dissoc :business-rule-catalog
                                     :assurance-coverage))
                retained (dissoc audit
                                 :business-rule-catalog
                                 :assurance-coverage
                                 :relational-assurance)]
            (is (some? @canonical))
            (is (= (dissoc @canonical :semantic-audit)
                   (dissoc explained :semantic-audit)))
            (is (= adjusted retained))
            (is (= (:assurance-coverage base)
                   (subvec (:assurance-coverage audit)
                           0 (count (:assurance-coverage base)))))
            (is (= :not-inspected (get-in audit [:rule-dependencies :status])))
            (is (= :not-inspected
                   (get-in audit [:authorization-disclosure-view :status])))
            (is (= :not-inspected
                   (get-in audit [:lifecycle-transition-view :status])))
            (is (false? (get-in audit [:inspection-scope
                                      :complete-business-policy-inventory?])))
            (is (false? (get-in audit [:inspection-scope
                                      :zero-undetected-policy-escapes-established?])))
            (is (= 3 (count (:opaque-model-boundaries audit))))
            (is (every? #(= :trusted (:class %))
                        (:opaque-model-boundaries audit)))
            (is (= :opaque-host-implementation
                   (get-in audit [:operation-cards :request/claim
                                  :model-policy-inspection])))
            (is (contains? (:not-established sut/assurance-scope)
                           :all-executions-correspond-to-model))))))))

(deftest unexercised-rule-is-visible-but-not-treated-as-exercised
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [definition (update-in (definition options)
                                  [:relational-model :declarations]
                                  conj unexercised-parent-rule)
            handle (sut/construct! definition)
            audit (relational-audit handle)
            catalog (:business-rule-catalog audit)
            by-id (into {} (map (juxt :id identity)) (:rules catalog))
            evidence (->> (:assurance-coverage audit)
                          (filter #(= :bounded-model-checked (:class %)))
                          (into {} (map (fn [entry]
                                          [(second (:subject entry)) entry]))))]
        (is (= 2 (:declared-count catalog)))
        (is (= [:counter/parent-active :counter/parent-at-ninety-nine]
               (mapv :id (:rules catalog))))
        (is (= :unknown (:total-business-rule-count catalog)))
        (is (= :not-established (:completeness catalog)))
        (is (= [:counter/parent-at-ninety-nine]
               (get-in audit [:relational-assurance
                              :initially-unexercised-rule-ids])))
        (is (= 1 (get-in by-id [:counter/parent-active
                               :initial-model-coverage
                               :initially-matching-source-count])))
        (is (true? (get-in by-id [:counter/parent-active
                                 :initial-model-coverage
                                 :initially-exercised?])))
        (is (= 0 (get-in by-id [:counter/parent-at-ninety-nine
                               :initial-model-coverage
                               :initially-matching-source-count])))
        (is (false? (get-in by-id [:counter/parent-at-ninety-nine
                                  :initial-model-coverage
                                  :initially-exercised?])))
        (is (true? (get-in by-id [:counter/parent-at-ninety-nine
                                 :initial-model-coverage
                                 :initially-holds?])))
        (is (= #{:counter/parent-active :counter/parent-at-ninety-nine}
               (set (keys evidence))))
        (is (true? (get-in evidence [:counter/parent-active
                                    :initially-exercised?])))
        (is (false? (get-in evidence [:counter/parent-at-ninety-nine
                                     :initially-exercised?])))
        (is (= (get-in evidence [:counter/parent-active :not-established])
               (get-in evidence [:counter/parent-at-ninety-nine
                                 :not-established])))))))

(deftest explanation-leaks-neither-modeled-state-nor-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v839-opaque-model-state-token"
            prepared (atom 0)
            definition (-> (definition options)
                           (assoc-in [:relational-model :initial-state counter-key
                                      :private-state-token] secret)
                           (update-in [:relational-model :families]
                                      (fn [families]
                                        (mapv (fn [family]
                                                (update family :prepare
                                                        (fn [prepare]
                                                          (fn [& args]
                                                            (swap! prepared inc)
                                                            (apply prepare args)))))
                                              families))))
            handle (sut/construct! definition)
            at-admission @prepared
            explained (sut/explain handle)
            printed (pr-str explained)]
        (is (pos? at-admission))
        (is (= at-admission @prepared)
            "Audit must not rerun the model's operation callbacks")
        (is (= explained (sut/explain handle)))
        (is (= at-admission @prepared))
        (is (not (.contains printed secret)))
        (is (not (.contains printed ":initial-state")))
        (is (not (.contains printed ":private-state-token")))
        (is (not (.contains printed "#object[")))
        (is (not (contains? (:semantic-audit explained) :initial-state)))
        (is (not (contains? (:semantic-audit explained) :families)))))))

(deftest explanation-metadata-does-not-act-as-an-admission-handle
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            explanation (sut/explain handle)
            tampered (-> explanation
                         (assoc :guarantee :whole-application-certified)
                         (assoc-in [:semantic-audit :business-rule-catalog
                                    :completeness] :proved))]
        (doseq [candidate [nil (Object.) explanation tampered {}]]
          (testing (str "explanation cannot grant authority: "
                        (if (map? candidate) :map (type candidate)))
            (is (= :unknown-construction
                   (error-kind #(sut/explain candidate))))
            (is (= :unknown-construction
                   (error-kind #(sut/inspect candidate))))))
        (is (= explanation (sut/explain handle)))))))

(deftest explanation-rechecks-physical-artifact-currentness
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))]
        (is (= :partial-source-owned-relational-catalog
               (get-in (sut/explain handle)
                       [:semantic-audit :business-rule-catalog :status])))
        (spit artifact-path "v839 replaced after admission\n")
        (is (= :stale-application-assembly
               (error-kind #(sut/explain handle))))))))


;; v840 adds the actual finite search boundary to the explanation, rather
;; than only displaying the very broad label :bounded-model-checked.  These
;; facts are captured from the successful admission; reading the report does
;; not run new modeled transitions and cannot promote it to a certificate.

(defn- finite-exploration [handle]
  (get-in (sut/explain handle)
          [:semantic-audit :relational-assurance :finite-exploration]))

(deftest explanation-identifies-exact-bounded-state-space
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            summary (finite-exploration handle)]
        (is (= #{:kind :status :max-depth :max-transitions :reached-depth
                 :attempted-transitions :visited-state-occurrences
                 :frontier-exhausted? :depth-limited?
                 :modeled-operation-instance-count :domain-completeness
                 :production-correspondence}
               (set (keys summary))))
        (is (= :bounded-reachable-serial-operations (:kind summary)))
        (is (= :fully-explored-within-declared-bounds (:status summary)))
        (is (= 2 (:max-depth summary)))
        (is (= 50 (:max-transitions summary)))
        (is (= 2 (:reached-depth summary)))
        (is (= 12 (:attempted-transitions summary)))
        (is (= 13 (:visited-state-occurrences summary)))
        (is (= 3 (:modeled-operation-instance-count summary)))
        (is (true? (:depth-limited? summary)))
        (is (false? (:frontier-exhausted? summary)))
        (is (= :not-established (:domain-completeness summary)))
        (is (= :not-established (:production-correspondence summary)))
        ;; Returned maps are inert snapshots, not mutable admission evidence.
        (is (not= (assoc summary :status :universally-proved)
                  (finite-exploration handle)))
        (is (= summary (finite-exploration handle)))
        (is (= :unknown-construction
               (error-kind #(sut/explain (sut/explain handle)))))))))

(deftest explanation-distinguishes-depth-limited-and-frontier-exhausted
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [short (assoc-in (definition options)
                            [:relational-model :options :max-depth] 1)
            exhaustive (update-in short [:relational-model :families]
                                  (fn [families]
                                    (mapv #(assoc % :enabled?
                                                  (fn [& _] false)) families)))
            short-summary (finite-exploration (sut/construct! short))
            exhausted-summary (finite-exploration
                               (sut/construct! exhaustive))]
        (is (= 1 (:max-depth short-summary)))
        (is (= 1 (:reached-depth short-summary)))
        (is (= 3 (:attempted-transitions short-summary)))
        (is (= 4 (:visited-state-occurrences short-summary)))
        (is (true? (:depth-limited? short-summary)))
        (is (false? (:frontier-exhausted? short-summary)))
        (is (= 0 (:attempted-transitions exhausted-summary)))
        (is (= 1 (:visited-state-occurrences exhausted-summary)))
        (is (true? (:frontier-exhausted? exhausted-summary)))
        (is (false? (:depth-limited? exhausted-summary)))
        (is (= 3 (:modeled-operation-instance-count exhausted-summary)))
        (is (= :not-established (:domain-completeness exhausted-summary)))))))

(deftest finite-exploration-report-is-redacted-and-does-not-run-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v841-sensitive-model-marker"
            invoked (atom 0)
            declared (-> (definition options)
                         (assoc-in [:relational-model :initial-state counter-key
                                    :unpublished-secret] secret)
                         (update-in [:relational-model :families]
                                    (fn [families]
                                      (mapv (fn [family]
                                              (update family :prepare
                                                      (fn [prepare]
                                                        (fn [& args]
                                                          (swap! invoked inc)
                                                          (apply prepare args)))))
                                            families))))
            handle (sut/construct! declared)
            initial-count @invoked
            explained (sut/explain handle)
            summary (finite-exploration handle)]
        (is (pos? initial-count))
        (is (= initial-count @invoked))
        (is (not (.contains (pr-str explained) secret)))
        (is (not (.contains (pr-str summary) ":initial-state")))
        (is (not (.contains (pr-str summary) ":prepare")))
        (is (not (.contains (pr-str summary) "#object[")))
        (is (= 12 (:attempted-transitions summary)))
        (is (= initial-count @invoked))
        (is (= :unknown-construction
               (error-kind #(sut/start-biff! summary nil []))))))))


;; v845: v844's explanation preserves the original declared-rule map shape
;; while joining the admitted v842 visited-state observation by :rule-id.
;; These counters record state *occurrences*, including repeated states, not
;; unique database states or proof that every real-world input was exercised.

(defn- reachable-model-facts [handle]
  (get-in (relational-audit handle)
          [:business-rule-catalog :reachable-state-rule-coverage]))

(deftest explanation-reports-actual-reachable-state-observations
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            audit (relational-audit handle)
            catalog (:business-rule-catalog audit)
            facts (reachable-model-facts handle)
            fact (first facts)
            summary (finite-exploration handle)
            evidence (last (:assurance-coverage audit))]
        (is (= :holds-in-explored-model
               (get-in audit [:relational-assurance :status])))
        (is (= [:counter/parent-active] (mapv :rule-id facts)))
        (is (= (:visited-state-occurrences summary)
               (:evaluated-state-occurrences fact)))
        (is (= 13 (:evaluated-state-occurrences fact)))
        (is (= 13 (:state-occurrences-with-matching-sources fact)))
        (is (= 13 (:matching-source-occurrences fact)))
        (is (true? (:ever-matched-source-in-explored-states? fact)))
        (is (= [] (get-in audit [:relational-assurance
                                :became-exercised-during-exploration-rule-ids])))
        (is (= [] (get-in audit [:relational-assurance
                                :never-matched-in-explored-states-rule-ids])))
        (is (true? (:ever-matched-source-in-explored-states? evidence)))
        (is (= :bounded-model-checked (:class evidence)))
        ;; The catalog still presents existing declarations and their initial
        ;; observations with no surprise change to callers of v838.
        (is (not (contains? (first (:rules catalog))
                            :reachable-model-coverage)))
        (is (= #{:counter/parent-active}
               (set (map :id (:rules catalog)))))
        (is (= :not-established (:completeness catalog)))
        (is (contains? (:not-established sut/assurance-scope)
                       :all-executions-correspond-to-model))))))

(deftest initially-vacuous-rule-can-become-exercised-only-after-a-transition
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [later (assoc parent-rule :id :counter/after-one
                         :source-where {:n 1})
            never (assoc parent-rule :id :counter/never-ninety-nine
                         :source-where {:n 99})
            changed (assoc-in (definition options)
                              [:relational-model :declarations]
                              [later never])
            handle (sut/construct! changed)
            audit (relational-audit handle)
            catalog (:business-rule-catalog audit)
            facts (into {} (map (juxt :rule-id identity))
                        (:reachable-state-rule-coverage catalog))
            evidence (into {} (map (fn [entry]
                                     [(second (:subject entry)) entry]))
                           (filter #(= :bounded-model-checked (:class %))
                                   (:assurance-coverage audit)))]
        (is (= 2 (:declared-count catalog)))
        (is (= [:counter/after-one :counter/never-ninety-nine]
               (mapv :id (:rules catalog))))
        (is (= [:counter/after-one :counter/never-ninety-nine]
               (get-in audit [:relational-assurance
                              :initially-unexercised-rule-ids])))
        (is (= [:counter/after-one]
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (= [:counter/never-ninety-nine]
               (get-in audit [:relational-assurance
                              :never-matched-in-explored-states-rule-ids])))
        (is (= 0 (get-in catalog [:rules 0 :initial-model-coverage
                                 :initially-matching-source-count])))
        (is (= 13 (get-in facts [:counter/after-one
                                :evaluated-state-occurrences])))
        (is (= 3 (get-in facts [:counter/after-one
                               :state-occurrences-with-matching-sources])))
        (is (= 3 (get-in facts [:counter/after-one
                               :matching-source-occurrences])))
        (is (true? (get-in facts [:counter/after-one
                                 :ever-matched-source-in-explored-states?])))
        (is (= 13 (get-in facts [:counter/never-ninety-nine
                                :evaluated-state-occurrences])))
        (is (zero? (get-in facts [:counter/never-ninety-nine
                                 :matching-source-occurrences])))
        (is (false? (get-in facts [:counter/never-ninety-nine
                                  :ever-matched-source-in-explored-states?])))
        (is (true? (get-in evidence [:counter/after-one
                                    :ever-matched-source-in-explored-states?])))
        (is (false? (get-in evidence [:counter/never-ninety-nine
                                     :ever-matched-source-in-explored-states?])))
        (is (= :not-established (:completeness catalog)))))))

(deftest explanation-does-not-extrapolate-beyond-admitted-depth
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [after-two (assoc parent-rule :id :counter/after-two
                             :source-where {:n 2})
            short (-> (definition options)
                      (assoc-in [:relational-model :declarations] [after-two])
                      (assoc-in [:relational-model :options :max-depth] 1))
            handle (sut/construct! short)
            audit (relational-audit handle)
            facts (first (reachable-model-facts handle))
            summary (finite-exploration handle)]
        (is (= 1 (:reached-depth summary)))
        (is (:depth-limited? summary))
        (is (= 4 (:visited-state-occurrences summary)))
        (is (= 4 (:evaluated-state-occurrences facts)))
        (is (= 0 (:state-occurrences-with-matching-sources facts)))
        (is (= 0 (:matching-source-occurrences facts)))
        (is (false? (:ever-matched-source-in-explored-states? facts)))
        (is (= [:counter/after-two]
               (get-in audit [:relational-assurance
                              :never-matched-in-explored-states-rule-ids])))
        (is (= []
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (= :not-established
               (get-in audit [:business-rule-catalog :completeness])))))))

(deftest reachable-explanation-does-not-rerun-model-or-leak-state
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v845-model-state-not-public"
            calls (atom 0)
            declared (-> (definition options)
                         (assoc-in [:relational-model :initial-state counter-key
                                    :hidden-attribute] secret)
                         (update-in [:relational-model :families]
                                    (fn [families]
                                      (mapv (fn [f]
                                              (update f :prepare
                                                      (fn [prepare]
                                                        (fn [& args]
                                                          (swap! calls inc)
                                                          (apply prepare args)))))
                                            families))))
            handle (sut/construct! declared)
            after-construction @calls
            report (sut/explain handle)
            catalog (get-in report [:semantic-audit :business-rule-catalog])
            facts (:reachable-state-rule-coverage catalog)]
        (is (pos? after-construction))
        (is (seq facts))
        (is (= 13 (:evaluated-state-occurrences (first facts))))
        (is (= after-construction @calls))
        (is (= report (sut/explain handle)))
        (is (= after-construction @calls))
        (is (not (.contains (pr-str report) secret)))
        (is (not (.contains (pr-str facts) "#object[")))
        (is (not (.contains (pr-str facts) ":prepare")))
        (is (= :unknown-construction
               (error-kind #(sut/compare! facts nil))))))))

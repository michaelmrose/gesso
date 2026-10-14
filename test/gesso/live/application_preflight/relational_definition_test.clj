(ns gesso.live.application-preflight.relational-definition-test
  "v874 regressions for relational construction and local-step correspondence.

   Preserve earlier admission, comparator, startup, and explanation tests.
   Verify that actual visited-state matching from v842 is retained by the
   handle-only semantic audit without reinterpreting policy declarations.
   Matching counts and operation-exercise counts remain bounded diagnostic
   evidence. Root-step and captured poststate agreement remain one controlled
   dependency-snapshot comparison, not pinned isolation or trace refinement."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.application-preflight.relational-application :as relational-app]
   [gesso.live.application-preflight.relational-rule-coverage :as relations]
   [gesso.live.application-preflight.relational-definition :as sut]
   [gesso.model.command :as command]
   [gesso.choreo.identity :as identity]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as resolved-adapter]
   [gesso.live.optimistic.resolution-assurance :as resolution-assurance]
   [gesso.live.optimistic.server :as optimistic-server]
   [gesso.model.resolved-operation :as resolved-model]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

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


;; v847: v846's source-owned opt-in nonvacuity gate belongs to the actual
;; admitted construction. A rule has been exercised iff at least one source
;; document matched it in the explored states, NOT merely because its invariant
;; evaluated to true. These tests intentionally do not claim that finite
;; exploration covers the real application or every branch of a predicate.

(defn- source-at
  [id n]
  (assoc parent-rule :id id :source-where {:n n}))

(deftest exercise-gate-is-optional-and-empty-set-does-not-strengthen-assurance
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [declared [(definition options)
                        (assoc (definition options)
                               :require-exercised-rule-ids #{})]]
        (let [handle (sut/construct! declared)
              audit (relational-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= [] (get-in audit [:relational-assurance
                                  :required-exercised-rule-ids])))
          (is (= :not-requested
                 (get-in audit [:relational-assurance :rule-exercise-gate])))
          (is (= :not-established
                 (get-in audit [:business-rule-catalog :completeness])))
          (is (false? (get-in audit [:inspection-scope
                                    :complete-business-policy-inventory?]))))))))

(deftest required-initially-matching-rule-is-admitted-and-explained
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct!
                    (assoc (definition options)
                           :require-exercised-rule-ids #{:counter/parent-active}))
            audit (relational-audit handle)]
        (is (= [:counter/parent-active]
               (get-in audit [:relational-assurance
                              :required-exercised-rule-ids])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [] (get-in audit [:relational-assurance
                                :initially-unexercised-rule-ids])))
        (is (true? (get-in audit [:business-rule-catalog
                                 :reachable-state-rule-coverage 0
                                 :ever-matched-source-in-explored-states?])))
        (is (contains? (:not-established sut/assurance-scope)
                       :rule-exercise-does-not-establish-all-predicate-branches))
        (is (= :not-established
               (get-in audit [:business-rule-catalog :completeness])))))))

(deftest required-rule-may-first-match-after-a-transition
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [later (source-at :counter/after-one 1)
            definition (-> (definition options)
                           (assoc-in [:relational-model :declarations]
                                     [later parent-rule])
                           (assoc :require-exercised-rule-ids
                                  #{:counter/parent-active :counter/after-one}))
            handle (sut/construct! definition)
            audit (relational-audit handle)]
        ;; Declaration order, not hash-set iteration order, owns the report.
        (is (= [:counter/after-one :counter/parent-active]
               (get-in audit [:relational-assurance
                              :required-exercised-rule-ids])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [:counter/after-one]
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (= [:counter/after-one]
               (get-in audit [:relational-assurance
                              :initially-unexercised-rule-ids])))
        (is (= 3
               (get-in audit [:business-rule-catalog
                              :reachable-state-rule-coverage 0
                              :matching-source-occurrences])))
        (is (= :not-established
               (get-in audit [:business-rule-catalog :completeness])))))))

(deftest required-but-never-matched-rule-rejects-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [never (source-at :counter/never-ninety-nine 99)
            base (assoc-in (definition options)
                           [:relational-model :declarations]
                           [parent-rule never])
            no-requirement (sut/construct! base)
            rejected (error-data
                      #(sut/construct!
                        (assoc base :require-exercised-rule-ids
                               #{:counter/never-ninety-nine})))]
        ;; The relation itself is valid and vacuously holds in this model.
        (is (= :holds-in-explored-model
               (get-in (relational-audit no-requirement)
                       [:relational-assurance :status])))
        (is (= :required-relational-rules-unexercised
               (:error/kind rejected)))
        (is (= [:counter/never-ninety-nine]
               (get-in rejected [:error/details :unexercised-rule-ids])))
        (is (= :declared-bounded-explored-states
               (get-in rejected [:error/details :scope])))
        (is (not (.contains (pr-str rejected) ":initial-state")))
        (is (not (.contains (pr-str rejected) ":prepare")))))))

(deftest bounded-depth-determines-whether-an-exercise-obligation-is-met
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [rule (source-at :counter/after-two 2)
            base (-> (definition options)
                     (assoc-in [:relational-model :declarations] [rule])
                     (assoc :require-exercised-rule-ids #{:counter/after-two}))
            shallow (assoc-in base [:relational-model :options :max-depth] 1)
            failure (error-data #(sut/construct! shallow))
            deep (sut/construct! base)
            audit (relational-audit deep)]
        (is (= :required-relational-rules-unexercised (:error/kind failure)))
        (is (= [:counter/after-two]
               (get-in failure [:error/details :unexercised-rule-ids])))
        (is (= 2 (get-in audit [:relational-assurance
                               :finite-exploration :reached-depth])))
        (is (= :required-rules-matched-in-explored-model
               (get-in audit [:relational-assurance :rule-exercise-gate])))
        (is (= [:counter/after-two]
               (get-in audit [:relational-assurance
                              :became-exercised-during-exploration-rule-ids])))
        (is (pos? (get-in audit [:business-rule-catalog
                                :reachable-state-rule-coverage 0
                                :matching-source-occurrences])))))))

(deftest unknown-required-rule-ids-fail-without-a-usable-handle
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            failure (error-data
                     #(sut/construct!
                       (assoc base :require-exercised-rule-ids
                              #{:counter/missing-b :counter/missing-a
                                :counter/parent-active})))]
        (is (= :unknown-required-rule-ids (:error/kind failure)))
        (is (= [:counter/missing-a :counter/missing-b]
               (get-in failure [:error/details :unknown-rule-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))
        (is (= :unknown-construction
               (error-kind #(sut/inspect failure))))))))

(deftest malformed-exercise-requirements-fail-before-admitting-an-application
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            calls (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Unexpected expensive admission" {})))]
          (doseq [invalid [nil false true [] [:counter/parent-active]
                           '(:counter/parent-active)
                           {:counter/parent-active true}
                           #{:not-qualified} #{nil}
                           #{:counter/parent-active "invalid"}]]
            (is (= :invalid-required-rule-ids
                   (error-kind #(sut/construct!
                                 (assoc base :require-exercised-rule-ids
                                        invalid))))))
          (is (zero? @calls)))))))

(deftest exercise-gate-does-not-override-actual-relational-violations
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-exercised-rule-ids
                           #{:counter/parent-active}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest rule-exercise-gate-precedes-building-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [never (source-at :counter/never-ninety-nine 99)
            base (-> (definition options)
                     (assoc-in [:relational-model :declarations] [never])
                     (assoc :require-exercised-rule-ids
                            #{:counter/never-ninety-nine}))
            comparator-calls (atom 0)]
        (with-redefs [gesso.live.application-preflight.relational-rule-coverage/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "Unexpected comparator build" {})))]
          (is (= :required-relational-rules-unexercised
                 (error-kind #(sut/construct! base))))
          (is (zero? @comparator-calls)))))))


;; v851: v850 exposes the v848 checker's source-ordered operation exercise
;; through the actual admitted application. This is about modeled choices
;; *evaluated within bounds*, not complete real input or production coverage.

(defn- modeled-operation-exercise
  [handle]
  (get-in (relational-audit handle)
          [:relational-assurance :modeled-operation-exercise]))

(def ^:private expected-operation-count-keys
  #{:enablement-checks :disabled-checks :enabled-attempts
    :committed-attempts :rejected-attempts})

(deftest admitted-explanation-reports-exact-operation-exercise
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)
            ids (get-in (sut/inspect handle) [:registered-operation-ids])]
        (is (= :observed-in-declared-bounded-model (:status report)))
        (is (= :declared-finite-operation-family-reachable-states
               (:scope report)))
        (is (= ids (mapv :registered-operation-id families)))
        (is (= 3 (count families) (count instances)))
        (is (= 12 (get-in (finite-exploration handle)
                          [:attempted-transitions])))
        (is (= 12 (reduce + (map :enabled-attempts families))))
        (is (= 12 (reduce + (map :committed-attempts instances))))
        (is (= 0 (reduce + (map :rejected-attempts instances))))
        (doseq [[i family-entry instance-entry] (map vector (range)
                                                     families instances)]
          (is (= 1 (:modeled-instance-count family-entry)))
          (is (= (select-keys family-entry expected-operation-count-keys)
                 (select-keys instance-entry expected-operation-count-keys)))
          (is (= {:enablement-checks 4 :disabled-checks 0
                  :enabled-attempts 4 :committed-attempts 4
                  :rejected-attempts 0}
                 (select-keys instance-entry expected-operation-count-keys)))
          (is (= (keyword "gesso.model.operation-family"
                          (str "choice-" i "-0-0"))
                 (:model-instance-id instance-entry)))
          (is (= 0 (:principal-choice-index instance-entry)))
          (is (= 0 (:argument-choice-index instance-entry)))
          (is (true? (:ever-attempted-in-explored-states? family-entry)))
          (is (true? (:ever-attempted-in-explored-states? instance-entry))))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= [] (:never-attempted-modeled-instance-ids report)))
        (is (= #{:completeness-of-real-principals-or-arguments
                 :faithfulness-of-family-callbacks-to-registered-executors
                 :exercise-in-production :unbounded-operation-coverage}
               (:not-established report)))
        (is (contains? (:not-established sut/assurance-scope)
                       :modeled-operation-exercise-does-not-establish-production-execution))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest fully-disabled-families-are-not-reported-as-exercised
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (update-in (definition options) [:relational-model :families]
                                (fn [families]
                                  (mapv #(assoc % :enabled? (constantly false)
                                                  :prepare (fn [& _]
                                                             (throw (ex-info "Disabled operation prepared" {}))))
                                        families)))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)]
        (is (= 0 (:attempted-transitions (finite-exploration handle))))
        (is (true? (:frontier-exhausted? (finite-exploration handle))))
        (is (= 3 (count families) (count instances)))
        (is (= (mapv :registered-operation-id families)
               (:never-attempted-registered-operation-ids report)))
        (is (= (mapv :model-instance-id instances)
               (:never-attempted-modeled-instance-ids report)))
        (doseq [entry (concat families instances)]
          (is (= {:enablement-checks 1 :disabled-checks 1
                  :enabled-attempts 0 :committed-attempts 0
                  :rejected-attempts 0}
                 (select-keys entry expected-operation-count-keys)))
          (is (false? (:ever-attempted-in-explored-states? entry))))))))

(deftest mixed-principal-choices-keep-family-totals-without-leaking-inputs
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [private-principal :helper/private-person-token-851
            private-arguments {:delta 1 :opaque-input "private-arg-token-851"}
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :families 0 :principals]
                                   [:helper/a private-principal])
                         (assoc-in [:relational-model :families 0 :arguments]
                                   [private-arguments])
                         (assoc-in [:relational-model :families 0 :enabled?]
                                   (fn [_ principal _]
                                     (not= principal private-principal))))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            families (:registered-operation-exercise report)
            instances (:modeled-instance-exercise report)
            claim (first (filter #(= :request/claim (:registered-operation-id %)) families))
            claim-choices (filterv #(= :request/claim
                                       (:registered-operation-id %)) instances)
            printed (pr-str (sut/explain handle))]
        (is (= 4 (count instances)))
        (is (= 2 (:modeled-instance-count claim)))
        (is (= 1 (:enabled-attempts claim)))
        (is (= 1 (:disabled-checks claim)))
        (is (= [0 1] (mapv :principal-choice-index claim-choices)))
        (is (= [0 0] (mapv :argument-choice-index claim-choices)))
        (is (= [1 0] (mapv :enabled-attempts claim-choices)))
        (is (= [0 1] (mapv :disabled-checks claim-choices)))
        (is (= [:request/claim] (->> instances
                                  (filter (complement :ever-attempted-in-explored-states?))
                                  (mapv :registered-operation-id))))
        (is (= [(:model-instance-id (second claim-choices))]
               (:never-attempted-modeled-instance-ids report)))
        (is (= [] (:never-attempted-registered-operation-ids report))
            "One disabled choice must not mark its whole family unexercised")
        (is (= 3 (get-in (finite-exploration handle)
                         [:attempted-transitions])))
        (is (= 3 (reduce + (map :enabled-attempts instances))))
        (is (not (.contains printed "private-person-token-851")))
        (is (not (.contains printed "private-arg-token-851")))
        (is (not (contains? (first instances) :principal)))
        (is (not (contains? (first instances) :arguments)))
        (is (not (.contains printed "#object[")))))))

(deftest rejected-model-attempts-are-attempted-not-disabled
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [aux-key [:counters :aux]
            aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
            stale (assoc aux-doc :revision 99 :updated-at :stale)
            guard (command/guard :counters stale version-keys)
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :initial-state aux-key] aux-doc)
                         (update-in [:relational-model :families 0 :prepare]
                                    (fn [prepare]
                                      (fn [& args]
                                        (update (apply prepare args) :guards
                                                conj guard)))))
            handle (sut/construct! declared)
            report (modeled-operation-exercise handle)
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 (:registered-operation-exercise report)))
            instance (first (filter #(= :request/claim (:registered-operation-id %))
                                    (:modeled-instance-exercise report)))]
        (is (= 3 (:attempted-transitions (finite-exploration handle))))
        (is (= 1 (:enablement-checks claim)))
        (is (= 0 (:disabled-checks claim)))
        (is (= 1 (:enabled-attempts claim)))
        (is (= 0 (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= (select-keys claim expected-operation-count-keys)
               (select-keys instance expected-operation-count-keys)))
        (is (true? (:ever-attempted-in-explored-states? claim)))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest operation-exercise-summary-fails-closed-before-comparator-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (definition options)
            original (relational-app/require-relational-application!
                      (:application-options declared)
                      (:relational-model declared))
            counts (get-in original [:finite-relational-verdict :operation-exercise])
            verdict (:finite-relational-verdict original)
            mutations [(fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise] pop))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :operation-id]
                                    (constantly :other/instance)))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :enabled-attempts] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :exploration :attempted-transitions] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :operation-exercise 0 :disabled-checks] inc))
                       (fn [result]
                         (update-in result [:finite-relational-verdict
                                            :family-expansion :instances 0 :instance-id]
                                    (constantly :other/choice)))]
            comparator-calls (atom 0)]
        (is (= 3 (count counts)))
        (is (= 12 (get-in verdict [:exploration :attempted-transitions])))
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "Comparator should not run" {})))]
          (doseq [mutate mutations]
            (let [spoofed (mutate original)]
              (with-redefs [relational-app/require-relational-application!
                            (fn [& _] spoofed)]
                (let [failure (error-data #(sut/construct! declared))]
                  (is (= :incomplete-modeled-operation-exercise
                         (:error/kind failure)))
                  (is (not (contains? (:error/details failure) :initial-state)))
                  (is (not (contains? (:error/details failure) :families)))))))
          (is (zero? @comparator-calls)))))))

(deftest reported-operation-exercise-is-inert-and-does-not-reexecute-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [private-marker "v851-private-callback-capture"
            calls (atom {:enabled 0 :prepare 0})
            declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (-> family
                                              (update :enabled?
                                                      (fn [original]
                                                        (fn [& args]
                                                          (swap! calls update :enabled inc)
                                                          (let [_ private-marker]
                                                            (apply original args)))))
                                              (update :prepare
                                                      (fn [original]
                                                        (fn [& args]
                                                          (swap! calls update :prepare inc)
                                                          (let [_ private-marker]
                                                            (apply original args)))))))
                                        families)))
            handle (sut/construct! declared)
            after-admission @calls
            report (modeled-operation-exercise handle)
            printed (pr-str (sut/explain handle))]
        (is (pos? (:enabled after-admission)))
        (is (pos? (:prepare after-admission)))
        (is (= after-admission @calls))
        (is (= report (modeled-operation-exercise handle)))
        (is (= after-admission @calls))
        (is (not (.contains printed private-marker)))
        (is (not (.contains printed ":initial-state")))
        (is (not (.contains printed "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/inspect report))))
        (is (= :unknown-construction
               (error-kind #(sut/explain report))))))))

;; v853: v852's required-operation attempt gate is a bounded nonvacuity
;; requirement. A modeled enabled attempt (including a rejected transaction)
;; satisfies it, not merely registration or checking an enablement predicate.
;; Neither the gate nor its report proves production execution or commits.

(defn- operation-attempt-audit
  [handle]
  (get-in (relational-audit handle) [:relational-assurance]))

(deftest attempted-operation-gate-is-optional-or-explicitly-empty
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [candidate [(definition options)
                         (assoc (definition options)
                                :require-attempted-operation-ids #{})]]
        (let [handle (sut/construct! candidate)
              audit (operation-attempt-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= [] (:required-attempted-operation-ids audit)))
          (is (= :not-requested (:operation-attempt-gate audit)))
          (is (= :not-established
                 (get-in (relational-audit handle)
                         [:business-rule-catalog :completeness])))
          (is (contains? (:not-established sut/assurance-scope)
                         :modeled-operation-exercise-does-not-establish-production-execution)))))))

(deftest required-attempted-operations-are-reported-in-registered-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [required #{:request/reassign :request/claim :request/cancel}
            handle (sut/construct!
                    (assoc (definition options)
                           :require-attempted-operation-ids required))
            registered (:registered-operation-ids (sut/inspect handle))
            audit (operation-attempt-audit handle)
            report (:modeled-operation-exercise audit)]
        (is (= (set registered) required))
        (is (= registered (:required-attempted-operation-ids audit)))
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (= registered (mapv :registered-operation-id
                                (:registered-operation-exercise report))))
        (is (every? pos? (map :enabled-attempts
                              (:registered-operation-exercise report))))
        (is (= [] (:never-attempted-registered-operation-ids report)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))
        (is (contains? (:not-established sut/assurance-scope)
                       :operation-attempts-do-not-establish-successful-commits))))))

(deftest disabled-registered-operations-fail-required-attempt-gate
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [prepare-count (atom 0)
            disabled (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (assoc family
                                                 :enabled? (constantly false)
                                                 :prepare (fn [& _]
                                                            (swap! prepare-count inc)
                                                            (throw (ex-info "Disabled" {})))))
                                        families)))
            handle (sut/construct! disabled)
            report (:modeled-operation-exercise (operation-attempt-audit handle))
            registered (:registered-operation-ids (sut/inspect handle))
            requested #{:request/cancel :request/claim}
            rejected (error-data
                      #(sut/construct!
                        (assoc disabled :require-attempted-operation-ids requested)))]
        (is (zero? @prepare-count))
        (is (= registered (:never-attempted-registered-operation-ids report)))
        (is (= :required-operations-unattempted (:error/kind rejected)))
        (is (= (filterv requested registered)
               (get-in rejected [:error/details :unattempted-operation-ids])))
        (is (= :declared-bounded-explored-states
               (get-in rejected [:error/details :scope])))
        (is (not (.contains (pr-str rejected) ":initial-state")))
        (is (not (.contains (pr-str rejected) ":prepare")))
        (is (zero? @prepare-count))))))

(deftest required-operation-can-become-enabled-later-in-the-search
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (-> (definition options)
                     (assoc-in [:relational-model :families 0 :enabled?]
                               (fn [state _ _]
                                 (pos? (get-in state [counter-key :n]))))
                     (assoc :require-attempted-operation-ids #{:request/claim}))
            shallow (assoc-in base [:relational-model :options :max-depth] 1)
            deep (assoc-in base [:relational-model :options :max-depth] 2)
            failure (error-data #(sut/construct! shallow))
            handle (sut/construct! deep)
            audit (operation-attempt-audit handle)
            families (get-in audit [:modeled-operation-exercise
                                    :registered-operation-exercise])
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 families))]
        (is (= :required-operations-unattempted (:error/kind failure)))
        (is (= [:request/claim]
               (get-in failure [:error/details :unattempted-operation-ids])))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (pos? (:disabled-checks claim)))
        (is (pos? (:enabled-attempts claim)))
        (is (= 2 (get-in audit [:finite-exploration :reached-depth])))))))

(deftest unknown-required-operation-ids-reject-in-registered-gate
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [failure (error-data
                     #(sut/construct!
                       (assoc (definition options)
                              :require-attempted-operation-ids
                              #{:request/claim :absent/z :absent/a})))]
        (is (= :unknown-required-operation-ids (:error/kind failure)))
        (is (= [:absent/a :absent/z]
               (get-in failure [:error/details :unknown-operation-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))
        (is (= :unknown-construction
               (error-kind #(sut/explain failure))))))))

(deftest invalid-operation-attempt-requirements-fail-before-application-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            attempts (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! attempts inc)
                        (throw (ex-info "Unexpected application admission" {})))]
          (doseq [invalid [nil false true [] [:request/claim]
                           '(:request/claim) {:request/claim true}
                           :request/claim #{:unqualified} #{nil}
                           #{:request/claim "bad"}]]
            (is (= :invalid-required-operation-ids
                   (error-kind
                    #(sut/construct!
                      (assoc base :require-attempted-operation-ids invalid))))))
          (is (zero? @attempts)))))))

(deftest a-rejected-model-transaction-still-counts-as-an-enabled-attempt
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [aux-key [:counters :aux]
            aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
            stale (assoc aux-doc :revision 99 :updated-at :stale)
            guard (command/guard :counters stale version-keys)
            declared (-> (definition options)
                         (assoc-in [:relational-model :options :max-depth] 1)
                         (assoc-in [:relational-model :initial-state aux-key] aux-doc)
                         (update-in [:relational-model :families 0 :prepare]
                                    (fn [prepare]
                                      (fn [& args]
                                        (update (apply prepare args)
                                                :guards conj guard))))
                         (assoc :require-attempted-operation-ids #{:request/claim}))
            handle (sut/construct! declared)
            audit (operation-attempt-audit handle)
            claim (first (filter #(= :request/claim (:registered-operation-id %))
                                 (get-in audit [:modeled-operation-exercise
                                                :registered-operation-exercise])))]
        (is (= :required-operations-attempted-in-explored-model
               (:operation-attempt-gate audit)))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (= 1 (:enabled-attempts claim)))
        (is (zero? (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest actual-invariant-violations-take-precedence-over-attempt-gating
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-attempted-operation-ids #{:request/claim}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample
               (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest unattempted-required-operation-rejects-before-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (-> (definition options)
                     (assoc-in [:relational-model :families 0 :enabled?]
                               (constantly false))
                     (assoc :require-attempted-operation-ids #{:request/claim}))
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Comparator must not run" {})))]
          (is (= :required-operations-unattempted
                 (error-kind #(sut/construct! base))))
          (is (zero? @calls)))))))

(deftest operation-attempt-gate-does-not-reexecute-or-leak-modeled-callbacks
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v853-secret-private-callback"
            invocations (atom 0)
            declared (-> (definition options)
                         (update-in [:relational-model :families 0 :enabled?]
                                    (fn [enabled?]
                                      (fn [& args]
                                        (swap! invocations inc)
                                        (let [_ secret] (apply enabled? args)))))
                         (assoc :require-attempted-operation-ids #{:request/claim}))
            handle (sut/construct! declared)
            after-admission @invocations
            first-report (sut/explain handle)
            second-report (sut/explain handle)
            audit (get-in first-report [:semantic-audit :relational-assurance])
            printed (pr-str first-report)]
        (is (pos? after-admission))
        (is (= after-admission @invocations))
        (is (= first-report second-report))
        (is (= after-admission @invocations))
        (is (= [:request/claim] (:required-attempted-operation-ids audit)))
        (is (not (.contains printed secret)))
        (is (not (.contains printed ":initial-state")))
        (is (not (.contains printed "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/inspect audit))))))))


;; v855: The :require-operation-outcomes admission gate is intentionally
;; narrower than production correspondence.  It demands observations of
;; selected finite committed/rejected branches for registered operations;
;; registration, enablement, and attempts alone cannot satisfy it.

(defn- outcome-audit [handle]
  (get-in (sut/explain handle) [:semantic-audit :relational-assurance]))

(defn- stale-guard-definition
  "Use a valid but unsatisfied guard against a separate, existing document.
   This exercises transaction rejection, not a malformed command or an
   undeclared relational invariant."
  [base]
  (let [aux-key [:counters :aux]
        aux-doc (assoc (get initial-state counter-key) :xt/id :aux)
        stale (assoc aux-doc :revision 99 :updated-at :stale)
        guard (command/guard :counters stale version-keys)]
    (-> base
        (assoc-in [:relational-model :options :max-depth] 1)
        (assoc-in [:relational-model :initial-state aux-key] aux-doc)
        (update-in [:relational-model :families 0 :prepare]
                   (fn [prepare]
                     (fn [& args]
                       (update (apply prepare args) :guards conj guard)))))))

(deftest required-outcome-gate-defaults-to-unrequested
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (doseq [candidate [(definition options)
                         (assoc (definition options) :require-operation-outcomes {})]]
        (let [handle (sut/construct! candidate)
              audit (outcome-audit handle)]
          (is (= :admitted-finite-relational-construction
                 (:status (sut/inspect handle))))
          (is (= {} (:required-operation-outcomes audit)))
          (is (= :not-requested (:operation-outcome-gate audit)))
          (is (= :not-established
                 (get-in (relational-audit handle)
                         [:business-rule-catalog :completeness])))
          (is (contains? (:not-established sut/assurance-scope)
                         :modeled-operation-outcomes-do-not-establish-production-outcomes)))))))

(deftest required-commits-are-reported-in-registered-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [required {:request/cancel #{:committed}
                      :request/claim #{:committed}}
            handle (sut/construct!
                    (assoc (definition options) :require-operation-outcomes required))
            registered (:registered-operation-ids (sut/inspect handle))
            audit (outcome-audit handle)
            expected (into (array-map)
                           (keep (fn [id] (when (required id) [id [:committed]])))
                           registered)]
        (is (= expected (:required-operation-outcomes audit)))
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (= (vec (keys expected))
               (filterv (set (keys required)) registered)))
        (is (every? pos?
                    (map :committed-attempts
                         (filter #(contains? required (:registered-operation-id %))
                                 (get-in audit [:modeled-operation-exercise
                                                :registered-operation-exercise])))))
        (is (contains? (:not-established sut/assurance-scope)
                       :operation-attempts-do-not-establish-successful-commits))
        (is (contains? (:not-established sut/assurance-scope)
                       :modeled-operation-outcomes-do-not-establish-production-outcomes))))))

(deftest missing-rejections-are-reported-without-claiming-commits-suffice
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            requirements {:request/cancel #{:rejected}
                          :request/claim #{:committed :rejected}}
            registered (:registered-operation-ids
                        (sut/inspect (sut/construct! base)))
            failure (error-data
                     #(sut/construct! (assoc base :require-operation-outcomes requirements)))
            missing (get-in failure [:error/details :unobserved-operation-outcomes])]
        (is (= :required-operation-outcomes-unobserved (:error/kind failure)))
        (is (= (filterv (set (keys requirements)) registered)
               (mapv :operation-id missing)))
        (is (= [{:operation-id :request/claim
                 :unobserved-outcomes [:rejected]}]
               (filterv #(= :request/claim (:operation-id %)) missing)))
        (is (= [{:operation-id :request/cancel
                 :unobserved-outcomes [:rejected]}]
               (filterv #(= :request/cancel (:operation-id %)) missing)))
        (is (= :declared-bounded-explored-states
               (get-in failure [:error/details :scope])))
        (is (not (.contains (pr-str failure) ":initial-state")))
        (is (= :unknown-construction (error-kind #(sut/explain failure))))))))

(deftest unknown-outcome-operation-ids-are-rejected-in-stable-order
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [failure (error-data
                     #(sut/construct!
                       (assoc (definition options) :require-operation-outcomes
                              {:missing/z #{:committed}
                               :request/claim #{:committed}
                               :missing/a #{:rejected}})))]
        (is (= :unknown-required-operation-outcome-ids (:error/kind failure)))
        (is (= [:missing/a :missing/z]
               (get-in failure [:error/details :unknown-operation-ids])))
        (is (not (contains? (:error/details failure) :initial-state)))))))

(deftest malformed-outcome-requirements-fail-before-model-admission
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            calls (atom 0)]
        (with-redefs [relational-app/require-relational-application!
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Invalid outcomes reached model admission" {})))]
          (doseq [invalid [nil true false [] #{:committed} :committed
                           {:request/claim nil} {:request/claim #{}}
                           {:request/claim [:committed]}
                           {:request/claim #{:committed :unknown}}
                           {:request/claim #{:unknown}}
                           {:request/claim #{nil}}
                           {:request/claim :committed}
                           {:unqualified #{:committed}}
                           {"request/claim" #{:committed}}]]
            (is (= :invalid-required-operation-outcomes
                   (error-kind #(sut/construct!
                                 (assoc base :require-operation-outcomes invalid))))
                (pr-str invalid)))
          (is (zero? @calls)))))))

(deftest rejected-transaction-can-satisfy-rejection-but-not-commit
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (stale-guard-definition (definition options))
            handle (sut/construct!
                    (assoc base :require-operation-outcomes
                           {:request/claim #{:rejected}}))
            audit (outcome-audit handle)
            claim (first
                   (filter #(= :request/claim (:registered-operation-id %))
                           (get-in audit [:modeled-operation-exercise
                                          :registered-operation-exercise])))
            failure (error-data
                     #(sut/construct! (assoc base :require-operation-outcomes
                                            {:request/claim #{:committed}})))]
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (= {:request/claim [:rejected]}
               (:required-operation-outcomes audit)))
        (is (= 1 (:enabled-attempts claim)))
        (is (zero? (:committed-attempts claim)))
        (is (= 1 (:rejected-attempts claim)))
        (is (= :required-operation-outcomes-unobserved (:error/kind failure)))
        (is (= [{:operation-id :request/claim
                 :unobserved-outcomes [:committed]}]
               (get-in failure [:error/details :unobserved-operation-outcomes])))))))

(deftest both-outcomes-must-be-observed-on-reachable-branches
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [base (definition options)
            rejected (stale-guard-definition base)
            guarded-prepare (get-in rejected [:relational-model :families 0 :prepare])
            plain-prepare (get-in base [:relational-model :families 0 :prepare])
            ;; The stale guard applies only at the initial state; a different
            ;; operation can advance the counter so the claim then commits.
            mixed (-> rejected
                      (assoc-in [:relational-model :options :max-depth] 2)
                      (assoc-in [:relational-model :families 0 :prepare]
                                (fn [state & args]
                                  (if (zero? (get-in state [counter-key :n]))
                                    (apply guarded-prepare state args)
                                    (apply plain-prepare state args))))
                      (assoc :require-operation-outcomes
                             {:request/claim #{:rejected :committed}}))
            handle (sut/construct! mixed)
            audit (outcome-audit handle)
            claim (first
                   (filter #(= :request/claim (:registered-operation-id %))
                           (get-in audit [:modeled-operation-exercise
                                          :registered-operation-exercise])))]
        (is (= {:request/claim [:committed :rejected]}
               (:required-operation-outcomes audit)))
        (is (= :required-outcomes-observed-in-explored-model
               (:operation-outcome-gate audit)))
        (is (pos? (:committed-attempts claim)))
        (is (pos? (:rejected-attempts claim)))
        (is (= 2 (get-in audit [:finite-exploration :max-depth])))
        (is (= :not-established
               (get-in (relational-audit handle)
                       [:business-rule-catalog :completeness])))))))

(deftest outcome-exercise-failures-do-not-create-a-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)
            base (assoc (definition options) :require-operation-outcomes
                        {:request/claim #{:rejected}})]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! calls inc)
                        (throw (ex-info "Comparator must not run" {})))]
          (is (= :required-operation-outcomes-unobserved
                 (error-kind #(sut/construct! base))))
          (is (zero? @calls)))))))

(deftest invariant-counterexamples-precede-outcome-exercise-gates
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [bad (-> (definition options)
                    (assoc :relational-model (relational-model true))
                    (assoc :require-operation-outcomes
                           {:request/claim #{:rejected}}))
            failure (error-data #(sut/construct! bad))]
        (is (= :finite-relational-invariant-violation (:error/kind failure)))
        (is (= :counterexample (get-in failure [:error/details :status])))
        (is (= :counter/parent-active
               (get-in failure [:error/details :counterexamples 0 :rule-id])))))))

(deftest outcome-audit-is-inert-redacted-and-not-an-authority
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret "v855-private-model-predicate"
            invocations (atom 0)
            declared (-> (definition options)
                         (update-in [:relational-model :families 0 :enabled?]
                                    (fn [enabled?]
                                      (fn [& args]
                                        (swap! invocations inc)
                                        (let [_ secret] (apply enabled? args)))))
                         (assoc :require-operation-outcomes
                                {:request/claim #{:committed}}))
            handle (sut/construct! declared)
            after-admission @invocations
            explanation (sut/explain handle)
            again (sut/explain handle)
            audit (get-in explanation [:semantic-audit :relational-assurance])
            serialized (pr-str explanation)]
        (is (pos? after-admission))
        (is (= after-admission @invocations))
        (is (= explanation again))
        (is (= after-admission @invocations))
        (is (= {:request/claim [:committed]}
               (:required-operation-outcomes audit)))
        (is (not (.contains serialized secret)))
        (is (not (.contains serialized ":initial-state")))
        (is (not (.contains serialized "#object[")))
        (is (= :unknown-construction
               (error-kind #(sut/explain audit))))))))


;; v860: v859 joins the checker-owned bounded transition prefix tree to the
;; registered operations through the admitted finite family expansion. Test
;; the PUBLIC opaque-handle explanation and fail-before-comparator boundary,
;; not a separately crafted explanation or a model passed by the caller.

(defn- modeled-transition-tree [handle]
  (get-in (relational-audit handle)
          [:relational-assurance :modeled-transition-tree]))

(deftest transition-tree-explanation-has-an-exact-redacted-boundary
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            tree (modeled-transition-tree handle)
            edges (:edges tree)
            summary (finite-exploration handle)
            expected-edge-keys #{:node-id :parent-node-id :depth
                                 :model-instance-id :registered-operation-id
                                 :status}]
        (is (= #{:kind :status :scope :root-node-id :node-count :edges
                 :state-identity :domain-completeness
                 :production-correspondence :not-established}
               (set (keys tree))))
        (is (= :bounded-reachable-transition-prefix-tree (:kind tree)))
        (is (= :observed-in-declared-bounded-model (:status tree)))
        (is (= :declared-finite-operation-family-reachable-states (:scope tree)))
        (is (zero? (:root-node-id tree)))
        (is (= 13 (:node-count tree)))
        (is (= 12 (count edges)))
        (is (= (:node-count tree) (:visited-state-occurrences summary)))
        (is (= (count edges) (:attempted-transitions summary)))
        (is (= :occurrences-not-deduplicated (:state-identity tree)))
        (is (every? #(= expected-edge-keys (set (keys %))) edges))
        (is (= (vec (range 1 13)) (mapv :node-id edges)))
        (is (= :not-established (:domain-completeness tree)))
        (is (= :not-established (:production-correspondence tree)))
        (is (contains? (:not-established tree)
                       :production-implementation-correspondence))))))

(deftest transition-tree-preserves-exploration-order-and-parent-depths
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            edges (:edges (modeled-transition-tree handle))
            depth-by-id (into {0 0} (map (juxt :node-id :depth)) edges)
            registered (:registered-operation-ids (sut/inspect handle))
            declared-order (mapv :id (get-in (definition options)
                                             [:relational-model :families]))]
        (is (= [1 1 1] (mapv :depth (take 3 edges))))
        (is (= [0 0 0] (mapv :parent-node-id (take 3 edges))))
        (is (= declared-order (mapv :registered-operation-id (take 3 edges))))
        (is (= (set registered) (set declared-order)))
        (is (= (vec (repeat 9 2)) (mapv :depth (drop 3 edges))))
        (is (= (vec (mapcat #(repeat 3 %) [1 2 3]))
               (mapv :parent-node-id (drop 3 edges))))
        (is (every? (fn [{:keys [parent-node-id depth]}]
                      (= (inc (get depth-by-id parent-node-id)) depth))
                    edges))
        (is (= #{:committed} (set (map :status edges))))
        ;; The finite search records occurrences, not a deduplicated graph:
        ;; paths A->B and B->A are both present.
        (is (= 13 (count (conj (mapv :node-id edges) 0))))
        (is (= (modeled-transition-tree handle)
               (modeled-transition-tree handle)))))))

(deftest transition-tree-uses-registered-operation-identities-and-counts
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct! (definition options))
            audit (get (relational-audit handle) :relational-assurance)
            tree (:modeled-transition-tree audit)
            exercise (:modeled-operation-exercise audit)
            instances (:modeled-instance-exercise exercise)
            mapped (into {} (map (juxt :model-instance-id
                                      :registered-operation-id)) instances)]
        (is (= (count instances) (count mapped)))
        (is (every? (fn [{:keys [model-instance-id
                                  registered-operation-id]}]
                      (= registered-operation-id (get mapped model-instance-id)))
                    (:edges tree)))
        (doseq [{:keys [model-instance-id committed-attempts
                        rejected-attempts]} instances]
          (let [edges (filter #(= model-instance-id (:model-instance-id %))
                              (:edges tree))]
            (is (= committed-attempts
                   (count (filter #(= :committed (:status %)) edges))))
            (is (= rejected-attempts
                   (count (filter #(= :rejected (:status %)) edges))))))
        (is (= (set (:registered-operation-ids (sut/inspect handle)))
               (set (map :registered-operation-id (:edges tree)))))))))

(deftest completely-disabled-model-has-only-a-root-occurrence
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (assoc family
                                                 :enabled? (constantly false)
                                                 :prepare (fn [& _]
                                                            (throw (ex-info
                                                                    "disabled callback ran" {})))))
                                        families)))
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            summary (finite-exploration handle)]
        (is (= 0 (:root-node-id tree)))
        (is (= 1 (:node-count tree)))
        (is (= [] (:edges tree)))
        (is (zero? (:attempted-transitions summary)))
        (is (true? (:frontier-exhausted? summary)))
        (is (= [] (filter :ever-attempted-in-explored-states?
                          (:registered-operation-exercise
                           (modeled-operation-exercise handle)))))))))

(deftest transition-tree-respects-declared-depth-one-boundary
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (assoc-in (definition options)
                               [:relational-model :options :max-depth] 1)
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            summary (finite-exploration handle)]
        (is (= 4 (:node-count tree)))
        (is (= 3 (count (:edges tree))))
        (is (= [0 0 0] (mapv :parent-node-id (:edges tree))))
        (is (= [1 1 1] (mapv :depth (:edges tree))))
        (is (true? (:depth-limited? summary)))
        (is (false? (:frontier-exhausted? summary)))
        (is (= :not-established (:domain-completeness tree)))))))

(deftest rejected-model-attempt-appears-in-tree-without-pretending-to-commit
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [handle (sut/construct!
                    (stale-guard-definition (definition options)))
            tree (modeled-transition-tree handle)
            edges (:edges tree)
            claim-edge (first (filter #(= :request/claim
                                          (:registered-operation-id %)) edges))
            claim-exercise (first
                            (filter #(= :request/claim
                                        (:registered-operation-id %))
                                    (:registered-operation-exercise
                                     (modeled-operation-exercise handle))))]
        (is (= 3 (count edges)))
        (is (= :rejected (:status claim-edge)))
        (is (= 1 (:rejected-attempts claim-exercise)))
        (is (zero? (:committed-attempts claim-exercise)))
        (is (= 2 (count (filter #(= :committed (:status %)) edges))))
        (is (= :not-established (:production-correspondence tree)))))))

(deftest transition-tree-explanation-redacts-principals-arguments-and-state
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [secret-state "v860-private-state-payload"
            secret-principal :helper/private-v860-token
            secret-argument "v860-private-argument-token"
            declared (-> (definition options)
                         (assoc-in [:relational-model :initial-state
                                    counter-key :private-token] secret-state)
                         (assoc-in [:relational-model :families 0 :principals]
                                   [:helper/a secret-principal])
                         (assoc-in [:relational-model :families 0 :arguments]
                                   [{:delta 1 :private secret-argument}])
                         (assoc-in [:relational-model :options :max-depth] 1))
            handle (sut/construct! declared)
            tree (modeled-transition-tree handle)
            rendered (pr-str tree)
            instances (:modeled-instance-exercise
                       (modeled-operation-exercise handle))]
        (is (= 4 (count instances)))
        (is (= 4 (count (:edges tree))))
        (is (every? #(contains? % :model-instance-id) (:edges tree)))
        (is (not (.contains rendered secret-state)))
        (is (not (.contains rendered (name secret-principal))))
        (is (not (.contains rendered secret-argument)))
        (is (not (.contains rendered "#object[")))
        (is (not (.contains rendered ":initial-state")))
        (is (not (.contains rendered ":prepare")))
        (is (not (.contains rendered ":arguments")))))))

(deftest transition-tree-is-inert-and-not-an-admission-capability
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [calls (atom 0)
            declared (update-in (definition options)
                                [:relational-model :families]
                                (fn [families]
                                  (mapv (fn [family]
                                          (update family :prepare
                                                  (fn [prepare]
                                                    (fn [& args]
                                                      (swap! calls inc)
                                                      (apply prepare args)))))
                                        families)))
            handle (sut/construct! declared)
            before @calls
            tree (modeled-transition-tree handle)
            explanation (sut/explain handle)
            forged (assoc-in explanation
                             [:semantic-audit :relational-assurance
                              :modeled-transition-tree :production-correspondence]
                             :proved)]
        (is (pos? before))
        (is (= before @calls))
        (is (= tree (modeled-transition-tree handle)))
        (is (= before @calls))
        (doseq [candidate [tree explanation forged]]
          (is (= :unknown-construction (error-kind #(sut/explain candidate))))
          (is (= :unknown-construction (error-kind #(sut/inspect candidate)))))
        (is (= tree (modeled-transition-tree handle)))))))

(deftest transition-tree-evidence-tampering-fails-before-physical-comparator
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [declared (definition options)
            original (relational-app/require-relational-application!
                      (:application-options declared)
                      (:relational-model declared))
            mutations [[:missing-tree #(update % :finite-relational-verdict
                                              dissoc :transition-tree)]
                       [:missing-edge #(update-in % [:finite-relational-verdict
                                                     :transition-tree :edges] pop)]
                       [:extra-edge #(update-in % [:finite-relational-verdict
                                                   :transition-tree :edges]
                                                conj {:node-id 99})]
                       [:incorrect-outcome #(assoc-in % [:finite-relational-verdict
                                                         :transition-tree :edges 0
                                                         :status] :rejected)]
                       [:invented-outcome #(assoc-in % [:finite-relational-verdict
                                                        :transition-tree :edges 0
                                                        :status] :pending)]
                       [:forged-model-choice #(assoc-in % [:finite-relational-verdict
                                                           :transition-tree :edges 0
                                                           :operation-id]
                                                        :forged/operation)]
                       [:wrong-parent #(assoc-in % [:finite-relational-verdict
                                                    :transition-tree :edges 1
                                                    :parent-node-id] 999)]
                       [:wrong-depth #(assoc-in % [:finite-relational-verdict
                                                   :transition-tree :edges 1
                                                   :depth] 99)]
                       [:duplicate-node-id #(assoc-in % [:finite-relational-verdict
                                                         :transition-tree :edges 1
                                                         :node-id] 1)]
                       [:incorrect-node-count #(assoc-in % [:finite-relational-verdict
                                                             :transition-tree :node-count] 500)]
                       [:unknown-edge-key #(assoc-in % [:finite-relational-verdict
                                                        :transition-tree :edges 0
                                                        :trusted?] true)]]
            comparator-calls (atom 0)]
        (is (= 12 (count (get-in original
                                 [:finite-relational-verdict :transition-tree
                                  :edges]))))
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (swap! comparator-calls inc)
                        (throw (ex-info "invalid comparator construction" {})))]
          (doseq [[label mutation] mutations]
            (testing (name label)
              (with-redefs [relational-app/require-relational-application!
                            (fn [& _] (mutation original))]
                (let [error (error-data #(sut/construct! declared))]
                  (is (= :incomplete-modeled-transition-tree (:error/kind error)))
                  (is (not (contains? (:error/details error) :initial-state)))
                  (is (not (contains? (:error/details error) :families)))))))
          (is (zero? @comparator-calls)))))))

(deftest transition-tree-explanation-rejects-stale-browser-artifacts
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [handle (sut/construct! (definition options))]
        (is (= :bounded-reachable-transition-prefix-tree
               (:kind (modeled-transition-tree handle))))
        (spit artifact-path "v860 changed after admission\n")
        (is (= :stale-application-assembly
               (error-kind #(sut/explain handle))))
        (is (= :stale-application-assembly
               (error-kind #(sut/inspect handle))))))))


;; v862: v861's compare! adds a NON-AUTHORIZING link from executor-bound
;; comparison metadata to nodes already visited by the admitted finite model.
;; For these integration tests, stub ONLY the downstream physical comparator;
;; the application assembly, opaque handle, finite checker, registry identities,
;; and tree are all genuine. These stubs are NOT physical XTDB evidence tests.

(defn- with-stubbed-transition-comparison
  [make-declaration f]
  (fixture/with-closed-application
    (fn [{:keys [options]}]
      (let [physical-comparison (atom nil)
            comparator-calls (atom 0)
            comparator-evidence (atom [])]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [evidence]
                          (swap! comparator-calls inc)
                          (swap! comparator-evidence conj evidence)
                          @physical-comparison))]
          (let [handle (sut/construct! (make-declaration options))
                tree (modeled-transition-tree handle)]
            (f {:handle handle
                :tree tree
                :physical-comparison physical-comparison
                :comparator-calls comparator-calls
                :comparator-evidence comparator-evidence})))))))

(defn- comparison-for-edge
  [edge]
  {:status :matched
   :reason :exact-observed-domain-plan-match
   :operation (:registered-operation-id edge)
   :family-instance-id (:model-instance-id edge)
   :physical-status (:status edge)})

(deftest physical-comparison-locates-exact-admitted-choice-and-outcome-occurrences
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls
                 comparator-evidence]}]
      (let [edge (first (:edges tree))
            stub-result (comparison-for-edge edge)
            expected-nodes (->> (:edges tree)
                                (filter #(and (= (:model-instance-id edge)
                                                 (:model-instance-id %))
                                              (= (:status edge) (:status %))))
                                (mapv :node-id))
            evidence (Object.)]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle evidence)
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :same-choice-and-outcome-explored (:status context)))
          (is (= expected-nodes (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions context)))
          (is (= :declared-finite-operation-family-reachable-states
                 (:scope context)))
          (is (= :not-established
                 (:modeled-state-equals-observed-state? context)))
          (is (= :not-established (:production-trace-refinement context)))
          (is (contains? (:not-established context)
                         :production-sequence-or-concurrency-correspondence))
          (is (= 1 @comparator-calls))
          (is (identical? evidence (first @comparator-evidence))))))))

(deftest comparison-matches-the-synthetic-choice-not-just-operation-name
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/a :helper/hidden-b])))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [claim-edges (filterv #(= :request/claim
                                    (:registered-operation-id %))
                                 (:edges tree))
            choices (distinct (map :model-instance-id claim-edges))]
        (is (= 2 (count choices)))
        (doseq [instance-id choices]
          (let [edge (first (filter #(= instance-id (:model-instance-id %))
                                    claim-edges))
                expected (->> claim-edges
                              (filter #(and (= instance-id (:model-instance-id %))
                                            (= :committed (:status %))))
                              (mapv :node-id))]
            (reset! physical-comparison (comparison-for-edge edge))
            (is (= expected
                   (get-in (sut/compare! handle (Object.))
                           [:modeled-transition-context
                            :modeled-transition-node-ids])))))))))

(deftest comparator-match-with-unexplored-outcome-keeps-comparison-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            other-outcome (if (= :committed (:status edge)) :rejected :committed)
            stub-result (assoc (comparison-for-edge edge)
                               :physical-status other-outcome)]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-not-explored (:status context)))
          (is (= [] (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions context)))
          (is (= :not-established (:production-trace-refinement context))))))))

(deftest comparator-counterexample-without-physical-outcome-remains-a-counterexample
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            stub-result (-> (comparison-for-edge edge)
                            (dissoc :physical-status)
                            (assoc :status :counterexample
                                   :reason :actual-execution-while-model-disabled))]
        (reset! physical-comparison stub-result)
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= stub-result (dissoc result :modeled-transition-context)))
          (is (= :counterexample (:status result)))
          (is (= :physical-outcome-not-reported (:status context)))
          (is (= [] (:modeled-transition-node-ids context)))
          (is (= #{:admitted-model-choice} (:matching-dimensions context)))
          (is (= :not-established
                 (:modeled-state-equals-observed-state? context))))))))

(deftest physically-rejected-outcome-links-to-a-rejected-modeled-occurrence
  (with-stubbed-transition-comparison
    (fn [options] (stale-guard-definition (definition options)))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [rejected (filterv #(= :rejected (:status %)) (:edges tree))
            edge (first rejected)]
        (is (seq rejected))
        (reset! physical-comparison (comparison-for-edge edge))
        (let [result (sut/compare! handle (Object.))
              context (:modeled-transition-context result)]
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-explored (:status context)))
          (is (= (mapv :node-id
                       (filter #(= (:model-instance-id edge)
                                   (:model-instance-id %)) rejected))
                 (:modeled-transition-node-ids context)))
          (is (= :not-established (:production-trace-refinement context))))))))

(deftest executor-bound-identity-mismatch-rejects-a-forged-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            correct (comparison-for-edge edge)]
        (doseq [mutant [(assoc correct :operation :invented/operation)
                        (assoc correct :operation nil)
                        (assoc correct :family-instance-id :invented/choice)
                        (assoc correct :family-instance-id nil)
                        (dissoc correct :family-instance-id)]]
          (reset! physical-comparison mutant)
          (is (= :invalid-execution-transition-identity
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= 5 @comparator-calls))))))

(deftest unsupported-physical-outcomes-fail-closed-without-promoting-unknown
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            correct (comparison-for-edge edge)]
        (doseq [invalid [:pending :unknown :aborted "committed" false]]
          (reset! physical-comparison (assoc correct :physical-status invalid))
          (is (= :invalid-execution-transition-identity
                 (error-kind #(sut/compare! handle (Object.))))))
        ;; A genuinely missing outcome is not the same as a made-up outcome.
        (reset! physical-comparison (assoc correct :physical-status nil))
        (is (= :physical-outcome-not-reported
               (get-in (sut/compare! handle (Object.))
                       [:modeled-transition-context :status])))))))

(deftest modeled-transition-link-is-redacted-inert-and-non-authorizing
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :initial-state counter-key :private]
                    "v862-secret-state")
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/v862-secret-principal])
          (assoc-in [:relational-model :families 0 :arguments]
                    [{:delta 1 :private "v862-secret-argument"}])))
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            comparison (comparison-for-edge edge)]
        (reset! physical-comparison comparison)
        (let [first-result (sut/compare! handle (Object.))
              context (:modeled-transition-context first-result)
              serialized (pr-str context)
              forged (assoc context :production-trace-refinement :proved)]
          (is (= 1 @comparator-calls))
          (is (= context (:modeled-transition-context
                          (sut/compare! handle (Object.)))))
          (is (= 2 @comparator-calls))
          (is (not (.contains serialized "v862-secret-state")))
          (is (not (.contains serialized "v862-secret-principal")))
          (is (not (.contains serialized "v862-secret-argument")))
          (is (not (.contains serialized "#object[")))
          (is (not (.contains serialized ":initial-state")))
          (is (not (.contains serialized ":prepare")))
          (is (= :unknown-construction (error-kind #(sut/explain context))))
          (is (= :unknown-construction (error-kind #(sut/inspect forged))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! forged (Object.))))))))))

(deftest current-assembly-is-required-before-comparison-or-transition-linking
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_] (swap! calls inc)
                          (throw (ex-info "Must not compare stale assembly" {}))))]
          (let [handle (sut/construct! (definition options))]
            (spit artifact-path "v862 stale browser artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (zero? @calls))))))))


;; v864: v863 makes the already bounded and redacted occurrence tree useful as
;; MODEL-ONLY predecessor histories for candidate observed comparisons.
;; These tests exercise the public API on genuine admitted constructions; only
;; the downstream physical comparator is stubbed. They do NOT establish a real
;; XTDB execution trace or a simulation/refinement relation.

(defn- prefix-context-for!
  [handle tree comparison-result edge]
  (reset! comparison-result (comparison-for-edge edge))
  (:modeled-transition-context (sut/compare! handle (Object.))))

(deftest candidate-prefix-reconstructs-the-full-depth-two-model-history
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [by-id (into {} (map (juxt :node-id identity)) (:edges tree))
            deep (first (filter #(= 2 (:depth %)) (:edges tree)))
            parent (get by-id (:parent-node-id deep))
            expected-steps (mapv #(select-keys % [:node-id :parent-node-id :depth
                                                  :model-instance-id
                                                  :registered-operation-id :status])
                                 [parent deep])]
        (is (some? deep))
        (is (= 1 (:depth parent)))
        (let [ctx (prefix-context-for! handle tree physical-comparison deep)
              witness (first (filter #(= (:node-id deep) (:matched-node-id %))
                                     (:modeled-prefix-witnesses ctx)))]
          (is (= :same-choice-and-outcome-explored (:status ctx)))
          (is (= (:node-id deep) (:matched-node-id witness)))
          (is (= expected-steps (:steps witness)))
          (is (= [1 2] (mapv :depth (:steps witness))))
          (is (= (:root-node-id tree)
                 (:parent-node-id (first (:steps witness)))))
          (is (= :modeled-occurrence-tree-only (:prefix-evidence ctx)))
          (is (= :not-established (:production-trace-refinement ctx)))
          (is (= 1 @comparator-calls)))))))

(deftest every-matching-occurrence-gets-a-distinct-correct-model-prefix
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edges (:edges tree)
            matched (first edges)
            matching (filterv #(and (= (:model-instance-id matched)
                                      (:model-instance-id %))
                                    (= (:status matched) (:status %))) edges)
            by-id (into {} (map (juxt :node-id identity)) edges)
            ctx (prefix-context-for! handle tree physical-comparison matched)
            witnesses (:modeled-prefix-witnesses ctx)]
        (is (> (count matching) 1))
        (is (= (mapv :node-id matching) (:modeled-transition-node-ids ctx)))
        (is (= (mapv :node-id matching) (mapv :matched-node-id witnesses)))
        (is (= (count matching) (count witnesses)))
        (doseq [{:keys [matched-node-id steps]} witnesses]
          (is (= (count steps) (:depth (by-id matched-node-id))))
          (is (= (vec (range 1 (inc (count steps)))) (mapv :depth steps)))
          (is (= matched-node-id (:node-id (last steps))))
          (is (= (:root-node-id tree) (:parent-node-id (first steps))))
          (doseq [[prior current] (partition 2 1 steps)]
            (is (= (:node-id prior) (:parent-node-id current)))))
        (is (= :not-established (:modeled-state-equals-observed-state? ctx)))))))

(deftest prefix-witnesses-are-scoped-to-the-exact-expanded-model-choice
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :principals]
                [:helper/a :helper/other]))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [claim-edges (filterv #(= :request/claim
                                    (:registered-operation-id %)) (:edges tree))
            choices (distinct (map :model-instance-id claim-edges))]
        (is (= 2 (count choices)))
        (doseq [choice choices]
          (let [edge (first (filter #(= choice (:model-instance-id %)) claim-edges))
                ctx (prefix-context-for! handle tree physical-comparison edge)
                witnesses (:modeled-prefix-witnesses ctx)]
            (is (seq witnesses))
            (is (= (->> claim-edges
                        (filter #(and (= choice (:model-instance-id %))
                                      (= (:status edge) (:status %))))
                        (mapv :node-id))
                   (mapv :matched-node-id witnesses)))
            (is (every? #(= choice (-> % :steps last :model-instance-id))
                        witnesses))
            (is (= :not-established (:production-trace-refinement ctx)))))))))

(deftest rejected-modeled-attempts-produce-rejected-terminal-prefix-steps
  (with-stubbed-transition-comparison
    (fn [options] (stale-guard-definition (definition options)))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [rejected (filterv #(= :rejected (:status %)) (:edges tree))
            edge (first rejected)]
        (is (seq rejected))
        (let [ctx (prefix-context-for! handle tree physical-comparison edge)]
          (is (= :same-choice-and-outcome-explored (:status ctx)))
          (is (seq (:modeled-prefix-witnesses ctx)))
          (doseq [{:keys [matched-node-id steps]} (:modeled-prefix-witnesses ctx)]
            (is (= matched-node-id (:node-id (last steps))))
            (is (= :rejected (:status (last steps)))))
          (is (contains? (:not-established ctx)
                         :model-prefix-is-not-observed-physical-history)))))))

(deftest missing-physical-outcome-does-not-fabricate-model-history
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            counterexample (-> (comparison-for-edge edge)
                               (dissoc :physical-status)
                               (assoc :status :counterexample
                                      :reason :observed-while-model-disabled))]
        (reset! physical-comparison counterexample)
        (let [result (sut/compare! handle (Object.))
              ctx (:modeled-transition-context result)]
          (is (= counterexample (dissoc result :modeled-transition-context)))
          (is (= :counterexample (:status result)))
          (is (= :physical-outcome-not-reported (:status ctx)))
          (is (= [] (:modeled-transition-node-ids ctx)))
          (is (= [] (:modeled-prefix-witnesses ctx)))
          (is (= #{:admitted-model-choice} (:matching-dimensions ctx)))
          (is (= :not-established (:production-trace-refinement ctx))))))))

(deftest unvisited-choice-outcome-has-no-prefix-but-preserves-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            opposite (if (= :committed (:status edge)) :rejected :committed)
            comparison (assoc (comparison-for-edge edge) :physical-status opposite)]
        (reset! physical-comparison comparison)
        (let [result (sut/compare! handle (Object.))
              ctx (:modeled-transition-context result)]
          (is (= comparison (dissoc result :modeled-transition-context)))
          (is (= :matched (:status result)))
          (is (= :same-choice-and-outcome-not-explored (:status ctx)))
          (is (= [] (:modeled-transition-node-ids ctx)))
          (is (= [] (:modeled-prefix-witnesses ctx)))
          (is (= #{:admitted-model-choice :physical-outcome}
                 (:matching-dimensions ctx)))
          (is (= :not-established (:modeled-state-equals-observed-state? ctx))))))))

(deftest prefix-witnesses-are-inert-redacted-and-not-admission-handles
  (with-stubbed-transition-comparison
    (fn [options]
      (-> (definition options)
          (assoc-in [:relational-model :initial-state counter-key :private]
                    "v864-hidden-state")
          (assoc-in [:relational-model :families 0 :principals]
                    [:helper/v864-secret-principal])
          (assoc-in [:relational-model :families 0 :arguments]
                    [{:delta 1 :private "v864-secret-argument"}])))
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            comparison (comparison-for-edge edge)]
        (reset! physical-comparison comparison)
        (let [one (sut/compare! handle (Object.))
              two (sut/compare! handle (Object.))
              ctx (:modeled-transition-context one)
              witnesses (:modeled-prefix-witnesses ctx)
              printed (pr-str witnesses)]
          (is (= ctx (:modeled-transition-context two)))
          (is (= 2 @comparator-calls))
          (is (seq witnesses))
          (is (not (re-find #"v864-hidden-state|v864-secret-principal|v864-secret-argument" printed)))
          (is (not (re-find #"#object\[|:initial-state|:prepare|:arguments|:principals|:commands" printed)))
          (is (= :unknown-construction (error-kind #(sut/explain witnesses))))
          (is (= :unknown-construction
                 (error-kind #(sut/compare! (first witnesses) (Object.)))))
          (is (= :not-established (:production-trace-refinement ctx)))
          (is (contains? (:not-established ctx)
                         :production-sequence-or-concurrency-correspondence)))))))

(deftest stale-admitted-assembly-prevents-comparison-and-prefix-construction
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparator-calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_] (swap! comparator-calls inc)
                          (throw (ex-info "stale assembly must not compare" {}))))]
          (let [handle (sut/construct! (definition options))]
            (spit artifact-path "v864 invalidated browser artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (zero? @comparator-calls))))))))


;; v868: public root-step correspondence checks. The controlled comparator
;; stubs below isolate the classification logic; only the final test uses
;; genuine executor-bound XTDB evidence. Neither kind proves trace refinement.

(def ^:private matching-root-observation
  {:status :matches-captured-pretransaction-state
   :relation :exact-clojure-state-map-equality
   :scope :declared-model-root-vs-controlled-dependency-snapshot})

(defn- root-step-for!
  [handle physical-comparison edge comparison]
  (reset! physical-comparison (merge (comparison-for-edge edge)
                                     {:declared-root-state-comparison
                                      matching-root-observation}
                                     comparison))
  (get-in (sut/compare! handle (Object.))
          [:modeled-transition-context :root-step-correspondence]))

(deftest matching-root-child-requires-all-four-admitted-dimensions
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(= root (:parent-node-id %)) (:edges tree)))
            report (root-step-for! handle physical-comparison edge {})]
        (is (= :matching-root-step-explored (:status report)))
        (is (some #{(:node-id edge)}
                  (:matched-root-transition-node-ids report)))
        (is (every? (fn [node-id]
                      (some #(and (= node-id (:node-id %))
                                  (= root (:parent-node-id %)))
                            (:edges tree)))
                    (:matched-root-transition-node-ids report)))
        (is (= #{:admitted-operation-choice
                 :definitive-physical-outcome
                 :exact-declared-root-vs-captured-dependencies
                 :exact-observed-domain-preparation-plan}
               (:evidence-dimensions report)))
        (is (= :single-controlled-execution-versus-declared-finite-model-root
               (:scope report)))
        (is (= :not-established
               (:modeled-poststate-equals-observed-poststate? report)))
        (is (= :not-established (:physical-trace-refinement report)))
        (is (contains? (:not-established report)
                       :posttransaction-model-state-equality))))))

(deftest wrong-declared-root-never-qualifies-as-root-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            report (root-step-for!
                    handle physical-comparison edge
                    {:declared-root-state-comparison
                     (assoc matching-root-observation
                            :status :differs-from-captured-pretransaction-state)})]
        (is (= :declared-root-not-established-equal-to-observation
               (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest incomplete-or-wrongly-scoped-root-evidence-never-qualifies
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))]
        (doseq [root [nil {}
                      (dissoc matching-root-observation :relation)
                      (assoc matching-root-observation :scope :unbound-root)
                      (assoc matching-root-observation :relation :hash-equality)]]
          (let [report (root-step-for!
                        handle physical-comparison edge
                        {:declared-root-state-comparison root})]
            (is (= :declared-root-not-established-equal-to-observation
                   (:status report)))
            (is (empty? (:matched-root-transition-node-ids report)))
            (is (empty? (:evidence-dimensions report)))))))))

(deftest mismatched-submitted-plan-remains-a-counterexample
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            comparator (merge (comparison-for-edge edge)
                              {:status :counterexample
                               :reason :modeled-and-submitted-plans-disagree
                               :declared-root-state-comparison matching-root-observation})]
        (reset! physical-comparison comparator)
        (let [actual (sut/compare! handle (Object.))
              report (get-in actual
                             [:modeled-transition-context :root-step-correspondence])]
          (is (= :counterexample (:status actual)))
          (is (= :modeled-and-submitted-plans-disagree (:reason actual)))
          (is (= comparator (dissoc actual :modeled-transition-context)))
          (is (= :submitted-plan-not-matched (:status report)))
          (is (empty? (:matched-root-transition-node-ids report))))))))

(deftest missing-physical-outcome-cannot-produce-a-root-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            report (root-step-for! handle physical-comparison edge
                                   {:physical-status nil})]
        (is (= :physical-outcome-not-reported (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest matching-deeper-transition-does-not-imply-matching-root-step
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :enabled?]
                (fn [state _ _] (pos? (:n (get state counter-key))))))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(and (= :request/claim
                                        (:registered-operation-id %))
                                      (not= root (:parent-node-id %)))
                                (:edges tree)))]
        (is (some? edge))
        (when edge
          (let [report (root-step-for! handle physical-comparison edge {})]
            (is (= :same-choice-outcome-not-explored-at-root
                   (:status report)))
            (is (empty? (:matched-root-transition-node-ids report)))
            (is (empty? (:evidence-dimensions report)))
            (is (= :not-established (:physical-trace-refinement report)))))))))

(deftest unexplored-outcome-preserves-matched-plan-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            opposite (if (= :committed (:status edge)) :rejected :committed)
            report (root-step-for! handle physical-comparison edge
                                   {:physical-status opposite})]
        (is (= :same-choice-outcome-not-explored-at-root (:status report)))
        (is (= [] (:matched-root-transition-node-ids report)))
        (is (= #{} (:evidence-dimensions report)))))))

(deftest root-step-report-is-inert-redacted-and-currentness-bound
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            comparator-calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_]
                          (swap! comparator-calls inc)
                          @comparison))]
          (let [model (-> (definition options)
                          (assoc-in [:relational-model :initial-state counter-key :secret]
                                    "v868-secret-state")
                          (assoc-in [:relational-model :families 0 :arguments]
                                    [{:delta 1 :secret "v868-secret-input"}]))
                handle (sut/construct! model)
                edge (first (:edges (modeled-transition-tree handle)))
                _ (reset! comparison
                          (assoc (comparison-for-edge edge)
                                 :declared-root-state-comparison
                                 matching-root-observation))
                one (sut/compare! handle (Object.))
                two (sut/compare! handle (Object.))
                report (get-in one [:modeled-transition-context
                                    :root-step-correspondence])]
            (is (= report
                   (get-in two [:modeled-transition-context
                                :root-step-correspondence])))
            (is (= 2 @comparator-calls))
            (is (not (re-find #"v868-secret-state|v868-secret-input|#object\["
                              (pr-str report))))
            (is (= :unknown-construction (error-kind #(sut/explain report))))
            (is (= :unknown-construction
                   (error-kind #(sut/compare! report (Object.)))))
            (spit artifact-path "v868-invalid-runtime-artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (= 2 @comparator-calls))))))))

(defn- with-real-xtdb-root-step
  "Use the same actual resolved-operation -> executor -> resolution-assurance
   path as the operation-family-correspondence test, but submit the genuine
   evidence to a relational-definition handle. Each invocation owns its node."
  [f]
  (with-open [node (xtn/start-node {})]
    (let [suffix (str (random-uuid))
          request-id (str "v868-request-" suffix)
          member-id (str "v868-member-" suffix)
          principal-label "v868-private-helper"
          now "2026-10-13T12:00:00Z"
          before {:xt/id request-id :revision 0
                  :created-at "t0" :updated-at "t0" :owner "unclaimed"}
          member {:xt/id member-id :revision 0
                  :created-at "t0" :updated-at "t0" :active true}
          claimed (assoc before :revision 1 :updated-at now
                         :owner principal-label :membership-id member-id)
          principal (identity/principal principal-label)
          command-wire
          (protocol/command
           {:command-id (identity/command-id (str "v868-command-" suffix))
            :execution-id (identity/execution-id (str "v868-execution-" suffix))
            :operation :request/claim
            :arguments {:request-id request-id}
            :observed-basis {:tx-id 1 :system-time now}
            :scope [:request request-id]})
          operation
          (resolved-model/build-operation
           {:operation :request/claim
            :read-record! (fn [_ id]
                            (first (xt/q node
                                         ["SELECT * FROM gesso_commands WHERE _id = ?"
                                          id])))
            :may-disclose? (fn [& _] true)
            :construct! (fn [& _]
                          {:now now
                           :outcome {:status :claimed}
                           :plan {:commands [(command/update-command
                                              :requests :claim before claimed
                                              version-keys)]
                                  :guards [(command/guard :memberships member
                                                          version-keys)]
                                  :emit false}})})
          executor
          (resolved-adapter/build-executor
           {:operation :request/claim
            :resolved-operation operation
            :submission-mode! (fn [_] :submit)
            :project! (fn [_ _]
                        {:resolution :confirmed
                         :authoritative
                         (protocol/authoritative
                          {:presence :present
                           :basis {:tx-id 2 :system-time now}
                           :projection {:request/owner principal-label}})
                         :outcome :request/claimed})})
          server
          (optimistic-server/server
           {:principal-fn (fn [ctx] (:authenticated-principal ctx))
            :operations
            {:request/claim
             (optimistic-server/operation
              {:name :v868-claim-optimistic
               :operation :request/claim
               :execute! executor
               :settlement-contract
               {:confirmed {:outcomes #{:request/claimed}
                            :commit/status :committed}
                :already-incorporated {:outcomes #{:request/claimed}
                                       :commit/status :committed}}})}})
          modeled-state {[:requests request-id] before
                         [:memberships member-id] member}
          families
          [{:id :request/claim
            :principals [principal-label]
            :arguments [{:request-id request-id}]
            :enabled? (fn [state who {:keys [request-id]}]
                        (and (= principal-label who)
                             (= "unclaimed"
                                (:owner (get state [:requests request-id])))
                             (true? (:active (get state [:memberships member-id])))))
            :prepare (fn [state _ {:keys [request-id]}]
                       {:commands [(command/update-command
                                    :requests :claim
                                    (get state [:requests request-id])
                                    claimed version-keys)]
                        :guards [(command/guard
                                  :memberships (get state [:memberships member-id])
                                  version-keys)]
                        :emit false})}
           {:id :request/reassign
            :principals [:manager] :arguments [{:request-id request-id}]
            :enabled? (fn [& _] false)
            :prepare (fn [& _] (throw (ex-info "Disabled" {})))}
           {:id :request/cancel
            :principals [:manager] :arguments [{:request-id request-id}]
            :enabled? (fn [& _] false)
            :prepare (fn [& _] (throw (ex-info "Disabled" {})))}]
          relational-model
          {:initial-state modeled-state
           :families families
           :declarations [{:id :request/owner-member-active
                           :source-kind :requests
                           :source-where {:owner principal-label}
                           :reference-attr :membership-id
                           :allow-unset? false
                           :target-kind :memberships
                           :target-where {:active true}}]
           :options {:max-depth 1 :max-transitions 10}}]
      (xt/execute-tx node [[:put-docs :requests before]
                           [:put-docs :memberships member]])
      (fixture/with-closed-application
        {:operations
         (assoc (fixture/standard-operations)
                :request/claim
                (fixture/trusted-operation
                 :request/claim
                 {:published-change-topics #{:request}
                  :execute! executor}))}
        (fn [{:keys [options]}]
          (let [handle (sut/construct!
                        {:application-options options
                         :relational-model relational-model})
                evidence
                (:evidence
                 (resolution-assurance/run-and-check!
                  {:server server
                   :ctx {:biff.xtdb/node node
                         :authenticated-principal principal}
                   :command command-wire}))]
            (f {:handle handle
                :evidence evidence
                :request-id request-id
                :modeled-state modeled-state})))))))

(deftest genuine-xtdb-execution-can-correspond-to-admitted-model-root-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [result (sut/compare! handle evidence)
            context (:modeled-transition-context result)
            root-step (:root-step-correspondence context)
            tree (modeled-transition-tree handle)
            root (:root-node-id tree)
            children (->> (:edges tree)
                          (filterv #(and (= root (:parent-node-id %))
                                         (= :request/claim
                                            (:registered-operation-id %))
                                         (= :committed (:status %))))
                          (mapv :node-id))]
        (is (= :matched (:status result)))
        (is (= :committed (:physical-status result)))
        (is (= :matches-captured-pretransaction-state
               (get-in result [:declared-root-state-comparison :status])))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= children (:matched-root-transition-node-ids root-step)))
        (is (seq children))
        (is (= :not-established
               (:modeled-poststate-equals-observed-poststate? root-step)))
        (is (= :not-established (:physical-trace-refinement root-step)))
        (is (not (re-find (re-pattern (java.util.regex.Pattern/quote request-id))
                          (pr-str root-step))))))))

;; v872: v871 joins the executor-bound root-poststate result to the admitted
;; finite ROOT child. The nested result is diagnostic, not an alternative
;; comparator verdict or proof of pinned snapshots / trace refinement. Stub
;; only the downstream comparator for branch tests; verify the complete
;; executor-bound path separately against actual embedded XTDB.

(def ^:private v872-matched-poststate
  {:status :matches-captured-posttransaction-dependencies
   :scope :one-controlled-serial-root-step-vs-captured-dependencies
   :relation :exact-clojure-domain-document-map-equality
   :poststate-data-exposed? false
   :pinned-xtdb-snapshot :not-established
   :production-trace-refinement :not-established})

(defn- v872-root-poststate-for!
  [handle physical-comparison edge comparison]
  (:captured-posttransaction-dependency-comparison
   (root-step-for! handle physical-comparison edge comparison)))

(deftest v872-real-xtdb-poststate-completes-admitted-root-step
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [comparison (sut/compare! handle evidence)
            root-step (get-in comparison
                              [:modeled-transition-context :root-step-correspondence])
            post (:captured-posttransaction-dependency-comparison root-step)
            source (:declared-root-poststate-comparison comparison)]
        (is (= :matched (:status comparison)))
        (is (= :committed (:physical-status comparison)))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (seq (:matched-root-transition-node-ids root-step)))
        (is (= :matches-captured-posttransaction-dependencies (:status source)))
        (is (= (:status source) (:status post)))
        (is (= :one-controlled-root-step-vs-captured-domain-dependencies
               (:scope post)))
        (is (= :exact-clojure-domain-document-map-equality (:relation post)))
        (is (true? (:observed-dependencies-only? post)))
        (is (false? (:domain-state-exposed? post)))
        (is (= :not-established (:pinned-xtdb-snapshot post)))
        (is (= :not-established (:physical-trace-refinement post)))
        (is (= :not-established (:modeled-poststate-equals-observed-poststate?
                                 root-step)))
        (is (= :not-established (:physical-trace-refinement root-step)))
        (is (not (.contains (pr-str post) request-id)))
        (is (not (re-find #":initial-state|:commands|:guards|#object\["
                          (pr-str post))))))))

(deftest v872-absent-poststate-is-unknown-not-equality
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for! handle physical-comparison edge {})]
        (is (= :captured-poststate-not-reported (:status post)))
        (is (= :one-controlled-root-step-vs-captured-domain-dependencies
               (:scope post)))
        (is (= :not-established (:physical-trace-refinement post)))
        (is (false? (:domain-state-exposed? post)))))))

(deftest v872-present-matching-poststate-preserves-comparison-verdict
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})]
        (reset! physical-comparison source)
        (let [result (sut/compare! handle (Object.))
              root-step (get-in result
                                [:modeled-transition-context :root-step-correspondence])
              post (:captured-posttransaction-dependency-comparison root-step)]
          (is (= source (dissoc result :modeled-transition-context)))
          (is (= :matching-root-step-explored (:status root-step)))
          (is (= :matches-captured-posttransaction-dependencies (:status post)))
          (is (= :exact-clojure-domain-document-map-equality (:relation post)))
          (is (= 1 @comparator-calls))
          (is (not (contains? post :documents)))
          (is (not (contains? root-step :posttransaction-documents))))))))

(deftest v872-differing-poststate-cannot-be-promoted-by-matching-root
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for!
                  handle physical-comparison edge
                  {:declared-root-poststate-comparison
                   (assoc v872-matched-poststate
                          :status :differs-from-captured-posttransaction-dependencies)})]
        (is (= :differs-from-captured-posttransaction-dependencies (:status post)))
        (is (= :not-established (:pinned-xtdb-snapshot post)))
        (is (false? (:domain-state-exposed? post)))))))

(deftest v872-disagreeing-outcomes-are-preserved-not-interpreted-as-equality
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            post (v872-root-poststate-for!
                  handle physical-comparison edge
                  {:declared-root-poststate-comparison
                   (assoc v872-matched-poststate
                          :status :modeled-and-physical-outcomes-disagree)})]
        (is (= :modeled-and-physical-outcomes-disagree (:status post)))
        (is (= :exact-clojure-domain-document-map-equality (:relation post)))
        (is (= :not-established (:physical-trace-refinement post)))))))

(deftest v872-root-mismatch-withholds-poststate-even-if-comparator-says-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-state-comparison
                        (assoc matching-root-observation
                               :status :differs-from-captured-pretransaction-state)
                        :declared-root-poststate-comparison v872-matched-poststate})]
        (is (= :declared-root-not-established-equal-to-observation
               (:status root-step)))
        (is (= :modeled-root-step-not-established
               (get-in root-step
                       [:captured-posttransaction-dependency-comparison :status])))
        (is (empty? (:matched-root-transition-node-ids root-step)))))))

(deftest v872-counterexample-plan-cannot-be-rehabilitated-by-poststate
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:status :counterexample
                           :reason :modeled-and-submitted-plans-disagree
                           :declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})]
        (reset! physical-comparison source)
        (let [result (sut/compare! handle (Object.))
              root-step (get-in result
                                [:modeled-transition-context :root-step-correspondence])]
          (is (= :counterexample (:status result)))
          (is (= :modeled-and-submitted-plans-disagree (:reason result)))
          (is (= source (dissoc result :modeled-transition-context)))
          (is (= :submitted-plan-not-matched (:status root-step)))
          (is (= :modeled-root-step-not-established
                 (get-in root-step
                         [:captured-posttransaction-dependency-comparison :status]))))))))

(deftest v872-no-definitive-physical-outcome-withholds-poststate
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:physical-status nil
                        :declared-root-poststate-comparison v872-matched-poststate})]
        (is (= :physical-outcome-not-reported (:status root-step)))
        (is (= :modeled-root-step-not-established
               (get-in root-step
                       [:captured-posttransaction-dependency-comparison :status])))))))

(deftest v872-deeper-only-transition-does-not-establish-root-poststate
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :enabled?]
                (fn [state _ _]
                  (pos? (:n (get state counter-key))))))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(and (= :request/claim
                                        (:registered-operation-id %))
                                      (not= root (:parent-node-id %)))
                                (:edges tree)))]
        (is (some? edge))
        (when edge
          (let [post (v872-root-poststate-for!
                      handle physical-comparison edge
                      {:declared-root-poststate-comparison v872-matched-poststate})]
            (is (= :modeled-root-step-not-established (:status post)))
            (is (false? (:domain-state-exposed? post)))))))))

(deftest v872-invalid-controlled-poststate-metadata-fails-closed
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            original (merge (comparison-for-edge edge)
                            {:declared-root-state-comparison matching-root-observation})
            invalid [(assoc v872-matched-poststate :scope :untrusted)
                     (assoc v872-matched-poststate :relation :hash-equality)
                     (assoc v872-matched-poststate :status :unrecognized)
                     (assoc v872-matched-poststate :poststate-data-exposed? true)
                     (assoc v872-matched-poststate :pinned-xtdb-snapshot :proven)
                     (assoc v872-matched-poststate :production-trace-refinement :proved)
                     (dissoc v872-matched-poststate :poststate-data-exposed?)
                     (dissoc v872-matched-poststate :scope)]]
        (doseq [post invalid]
          (reset! physical-comparison
                  (assoc original :declared-root-poststate-comparison post))
          (is (= :invalid-controlled-root-poststate-evidence
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= (count invalid) @comparator-calls))
        (reset! physical-comparison
                (assoc original :declared-root-poststate-comparison v872-matched-poststate))
        (is (= :matches-captured-posttransaction-dependencies
               (:status (v872-root-poststate-for!
                         handle physical-comparison edge
                         {:declared-root-poststate-comparison
                          v872-matched-poststate}))))))))

(deftest v872-poststate-report-cannot-authorize-or-survive-stale-assembly
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _]
                        (fn [_]
                          (swap! calls inc)
                          @comparison))]
          (let [handle (sut/construct! (definition options))
                tree (modeled-transition-tree handle)
                edge (first (:edges tree))]
            (reset! comparison
                    (merge (comparison-for-edge edge)
                           {:declared-root-state-comparison matching-root-observation
                            :declared-root-poststate-comparison v872-matched-poststate}))
            (let [one (sut/compare! handle (Object.))
                  post (get-in one
                               [:modeled-transition-context
                                :root-step-correspondence
                                :captured-posttransaction-dependency-comparison])]
              (is (= :matches-captured-posttransaction-dependencies
                     (:status post)))
              (is (not (map? handle)))
              (is (= :unknown-construction (error-kind #(sut/explain post))))
              (is (= :unknown-construction
                     (error-kind #(sut/compare! post (Object.)))))
              (is (= 1 @calls))
              (spit artifact-path "v872-invalidated-artifact\n")
              (is (= :stale-application-assembly
                     (error-kind #(sut/compare! handle (Object.)))))
              (is (= :stale-application-assembly
                     (error-kind #(sut/explain handle))))
              (is (= 1 @calls)))))))))

;; v874: v873's :local-step-observation is a redacted synthesis of an
;; executor-bound root-step comparison and the controlled domain after-read.
;; A local match is one finite modeled ROOT step, never a general proof of
;; pinned snapshots, multi-step correspondence, or production refinement.
;; Stub only the trusted downstream comparator for branch tests; also assert
;; the complete executor-bound route with genuine embedded XTDB evidence.

(def ^:private v874-established-dimensions
  #{:same-admitted-model-choice
    :definitive-matching-outcome
    :modeled-root-vs-observed-pretransaction-dependencies
    :submitted-domain-plan-agreement
    :modeled-root-step-vs-observed-posttransaction-dependencies})

(defn- v874-local-for!
  [handle physical-comparison edge overrides]
  (:local-step-observation
   (root-step-for! handle physical-comparison edge
                   (merge {:declared-root-poststate-comparison
                           v872-matched-poststate}
                          overrides))))

(deftest v874-real-xtdb-execution-establishes-local-step-observation
  (with-real-xtdb-root-step
    (fn [{:keys [handle evidence request-id]}]
      (let [result (sut/compare! handle evidence)
            root-step (get-in result [:modeled-transition-context
                                      :root-step-correspondence])
            local (:local-step-observation root-step)]
        (is (= :matched (:status result)))
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= :matches-captured-posttransaction-dependencies
               (get-in root-step [:captured-posttransaction-dependency-comparison
                                  :status])))
        (is (= :matching-captured-root-step-before-and-after (:status local)))
        (is (= (:matched-root-transition-node-ids root-step)
               (:matched-root-transition-node-ids local)))
        (is (seq (:matched-root-transition-node-ids local)))
        (is (= v874-established-dimensions (:established-dimensions local)))
        (is (= :one-executor-bound-attempt-vs-one-modeled-root-step
               (:scope local)))
        (is (= :bounded-controlled-execution-observation
               (:evidence-class local)))
        (is (false? (:state-contents-exposed? local)))
        (is (every? #(= :not-established (get local %))
                    [:pinned-xtdb-snapshot :non-root-step-correspondence
                     :physical-trace-refinement :application-certification]))
        (is (not (.contains (pr-str local) request-id)))))))

(deftest v874-matching-controlled-root-and-poststate-produce-exact-scope
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :matching-captured-root-step-before-and-after (:status local)))
        (is (= (:matched-root-transition-node-ids root-step)
               (:matched-root-transition-node-ids local)))
        (is (= v874-established-dimensions (:established-dimensions local)))
        (is (= 1 @comparator-calls))
        (is (= #{:status :matched-root-transition-node-ids
                 :established-dimensions :scope :evidence-class
                 :state-contents-exposed? :pinned-xtdb-snapshot
                 :non-root-step-correspondence :physical-trace-refinement
                 :application-certification}
               (set (keys local))))))))

(deftest v874-missing-after-read-is-unknown-not-a-matching-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for! handle physical-comparison edge {})
            local (:local-step-observation root-step)]
        (is (= :matching-root-step-explored (:status root-step)))
        (is (= :captured-poststate-not-reported
               (get-in root-step [:captured-posttransaction-dependency-comparison
                                  :status])))
        (is (= :captured-poststate-not-established (:status local)))
        (is (= [] (:matched-root-transition-node-ids local)))
        (is (= #{} (:established-dimensions local)))))))

(deftest v874-different-poststate-blocks-combined-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            local (v874-local-for!
                   handle physical-comparison edge
                   {:declared-root-poststate-comparison
                    (assoc v872-matched-poststate :status
                           :differs-from-captured-posttransaction-dependencies)})]
        (is (= :captured-poststate-differs (:status local)))
        (is (= [] (:matched-root-transition-node-ids local)))
        (is (= #{} (:established-dimensions local)))
        (is (= :not-established (:physical-trace-refinement local)))))))

(deftest v874-different-modeled-and-physical-outcomes-block-match
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            local (v874-local-for!
                   handle physical-comparison edge
                   {:declared-root-poststate-comparison
                    (assoc v872-matched-poststate :status
                           :modeled-and-physical-outcomes-disagree)})]
        (is (= :modeled-and-physical-outcomes-disagree (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-root-mismatch-cannot-borrow-poststate-agreement
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:declared-root-state-comparison
                        (assoc matching-root-observation :status
                               :differs-from-captured-pretransaction-state)
                        :declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :declared-root-not-established-equal-to-observation
               (:status root-step)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-plan-counterexample-is-never-rehabilitated
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:status :counterexample
                           :reason :modeled-and-submitted-plans-disagree
                           :declared-root-state-comparison matching-root-observation
                           :declared-root-poststate-comparison v872-matched-poststate})
            _ (reset! physical-comparison source)
            compared (sut/compare! handle (Object.))
            local (get-in compared [:modeled-transition-context
                                    :root-step-correspondence
                                    :local-step-observation])]
        (is (= :counterexample (:status compared)))
        (is (= :modeled-and-submitted-plans-disagree (:reason compared)))
        (is (= source (dissoc compared :modeled-transition-context)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:matched-root-transition-node-ids local)))))))

(deftest v874-unknown-physical-outcome-does-not-create-local-step
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison]}]
      (let [edge (first (:edges tree))
            root-step (root-step-for!
                       handle physical-comparison edge
                       {:physical-status nil
                        :declared-root-poststate-comparison v872-matched-poststate})
            local (:local-step-observation root-step)]
        (is (= :physical-outcome-not-reported (:status root-step)))
        (is (= :root-step-not-established (:status local)))
        (is (empty? (:established-dimensions local)))))))

(deftest v874-deeper-model-transition-is-not-root-correspondence
  (with-stubbed-transition-comparison
    (fn [options]
      (assoc-in (definition options) [:relational-model :families 0 :enabled?]
                (fn [state _ _] (pos? (:n (get state counter-key))))))
    (fn [{:keys [handle tree physical-comparison]}]
      (let [root (:root-node-id tree)
            edge (first (filter #(and (= :request/claim
                                        (:registered-operation-id %))
                                      (not= root (:parent-node-id %)))
                                (:edges tree)))]
        (is (some? edge))
        (when edge
          (let [root-step (root-step-for!
                           handle physical-comparison edge
                           {:declared-root-poststate-comparison
                            v872-matched-poststate})
                local (:local-step-observation root-step)]
            (is (= :same-choice-outcome-not-explored-at-root
                   (:status root-step)))
            (is (= :root-step-not-established (:status local)))
            (is (empty? (:matched-root-transition-node-ids local)))))))))

(deftest v874-invalid-controlled-after-read-rejects-comparison
  (with-stubbed-transition-comparison
    definition
    (fn [{:keys [handle tree physical-comparison comparator-calls]}]
      (let [edge (first (:edges tree))
            source (merge (comparison-for-edge edge)
                          {:declared-root-state-comparison matching-root-observation})
            malformed [(assoc v872-matched-poststate :scope :fabricated)
                       (assoc v872-matched-poststate :poststate-data-exposed? true)
                       (assoc v872-matched-poststate :status :invented)
                       (assoc v872-matched-poststate :pinned-xtdb-snapshot :proven)]]
        (doseq [poststate malformed]
          (reset! physical-comparison
                  (assoc source :declared-root-poststate-comparison poststate))
          (is (= :invalid-controlled-root-poststate-evidence
                 (error-kind #(sut/compare! handle (Object.))))))
        (is (= (count malformed) @comparator-calls))))))

(deftest v874-local-step-report-is-inert-redacted-and-freshness-bound
  (fixture/with-closed-application
    (fn [{:keys [options artifact-path]}]
      (let [comparison (atom nil)
            calls (atom 0)]
        (with-redefs [relations/build-comparator
                      (fn [& _] (fn [_] (swap! calls inc) @comparison))]
          (let [private-definition
                (-> (definition options)
                    (assoc-in [:relational-model :initial-state counter-key :private]
                              "v874-sensitive-root")
                    (assoc-in [:relational-model :families 0 :arguments]
                              [{:delta 1 :private "v874-sensitive-args"}]))
                handle (sut/construct! private-definition)
                edge (first (:edges (modeled-transition-tree handle)))
                _ (reset! comparison
                          (merge (comparison-for-edge edge)
                                 {:declared-root-state-comparison
                                  matching-root-observation
                                  :declared-root-poststate-comparison
                                  v872-matched-poststate}))
                first-report (sut/compare! handle (Object.))
                second-report (sut/compare! handle (Object.))
                local (get-in first-report
                              [:modeled-transition-context
                               :root-step-correspondence
                               :local-step-observation])]
            (is (= :matching-captured-root-step-before-and-after
                   (:status local)))
            (is (= local (get-in second-report
                                 [:modeled-transition-context
                                  :root-step-correspondence
                                  :local-step-observation])))
            (is (not (re-find #"v874-sensitive-root|v874-sensitive-args|#object\["
                              (pr-str local))))
            (is (= :unknown-construction (error-kind #(sut/explain local))))
            (is (= :unknown-construction
                   (error-kind #(sut/compare! local (Object.)))))
            (is (= 2 @calls))
            (spit artifact-path "v874-expired-artifact\n")
            (is (= :stale-application-assembly
                   (error-kind #(sut/compare! handle (Object.)))))
            (is (= 2 @calls))))))))

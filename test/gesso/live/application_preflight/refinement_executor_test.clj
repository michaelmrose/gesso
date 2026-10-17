(ns gesso.live.application-preflight.refinement-executor-test
  "v1044 permanent regressions for executor-bound, observed-attempt candidate
   refinement review. Stubs isolate the application coordinator's trusted
   process-local inspector and source-derived bridge; the native registries,
   XTDB hook and finite correspondence have their own separate test suites.
   These tests NEVER claim their synthetic witnesses are real XTDB receipts."
  (:require [clojure.test :refer [deftest is]]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.fixture :refer [with-closed-application]]
            [gesso.live.browser.build :as browser-build]
            [gesso.live.optimistic.resolution-assurance :as resolution-assurance]
            [gesso.model.command :as command]
            [gesso.model.formal-observation :as formal-observation]
            [gesso.model.tx-refinement-premises :as premises]
            [gesso.model.tx-refinement-subject :as subject]))

(def ^:private sha-a (apply str (repeat 64 "a")))
(def ^:private operation :request/claim)
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private plan
  {:commands [(command/create :requests
                              {:xt/id "v1044-private-request" :revision 0
                               :created-at "t0" :updated-at "t0"} version)]
   :guards [] :assertions [] :emit false})
(def ^:private prestate {})
(def ^:private env
  {:backend {:product :xtdb :version "2.2.0-beta1" :build-sha256 sha-a
             :settings {:clock :system}}
   :gesso {:source-sha256 sha-a}
   :application {:id :fixture/v1044 :operation operation
                 :source-sha256 sha-a
                 :dependency-targets [[:requests "v1044-private-request"]]
                 :writer-ids [:fixture/only-writer]
                 :policy-rule-ids [:fixture/only-policy]}})

(defn- kinds [route]
  (case route
    :versioned-backend-contract-or-proof #{:versioned-backend-contract}
    :predicate-equivalence-proof #{:independent-predicate-proof}
    :versioned-backend-contract-and-representation-proof
    #{:versioned-backend-contract :representation-refinement-proof}
    :internal-read-state-refinement-proof #{:internal-read-state-proof}
    :closed-world-and-dependency-proof #{:application-dependency-closure-proof}
    :independent-prestate-provenance-proof #{:abstract-prestate-provenance-proof}))

(defn- proposals
  ([] (proposals prestate plan env))
  ([state selected-plan selected-env]
   (let [digest (:subject-sha256
                 (subject/derive-subject state selected-plan selected-env))
         ledger (premises/derive-obligation-ledger state selected-plan)]
     (vec (mapcat
           (fn [{:keys [id discharge-route]}]
             (for [kind (sort (kinds discharge-route))]
               {:premise-id id :evidence-kind kind
                :subject (:obligation ledger)
                :artifact-sha256 sha-a
                :claimed-issuer "v1044-confidential-issuer"
                :artifact-version "unverified-v1044"
                :instance-subject-sha256 digest}))
           (:open-premises ledger))))))

(defn- error [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         {:reason (:reason (ex-data e)) :data (ex-data e)
          :message (.getMessage e)})))

(defn- executor-of [assembly]
  (get-in assembly [:operation-acquisition-assembly
                    :execution-assembly :server :operations operation :execute!]))

(defn- observed [selected-plan state]
  {:status :bounded-execution-correspondence
   :binding {:operation operation}
   :observation {:binding {:operation operation}
                 :evidence {:status :consistent}
                 :submitted-incorporation {:plan selected-plan}
                 :initial-state state}})

(defn- with-observed-stub
  "Bind simulated inspector and source-derived bridge to the exact fixture
   executor. The real production gate remains responsible for calling BOTH."
  [assembly handle selected-plan state f]
  (let [executor (executor-of assembly)
        witness (observed selected-plan state)]
    (with-redefs [resolution-assurance/inspect-evidence-for-executor
                  (fn [supplied actual]
                    (when (and (identical? handle supplied)
                               (identical? executor actual)) witness))
                  formal-observation/prepared-event-case
                  (fn [incoming]
                    (when-not (identical? incoming (:observation witness))
                      (throw (ex-info "Unrecognized source event" {})))
                    {:observation-origin :single-executor-attempt-diagnostic})]
      (f))))

(defn- selection [handle]
  {operation {:evidence-handle handle :environment env :proposals (proposals)}})

(defn- review [assembly cases]
  (app/require-executor-bound-refinement-candidates! assembly cases))

(deftest exact-observed-plan-produces-only-unverified-candidate-readiness
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (is (fn? (executor-of assembly)))
        (with-observed-stub assembly handle plan prestate
          (fn []
            (let [r (review assembly (selection handle))]
              (is (= :observed-executor-candidates-ready-for-independent-review
                     (:status r)))
              (is (= :exact-registered-executor-controlled-attempt (:scope r)))
              (is (= 1 (:observation-count r)))
              (is (= :process-local-executor-bound-observation
                     (:evidence-origin r)))
              (is (false? (:evidence-authenticated? r)))
              (is (false? (:premises-discharged? r)))
              (is (false? (:application-admitted? r)))
              (is (false? (:application-guarantee-upgraded? r)))
              (is (contains? (:not-established r)
                             :unobserved-executor-branches-and-prestates))
              (is (contains? (:not-established r)
                             :complete-executor-model-preparation-correspondence))
              (is (not (contains? (:not-established r)
                                  :actual-executor-to-transaction-plan-binding))))))))))

(deftest null-copied-and-detached-handles-never-substitute-for-live-registry
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle plan prestate
          (fn []
            (doseq [invalid [(Object.) {:status :bounded-execution-correspondence}
                             (str handle)]]
              (is (= :unrecognized-executor-observation
                     (:reason (error #(review assembly (selection invalid)))))))
            (is (= :invalid-or-mismatched-executor-selection
                   (:reason (error #(review assembly (selection nil))))))))))))

(deftest supplied-plan-and-prestate-are-forbidden
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle plan prestate
          (fn []
            (doseq [extra [:plan :abstract-state :trusted-report :executor]]
              (is (= :invalid-or-mismatched-executor-selection
                     (:reason (error #(review assembly
                                             (assoc-in (selection handle)
                                                       [operation extra] {})))))))))))))

(deftest changed-actually-submitted-plan-invalidates-previous-evidence
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)
            different-plan (assoc-in plan [:commands 0 :model/after :updated-at] "t1")]
        (with-observed-stub assembly handle different-plan prestate
          (fn []
            (is (= :aggregate-package-not-ready
                   (:reason (error #(review assembly (selection handle))))))))))))

(deftest changed-actually-observed-prestate-invalidates-previous-evidence
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)
            altered {[:requests "v1044-private-request"]
                     {:xt/id "v1044-private-request" :revision 0
                      :created-at "t0" :updated-at "t0"}}]
        (with-observed-stub assembly handle plan altered
          (fn []
            (is (= :aggregate-package-not-ready
                   (:reason (error #(review assembly (selection handle))))))))))))

(deftest altered-declared-backend-does-not-reuse-previous-instance-proposals
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle plan prestate
          (fn []
            (is (= :aggregate-package-not-ready
                   (:reason (error #(review assembly
                                            (assoc-in (selection handle)
                                                      [operation :environment :backend :version]
                                                      "different-version"))))))))))))

(deftest bad-handle-observation-status-or-correspondence-fails-closed
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)
            real-executor (executor-of assembly)]
        (doseq [bad [(assoc (observed plan prestate) :status :unverified)
                     (assoc-in (observed plan prestate)
                               [:observation :evidence :status] :counterexample)
                     (assoc-in (observed plan prestate)
                               [:observation :binding :operation] :request/cancel)]]
          (with-redefs [resolution-assurance/inspect-evidence-for-executor
                        (fn [h executor]
                          (when (and (identical? h handle)
                                     (identical? executor real-executor)) bad))]
            (is (= :unrecognized-executor-observation
                   (:reason (error #(review assembly (selection handle))))))))
        (with-redefs [resolution-assurance/inspect-evidence-for-executor
                      (fn [_ _] (observed plan prestate))
                      formal-observation/prepared-event-case
                      (fn [_] (throw (ex-info "physical mismatch with secret" {})))]
          (is (= :noncorresponding-executor-observation
                 (:reason (error #(review assembly (selection handle)))))))))))

(deftest incomplete-observation-cannot-enter-subject-construction
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle nil prestate
          (fn []
            (is (= :incomplete-executor-observation
                   (:reason (error #(review assembly (selection handle))))))))))))

(deftest invalid-operation-and-selection-keys-fail-before-observation
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [bad [nil {} [] {:unknown/op (get (selection (Object.)) operation)}
                   {"request/claim" (get (selection (Object.)) operation)}]]
        (is (= :invalid-selected-operations
               (:reason (error #(review assembly bad))))))
      (is (= :invalid-or-mismatched-executor-selection
             (:reason (error #(review assembly
                                     (assoc-in (selection (Object.))
                                               [operation :environment :application :operation]
                                               :request/cancel)))))))))

(deftest stale-browser-artifact-rejects-even-previously-valid-attempt
  (with-closed-application
    (fn [{:keys [assembly artifact-path]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle plan prestate
          (fn []
            (is (= :observed-executor-candidates-ready-for-independent-review
                   (:status (review assembly (selection handle)))))
            (spit artifact-path "console.log('changed')" :encoding "UTF-8")
            (is (= :invalid-or-stale-application-assembly
                   (:reason (error #(review assembly (selection handle))))))))))))

(deftest invalid-or-stale-assembly-cannot-rely-on-detached-explanation
  (doseq [forged [nil {} {:valid? true}
                  {:analysis {:operations {operation {}}}}]]
    (is (= :invalid-or-stale-application-assembly
           (:reason (error #(review forged (selection (Object.)))))))))

(deftest confidential-observation-and-declaration-data-stay-out-of-errors
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [handle (Object.)]
        (with-observed-stub assembly handle plan prestate
          (fn []
            (let [invalid (assoc-in (selection handle)
                                    [operation :environment :backend :settings]
                                    {:secret "v1044-highly-private-secret"
                                     :mutable (atom 3)})
                  failure (error #(review assembly invalid))
                  repr (pr-str failure)]
              (is (= :invalid-instance-or-evidence-input (:reason failure)))
              (doseq [secret ["v1044-highly-private-secret"
                              "v1044-private-request" sha-a
                              "v1044-confidential-issuer"]]
                (is (not (.contains repr secret)))))))))))

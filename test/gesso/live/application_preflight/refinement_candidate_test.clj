(ns gesso.live.application-preflight.refinement-candidate-test
  "Permanent integration regressions for the v1041 opt-in aggregate XTDB
   candidate review consumer. Genuine ApplicationAssemblies are rechecked
   against physical browser artifacts and receipts. Candidate readiness is
   neither evidence verification nor application admission."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.fixture
             :refer [with-closed-application]]
            [gesso.live.browser.build :as browser-build]
            [gesso.model.command :as command]
            [gesso.model.tx-refinement-evidence :as evidence]
            [gesso.model.tx-refinement-premises :as premises]
            [gesso.model.tx-refinement-subject :as subject]))

(def ^:private private-id "v1042-private-request-never-explain")
(def ^:private secret-issuer "v1042-confidential-issuer")
(def ^:private hash-a (apply str (repeat 64 "a")))
(def ^:private hash-b (apply str (repeat 64 "b")))
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private plan
  {:commands [(command/create :requests
                              {:xt/id private-id :revision 0
                               :created-at "t0" :updated-at "t0"}
                              version)]
   :guards [] :assertions [] :emit false})

(defn- environment [operation]
  {:backend {:product :xtdb :version "2.2.0-beta1" :build-sha256 hash-a
             :settings {:clock :system :features #{:one :two}}}
   :gesso {:source-sha256 hash-a}
   :application {:id :fixture/refinement-review :operation operation
                 :source-sha256 hash-a
                 :dependency-targets [[:requests private-id]]
                 :writer-ids [:fixture/claim-writer]
                 :policy-rule-ids [:fixture/membership-policy]}})

(defn- chosen-kinds [route]
  (case route
    :versioned-backend-contract-or-proof #{:versioned-backend-contract}
    :predicate-equivalence-proof #{:independent-predicate-proof}
    :versioned-backend-contract-and-representation-proof
    #{:versioned-backend-contract :representation-refinement-proof}
    :internal-read-state-refinement-proof #{:internal-read-state-proof}
    :closed-world-and-dependency-proof #{:application-dependency-closure-proof}
    :independent-prestate-provenance-proof #{:abstract-prestate-provenance-proof}
    (throw (ex-info "Unknown required premise route." {:route route}))))

(defn- candidate
  ([] (candidate :request/claim))
  ([operation]
   (let [env (environment operation)
         digest (:subject-sha256 (subject/derive-subject {} plan env))
         ledger (premises/derive-obligation-ledger {} plan)]
     {:abstract-state {}
      :plan plan
      :environment env
      :proposals (vec (mapcat
                       (fn [{:keys [id discharge-route]}]
                         (for [kind (sort (chosen-kinds discharge-route))]
                           {:premise-id id :evidence-kind kind
                            :subject (:obligation ledger)
                            :artifact-sha256 hash-a
                            :claimed-issuer secret-issuer
                            :artifact-version "unverified-v1042"
                            :instance-subject-sha256 digest}))
                       (:open-premises ledger)))})))

(defn- failure [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         {:data (ex-data e) :message (.getMessage e)})))

(defn- reason [f]
  (get-in (failure f) [:data :reason]))

(defn- review [assembly cases]
  (application/require-refinement-candidate-packages! assembly cases))

(defn- selected [c]
  {:request/claim c})

(defn- remains-unverified! [result]
  (is (= :selected-candidates-ready-for-independent-review (:status result)))
  (is (false? (:evidence-authenticated? result)))
  (is (false? (:premises-discharged? result)))
  (is (false? (:application-admitted? result)))
  (is (false? (:application-guarantee-upgraded? result)))
  (is (contains? (:not-established result)
                 :actual-executor-to-transaction-plan-binding))
  (is (contains? (:not-established result)
                 :application-policy-or-external-writer-closure))
  (is (contains? (:not-established result) :independent-proof-verification))
  (doseq [[_ item] (:selected-operations result)]
    (is (= :complete-unverified-candidate-package (:status item)))
    (is (true? (:ready-for-independent-review? item)))
    (is (true? (:instance-subject-derived? item)))
    (is (false? (:evidence-verified? item)))
    (is (false? (:premises-discharged? item)))
    (is (false? (:application-admitted? item)))))

(deftest genuine-current-assembly-produces-only-unverified-review-readiness
  (with-closed-application
    (fn [{:keys [assembly report]}]
      (let [result (review assembly (selected (candidate)))]
        (is (true? (:valid? report)))
        (is (= 1 (:reviewed-operation-count result)))
        (is (= (count (get-in report [:analysis :operations]))
               (:registered-operation-count result)))
        (is (= #{:request/claim} (set (keys (:selected-operations result)))))
        (remains-unverified! result)
        (doseq [secret [private-id secret-issuer hash-a]]
          (is (not (.contains (pr-str result) secret))))))))

(deftest two-current-registered-selections-are-independently-subject-bound
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [result (review assembly {:request/claim (candidate :request/claim)
                                     :request/cancel (candidate :request/cancel)})]
        (is (= 2 (:reviewed-operation-count result)))
        (is (= #{:request/claim :request/cancel}
               (set (keys (:selected-operations result)))))
        (remains-unverified! result)))))

(deftest empty-forged-and-unknown-selections-fail-closed
  (with-closed-application
    (fn [{:keys [assembly]}]
      (doseq [cases [nil {} [] {:other/claim (candidate :other/claim)}
                     {"request/claim" (candidate)} {17 (candidate)}]]
        (is (= :invalid-selected-operations
               (reason #(review assembly cases))))))))

(deftest caller-supplied-reports-do-not-confer-current-assembly-authority
  (doseq [fake [nil {} {:valid? true}
                {:type application/report-type
                 :version application/preflight-version
                 :valid? true :analysis {:operations {:request/claim {}}}}]]
    (is (= :invalid-or-stale-application-assembly
           (reason #(review fake (selected (candidate))))))))

(deftest tampered-browser-artifact-invalidates-existing-assembly
  (with-closed-application
    (fn [{:keys [assembly artifact-path]}]
      (remains-unverified! (review assembly (selected (candidate))))
      (spit artifact-path "console.log('tampered-v1042')\n" :encoding "UTF-8")
      (is (= :invalid-or-stale-application-assembly
             (reason #(review assembly (selected (candidate)))))))))

(deftest tampered-browser-receipt-invalidates-existing-assembly
  (with-closed-application
    (fn [{:keys [assembly artifact-path]}]
      (remains-unverified! (review assembly (selected (candidate))))
      (spit (browser-build/receipt-path artifact-path)
            "{:forged :receipt}\n" :encoding "UTF-8")
      (is (= :invalid-or-stale-application-assembly
             (reason #(review assembly (selected (candidate)))))))))

(deftest unrecognized-nested-assembly-fields-cannot-forge-currentness
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [tampered (assoc-in assembly
                               [:operation-acquisition-assembly :unrecognized]
                               :forged)]
        (is (= :invalid-or-stale-application-assembly
               (reason #(review tampered (selected (candidate))))))))))

(deftest candidate-must-have-exact-declaration-keys-and-matching-operation
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)]
        (doseq [bad [(dissoc good :plan)
                     (assoc good :unexpected :extra)
                     (assoc good :environment nil)
                     (assoc-in good [:environment :application :operation]
                               :request/cancel)]]
          (is (= :invalid-or-mismatched-instance-declaration
                 (reason #(review assembly (selected bad))))))
        (remains-unverified! (review assembly (selected good)))))))

(deftest empty-or-incomplete-evidence-batch-is-not-ready
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)]
        (doseq [proposals [[] (vec (butlast (:proposals good)))]]
          (is (= :aggregate-package-not-ready
                 (reason #(review assembly
                                  (selected (assoc good :proposals proposals)))))))))))

(deftest multi-part-premise-needs-both-candidate-evidence-kinds
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)
            filtered (->> (:proposals good)
                          (remove #(and (= :transaction-valid-time-and-value-representation-is-admitted
                                           (:premise-id %))
                                        (= :representation-refinement-proof
                                           (:evidence-kind %))))
                          vec)]
        (is (= 1 (- (count (:proposals good)) (count filtered))))
        (is (= :aggregate-package-not-ready
               (reason #(review assembly
                                (selected (assoc good :proposals filtered))))))))))

(deftest otherwise-complete-nine-premise-batch-contaminated-by-one-rejection
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)
            all-plus-duplicate (conj (:proposals good) (first (:proposals good)))
            raw (evidence/review-instance-evidence
                 (:abstract-state good) (:plan good)
                 (:environment good) all-plus-duplicate)]
        ;; Per-premise completeness is useful diagnostic information,
        ;; never an aggregate decision.
        (is (every? true? (map :review-package-complete? (:requirements raw))))
        (is (= :rejected-proposals
               (get-in raw [:aggregate-candidate-package :status])))
        (is (= :aggregate-package-not-ready
               (reason #(review assembly
                                (selected (assoc good :proposals
                                                 all-plus-duplicate))))))))))

(deftest false-evidence-kinds-forged-trust-and-wrong-instance-are-rejected
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)
            p (first (:proposals good))]
        (doseq [proposal [(assoc p :evidence-kind :passing-test-suite)
                          (assoc p :authenticated? true)
                          (assoc p :proof-checked? true)
                          (assoc p :certified? true)
                          (assoc p :instance-subject-sha256 hash-b)
                          (dissoc p :instance-subject-sha256)]]
          (is (= :aggregate-package-not-ready
                 (reason #(review assembly
                                  (selected (assoc good :proposals
                                                   (assoc (:proposals good)
                                                          0 proposal))))))))))))

(deftest environment-and-plan-drift-invalidates-previous-binding
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)
            changed [(assoc-in good [:environment :backend :version] "changed")
                     (assoc-in good [:environment :backend :build-sha256] hash-b)
                     (assoc-in good [:environment :backend :settings :clock] :wall)
                     (assoc-in good [:environment :gesso :source-sha256] hash-b)
                     (assoc-in good [:environment :application :source-sha256] hash-b)
                     (update-in good [:environment :application :writer-ids]
                                conj :fixture/other-writer)
                     (update-in good [:environment :application :policy-rule-ids]
                                conj :fixture/other-policy)
                     (update-in good [:environment :application :dependency-targets]
                                conj [:requests "another-request"])
                     (update-in good [:plan :commands] conj
                                (command/create :requests
                                                {:xt/id "another-request" :revision 0
                                                 :created-at "t0" :updated-at "t0"}
                                                version))]]
        (doseq [bad (butlast changed)]
          (is (= :aggregate-package-not-ready
                 (reason #(review assembly (selected bad))))))
        ;; The extra command references an undeclared target, so subject
        ;; construction must reject it before aggregate evidence review.
        (is (= :invalid-instance-or-evidence-input
               (reason #(review assembly (selected (last changed))))))))))

(deftest malformed-instance-input-redacts-private-values
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [bad (assoc-in (candidate) [:environment :backend :settings]
                          {:token "v1042-private-backend-token"
                           :mutable (atom :moving)})
            result (failure #(review assembly (selected bad)))
            printed (pr-str result)]
        (is (= :invalid-instance-or-evidence-input
               (get-in result [:data :reason])))
        (doseq [secret ["v1042-private-backend-token"
                        private-id secret-issuer hash-a]]
          (is (not (.contains printed secret))))))))

(deftest malformed-proposal-container-cannot-pose-as-reviewable-evidence
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)]
        (doseq [bad [nil {} (list (first (:proposals good)))
                     (vec (repeat 65 (first (:proposals good))))]]
          (is (= :invalid-instance-or-evidence-input
                 (reason #(review assembly
                                  (selected (assoc good :proposals bad)))))))))))

(deftest failure-of-one-selected-operation-prevents-partial-batch-readiness
  (with-closed-application
    (fn [{:keys [assembly]}]
      (is (= :aggregate-package-not-ready
             (reason #(review assembly
                              {:request/claim (candidate :request/claim)
                               :request/cancel (assoc (candidate :request/cancel)
                                                      :proposals [])})))))))

(deftest contradictory-or-forged-reviewer-claims-do-not-cross-consumer-gate
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [good (candidate)
            original (evidence/review-instance-evidence
                      (:abstract-state good) (:plan good)
                      (:environment good) (:proposals good))
            corruptions
            [(assoc-in original [:aggregate-candidate-package :status]
                       :rejected-proposals)
             (assoc-in original [:aggregate-candidate-package :binding-scope]
                       :generic-premise-catalog)
             (assoc-in original [:aggregate-candidate-package
                                 :all-proposals-reviewable?] false)
             (assoc-in original [:aggregate-candidate-package
                                 :all-premise-candidate-packages-complete?] false)
             (assoc-in original [:aggregate-candidate-package
                                 :ready-for-independent-review?] false)
             (assoc-in original [:aggregate-candidate-package :verified?] true)
             (assoc original :any-proof-checked? true)
             (assoc original :external-premises-discharged #{:fake-premise})
             (assoc original :plan-specific-subject-bound? false)
             (assoc original :application-dependencies-proved-complete? true)
             (assoc-in original [:requirements 0 :review-package-complete?] false)
             (assoc-in original [:proposals 0 :authenticated?] true)]]
        (doseq [forged corruptions]
          (with-redefs [evidence/review-instance-evidence (fn [& _] forged)]
            (is (= :aggregate-package-not-ready
                   (reason #(review assembly (selected good)))))))))))

(deftest opting-into-evidence-review-does-not-change-ordinary-assembly-guarantees
  (with-closed-application
    (fn [{:keys [assembly]}]
      (let [before (application/explain assembly)
            result (review assembly (selected (candidate)))
            after (application/explain assembly)]
        (remains-unverified! result)
        (is (= before after))
        (is (not= :general-xtdb-refinement-proved (:guarantee after)))
        (is (not= :application-certified (:guarantee after)))))))

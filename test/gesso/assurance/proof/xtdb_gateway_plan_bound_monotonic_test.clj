(ns gesso.assurance.proof.xtdb-gateway-plan-bound-monotonic-test
  "v1591: Permanent bounded real-XTDB regressions for the v1590 ONE-submission
   composition of certified-plan target identity, independently acquired
   complete-CAS prestate, and exact-successor revision policy.

   Deliberately reuse v1587's genuine certified application and XTDB fixture,
   so the assertions concern actual canonical commands and physical writes.
   They establish observed local behavior, NOT deployed writer adoption,
   target-table business truth, principal authorization, external-writer
   exclusion, hidden transaction-internal prestate, or universal refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate
             :as plan-bound]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.model.command :as command]
            [xtdb.api :as xt]))

;; The established real-XTDB fixture supplies a *genuine* certified plan,
;; issued target resolver and closed application. It is intentionally shared
;; rather than creating a second, weaker imitation of that contract.
(defn- helper [sym]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      sym)]
    (when-not v
      (throw (ex-info "Missing real-XTDB certified-plan fixture"
                      {:reason :missing-physical-fixture})))
    (var-get v)))

(defn- node [] (helper '*node*))
(defn- certified [] (helper '*case*))
(defn- before [] ((helper 'before)))
(defn- after [b] ((helper 'after) b))
(defn- candidate [b] ((helper 'candidate) b))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))
(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})

(use-fixtures :once (fn [f] ((helper 'with-certified-xtdb) f)))

(defn- prepare
  ([source] (prepare source :revision))
  ([source revision-field]
   (let [{:keys [contract issuance]} (certified)]
     (monotonic/prepare-plan-bound-monotonic-revision-one-shot!
      (node) contract issuance operation request source revision-field))))

(defn- summary [g] ((:summary! g)))

(defn- reason [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           code
           (throw e)))))

(defn- reject! [expected f]
  (is (= expected (reason f)) (str "Expected rejection: " expected)))

(defn- assert-scoped-report! [r status]
  (is (= monotonic/plan-bound-monotonic-type (:type r)))
  (is (= monotonic/plan-bound-monotonic-version (:version r)))
  (is (= status (:status r)))
  (doseq [[k v]
          {:physical-submission-gateways-prepared 1
           :revision-policy :source-checked-exactly-one-nonwrapping-successor
           :original-revision-version-guard :required-and-source-matched
           :pre-submit-revision-recheck :required
           :plan-target-correspondence :exact-current-issued-callback-checked-at-preparation
           :pre-submit-plan-target-recheck :required
           :external-prestate :same-node-pinned-read-matched-at-preparation
           :transaction-time-revision-guard :canonical-complete-top-level-cas
           :prestate-submission-correspondence :one-prepared-acquired-gateway-for-this-submission
           :monotonic-complete-writer-premise :not-established
           :external-writer-aba :not-excluded
           :aba-history-detection :not-established
           :caller-node-authenticity :not-established
           :transaction-internal-prestate :not-authenticated
           :backend-assert-semantics :trusted-bounded-premise
           :physical-poststate-causality :not-established
           :complete-deployment-writer-coverage :not-established
           :principal-authorization :not-established
           :abstract-operation-refinement :not-established
           :target-table-business-truth :application-premise
           :registered-biff-writer-adoption :not-established
           :proof-status :conditional-plan-bound-monotonic-prestate
           :certificate :not-issued
           :admission :not-admitted
           :transaction-authority :one-shot-submission-capability}]
    (is (= v (get r k)) (str "Overstated or missing claim: " k)))
  (is (contains? (:open-obligations r)
                 :complete-deployment-monotonic-writer-discipline))
  (is (contains? (:open-obligations r)
                 :biff-writer-identity-not-yet-integrated))
  (is (not-any? #(contains? r %)
                [:source :contract :resolver :target :node :plan :before
                 :after :document :id :tx-key :tx-ops :gateway
                 :snapshot-token :transaction-result]))
  (is (not (str/includes? (pr-str r) "v1587-private-physical-target")))
  (is (not (str/includes? (pr-str r) "v1587-secret-must-not-be-disclosed"))))

(deftest genuine-certified-plan-gets-only-one-guarded-submission
  (let [b (before) _ (put! b) g (prepare (candidate b))
        initial (summary g)]
    (is (= #{:type :version :submit! :summary!} (set (keys g)))
        "No independently usable XTDB gateway or raw client may escape")
    (is (= :prepared (:underlying-gateway-status initial)))
    (assert-scoped-report! initial :prepared)
    (is (= b (physical)) "Preparation is read-only")
    (is (some? ((:submit! g))))
    (is (= (after b) (physical)))
    (let [final (summary g)]
      (is (= :returned-transaction-key (:underlying-gateway-status final)))
      (assert-scoped-report! final :permit-consumed))
    (reject! :one-shot-already-attempted (:submit! g))
    (is (= (after b) (physical)))))

(deftest invalid-revisions-cannot-acquire-a-physical-gateway
  (let [b (before) _ (put! b) c (candidate b)]
    (doseq [bad [-1 0 2 1.5 1N 1M "1" nil false Long/MAX_VALUE]]
      (reject! :revision-not-exact-successor
               #(prepare (assoc-in c [:plan :commands 0 :model/after :revision]
                                   bad))))
    (doseq [field [nil :xt/id :domain/revision "revision"]]
      (reject! :unsupported-revision-field #(prepare c field)))
    (is (= b (physical)))))

(deftest original-revision-guard-is-not-optional
  (let [b (before) _ (put! b) c (candidate b)]
    (reject! :revision-not-originally-version-guarded
             #(prepare (assoc-in c [:plan :commands 0
                                    :model/expected :model/checks]
                                  [[:created-at "created"]])))
    (is (= b (physical)))))

(deftest target-and-command-substitution-rejected-before-submission
  (let [b (before) _ (put! b) c (candidate b)]
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare (assoc-in c [:protected-documents 0 :target]
                                 [:requests "unintended-document"])))
    (reject! :command-or-protection-disagrees-with-plan-target
             #(prepare (assoc-in c [:plan :commands 0 :model/operation]
                                 :cancel)))
    (is (= b (physical)))))

(deftest forged-external-before-never-issues-authority
  (let [b (before) _ (put! b) c (candidate b)]
    (reject! :guarded-external-prestate-acquisition-failed
             #(prepare (assoc-in c [:plan :commands 0 :model/before :note]
                                 "forged before")))
    (is (= b (physical)))))

(deftest stale-read-after-preparation-fails-transaction-time-guards
  (let [b (before) _ (put! b) g (prepare (candidate b))
        outside (assoc b :note "outside-but-same-revision")]
    (put! outside)
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= outside (physical)) "Conflicting physical write must survive")
    (assert-scoped-report! (summary g) :permit-consumed)
    (is (= :threw-outcome-unknown (:underlying-gateway-status (summary g))))
    (reject! :one-shot-already-attempted (:submit! g))
    (is (= outside (physical)))))

(deftest independent-prepared-operations-cannot-both-claim-one-version
  (let [b (before) _ (put! b)
        first-gateway (prepare (candidate b))
        second-gateway (prepare (candidate b))]
    (is (some? ((:submit! first-gateway))))
    (reject! :xtdb/assert-failed (:submit! second-gateway))
    (is (= (after b) (physical)))
    (reject! :one-shot-already-attempted (:submit! first-gateway))
    (reject! :one-shot-already-attempted (:submit! second-gateway))
    (assert-scoped-report! (summary second-gateway) :permit-consumed)))

(deftest failed-pre-submit-seam-check-consumes-outer-permit
  (let [b (before) _ (put! b) g (prepare (candidate b))]
    (with-redefs [plan-bound/prepare-plan-bound-acquired-one-shot!
                  (fn [& _] (throw (ex-info "substituted" {})))]
      (reject! :substituted-plan-bound-prestate-gateway (:submit! g)))
    (is (= :permit-consumed (:status (summary g))))
    (reject! :one-shot-already-attempted (:submit! g))
    (is (= b (physical)))))

(deftest swapped-preparation-seam-fails-before-authority
  (let [b (before) _ (put! b)]
    (with-redefs [plan-bound/prepare-plan-bound-acquired-one-shot!
                  (fn [& _] {:type plan-bound/report-type
                             :version plan-bound/report-version
                             :submit! (fn [] :forged)
                             :summary! (fn [] {:status :prepared})})]
      (reject! :substituted-plan-bound-prestate-gateway
               #(prepare (candidate b))))
    (is (= b (physical)))))

(deftest raw-writer-and-aba-premises-remain-visible
  (let [b (before) _ (put! b) stale (prepare (candidate b))
        outside (assoc b :note "intervening-write")]
    (put! outside)
    (put! b)
    ;; The exact complete prestate has been restored by an unmediated writer.
    ;; Neither this gateway nor its revision check can detect that history.
    (is (some? ((:submit! stale))))
    (is (= (after b) (physical)))
    (let [report (summary stale)]
      (is (= :not-excluded (:external-writer-aba report)))
      (is (= :not-established (:aba-history-detection report)))
      (is (= :not-established (:complete-deployment-writer-coverage report)))
      (assert-scoped-report! report :permit-consumed))))

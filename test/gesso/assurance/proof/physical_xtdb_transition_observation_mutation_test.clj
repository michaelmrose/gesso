(ns gesso.assurance.proof.physical-xtdb-transition-observation-mutation-test
  "v1466: Adversarial and actual-XTDB checks for the redacted v1465
   physical transition phase contract. These tests establish one controlled
   execution and fail-closed diagnostic boundaries, NOT general physical
   refinement, production writer completeness, or transaction authority."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.physical-xtdb-transition-observation :as physical]
            [gesso.model.command :as command]
            [gesso.model.tx-refinement :as refinement]
            [gesso.model.tx-refinement-xtdb :as collector]
            [xtdb.node :as xtn]))

(defn- raw-report
  ([] (raw-report :committed true))
  ([status agreement?]
   (let [checks (zipmap (var-get #'physical/mandatory-comparison-checks)
                        (repeat true))
         checks (if agreement? checks (assoc checks :poststate false))]
     {:comparison
      {:checks checks
       :corresponds? agreement?
       :expected-status status
       :observed-status status
       :scope refinement/submission-comparison-scope
       :evidence-kind :caller-supplied-recorded-step-comparison
       :certifies-physical-execution? false
       :recorded-op-count 2
       :canonical-op-count 2
       :mismatched-op-indices []}
      :execution (case status
                   :committed {:status :committed}
                   :rejected {:status :rejected
                              :error/code :xtdb/assert-failed})
      :collected-target-count 2
      :submitted-operation-count 2
      :collection-scope collector/repeatable-ledger-window-collection-scope
      :evidence-kind
      :direct-controlled-xtdb-repeatable-ledger-window-observation
      :observation-basis :separate-pinned-before-and-after-phases
      :certifies-physical-execution? false
      :physical-tx-id 1466
      :transaction-ledger-matches? true
      :conditional-step {:conditional-argument-admissible? true}
      :composed-assurance
      {:scope collector/composed-step-scope
       :status (if agreement?
                 :locally-valid-and-observationally-consistent
                 :observation-mismatch)
       :local-argument-valid? true
       :physical-observations-correspond? agreement?
       :joint-evidence-consistent? agreement?
       :undischarged-premises
       (:undischarged-premises collector/composed-step-scope)
       :not-established (:not-established collector/composed-step-scope)
       :certifies-physical-execution? false
       :proves-internal-prestate? false
       :proves-general-refinement? false}})))

(defn- rejection [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-xtdb-transition-observation/rejection (ex-data e))
        (:reason (ex-data e))
        :unexpected-exception))))

(defn- diverge-on [raw check]
  (cond-> (-> raw
              (assoc-in [:comparison :checks check] false)
              (assoc-in [:comparison :corresponds?] false)
              (assoc-in [:composed-assurance :status] :observation-mismatch)
              (assoc-in [:composed-assurance :physical-observations-correspond?]
                        false)
              (assoc-in [:composed-assurance :joint-evidence-consistent?]
                        false))
    (#{:tx-ledger-record-present :tx-ledger-id :tx-ledger-commit-status
       :tx-ledger-system-time} check)
    (assoc :transaction-ledger-matches? false)))

(deftest definitive-outcomes-remain-distinct
  (doseq [[status agreement? classified-status]
          [[:committed true :bounded-committed-step-agreement]
           [:rejected true :bounded-rejected-step-agreement]
           [:committed false :bounded-observations-diverge]
           [:rejected false :bounded-observations-diverge]]]
    (let [report (physical/classify-controlled-report!
                  (raw-report status agreement?))]
      (is (= classified-status (:status report)))
      (is (= status (get-in report [:outcome :reported-status])))
      (is (= agreement?
             (get-in report [:abstract-correspondence
                             :observed-step-agreement?])))
      (is (= (= status :rejected)
             (get-in report [:outcome :recognized-assertion-rejection?])))
      (is (= :not-established
             (get-in report [:abstract-correspondence :physical-refinement])))
      (is (false? (get-in report [:outcome :physical-receipt-authenticated?]))))))

(deftest all-original-premises-remain-open
  (let [report (physical/classify-controlled-report! (raw-report))
        open (:open-obligations report)]
    (is (set/subset? (:undischarged-premises collector/composed-step-scope)
                     open))
    (is (set/subset? (:not-established collector/composed-step-scope) open))
    (is (set/subset?
         (:assumed collector/repeatable-ledger-window-collection-scope)
         open))
    (is (set/subset?
         (:not-established collector/repeatable-ledger-window-collection-scope)
         open))
    (is (set/subset? #{:production-node-and-handler-provenance
                       :physical-submission-payload-authenticity
                       :transaction-internal-read-snapshot-equivalence
                       :all-physical-writer-paths-accounted-for
                       :unknown-outcome-reconciliation
                       :universal-execution-refinement} open))
    (is (= :bounded-diagnostic-only (:proof-status report)))
    (is (= :controlled-collector-report-unattested
           (:observation-origin report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))
    (is (= :none (:transaction-authority report)))))

(deftest no-physical-identifiers-or-private-inputs-escape
  (let [classified (physical/classify-controlled-report! (raw-report))
        printed (pr-str classified)]
    (is (= #{:type :version :status :submission :outcome :readback
             :abstract-correspondence :open-obligations :observation-origin
             :scope :proof-status :certificate :admission
             :transaction-authority :authority}
           (set (keys classified))))
    (is (= 2 (get-in classified [:submission :operation-count])))
    (is (= :not-authenticated
           (get-in classified [:submission :physical-payload-provenance])))
    (is (= :not-established
           (get-in classified [:readback :transaction-internal-prestate])))
    (is (not (re-find #"1466" printed)))
    (is (not (re-find #"physical-tx-id|snapshot-token|:targets|:plan|:node"
                      printed)))
    (is (true? (get-in classified [:readback :pinned-phase-replay-agrees?])))))

(deftest top-level-mutation-rejected
  (let [base (raw-report)
        corrupt [(dissoc base :comparison)
                 (dissoc base :conditional-step)
                 (assoc base :certificate :issued)
                 (assoc base :certifies-physical-execution? true)
                 (assoc base :transaction-authority :issued)
                 (assoc base :physical-tx-id -1)
                 (assoc base :physical-tx-id "1466")
                 (assoc base :collected-target-count 0)
                 (assoc base :submitted-operation-count 257)
                 (assoc base :observation-basis :unbounded-latest)
                 (assoc base :evidence-kind :production-receipt)
                 (assoc base :collection-scope {})
                 (assoc base :transaction-ledger-matches? nil)
                 (assoc base :composed-assurance {})
                 (with-meta base {:proof :forged})
                 (assoc base :execution {:status :commit-unknown})]]
    (doseq [r corrupt]
      (is (= :malformed-or-inconsistent-controlled-report
             (rejection #(physical/classify-controlled-report! r)))))))

(deftest contradictory-ledger-and-outcome-evidence-rejected
  (let [base (raw-report)]
    (is (= :ledger-status-disagrees-with-checks
           (rejection #(physical/classify-controlled-report!
                        (assoc base :transaction-ledger-matches? false)))))
    (doseq [r [(assoc-in base [:comparison :checks :tx-ledger-id] false)
               (assoc-in base [:comparison :checks :tx-ledger-commit-status]
                         :unknown)
               (assoc-in base [:comparison :observed-status] :rejected)
               (assoc base :execution {:status :rejected})
               (assoc base :execution {:status :rejected
                                       :error/code :something-else})
               (assoc base :execution {:status :committed
                                       :physical-tx-id 123})
               (assoc-in base [:comparison :corresponds?] false)
               (assoc-in base [:composed-assurance :status]
                         :observation-mismatch)
               (assoc-in base [:composed-assurance
                               :physical-observations-correspond?] false)
               (assoc-in base [:composed-assurance :joint-evidence-consistent?]
                         false)]]
      (is (= :malformed-or-inconsistent-controlled-report
             (rejection #(physical/classify-controlled-report! r)))))
    (let [coherent-failed-ledger (diverge-on base :tx-ledger-id)]
      (is (= :bounded-observations-diverge
             (:status (physical/classify-controlled-report!
                       coherent-failed-ledger))))
      (is (false? (get-in (physical/classify-controlled-report!
                           coherent-failed-ledger)
                          [:outcome :ledger-row-agrees?]))))))

(deftest every-failed-comparison-check-defeats-positive-correspondence
  (let [base (raw-report)
        keys (sort (var-get #'physical/mandatory-comparison-checks))]
    (is (<= 10 (count keys)))
    (doseq [k keys]
      (let [report (physical/classify-controlled-report!
                    (diverge-on base k))]
        (is (= :bounded-observations-diverge (:status report))
            (str "observation must diverge for " k))
        (is (false? (get-in report [:abstract-correspondence
                                   :observed-step-agreement?])))))))

(deftest source-and-comparison-substitutions-rejected
  (let [base (raw-report)
        corrupt [(assoc-in base [:comparison :scope] :different)
                 (assoc-in base [:comparison :evidence-kind] :attested)
                 (assoc-in base [:comparison :certifies-physical-execution?]
                           true)
                 (assoc-in base [:comparison :checks :prestate] :unknown)
                 (update-in base [:comparison :checks] dissoc :poststate)
                 (assoc-in base [:comparison :recorded-op-count] 0)
                 (assoc-in base [:comparison :canonical-op-count] 257)
                 (assoc-in base [:comparison :mismatched-op-indices] [1])
                 (assoc-in base [:conditional-step
                                 :conditional-argument-admissible?] false)
                 (assoc-in base [:composed-assurance :scope] :different)
                 (assoc-in base [:composed-assurance :local-argument-valid?]
                           false)
                 (assoc-in base [:composed-assurance :certifies-physical-execution?]
                           true)
                 (assoc-in base [:composed-assurance :proves-general-refinement?]
                           true)
                 (assoc-in base [:composed-assurance :proves-internal-prestate?]
                           true)
                 (update-in base [:composed-assurance :not-established]
                            disj :transaction-internal-snapshot-equals-observed-prestate)
                 (update-in base [:composed-assurance :undischarged-premises]
                            disj (first (:undischarged-premises
                                         collector/composed-step-scope)))]]
    (doseq [r corrupt]
      (is (= :malformed-or-inconsistent-controlled-report
             (rejection #(physical/classify-controlled-report! r)))))))

(deftest physically-unattested-collector-reports-are-not-certificates
  (let [one (physical/classify-controlled-report! (raw-report))
        two (physical/classify-controlled-report!
             (assoc (raw-report) :physical-tx-id 1467))]
    ;; The pure classifier has no access to XTDB and cannot verify whether
    ;; either caller-supplied transaction ID was ever actually issued.
    (is (= one two))
    (is (= :none (:authority two)))
    (is (= :not-issued (:certificate two)))
    (is (= :not-authenticated
           (get-in two [:submission :physical-payload-provenance])))))

(deftest active-observer-uses-the-existing-collector-exactly-once
  (let [calls (atom [])
        input {:node ::private-node :plan ::private-plan
               :targets [[:requests "secret-target"]]}
        collector-fake (fn [received]
                         (swap! calls conj received)
                         (raw-report))]
    (with-redefs [collector/observe-and-derive-conditional! collector-fake
                  physical/original-collector collector-fake]
      (is (= :bounded-committed-step-agreement
             (:status (physical/observe-controlled-transition! input))))
      (is (= [input] @calls)))))

(deftest unknown-post-submission-outcome-propagates-without-retry
  (let [calls (atom 0)
        failure (ex-info "unknown physical transaction outcome"
                         {:error/type :commit-unknown
                          :possibly-committed true})
        collector-fake (fn [_] (swap! calls inc) (throw failure))]
    (with-redefs [collector/observe-and-derive-conditional! collector-fake
                  physical/original-collector collector-fake]
      (is (identical? failure
                      (try
                        (physical/observe-controlled-transition!
                         {:node ::node :plan ::plan :targets []})
                        (catch clojure.lang.ExceptionInfo e e))))
      (is (= 1 @calls)))))

(deftest input-validation-and-upstream-substitution-before-submission
  (let [calls (atom 0)
        collector-fake (fn [_] (swap! calls inc) (raw-report))]
    (with-redefs [collector/observe-and-derive-conditional! collector-fake
                  physical/original-collector collector-fake]
      (doseq [input [nil {} {:node ::node :plan ::plan}
                     {:node ::node :plan ::plan :targets [] :retry true}
                     (with-meta {:node ::node :plan ::plan :targets []}
                       {:forged :source})]]
        (is (= :invalid-observation-input
               (rejection #(physical/observe-controlled-transition! input)))))
      (is (zero? @calls))))
  ;; An unauthorized substitution of the upstream collector is detected even
  ;; when a submitted report would otherwise pass the pure shape checker.
  (with-redefs [collector/observe-and-derive-conditional!
                (fn [_] (raw-report))]
    (is (= :substituted-upstream-collector-or-scope
           (rejection #(physical/classify-controlled-report!
                        (raw-report)))))))

(deftest actual-embedded-xtdb-commit-and-assertion-rejection
  ;; One controlled embedded node; the second identical create triggers a
  ;; real XTDB2 assertion rejection. Neither outcome authenticates a deployed
  ;; production handler or a general physical refinement theorem.
  (with-open [node (xtn/start-node {})]
    (let [revision {:revision-key :revision
                    :created-at-key :created-at
                    :updated-at-key :updated-at}
          document {:xt/id "v1466-physical-private-target"
                    :revision 0 :created-at "t0" :updated-at "t0"
                    :owner "private-v1466-owner"}
          plan {:commands [(command/create :requests document revision)]
                :guards [] :emit false}
          input {:node node
                 :plan plan
                 :targets [[:requests (:xt/id document)]]}
          committed (physical/observe-controlled-transition! input)
          rejected (physical/observe-controlled-transition! input)]
      (is (= :bounded-committed-step-agreement (:status committed)))
      (is (= :bounded-rejected-step-agreement (:status rejected)))
      (is (= :committed (get-in committed [:outcome :reported-status])))
      (is (= :rejected (get-in rejected [:outcome :reported-status])))
      (is (false? (get-in committed [:outcome
                                    :recognized-assertion-rejection?])))
      (is (true? (get-in rejected [:outcome
                                   :recognized-assertion-rejection?])))
      (doseq [report [committed rejected]]
        (is (true? (get-in report [:readback :pinned-phase-replay-agrees?])))
        (is (true? (get-in report [:outcome :ledger-row-agrees?])))
        (is (false? (get-in report [:outcome :physical-receipt-authenticated?])))
        (is (= :none (:authority report)))
        (is (= :not-issued (:certificate report)))
        (is (= :not-established
               (get-in report [:readback :transaction-internal-prestate])))
        (is (contains? (:open-obligations report)
                       :production-node-and-handler-provenance))
        (is (not (re-find #"private-v1466-owner|v1466-physical-private-target"
                          (pr-str report))))))))

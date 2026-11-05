(ns gesso.assurance.proof.controlled-xtdb-frame-acquisition-test
  "v1418: Permanent adversarial regressions for bounded XTDB finite-frame
   acquisition. These tests distinguish Gesso-issued pre/post XTDB reads and
   one process-local adapter capture from authenticated transaction causality,
   shared-snapshot consistency, universal writer closure, or frame proof.
   In particular, a read failure AFTER submission must never retry the tx."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.controlled-xtdb-frame-acquisition :as subject]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.live.consistency.xtdb :as live]
            [gesso.assurance.proof.finite-physical-frame-observation :as frame]
            [gesso.assurance.proof.captured-write-domain-correspondence :as captured]
            [gesso.assurance.proof.protected-write-domain :as domain]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- scenario
  ([] (scenario :all 1))
  ([field-domain frame-count]
   (let [write-id (str "v1418-write-" (random-uuid))
         frame-ids (mapv #(str "v1418-frame-" % "-" (random-uuid))
                         (range frame-count))
         doc {:xt/id write-id :doc/rev 0 :doc/created "BEFORE"
              :doc/updated "BEFORE" :doc/value "SENSITIVE_WRITE_1418"}
         plan {:commands [(command/create :document doc
                            {:revision-key :doc/rev :created-at-key :doc/created
                             :updated-at-key :doc/updated})]
               :emit false}
         source {:plan plan
                 :protected-documents
                 (into [{:target [:document write-id] :fields :all}]
                       (map (fn [id] {:target [:document id] :fields field-domain})
                            frame-ids))}]
     {:write-id write-id
      :frame-ids frame-ids
      :source source
      :ops (:tx-ops (tx/prepare nil plan))})))

(defn- outcome-reason [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:reason (ex-data e)))))

(defn- acquire [db {:keys [source ops]} submit!]
  (subject/acquire-controlled-xtdb-frame!
   {:node db :domain-source source
    :submit! (or submit! (fn [n] (live/execute-tx! n ops)))}))

(defn- seed! [db id & [extra]]
  (xt/execute-tx db [[:put-docs :document
                      (merge {:xt/id id :doc/locked 7
                              :doc/free "SENSITIVE_FRAME_1418"} extra)]]))

(defn- assert-no-authority! [r]
  (is (= :not-established (:physical-frame-for-protected-documents r)))
  (is (= :not-established (:physical-frame-outside-enumeration r)))
  (is (= :not-established (:transaction-internal-prestate r)))
  (is (= :not-established (:transaction-causality r)))
  (is (= :not-established (:transaction-outcome r)))
  (is (= :not-established (:complete-writer-coverage r)))
  (is (= :not-established (:each-phase-shared-snapshot? r)))
  (is (false? (:node-identity-authenticated? r)))
  (is (false? (:read-basis-authenticated? r)))
  (is (false? (:submission-node-use-authenticated? r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:retry-authority r)))
  (is (= :none (:settlement-authority r))))

(defn- assert-rechecks! [r]
  (is (= r (subject/require-current-controlled-xtdb-frame! (:source r) r)))
  (let [s (subject/redacted-summary r)
        rendered (pr-str s)]
    (is (= (dissoc r :source) s))
    (is (not (contains? s :source)))
    (is (not (re-find #"SENSITIVE_WRITE_1418|SENSITIVE_FRAME_1418|:doc/locked|:doc/free|v1418-frame-|v1418-write-" rendered)))))

(deftest real-xtdb-unchanged-whole-document-frame
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          _ (seed! db (first frame-ids))
          seen-node (atom nil)
          attempts (atom 0)
          r (acquire db sc (fn [n] (reset! seen-node n)
                              (swap! attempts inc)
                              (live/execute-tx! n (:ops sc))))]
      (is (identical? db @seen-node))
      (is (= 1 @attempts))
      (is (= :bounded-queried-protected-frame-agrees (:status r)))
      (is (= :bounded-captured-adapter-operations-agree (:capture-status r)))
      (is (= :bounded-supplied-protected-frame-agrees (:frame-status r)))
      (is (= 1 (:frame-only-target-count r)))
      (is (= 2 (:read-query-count r)))
      (is (= 1 (:captured-transaction-call-count r)))
      (is (empty? (:changed-frame-target-indices r)))
      (is (= :pre-reads-then-captured-invocation-then-post-reads (:read-sequence r)))
      (is (= :gesso-issued-xtdb-query-on-caller-node (:read-origin r)))
      (assert-rechecks! r)
      (assert-no-authority! r))))

(deftest real-xtdb-multiple-targets-including-absent-document
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario :all 3)
          _ (seed! db (first frame-ids))
          _ (seed! db (last frame-ids) {:doc/more {:nested "PRIVATE"}})
          r (acquire db sc nil)]
      (is (= :bounded-queried-protected-frame-agrees (:status r)))
      (is (= 3 (:frame-only-target-count r)))
      (is (= 6 (:read-query-count r)))
      (is (= [nil] (mapv :document (-> r :source :frame-input :frame-observation :pre-rows (subvec 1 2)))))
      (assert-rechecks! r)
      (assert-no-authority! r))))

(deftest raw-xtdb-write-to-frame-is-visible-without-causal-attribution
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids ops] :as sc} (scenario)
          id (first frame-ids)
          _ (seed! db id)
          r (acquire db sc (fn [n]
                             (xt/execute-tx n [[:put-docs :document
                                                {:xt/id id :doc/locked 8}]])
                             (live/execute-tx! n ops)))]
      (is (= :bounded-queried-protected-frame-diverges (:status r)))
      (is (= [0] (:changed-frame-target-indices r)))
      (is (pos? (:protected-field-difference-count r)))
      (is (= 1 (:captured-transaction-call-count r)))
      (is (= :bounded-captured-adapter-operations-agree (:capture-status r)))
      (assert-rechecks! r)
      (assert-no-authority! r))))

(deftest direct-untracked-write-to-declared-command-document-is-not-writer-closure
  (with-open [db (xtn/start-node {})]
    (let [{:keys [ops write-id] :as sc} (scenario)
          r (acquire db sc (fn [n]
                             (live/execute-tx! n ops)
                             (xt/execute-tx n [[:put-docs :document
                                                {:xt/id write-id :doc/free "UNTRACKED"}]])))]
      (is (= :bounded-queried-protected-frame-agrees (:status r)))
      (is (= 1 (:captured-transaction-call-count r)))
      (is (= :not-established (:complete-writer-coverage r)))
      (assert-no-authority! r))))

(deftest changes-outside-partially-protected-fields-do-not-trigger-frame-change
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids ops] :as sc} (scenario #{:doc/locked} 1)
          id (first frame-ids)
          _ (seed! db id)
          r (acquire db sc (fn [n]
                             (xt/execute-tx n [[:put-docs :document
                                                {:xt/id id :doc/locked 7
                                                 :doc/free "CHANGED-UNPROTECTED"}]])
                             (live/execute-tx! n ops)))]
      (is (= :bounded-queried-protected-frame-agrees (:status r)))
      (is (= 0 (:protected-field-difference-count r)))
      (is (empty? (:changed-frame-target-indices r)))
      (assert-no-authority! r))))

(deftest changes-to-partially-protected-fields-are-detected
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids ops] :as sc} (scenario #{:doc/locked} 1)
          id (first frame-ids)
          _ (seed! db id)
          r (acquire db sc (fn [n]
                             (xt/execute-tx n [[:put-docs :document
                                                {:xt/id id :doc/locked 9}]])
                             (live/execute-tx! n ops)))]
      (is (= :bounded-queried-protected-frame-diverges (:status r)))
      (is (= [0] (:changed-frame-target-indices r)))
      (is (pos? (:protected-field-difference-count r)))
      (assert-no-authority! r))))

(deftest whole-document-creation-is-frame-change
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids ops] :as sc} (scenario)
          id (first frame-ids)
          r (acquire db sc (fn [n]
                             (seed! n id)
                             (live/execute-tx! n ops)))]
      (is (= :bounded-queried-protected-frame-diverges (:status r)))
      (is (= [0] (:changed-frame-target-indices r)))
      (assert-no-authority! r))))

(deftest callback-throws-after-commit-is-unknown-not-retried
  (with-open [db (xtn/start-node {})]
    (let [{:keys [write-id ops] :as sc} (scenario)
          attempts (atom 0)
          r (acquire db sc (fn [n]
                             (swap! attempts inc)
                             (live/execute-tx! n ops)
                             (throw (ex-info "simulated lost response" {}))))]
      (is (= 1 @attempts))
      (is (true? (:submission-threw? r)))
      (is (= :submission-threw-physical-outcome-unknown (:status r)))
      (is (some? (first (xt/q db ["SELECT * FROM document WHERE _id = ?" write-id]))))
      (assert-rechecks! r)
      (assert-no-authority! r))))

(deftest failed-physical-submit-call-preserves-uncertainty
  (with-open [db (xtn/start-node {})]
    (let [{:keys [ops] :as sc} (scenario)
          attempts (atom 0)
          r (acquire db sc (fn [n]
                             (swap! attempts inc)
                             (try
                               (live/execute-tx! n [[:put-docs :document {:xt/id "bad"}]
                                                    ["ASSERT FALSE"]])
                               (catch Throwable e (throw e)))))]
      (is (= 1 @attempts))
      (is (= :submission-threw-physical-outcome-unknown (:status r)))
      (assert-no-authority! r))))

(deftest no-adapter-call-fails-closed-even-if-raw-xtdb-write-committed
  (with-open [db (xtn/start-node {})]
    (let [{:keys [ops write-id] :as sc} (scenario)
          attempts (atom 0)
          reason (outcome-reason
                  #(acquire db sc (fn [n] (swap! attempts inc)
                                    (xt/execute-tx n ops))))]
      (is (= :not-exactly-one-observed-adapter-call reason))
      (is (= 1 @attempts))
      (is (some? (first (xt/q db ["SELECT * FROM document WHERE _id = ?" write-id])))))))

(deftest multiple-adapter-calls-cannot-be-selected-opportunistically
  (with-open [db (xtn/start-node {})]
    (let [{:keys [ops] :as sc} (scenario)
          attempts (atom 0)
          reason (outcome-reason
                  #(acquire db sc (fn [n]
                                    (swap! attempts inc)
                                    (live/execute-tx! n ops)
                                    (live/execute-tx! n ops))))]
      (is (= 1 @attempts))
      (is (= :not-exactly-one-observed-adapter-call reason)))))

(deftest wrong-adapter-operations-do-not-produce-agreement
  (with-open [db (xtn/start-node {})]
    (let [{:keys [ops] :as sc} (scenario)
          r (acquire db sc (fn [n]
                             (live/execute-tx! n (conj ops ["ASSERT TRUE"]))))]
      (is (= :adapter-or-frame-comparison-unavailable (:status r)))
      (is (= :captured-operations-diverge (:capture-status r)))
      (is (= :adapter-plan-correspondence-unavailable (:frame-status r)))
      (assert-no-authority! r))))

(deftest unsupported-domain-rejected-before-submission
  (with-open [db (xtn/start-node {})]
    (let [{:keys [source] :as sc} (scenario)
          calls (atom 0)
          trimmed (assoc source :protected-documents [(first (:protected-documents source))])]
      (is (= :no-supported-finite-frame-targets
             (outcome-reason #(subject/acquire-controlled-xtdb-frame!
                              {:node db :domain-source trimmed
                               :submit! (fn [_] (swap! calls inc))}))))
      (is (= 0 @calls)))))

(deftest disallowed-table-identifier-rejected-before-submission
  (with-open [db (xtn/start-node {})]
    (let [{:keys [source]} (scenario)
          calls (atom 0)
          altered (assoc-in source [:protected-documents 1 :target 0]
                            :schema/document)]
      (is (= :unsupported-sql-table-or-target
             (outcome-reason #(subject/acquire-controlled-xtdb-frame!
                              {:node db :domain-source altered
                               :submit! (fn [_] (swap! calls inc))}))))
      (is (= 0 @calls)))))

(deftest malformed-input-fails-without-submission
  (let [{:keys [source]} (scenario)
        calls (atom 0)
        submit! (fn [_] (swap! calls inc))]
    (doseq [bad [{:node nil :domain-source source :submit! submit!}
                 {:node :unused :domain-source source :submit! :not-a-function}
                 {:node :unused :domain-source source :submit! submit! :extra :forged}
                 (with-meta {:node :unused :domain-source source :submit! submit!}
                   {:certified true})]]
      (is (#{:missing-node-or-submit-function :invalid-map-shape}
           (outcome-reason #(subject/acquire-controlled-xtdb-frame! bad)))))
    (is (= 0 @calls))))

(deftest physical-pre-read-failure-prevents-submission
  (let [{:keys [source]} (scenario)
        calls (atom 0)]
    (is (= :physical-read-failed
           (outcome-reason #(subject/acquire-controlled-xtdb-frame!
                            {:node :not-a-node :domain-source source
                             :submit! (fn [_] (swap! calls inc))}))))
    (is (= 0 @calls))))

(deftest post-read-failure-after-submit-does-not-resubmit
  (let [db (xtn/start-node {})
        {:keys [ops] :as sc} (scenario)
        calls (atom 0)]
    (try
      (is (= :physical-read-failed
             (outcome-reason
              #(acquire db sc (fn [n]
                                (swap! calls inc)
                                (live/execute-tx! n ops)
                                (.close n))))))
      (is (= 1 @calls))
      (finally (.close db)))))

(deftest malformed-recheck-and-forged-authority-are-rejected
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario) nil)
          s (:source r)]
      (doseq [bad [(assoc r :authority :granted)
                   (assoc r :certificate :issued)
                   (assoc r :physical-frame-for-protected-documents :proved)
                   (assoc r :read-query-count 0)
                   (assoc r :status :certified)
                   (with-meta r {:approved true})]]
        (is (= :stale-or-forged-acquisition-report
               (outcome-reason #(subject/require-current-controlled-xtdb-frame! s bad)))))
      (is (= :invalid-map-shape
             (outcome-reason #(subject/require-current-controlled-xtdb-frame!
                              (assoc s :extra :forged) r)))))))

(deftest nested-frame-or-captured-report-tampering-is-detected
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario) nil)
          s (:source r)]
      (is (= :stale-or-forged-frame-report
             (outcome-reason
              #(subject/require-current-controlled-xtdb-frame!
                (assoc-in s [:frame-report :authority] :granted) r))))
      (is (= :stale-or-forged-comparison-report
             (outcome-reason
              #(subject/require-current-controlled-xtdb-frame!
                (assoc-in s [:captured-report :authority] :granted) r)))))))

(deftest altered-read-documents-cannot-recheck-original-report
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario) nil)
          s (:source r)]
      (is (some? (outcome-reason
                  #(subject/require-current-controlled-xtdb-frame!
                    (assoc-in s [:frame-input :frame-observation :post-rows 0 :document]
                              {:doc/locked "FORGED"}) r)))))))

(deftest validator-and-query-replacement-fail-closed
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          r (acquire db sc nil)
          cases [[#'xt/q (fn [& _] [])]
                 [#'live/capture-tx-api-calls! (fn [& _] :fake)]
                 [#'frame/compare-finite-physical-frame! (fn [& _] :fake)]
                 [#'captured/compare-captured-write-domain! (fn [& _] :fake)]
                 [#'domain/derive-protected-write-domain! (fn [& _] :fake)]]]
      (doseq [[v f] cases]
        (is (= :substituted-acquisition-or-checker
               (with-redefs-fn {v f}
                 #(outcome-reason
                   (fn [] (subject/require-current-controlled-xtdb-frame!
                           (:source r) r))))))))))

(deftest redacted-summary-omits-sensitive-arguments-and-rows
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          _ (seed! db (first frame-ids))
          r (acquire db sc nil)
          summary (subject/redacted-summary r)
          rendered (pr-str summary)]
      (is (not (contains? summary :source)))
      (is (= 2 (:read-query-count summary)))
      (is (not (re-find #"SENSITIVE_WRITE_1418|SENSITIVE_FRAME_1418|v1418-frame-|v1418-write-|:doc/locked" rendered)))
      (assert-no-authority! summary))))

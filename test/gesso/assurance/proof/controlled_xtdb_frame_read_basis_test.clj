(ns gesso.assurance.proof.controlled-xtdb-frame-read-basis-test
  "v1420: Permanent adversarial regression for the v1419 pinned XTDB
   pre/post observation phases and transaction-time diagnostics.

   Real XTDB tests exercise the actual read and submission paths. The
   deliberately rebased tokens in other tests are SYNTHETIC caller-origin
   observations: they exercise conservative diagnostics, not database
   attestation or transaction-internal snapshot provenance. None of these
   tests certifies writer closure, physical refinement, retry or settlement."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.controlled-xtdb-frame-acquisition :as subject]
            [gesso.assurance.proof.finite-physical-frame-observation :as frame]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- scenario
  ([] (scenario 2))
  ([n]
   (let [write-id (str "v1420-write-" (random-uuid))
         frame-ids (mapv (fn [_] (str "v1420-frame-" (random-uuid))) (range n))
         document {:xt/id write-id :doc/rev 0 :doc/created "BEFORE"
                   :doc/updated "BEFORE" :doc/value "PRIVATE_WRITE_1420"}
         plan {:commands [(command/create :document document
                            {:revision-key :doc/rev :created-at-key :doc/created
                             :updated-at-key :doc/updated})]
               :emit false}]
     {:write-id write-id :frame-ids frame-ids
      :domain-source {:plan plan
                      :protected-documents
                      (into [{:target [:document write-id] :fields :all}]
                            (map (fn [id] {:target [:document id] :fields :all})
                                 frame-ids))}
      :ops (:tx-ops (tx/prepare nil plan))})))

(defn- seed! [db id]
  (xt/execute-tx db [[:put-docs :document
                      {:xt/id id :doc/fixed 10 :doc/free "PRIVATE_FRAME_1420"}]]))

(defn- acquire [db {:keys [domain-source ops]}]
  (subject/acquire-controlled-xtdb-frame!
   {:node db :domain-source domain-source
    :submit! (fn [node] (live/execute-tx! node ops))}))

(defn- reason [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- observation [report]
  (get-in report [:source :frame-input :frame-observation]))

(defn- observed-tx-time [report]
  (let [tx-key (get-in report [:source :frame-input :captured-source
                               :capture-report :events 1 :tx-result])]
    (:system-time (live/tx-result-consistency tx-key))))

(defn- token-at [existing-token instant]
  (let [decoded (basis/<-time-basis-str existing-token)]
    (basis/->time-basis-str {(key (first decoded)) [instant]})))

(defn- rebase-synthetic-report
  "Replace only claimed bases, recomputing dependent supplied-frame evidence.
   This intentionally does NOT redo any XTDB reads, and cannot establish
   that the returned basis was the one physically used."
  [report pre-token post-token]
  (let [source (-> (:source report)
                   (assoc-in [:frame-input :frame-observation :reported-pre-read-basis]
                             pre-token)
                   (assoc-in [:frame-input :frame-observation :reported-post-read-basis]
                             post-token))
        source (assoc source :frame-report
                      (frame/compare-finite-physical-frame! (:frame-input source)))]
    (#'subject/assemble! source)))

(defn- assert-no-physical-authority! [report]
  (is (= :not-established (:transaction-internal-prestate report)))
  (is (= :not-established (:transaction-causality report)))
  (is (= :not-established (:transaction-outcome report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-established (:physical-frame-for-protected-documents report)))
  (is (= :not-established (:physical-frame-outside-enumeration report)))
  (is (= :not-established (:each-phase-shared-snapshot? report)))
  (is (false? (:read-basis-authenticated? report)))
  (is (false? (:node-identity-authenticated? report)))
  (is (false? (:submission-node-use-authenticated? report)))
  (is (false? (:public-tx-time-is-causal-proof? report)))
  (is (= :none (:retry-authority report)))
  (is (= :none (:settlement-authority report)))
  (is (= :none (:transaction-authority report)))
  (is (= :none (:authority report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report))))

(deftest real-xtdb-pins-both-multi-document-read-phases
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario 3)
          _ (doseq [id (take 2 frame-ids)] (seed! db id))
          report (acquire db sc)
          before (:reported-pre-read-basis (observation report))
          after (:reported-post-read-basis (observation report))]
      (is (string? before))
      (is (string? after))
      (is (map? (basis/<-time-basis-str before)))
      (is (map? (basis/<-time-basis-str after)))
      (is (= 3 (:frame-only-target-count report)))
      (is (= 6 (:read-query-count report)))
      (is (= 2 (:phase-snapshot-token-query-count report)))
      (is (true? (:per-phase-document-queries-use-pinned-token? report)))
      (is (true? (:per-phase-fixed-valid-time? report)))
      (is (= :pre-at-or-before-post (:phase-snapshot-time-order report)))
      (is (= :reported-transaction-time-between-phase-bases
             (:reported-transaction-time-order report)))
      (is (true? (:phase-token-time-comparable? report)))
      (is (= :bounded-queried-protected-frame-agrees (:status report)))
      (is (nil? (get-in (observation report) [:pre-rows 2 :document])))
      (is (nil? (get-in (observation report) [:post-rows 2 :document])))
      (is (= report (subject/require-current-controlled-xtdb-frame!
                     (:source report) report)))
      (assert-no-physical-authority! report)
      (let [summary (subject/redacted-summary report)
            printed (pr-str summary)]
        (is (= (dissoc report :source) summary))
        (is (not (contains? summary :source)))
        (is (not (.contains printed before)))
        (is (not (.contains printed after)))
        (is (not (re-find #"PRIVATE_(WRITE|FRAME)_1420|v1420-frame-|v1420-write-" printed)))
        (assert-no-physical-authority! summary)))))

(deftest real-submit-tx-has-no-complete-transaction-time
  (with-open [db (xtn/start-node {})]
    (let [{:keys [domain-source ops]} (scenario)
          attempts (atom 0)
          report (subject/acquire-controlled-xtdb-frame!
                  {:node db :domain-source domain-source
                   :submit! (fn [n] (swap! attempts inc)
                              (live/submit-tx! n ops))})]
      (is (= 1 @attempts))
      (is (= :no-complete-reported-transaction-time
             (:reported-transaction-time-order report)))
      (is (#{:pre-at-or-before-post :not-comparable}
           (:phase-snapshot-time-order report))
          "submit-tx! has no complete tx time and a phase token may be non-comparable")
      (is (= :bounded-queried-protected-frame-agrees (:status report)))
      (is (= report (subject/require-current-controlled-xtdb-frame!
                     (:source report) report)))
      (assert-no-physical-authority! report))))

(deftest real-intervening-raw-writer-is-visible-but-not-attributed
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids ops domain-source]} (scenario)
          id (first frame-ids)
          _ (seed! db id)
          report (subject/acquire-controlled-xtdb-frame!
                  {:node db :domain-source domain-source
                   :submit! (fn [n]
                              ;; This physical write is outside the captured adapter.
                              (xt/execute-tx n [[:put-docs :document
                                                 {:xt/id id :doc/fixed 11}]])
                              (live/execute-tx! n ops))})]
      (is (= :bounded-queried-protected-frame-diverges (:status report)))
      (is (= [0] (:changed-frame-target-indices report)))
      (is (= 1 (:captured-transaction-call-count report)))
      (is (= :pre-at-or-before-post (:phase-snapshot-time-order report)))
      (is (= :reported-transaction-time-between-phase-bases
             (:reported-transaction-time-order report)))
      (assert-no-physical-authority! report))))

(deftest returned-frame-report-does-not-monitor-later-writes
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          id (first frame-ids)
          _ (seed! db id)
          report (acquire db sc)
          _ (xt/execute-tx db [[:put-docs :document
                                {:xt/id id :doc/fixed 999}]])]
      (is (= :bounded-queried-protected-frame-agrees (:status report)))
      (is (= report (subject/require-current-controlled-xtdb-frame!
                     (:source report) report)))
      (is (= 999 (:doc/fixed (first (xt/q db ["SELECT * FROM document WHERE _id = ?" id])))))
      (assert-no-physical-authority! report))))

(deftest lost-response-after-real-commit-does-not-grant-retry
  (with-open [db (xtn/start-node {})]
    (let [{:keys [domain-source ops write-id]} (scenario)
          attempts (atom 0)
          report (subject/acquire-controlled-xtdb-frame!
                  {:node db :domain-source domain-source
                   :submit! (fn [node]
                              (swap! attempts inc)
                              (live/execute-tx! node ops)
                              (throw (ex-info "response lost AFTER submission"
                                              {:private-data "PRIVATE_WRITE_1420"})))})]
      (is (= 1 @attempts))
      (is (true? (:submission-threw? report)))
      (is (= :submission-threw-physical-outcome-unknown (:status report)))
      (is (some? (first (xt/q db ["SELECT * FROM document WHERE _id = ?" write-id]))))
      (assert-no-physical-authority! report)
      (is (not (.contains (pr-str (subject/redacted-summary report))
                          "PRIVATE_WRITE_1420"))))))

(deftest regressed-synthetic-read-bases-are-diagnosed-not-certified
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          t (observed-tx-time r)
          original (observation r)
          before (token-at (:reported-pre-read-basis original) (.plusSeconds t 30))
          after (token-at (:reported-post-read-basis original) (.minusSeconds t 30))
          altered (rebase-synthetic-report r before after)]
      (is (instance? Instant t))
      (is (= :pre-after-post (:phase-snapshot-time-order altered)))
      (is (= :pre-phase-after-reported-transaction-time
             (:reported-transaction-time-order altered)))
      (is (= :phase-snapshot-time-regressed (:status altered)))
      (is (true? (:phase-token-time-comparable? altered)))
      (is (= altered (subject/require-current-controlled-xtdb-frame!
                      (:source altered) altered)))
      (assert-no-physical-authority! altered))))

(deftest pre-read-synthetically-after-reported-transaction-is-contradiction
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          t (observed-tx-time r)
          original (observation r)
          before (token-at (:reported-pre-read-basis original) (.plusSeconds t 10))
          after (token-at (:reported-post-read-basis original) (.plusSeconds t 20))
          altered (rebase-synthetic-report r before after)]
      (is (= :pre-at-or-before-post (:phase-snapshot-time-order altered)))
      (is (= :pre-phase-after-reported-transaction-time
             (:reported-transaction-time-order altered)))
      (is (= :phase-snapshot-time-contradicts-reported-transaction
             (:status altered)))
      (assert-no-physical-authority! altered))))

(deftest post-read-synthetically-before-reported-transaction-is-contradiction
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          t (observed-tx-time r)
          original (observation r)
          before (token-at (:reported-pre-read-basis original) (.minusSeconds t 20))
          after (token-at (:reported-post-read-basis original) (.minusSeconds t 10))
          altered (rebase-synthetic-report r before after)]
      (is (= :pre-at-or-before-post (:phase-snapshot-time-order altered)))
      (is (= :post-phase-before-reported-transaction-time
             (:reported-transaction-time-order altered)))
      (is (= :phase-snapshot-time-contradicts-reported-transaction
             (:status altered)))
      (assert-no-physical-authority! altered))))

(deftest equal-phase-bases-do-not-establish-causality
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          t (observed-tx-time r)
          original (observation r)
          token (token-at (:reported-pre-read-basis original) t)
          altered (rebase-synthetic-report r token token)]
      (is (= :pre-at-or-before-post (:phase-snapshot-time-order altered)))
      (is (= :reported-transaction-time-between-phase-bases
             (:reported-transaction-time-order altered)))
      (is (= :bounded-queried-protected-frame-agrees (:status altered)))
      (assert-no-physical-authority! altered))))

(deftest valid-but-multidatabase-synthetic-basis-is-not-comparable
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          original (observation r)
          decoded (basis/<-time-basis-str (:reported-pre-read-basis original))
          t (observed-tx-time r)
          token (basis/->time-basis-str (assoc decoded "gesso_additional_db" [t]))
          altered (rebase-synthetic-report r token
                                           (:reported-post-read-basis original))]
      (is (= :not-comparable (:phase-snapshot-time-order altered)))
      (is (= :phase-time-not-comparable
             (:reported-transaction-time-order altered)))
      (is (false? (:phase-token-time-comparable? altered)))
      (is (= :bounded-queried-protected-frame-agrees (:status altered)))
      (assert-no-physical-authority! altered))))

(deftest invalid-phase-token-types-rejected-during-recheck
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          source (:source r)]
      (doseq [phase [:reported-pre-read-basis :reported-post-read-basis]
              invalid [nil "" 0 false]]
        (is (= :missing-or-invalid-phase-snapshot-token
               (reason #(subject/require-current-controlled-xtdb-frame!
                         (assoc-in source [:frame-input :frame-observation phase]
                                   invalid) r)))))
      (doseq [phase [:reported-pre-read-basis :reported-post-read-basis]
              invalid ["FORGED-TOKEN" "this-is-not-an-xtdb-time-basis"]]
        (is (= :unreadable-phase-snapshot-token
               (reason #(subject/require-current-controlled-xtdb-frame!
                         (assoc-in source [:frame-input :frame-observation phase]
                                   invalid) r))))))))

(deftest changing-basis-without-updating-dependent-evidence-fails-closed
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))
          t (observed-tx-time r)
          old (observation r)
          changed (token-at (:reported-post-read-basis old) (.plusSeconds t 90))]
      (is (= :stale-or-forged-frame-report
             (reason #(subject/require-current-controlled-xtdb-frame!
                       (assoc-in (:source r)
                                 [:frame-input :frame-observation
                                  :reported-post-read-basis] changed) r)))))))

(deftest changing-reported-timing-or-authority-is-detected
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))]
      (doseq [[field value]
              [[:phase-snapshot-time-order :certified]
               [:reported-transaction-time-order :certified]
               [:phase-token-time-comparable? :proved]
               [:per-phase-document-queries-use-pinned-token? false]
               [:phase-snapshot-token-query-count 1]
               [:transaction-internal-prestate :proved]
               [:transaction-causality :authenticated]
               [:retry-authority :allowed]
               [:settlement-authority :allowed]
               [:certificate :issued]]]
        (is (= :stale-or-forged-acquisition-report
               (reason #(subject/require-current-controlled-xtdb-frame!
                         (:source r) (assoc r field value)))))))))

(deftest unauthorized-substitutions-of-snapshot-code-are-detected
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))]
      (doseq [[the-var replacement]
              [[#'xt/q (fn [& _] [])]
               [#'basis/<-time-basis-str (fn [& _] {})]
               [#'live/tx-result-consistency (fn [& _] {})]]]
        (is (= :substituted-acquisition-or-checker
               (with-redefs-fn {the-var replacement}
                 #(reason (fn []
                            (subject/require-current-controlled-xtdb-frame!
                             (:source r) r))))))))))

(deftest malformed-report-and-source-are-rejected
  (with-open [db (xtn/start-node {})]
    (let [r (acquire db (scenario))]
      (is (= :invalid-map-shape
             (reason #(subject/require-current-controlled-xtdb-frame!
                       (assoc (:source r) :extra :forged) r))))
      (is (= :stale-or-forged-acquisition-report
             (reason #(subject/require-current-controlled-xtdb-frame!
                       (:source r) (with-meta r {:proof true}))))))))

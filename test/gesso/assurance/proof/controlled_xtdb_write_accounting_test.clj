(ns gesso.assurance.proof.controlled-xtdb-write-accounting-test
  "v1422: Permanent adversarial regression for v1421 controlled XTDB write
   accounting. Actual XTDB queries and writes are distinguished from SYNTHETIC
   tampering of prior observations. Neither kind establishes transaction-
   internal prestate, authenticated causality, writer closure, or a global
   physical frame. All submission callbacks are single-attempt; NEVER retry."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.controlled-xtdb-write-accounting :as subject]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- scenario
  ([] (scenario 2))
  ([frame-count]
   (let [write-id (str "v1422-WRITE-PRIVATE-" (random-uuid))
         frame-ids (mapv (fn [_] (str "v1422-FRAME-PRIVATE-" (random-uuid)))
                         (range frame-count))
         document {:xt/id write-id :doc/rev 0 :doc/created "BEFORE"
                   :doc/updated "BEFORE" :doc/value "PRIVATE_VALUE_1422"}
         plan {:commands [(command/create :document document
                            {:revision-key :doc/rev :created-at-key :doc/created
                             :updated-at-key :doc/updated})]
               :emit false}]
     {:write-id write-id :frame-ids frame-ids
      :source {:plan plan
               :protected-documents
               (into [{:target [:document write-id] :fields :all}]
                     (map (fn [id] {:target [:document id] :fields :all})
                          frame-ids))}
      :ops (:tx-ops (tx/prepare nil plan))})))

(defn- seed! [db id document]
  (xt/execute-tx db [[:put-docs :document (assoc document :xt/id id)]]))

(defn- fetch [db id]
  (first (xt/q db ["SELECT * FROM document WHERE _id = ?" id])))

(defn- acquire!
  [db sc submit!]
  (subject/acquire-controlled-xtdb-write-accounting!
   {:node db :domain-source (:source sc)
    :submit! (fn [n] (submit! n (:ops sc)))}))

(defn- ordinary! [db sc]
  (acquire! db sc (fn [n ops] (live/execute-tx! n ops))))

(defn- rejection [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- no-authority! [r]
  (is (= :not-established (:actual-commit-causality r)))
  (is (= :not-established (:transaction-internal-prestate r)))
  (is (= :not-established (:all-writer-paths-covered? r)))
  (is (= :not-established (:complete-physical-write-footprint r)))
  (is (= :not-established (:global-physical-frame r)))
  (is (false? (:read-basis-authenticated? r)))
  (is (false? (:node-identity-authenticated? r)))
  (is (false? (:submission-node-use-authenticated? r)))
  (is (false? (:actual-submitted-ops-authenticated? r)))
  (is (= :bounded-local-write-and-frame-diagnostic-only (:proof-status r)))
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (doseq [k [:retry-authority :settlement-authority :transaction-authority :authority]]
    (is (= :none (get r k)))))

(defn- recheck! [report]
  (subject/require-current-controlled-xtdb-write-accounting! (:source report) report))

(defn- synthetic-from
  "Recompute from a substituted *caller-supplied* observation. These are NOT
   newly acquired XTDB reads and cannot establish physical provenance."
  [report f]
  (#'subject/assemble! (f (:source report))))

(defn- token-at [token t]
  (let [decoded (basis/<-time-basis-str token)]
    (basis/->time-basis-str {(key (first decoded)) [t]})))

(deftest actual-xtdb-committed-command-and-several-frame-documents
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids write-id] :as sc} (scenario 3)
          _ (seed! db (first frame-ids) {:doc/lock 7 :doc/secret "FRAME_SECRET_1422"})
          _ (seed! db (second frame-ids) {:doc/lock 8})
          report (ordinary! db sc)
          source (:source report)
          summary (subject/redacted-summary report)]
      (is (= :bounded-queried-write-and-frame-agreement (:status report)))
      (is (= :bounded-captured-adapter-operations-agree (:captured-write-status report)))
      (is (= :bounded-supplied-protected-frame-agrees (:finite-frame-status report)))
      (is (= 1 (:write-target-count report)))
      (is (= 3 (:frame-target-count report)))
      (is (= 4 (:protected-target-count report)))
      (is (= 8 (:read-query-count report)))
      (is (= 2 (:phase-snapshot-token-query-count report)))
      (is (true? (:phase-queries-use-shared-pinned-token? report)))
      (is (= 4 (count (get-in source [:pre-phase :rows]))))
      (is (= 4 (count (get-in source [:post-phase :rows]))))
      (is (nil? (get-in source [:pre-phase :rows 0 :document])))
      (is (some? (fetch db write-id)))
      (is (nil? (get-in source [:post-phase :rows 3 :document])))
      (is (empty? (:prestate-mismatch-command-indices report)))
      (is (empty? (:poststate-mismatch-command-indices report)))
      (is (empty? (:changed-protected-frame-indices report)))
      (is (= report (recheck! report)))
      (is (= summary (dissoc report :source)))
      (is (not (contains? summary :source)))
      (is (not (re-find #"PRIVATE_|FRAME_SECRET|:doc/value|:doc/secret|:snapshot-token|v1422-"
                        (pr-str summary))))
      (no-authority! report)
      (no-authority! summary))))

(deftest actual-xtdb-command-poststate-overwritten-by-untracked-writer
  (with-open [db (xtn/start-node {})]
    (let [{:keys [write-id] :as sc} (scenario)
          report (acquire! db sc
                       (fn [n ops]
                         (live/execute-tx! n ops)
                         ;; Deliberately bypass the captured adapter.
                         (seed! n write-id {:doc/value "UNTRACKED_PRIVATE_1422"})))]
      (is (= :queried-poststate-diverges-from-prepared-writes (:status report)))
      (is (= [0] (:poststate-mismatch-command-indices report)))
      (is (empty? (:changed-protected-frame-indices report)))
      (is (= :bounded-captured-adapter-operations-agree (:captured-write-status report)))
      (is (= report (recheck! report)))
      (no-authority! report))))

(deftest actual-xtdb-untracked-frame-write-detected
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          id (first frame-ids)
          _ (seed! db id {:doc/lock 7})
          report (acquire! db sc
                       (fn [n ops]
                         (live/execute-tx! n ops)
                         (seed! n id {:doc/lock 9})))]
      (is (= :queried-protected-frame-diverges (:status report)))
      (is (empty? (:poststate-mismatch-command-indices report)))
      (is (= [0] (:changed-protected-frame-indices report)))
      (no-authority! report))))

(deftest actual-xtdb-raw-writer-before-captured-submit-still-unattributed
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          id (first frame-ids)
          report (acquire! db sc
                       (fn [n ops]
                         (seed! n id {:doc/new "OUTSIDE_CAPTURE_1422"})
                         (live/execute-tx! n ops)))]
      (is (= :queried-protected-frame-diverges (:status report)))
      (is (= [0] (:changed-protected-frame-indices report)))
      (is (= :bounded-captured-adapter-operations-agree (:captured-write-status report)))
      (no-authority! report))))

(deftest actual-xtdb-command-prestate-diverges-from-admitted-create
  (with-open [db (xtn/start-node {})]
    (let [{:keys [write-id] :as sc} (scenario)
          _ (seed! db write-id {:doc/value "PREEXISTING_PRIVATE_1422"})
          report (ordinary! db sc)]
      (is (= :submission-outcome-unknown (:status report)))
      (is (= [0] (:prestate-mismatch-command-indices report)))
      (is (true? (:submission-threw? report)))
      (no-authority! report))))

(deftest actual-xtdb-response-lost-after-commit-preserves-uncertainty
  (with-open [db (xtn/start-node {})]
    (let [{:keys [write-id] :as sc} (scenario)
          submissions (atom 0)
          report (acquire! db sc
                       (fn [n ops]
                         (swap! submissions inc)
                         (live/execute-tx! n ops)
                         (throw (ex-info "network response lost"
                                         {:private "PRIVATE_VALUE_1422"}))))]
      (is (= 1 @submissions))
      (is (= :submission-outcome-unknown (:status report)))
      (is (true? (:submission-threw? report)))
      (is (some? (fetch db write-id)))
      (is (empty? (:poststate-mismatch-command-indices report)))
      (is (= report (recheck! report)))
      (is (not (re-find #"PRIVATE_VALUE_1422|network response lost"
                        (pr-str (subject/redacted-summary report)))))
      (no-authority! report))))

(deftest actual-xtdb-submit-only-has-no-causal-proof
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          attempts (atom 0)
          report (acquire! db sc (fn [n ops]
                               (swap! attempts inc)
                               (live/submit-tx! n ops)))]
      (is (= 1 @attempts))
      (is (= :bounded-captured-adapter-operations-agree (:captured-write-status report)))
      (is (= 1 (:write-target-count report)))
      (is (= report (recheck! report)))
      (no-authority! report))))

(deftest no-adapter-call-not-equivalent-to-successful-capture
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          attempts (atom 0)]
      (is (some? (rejection #(acquire! db sc
                                  (fn [n _]
                                    (swap! attempts inc)
                                    (xt/execute-tx n [[:put-docs :document
                                                       {:xt/id "RAW_ONLY_1422"}]]))))))
      (is (= 1 @attempts)))))

(deftest multiple-adapter-calls-not-equivalent-to-one-command
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario) attempts (atom 0)]
      (is (some? (rejection #(acquire! db sc (fn [n ops]
                                            (swap! attempts inc)
                                            (live/execute-tx! n ops)
                                            (live/execute-tx! n ops))))))
      (is (= 1 @attempts)))))

(deftest captured-wrong-operation-must-not-pass-canonical-comparison
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          report (acquire! db sc (fn [n _]
                               (live/execute-tx!
                                n [[:put-docs :document
                                    {:xt/id "UNEXPECTED_DOC_1422"
                                     :doc/value "UNEXPECTED_PRIVATE_1422"}]])))]
      (is (not= :bounded-queried-write-and-frame-agreement (:status report)))
      (is (not= :bounded-captured-adapter-operations-agree
                (:captured-write-status report)))
      (no-authority! report))))

(deftest missing-frame-coverage-fails-before-callback
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          attempts (atom 0)
          source (update (:source sc) :protected-documents subvec 0 1)]
      (is (= :incomplete-declared-write-or-frame-domain
             (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                          {:node db :domain-source source
                           :submit! (fn [_] (swap! attempts inc))}))))
      (is (zero? @attempts)))))

(deftest empty-or-invalid-submission-input-rejected-before-use
  (let [sc (scenario)]
    (is (= :missing-node-or-submit-function
           (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                        {:node nil :domain-source (:source sc) :submit! identity}))))
    (is (= :missing-node-or-submit-function
           (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                        {:node :placeholder :domain-source (:source sc)
                         :submit! nil}))))
    (is (= :invalid-map-shape
           (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                        {:node :placeholder :domain-source (:source sc)
                         :submit! identity :additional :no}))))))

(deftest oversized-or-invalid-physical-targets-rejected-before-submission
  (with-open [db (xtn/start-node {})]
    (let [sc (scenario)
          attempts (atom 0)
          input (fn [source] {:node db :domain-source source
                              :submit! (fn [_] (swap! attempts inc))})
          large (scenario 256)
          invalid (update (:source sc) :protected-documents
                          conj {:target [:invalid-table 42] :fields :all})]
      (is (= :invalid-protected-document-vector
             (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                          (input (:source large))))))
      (is (some? (rejection #(subject/acquire-controlled-xtdb-write-accounting!
                             (input invalid)))))
      (is (zero? @attempts)))))

(deftest source-shape-forgery-is-rejected-during-recheck
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          source (:source r)]
      (is (= :invalid-map-shape
             (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                          (assoc source :extra :tamper) r))))
      (is (= :invalid-map-shape
             (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                          (dissoc source :domain-report) r))))
      (no-authority! r))))

(deftest reported-authority-and-status-forgery-detected
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario)) source (:source r)]
      (doseq [[key value] [[:certificate :issued]
                           [:admission :admitted]
                           [:retry-authority :safe]
                           [:settlement-authority :safe]
                           [:transaction-authority :safe]
                           [:authority :all]
                           [:actual-commit-causality :proven]
                           [:status :bounded-queried-write-and-frame-agreement-fabricated]
                           [:read-query-count 0]
                           [:extra-key :unadmitted]]]
        (is (= :stale-or-forged-accounting-report
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            source (assoc r key value))))))
      (no-authority! r))))

(deftest missing-and-reordered-physical-rows-fail-closed
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario)) source (:source r)]
      (doseq [phase [:pre-phase :post-phase]]
        (is (= :incomplete-phase-readback
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            (assoc-in source [phase :rows] []) r))))
        (is (= :missing-reordered-or-unexpected-row
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            (update-in source [phase :rows] (fn [rows] (vec (reverse rows)))) r))))
        (is (= :invalid-map-shape
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            (update-in source [phase :rows 0] assoc :extra :tamper)
                            r))))))))

(deftest invalid-physical-observation-document-rejected
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario)) source (:source r)]
      (doseq [bad-doc [17 [] {:xt/id :wrong-id} {"non-kw" "BAD"}]]
        (is (= :invalid-observed-document
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            (assoc-in source [:post-phase :rows 0 :document] bad-doc)
                            r))))))))

(deftest invalid-and-forged-phase-tokens-are-rejected
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario)) source (:source r)]
      (doseq [phase [:pre-phase :post-phase]
              bad [nil "" 123 "NOT_AN_XTDB_TOKEN"]]
        (is (= :invalid-phase-token
               (rejection #(subject/require-current-controlled-xtdb-write-accounting!
                            (assoc-in source [phase :token] bad) r))))))))

(deftest synthetic-regressed-phase-time-is-not-positive-agreement
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          pre (get-in r [:source :pre-phase :token])
          post (get-in r [:source :post-phase :token])
          t (Instant/now)
          altered (synthetic-from r #(-> %
                                          (assoc-in [:pre-phase :token]
                                                    (token-at pre (.plusSeconds t 30)))
                                          (assoc-in [:post-phase :token]
                                                    (token-at post (.minusSeconds t 30)))))]
      (is (= :pre-after-post (:pre-read-token-time-order altered)))
      (is (= :read-basis-time-regressed (:status altered)))
      (is (= altered (recheck! altered)))
      (no-authority! altered))))

(deftest synthetic-prestate-divergence-is-diagnostic-not-attested
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          altered (synthetic-from r #(assoc-in % [:pre-phase :rows 0 :document]
                                             {:doc/rev 0 :doc/value "INJECTED_PRIVATE"}))]
      (is (= :queried-prestate-diverges-from-admitted-before (:status altered)))
      (is (= [0] (:prestate-mismatch-command-indices altered)))
      (is (= altered (recheck! altered)))
      (no-authority! altered))))

(deftest synthetic-poststate-missing-field-is-not-equality
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          altered (synthetic-from r #(update-in % [:post-phase :rows 0 :document]
                                               dissoc :doc/value))]
      (is (= :queried-poststate-diverges-from-prepared-writes (:status altered)))
      (is (= [0] (:poststate-mismatch-command-indices altered)))
      (no-authority! altered))))

(deftest synthetic-poststate-extra-field-is-not-equality
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          altered (synthetic-from r #(assoc-in % [:post-phase :rows 0 :document :doc/extra]
                                             "EXTRA_PRIVATE_1422"))]
      (is (= :queried-poststate-diverges-from-prepared-writes (:status altered)))
      (is (= [0] (:poststate-mismatch-command-indices altered)))
      (no-authority! altered))))

(deftest synthetic-poststate-nil-versus-absent-not-interchangeable
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          altered (synthetic-from r #(assoc-in % [:post-phase :rows 0 :document :doc/value] nil))]
      (is (= :queried-poststate-diverges-from-prepared-writes (:status altered)))
      (is (= [0] (:poststate-mismatch-command-indices altered)))
      (no-authority! altered))))

(deftest synthetic-frame-change-with-matching-command-is-not-frame-proof
  (with-open [db (xtn/start-node {})]
    (let [r (ordinary! db (scenario))
          altered (synthetic-from r #(assoc-in % [:post-phase :rows 1 :document]
                                             {:doc/lock 987}))]
      (is (= :queried-protected-frame-diverges (:status altered)))
      (is (= [0] (:changed-protected-frame-indices altered)))
      (is (empty? (:poststate-mismatch-command-indices altered)))
      (no-authority! altered))))

(deftest rechecking-a-prior-report-does-not-monitor-subsequent-xtdb-writes
  (with-open [db (xtn/start-node {})]
    (let [{:keys [frame-ids] :as sc} (scenario)
          id (first frame-ids)
          report (ordinary! db sc)
          _ (seed! db id {:doc/value "LATER_PRIVATE_1422"})]
      (is (= :bounded-queried-write-and-frame-agreement (:status report)))
      (is (= report (recheck! report)))
      (is (= "LATER_PRIVATE_1422" (:doc/value (fetch db id))))
      (no-authority! report))))

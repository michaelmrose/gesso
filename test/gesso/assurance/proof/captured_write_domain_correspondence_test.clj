(ns gesso.assurance.proof.captured-write-domain-correspondence-test
  "v1414: Persistent adversarial regression tests for v1413 adapter-call versus
   declared finite write-domain correspondence. The reported capture is
   process-local and unauthenticated; these tests do not establish XTDB
   receipt, committed physical frame, complete writer paths, or certification."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.captured-write-domain-correspondence :as subject]
            [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.live.consistency.xtdb :as live]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]))

(defn- create [id secret]
  (command/create :document {:xt/id id :doc/rev 0 :doc/created :t0
                             :doc/updated :t0 :doc/value secret}
                  {:revision-key :doc/rev :created-at-key :doc/created
                   :updated-at-key :doc/updated}))
(defn- dsource [commands]
  {:plan {:commands commands :emit false}
   :protected-documents [{:target [:document :a] :fields :all}
                         {:target [:document :b] :fields :all}
                         {:target [:document :frame] :fields #{:doc/secret}}]})
(defn- source [] (dsource [(create :a "PRIVATE_1414") (create :b "SECRET_1414")]))
(defn- ops [s] (:tx-ops (tx/prepare nil (:plan s))))
(defn- capture [tx-ops]
  (with-redefs-fn
    {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
     (fn [_ _ _] {:tx-id 41})}
    #(live/capture-tx-api-calls!
      (fn [] (live/submit-tx! :test tx-ops)))))
(defn- inspect [s c]
  (let [d (domain/derive-protected-write-domain! s)]
    (subject/compare-captured-write-domain!
      {:domain-source s :domain-report d :capture-report c})))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- no-authority [r]
  (is (= :not-established (:actual-xtdb-write-footprint-complete? r)))
  (is (= :not-established (:physical-frame-outside-enumeration r)))
  (is (= :not-established (:writer-path-closure r)))
  (is (false? (:actual-xtdb-submission-authenticated? r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :not-issued (:certificate r))))
(deftest exact-captured-vector-only-conditional-agreement
  (let [s (source) r (inspect s (capture (ops s)))]
    (is (= :bounded-captured-adapter-operations-agree (:status r)))
    (is (:one-adapter-call-matches-declared-domain? r))
    (is (= 2 (:observed-single-put-count r)))
    (is (empty? (:extra-observed-write-op-indices r)))
    (is (empty? (:missing-declared-command-indices r)))
    (is (= r (subject/require-current-captured-write-domain! (:source r) r)))
    (no-authority r)))
(deftest extra-target-is-detected
  (let [s (source) r (inspect s (capture (conj (ops s) [:put-docs :document {:xt/id :z :doc/secret "leak"}]))) ]
    (is (= :captured-operations-diverge (:status r)))
    (is (= 1 (count (:extra-observed-write-op-indices r))))
    (is (= 1 (count (:writes-outside-protection-op-indices r))))
    (is (false? (:one-adapter-call-matches-declared-domain? r)))))
(deftest frame-only-target-write-is-detected
  (let [s (source) r (inspect s (capture (conj (ops s) [:put-docs :document {:xt/id :frame :doc/secret 5}]))) ]
    (is (= 1 (count (:frame-only-target-write-op-indices r))))
    (is (empty? (:writes-outside-protection-op-indices r)))
    (is (false? (:one-adapter-call-matches-declared-domain? r)))))
(deftest missing-and-duplicate-writes
  (let [s (source) base (ops s)
        without (vec (remove #(and (= :put-docs (first %)) (= :b (get-in % [2 :xt/id]))) base))
        r1 (inspect s (capture without))
        r2 (inspect s (capture (conj base (last base))))]
    (is (= [1] (:missing-declared-command-indices r1)))
    (is (= 2 (count (:duplicate-observed-write-op-indices r2))))
    (is (not (:one-adapter-call-matches-declared-domain? r1)))
    (is (not (:one-adapter-call-matches-declared-domain? r2)))))
(deftest payload-change-and-unexpected-assertion
  (let [s (source) base (ops s)
        widx (.indexOf (mapv first base) :put-docs)
        r1 (inspect s (capture (assoc-in base [widx 2 :doc/value] "BAD")))
        r2 (inspect s (capture (into [["ASSERT FALSE"]] base)))]
    (is (= [widx] (:payload-divergence-op-indices r1)))
    (is (= :captured-operations-diverge (:status r2)))
    (is (empty? (:unsupported-observed-op-indices r2)))
    (is (not (:one-adapter-call-matches-declared-domain? r2)))))
(deftest unsupported-forms-fail-correspondence
  (let [s (source) base (ops s)]
    (doseq [op [[:delete-docs :document :a]
                [:patch-docs :document {:xt/id :a :secret 5}]
                [:put-docs :document {:xt/id :a} {:xt/id :b}]
                ["UPDATE document SET x = 3"]
                [:erase-docs :document :a]]]
      (let [r (inspect s (capture (conj base op)))]
        (is (= :captured-operations-diverge (:status r)))
        (is (= 1 (count (:unsupported-observed-op-indices r))))
        (is (false? (:one-adapter-call-matches-declared-domain? r)))))))
(deftest multiple-and-zero-calls-never-select-favorable-subset
  (let [s (source)
        empty-c (live/capture-tx-api-calls! (fn [] :no-write))
        multi (with-redefs-fn
                {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
                 (fn [_ _ _] {:tx-id 7})}
                #(live/capture-tx-api-calls!
                  (fn [] (live/submit-tx! :fake (ops s))
                    (live/submit-tx! :fake (ops s)))))
        r0 (inspect s empty-c) r2 (inspect s multi)]
    (is (= :no-adapter-call-observed (:status r0)))
    (is (= :multiple-adapter-calls-ambiguous (:status r2)))
    (is (not (:one-adapter-call-matches-declared-domain? r0)))
    (is (not (:one-adapter-call-matches-declared-domain? r2)))))
(deftest partial-protection-blocks-positive-match
  (let [s (assoc-in (source) [:protected-documents 0 :fields] #{:xt/id :doc/rev})
        r (inspect s (capture (ops s)))]
    (is (:captured-transaction-vector-exact? r))
    (is (= :declared-domain-not-fully-protected (:status r)))
    (is (false? (:one-adapter-call-matches-declared-domain? r)))))
(deftest tampering-and-redaction
  (let [s (source) r (inspect s (capture (ops s)))
        summary (subject/redacted-summary r)]
    (is (not (re-find #"PRIVATE_1414|SECRET_1414|:doc/value|:document"
                      (pr-str summary))))
    (is (not (contains? summary :source)))
    (no-authority summary)
    (doseq [bad [(assoc r :authority :certified)
                 (assoc r :one-adapter-call-matches-declared-domain? false)
                 (assoc r :status :proof)] ]
      (is (= :stale-or-forged-comparison-report
             (reason #(subject/require-current-captured-write-domain! (:source r) bad)))))))
(deftest malformed-capture-rejected
  (let [s (source) c (capture (ops s))]
    (doseq [[bad expected] [[(assoc c :authority :certified) :invalid-adapter-capture-envelope]
                            [(assoc c :events (subvec (:events c) 0 1)) :incomplete-adapter-call-pair]
                            [(assoc-in c [:events 0 :api] :fake) :invalid-adapter-call-pair]
                            [(assoc-in c [:events 0 :tx-ops] :fake) :invalid-adapter-call-pair]]]
      (is (= expected (reason #(inspect s bad)))))))
(deftest thrown-api-must-not-be-interpreted-as-a-rejection
  (let [s (source)
        c (with-redefs-fn {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
                           (fn [& _] (throw (ex-info "lost response" {})))}
            #(live/capture-tx-api-calls!
              (fn [] (live/submit-tx! :fake (ops s)))))
        r (inspect s c)]
    (is (= :adapter-api-threw-outcome-unknown (:status r)))
    (is (= :threw (:captured-call-phase r)))
    (is (= :not-established (:commit-or-reject-outcome r)))
    (no-authority r)))

(deftest sql-guard-and-option-mutation-do-not-pass
  (let [s (source) base (ops s)
        assertion-index (first (keep-indexed (fn [i op]
                                               (when (string? (first op)) i)) base))
        altered (assoc-in base [assertion-index 0] "ASSERT TRUE")
        r (inspect s (capture altered))
        options (-> (capture base) (assoc-in [:events 0 :tx-options] {:extra :unsafe}))
        r2 (inspect s options)]
    (is (some? assertion-index))
    (is (= :captured-operations-diverge (:status r)))
    (is (false? (:captured-transaction-vector-exact? r)))
    (is (= :captured-operations-diverge (:status r2)))
    (is (false? (:captured-transaction-vector-exact? r2)))))

;; Regression matrix: captures below are deliberately simulated adapter
;; observations, not authenticated records of executed XTDB transactions.
(defn- rejected-reason [s c]
  (reason #(inspect s c)))

(defn- divergent! [report]
  (is (false? (:one-adapter-call-matches-declared-domain? report)))
  (is (not= :bounded-captured-adapter-operations-agree (:status report)))
  (no-authority report))

(deftest op-order-is-part-of-the-transaction-contract
  (let [s (source)
        expected (ops s)
        ;; Swapping SQL assertions, or a canonical put and its assertion,
        ;; cannot be treated as equivalent just because write targets match.
        swapped (assoc expected 0 (nth expected 1) 1 (nth expected 0))
        report (inspect s (capture swapped))]
    (is (not= expected swapped))
    (is (= 2 (:observed-single-put-count report)))
    (is (empty? (:missing-declared-command-indices report)))
    (is (empty? (:extra-observed-write-op-indices report)))
    (is (false? (:captured-transaction-vector-exact? report)))
    (divergent! report)))

(deftest omitting-an-assertion-is-not-a-valid-write-domain-match
  (let [s (source)
        base (ops s)
        assertions (keep-indexed (fn [i op]
                                   (when (string? (first op)) i)) base)
        without (vec (remove #(string? (first %)) base))
        report (inspect s (capture without))]
    (is (seq assertions))
    (is (= 2 (:observed-single-put-count report)))
    (is (empty? (:missing-declared-command-indices report)))
    (is (false? (:captured-transaction-vector-exact? report)))
    (divergent! report)))

(deftest duplicate-assertion-does-not-preserve-exact-vector
  (let [s (source)
        base (ops s)
        assertion (first (filter #(string? (first %)) base))
        report (inspect s (capture (conj base assertion)))]
    (is assertion)
    (is (= (inc (count base)) (:observed-transaction-op-count report)))
    (is (empty? (:extra-observed-write-op-indices report)))
    (is (false? (:captured-transaction-vector-exact? report)))
    (divergent! report)))

(deftest target-identity-and-nested-document-payloads-matter
  (let [s (source)
        base (ops s)
        idx (first (keep-indexed (fn [i op]
                                   (when (= :put-docs (first op)) i)) base))
        cases [(assoc-in base [idx 2 :xt/id] :other)
               (assoc-in base [idx 1] :alternate-document)
               (assoc-in base [idx 2 :doc/nested] {:secret "SENSITIVE_1414"})]]
    (is (some? idx))
    (doseq [changed cases]
      (let [report (inspect s (capture changed))]
        (is (false? (:captured-transaction-vector-exact? report)))
        (divergent! report)))))

(deftest extra-and-unprotected-write-indices-are-position-stable
  (let [s (source)
        base (ops s)
        extra1 [:put-docs :document {:xt/id :outside :doc/value "PRIVATE_1414"}]
        extra2 [:put-docs :document {:xt/id :frame :doc/value "PRIVATE_1414"}]
        report (inspect s (capture (into base [extra1 extra2])))
        n (count base)]
    (is (= [n (inc n)] (:extra-observed-write-op-indices report)))
    (is (= [n] (:writes-outside-protection-op-indices report)))
    (is (= [(inc n)] (:frame-only-target-write-op-indices report)))
    (is (empty? (:missing-declared-command-indices report)))
    (divergent! report)))

(deftest missing-put-is-not-excused-by-an-added-assertion
  (let [s (source)
        base (ops s)
        without (filterv #(not= :b (get-in % [2 :xt/id])) base)
        report (inspect s (capture (conj without ["ASSERT TRUE"])))]
    (is (= [1] (:missing-declared-command-indices report)))
    (is (empty? (:extra-observed-write-op-indices report)))
    (is (false? (:captured-transaction-vector-exact? report)))
    (divergent! report)))

(deftest adapter-options-change-is-not-merely-metadata
  (let [s (source)
        base (ops s)
        c (capture base)]
    (doseq [options [{:timeout 5000} {:await-tx-id 42} {:basis "spoof"}]]
      (let [report (inspect s (assoc-in c [:events 0 :tx-options] options))]
        (is (= :captured-operations-diverge (:status report)))
        (is (false? (:captured-transaction-vector-exact? report)))
        (divergent! report)))))

(deftest malformed-capture-envelopes-fail-closed
  (let [s (source) c (capture (ops s))
        cases [[(assoc c :type :counterfeit) :invalid-adapter-capture-envelope]
               [(assoc c :version 99) :invalid-adapter-capture-envelope]
               [(assoc c :observation-origin :external) :invalid-adapter-capture-envelope]
               [(assoc c :actual-xtdb-api-authenticated? true) :invalid-adapter-capture-envelope]
               [(assoc c :complete-writer-coverage :proved) :invalid-adapter-capture-envelope]
               [(assoc c :transaction-internal-prestate :proved) :invalid-adapter-capture-envelope]
               [(assoc c :proof-status :certified) :invalid-adapter-capture-envelope]
               [(assoc c :certificate :issued) :invalid-adapter-capture-envelope]
               [(assoc c :outcome :committed) :invalid-adapter-capture-envelope]
               [(assoc c :outcome :threw) :invalid-adapter-capture-envelope]
               [(dissoc c :result) :invalid-adapter-capture-envelope]
               [(assoc c :events (list)) :invalid-adapter-capture-envelope]
               [(assoc c :events (vec (repeat 34 :wrong))) :invalid-adapter-capture-envelope]]]
    (doseq [[bad expected] cases]
      (is (= expected (rejected-reason s bad)) (pr-str (keys bad))))))

(deftest captured-call-pair-integrity-is-required
  (let [s (source) c (capture (ops s))
        cases [[(assoc-in c [:events 0 :call-id] (random-uuid))
                :invalid-adapter-call-pair]
               [(assoc-in c [:events 0 :phase] :returned)
                :invalid-adapter-call-pair]
               [(assoc-in c [:events 1 :phase] :invoked)
                :invalid-adapter-call-pair]
               [(assoc-in c [:events 1 :api] :execute-tx)
                :invalid-adapter-call-pair]
               [(assoc-in c [:events 1 :tx-result] nil)
                nil]
               [(assoc-in c [:events 0 :tx-options] :not-a-map)
                :invalid-adapter-call-pair]
               [(update-in c [:events 0] assoc :additional :untrusted)
                :invalid-adapter-call-pair]
               [(update-in c [:events 1] assoc :additional :untrusted)
                :invalid-adapter-call-pair]]]
    (doseq [[bad expected] cases]
      (if expected
        (is (= expected (rejected-reason s bad)))
        ;; A returned null result is not proof of commit: exact operation
        ;; matching may remain diagnostic, yet all authority remains absent.
        (let [r (inspect s bad)]
          (is (= :bounded-captured-adapter-operations-agree (:status r)))
          (no-authority r))))))

(deftest duplicate-call-identifiers-and-multiple-observations
  (let [s (source) c (capture (ops s))
        duplicated (update c :events into (:events c))
        result (capture (ops s))
        two (update c :events into (:events result))]
    (is (= :duplicate-adapter-call-identity (rejected-reason s duplicated)))
    (is (= :multiple-adapter-calls-ambiguous (:status (inspect s two))))
    (divergent! (inspect s two))))

(deftest outer-wrapper-throws-after-adapter-return
  (let [s (source)
        c (with-redefs-fn
            {(ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
             (fn [_ _ _] {:tx-id 42})}
            #(live/capture-tx-api-calls!
              (fn [] (live/submit-tx! :fake (ops s))
                (throw (ex-info "lost after returning" {})))))
        r (inspect s c)]
    (is (= :threw (:outcome c)))
    (is (= :returned (:captured-call-phase r)))
    (is (= :capture-threw-after-adapter-call (:status r)))
    (is (= :not-established (:commit-or-reject-outcome r)))
    ;; The transmitted vector can match while the enclosing call outcome
    ;; is unknown; correspondence is not commit or settlement authority.
    (is (:one-adapter-call-matches-declared-domain? r))
    (no-authority r)))

(deftest missing-protection-and-forged-source-relative-report
  (let [s (source)
        c (capture (ops s))
        report (domain/derive-protected-write-domain! s)
        modified (update-in s [:protected-documents] subvec 1)
        submitted {:domain-source s :domain-report report :capture-report c}]
    (is (= :declared-domain-not-fully-protected
           (:status (inspect modified (capture (ops modified))))))
    (is (= :stale-or-forged-domain-report
           (reason #(subject/compare-captured-write-domain!
                     (assoc submitted :domain-report (assoc report :authority :certified))))))
    (is (= :stale-or-forged-domain-report
           (reason #(subject/compare-captured-write-domain!
                     (assoc submitted :domain-source modified)))))))

(deftest metadata-and-input-shapes-are-not-valid-evidence
  (let [s (source) c (capture (ops s))
        d (domain/derive-protected-write-domain! s)
        input {:domain-source s :domain-report d :capture-report c}
        report (subject/compare-captured-write-domain! input)]
    (doseq [bad [(assoc input :certificate :issued)
                 (dissoc input :domain-report)
                 (with-meta input {:transient :spoof})]]
      (is (= :invalid-map-shape
             (reason #(subject/compare-captured-write-domain! bad)))))
    (is (= :stale-or-forged-comparison-report
           (reason #(subject/require-current-captured-write-domain!
                     input (with-meta report {:spoof :yes})))))))

(deftest report-rechecks-cannot-promote-uncorroborated-proof
  (let [s (source)
        c (capture (ops s))
        r (inspect s c)
        src (:source r)
        summary (subject/redacted-summary r)]
    (is (= subject/report-type (:type r)))
    (is (= subject/report-version (:version r)))
    (is (= :bounded-adapter-call-diagnostic-only (:proof-status r)))
    (is (= :not-admitted (:admission r)))
    (is (= r (subject/require-current-captured-write-domain! src r)))
    (is (= (dissoc r :source) summary))
    (is (not (re-find #"PRIVATE_1414|SECRET_1414|:document|:doc/value|:doc/secret"
                      (pr-str summary))))
    (doseq [bad [(assoc r :certificate :issued)
                 (assoc r :actual-xtdb-submission-authenticated? true)
                 (assoc r :writer-path-closure :established)
                 (assoc r :physical-frame-outside-enumeration :established)
                 (assoc r :status :physical-proof)
                 (dissoc r :source)]]
      (is (= :stale-or-forged-comparison-report
             (reason #(subject/require-current-captured-write-domain! src bad)))))))

(deftest swapped-upstream-plan-builder-rejected
  (let [s (source) c (capture (ops s))]
    (is (= :substituted-upstream-checker
           (with-redefs-fn {#'tx/prepare (fn [& _] {:tx-ops []})}
             #(reason (fn [] (inspect s c))))))
    (is (= :substituted-upstream-checker
           (with-redefs-fn
             {#'domain/require-current-protected-write-domain!
              (fn [source report] report)}
             #(reason (fn [] (inspect s c))))))))

(deftest redacted-errors-must-not-disclose-raw-submission
  (let [s (source)
        input {:domain-source s
               :domain-report (domain/derive-protected-write-domain! s)
               :capture-report (assoc (capture (ops s)) :authority :fabricated)}]
    (try
      (subject/compare-captured-write-domain! input)
      (is false "Malformed capture must be rejected")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :invalid-adapter-capture-envelope (:reason (ex-data ex))))
        (is (not (re-find #"PRIVATE_1414|SECRET_1414|:doc/value|:document"
                          (str (.getMessage ex) (pr-str (ex-data ex))))))))))

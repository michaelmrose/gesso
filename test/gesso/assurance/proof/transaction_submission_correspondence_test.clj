(ns gesso.assurance.proof.transaction-submission-correspondence-test
  "v1376: Permanent adversarial coverage for the v1375 diagnostic comparison.
   Synthetic submission vectors are never authenticated execution evidence.
   The one embedded XTDB test instruments a controlled actual execution but
   does not claim universal refinement, deployed provenance, or proof authority."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.transaction-submission-correspondence :as submission]
            [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acquisition]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as prior]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]
            [gesso.assurance.certified-claim-fixture :as claim]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(defn- request []
  {:writer-source (:writer-source prior/*source*)
   :operation-id :example/claim
   :binding (get-in prior/*source*
                    [:observed-source :transition-source :observation :command-binding])})
(defn- event []
  (let [o (get-in prior/*source* [:observed-source :transition-source :observation])]
    {:scope resolved/execution-observation-scope
     :phase :prepared :attempt-id (random-uuid)
     :operation :example/claim :binding (:command-binding o)
     :record (:persisted-command-record o)
     :plan (:declared-plan o)}))
(defn- prepared-synthetic
  ([] (prepared-synthetic :committed))
  ([status]
   (let [source (request)
         s (acquisition/start-acquisition! source)
         p (event)
         outcome (assoc p :phase :physical-outcome
                        :physical/status status
                        :physical/source (if (= status :committed)
                                           :gesso-model-transaction-ack
                                           :xtdb-explicit-assertion-failure))]
     ((:observe! s) p)
     ((:observe! s) outcome)
     (let [acquired ((:complete! s))]
       {:acquisition-source source
        :acquisition-report acquired
        :recorded-tx-ops (:tx-ops (tx/prepare {} (:plan p)))}))))
(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))

(deftest canonical-ops-agree-but-are-not-attested
  (let [input (prepared-synthetic)
        r (submission/audit-recorded-submission! input)]
    (is (= :recorded-operations-agree (:status r)))
    (is (:recorded-operations-match? r))
    (is (pos? (:canonical-op-count r)))
    (is (= [] (:mismatched-op-indices r)))
    (is (= :caller-supplied-unattested (:recorded-operation-origin r)))
    (is (false? (:actual-low-level-submission-authenticated? r)))
    (is (= :not-established (:actual-submitted-payload-provenance r)))
    (is (= :not-established (:universal-execution-refinement r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))))

(deftest changed-low-level-operation-is-diagnostic-divergence
  (let [input (prepared-synthetic)
        altered (update-in input [:recorded-tx-ops 0] (fn [_] [:put-docs :forged {:xt/id :fake}]))
        r (submission/audit-recorded-submission! altered)]
    (is (= :recorded-operations-diverge (:status r)))
    (is (= [0] (:mismatched-op-indices r)))
    (is (false? (:recorded-operations-match? r)))
    (is (= :none (:authority r)))))

(deftest omitted-operation-reports-count-and-index
  (let [input (prepared-synthetic)
        ops (:recorded-tx-ops input)
        altered (assoc input :recorded-tx-ops (pop ops))
        r (submission/audit-recorded-submission! altered)]
    (is (= :recorded-operations-diverge (:status r)))
    (is (= [(dec (count ops))] (:mismatched-op-indices r)))
    (is (= (dec (count ops)) (:recorded-op-count r)))))

(deftest added-operation-reports-count-and-index
  (let [input (prepared-synthetic)
        count' (count (:recorded-tx-ops input))
        r (submission/audit-recorded-submission!
           (update input :recorded-tx-ops conj [:put-docs :forged {:xt/id :f}]))]
    (is (= :recorded-operations-diverge (:status r)))
    (is (= [count'] (:mismatched-op-indices r)))))

(deftest altered-acquisition-not-accepted-as-current
  (let [input (prepared-synthetic)]
    (is (= :invalid-acquisition-report
           (reason #(submission/audit-recorded-submission!
                     (assoc-in input [:acquisition-report :authority] :certified)))))
    (is (= :stale-or-altered-acquisition-report
           (reason #(submission/audit-recorded-submission!
                     (assoc-in input [:acquisition-report :issued-application-fingerprint]
                               :fake)))))))

(deftest changed-source-binding-invalidates-acquisition
  (let [input (prepared-synthetic)]
    (is (some? (reason #(submission/audit-recorded-submission!
                        (assoc-in input [:acquisition-source :operation-id] :example/not-claim)))))))

(deftest rejected-attempt-can-still-have-a-matching-record
  (let [r (submission/audit-recorded-submission! (prepared-synthetic :rejected))]
    (is (= :rejected (:captured-physical-status r)))
    (is (:recorded-operations-match? r))
    (is (= :not-established (:actual-submitted-payload-provenance r)))))

(deftest fake-comparison-report-rejected
  (let [input (prepared-synthetic)
        r (submission/audit-recorded-submission! input)]
    (is (= r (submission/require-current-recorded-submission! input r)))
    (is (= :stale-or-altered-comparison-report
           (reason #(submission/require-current-recorded-submission!
                     input (assoc r :authority :proof)))))))

(deftest bad-recorded-shapes-fail-closed
  (let [input (prepared-synthetic)]
    (doseq [bad [nil [] {} (list [:put-docs :foo {}])
                 (vec (repeat 129 [:put-docs :foo {}]))]]
      (is (= :invalid-recorded-operations
             (reason #(submission/audit-recorded-submission!
                       (assoc input :recorded-tx-ops bad))))))))

(deftest invalid-source-shape-fails-closed
  (let [input (prepared-synthetic)]
    (doseq [bad [nil {} [] (dissoc input :recorded-tx-ops)
                 (assoc input :certificate :valid)
                 (with-meta input {:trusted true})]]
      (is (= :invalid-source-shape
             (reason #(submission/audit-recorded-submission! bad)))))))


;; Every audit below replays the complete original v1373 session against the
;; current issued-application fixture. We deliberately do not mock the source
;; checker or make the supplied operation vector into an authenticated receipt.
(defn- compare-input [input]
  (submission/audit-recorded-submission! input))

(defn- require-reason [input reported]
  (reason #(submission/require-current-recorded-submission! input reported)))

(defn- no-physical-authority! [report]
  (doseq [[k expected]
          [[:recorded-operation-origin :caller-supplied-unattested]
           [:comparison-scope :canonical-model-tx-prepare-vs-recorded-xtdb-ops]
           [:actual-low-level-submission-authenticated? false]
           [:actual-submitted-payload-provenance :not-established]
           [:transaction-id-authenticated? false]
           [:transaction-internal-prestate-provenance :not-established]
           [:complete-physical-write-footprint :not-established]
           [:issued-abstract-transition-equivalence :not-established]
           [:universal-execution-refinement :not-established]
           [:original-executable-writer-obligations :still-open]
           [:proof-status :recorded-submission-diagnostic-only]
           [:admission :not-admitted]
           [:certificate :not-issued]
           [:transaction-authority :none]
           [:authority :none]]]
    (is (= expected (get report k)) (str "Unproved claim: " k))))

(deftest comparison-retains-original-identities-without-proving-equivalence
  (let [input (prepared-synthetic)
        report (compare-input input)
        acquired (:acquisition-report input)]
    (is (= submission/comparison-type (:type report)))
    (is (= submission/comparison-version (:version report)))
    (is (= :fresh-acquisition-replay-and-production-plan-lowering
           (:checking report)))
    (doseq [key [:operation-id :attempt-id
                 :issued-application-fingerprint
                 :issued-plan-transition-fingerprint
                 :independent-transition-fingerprint
                 :captured-physical-status]]
      (is (= (get acquired key) (get report key)) (str key)))
    (no-physical-authority! report)))

(deftest bounded-comparison-report-never-contains-submitted-objects
  (let [report (compare-input (prepared-synthetic))]
    (is (= #{:type :version :status :checking
             :operation-id :attempt-id :issued-application-fingerprint
             :issued-plan-transition-fingerprint :independent-transition-fingerprint
             :captured-physical-status :canonical-op-count :recorded-op-count
             :mismatched-op-indices :recorded-operations-match?
             :recorded-operation-origin :comparison-scope
             :actual-low-level-submission-authenticated?
             :actual-submitted-payload-provenance :transaction-id-authenticated?
             :transaction-internal-prestate-provenance :complete-physical-write-footprint
             :issued-abstract-transition-equivalence :universal-execution-refinement
             :original-executable-writer-obligations :proof-status :admission
             :certificate :transaction-authority :authority}
           (set (keys report))))
    (is (vector? (:mismatched-op-indices report)))
    (is (not (contains? report :recorded-tx-ops)))
    (is (not (contains? report :captured-prepared-plan)))
    (is (not (contains? report :captured-command-record)))))

(deftest repeated-audit-of-the-same-source-is-deterministic
  (let [input (prepared-synthetic)
        a (compare-input input)
        b (compare-input input)]
    (is (= a b))
    (is (= a (submission/require-current-recorded-submission! input a)))))

(deftest edn-round-trip-does-not-make-evidence-authoritative
  (let [input (prepared-synthetic)
        report (compare-input input)
        copy (edn/read-string (pr-str report))]
    (is (= report copy))
    (is (= report (submission/require-current-recorded-submission! input copy)))
    (no-physical-authority! copy)))

(deftest mismatch-can-not-be-turned-into-matching-by-tampering-with-report
  (let [input (update-in (prepared-synthetic) [:recorded-tx-ops 0]
                         (constantly [:put-docs :fake {:xt/id :wrong}]))
        report (compare-input input)]
    (is (= :recorded-operations-diverge (:status report)))
    (is (= [0] (:mismatched-op-indices report)))
    (is (= :stale-or-altered-comparison-report
           (require-reason input (assoc report :recorded-operations-match? true))))
    (is (= :stale-or-altered-comparison-report
           (require-reason input (assoc report :status :recorded-operations-agree))))))

(deftest changed-report-count-and-mismatch-indices-are-rejected
  (let [input (prepared-synthetic)
        report (compare-input input)]
    (is (= :stale-or-altered-comparison-report
           (require-reason input (update report :canonical-op-count inc))))
    (is (= :stale-or-altered-comparison-report
           (require-reason input (assoc report :mismatched-op-indices [0]))))))

(deftest report-provenance-and-authority-forgeries-rejected
  (let [input (prepared-synthetic)
        report (compare-input input)]
    (doseq [[k forged]
            [[:recorded-operation-origin :authenticated]
             [:actual-low-level-submission-authenticated? true]
             [:transaction-id-authenticated? true]
             [:universal-execution-refinement :proved]
             [:issued-abstract-transition-equivalence :established]
             [:proof-status :proved]
             [:admission :admitted]
             [:certificate :valid]
             [:transaction-authority :issued]
             [:authority :proof]]]
      (is (= :stale-or-altered-comparison-report
             (require-reason input (assoc report k forged))) (str k)))))

(deftest report-shape-modifications-cannot-be-accepted
  (let [input (prepared-synthetic)
        report (compare-input input)]
    (doseq [forged [(dissoc report :authority)
                    (assoc report :extra :untrusted)
                    nil {} []]]
      (is (= :stale-or-altered-comparison-report
             (require-reason input forged))))))

(deftest comparison-report-metadata-cannot-become-proof-authority
  (let [input (prepared-synthetic)
        copy (with-meta (compare-input input) {:authority :claimed-proof})
        result (submission/require-current-recorded-submission! input copy)]
    ;; Map equality ignores metadata. The checker returns its newly derived
    ;; plain report and never carries untrusted metadata into proof authority.
    (is (nil? (meta result)))
    (is (= :none (:authority result)))
    (is (= :not-issued (:certificate result)))))

(deftest recorded-ops-are-ordered-and-cannot-be-swapped
  (let [input (prepared-synthetic)
        ops (:recorded-tx-ops input)]
    (is (< 1 (count ops)) "Fixture must exercise a multi-op transaction")
    (when (< 1 (count ops))
      (let [swapped (assoc ops 0 (nth ops 1) 1 (nth ops 0))
            report (compare-input (assoc input :recorded-tx-ops swapped))]
        (is (not= (first ops) (second ops)))
        (is (= :recorded-operations-diverge (:status report)))
        (is (= [0 1] (:mismatched-op-indices report)))
        (is (false? (:recorded-operations-match? report)))))))

(deftest copied-operation-in-place-of-another-is-a-divergence
  (let [input (prepared-synthetic)
        ops (:recorded-tx-ops input)]
    (is (< 1 (count ops)))
    (when (< 1 (count ops))
      (let [report (compare-input (assoc input :recorded-tx-ops
                                         (assoc ops 0 (nth ops 1))))]
        (is (= :recorded-operations-diverge (:status report)))
        (is (= [0] (:mismatched-op-indices report)))
        (is (= (count ops) (:recorded-op-count report)))))))

(deftest changed-two-operations-return-two-mismatch-indices
  (let [input (prepared-synthetic)
        ops (:recorded-tx-ops input)]
    (is (< 1 (count ops)))
    (when (< 1 (count ops))
      (let [altered (-> ops
                        (assoc 0 [:put-docs :forged {:xt/id 100}])
                        (assoc 1 [:put-docs :forged {:xt/id 101}]))
            r (compare-input (assoc input :recorded-tx-ops altered))]
        (is (= :recorded-operations-diverge (:status r)))
        (is (= [0 1] (:mismatched-op-indices r)))
        (no-physical-authority! r)))))

(deftest prepended-fake-operation-reports-every-affected-position
  (let [input (prepared-synthetic)
        ops (:recorded-tx-ops input)
        recorded (into [[:put-docs :unknown {:xt/id :extra}]] ops)
        r (compare-input (assoc input :recorded-tx-ops recorded))]
    (is (= :recorded-operations-diverge (:status r)))
    (is (= (inc (count ops)) (:recorded-op-count r)))
    (is (some #{0} (:mismatched-op-indices r)))
    (is (some #{(count ops)} (:mismatched-op-indices r)))))

(deftest no-validation-of-caller-supplied-xtdb-operation-objects-is-implied
  (let [input (prepared-synthetic)
        r (compare-input (assoc input :recorded-tx-ops [nil]))]
    ;; The validator compares supplied op *values*. It does not parse every
    ;; possible XTDB transaction operation or authenticate its provenance.
    (is (= :recorded-operations-diverge (:status r)))
    (is (false? (:recorded-operations-match? r)))
    (no-physical-authority! r)))

(deftest recorded-ops-limit-at-128-is-explicit
  (let [input (prepared-synthetic)
        filler [:put-docs :false {:xt/id :untrusted}]
        accepted (assoc input :recorded-tx-ops (vec (repeat 128 filler)))
        rejected (assoc input :recorded-tx-ops (vec (repeat 129 filler)))
        r (compare-input accepted)]
    (is (= 128 (:recorded-op-count r)))
    (is (= :recorded-operations-diverge (:status r)))
    (is (= :invalid-recorded-operations
           (reason #(compare-input rejected))))))

(deftest metadata-on-recorded-vector-rejected
  (let [input (prepared-synthetic)]
    (is (= :invalid-recorded-operations
           (reason #(compare-input
                     (update input :recorded-tx-ops
                             with-meta {:source :forged})))))))

(deftest non-vector-records-never-enter-comparison
  (let [input (prepared-synthetic)]
    (doseq [recorded ['not-a-vector 23 true "fake" #{1 2}]]
      (is (= :invalid-recorded-operations
             (reason #(compare-input (assoc input :recorded-tx-ops recorded))))))))

(deftest metadata-on-acquisition-report-is-not-trusted
  (let [input (prepared-synthetic)]
    (is (= :invalid-acquisition-report
           (reason #(compare-input
                     (update input :acquisition-report
                             with-meta {:authentication :claimed})))))))

(deftest acquisition-report-rejects-fabricated-source-authentication
  (let [input (prepared-synthetic)]
    (doseq [[k forged]
            [[:hook-origin-authenticated? true]
             [:actual-submitted-payload-provenance :authenticated]
             [:transaction-internal-state-provenance :authenticated]
             [:universal-execution-refinement :established]
             [:submitted-xtdb-ops-equivalence :proved]
             [:captured-plan-to-issued-plan-correspondence :proved]
             [:proof-status :proved]
             [:admission :admitted]
             [:certificate :issued]
             [:authority :proof]]]
      (is (= :invalid-acquisition-report
             (reason #(compare-input
                       (assoc-in input [:acquisition-report k] forged))))
          (str k)))))

(deftest acquisition-report-rejects-unrecognized-physical-status
  (let [input (prepared-synthetic)]
    (doseq [status [:indeterminate :unknown :prepared nil :committed-with-warning]]
      (is (= :invalid-acquisition-report
             (reason #(compare-input
                       (assoc-in input [:acquisition-report :captured-physical-status]
                                 status))))))))

(deftest acquisition-report-rejects-invalid-attempt-id
  (let [input (prepared-synthetic)]
    (doseq [attempt [nil 42 :attempt/forged "not-a-uuid"]]
      (is (= :invalid-acquisition-report
             (reason #(compare-input
                       (assoc-in input [:acquisition-report :attempt-id] attempt))))))))

(deftest forged-application-fingerprint-fails-replay
  (let [input (prepared-synthetic)]
    (is (= :stale-or-altered-acquisition-report
           (reason #(compare-input
                     (assoc-in input [:acquisition-report
                                      :issued-application-fingerprint]
                               :replayed-from-different-deployment)))))))

(deftest forged-abstract-transition-fingerprint-fails-replay
  (let [input (prepared-synthetic)]
    (is (= :stale-or-altered-acquisition-report
           (reason #(compare-input
                     (assoc-in input [:acquisition-report
                                      :independent-transition-fingerprint]
                               :unrelated-abstract-transition)))))))

(deftest forged-attempt-and-command-binding-fail-replay
  (let [input (prepared-synthetic)]
    (doseq [[k forged] [[:operation-id :example/other]
                        [:command-binding {:command-id :faked}]]]
      (is (some? (reason #(compare-input
                          (assoc-in input [:acquisition-report k] forged))))
          (str k)))))

(deftest changed-captured-plan-never-reuses-a-prior-report
  (let [input (prepared-synthetic)
        plan (get-in input [:acquisition-report :captured-prepared-plan])]
    (is (some? (reason #(compare-input
                        (assoc-in input [:acquisition-report :captured-prepared-plan]
                                  (assoc plan :emit (not (:emit plan))))))))))

(deftest changed-supplied-source-binding-refused
  (let [input (prepared-synthetic)]
    ;; Invalid canonical binding is rejected by the upstream model validator,
    ;; whose exception data is not required to carry our :reason field.
    (is (thrown? clojure.lang.ExceptionInfo
                 (compare-input
                  (assoc-in input [:acquisition-source :binding]
                            {:command-id :different}))))))

(deftest metadata-and-extra-input-fields-rejected-before-comparison
  (let [input (prepared-synthetic)]
    (doseq [bad [(with-meta input {:proof :fabricated})
                 (assoc input :certificate :signed)
                 (assoc input :transaction-id 7)
                 (dissoc input :acquisition-report)]]
      (is (= :invalid-source-shape (reason #(compare-input bad)))))))

(deftest altered-recorded-ops-invalidate-current-previous-report
  (let [input (prepared-synthetic)
        r (compare-input input)
        changed (update-in input [:recorded-tx-ops 0]
                           (constantly [:put-docs :fake {:xt/id :changed}]))]
    (is (= :stale-or-altered-comparison-report
           (require-reason changed r)))))

(deftest rejected-attempt-is-not-proof-of-physical-transaction
  (let [r (compare-input (prepared-synthetic :rejected))]
    (is (= :rejected (:captured-physical-status r)))
    (is (= :recorded-operations-agree (:status r)))
    (no-physical-authority! r)))

(deftest fabricated-receipt-identity-is-not-transaction-attestation
  (let [source (request)
        s (acquisition/start-acquisition! source)
        p (event)
        outcome (assoc p :phase :physical-outcome
                       :physical/status :committed
                       :physical/source :gesso-model-transaction-ack
                       :physical/receipt {:tx-id 987654321})
        _ ((:observe! s) p)
        _ ((:observe! s) outcome)
        acquisition-report ((:complete! s))
        input {:acquisition-source source
               :acquisition-report acquisition-report
               :recorded-tx-ops (:tx-ops (tx/prepare {} (:plan p)))}
        r (compare-input input)]
    (is (= :recorded-operations-agree (:status r)))
    (is (= :committed (:captured-physical-status r)))
    (is (false? (:transaction-id-authenticated? r)))
    (no-physical-authority! r)))

(deftest transaction-prepare-substitution-is-refused
  (let [input (prepared-synthetic)]
    (with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
      (is (= :substituted-upstream-checker
             (reason #(compare-input input)))))))

(deftest acquisition-checker-substitution-is-refused
  (let [input (prepared-synthetic)]
    (with-redefs [acquisition/start-acquisition! (fn [& _] {:bogus true})]
      (is (= :substituted-upstream-checker
             (reason #(compare-input input)))))))

(deftest acquired-status-and-comparison-status-are-independent
  (let [committed (compare-input (prepared-synthetic))
        rejected (compare-input (prepared-synthetic :rejected))]
    (is (= :committed (:captured-physical-status committed)))
    (is (= :rejected (:captured-physical-status rejected)))
    (is (= :recorded-operations-agree (:status committed)))
    (is (= :recorded-operations-agree (:status rejected)))
    (is (= :not-established (:actual-submitted-payload-provenance rejected)))))


;; The separately-instrumented XTDB call makes the recorded vector originate
;; from an actual *controlled test invocation* of the low-level execute path.
;; This does not authenticate an uninstrumented deployed application or prove
;; the absence of an intervening writer / missing transaction dependencies.
(deftest embedded-xtdb-actual-low-level-call-ops-agree
  (with-open [database (xtn/start-node {})]
    (let [id (+ 1375000 (rand-int 1000000))
          before {:xt/id id :request/status :open :request/owner nil
                  :request/revision 0 :request/organization :organization/one
                  :request/description "v1375 submission correspondence"}
          read-one (fn [table row-id]
                     (first (xt/q database [(str "SELECT * FROM " (name table)
                                                  " WHERE _id = ?") row-id])))
          acquired (fn []
                     (when-let [raw (read-one :requests id)]
                       (if (contains? raw :request/owner)
                         raw (assoc raw :request/owner nil))))
          source (request)
          binding (:binding source)
          sess (acquisition/start-acquisition! source)
          observed-ops (atom [])
          original-physical xt/execute-tx
          low-level-var (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
          environment (claim/new-claim-environment!
                       {:request-id id :actor :helper/alice
                        :organization :organization/one
                        :read-current! (fn [_ _] (acquired))
                        :authorization! (fn [_ _] {:auth/decision :permit})
                        :read-record! (fn [_ rid] (read-one :gesso_commands rid))
                        :may-disclose? (fn [_ _ _] true)})]
      (xt/execute-tx database [[:put-docs :requests before]])
      (is (some? low-level-var))
      (let [result (with-bindings {low-level-var
                                   (fn [node ops options]
                                     (swap! observed-ops conj ops)
                                     (original-physical node ops options))}
                     ((:executor environment)
                      {:biff.xtdb/node database
                       resolved/observation-context-key (:observe! sess)}
                      {:binding binding :mode :submit}))
            report ((:complete! sess))
            input {:acquisition-source source
                   :acquisition-report report
                   :recorded-tx-ops (first @observed-ops)}
            comparison (submission/audit-recorded-submission! input)]
        (is (= :incorporated (:status result)))
        (is (= 1 (count @observed-ops)))
        (is (= :recorded-operations-agree (:status comparison)))
        (is (= :claimed (:request/status (acquired))))
        (is (= binding (:gesso.command/binding
                        (read-one :gesso_commands (resolution/record-id binding)))))
        (is (false? (:actual-low-level-submission-authenticated? comparison)))
        (is (= :not-established (:universal-execution-refinement comparison)))
        (is (= :none (:authority comparison)))))))

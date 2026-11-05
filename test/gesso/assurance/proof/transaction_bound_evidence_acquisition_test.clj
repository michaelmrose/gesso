(ns gesso.assurance.proof.transaction-bound-evidence-acquisition-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acq]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as prior]
            [gesso.model.resolved-operation :as resolved]
            [gesso.assurance.proof.executable-writer-correspondence-obligations :as writers]
            [gesso.assurance.certified-claim-fixture :as claim]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

;; v1374 permanent tests. The v1372 once-fixture issues a real application
;; executor and captures real handler/source identities. Most cases deliberately
;; INJECT SYNTHETIC events into the v1373 observer and assert they remain
;; diagnostic only. The final integration test separately executes REAL XTDB.
;; Neither category establishes universal refinement or deploy-time provenance.
(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(defn- req []
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
(defn- outcome
  ([p] (outcome p :committed :gesso-model-transaction-ack))
  ([p status source]
   (assoc p :phase :physical-outcome
          :physical/status status :physical/source source)))
(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
(defn- sess [] (acq/start-acquisition! (req)))
(defn- report! [s p q]
  ((:observe! s) p)
  ((:observe! s) q)
  ((:complete! s)))

(deftest committed-pair-diagnostic-only
  (let [s (sess) p (event) r (report! s p (outcome p))]
    (is (= :example/claim (:operation-id r)))
    (is (= acq/acquisition-type (:type r)))
    (is (= acq/acquisition-version (:version r)))
    (is (= :committed (:captured-physical-status r)))
    (is (= (:plan p) (:captured-prepared-plan r)))
    (is (= (:record p) (:captured-command-record r)))
    (is (= (:attempt-id p) (:attempt-id r)))
    (is (= :injected-process-local-hook (:observation-origin r)))
    (is (false? (:hook-origin-authenticated? r)))
    (is (= :not-established (:transaction-internal-state-provenance r)))
    (is (= :not-established (:actual-submitted-payload-provenance r)))
    (is (false? (:transaction-id-authenticated? r)))
    (is (= :not-established (:universal-execution-refinement r)))
    (is (= :not-established (:cross-source-transition-equivalence r)))
    (is (= :not-established (:captured-plan-to-issued-plan-correspondence r)))
    (is (= :not-established (:submitted-xtdb-ops-equivalence r)))
    (is (= :hook-observation-diagnostic-only (:proof-status r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))
    (is (= 9 (count (:operation-obligations r))))
    (is (every? #(= :open (:status %)) (:operation-obligations r)))))

(deftest rejected-physical-attempt-is-not-recast-as-commit
  (let [s (sess) p (event)
        r (report! s p (outcome p :rejected :xtdb-explicit-assertion-failure))]
    (is (= :rejected (:captured-physical-status r)))
    (is (false? (:reported-receipt-present? r)))
    (is (= :none (:transaction-authority r)))))

(deftest classified-post-commit-error-remains-committed
  (let [s (sess) p (event)
        r (report! s p
                   (outcome p :committed :gesso-live-classified-post-commit-failure))]
    (is (= :committed (:captured-physical-status r)))
    (is (= :gesso-live-classified-post-commit-failure
           (:captured-physical-source r)))))

(deftest receipt-presence-does-not-authenticate-transaction-id
  (let [s (sess) p (event)
        r (report! s p (assoc (outcome p) :physical/receipt {:tx-id 999}))]
    (is (true? (:reported-receipt-present? r)))
    (is (= {:tx-id 999} (:captured-physical-receipt r)))
    (is (false? (:transaction-id-authenticated? r)))))

(deftest non-definitive-outcome-never-produces-a-report
  (let [s (sess) p (event)]
    ((:observe! s) p)
    ((:observe! s) (outcome p :indeterminate :unclassified-transaction-exception))
    (is (= :nondefinitive-physical-outcome (reason #((:complete! s)))))
    (is (= :incomplete-physical-attempt (reason #((:complete! s)))))))

(deftest missing-prepared-fails
  (is (= :incomplete-physical-attempt (reason #((:complete! (sess)))))))

(deftest physical-outcome-without-prepared-poisons-session
  (let [s (sess) p (event)]
    (is (= :outcome-without-prepared
           (reason #((:observe! s) (outcome p)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest duplicate-prepared-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :duplicate-prepared-event (reason #((:observe! s) p))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest mismatched-attempt-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :event-pair-mismatch
           (reason #((:observe! s) (assoc (outcome p) :attempt-id (random-uuid))))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest mismatched-plan-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (thrown? clojure.lang.ExceptionInfo
                 ((:observe! s)
                  (assoc (outcome p) :plan {:commands [] :emit false}))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest malformed-events-poison-session
  (doseq [mutate [(fn [e] (assoc e :operation :example/wrong))
                  (fn [e] (assoc e :scope :fake))
                  (fn [e] (assoc e :phase :unrecognized))
                  (fn [e] (assoc e :extra :extra))
                  (fn [e] (dissoc e :attempt-id))]]
    (let [s (sess)]
      (is (some? (reason #((:observe! s) (mutate (event))))))
      (is (= :observer-failure-poisoned-session (reason #((:complete! s))))))))

(deftest rejected-with-fake-receipt-fails
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :rejected-attempt-with-receipt
           (reason #((:observe! s)
                      (assoc (outcome p :rejected :xtdb-explicit-assertion-failure)
                             :physical/receipt {:tx-id 3})))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest unknown-operation-refused
  (is (= :operation-binding-mismatch
         (reason #(acq/start-acquisition! (assoc (req) :operation-id :example/wrong))))))

(deftest exactly-shaped-request-enforced
  (is (= :invalid-source-shape
         (reason #(acq/start-acquisition! (assoc (req) :authority :proof)))))
  (is (= :invalid-source-shape
         (reason #(acq/start-acquisition! (dissoc (req) :writer-source))))))

(deftest substituted-writer-checker-rejected
  (with-redefs [writers/derive-executable-writer-obligations!
                (fn [& _] {:status :refinement-obligations-generated})]
    (is (= :substituted-writer-checker
           (reason #(acq/start-acquisition! (req)))))))

(deftest final-report-rejects-extra-events-and-repeated-completion
  (let [s (sess) p (event)]
    (is (= :committed (:captured-physical-status (report! s p (outcome p)))))
    (is (= :incomplete-physical-attempt (reason #((:complete! s)))))
    (is (= :unexpected-extra-event (reason #((:observe! s) p))))))


;; Source and event inputs are ALWAYS reconstructed from the current fixture.
;; Diagnostic data may be copied, forged, or stale; these tests cannot turn
;; event agreement into independent authentication of the physical database.
(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- no-authority! [r]
  (doseq [[k expected]
          [[:observation-origin :injected-process-local-hook]
           [:hook-origin-authenticated? false]
           [:actual-submitted-payload-provenance :not-established]
           [:transaction-internal-state-provenance :not-established]
           [:transaction-id-authenticated? false]
           [:complete-write-footprint-provenance :not-established]
           [:all-physical-writer-paths-enumerated :not-established]
           [:universal-execution-refinement :not-established]
           [:captured-plan-to-issued-plan-correspondence :not-established]
           [:cross-source-transition-equivalence :not-established]
           [:submitted-xtdb-ops-equivalence :not-established]
           [:host-snapshot-fresh-at-completion :not-established]
           [:proof-status :hook-observation-diagnostic-only]
           [:certificate :not-issued]
           [:admission :not-admitted]
           [:transaction-authority :none]
           [:authority :none]]]
    (is (= expected (get r k)) (str "Unestablished premise: " k)))
  (is (seq (:open-obligations r)))
  (is (= 9 (count (:operation-obligations r))))
  (is (every? #(= :open (:status %)) (:operation-obligations r))))

(deftest synthetic-event-pair-cannot-confer-authority
  (let [s (sess) p (event) r (report! s p (outcome p))]
    (no-authority! r)
    (is (= :committed (:captured-physical-status r)))
    (is (uuid? (:attempt-id r)))))

(deftest captured-source-identities-are-current-but-not-equivalence
  (let [s (sess) p (event) r (report! s p (outcome p))
        ledger (writers/derive-executable-writer-obligations! (:writer-source (req)))
        row (first (filter #(= :example/claim (:operation-id %)) (:operation-rows ledger)))]
    (is (= (:application-catalog-fingerprint ledger) (:issued-application-fingerprint r)))
    (is (= (:issued-plan-transition-fingerprint row)
           (:issued-plan-transition-fingerprint r)))
    (is (= (:abstract-transition-fingerprint row)
           (:independent-transition-fingerprint r)))
    (no-authority! r)))

(deftest captured-plan-is-not-proved-equivalent-to-issued-transition
  (let [s (sess) p (event) r (report! s p (outcome p))]
    (is (= (:plan p) (:captured-prepared-plan r)))
    (is (= :not-established (:captured-plan-to-issued-plan-correspondence r)))
    (is (= :not-established (:submitted-xtdb-ops-equivalence r)))))

(deftest receipt-copies-are-not-physical-attestations
  (doseq [receipt [nil {:tx-id 0} {:tx-id "fabricated"} {:tx-id 123456789}]]
    (let [s (sess) p (event)
          r (report! s p (assoc (outcome p) :physical/receipt receipt))]
      (is (:reported-receipt-present? r))
      (is (= receipt (:captured-physical-receipt r)))
      (is (false? (:transaction-id-authenticated? r)))
      (is (= :none (:authority r))))))

(deftest rejected-physical-pair-leaves-all-obligations-open
  (let [s (sess) p (event)
        r (report! s p (outcome p :rejected :xtdb-explicit-assertion-failure))]
    (is (= :rejected (:captured-physical-status r)))
    (is (false? (:reported-receipt-present? r)))
    (no-authority! r)))

(deftest rejected-pairs-cannot-carry-even-nil-receipts
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :rejected-attempt-with-receipt
           (reason #((:observe! s)
                     (assoc (outcome p :rejected :xtdb-explicit-assertion-failure)
                            :physical/receipt nil)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest invalid-physical-status-source-pair-fails-closed
  (doseq [[status source] [[:committed :xtdb-explicit-assertion-failure]
                           [:rejected :gesso-model-transaction-ack]
                           [:committed :unclassified-transaction-exception]
                           [:failed :gesso-model-transaction-ack]
                           [:indeterminate :gesso-model-transaction-ack]]]
    (let [s (sess) p (event)]
      ((:observe! s) p)
      (is (= :unclassified-physical-outcome
             (reason #((:observe! s) (outcome p status source)))))
      (is (= :observer-failure-poisoned-session
             (reason #((:complete! s))))))))

(deftest unknown-indeterminate-physical-source-fails-closed
  (let [s (sess) p (event)]
    ((:observe! s) p)
    ((:observe! s) (outcome p :indeterminate :unrecognized-transaction-response))
    (is (= :nondefinitive-physical-outcome (reason #((:complete! s)))))
    (is (= :incomplete-physical-attempt (reason #((:complete! s)))))))

(deftest attempted-completion-before-outcome-closes-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :incomplete-physical-attempt (reason #((:complete! s)))))
    (is (= :unexpected-extra-event (reason #((:observe! s) (outcome p)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest prepared-event-receipt-is-not-accepted
  (let [s (sess) p (event)]
    (is (= :invalid-source-shape
           (reason #((:observe! s) (assoc p :physical/receipt {:tx-id 1})))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest event-metadata-is-not-admitted
  (let [s (sess)]
    (is (= :invalid-source-shape
           (reason #((:observe! s) (with-meta (event) {:trusted true})))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest event-must-be-a-plain-map
  (doseq [bad [nil false [] (sorted-map) "event" 7]]
    (let [s (sess)]
      (is (= :invalid-event (reason #((:observe! s) bad))))
      (is (= :observer-failure-poisoned-session (reason #((:complete! s))))))))

(deftest event-requires-real-uuid-attempt-id
  (doseq [bad [nil 1 "not-a-uuid" :uuid]]
    (let [s (sess)]
      (is (= :event-identity-mismatch
             (reason #((:observe! s) (assoc (event) :attempt-id bad)))))
      (is (= :observer-failure-poisoned-session (reason #((:complete! s))))))))

(deftest absent-required-prepared-fields-are-rejected
  (doseq [k [:scope :phase :attempt-id :operation :binding :record :plan]]
    (let [s (sess)]
      (is (some? (error-data #((:observe! s) (dissoc (event) k)))))
      (is (= :observer-failure-poisoned-session (reason #((:complete! s))))))))

(deftest absent-required-outcome-fields-are-rejected
  (doseq [k [:scope :phase :attempt-id :operation :binding :record
             :plan :physical/status :physical/source]]
    (let [s (sess) p (event)]
      ((:observe! s) p)
      (is (some? (error-data #((:observe! s) (dissoc (outcome p) k)))))
      (is (= :observer-failure-poisoned-session (reason #((:complete! s))))))))

(deftest extra-outcome-claims-are-rejected
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :invalid-source-shape
           (reason #((:observe! s) (assoc (outcome p) :certificate :issued)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest wrong-operation-in-physical-outcome-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :event-identity-mismatch
           (reason #((:observe! s) (assoc (outcome p) :operation :example/cancel)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest changed-command-binding-in-outcome-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :event-identity-mismatch
           (reason #((:observe! s)
                     (assoc-in (outcome p) [:binding :arguments] {:forged true})))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest changed-command-record-in-outcome-poisons-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (some? (error-data #((:observe! s)
                              (assoc-in (outcome p) [:record :gesso.command/status]
                                        :fake))))
        "Record substitution must either be rejected by its validator or pair check")
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest changed-transaction-plan-in-outcome-poisons-session
  (let [s (sess) p (event) old (:plan p)]
    ((:observe! s) p)
    (is (some? (error-data #((:observe! s)
                              (assoc (outcome p) :plan (assoc old :emit :async))))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest malformed-event-error-does-not-leak-record-or-plan
  (let [s (sess) p (event)
        data (error-data #((:observe! s) (assoc p :operation :example/forged)))]
    (is (= :event-identity-mismatch (:reason data)))
    (is (= #{:transaction-bound-evidence-acquisition/rejection :reason :at}
           (set (keys data))))
    (is (not-any? #(contains? data %)
                  [:record :plan :binding :command-id :receipt]))))

(deftest malformed-source-rejected-before-event-acquisition
  (doseq [bad [nil [] {} (assoc (req) :certificate :issued)
               (dissoc (req) :binding)
               (with-meta (req) {:trusted true})]]
    (is (some? (error-data #(acq/start-acquisition! bad))))))

(deftest invalid-binding-and-operation-are-not-substituted
  (is (some? (error-data #(acq/start-acquisition!
                          (assoc-in (req) [:binding :operation] :example/cancel)))))
  (is (some? (error-data #(acq/start-acquisition!
                          (assoc (req) :operation-id :example/cancel))))))

(deftest repeat-completion-of-rejected-physical-attempt-is-refused
  (let [s (sess) p (event)]
    (is (= :rejected (:captured-physical-status
                      (report! s p (outcome p :rejected :xtdb-explicit-assertion-failure)))))
    (is (= :incomplete-physical-attempt (reason #((:complete! s)))))))

(deftest a-second-outcome-always-poisons-the-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    ((:observe! s) (outcome p))
    (is (= :unexpected-extra-event (reason #((:observe! s) (outcome p)))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest changed-attempt-identity-does-not-create-a-new-session
  (let [s (sess) p (event)]
    ((:observe! s) p)
    (is (= :event-pair-mismatch
           (reason #((:observe! s) (assoc (outcome p) :attempt-id (random-uuid))))))
    (is (= :observer-failure-poisoned-session (reason #((:complete! s)))))))

(deftest synthetic-events-have-no-physical-writer-effects
  (let [s (sess) p (event)
        r (report! s p (outcome p))]
    (is (= :one-injected-hook-attempt (:scope r)))
    (is (= :not-established (:actual-submitted-payload-provenance r)))
    (is (= :not-established (:transaction-internal-state-provenance r)))))

;; This integration case uses the ACTUAL Gesso resolved-operation transaction
;; hook and an embedded XTDB node. Neither tx/transact! nor xt/execute-tx is
;; mocked. A real committed domain row and durable command row are read back
;; independently. Successful observation STILL cannot authenticate the hook.
;; This one run does not discharge universal refinement, writer enumeration,
;; deployment provenance, or transaction-internal pre-state correspondence.
(deftest embedded-xtdb-hook-observes-real-claim-submission
  (with-open [database (xtn/start-node {})]
    (let [id (+ 1374000 (rand-int 1000000))
          before {:xt/id id :request/status :open :request/owner nil
                  :request/revision 0 :request/organization :organization/one
                  :request/description "v1374 embedded XTDB observation"}
          read-one (fn [table row-id]
                     (first (xt/q database
                                  [(str "SELECT * FROM " (name table)
                                        " WHERE _id = ?") row-id])))
          acquired (fn []
                     (when-let [raw (read-one :requests id)]
                       (if (contains? raw :request/owner)
                         raw (assoc raw :request/owner nil))))
          req' (req)
          binding (:binding req')
          session (acq/start-acquisition! req')
          env (claim/new-claim-environment!
               {:request-id id
                :actor :helper/alice
                :organization :organization/one
                :read-current! (fn [_ _] (acquired))
                :authorization! (fn [_ _] {:auth/decision :permit})
                :read-record! (fn [_ record-id]
                                (read-one :gesso_commands record-id))
                :may-disclose? (fn [_ _ _] true)})]
      (xt/execute-tx database [[:put-docs :requests before]])
      (let [result ((:executor env)
                    {:biff.xtdb/node database
                     resolved/observation-context-key (:observe! session)}
                    {:binding binding :mode :submit})
            report ((:complete! session))
            after (acquired)
            record (read-one :gesso_commands (resolution/record-id binding))]
        (is (= :incorporated (:status result)))
        (is (= :committed (get-in result [:transaction :commit/status])))
        (is (= :committed (:captured-physical-status report)))
        (is (= :gesso-model-transaction-ack (:captured-physical-source report)))
        (is (= :claimed (:request/status after)))
        (is (= :helper/alice (:request/owner after)))
        (is (= 1 (:request/revision after)))
        (is (= (:request/organization before) (:request/organization after)))
        (is (= (:request/description before) (:request/description after)))
        (is (= binding (:gesso.command/binding record)))
        (is (= :incorporated (:gesso.command/status record)))
        (is (= :not-established (:captured-plan-to-issued-plan-correspondence report)))
        (no-authority! report)))))

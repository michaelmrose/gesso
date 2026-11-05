(ns gesso.assurance.proof.transaction-outcome-state-provenance-test
  "v1379: Permanent adversarial coverage for the bounded v1378 diagnostic.

   A caller-provided matching readback is not a transaction receipt; a
   rejected outcome cannot be promoted, an indeterminate attempt cannot be
   completed, and the comparison must reject source/report mismatches.
   All these assertions are about ONE observed attempt, not universal XTDB
   refinement, authenticated writer coverage, or deployment certification.
   The controlled XTDB case is real execution with an explicitly caller-
   supplied node; it is not evidence of general transaction causality."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.transaction-outcome-state-provenance :as provenance]
            [gesso.assurance.proof.transaction-bound-evidence-acquisition :as acquisition]
            [gesso.assurance.proof.transaction-submission-correspondence :as submission]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as fixture]
            [gesso.assurance.certified-claim-fixture :as claim]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]
            [gesso.model.tx-model :as tx-model]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant ZoneId ZonedDateTime]))

;; The real v1368 writer/source fixture is required; synthetic events alone
;; must not bypass its current-source rechecks.
(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(defn- request []
  {:writer-source (:writer-source fixture/*source*)
   :operation-id :example/claim
   :binding (get-in fixture/*source*
                    [:observed-source :transition-source :observation
                     :command-binding])})

(defn- prepared-event []
  (let [o (get-in fixture/*source*
                  [:observed-source :transition-source :observation])]
    {:scope resolved/execution-observation-scope
     :phase :prepared
     :attempt-id (random-uuid)
     :operation :example/claim
     :binding (:command-binding o)
     :record (:persisted-command-record o)
     :plan (:declared-plan o)}))

(defn- outcome-event [prepared status]
  (assoc prepared :phase :physical-outcome
         :physical/status status
         :physical/source (case status
                            :committed :gesso-model-transaction-ack
                            :rejected :xtdb-explicit-assertion-failure
                            :indeterminate :unclassified-transaction-exception)))

(defn- synthetic-source!
  ([] (synthetic-source! :committed))
  ([status]
   (let [source (request)
         prepared (prepared-event)
         session (acquisition/start-acquisition! source)]
     ((:observe! session) prepared)
     ((:observe! session) (outcome-event prepared status))
     (let [acquired ((:complete! session))
           submission-source
           {:acquisition-source source
            :acquisition-report acquired
            :recorded-tx-ops (:tx-ops (tx/prepare {} (:plan prepared)))}]
       [submission-source
        (submission/audit-recorded-submission! submission-source)]))))

(defn- post-observation [source report]
  (let [acquired (:acquisition-report source)
        plan (tx-model/admitted-plan (:captured-prepared-plan acquired))]
    {:attempt-id (:attempt-id report)
     :operation-id (:operation-id report)
     :command-binding (:command-binding acquired)
     :command-record-id (resolution/record-id (:command-binding acquired))
     :captured-physical-status (:captured-physical-status report)
     :reported-receipt-present? (:reported-receipt-present? acquired)
     :reported-receipt (:captured-physical-receipt acquired)
     :observation-origin :caller-supplied-unattested
     :read-phase :after-attempt
     :reported-read-basis nil
     :post-rows (mapv (fn [c] {:target (command/target c)
                               :document (command/after c)})
                      (:commands plan))}))

(defn- input!
  ([] (input! :committed))
  ([status]
   (let [[source report] (synthetic-source! status)]
     {:submission-source source
      :submission-report report
      :state-observation (post-observation source report)})))

(defn- failure [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reason [f] (:reason (failure f)))

(defn- no-authority! [r]
  (doseq [[k v] {:post-read-origin-authenticated? false
                 :post-read-basis-authenticated? false
                 :reported-outcome-authenticated? false
                 :transaction-id-authenticated? false
                 :actual-submission-authenticated? false
                 :post-state-caused-by-this-attempt? :not-established
                 :transaction-internal-prestate-provenance :not-established
                 :complete-physical-write-footprint :not-established
                 :guard-authorization :not-established
                 :serial-concurrency-validation :not-established
                 :committed-outcome-incorporation :not-established
                 :writer-provenance-and-deployment :not-established
                 :universal-execution-refinement :not-established
                 :commit-unknown :not-reclassifiable-as-committed
                 :proof-status :outcome-and-state-diagnostics-only
                 :admission :not-admitted
                 :certificate :not-issued
                 :transaction-authority :none
                 :authority :none}]
    (is (= v (get r k)) (str "Unauthorized strengthening of " k)))
  (is (= 9 (count (:physical-refinement-obligations r))))
  (is (every? #(= :open (:status %)) (:physical-refinement-obligations r))))

(deftest matching-poststate-is-not-physical-certification
  (let [input (input!)
        r (provenance/audit-outcome-state! input)]
    (is (= provenance/provenance-type (:type r)))
    (is (= provenance/provenance-version (:version r)))
    (is (= :observed-committed-projection-agrees (:status r)))
    (is (= :committed (:captured-physical-status r)))
    (is (= :recorded-operations-agree (:comparison-status r)))
    (is (= 2 (:post-read-target-count r)))
    (is (= 2 (:post-read-matching-target-count r)))
    (is (= [] (:post-read-mismatched-target-indices r)))
    (is (true? (:command-record-read-present? r)))
    (is (true? (:command-record-expected-fields-match? r)))
    (is (true? (:bounded-post-state-agreement? r)))
    (no-authority! r)
    (is (= r (provenance/require-current-outcome-state! input r)))))

(deftest rejected-attempt-stays-rejected-even-with-matching-poststate
  (let [r (provenance/audit-outcome-state! (input! :rejected))]
    (is (= :rejected (:captured-physical-status r)))
    (is (= :rejected-outcome-no-causal-attribution (:status r)))
    (is (true? (:bounded-post-state-agreement? r)))
    (no-authority! r)))

(deftest rejected-attempt-with-divergent-poststate-is-not-reclassified
  (let [input (-> (input! :rejected)
                  (assoc-in [:state-observation :post-rows 0 :document] nil))
        r (provenance/audit-outcome-state! input)]
    (is (= :rejected-outcome-no-causal-attribution (:status r)))
    (is (false? (:bounded-post-state-agreement? r)))
    (is (= [0] (:post-read-absent-target-indices r)))
    (is (= [0] (:post-read-mismatched-target-indices r)))
    (no-authority! r)))

(deftest unknown-or-incomplete-physical-attempt-cannot-be-completed
  (let [session (acquisition/start-acquisition! (request))
        prepared (prepared-event)]
    ((:observe! session) prepared)
    ((:observe! session) (outcome-event prepared :indeterminate))
    (is (= :nondefinitive-physical-outcome
           (reason #((:complete! session))))))
  (let [session (acquisition/start-acquisition! (request))]
    (is (= :incomplete-physical-attempt
           (reason #((:complete! session))))))
  (let [session (acquisition/start-acquisition! (request))
        prepared (prepared-event)]
    ((:observe! session) prepared)
    (is (= :unclassified-physical-outcome
           (reason #((:observe! session)
                     (assoc (outcome-event prepared :committed)
                            :physical/status :commit-unknown)))))
    (is (= :observer-failure-poisoned-session
           (reason #((:complete! session)))))))

(deftest observation-identity-and-receipt-substitution-refused
  (let [input (input!)]
    (doseq [[k replacement]
            {:attempt-id (random-uuid)
             :operation-id :example/other
             :command-binding {:forged true}
             :command-record-id (random-uuid)
             :captured-physical-status :rejected
             :reported-receipt-present? true
             :reported-receipt {:forged :receipt}}]
      (is (= :attempt-outcome-receipt-or-binding-mismatch
             (reason #(provenance/audit-outcome-state!
                       (assoc-in input [:state-observation k] replacement))))
          (str "Field must be bound: " k)))))

(deftest no-replay-across-distinct-attempts
  (let [first-attempt (input!)
        second-attempt (input!)]
    (is (not= (get-in first-attempt [:state-observation :attempt-id])
              (get-in second-attempt [:state-observation :attempt-id])))
    (is (some? (reason #(provenance/audit-outcome-state!
                        (assoc second-attempt :submission-report
                               (:submission-report first-attempt))))))
    (is (= :attempt-outcome-receipt-or-binding-mismatch
           (reason #(provenance/audit-outcome-state!
                     (assoc second-attempt :state-observation
                            (:state-observation first-attempt))))))))

(deftest malformed-inputs-and-extra-trust-claims-refused
  (let [input (input!)]
    (doseq [value [nil {} [] (assoc input :trusted? true)
                   (dissoc input :state-observation)
                   (with-meta input {:trusted? true})]]
      (is (= :invalid-source-shape
             (reason #(provenance/audit-outcome-state! value)))))
    (doseq [value [(assoc (:state-observation input) :authenticated? true)
                   (dissoc (:state-observation input) :post-rows)
                   (with-meta (:state-observation input) {:attested? true})]]
      (is (= :invalid-source-shape
             (reason #(provenance/audit-outcome-state!
                       (assoc input :state-observation value))))))))

(deftest spoofed-origin-phase-and-basis-refused
  (let [input (input!)]
    (doseq [[k v] {:observation-origin :authenticated-executor
                   :read-phase :before-attempt
                   :reported-read-basis " "}]
      (is (= :unsupported-read-origin-phase-or-basis
             (reason #(provenance/audit-outcome-state!
                       (assoc-in input [:state-observation k] v))))))
    (doseq [invalid-basis [[] {} 1.25 :trusted-snapshot]]
      (is (= :unsupported-read-origin-phase-or-basis
             (reason #(provenance/audit-outcome-state!
                       (assoc-in input [:state-observation :reported-read-basis]
                                 invalid-basis))))))
    (doseq [basis ["unverified-token" 42 (random-uuid)]]
      (let [r (provenance/audit-outcome-state!
               (assoc-in input [:state-observation :reported-read-basis] basis))]
        (is (true? (:post-read-basis-present? r)))
        (is (false? (:post-read-basis-authenticated? r)))
        (no-authority! r)))))

(deftest caller-can-forge-direct-query-origin-but-not-authenticate-it
  (let [input (assoc-in (input!)
                        [:state-observation :observation-origin]
                        :gesso-xtdb-query-on-caller-node)
        r (provenance/audit-outcome-state! input)]
    (is (= :gesso-xtdb-query-on-caller-node (:post-read-origin-claim r)))
    (is (false? (:post-read-origin-authenticated? r)))
    (no-authority! r)))

(deftest missing-extra-and-reordered-post-read-targets-refused
  (let [input (input!)
        rows (get-in input [:state-observation :post-rows])]
    (is (= :missing-or-extra-post-read-targets
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows] (pop rows))))))
    (is (= :missing-or-extra-post-read-targets
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows]
                               (conj rows (first rows)))))))
    (is (= :reordered-or-mismatched-read-target
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows]
                               (vec (reverse rows)))))))
    (is (= :reordered-or-mismatched-read-target
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows 0 :target]
                               [:forged :id])))))
    (is (= :missing-or-extra-post-read-targets
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows]
                               (with-meta rows {:verified? true}))))))
    (is (= :missing-or-extra-post-read-targets
           (reason #(provenance/audit-outcome-state!
                     (assoc-in input [:state-observation :post-rows]
                               (seq rows))))))))

(deftest invalid-row-shapes-and-documents-refused
  (let [input (input!)
        row (get-in input [:state-observation :post-rows 0])]
    (doseq [invalid [(assoc row :proof true)
                     (dissoc row :target)
                     (with-meta row {:proof true})]]
      (is (= :invalid-source-shape
             (reason #(provenance/audit-outcome-state!
                       (assoc-in input [:state-observation :post-rows 0]
                                 invalid))))))
    (doseq [invalid ["raw" [:not-a-document]
                     (with-meta (:document row) {:proof true})]]
      (is (= :invalid-read-document
             (reason #(provenance/audit-outcome-state!
                       (assoc-in input [:state-observation :post-rows 0 :document]
                                 invalid))))))))

(deftest bounded-projection-mismatches-report-index-not-domain-values
  (let [input (input!)
        one (provenance/audit-outcome-state!
             (assoc-in input [:state-observation :post-rows 0 :document]
                       {:xt/id :wrong}))
        both (-> input
                 (assoc-in [:state-observation :post-rows 0 :document] nil)
                 (assoc-in [:state-observation :post-rows 1 :document] nil)
                 provenance/audit-outcome-state!)]
    (is (= :observed-committed-projection-diverges (:status one)))
    (is (= [0] (:post-read-mismatched-target-indices one)))
    (is (= [] (:post-read-absent-target-indices one)))
    (is (= 1 (:post-read-matching-target-count one)))
    (is (false? (:bounded-post-state-agreement? one)))
    (is (= :observed-committed-projection-diverges (:status both)))
    (is (= [0 1] (:post-read-mismatched-target-indices both)))
    (is (= [0 1] (:post-read-absent-target-indices both)))
    (is (false? (:command-record-read-present? both)))
    (is (false? (:command-record-expected-fields-match? both)))
    (is (not (re-find #"wrong|gesso.command/binding"
                      (pr-str one))))
    (no-authority! both)))

(deftest expected-fields-only-is-not-full-write-frame
  (let [input (input!)
        original (get-in input [:state-observation :post-rows 0 :document])
        r (provenance/audit-outcome-state!
           (assoc-in input [:state-observation :post-rows 0 :document]
                     (assoc original :unknown/extra-physical-write 7)))]
    (is (= :observed-committed-projection-agrees (:status r)))
    (is (:bounded-post-state-agreement? r))
    (is (= :not-established (:complete-physical-write-footprint r)))
    (no-authority! r)))

(deftest stale-and-forged-reports-rejected
  (let [input (input!)
        r (provenance/audit-outcome-state! input)]
    (doseq [forged [(assoc r :authority :certified)
                    (assoc r :certificate :issued)
                    (assoc r :bounded-post-state-agreement? false)
                    (dissoc r :physical-refinement-obligations)
                    (assoc r :physical-refinement-obligations [])]]
      (is (= :stale-or-altered-outcome-state-report
             (reason #(provenance/require-current-outcome-state! input forged)))))
    (is (= :stale-or-altered-outcome-state-report
           (reason #(provenance/require-current-outcome-state!
                     (assoc-in input [:state-observation :post-rows 0 :document]
                               nil)
                     r))))
    (is (= r (provenance/require-current-outcome-state! input r)))))

(deftest altered-submission-and-acquisition-never-inherit-acceptance
  (let [input (input!)
        src (:submission-source input)
        ops (:recorded-tx-ops src)
        altered (assoc src :recorded-tx-ops
                       (assoc ops 0 [:put-docs :forged {:xt/id :forged}]))]
    (is (some? (reason #(provenance/audit-outcome-state!
                        (assoc input :submission-source altered)))))
    (is (some? (reason #(provenance/audit-outcome-state!
                        (assoc-in input
                                  [:submission-source :acquisition-report
                                   :issued-application-fingerprint]
                                  :forged)))))
    (is (some? (reason #(provenance/audit-outcome-state!
                        (assoc-in input [:submission-report :authority]
                                  :certified)))))
    (is (some? (reason #(provenance/audit-outcome-state!
                        (assoc-in input [:submission-source :acquisition-source
                                         :operation-id]
                                  :example/not-claim)))))))

(deftest exact-time-comparison-does-not-silently-truncate-to-microseconds
  ;; v1378 XTDB readback revealed fractional nanoseconds discarded at physical
  ;; storage. Equality must not silently round or truncate the expected plan.
  ;; This directly exercises the small representation boundary; the externally
  ;; observable integration path is covered by the real-XTDB test below.
  (let [same-instant (Instant/parse "2026-11-05T00:00:00.123456789Z")
        truncated (Instant/parse "2026-11-05T00:00:00.123456000Z")
        zoned (.atZone same-instant (ZoneId/of "UTC"))
        lower-precision (.atZone truncated (ZoneId/of "UTC"))
        equals? (var-get #'provenance/persisted-value-agrees?)]
    (is (true? (equals? same-instant zoned)))
    (is (false? (equals? same-instant lower-precision)))
    (is (false? (equals? same-instant (.toString zoned))))
    (is (false? (equals? :unrelated zoned)))
    (is (true? (equals? "now" "now")))))

(deftest controlled-xtdb-readback-does-not-attest-its-caller-node
  (with-open [node (xtn/start-node {})]
    (let [id (+ 1379000 (rand-int 100000))
          initial {:xt/id id :request/status :open :request/owner nil
                   :request/revision 0 :request/organization :organization/one
                   :request/description "v1379"}
          read-one (fn [table rid]
                     (first (xt/q node [(str "SELECT * FROM " (name table)
                                             " WHERE _id = ?") rid])))
          request-now (fn []
                        (when-let [row (read-one :requests id)]
                          (if (contains? row :request/owner)
                            row (assoc row :request/owner nil))))
          request (request)
          binding (:binding request)
          session (acquisition/start-acquisition! request)
          recorded (atom [])
          original xt/execute-tx
          low-level-var (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
          environment (claim/new-claim-environment!
                       {:request-id id :actor :helper/alice
                        :organization :organization/one
                        :read-current! (fn [_ _] (request-now))
                        :authorization! (fn [_ _] {:auth/decision :permit})
                        :read-record! (fn [_ rid] (read-one :gesso_commands rid))
                        :may-disclose? (fn [_ _ _] true)})]
      (xt/execute-tx node [[:put-docs :requests initial]])
      (let [result (with-bindings
                     {low-level-var (fn [n txops options]
                                      (swap! recorded conj txops)
                                      (original n txops options))}
                     ((:executor environment)
                      {:biff.xtdb/node node
                       resolved/observation-context-key (:observe! session)}
                      {:binding binding :mode :submit}))
            acquired ((:complete! session))
            source {:acquisition-source request
                    :acquisition-report acquired
                    :recorded-tx-ops (first @recorded)}
            sr (submission/audit-recorded-submission! source)
            observed (provenance/capture-xtdb-poststate! source sr node)
            input {:submission-source source
                   :submission-report sr
                   :state-observation observed}
            report (provenance/audit-outcome-state! input)
            plan (tx-model/admitted-plan (:captured-prepared-plan acquired))
            expected-record (->> (:commands plan)
                                 (map (fn [c] [(command/target c) (command/after c)]))
                                 (filter (fn [[[table _] _]] (= :gesso_commands table)))
                                 first second)
            actual-record (get-in observed [:post-rows 1 :document])
            expected-creation (:gesso.command/created-at expected-record)
            actual-creation (:gesso.command/created-at actual-record)
            exact-time? (= expected-creation
                           (if (instance? ZonedDateTime actual-creation)
                             (.toInstant ^ZonedDateTime actual-creation)
                             actual-creation))]
        (is (= :incorporated (:status result)))
        (is (= 1 (count @recorded)))
        (is (= :recorded-operations-agree (:comparison-status report)))
        (is (= :committed (:captured-physical-status report)))
        (is (= :gesso-xtdb-query-on-caller-node (:post-read-origin-claim report)))
        (is (= [] (filter #{0} (:post-read-mismatched-target-indices report))))
        (is (= exact-time?
               (:command-record-expected-fields-match? report)))
        (is (= (if exact-time?
                 :observed-committed-projection-agrees
                 :observed-committed-projection-diverges)
               (:status report)))
        (is (= report (provenance/require-current-outcome-state! input report)))
        (no-authority! report)))))

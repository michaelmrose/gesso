(ns gesso.assurance.proof.physical-xtdb-transition-provenance-mutation-test
  "v1468: Permanent adversarial tests for the v1467 LOCAL XTDB-origin witness.

   A witness authenticates a return from the pinned controlled collector in
   this JVM, subject to the collector and caller-supplied-node assumptions.
   It does not authenticate a production node, a deployed handler, or an
   independent physical transaction receipt. In particular, equal REDACTED
   reports from separate invocations are indistinguishable as value maps.

   The synthetic tests explicitly override the pinned collector test seam to
   exercise failure boundaries. The embedded-XTDB test exercises the original
   unmodified collector against actual commit and assertion-reject outcomes."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.physical-xtdb-transition-provenance :as provenance]
            [gesso.assurance.proof.physical-xtdb-transition-observation :as phase]
            [gesso.assurance.proof.physical-xtdb-transition-observation-mutation-test :as fixture]
            [gesso.model.command :as command]
            [gesso.model.tx-refinement-xtdb :as collector]
            [xtdb.node :as xtn]))

(defn- base-raw [] ((var-get #'fixture/raw-report)))
(defn- rejected-raw [] ((var-get #'fixture/raw-report) :rejected true))
(defn- divergent-raw [] ((var-get #'fixture/raw-report) :committed false))

(defn- input
  ([] (input (Object.)))
  ([node]
   {:node node
    :plan {:commands [:secret-private-plan] :guards []}
    :targets [[:requests :secret-private-target]]}))

(defn- failure-reason [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-xtdb-transition-provenance/rejection (ex-data e))
        (:reason (ex-data e))
        :unexpected-exception))))

(defn- stub-collector!
  "Replace only the ORIGINAL pinned collector boundary with controlled
   outputs; a Throwable is thrown identically, never retried."
  [outputs run!]
  (let [counted (atom 0)
        collector-fn (fn [_]
                       (let [n (swap! counted inc)
                             output (nth outputs (dec n) ::unexpected-call)]
                         (if (instance? Throwable output)
                           (throw output)
                           (if (= ::unexpected-call output)
                             (throw (ex-info "Unexpected duplicate collector call"
                                             {:call n}))
                             output))))]
    (with-redefs [collector/observe-and-derive-conditional! collector-fn
                  provenance/original-collector collector-fn
                  phase/original-collector collector-fn]
      (run! counted))))

(deftest issued-witness-is-private-and-description-is-not-credential
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report] :as issue}
           (provenance/observe-provenanced-transition! source)
           description (provenance/require-current-process-local-witness!
                        handle source report)]
       (is (= 1 @calls))
       (is (= #{:handle :report} (set (keys issue))))
       (is (provenance/process-local-witness? handle))
       (is (= description (provenance/describe-process-local-witness handle)))
       (is (= :completed-controlled-collector-invocation (:status description)))
       (is (= :process-local-diagnostic-only (:proof-status description)))
       (is (= :none (:authority description)))
       (is (= :none (:transaction-authority description)))
       (is (= :not-issued (:certificate description)))
       (is (= :not-admitted (:admission description)))
       (is (false? (:physical-receipt-authenticated? description)))
       (is (= :none (:authority report)))
       (is (set/subset?
            #{:authenticated-production-node-and-handler
              :independent-physical-transaction-receipt
              :production-executor-to-collected-plan-correspondence
              :physical-submission-and-payload-attestation
              :complete-physical-writer-inventory
              :universal-xtdb-refinement}
            (:open-obligations description)))
       (is (= 1 @calls) "description and recheck never submit twice")))))

(deftest copied-report-or-constructed-handle-is-not-live-origin
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)
           description (provenance/describe-process-local-witness handle)
           forged (gesso.assurance.proof.physical_xtdb_transition_provenance.ControlledTransitionHandle.)]
       (is (not (identical? forged handle)))
       (doseq [candidate [nil {} [] report description forged
                          (assoc description :authority :certified)]]
         (is (false? (provenance/process-local-witness? candidate)))
         (is (= :unknown-or-forged-witness
                (failure-reason #(provenance/require-current-process-local-witness!
                                  candidate source report)))))
       (is (= :unknown-or-forged-witness
              (failure-reason #(provenance/describe-process-local-witness forged))))
       (is (= 1 @calls))))))

(deftest distinct-invocations-are-not-interchangeable-when-evidence-differs
  (stub-collector!
   [(base-raw) (rejected-raw)]
   (fn [calls]
     (let [source (input)
           first-return (provenance/observe-provenanced-transition! source)
           second-return (provenance/observe-provenanced-transition! source)
           f (:handle first-return)
           s (:handle second-return)
           first-report (:report first-return)
           second-report (:report second-return)]
       (is (= 2 @calls))
       (is (not (identical? f s)))
       (is (not= first-report second-report))
       (is (= :committed (:tx-outcome (provenance/describe-process-local-witness f))))
       (is (= :rejected (:tx-outcome (provenance/describe-process-local-witness s))))
       (is (= :stale-or-altered-phase-observation
              (failure-reason #(provenance/require-current-process-local-witness!
                                f source second-report))))
       (is (= :stale-or-altered-phase-observation
              (failure-reason #(provenance/require-current-process-local-witness!
                                s source first-report))))
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        f source first-report))))
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        s source second-report))))
       (is (= 2 @calls) "cross-witness recheck is read-only")))))

(deftest equal-redacted-reports-cannot-attest-cross-transaction-identity
  (stub-collector!
   [(base-raw) (base-raw)]
   (fn [calls]
     (let [source (input)
           first-return (provenance/observe-provenanced-transition! source)
           second-return (provenance/observe-provenanced-transition! source)]
       (is (= 2 @calls))
       (is (not (identical? (:handle first-return) (:handle second-return))))
       (is (= (:report first-return) (:report second-return)))
       ;; This is an important LIMITATION, not a positive authentication test.
       ;; Because the values are equal, swapping redacted maps is undetectable.
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        (:handle first-return) source (:report second-return)))))
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        (:handle second-return) source (:report first-return)))))
       (is (false? (:physical-receipt-authenticated?
                    (provenance/describe-process-local-witness
                     (:handle first-return)))))
       (is (= 2 @calls))))))

(deftest node-object-identity-is-required-even-with-equal-looking-input
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)
           equivalent-but-distinct (input)]
       (is (= (:plan source) (:plan equivalent-but-distinct)))
       (is (= (:targets source) (:targets equivalent-but-distinct)))
       (is (= :controlled-node-substitution
              (failure-reason #(provenance/require-current-process-local-witness!
                                handle equivalent-but-distinct report))))
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        handle (assoc source :plan (into {} (:plan source)))
                        report))))
       (is (= 1 @calls))))))

(deftest plan-target-or-observation-changes-invalidate-live-handle
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)]
       (doseq [changed [(assoc source :plan {:commands [:other]})
                        (update-in source [:plan :commands] conj :extra-write)
                        (assoc source :targets [[:requests :other-target]])
                        (update source :targets conj [:requests :extra-target])]]
         (is (= :plan-or-target-substitution
                (failure-reason #(provenance/require-current-process-local-witness!
                                  handle changed report)))))
       (doseq [changed [(assoc report :status :bounded-rejected-step-agreement)
                        (assoc report :certificate :issued)
                        (assoc report :transaction-authority :granted)
                        (assoc report :authority :production)
                        (assoc-in report [:outcome :reported-status] :rejected)
                        (assoc-in report [:readback :prestate-agrees?] false)
                        (assoc-in report [:submission :physical-payload-provenance]
                                  :authenticated)
                        (update report :open-obligations disj
                                :universal-execution-refinement)
                        (dissoc report :open-obligations)]]
         (is (= :stale-or-altered-phase-observation
                (failure-reason #(provenance/require-current-process-local-witness!
                                  handle source changed)))))
       (is (= 1 @calls))))))

(deftest invalid-input-never-reaches-collector
  (stub-collector!
   []
   (fn [calls]
     (doseq [bad [nil {} [] {:node (Object.) :plan {} :targets nil}
                  {:node nil :plan {} :targets []}
                  {:node (Object.) :plan nil :targets []}
                  {:node (Object.) :plan {} :targets ()}
                  {:node (Object.) :plan {} :targets [1] :retry true}
                  (with-meta (input) {:caller-claims-proof true})
                  (assoc (input) :node nil)]]
       (is (= :invalid-controlled-input
              (failure-reason #(provenance/observe-provenanced-transition! bad)))))
     (is (= 0 @calls)))))

(deftest pinned-collector-cannot-be-substituted-even-for-existing-witness
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)
           replacement-calls (atom 0)]
       (with-redefs [collector/observe-and-derive-conditional!
                     (fn [_] (swap! replacement-calls inc) (base-raw))]
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/observe-provenanced-transition!
                                  source))))
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/require-current-process-local-witness!
                                  handle source report))))
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/describe-process-local-witness
                                  handle)))))
       (is (zero? @replacement-calls))
       (is (= :completed-controlled-collector-invocation
              (:status (provenance/require-current-process-local-witness!
                        handle source report))))
       (is (= 1 @calls))))))

(deftest pinned-classifier-cannot-be-substituted-or-bypass-validation
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)]
       (with-redefs [phase/classify-controlled-report! (fn [_] {:certificate :issued})]
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/observe-provenanced-transition!
                                  source))))
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/require-current-process-local-witness!
                                  handle source report))))
         (is (= :substituted-collector-or-classifier
                (failure-reason #(provenance/describe-process-local-witness handle)))))
       (is (= 1 @calls))))))

(deftest malformed-or-contradictory-collector-output-issues-no-witness
  (let [corrupt (assoc (base-raw) :certificate :forged)]
    (stub-collector!
     [corrupt (base-raw)]
     (fn [calls]
       (let [failure (try
                       (provenance/observe-provenanced-transition! (input))
                       :not-rejected
                       (catch clojure.lang.ExceptionInfo e e))]
         (is (instance? clojure.lang.ExceptionInfo failure))
         ;; Source classification errors keep their OWN typed rejection;
         ;; the provenance wrapper must not swallow or downgrade them.
         (when (instance? clojure.lang.ExceptionInfo failure)
           (is (true? (:physical-xtdb-transition-observation/rejection
                       (ex-data failure))))
           (is (= :malformed-or-inconsistent-controlled-report
                  (:reason (ex-data failure))))))
       (is (= 1 @calls))
       (let [success (provenance/observe-provenanced-transition! (input))]
         (is (provenance/process-local-witness? (:handle success))))
       (is (= 2 @calls) "invalid output must not trigger automatic retry")))))

(deftest unknown-post-submission-outcome-is-rethrown-identically
  (let [unknown (ex-info "secret unknown outcome" {:error/type :commit-unknown})]
    (stub-collector!
     [unknown (base-raw)]
     (fn [calls]
       (let [source (input)]
         (is (identical? unknown
                         (try (provenance/observe-provenanced-transition! source)
                              (catch Throwable e e))))
         (is (= 1 @calls))
         (is (= :completed-controlled-collector-invocation
                (:status (provenance/describe-process-local-witness
                          (:handle (provenance/observe-provenanced-transition!
                                    source))))))
         (is (= 2 @calls) "the second call is explicit, not an automatic retry"))))))

(deftest disagreeing-physical-observations-still-have-only-diagnostic-status
  (stub-collector!
   [(divergent-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)
           description (provenance/require-current-process-local-witness!
                        handle source report)]
       (is (= :bounded-observations-diverge (:status report)))
       (is (false? (:model-step-agreement description)))
       (is (= :none (:authority description)))
       (is (= :process-local-diagnostic-only (:proof-status description)))
       (is (contains? (:open-obligations description) :universal-xtdb-refinement))
       (is (= 1 @calls))))))

(deftest private-plan-and-physical-identifiers-never-enter-public-description
  (stub-collector!
   [(base-raw)]
   (fn [calls]
     (let [source (input)
           {:keys [handle report]} (provenance/observe-provenanced-transition! source)
           description (provenance/describe-process-local-witness handle)
           printed (pr-str description)]
       (is (= :not-authenticated
              (get-in report [:submission :physical-payload-provenance])))
       (is (not (re-find #"secret-private-plan|secret-private-target|:node\\s|:plan\\s|:targets\\s|physical-tx-id|snapshot-token|:physical-tx-id" printed)))
       (is (not (contains? description :node)))
       (is (not (contains? description :plan)))
       (is (not (contains? description :targets)))
       (is (not (contains? description :handle)))
       (is (= 1 @calls))))))

(deftest real-embedded-xtdb-commit-and-rejection-remain-distinct-and-unprivileged
  (with-open [node (xtn/start-node {})]
    (let [revision {:revision-key :revision
                    :created-at-key :created-at
                    :updated-at-key :updated-at}
          doc {:xt/id "v1468-private-xtdb-id"
               :revision 0
               :created-at "now0"
               :updated-at "now0"
               :owner "v1468-private-owner"}
          source {:node node
                  :plan {:commands [(command/create :requests doc revision)]
                         :guards [] :emit false}
                  :targets [[:requests (:xt/id doc)]]}
          first-return (provenance/observe-provenanced-transition! source)
          second-return (provenance/observe-provenanced-transition! source)
          first-description (provenance/require-current-process-local-witness!
                             (:handle first-return) source (:report first-return))
          second-description (provenance/require-current-process-local-witness!
                              (:handle second-return) source (:report second-return))]
      (is (not (identical? (:handle first-return) (:handle second-return))))
      (is (= :committed (:tx-outcome first-description)))
      (is (= :rejected (:tx-outcome second-description)))
      (is (= :bounded-committed-step-agreement
             (get-in first-return [:report :status])))
      (is (= :bounded-rejected-step-agreement
             (get-in second-return [:report :status])))
      (is (= :stale-or-altered-phase-observation
             (failure-reason #(provenance/require-current-process-local-witness!
                               (:handle first-return) source (:report second-return)))))
      (is (false? (:physical-receipt-authenticated? first-description)))
      (is (= :none (:authority second-description)))
      (is (not (re-find #"v1468-private-xtdb-id|v1468-private-owner"
                        (str (pr-str first-description)
                             (pr-str second-description))))))))

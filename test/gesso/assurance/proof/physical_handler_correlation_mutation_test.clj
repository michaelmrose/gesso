(ns gesso.assurance.proof.physical-handler-correlation-mutation-test
  "v1470: Permanent fail-closed tests for v1469 synchronous handler/XTDB
   correlation. These prove in-JVM controlled-invocation behavior only,
   NOT certified-route deployment, issued-executor correspondence, or
   authenticated physical receipts. The mocked collector cases substitute
   the already pinned v1467 fixture seam; the embedded-XTDB case performs
   actual controlled commits and assertion rejects."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.physical-handler-correlation :as correlation]
            [gesso.assurance.proof.physical-xtdb-transition-provenance :as provenance]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.assurance.proof.physical-xtdb-transition-observation :as phase]
            [gesso.assurance.proof.physical-xtdb-transition-observation-mutation-test :as fixture]
            [gesso.model.tx-refinement-xtdb :as collector]
            [gesso.model.command :as command]
            [xtdb.node :as xtn]))

(defn- input ([] (input (Object.)))
  ([node] {:node node
           :plan {:commands [:private-plan] :guards []}
           :targets [[:requests :private-target]]}))

(defn- reject-reason [f]
  (try (f) :no-rejection
       (catch clojure.lang.ExceptionInfo e
         (if (:physical-handler-correlation/rejection (ex-data e))
           (:reason (ex-data e)) :unrelated-exception))))

(defn- mocked [outputs run!]
  (let [calls (atom 0)
        fake (fn [_]
               (let [n (swap! calls inc)
                     result (nth outputs (dec n) ::extra)]
                 (cond (instance? Throwable result) (throw result)
                       (= result ::extra) (throw (ex-info "unexpected extra write" {}))
                       :else result)))]
    (with-redefs [collector/observe-and-derive-conditional! fake
                  provenance/original-collector fake
                  phase/original-collector fake]
      (run! calls))))

(deftest synchronous-handler-binds-exact-function-and-one-physical-witness
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           handler (fn [{:keys [tag]}]
                     (let [r (correlation/observe-in-handler! source)]
                       {:tag tag :status (get-in r [:outcome :reported-status])}))
           request {:tag :private-request}
           {:keys [response handle report]} (correlation/invoke-correlated-handler!
                                            handler request)]
       (is (= 1 @calls))
       (is (= {:tag :private-request :status :committed} response))
       (is (correlation/handler-correlation? handle))
       (is (= report (correlation/describe-handler-correlation handle)))
       (is (= report (correlation/require-current-handler-correlation!
                      handle handler request report)))
       (is (= :one-controlled-step-inside-synchronous-handler (:status report)))
       (is (= :one-handler-invocation-on-original-thread (:capture-origin report)))
       (is (= :identical-in-process-argument (:request-binding report)))
       (is (false? (:registered-issued-handler-at-invocation? report)))
       (is (= :not-established (:issued-executor-to-physical-plan report)))
       (is (false? (:physical-receipt-authenticated? report)))
       (is (= :none (:authority report)))
       (is (= :not-issued (:certificate report)))
       (is (= :not-admitted (:admission report)))
       (is (= :none (:transaction-authority report)))
       (is (contains? (:open-obligations report)
                      :current-issued-executor-plan-and-collector-plan-equivalence))
       (is (not (re-find #"private-plan|private-target|private-request"
                         (pr-str report))))
       (is (= 1 @calls) "Rechecking cannot perform a second transaction")))))

(deftest no-observation-and-nested-handler-invocations-fail-closed
  (mocked []
   (fn [calls]
     (is (= :missing-completed-controlled-transition
            (reject-reason #(correlation/invoke-correlated-handler!
                              identity :ctx))))
     (is (= :nested-handler-correlation-scope
            (reject-reason
             #(correlation/invoke-correlated-handler!
               (fn [ctx] (correlation/invoke-correlated-handler! identity ctx))
               :ctx))))
     (is (= :no-current-synchronous-handler-scope
            (reject-reason #(correlation/observe-in-handler! (input)))))
     (is (zero? @calls)))))

(deftest duplicate-observation-cannot-be-hidden-by-handler-catching-rejection
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           caught (atom nil)]
       (is (= :multiple-observation-attempts
              (reject-reason
               (fn [] (correlation/invoke-correlated-handler!
                 (fn [_]
                   (correlation/observe-in-handler! source)
                   (reset! caught
                           (reject-reason
                            #(correlation/observe-in-handler! source)))
                   :response)
                 :ctx)))))
       (is (= :second-or-reentrant-controlled-transition @caught))
       (is (= 1 @calls) "Second attempt must never reach XTDB")))))

(deftest source-rechecking-rejects-swapped-handler-and-mutated-report
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           handler (fn [_] (correlation/observe-in-handler! source))
           request {:id :original-request}
           {:keys [handle report]} (correlation/invoke-correlated-handler! handler request)
           forged (gesso.assurance.proof.physical_handler_correlation.HandlerCorrelationHandle.)]
       (doseq [wrong [nil {} report forged]]
         (is (= :unknown-or-forged-handler-correlation
                (reject-reason #(correlation/describe-handler-correlation wrong)))))
       (is (= :handler-identity-substitution
              (reject-reason #(correlation/require-current-handler-correlation!
                                handle (fn [_] nil) request report))))
       (is (= :request-identity-substitution
              (reject-reason #(correlation/require-current-handler-correlation!
                                handle handler (into {} request) report))))
       (doseq [bad [(assoc report :certificate :issued)
                    (assoc report :authority :production)
                    (assoc report :issued-executor-to-physical-plan :established)
                    (dissoc report :open-obligations)
                    (update report :open-obligations disj
                            :complete-physical-writer-inventory)
                    (with-meta report {:proof :forged})]]
         (is (= :stale-or-forged-handler-report
                (reject-reason #(correlation/require-current-handler-correlation!
                                  handle handler request bad)))))
       (is (= 1 @calls))))))

(deftest different-invocations-may-have-identical-copyable-reports-but-distinct-handles
  (let [raw ((var-get #'fixture/raw-report))]
    (mocked [raw raw]
     (fn [calls]
       (let [source (input)
             handler (fn [_] (correlation/observe-in-handler! source))
             a (correlation/invoke-correlated-handler! handler :a)
             b (correlation/invoke-correlated-handler! handler :b)]
         (is (= 2 @calls))
         (is (not (identical? (:handle a) (:handle b))))
         (is (= (:report a) (:report b)))
         (is (= :one-controlled-step-inside-synchronous-handler
                (:status (correlation/require-current-handler-correlation!
                          (:handle a) handler :a (:report b)))))
         (is (= 2 @calls)))))))

(deftest thrown-physical-outcome-propagates-exactly-once-without-correlation
  (let [thrown (ex-info "private unknown" {:commit :unknown})]
    (mocked [thrown]
     (fn [calls]
       (is (identical? thrown
                       (try (correlation/invoke-correlated-handler!
                             (fn [_] (correlation/observe-in-handler! (input)))
                             :ctx)
                            (catch Throwable t t))))
       (is (= 1 @calls))))))

(deftest handler-catching-physical-exception-cannot-produce-a-success-witness
  (let [failure (ex-info "private commit unknown" {:status :unknown})]
    (mocked [failure]
     (fn [calls]
       (is (= :caught-unknown-physical-outcome
              (reject-reason
               (fn []
                 (correlation/invoke-correlated-handler!
                  (fn [_]
                    (try (correlation/observe-in-handler! (input))
                         (catch Throwable _ :handled)))
                  :request)))))
       (is (= 1 @calls))))))

(deftest cross-thread-calls-are-not-correlated
  (mocked []
   (fn [calls]
     (let [reason (atom nil)
           handler (fn [_]
                     (let [worker (bound-fn []
                                    (reset! reason
                                            (reject-reason
                                             (fn []
                                               (correlation/observe-in-handler!
                                                (input))))))
                           thread (Thread. (reify Runnable
                                             (run [_] (worker))))]
                       (.start thread)
                       (.join thread 5000))
                     :response)]
       (is (= :missing-completed-controlled-transition
              (reject-reason (fn [] (correlation/invoke-correlated-handler!
                                    handler :ctx)))))
       (is (= :no-current-synchronous-handler-scope @reason))
       (is (zero? @calls))))))

(deftest collector-function-substitution-is-rejected-before-invocation
  (let [calls (atom 0)]
    (with-redefs [provenance/observe-provenanced-transition!
                  (fn [_] (swap! calls inc) {})]
      (is (= :substituted-correlation-boundary
             (reject-reason #(correlation/invoke-correlated-handler!
                               (fn [_] :response) :ctx)))))
    (is (zero? @calls))))

(deftest actual-embedded-xtdb-commit-and-rejection-remain-distinct
  (with-open [node (xtn/start-node {})]
    (let [revision {:revision-key :revision
                    :created-at-key :created-at
                    :updated-at-key :updated-at}
          document {:xt/id "v1470-private-id" :revision 0
                    :created-at "t0" :updated-at "t0"
                    :owner "v1470-private-owner"}
          plan {:commands [(command/create :requests document revision)]
                :guards [] :emit false}
          input {:node node :plan plan
                 :targets [[:requests (:xt/id document)]]}
          handler (fn [_] (correlation/observe-in-handler! input))
          committed (correlation/invoke-correlated-handler! handler :request)
          rejected (correlation/invoke-correlated-handler! handler :request)]
      (is (= :committed
             (get-in committed [:report :physical-observation :outcome :reported-status])))
      (is (= :rejected
             (get-in rejected [:report :physical-observation :outcome :reported-status])))
      (is (not= (:report committed) (:report rejected)))
      (is (not (identical? (:handle committed) (:handle rejected))))
      (is (= :one-controlled-step-inside-synchronous-handler
             (:status (correlation/require-current-handler-correlation!
                       (:handle committed) handler :request (:report committed)))))
      (is (not (re-find #"v1470-private-id|v1470-private-owner"
                        (pr-str (:report committed))))))))

(deftest correlation-does-not-substitute-for-issued-handler-registration
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           ordinary (fn [_] (correlation/observe-in-handler! source))
           annotated (with-meta ordinary {:gesso/certified-handler true
                                          :verified-proof true
                                          :transaction-authority :all})
           request (Object.)
           {:keys [handle report]} (correlation/invoke-correlated-handler!
                                    annotated request)]
       (is (not (certified/certified-handler? annotated)))
       (is (false? (:registered-issued-handler-at-invocation? report)))
       (is (= :not-established (:issued-handler-current-source report)))
       (is (= :not-established (:issued-executor-to-physical-plan report)))
       (is (false? (:physical-receipt-authenticated? report)))
       (is (set/subset?
            #{:actual-deployed-route-and-handler-adoption
              :current-issued-executor-plan-and-collector-plan-equivalence
              :physical-transaction-receipt-authenticity
              :actual-authorization-and-request-input-binding
              :complete-physical-writer-inventory}
            (:open-obligations report)))
       (is (= report (correlation/require-current-handler-correlation!
                      handle annotated request report)))
       (is (= :handler-identity-substitution
              (reject-reason #(correlation/require-current-handler-correlation!
                                handle ordinary request report))))
       (is (= 1 @calls))))))

(deftest copied-request-value-and-copied-report-are-not-identity-witnesses
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           original-request {:private :value :number 78}
           equal-but-new (into (array-map) original-request)
           handler (fn [_] (correlation/observe-in-handler! source))
           {:keys [handle report]} (correlation/invoke-correlated-handler!
                                    handler original-request)]
       (is (= original-request equal-but-new))
       (is (not (identical? original-request equal-but-new)))
       (is (= :request-identity-substitution
              (reject-reason #(correlation/require-current-handler-correlation!
                                handle handler equal-but-new report))))
       (is (= :unknown-or-forged-handler-correlation
              (reject-reason #(correlation/describe-handler-correlation
                                (:physical-observation report)))))
       (is (= :unknown-or-forged-handler-correlation
              (reject-reason #(correlation/describe-handler-correlation
                                (gesso.assurance.proof.physical_handler_correlation.HandlerCorrelationHandle.)))))
       (is (= report (correlation/require-current-handler-correlation!
                      handle handler original-request report)))
       (is (= 1 @calls))))))

(deftest all-public-authority-escalations-and-open-obligation-removals-rejected
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           handler (fn [_] (correlation/observe-in-handler! source))
           request (Object.)
           {:keys [handle report]} (correlation/invoke-correlated-handler!
                                    handler request)
           changes [(assoc report :certificate :issued)
                    (assoc report :authority :production)
                    (assoc report :transaction-authority :submit)
                    (assoc report :admission :admitted)
                    (assoc report :proof-status :fully-proven)
                    (assoc report :scope :all-installed-physical-xtdb-writes)
                    (assoc report :physical-receipt-authenticated? true)
                    (assoc report :registered-issued-handler-at-invocation? true)
                    (assoc report :issued-handler-current-source :established)
                    (assoc report :issued-executor-to-physical-plan :proven)
                    (assoc report :controlled-observations 2)
                    (dissoc report :open-obligations)
                    (assoc report :open-obligations #{})
                    (update report :open-obligations disj
                            :current-issued-executor-plan-and-collector-plan-equivalence)
                    (assoc-in report [:physical-observation :outcome
                                      :physical-receipt-authenticated?] true)
                    (with-meta report {:gesso/accepted true})]]
       (is (= :one-controlled-step-inside-synchronous-handler (:status report)))
       (doseq [changed changes]
         (is (= :stale-or-forged-handler-report
                (reject-reason #(correlation/require-current-handler-correlation!
                                  handle handler request changed)))))
       (is (= 1 @calls))))))

(deftest handler-errors-propagate-identically-without-submitting
  (mocked []
   (fn [calls]
     (let [exception (ex-info "private handler exception" {:token :private})
           thrown (try (correlation/invoke-correlated-handler!
                        (fn [_] (throw exception)) (Object.))
                       (catch Throwable t t))]
       (is (identical? exception thrown))
       (is (= 0 @calls))))))

(deftest callback-after-completed-handler-is-not-a-new-observation
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [late (atom nil)
           source (input)
           handler (fn [_]
                     (reset! late #(correlation/observe-in-handler! source))
                     (correlation/observe-in-handler! source)
                     :response)
           issued (correlation/invoke-correlated-handler! handler :request)]
       (is (= :response (:response issued)))
       (is (= :no-current-synchronous-handler-scope
              (reject-reason @late)))
       (is (= (:report issued)
              (correlation/require-current-handler-correlation!
               (:handle issued) handler :request (:report issued))))
       (is (= 1 @calls))))))

(deftest handler-cannot-recover-from-second-attempt-by-catching-exception
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           all-reasons (atom [])]
       (is (= :multiple-observation-attempts
              (reject-reason (fn [] (correlation/invoke-correlated-handler!
                               (fn [_]
                                 (correlation/observe-in-handler! source)
                                 (dotimes [_ 2]
                                   (swap! all-reasons conj
                                          (reject-reason
                                           #(correlation/observe-in-handler! source))))
                                 :pretend-success)
                               :request)))))
       (is (= [:second-or-reentrant-controlled-transition
               :second-or-reentrant-controlled-transition] @all-reasons))
       (is (= 1 @calls))))))

(deftest source-boundary-replacement-rejected-before-handler-and-submission
  (let [run-count (atom 0)
        handler (fn [_] (swap! run-count inc) :unreachable)
        replacement (fn [_] nil)]
    (with-redefs [provenance/observe-provenanced-transition! replacement]
      (is (= :substituted-correlation-boundary
             (reject-reason #(correlation/invoke-correlated-handler!
                               handler :request)))))
    (with-redefs [provenance/require-current-process-local-witness! replacement]
      (is (= :substituted-correlation-boundary
             (reject-reason #(correlation/invoke-correlated-handler!
                               handler :request)))))
    (with-redefs [certified/certified-handler? replacement]
      (is (= :substituted-correlation-boundary
             (reject-reason #(correlation/invoke-correlated-handler!
                               handler :request)))))
    (is (= 0 @run-count))))

(deftest response-is-preserved-but-not-proven-to-come-from-issued-executor
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           raw-response {:body {:private-data "not provenance"}
                         :status 201}
           handler (fn [_]
                     (correlation/observe-in-handler! source)
                     raw-response)
           request (Object.)
           {:keys [response handle report]} (correlation/invoke-correlated-handler!
                                             handler request)]
       (is (identical? raw-response response))
       (is (not (re-find #"not provenance|private-data"
                         (pr-str report))))
       (is (= :not-established (:issued-executor-to-physical-plan report)))
       (is (= :none (:transaction-authority report)))
       (is (= report (correlation/require-current-handler-correlation!
                      handle handler request report)))
       (is (= 1 @calls))))))

(deftest descriptions-and-rechecks-never-repeat-physical-collector
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           request (Object.)
           handler (fn [_] (correlation/observe-in-handler! source))
           {:keys [handle report]} (correlation/invoke-correlated-handler!
                                    handler request)]
       (dotimes [_ 5]
         (is (= report (correlation/describe-handler-correlation handle)))
         (is (= report (correlation/require-current-handler-correlation!
                        handle handler request report))))
       (is (= 1 @calls))))))

(deftest no-successful-correlation-after-unknown-outcome-even-if-caught
  (let [failure (ex-info "unknown XTDB outcome, not safe to retry"
                         {:private true})]
    (mocked [failure]
     (fn [calls]
       (let [caught (atom nil)]
         (is (= :caught-unknown-physical-outcome
                (reject-reason #(correlation/invoke-correlated-handler!
                                 (fn [_]
                                   (try (correlation/observe-in-handler! (input))
                                        (catch Throwable t (reset! caught t)))
                                   :pretend-success)
                                 :request))))
         (is (identical? failure @caught))
         (is (= 1 @calls)))))))

(deftest diagnosed-rejection-does-not-leak-private-request-or-plan
  (mocked [((var-get #'fixture/raw-report))]
   (fn [calls]
     (let [source (input)
           handler (fn [_] (correlation/observe-in-handler! source))
           request {:private "v1470-sensitive-request-secret"}
           {:keys [handle report]} (correlation/invoke-correlated-handler!
                                    handler request)
           failed (try (correlation/require-current-handler-correlation!
                        handle handler (Object.) report)
                       (catch clojure.lang.ExceptionInfo e e))]
       (is (instance? clojure.lang.ExceptionInfo failed))
       (is (= :request-identity-substitution (:reason (ex-data failed))))
       (is (not (re-find #"v1470-sensitive-request-secret|private-plan|private-target"
                         (str (.getMessage failed) " " (pr-str (ex-data failed))))))
       (is (= 1 @calls))))))

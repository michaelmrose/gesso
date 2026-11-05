(ns gesso.assurance.proof.xtdb-submission-outcome-correlation-test
  "v1393: Permanent adversarial checks of v1392's process-local XTDB
   submission/identity/ledger correlation. Agreement is not an XTDB receipt,
   a payload attestation, or a physical-refinement certificate."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-submission-outcome-correlation :as c]
            [gesso.live.consistency.xtdb :as live]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant ZoneOffset ZonedDateTime]
           [xtdb.api TransactionKey]))

(def ^:private at (Instant/parse "2026-02-14T10:11:12.123456Z"))
(def ^:private ops
  [[:put-docs :v1393_submission_correlation
    {:xt/id "diagnostic-only" :sensitive "must-not-escape"}]])

(defn- tx-key
  ([id] (tx-key id at))
  ([id instant]
   (reify TransactionKey
     (getTxId [_] id)
     (getSystemTime [_] instant))))

(defn- seam [namespace symbol]
  (or (ns-resolve namespace symbol)
      (throw (ex-info "Missing test seam" {:namespace namespace :symbol symbol}))))

(defn- with-adapter [api function body]
  (with-redefs-fn {(seam 'gesso.live.consistency.xtdb api) function} body))

(defn- with-ledger [function body]
  ;; A ledger stub controls the later observation only, never the API call.
  (with-redefs-fn
    {(seam 'gesso.assurance.proof.xtdb-submission-outcome-correlation
           'ledger-row) function}
    body))

(defn- observe-submit [node]
  (c/observe-one-submission! node #(live/submit-tx! node ops)))

(defn- observe-execute [node]
  (c/observe-one-submission! node #(live/execute-tx! node ops)))

(defn- checks [id instant]
  {:status :observed
   :row {:xt/id id :committed true :system-time instant}})

(defn- assert-no-authority [diagnostic]
  (is (= c/report-type (:type diagnostic)))
  (is (= c/report-version (:version diagnostic)))
  (is (= :process-local-adapter-and-caller-node
         (:observation-origin diagnostic)))
  (is (false? (:actual-xtdb-api-authenticated? diagnostic)))
  (is (= :not-established (:node-identical-to-submission-connectable diagnostic)))
  (is (= :not-established (:transaction-internal-prestate diagnostic)))
  (is (= :not-established (:complete-writer-coverage diagnostic)))
  (is (= 9 (count (:open-obligations diagnostic))))
  (is (= :diagnostic-only (:proof-status diagnostic)))
  (is (= :not-issued (:certificate diagnostic)))
  (is (= :not-admitted (:admission diagnostic)))
  (is (= :none (:authority diagnostic)))
  (is (not (contains? diagnostic :tx-ops)))
  (is (not (contains? diagnostic :events)))
  (is (not (contains? diagnostic :error)))
  (is (not (re-find #"must-not-escape" (pr-str diagnostic)))))

(deftest invalid-invocations-reject-before-submitting-test
  (let [invocations (atom 0)]
    (doseq [[node thunk] [[nil #(swap! invocations inc)]
                          [:fake nil]
                          [:fake :not-a-function]
                          [:fake {:function :not-allowed}]]]
      (let [e (try (c/observe-one-submission! node thunk)
                   nil
                   (catch clojure.lang.ExceptionInfo x x))]
        (is (instance? clojure.lang.ExceptionInfo e))
        (is (true? (:submission-outcome-correlation/rejection (ex-data e))))))
    (is (zero? @invocations))))

(deftest no-submission-cannot-be-correlated-test
  (let [r (c/observe-one-submission! :fake (fn [] :read-only))
        d (:diagnostic r)]
    (is (= {:outcome :returned :result :read-only} (:execution r)))
    (is (= :not-one-complete-api-call (:status d)))
    (is (nil? (:call-id d)))
    (is (nil? (:reported-tx-id d)))
    (assert-no-authority d)))

(deftest one-submit-id-is-not-a-commit-test
  (let [query-count (atom 0)]
    (with-adapter '*submit-tx* (fn [_ _ _] {:tx-id 17})
      #(with-ledger (fn [& _] (swap! query-count inc) (checks 17 at))
         (fn []
           (let [r (observe-submit :fake)
                 d (:diagnostic r)]
             (is (= :returned (get-in r [:execution :outcome])))
             (is (= :submit-tx (:api d)))
             (is (= 17 (:reported-tx-id d)))
             (is (nil? (:reported-system-time d)))
             (is (= :submission-id-only (:status d)))
             (is (= :not-applicable (get-in d [:ledger-observation :status])))
             (is (zero? @query-count))
             (assert-no-authority d)))))))

(deftest submit-id-shape-cannot-be-guessed-test
  (doseq [[value expected-id status]
          [[{:tx-id "17"} nil :unrecognized-public-transaction-identity]
           [{:tx-id -1} nil :unrecognized-public-transaction-identity]
           [{:tx-id 3.5} nil :unrecognized-public-transaction-identity]
           [{:tx-id (inc (bigint Long/MAX_VALUE))} nil :unrecognized-public-transaction-identity]
           [{:transaction-id 17} nil :unrecognized-public-transaction-identity]
           [nil nil :unrecognized-public-transaction-identity]
           [{:tx-id Long/MAX_VALUE} Long/MAX_VALUE :submission-id-only]]]
    (with-adapter '*submit-tx* (fn [_ _ _] value)
      #(let [d (:diagnostic (observe-submit :fake))]
         (is (= status (:status d)))
         (is (= expected-id (:reported-tx-id d)))
         (assert-no-authority d)))))

(deftest identical-public-id-does-not-collapse-replayed-calls-test
  (with-adapter '*submit-tx* (fn [_ _ _] {:tx-id 31})
    #(let [first-run (observe-submit :fake)
           second-run (observe-submit :fake)
           a (:diagnostic first-run)
           b (:diagnostic second-run)]
       (is (= 31 (:reported-tx-id a) (:reported-tx-id b)))
       (is (uuid? (:call-id a)))
       (is (uuid? (:call-id b)))
       (is (not= (:call-id a) (:call-id b)))
       (is (= :submission-id-only (:status a) (:status b)))
       (assert-no-authority a)
       (assert-no-authority b))))

(deftest repeated-submissions-in-one-capture-are-not-one-transaction-test
  (with-adapter '*submit-tx* (fn [_ _ _] {:tx-id 32})
    #(let [times (atom 0)
           r (c/observe-one-submission! :fake
                 (fn [] (live/submit-tx! :fake ops)
                        (swap! times inc)
                        (live/submit-tx! :fake ops)
                        (swap! times inc)))
           d (:diagnostic r)]
       (is (= 2 @times))
       (is (= :not-one-complete-api-call (:status d)))
       (is (nil? (:reported-tx-id d)))
       (assert-no-authority d))))

(deftest mixed-submission-types-in-one-capture-are-not-one-transaction-test
  (with-adapter '*submit-tx* (fn [_ _ _] {:tx-id 33})
    #(with-adapter '*execute-tx* (fn [_ _ _] (tx-key 34))
       (fn []
         (let [d (:diagnostic
                  (c/observe-one-submission! :fake
                    (fn [] (live/submit-tx! :fake ops)
                         (live/execute-tx! :fake ops))))]
           (is (= :not-one-complete-api-call (:status d)))
           (is (nil? (:reported-tx-id d)))
           (assert-no-authority d))))))

(deftest execute-key-matches-supplied-ledger-only-diagnostically-test
  (with-adapter '*execute-tx* (fn [_ _ _] (tx-key 40))
    #(with-ledger (fn [_ key]
                    (is (= 40 (:tx-id key)))
                    (checks 40 at))
       (fn []
         (let [r (observe-execute :fake)
               d (:diagnostic r)]
           (is (= :returned (get-in r [:execution :outcome])))
           (is (= :reported-execute-ledger-agrees (:status d)))
           (is (= :execute-tx (:api d)))
           (is (= 40 (:reported-tx-id d)))
           (is (= at (:reported-system-time d)))
           (is (= {:tx-id true :committed true :system-time true}
                  (get-in d [:ledger-observation :checks])))
           (assert-no-authority d))))))

(deftest reported-ledger-with-zoned-date-time-can-match-test
  (with-adapter '*execute-tx* (fn [_ _ _] (tx-key 41))
    #(with-ledger (fn [& _] (checks 41 (ZonedDateTime/ofInstant at ZoneOffset/UTC)))
       (fn []
         (let [d (:diagnostic (observe-execute :fake))]
           (is (= :reported-execute-ledger-agrees (:status d)))
           (is (= true (get-in d [:ledger-observation :checks :system-time])))
           (assert-no-authority d))))))

(deftest ledger-key-divergence-always-blocks-agreement-test
  (doseq [[label row bad-check]
          [[:wrong-id {:xt/id 99 :committed true :system-time at} :tx-id]
           [:not-committed {:xt/id 42 :committed false :system-time at} :committed]
           [:missing-commit {:xt/id 42 :system-time at} :committed]
           [:wrong-time {:xt/id 42 :committed true :system-time (.minusNanos at 1000)} :system-time]
           [:missing-time {:xt/id 42 :committed true} :system-time]
           [:wrong-time-type {:xt/id 42 :committed true :system-time "2026-02-14T10:11:12.123456Z"} :system-time]]]
    (testing (name label)
      (with-adapter '*execute-tx* (fn [_ _ _] (tx-key 42))
        #(with-ledger (fn [& _] {:status :observed :row row})
           (fn []
             (let [d (:diagnostic (observe-execute :fake))]
               (is (= :reported-execute-ledger-unconfirmed (:status d)))
               (is (= :reported-key-diverges-from-ledger-row
                      (get-in d [:ledger-observation :status])))
               (is (= false (get-in d [:ledger-observation :checks bad-check])))
               (assert-no-authority d))))))))

(deftest unavailable-and-ambiguous-ledger-observations-remain-unknown-test
  (doseq [status [:unavailable-snapshot-token :invalid-ledger-result
                  :not-visible :ambiguous-ledger-result]]
    (testing (name status)
      (with-adapter '*execute-tx* (fn [_ _ _] (tx-key 43))
        #(with-ledger (fn [& _] {:status status})
           (fn []
             (let [d (:diagnostic (observe-execute :fake))]
               (is (= :reported-execute-ledger-unconfirmed (:status d)))
               (is (= status (get-in d [:ledger-observation :status])))
               (is (= {} (get-in d [:ledger-observation :checks])))
               (assert-no-authority d))))))))

(deftest failed-ledger-query-is-not-a-failed-execution-test
  (let [exception (ex-info "query failed; secret" {:private :secret})]
    (with-adapter '*execute-tx* (fn [_ _ _] (tx-key 44))
      #(with-ledger (fn [& _] (throw exception))
         (fn []
           (let [r (observe-execute :fake)
                 d (:diagnostic r)]
             (is (= :returned (get-in r [:execution :outcome])))
             (is (= :reported-execute-ledger-unconfirmed (:status d)))
             (is (= :ledger-query-failed (get-in d [:ledger-observation :status])))
             (is (not (re-find #"query failed; secret" (pr-str d))))
             (assert-no-authority d)))))))

(deftest execute-throw-preserves-error-and-does-not-synthesize-identity-test
  (let [failure (ex-info "private failure" {:xtdb/error :assert-failed :tx-id 45})]
    (with-adapter '*execute-tx* (fn [_ _ _] (throw failure))
      #(let [r (observe-execute :fake)
             d (:diagnostic r)]
         (is (= :threw (get-in r [:execution :outcome])))
         (is (identical? failure (get-in r [:execution :error])))
         (is (= :api-threw-outcome-unresolved (:status d)))
         (is (= :threw (:api-completion d)))
         (is (nil? (:reported-tx-id d)))
         (is (not (re-find #"private failure" (pr-str d))))
         (assert-no-authority d)))))

(deftest caught-api-throw-still-is-not-commit-test
  (with-adapter '*execute-tx*
    (fn [_ _ _] (throw (ex-info "conflict" {:tx-id 46})))
    #(let [r (c/observe-one-submission! :fake
                (fn [] (try (live/execute-tx! :fake ops)
                            (catch Exception _ :handled))))
           d (:diagnostic r)]
       (is (= {:outcome :returned :result :handled} (:execution r)))
       (is (= :api-threw-outcome-unresolved (:status d)))
       (is (= :returned (:thunk-outcome d)))
       (assert-no-authority d))))

(deftest debug-exception-after-api-return-does-not-confirm-transaction-test
  (with-adapter '*submit-tx* (fn [_ _ _] {:tx-id 47})
    #(let [r (c/observe-one-submission! :fake
                (fn [] (live/submit-tx! :fake ops
                         {:debug-fn (fn [event]
                                      (when (= :gesso.live.xtdb/submit-tx-succeeded
                                               (:event event))
                                        (throw (ex-info "debug after return" {}))))})))
           d (:diagnostic r)]
       (is (= :threw (get-in r [:execution :outcome])))
       (is (= :outer-result-diverges-from-api-return (:status d)))
       (is (= :returned (:api-completion d)))
       (is (= 47 (:reported-tx-id d)))
       (is (= :not-applicable (get-in d [:ledger-observation :status])))
       (assert-no-authority d))))

(deftest unrecognized-execute-key-remains-unverified-test
  (with-adapter '*execute-tx* (fn [_ _ _] nil)
    #(let [d (:diagnostic (observe-execute :fake))]
       (is (= :unrecognized-public-transaction-identity (:status d)))
       (is (nil? (:reported-tx-id d)))
       (assert-no-authority d))))

(deftest invalid-execute-key-id-does-not-query-ledger-test
  (let [seen (atom 0)]
    (with-adapter '*execute-tx* (fn [_ _ _] (tx-key -1))
      #(with-ledger (fn [& _] (swap! seen inc) (checks -1 at))
         (fn []
           (let [d (:diagnostic (observe-execute :fake))]
             (is (= :unrecognized-public-transaction-identity (:status d)))
             (is (nil? (:reported-tx-id d)))
             (is (zero? @seen))
             (assert-no-authority d)))))))

(deftest concrete-execute-ledger-readback-test
  (with-open [node (xtn/start-node {})]
    (let [id (str (random-uuid))
          r (c/observe-one-submission! node
              #(live/execute-tx! node
                 [[:put-docs :v1393_submission_correlation
                   {:xt/id id :kind "real-commit"}]]))
          d (:diagnostic r)]
      (is (= :returned (get-in r [:execution :outcome])))
      (is (= :reported-execute-ledger-agrees (:status d)) (pr-str d))
      (is (= {:tx-id true :committed true :system-time true}
             (get-in d [:ledger-observation :checks])))
      (assert-no-authority d))))

(deftest returned-public-key-and-matching-ledger-can-still-be-impersonated-test
  ;; A substituted adapter API function can return the key of an older real
  ;; transaction without submitting THIS payload. Ledger agreement is therefore
  ;; only diagnostic; it never certifies the payload or actual writer identity.
  (with-open [node (xtn/start-node {})]
    (let [seed (xt/execute-tx node
                 [[:put-docs :v1393_submission_correlation
                   {:xt/id (str (random-uuid)) :kind "seed"}]])
          calls (atom 0)
          r (with-adapter '*execute-tx* (fn [_ _ _] (swap! calls inc) seed)
              (fn [] (observe-execute node)))
          d (:diagnostic r)]
      (is (= 1 @calls))
      (is (= :returned (get-in r [:execution :outcome])))
      (is (= :reported-execute-ledger-agrees (:status d)) (pr-str d))
      (is (= (.getTxId ^TransactionKey seed) (:reported-tx-id d)))
      (assert-no-authority d))))

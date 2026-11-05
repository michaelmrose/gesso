(ns gesso.live.consistency.xtdb-submission-boundary-test
  "Permanent adversarial regression coverage for the v1390 process-local XTDB
   adapter submission capture. This tests diagnostic fidelity and explicit
   limitations; it neither authenticates XTDB nor certifies physical writes."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.consistency.xtdb :as live]
   [xtdb.api :as xt]
   [xtdb.node :as xtn])
  (:import
   [java.time Instant]
   [xtdb.api TransactionKey]))

(def ^:private source-ops
  [[:put-docs :v1391_capture {:xt/id "id-1" :name "first"}]])

(defn- seam [symbol]
  (or (ns-resolve 'gesso.live.consistency.xtdb symbol)
      (throw (ex-info "XTDB adapter seam is missing" {:symbol symbol}))))

(defn- with-seams [bindings f]
  (with-redefs-fn (into {} (map (fn [[k v]] [(seam k) v]) bindings)) f))

(defn- capture [f]
  (live/capture-tx-api-calls! f))

(defn- pair [report]
  (let [[started finished :as events] (:events report)]
    (is (= 2 (count events)) "One API call must yield two ordered events")
    (when (= 2 (count events))
      (is (= :invoked (:phase started)))
      (is (#{:returned :threw} (:phase finished)))
      (is (uuid? (:call-id started)))
      (is (= (:call-id started) (:call-id finished)))
      [started finished])))

(defn- assert-diagnostic-only [report]
  (is (= live/submission-capture-type (:type report)))
  (is (= live/submission-capture-version (:version report)))
  (is (= :instrumented-xtdb-adapter-call-site
         (:observation-origin report)))
  (is (false? (:actual-xtdb-api-authenticated? report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-established (:transaction-internal-prestate report)))
  (is (= :submission-diagnostic-only (:proof-status report)))
  (is (= :not-issued (:certificate report)))
  (is (= :none (:authority report))))

(defn- transaction-key [id]
  (let [when-at (Instant/parse "2026-02-14T10:11:12Z")]
    (reify TransactionKey
      (getTxId [_] id)
      (getSystemTime [_] when-at))))

(deftest requires-function-before-capture-test
  (doseq [value [nil {} [] :not-a-function 42]]
    (let [error (try (capture value) nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (= :invalid-thunk (:reason (ex-data error))))
      (is (true? (:submission-capture/rejection (ex-data error)))))))

(deftest empty-capture-does-not-invent-api-events-test
  (let [report (capture (fn [] :no-write))]
    (assert-diagnostic-only report)
    (is (= :returned (:outcome report)))
    (is (= :no-write (:result report)))
    (is (= [] (:events report)))
    (is (not (contains? report :error)))))

(deftest submit-captures-exact-operations-and-filtered-options-test
  (let [observed (atom nil)
        options {:database :xtdb :metadata {:request-id "123"}
                 :snapshot-token "read-only" :await-token "read-only"
                 :debug-fn (fn [_] nil)}]
    (with-seams
      {'*submit-tx* (fn [db operations passed-options]
                      (reset! observed [db operations passed-options])
                      {:tx-id 11})}
      (fn []
        (let [report (capture #(live/submit-tx! :connection source-ops options))
              [started finished] (pair report)]
          (assert-diagnostic-only report)
          (is (= :returned (:outcome report)))
          (is (= :submit-tx (:api started) (:api finished)))
          (is (= source-ops (:tx-ops started)))
          (is (= {:database :xtdb :metadata {:request-id "123"}}
                 (:tx-options started)))
          (is (= [:connection (:tx-ops started) (:tx-options started)] @observed))
          (is (= {:tx-id 11} (:tx-result finished)))
          (is (= {:tx-id 11} (get-in report [:result :tx-result])))
          (is (= {:tx-id 11} (get-in report [:result :consistency]))))))))

(deftest execute-records-original-transaction-key-test
  (let [tx-key (transaction-key 22)]
    (with-seams
      {'*execute-tx* (fn [_ _ _] tx-key)}
      (fn []
        (let [report (capture #(live/execute-tx! :connection source-ops))
              [started finished] (pair report)]
          (is (= :returned (:outcome report)))
          (is (= :execute-tx (:api started) (:api finished)))
          (is (= source-ops (:tx-ops started)))
          (is (= {} (:tx-options started)))
          (is (identical? tx-key (:tx-result finished)))
          (is (identical? tx-key (get-in report [:result :tx-result])))
          (is (= 22 (get-in report [:result :consistency :tx-id])))
          (is (= (Instant/parse "2026-02-14T10:11:12Z")
                 (get-in report [:result :consistency :snapshot-time]))))))))

(deftest context-wrappers-preserve-actual-connectable-test
  (let [calls (atom [])]
    (with-seams
      {'*submit-tx* (fn [db ops opts]
                      (swap! calls conj [:submit db ops opts])
                      {:tx-id 12})
       '*execute-tx* (fn [db ops opts]
                       (swap! calls conj [:execute db ops opts])
                       (transaction-key 13))}
      (fn []
        (let [ctx {:xtdb/connectable :preferred :xtdb/conn :ignored}
              report (capture (fn []
                                (live/submit-tx-from! ctx source-ops)
                                (live/execute-tx-from! ctx source-ops)))]
          (is (= :returned (:outcome report)))
          (is (= [[:submit :preferred source-ops {}]
                  [:execute :preferred source-ops {}]] @calls))
          (is (= [:submit-tx :submit-tx :execute-tx :execute-tx]
                 (mapv :api (:events report))))
          (is (= [:invoked :returned :invoked :returned]
                 (mapv :phase (:events report)))))))))

(deftest repeated-submissions-are-not-collapsed-test
  (with-seams
    {'*submit-tx* (let [next-id (atom 0)]
                    (fn [_ _ _] {:tx-id (swap! next-id inc)}))}
    (fn []
      (let [report (capture (fn []
                              (live/submit-tx! :connection source-ops)
                              (live/submit-tx! :connection source-ops)
                              (live/submit-tx! :connection source-ops)))
            events (:events report)
            started (filterv #(= :invoked (:phase %)) events)
            finished (filterv #(= :returned (:phase %)) events)]
        (is (= 6 (count events)))
        (is (= [:invoked :returned :invoked :returned :invoked :returned]
               (mapv :phase events)))
        (is (= 3 (count (set (map :call-id started)))))
        (is (every? uuid? (map :call-id started)))
        (is (= (mapv :call-id started) (mapv :call-id finished)))
        (is (= [{:tx-id 1} {:tx-id 2} {:tx-id 3}]
               (mapv :tx-result finished)))))))

(deftest both-apis-have-distinct-call-ids-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 2})
     '*execute-tx* (fn [_ _ _] (transaction-key 3))}
    (fn []
      (let [report (capture (fn []
                              (live/execute-tx! :connection source-ops)
                              (live/submit-tx! :connection source-ops)))
            events (:events report)]
        (is (= 4 (count events)))
        (is (= [:execute-tx :execute-tx :submit-tx :submit-tx]
               (mapv :api events)))
        (is (= (get-in events [0 :call-id]) (get-in events [1 :call-id])))
        (is (= (get-in events [2 :call-id]) (get-in events [3 :call-id])))
        (is (not= (get-in events [0 :call-id]) (get-in events [2 :call-id])))))))

(deftest submit-exception-remains-transaction-exception-test
  (let [failure (ex-info "upstream rejected submission" {:private-value "secret"})]
    (with-seams
      {'*submit-tx* (fn [_ _ _] (throw failure))}
      (fn []
        (let [report (capture #(live/submit-tx! :connection source-ops))
              [started finished] (pair report)]
          (is (= :threw (:outcome report)))
          (is (identical? failure (:error report)))
          (is (= :submit-tx (:api started) (:api finished)))
          (is (= :threw (:phase finished)))
          (is (= "clojure.lang.ExceptionInfo" (:exception-class finished)))
          (is (not (contains? finished :tx-result)))
          (is (not (contains? finished :private-value)))
          (is (not (contains? finished :exception-message)))
          (assert-diagnostic-only report))))))

(deftest execute-exception-remains-transaction-exception-test
  (let [failure (IllegalStateException. "execute failed")]
    (with-seams
      {'*execute-tx* (fn [_ _ _] (throw failure))}
      (fn []
        (let [report (capture #(live/execute-tx! :connection source-ops))
              [_ finished] (pair report)]
          (is (= :threw (:outcome report)))
          (is (identical? failure (:error report)))
          (is (= :threw (:phase finished)))
          (is (= "java.lang.IllegalStateException" (:exception-class finished)))
          (is (not (contains? finished :tx-result)))
          (assert-diagnostic-only report))))))

(deftest failure-followed-by-success-records-both-outcomes-test
  (let [calls (atom 0)]
    (with-seams
      {'*submit-tx* (fn [_ _ _]
                      (if (= 1 (swap! calls inc))
                        (throw (ex-info "rejected" {:kind :reject}))
                        {:tx-id 100}))}
      (fn []
        (let [report (capture (fn []
                                (try (live/submit-tx! :connection source-ops)
                                     (catch clojure.lang.ExceptionInfo _ :rejected))
                                (live/submit-tx! :connection source-ops)))
              events (:events report)]
          (is (= :returned (:outcome report)))
          (is (= 4 (count events)))
          (is (= [:invoked :threw :invoked :returned]
                 (mapv :phase events)))
          (is (not= (:call-id (first events)) (:call-id (nth events 2))))
          (is (= {:tx-id 100} (:tx-result (last events)))))))))

(deftest thrown-thunk-without-write-has-no-events-test
  (let [failure (ex-info "before submit" {:stage :preflight})
        report (capture #(throw failure))]
    (is (= :threw (:outcome report)))
    (is (identical? failure (:error report)))
    (is (= [] (:events report)))
    (assert-diagnostic-only report)))

(deftest invalid-connectable-does-not-invent-call-test
  (let [report (capture #(live/submit-tx! nil source-ops))]
    (is (= :threw (:outcome report)))
    (is (= [] (:events report)))
    (is (instance? clojure.lang.ExceptionInfo (:error report)))))

(deftest nested-captures-reject-before-instrumentation-test
  (let [report (capture #(capture (fn [] :inner)))]
    (is (= :threw (:outcome report)))
    (is (= [] (:events report)))
    (is (instance? clojure.lang.ExceptionInfo (:error report)))
    (is (= :nested-capture (:reason (ex-data (:error report)))))))

(deftest caught-inner-capture-error-does-not-taint-outer-session-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 31})}
    (fn []
      (let [report (capture (fn []
                              (try (capture (fn [] :inner))
                                   (catch clojure.lang.ExceptionInfo _ :caught))
                              (live/submit-tx! :connection source-ops)))
            [_ finished] (pair report)]
        (is (= :returned (:outcome report)))
        (is (= {:tx-id 31} (:tx-result finished)))))))

(deftest plain-submission-is-not-added-to-prior-capture-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 42})}
    (fn []
      (let [report (capture #(live/submit-tx! :connection source-ops))]
        (pair report)
        (is (= {:tx-id 42}
               (:tx-result (live/submit-tx! :connection source-ops))))
        (is (= 2 (count (:events report))))
        (is (= (get-in report [:events 0 :call-id])
               (get-in report [:events 1 :call-id])))))))

(deftest expired-binding-does-not-append-after-return-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 9})}
    (fn []
      ;; bound-fn deliberately carries the former dynamic binding. Session
      ;; shutdown, not merely unbinding on this thread, prevents later writes.
      (let [report (capture #(bound-fn []
                               (live/submit-tx! :connection source-ops)))
            delayed (:result report)]
        (is (fn? delayed))
        (is (= [] (:events report)))
        (is (= {:tx-id 9}
               (:tx-result (delayed))))
        (is (= [] (:events report)))))))

(deftest captured-ops-are-those-actually-submitted-test
  (let [planned (atom source-ops)
        replacement [[:put-docs :v1391_capture {:xt/id "id-1" :name "changed"}]
                     [:delete-docs :v1391_capture "id-2"]]
        calls (atom [])]
    (with-seams
      {'*submit-tx* (fn [db ops opts]
                      (swap! calls conj [db ops opts])
                      {:tx-id 55})}
      (fn []
        (let [report (capture (fn []
                                (reset! planned replacement)
                                (live/submit-tx! :connection @planned)))
              [started _] (pair report)]
          (is (= replacement (:tx-ops started)))
          (is (= [:connection replacement {}] (first @calls)))
          (is (not= source-ops (:tx-ops started)))
          (is (= [:put-docs :delete-docs]
                 (mapv first (:tx-ops started)))))))))

(deftest caller-reported-operation-list-is-not-an-input-to-capture-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 56})}
    (fn []
      (let [fabrication {:events [{:api :submit-tx :tx-ops :fake}]}
            report (capture (fn []
                              (let [_fabrication fabrication]
                                (live/submit-tx! :connection source-ops))))
            [started _] (pair report)]
        (is (= source-ops (:tx-ops started)))
        (is (not= (:events fabrication) (:events report)))))))

(deftest substituted-api-seam-is-visible-but-not-authenticated-test
  ;; with-redefs deliberately substitutes the API function: the adapter can
  ;; observe *what it called*, but cannot establish that XTDB executed it.
  (with-seams
    {'*execute-tx* (fn [_ _ _] (transaction-key 1000000))}
    (fn []
      (let [report (capture #(live/execute-tx! :connection source-ops))
            [started finished] (pair report)]
        (is (= :returned (:outcome report)))
        (is (= :execute-tx (:api started)))
        (is (instance? TransactionKey (:tx-result finished)))
        (is (false? (:actual-xtdb-api-authenticated? report)))
        (is (= :not-established (:complete-writer-coverage report)))
        (is (= :not-issued (:certificate report)))))))

(deftest report-data-is-not-tamper-evident-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 64})}
    (fn []
      (let [report (capture #(live/submit-tx! :connection source-ops))
            forged (assoc-in report [:events 0 :tx-ops] :fabricated)]
        (is (= :returned (:outcome forged)))
        (is (= :fabricated (get-in forged [:events 0 :tx-ops])))
        (is (not= (:events report) (:events forged)))
        (is (= :none (:authority forged)))
        (is (= :not-issued (:certificate forged)))))))

(deftest debug-hook-exception-after-api-return-does-not-relabel-api-event-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] {:tx-id 88})}
    (fn []
      (let [count-debug (atom 0)
            report (capture #(live/submit-tx! :connection source-ops
                                              {:debug-fn (fn [_]
                                                           (when (= 2 (swap! count-debug inc))
                                                             (throw (ex-info "debug post-return" {}))))}))
            [_ finished] (pair report)]
        (is (= 3 @count-debug))
        ;; Started, successful return, and failed debug handling. The
        ;; post-return hook exception does not rewrite the XTDB API event.
        (is (= :threw (:outcome report)))
        (is (= :returned (:phase finished)))
        (is (= {:tx-id 88} (:tx-result finished)))
        (is (= :submission-diagnostic-only (:proof-status report)))))))

(deftest debug-hook-before-api-invocation-does-not-invent-event-test
  (with-seams
    {'*submit-tx* (fn [_ _ _] (throw (AssertionError. "should not run")))}
    (fn []
      (let [report (capture #(live/submit-tx! :connection source-ops
                                              {:debug-fn (fn [_] (throw (ex-info "debug before" {}))) }))]
        (is (= :threw (:outcome report)))
        (is (= [] (:events report)))
        (is (= "debug before" (.getMessage ^Throwable (:error report))))))))

(deftest real-embedded-xtdb-submission-remains-diagnostic-test
  (with-open [node (xtn/start-node {})]
    (let [id (str "v1391-" (random-uuid))
          ops [[:put-docs :v1391_capture {:xt/id id :name "committed"}]]
          report (capture #(live/execute-tx! node ops))
          [started finished] (pair report)
          result (:result report)
          rows (xt/q node ["SELECT _id, name FROM v1391_capture WHERE _id = ?" id])]
      (assert-diagnostic-only report)
      (is (= :returned (:outcome report)))
      (is (= :execute-tx (:api started)))
      (is (= ops (:tx-ops started)))
      (is (= {} (:tx-options started)))
      (is (instance? TransactionKey (:tx-result finished)))
      (is (identical? (:tx-result finished) (:tx-result result)))
      (is (= [{:xt/id id :name "committed"}] rows))
      (is (= :not-issued (:certificate report))))))

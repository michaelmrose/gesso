(ns gesso.assurance.proof.installed-route-physical-correlation-test
  "v1481: Adversarial regression coverage for v1480's *one-shot*, in-process
   installed-route/physical-adapter diagnostic.

   The positive fixture uses an actual closed application assembly, issued
   certified-plan executor and handler, Biff's real module startup, and the
   returned adopted Ring handler. A test-only tx/transact! shim calls the real instrumented XTDB adapter;
   its bound XTDB API records arguments and returns an artificial TransactionKey. No test claims real XTDB
   persistence, genuine receipt authentication or deployed server adoption.

   Negative cases exercise request/source/handler substitution, forged and
   changed witnesses, extra observations, altered adapter ops
   and options, submit-only or uncertain calls, exceptions and recursion.
   Checks before dispatch must not execute the handler; witness rechecking
   must never execute it a second time."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.installed-route-physical-correlation :as correlation]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.model.certified-plan :as plan]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]
            [gesso.model.certified-plan-test])
  (:import (java.time Instant)
           (xtdb.api TransactionKey)))

(def ^:private operation :example/claim)
(def ^:dynamic *case* nil)
(def ^:dynamic *biff-test-modules* nil)

(defn- plan-fixture [symbol]
  (let [v (ns-resolve 'gesso.model.certified-plan-test symbol)]
    (when-not v
      (throw (ex-info "Missing existing certified-plan test fixture."
                      {:fixture symbol})))
    (var-get v)))

(defn- request
  ([] (request (random-uuid)))
  ([id] {:request-method :post
         :uri "/operations/example/claim"
         :request-id id}))

(defn- failure [thunk]
  (try (thunk) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (or (:installed-route-physical-correlation/rejection (ex-data e))
                 (:checked-biff-host-adoption/rejection (ex-data e)))
           (:reason (ex-data e))
           [:unexpected-ex-info (select-keys (ex-data e) [:reason :error/type])]))
       (catch Throwable e
         [:unexpected-throwable (.getName (class e))])))

(defn- rejects! [reason thunk]
  (is (= reason (failure thunk))
      (str "Expected installed physical correlation rejection: " reason)))

(defn- fake-key
  ([] (fake-key 71))
  ([tx-id]
   (reify TransactionKey
     (getTxId [_] tx-id)
     (getSystemTime [_] (Instant/parse "2026-11-06T10:00:00Z")))))

(use-fixtures
  :once
  (fn [run]
    (let [plan-source (:source ((plan-fixture 'environment)))
          executor (plan/build-certified-plan-operation! plan-source)
          calls (atom [])
          mode (atom :submit)
          on-input (atom nil)
          handler (certified/issue-certified-handler!
                   {:executor executor
                    :current-source plan-source
                    :request->input!
                    (fn [req]
                      (swap! calls conj :input)
                      (when-let [callback @on-input] (callback req))
                      ((plan-fixture 'invocation) @mode))
                    :result->response!
                    (fn [_ result]
                      (swap! calls conj :response)
                      {:status 200 :body result})})]
      (fixture/with-closed-application
        {:operations
         {operation (fixture/trusted-operation
                     operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [source {:application-assembly assembly
                        :semantic-route-handlers {operation handler}
                        :certified-operations
                        {operation {:executor executor
                                    :current-source plan-source}}}
                issued (host/issue-checked-biff-host-module! source)
                modules [application/application-handler-module (:module issued)]
                system (binding [*biff-test-modules* modules]
                         (application/start-biff-application!
                          assembly {} #'*biff-test-modules*
                          [application/application-handler-module-id host/module-id]))]
            (binding [*case* {:source source
                              :system system
                              :host-handle (:handle issued)
                              :calls calls :mode mode :on-input on-input}]
              (run))))))))

(use-fixtures
  :each
  (fn [run]
    (reset! (:calls *case*) [])
    (reset! (:mode *case*) :submit)
    (reset! (:on-input *case*) nil)
    (try (run)
         (finally (reset! (:on-input *case*) nil)))))

(defn- options
  ([] (options (request)))
  ([req] {:host-handle (:host-handle *case*)
          :contract-source (:source *case*)
          :system (:system *case*)
          :request req}))

(defn- artificial-execution
  "Exercise the real Gesso adapter call-site, without real database I/O.
   Settings modify only the test-controlled physical executor. All observed
   events must still be emitted by v1480 and the existing Gesso functions."
  ([settings thunk]
   (let [api-calls (atom [])
         transactions (atom 0)
         {:keys [change-ops tx-options tx-mode result status extra-call?]
          :or {change-ops identity tx-mode :execute-tx
               result (fake-key) status :committed}} settings]
     (with-redefs-fn
       {#'tx/transact!
        (fn [ctx p]
          (swap! transactions inc)
          (let [ops (change-ops (:tx-ops (tx/prepare ctx p)))
                invoke (fn []
                         (case tx-mode
                           :execute-tx (adapter/execute-tx! :test-node ops tx-options)
                           :submit-tx (adapter/submit-tx! :test-node ops tx-options)
                           :skip nil))]
            (when extra-call? (invoke))
            (invoke)
            {:commit/status status}))
        (ns-resolve 'gesso.live.consistency.xtdb '*execute-tx*)
        (fn [connectable ops opts]
          (swap! api-calls conj [:execute-tx connectable ops opts])
          (if (instance? Throwable result) (throw result) result))
        (ns-resolve 'gesso.live.consistency.xtdb '*submit-tx*)
        (fn [connectable ops opts]
          (swap! api-calls conj [:submit-tx connectable ops opts])
          (if (instance? Throwable result) (throw result)
              {:tx-id 71}))}
       (fn [] (thunk {:api-calls api-calls :transactions transactions}))))))

(defn- observe [opts]
  (correlation/observe-installed-route-physical! opts))

(defn- positive! [opts]
  (let [{:keys [execution report handle]} (observe opts)]
    (is (= :returned (:outcome execution)))
    (is (= :bounded-installed-route-to-adapter-agreement (:status report)))
    (is (correlation/installed-physical-witness? handle))
    (is (= 1 (:handler-invocation-count report)))
    (is (true? (:actual-issued-executor-attempt-observed? report)))
    (is (true? (:one-adapter-call-observed? report)))
    (is (true? (:prepared-operations-agree? report)))
    (is (true? (:prepared-options-agree? report)))
    (is (true? (:definitive-execute-key-observed? report)))
    (is (= :bounded-conditional-process-local-observation-only
           (:proof-status report)))
    (is (= :not-established (:actual-server-deployment report)))
    (is (false? (:xtdb-receipt-authenticated? report)))
    (is (= :not-established (:complete-writer-coverage report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))
    (is (contains? (:open-obligations report)
                   :atomic-transaction-internal-prestate-and-guards))
    {:execution execution :report report :handle handle}))

(deftest one-real-adopted-handler-and-one-adapter-call-can-correlate
  (let [opts (options)]
    (artificial-execution {}
      (fn [{:keys [api-calls transactions]}]
        (let [{:keys [handle report]} (positive! opts)]
          (is (= [:input :response] @(:calls *case*)))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls)))
          (is (= :execute-tx (ffirst @api-calls)))
          (is (= report
                 (correlation/require-current-installed-physical!
                  handle opts report)))
          (is (= 1 @transactions) "Witness recheck must never resubmit"))))))

(deftest later-observations-are-distinct-and-rechecks-are-side-effect-free
  (let [opts (options)]
    (artificial-execution {}
      (fn [{:keys [transactions api-calls]}]
        (let [one (positive! opts)
              two (positive! opts)]
          (is (= 2 @transactions) "Two explicit diagnostics, not hidden retries")
          (is (= 2 (count @api-calls)))
          (is (not (identical? (:handle one) (:handle two))))
          (is (= (:report one)
                 (correlation/require-current-installed-physical!
                  (:handle one) opts (:report one))))
          (is (= (:report two)
                 (correlation/require-current-installed-physical!
                  (:handle two) opts (:report two))))
          (is (= 2 @transactions)))))))

(deftest counterfeit-witnesses-and-copied-reports-have-no-authority
  (let [opts (options)]
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [handle report]} (positive! opts)
              previous @transactions]
          (is (not (correlation/installed-physical-witness? (Object.))))
          (is (not (correlation/installed-physical-witness?
                    (correlation/->InstalledPhysicalHandle))))
          (rejects! :unknown-or-forged-witness
                    #(correlation/require-current-installed-physical!
                      (correlation/->InstalledPhysicalHandle) opts report))
          (rejects! :unknown-or-forged-witness
                    #(correlation/require-current-installed-physical!
                      report opts report))
          (is (= report (correlation/require-current-installed-physical!
                         handle opts (into {} report))))
          (doseq [changed [(assoc report :status :claimed-proved)
                           (assoc report :authority :certified)
                           (assoc report :certificate :issued)
                           (dissoc report :open-obligations)]]
            (rejects! :stale-or-altered-observation
                      #(correlation/require-current-installed-physical!
                        handle opts changed)))
          (is (= previous @transactions)))))))

(deftest changed-request-object-and-changed-source-cannot-reuse-witness
  (let [opts (options)]
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [handle report]} (positive! opts)
              previous @transactions]
          (rejects! :changed-observation-source
                    #(correlation/require-current-installed-physical!
                      handle (assoc opts :request (into {} (:request opts))) report))
          (rejects! :changed-observation-source
                    #(correlation/require-current-installed-physical!
                      handle (assoc opts :contract-source
                                    (into {} (:contract-source opts))) report))
          (rejects! :changed-observation-source
                    #(correlation/require-current-installed-physical!
                      handle (assoc opts :host-handle (host/->HostAdoptionHandle))
                      report))
          (is (= previous @transactions)))))))

(deftest substituted-final-handler-cannot-authenticate-existing-witness
  (let [opts (options)]
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [handle report]} (positive! opts)
              changed (assoc-in opts [:system application/biff-ring-handler-key]
                                (fn [_] {:status 200}))
              previous @transactions]
          (rejects! :host-handler-slot-mismatch
                    #(correlation/require-current-installed-physical!
                      handle changed report))
          (rejects! :host-handler-slot-mismatch #(observe changed))
          (is (= previous @transactions)))))))

(deftest invalid-observation-options-are-rejected-before-dispatch
  (let [opts (options)]
    (doseq [bad [nil [] {} (assoc opts :extra true)
                 (dissoc opts :request)
                 (with-meta opts {:certified true})]]
      (rejects! :invalid-observation-options #(observe bad)))
    (is (empty? @(:calls *case*)))))

(deftest malformed-unmatched-or-preobserved-requests-are-not-executed
  (let [opts (options)]
    (doseq [bad [nil {} []
                 (assoc (request) :request-method "POST")
                 (assoc (request) :uri nil)
                 (with-meta (request) {:certified true})
                 (assoc (request) resolved/observation-context-key (fn [_] nil))
                 (assoc (request) resolved/construction-input-context-key
                        (fn [_] nil))]]
      (rejects! :invalid-request-or-preexisting-observer
                #(observe (assoc opts :request bad))))
    (doseq [bad [(assoc (request) :uri "/other")
                 (assoc (request) :uri "/operations/example/claim/")
                 (assoc (request) :request-method :get)]]
      (rejects! :unmatched-or-ambiguous-route
                #(observe (assoc opts :request bad))))
    (is (empty? @(:calls *case*)))))

(deftest forged-host-adoption-and-copied-source-block-before-handler
  (let [opts (options)]
    (rejects! :unissued-host-adoption
              #(observe (assoc opts :host-handle (host/->HostAdoptionHandle))))
    (rejects! :changed-host-adoption-source
              #(observe (assoc opts :contract-source
                               (into {} (:contract-source opts)))))
    (is (empty? @(:calls *case*)))))

(deftest predispatch-pinned-boundaries-reject-substitution
  (let [opts (options)]
    (with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
      (rejects! :substituted-correlation-boundary #(observe opts)))
    (with-redefs [adapter/capture-tx-api-calls!
                  (fn [_] {:outcome :returned :events []})]
      (rejects! :substituted-correlation-boundary #(observe opts)))
    (is (empty? @(:calls *case*)))))

(deftest changed-adapter-write-operations-cannot-issue-correlation
  (let [opts (options)]
    (artificial-execution {:change-ops #(conj % [:rogue :unaccounted])}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :prepared-plan-diverges-from-adapter (:status report)))
          (is (false? (:prepared-operations-agree? report)))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest changed-adapter-transaction-options-cannot-issue-correlation
  (let [opts (options)]
    (artificial-execution {:tx-options {:metadata {:unexpected :value}}}
      (fn [{:keys [transactions]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :prepared-options-diverge-from-adapter (:status report)))
          (is (false? (:prepared-options-agree? report)))
          (is (nil? handle))
          (is (= 1 @transactions)))))))

(deftest additional-adapter-call-is-not-treated-as-one-call
  (let [opts (options)]
    (artificial-execution {:extra-call? true}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :missing-or-invalid-adapter-call (:status report)))
          (is (false? (:one-adapter-call-observed? report)))
          (is (nil? handle))
          (is (= 1 @transactions) "Handler called once")
          (is (= 2 (count @api-calls)) "Underlying rogue duplicate observed"))))))

(deftest submit-only-is-not-a-definitive-execution-receipt
  (let [opts (options)]
    (artificial-execution {:tx-mode :submit-tx}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :submit-only-not-definitive (:status report)))
          (is (false? (:definitive-execute-key-observed? report)))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= :submit-tx (ffirst @api-calls))))))))

(deftest malformed-execute-result-cannot-be-treated-as-committed
  (let [opts (options)]
    (doseq [bad [nil {:tx-id 71} (fake-key -1)]]
      (artificial-execution {:result bad}
        (fn [{:keys [transactions]}]
          (let [{:keys [report handle]} (observe opts)]
            (is (= :unconfirmed-execute-tx (:status report)) (pr-str report))
            (is (nil? handle))
            (is (= 1 @transactions))))))))

(deftest exception-from-adapter-is-not-positive-evidence
  (let [opts (options)]
    (artificial-execution {:result (ex-info "test-only simulated XTDB failure" {})}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :unconfirmed-execute-tx (:status report)) (pr-str report))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest unrecognized-transaction-outcome-cannot-be-promoted
  (let [opts (options)]
    (artificial-execution {:status :rejected}
      (fn [{:keys [transactions]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :unconfirmed-or-uncertain-outcome (:status report))
              (pr-str report))
          (is (nil? handle))
          (is (= 1 @transactions)))))))

(deftest no-submission-does-not-produce-a-physical-witness
  (reset! (:mode *case*) :reconcile)
  (let [opts (options)
        {:keys [report handle execution]} (observe opts)]
    (is (= :returned (:outcome execution)))
    (is (= :missing-or-invalid-executor-attempt (:status report)))
    (is (nil? handle))
    (is (= [:input :response] @(:calls *case*)))))

(deftest injected-extra-executor-event-invalidates-correlation
  (let [opts (options)]
    (reset! (:on-input *case*)
            (fn [req]
              ((get req resolved/observation-context-key)
               {:phase :unsolicited-extra :operation operation})))
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :missing-or-invalid-executor-attempt (:status report)))
          (is (nil? handle))
          (is (= 1 @transactions)))))))

(deftest nested-observation-is-rejected-without-inner-execution
  (let [opts (options)
        nested (atom nil)]
    (reset! (:on-input *case*)
            (fn [_]
              (reset! nested (failure #(observe opts)))))
    (artificial-execution {}
      (fn [{:keys [transactions api-calls]}]
        (positive! opts)
        (is (= :nested-route-physical-observation @nested))
        (is (= 1 @transactions))
        (is (= 1 (count @api-calls)))))))

(deftest handler-failure-produces-no-physical-correlation
  (let [opts (options)]
    (reset! (:on-input *case*)
            (fn [_] (throw (ex-info "test-only pre-submit failure" {}))))
    (artificial-execution {}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle execution]} (observe opts)]
          (is (= :missing-or-invalid-executor-attempt (:status report)))
          (is (nil? handle))
          (is (= 0 @transactions))
          (is (empty? @api-calls))
          (is (contains? #{:returned :threw} (:outcome execution))))))))

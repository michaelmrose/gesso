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
            [gesso.live.application-preflight.certified-contract :as binding]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as adapter]
            [gesso.live.core :as live]
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
          response-status (atom 200)
          response-headers (atom {})
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
                      (cond-> {:status @response-status :body result}
                        (seq @response-headers)
                        (assoc :headers @response-headers)))
                    :response-status-contract {:status 200}})]
      (fixture/with-closed-application
        {:operations
         {operation (fixture/trusted-operation
                     operation {:published-change-topics #{}
                                :response-status-contract {:status 200}})}}
        (fn [{:keys [assembly]}]
          (let [source {:application-assembly assembly
                        :semantic-route-handlers {operation handler}
                        :certified-operations
                        {operation {:executor executor
                                    :current-source plan-source
                                    :response-status-contract {:status 200}}}}
                issued (host/issue-checked-biff-host-module! source)
                modules [application/application-handler-module (:module issued)]
                system (binding [*biff-test-modules* modules]
                         (application/start-biff-application!
                          assembly {} #'*biff-test-modules*
                          [application/application-handler-module-id host/module-id]))]
            (binding [*case* {:source source
                              :system system
                              :host-handle (:handle issued)
                              :calls calls :mode mode :on-input on-input
                              :response-status response-status
                              :response-headers response-headers}]
              (run))))))))

(use-fixtures
  :each
  (fn [run]
    (reset! (:calls *case*) [])
    (reset! (:mode *case*) :submit)
    (reset! (:on-input *case*) nil)
    (reset! (:response-status *case*) 200)
    (reset! (:response-headers *case*) {})
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
         {:keys [change-ops tx-options tx-mode result status extra-call?
                 post-submit-error receipt-mode consistency-mode]
          :or {change-ops identity tx-mode :execute-tx
               result (fake-key) status :committed receipt-mode :matching
               consistency-mode :matching}} settings]
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
            (let [api-response (invoke)]
              ;; The matching fixture retains the EXACT adapter result,
              ;; like the real Gesso Live -> tx/transact! return path. Two
              ;; fault-injection modes model a stripped or substituted
              ;; receipt without changing the adapter's own recorded call.
              ;; The adapter can have returned a definitive transaction key,
              ;; then the model transaction may throw AFTER commit when Live
              ;; delivery fails. The original resolved operation reconciles
              ;; rather than claiming the command was incorporated.
              (when post-submit-error (throw post-submit-error))
              (cond-> {:commit/status status}
                (not= :missing receipt-mode)
                (assoc :tx-result
                       (if (= :different receipt-mode)
                         (fake-key 999)
                         (:tx-result api-response)))
                (not= :missing consistency-mode)
                (assoc :consistency
                       (case consistency-mode
                         :matching (:consistency api-response)
                         :wrong-tx-id (assoc (:consistency api-response)
                                             :tx-id 9999)
                         :wrong-snapshot (assoc (:consistency api-response)
                                                :snapshot-time
                                                (Instant/parse "2020-01-01T00:00:00Z"))))))))
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
    (is (true? (:original-issued-response-returned-unchanged? report)))
    (is (= :matched (:issued-response-status-declaration report)))
    (is (= :incorporated (:issued-resolution-class report)))
    (is (true? (:actual-issued-executor-attempt-observed? report)))
    (is (true? (:one-adapter-call-observed? report)))
    (is (true? (:prepared-operations-agree? report)))
    (is (true? (:prepared-options-agree? report)))
    (is (true? (:definitive-execute-key-observed? report)))
    (is (true? (:model-commit-receipt-identical-to-adapter? report)))
    (is (true? (:model-consistency-agrees-with-xtdb-receipt? report)))
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

(deftest model-transaction-receipt-must-be-same-xtdb-api-result
  ;; A definitive execute-tx receipt observed at the adapter is NOT evidence
  ;; that the Gesso model's claimed committed result carries that same receipt.
  ;; Missing, or a different otherwise valid TransactionKey, is a distinct
  ;; correlation failure even when the original handler returns 200 and the
  ;; original resolved operation returned :incorporated.
  (doseq [receipt-mode [:missing :different]]
    (artificial-execution {:receipt-mode receipt-mode}
      (fn [{:keys [api-calls transactions]}]
        (let [{:keys [report handle execution]} (observe (options))]
          (is (= :returned (:outcome execution)))
          (is (= 200 (get-in execution [:result :status])))
          (is (= :incorporated (:issued-resolution-class report)))
          (is (true? (:definitive-execute-key-observed? report)))
          (is (= :model-receipt-not-identical-to-adapter
                 (:status report)))
          (is (false? (:model-commit-receipt-identical-to-adapter? report)))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest model-transaction-receipt-identity-preserves-positive-correlation
  (artificial-execution {:receipt-mode :matching}
    (fn [{:keys [api-calls transactions]}]
      (let [{:keys [report handle]} (observe (options))]
        (is (= :bounded-installed-route-to-adapter-agreement
               (:status report)))
        (is (true? (:model-commit-receipt-identical-to-adapter? report)))
        (is (correlation/installed-physical-witness? handle))
        (is (= 1 @transactions))
        (is (= 1 (count @api-calls)))))))

(deftest committed-model-consistency-must-match-actual-xtdb-receipt
  ;; A captured committed receipt may reach the model unchanged while its
  ;; read-your-writes consistency coordinates are omitted or substituted.
  ;; HTTP 200 and exact receipt identity alone must NOT qualify basis metadata.
  (doseq [consistency-mode [:missing :wrong-tx-id :wrong-snapshot]]
    (artificial-execution {:consistency-mode consistency-mode}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [report handle execution]} (observe (options))]
          (is (= :returned (:outcome execution)))
          (is (= 200 (get-in execution [:result :status])))
          (is (true? (:model-commit-receipt-identical-to-adapter? report)))
          (is (= :incorporated (:issued-resolution-class report)))
          (is (= :model-consistency-basis-diverges-from-receipt
                 (:status report)))
          (is (false? (:model-consistency-agrees-with-xtdb-receipt? report)))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest physically-committed-but-unresolved-operation-cannot-qualify-http-success
  ;; A post-commit Live failure leaves the physical write committed, but
  ;; after an authoritative read returns no command record, the application
  ;; sees :unresolved. A trusted response callback may nevertheless return
  ;; 200. Physical/HTTP correlation must not call that a resolved success.
  (let [error (ex-info "Post-commit delivery failed"
                       {:error/type live/post-commit-delivery-failure-type
                        :commit/status :committed
                        :failure/stage :post-commit-delivery})]
    (artificial-execution {:post-submit-error error}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [execution report handle]} (observe (options))]
          (is (= :returned (:outcome execution)))
          (is (= 200 (get-in execution [:result :status])))
          (is (= :unresolved (get-in execution [:result :body :status])))
          (is (true? (:definitive-execute-key-observed? report)))
          (is (= :physical-commit-with-unresolved-operation-result
                 (:status report)))
          (is (= :unresolved (:issued-resolution-class report)))
          (is (nil? handle))
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest substituted-response-after-original-callback-is-not-correlated
  ;; A second Ring middleware stage can replace an otherwise successful
  ;; callback result. The observed issued callback's exact response object
  ;; must reach the physical caller; an equivalent rebuilt map is not enough.
  (let [issued (get-in (:source *case*) [:semantic-route-handlers operation])
        req (request)]
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [observed
              (binding/observe-issued-response-correlation!
               issued req
               (fn []
                 (let [response (issued req)]
                   {:outcome :returned :result (into {} response)})))]
          (is (false? (:same-issued-response? observed)))
          (is (= 200 (get-in observed [:capture :result :status])))
          (is (= 1 @transactions)))))))

(deftest malformed-issued-response-declaration-is-rejected-before-issuance
  (let [{:keys [executor current-source]}
        (get-in (:source *case*) [:certified-operations operation])
        source {:executor executor
                :current-source current-source
                :request->input! (fn [_] nil)
                :result->response! (fn [_ _] {:status 200})}]
    (doseq [invalid [nil false {:status nil} {:status 503}
                     {:status 200 :extra true}]]
      (is (= :invalid-response-status-contract
             (try
               (certified/issue-certified-handler!
                (assoc source :response-status-contract invalid))
               :not-rejected
               (catch clojure.lang.ExceptionInfo e
                 (:reason (ex-data e)))))
          (str "Must reject malformed issuance policy: " (pr-str invalid))))))

(deftest declared-issued-response-status-must-match-the-actual-callback
  ;; A statically issued status declaration is separate from the opaque
  ;; result->response! callback. Even the exact original callback response
  ;; must not count as declaration-matched when it returns the wrong status.
  (let [{:keys [executor current-source]}
        (get-in (:source *case*) [:certified-operations operation])
        issued (certified/issue-certified-handler!
                {:executor executor
                 :current-source current-source
                 :request->input! (fn [_] ((plan-fixture 'invocation) :submit))
                 :result->response! (fn [_ result] {:status 201 :body result})
                 :response-status-contract {:status 200}})
        req (request)]
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [observation
              (binding/observe-issued-response-correlation!
               issued req
               (fn [] {:outcome :returned :result (issued req)}))]
          (is (true? (:same-issued-response? observation)))
          (is (= :mismatched (:issued-response-status-declaration observation)))
          (is (= 201 (get-in observation [:capture :result :status])))
          (is (= 1 @transactions)))))))

(deftest selected-issued-route-must-match-its-declared-response-status
  ;; This is the exact installed handler and same-invocation XTDB adapter,
  ;; not a second response simulation. The original response callback can
  ;; change its status while the physical write still occurs.
  (reset! (:response-status *case*) 201)
  (artificial-execution {}
    (fn [{:keys [transactions]}]
      (let [{:keys [report handle execution]} (observe (options))]
        (is (= :returned (:outcome execution)))
        (is (= 201 (get-in execution [:result :status])))
        (is (= :issued-response-status-declaration-mismatch (:status report)))
        (is (= :mismatched (:issued-response-status-declaration report)))
        (is (nil? handle))
        (is (= 1 @transactions))))))

(deftest committed-adapter-with-error-http-response-is-not-qualified
  ;; A definitive adapter observation is only one edge. The same installed
  ;; handler may map a committed operation to an HTTP error response. This
  ;; report must expose the disagreement rather than issue a positive witness
  ;; which a later consumer could mistake for request-to-response agreement.
  (let [opts (options)]
    (reset! (:response-status *case*) 503)
    (artificial-execution {}
      (fn [{:keys [transactions api-calls]}]
        (let [{:keys [execution handle report]} (observe opts)]
          (is (= :returned (:outcome execution)))
          (is (= 503 (get-in execution [:result :status])))
          (is (= :physical-commit-with-unqualified-ring-response
                 (:status report)))
          (is (= :http-error-response (:ring-response-class report)))
          (is (true? (:definitive-execute-key-observed? report)))
          (is (nil? handle) "Do not issue a positive cross-boundary witness")
          (is (= 1 @transactions))
          (is (= 1 (count @api-calls))))))))

(deftest malformed-ring-response-cannot-qualify-physical-observation
  (let [opts (options)]
    (reset! (:response-status *case*) nil)
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [report handle]} (observe opts)]
          (is (= :physical-commit-with-unqualified-ring-response
                 (:status report)))
          (is (= :invalid-ring-response (:ring-response-class report)))
          (is (nil? handle))
          (is (= 1 @transactions)))))))

(deftest non-error-ring-status-with-unqualified-htmx-effects-is-not-a-witness
  ;; A physical 200 can carry HTMX response-control instructions which mutate
  ;; browser state. Merely observing the commit and an HTTP success status
  ;; does NOT qualify that undeclared effect. Check headers case-insensitively.
  (doseq [headers [{"HX-Trigger" "claim-committed"}
                   {"hX-ReTaRgEt" "#other-panel"}
                   {:HX-Redirect "/other-page"}]]
    (reset! (:response-headers *case*) headers)
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [execution report handle]} (observe (options))]
          (is (= :returned (:outcome execution)))
          (is (= 200 (get-in execution [:result :status])))
          (is (= :physical-commit-with-unqualified-ring-response
                 (:status report)))
          (is (= :unqualified-browser-effect-response
                 (:ring-response-class report)))
          (is (true? (:definitive-execute-key-observed? report)))
          (is (nil? handle))
          (is (= :not-established
                 (:response-contract-correspondence report)))
          (is (= 1 @transactions)))))))

(deftest committed-adapter-with-http-redirect-is-not-a-witness
  ;; A redirect may be an application-valid response but is not a qualified
  ;; response to this operation until its navigation effect is declared.
  (reset! (:response-status *case*) 302)
  (reset! (:response-headers *case*) {"Location" "/changed-page"})
  (artificial-execution {}
    (fn [{:keys [transactions]}]
      (let [{:keys [execution report handle]} (observe (options))]
        (is (= 302 (get-in execution [:result :status])))
        (is (= :physical-commit-with-unqualified-ring-response
               (:status report)))
        (is (= :unqualified-redirect-response (:ring-response-class report)))
        (is (nil? handle))
        (is (= 1 @transactions))))))

(deftest malformed-ring-headers-cannot-qualify-physical-observation
  ;; Even an apparently successful HTTP status cannot authorize a malformed
  ;; header collection: the response must at least have Ring-shaped headers.
  (reset! (:response-headers *case*) [:HX-Trigger])
  (artificial-execution {}
    (fn [{:keys [transactions]}]
      (let [{:keys [report handle]} (observe (options))]
        (is (= :invalid-ring-response (:ring-response-class report)))
        (is (= :physical-commit-with-unqualified-ring-response
               (:status report)))
        (is (nil? handle))
        (is (= 1 @transactions))))))

(deftest inert-content-type-header-preserves-bounded-physical-correlation
  ;; An ordinary content-type header alone is not an HTMX control directive.
  ;; The physical witness still does not establish its response contract.
  (reset! (:response-headers *case*) {"cOnTeNt-TyPe" "text/html"})
  (artificial-execution {}
    (fn [{:keys [transactions]}]
      (let [{:keys [report handle]} (observe (options))]
        (is (= :bounded-installed-route-to-adapter-agreement (:status report)))
        (is (= :non-error-ring-response (:ring-response-class report)))
        (is (some? handle))
        (is (= :not-established (:response-contract-correspondence report)))
        (is (= 1 @transactions))))))

(deftest unclassified-response-headers-cannot-grant-physical-response-witness
  ;; v2077: HTMX's known control headers are not the entire surface.
  ;; Set-Cookie mutates browser state; Refresh navigates; Content-Disposition
  ;; can initiate downloads; Link may initiate browser preloads. A diagnostic
  ;; with no operation-owned response-effect declarations MUST fail closed
  ;; for unclassified headers, rather than treating every unknown name as inert.
  (doseq [headers [{"Set-Cookie" "session=changed; HttpOnly"}
                   {"rEfReSh" "0; url=/other"}
                   {"Content-Disposition" "attachment; filename=data.txt"}
                   {"Link" "</next>; rel=preload"}
                   {"X-Accel-Redirect" "/internal-private"}]]
    (reset! (:response-headers *case*) headers)
    (artificial-execution {}
      (fn [{:keys [transactions]}]
        (let [{:keys [execution report handle]} (observe (options))]
          (is (= 200 (get-in execution [:result :status])))
          (is (= :physical-commit-with-unqualified-ring-response
                 (:status report)))
          (is (= :unqualified-response-header-effect
                 (:ring-response-class report)))
          (is (nil? handle))
          (is (= :not-established (:response-contract-correspondence report)))
          (is (= 1 @transactions)))))))

(deftest ambiguous-case-colliding-ring-headers-do-not-qualify
  ;; A Ring map can contain two differently cased keys which become the same
  ;; HTTP field after normalization, with order and semantics not established.
  (reset! (:response-headers *case*)
          {"Content-Type" "text/html" "content-type" "application/json"})
  (artificial-execution {}
    (fn [{:keys [transactions]}]
      (let [{:keys [report handle]} (observe (options))]
        (is (= :invalid-ring-response (:ring-response-class report)))
        (is (= :physical-commit-with-unqualified-ring-response
               (:status report)))
        (is (nil? handle))
        (is (= 1 @transactions))))))

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

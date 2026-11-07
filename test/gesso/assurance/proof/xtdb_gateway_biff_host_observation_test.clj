(ns gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
  "v1554: permanent adversarial tests for an actual Biff-started, source-
   checked HTML-wrapped handler's invocation of an exact locally issued
   guarded XTDB writer. These are real closed-application and embedded-XTDB
   tests. Host-slot inspection and post-dispatch callback traces never prove
   deployed HTTP adoption, exhaustive writers, causal writes or refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.assurance.proof.installed-route-provenance-test :as ring-fixture]
            [gesso.assurance.proof.installed-route-provenance :as routes]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.live.application-preflight :as app]
            [gesso.live.application-preflight.certified-contract :as certified]
            [xtdb.api :as xt]))

(def ^:dynamic *biff-modules* [])
(def ^:private operation :example/claim)
(def ^:private writer-path :host/claim)
(def ^:private sentinel "private-v1549-sentinel")

;; Reuse genuine application-certificate and independent writer-inventory
;; fixtures. Neither the positive route nor any XTDB query is stubbed.
(use-fixtures :once
  (fn [run]
    (let [writer-fixture (first (:clojure.test/once-fixtures
                                (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-writer-coverage-test))))
          ring-once (first (:clojure.test/once-fixtures
                            (meta (the-ns 'gesso.assurance.proof.installed-route-provenance-test))))]
      (writer-fixture #(ring-once run)))))

(defn- certified-invocation []
  ((var-get (ns-resolve 'gesso.model.certified-plan-test 'invocation)) :reconcile))

(defn- rejection [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (:failure (ex-data e)) :foreign-exception))))

(defn- revision [ctx]
  (:revision (wc/physical (:before ctx))))

(defn- context
  "Construct a fresh genuine Biff lifecycle, certified handler, XTDB writer,
   and live host registry. Behavior runs only inside the real Biff handler."
  [behavior]
  (let [before (wc/seeded)
        issued (wc/issue before)
        writer (:writer issued)
        registry (atom {writer-path (wc/registered writer)})
        calls (atom 0)
        input! (fn [request]
                 (swap! calls inc)
                 (behavior {:before before :writer writer :registry registry
                            :request request})
                 (certified-invocation))
        handler (certified/issue-certified-handler!
                 {:executor (:executor ring-fixture/*case*)
                  :current-source (:plan-source ring-fixture/*case*)
                  :request->input! input!
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})
        source (assoc-in (:source ring-fixture/*case*)
                         [:semantic-route-handlers operation] handler)
        issued-host (adoption/issue-checked-biff-host-module! source)
        modules [app/application-handler-module (:module issued-host)]
        system (binding [*biff-modules* modules]
                 (app/start-biff-application!
                  (:application-assembly source) {} #'*biff-modules*
                  [app/application-handler-module-id adoption/module-id]))]
    {:before before :writer writer :writer-handle (:handle issued)
     :registry registry :calls calls :source source :system system
     :adoption (:handle issued-host)
     :request {:request-method :post :uri "/operations/example/claim"
               :request-id (random-uuid)}
     :inventory (wc/inv registry)}))

(defn- scenario [choice]
  (context
   (case choice
     :invoke (fn [{:keys [writer]}] (writer))
     :omit (fn [_] nil)
     :direct-only (fn [{:keys [before]}]
                    (xt/execute-tx wc/*node*
                                   [[:put-docs wc/table (assoc before :rogue true)]]))
     :direct-plus (fn [{:keys [before writer]}]
                    (xt/execute-tx wc/*node*
                                   [[:put-docs wc/table
                                     (assoc before :xt/id (str (random-uuid))
                                                   :rogue true)]])
                    (writer))
     :twice (fn [{:keys [writer]}]
              (writer) (try (writer) (catch Exception _ nil)))
     :async (fn [{:keys [writer]}] @(future (writer)))
     :swap-after (fn [{:keys [writer registry]}]
                   (writer)
                   (reset! registry
                           {writer-path (wc/registered (fn [] :replacement))}))
     :swap-before (fn [{:keys [writer registry]}]
                    (reset! registry
                            {writer-path (wc/registered (fn [] :replacement))})
                    (writer))
     :throw-before (fn [_]
                     (throw (ex-info "pre-write" {:failure :before-write})))
     :throw-after (fn [{:keys [writer]}]
                    (writer)
                    (throw (ex-info "post-write" {:failure :after-write})))
     (throw (ex-info "Unknown test behavior" {:choice choice})))))

(defn- observe
  ([ctx] (observe ctx (:request ctx)))
  ([ctx request]
   (coverage/observe-checked-biff-host-registered-writer!
    (:adoption ctx) (:source ctx) (:system ctx) request
    (:inventory ctx) {writer-path (:writer-handle ctx)} writer-path)))

(defn- require-nonclaims! [report]
  (is (= coverage/biff-observation-type (:type report)))
  (is (= coverage/biff-observation-version (:version report)))
  (is (= :observed-checked-biff-handler-invoked-exact-registered-writer
         (:status report)))
  (is (= :one-synchronous-in-process-biff-handler-dispatch (:scope report)))
  (is (= operation (:observed-operation-id report)
         (:observed-route-id report)))
  (is (= writer-path (:observed-writer-path-id report)))
  (is (= 1 (:observed-registered-callback-invocations report)))
  (is (true? (:registered-callback-returned? report)))
  (is (true? (:biff-handler-and-writer-registrations-rechecked? report)))
  (is (= :locally-observed (:biff-module-start report)))
  (is (= :exact-checked-html-wrapped-handler (:biff-handler-slot report)))
  (doseq [k [:host-deployment :host-writer-enumeration-exhaustiveness
             :other-xtdb-writer-bypasses :abstract-operation-correspondence
             :physical-commit-and-causality]]
    (is (= :not-established (get report k)) (str "Open premise " k)))
  (doseq [[k expected] [[:certificate :not-issued] [:admission :not-admitted]
                        [:transaction-authority :none] [:authority :none]]]
    (is (= expected (get report k))))
  (doseq [k [:server-consumes-the-returned-biff-handler-not-established
             :host-operation-id-matches-prepared-command-semantics
             :other-processes-and-external-xtdb-clients
             :unchecked-direct-and-asynchronous-writes-during-route-dispatch
             :physical-refinement-and-certification]]
    (is (contains? (:open-obligations report) k) (str "Open obligation " k)))
  (is (not-any? #(contains? report %)
                [:system :handler :writer :handle :node :tx-ops :tx-key
                 :before :after :request :response :physical-document]))
  (is (not (str/includes? (pr-str report) sentinel))))

(deftest real-biff-module-installs-and-invokes-guarded-writer
  (let [c (scenario :invoke) result (observe c)]
    (is (= 200 (get-in result [:response :status])))
    (is (= 1 @(:calls c)))
    (is (= 1 (revision c)))
    (require-nonclaims! (:report result))))

(deftest inspection-never-dispatches-handler-or-submits-transaction
  (let [c (scenario :invoke)]
    (is (zero? @(:calls c)))
    (is (= 0 (revision c)))
    (adoption/require-current-biff-host-adoption!
     (:adoption c) (:source c) (:system c))
    (coverage/require-registered-writer-coverage!
     (:inventory c) {writer-path (:writer-handle c)})
    (is (zero? @(:calls c)))
    (is (= 0 (revision c)))))

(deftest biff-handler-is-html-wrapped-not-naked-ring-router
  (let [c (scenario :invoke)
        host (adoption/require-current-biff-host-adoption!
              (:adoption c) (:source c) (:system c))]
    (is (= :wrapped-at-module-start (:canonical-html-preflight host)))
    (is (= :identical-to-guarded-checked-router-wrapper (:handler-slot host)))
    (is (fn? (get (:system c) app/biff-ring-handler-key)))
    (is (= 0 (revision c)))))

(deftest omitted-writer-cannot-be-inferred-from-successful-biff-response
  (let [c (scenario :omit)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (rejection #(observe c))))
    (is (= 1 @(:calls c)))
    (is (= 0 (revision c)))))

(deftest raw-write-only-does-not-satisfy-issued-callback-trace
  (let [c (scenario :direct-only)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (rejection #(observe c))))
    (is (true? (:rogue (wc/physical (:before c)))))
    (is (= 0 (revision c)))))

(deftest extra-unregistered-xtdb-write-is-still-invisible
  (let [c (scenario :direct-plus) result (observe c)]
    (require-nonclaims! (:report result))
    (is (= 1 (revision c)))
    (is (= :not-established
           (get-in result [:report :other-xtdb-writer-bypasses])))))

(deftest repeated-callback-invocation-never-counts-as-exactly-one
  (let [c (scenario :twice)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (rejection #(observe c))))
    (is (= 1 (revision c)))
    (is (= 1 @(:calls c)))))

(deftest asynchronous-callback-invocation-is-not-synchronous-evidence
  (let [c (scenario :async)]
    (is (= :missing-extra-or-foreign-writer-invocation
           (rejection #(observe c))))
    (is (= 1 (revision c)))))

(deftest host-registration-replacement-after-write-fails-postflight
  (let [c (scenario :swap-after)]
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 1 (revision c)))))

(deftest host-registration-replacement-before-write-fails-postflight
  (let [c (scenario :swap-before)]
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 1 (revision c)))))

(deftest replaced-writer-slot-before-dispatch-prevents-execution
  (let [c (scenario :invoke)]
    (reset! (:registry c) {writer-path (wc/registered (fn [] :foreign))})
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 0 @(:calls c)))
    (is (= 0 (revision c)))))

(deftest registered-delegating-wrapper-is-not-identical-issued-writer
  (let [c (scenario :invoke) wrapper (fn [] ((:writer c)))]
    (reset! (:registry c) {writer-path (wc/registered wrapper)})
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 0 @(:calls c)))))

(deftest extra-raw-host-writer-prevents-positive-inspection
  (let [c (scenario :invoke)]
    (swap! (:registry c) assoc :host/direct
           {:kind :raw-xtdb :operation-id nil :writer (fn [] :direct)})
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 0 @(:calls c)))))

(deftest wrong-operation-label-prevents-handler-dispatch
  (let [c (scenario :invoke)]
    (swap! (:registry c) assoc writer-path
           {:kind :issued-operation :operation-id :example/unrelated
            :writer (:writer c)})
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 0 @(:calls c)))))

(deftest absent-writer-registration-prevents-handler-dispatch
  (let [c (scenario :invoke)]
    (reset! (:registry c) {})
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(observe c))))
    (is (= 0 @(:calls c)))))

(deftest swapped-biff-handler-slot-rejected-before-running-the-replacement
  (let [c (scenario :invoke) fake-calls (atom 0)
        system (assoc (:system c) app/biff-ring-handler-key
                      (fn [_] (swap! fake-calls inc) {:status 200}))]
    (is (= :host-handler-slot-mismatch
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (:adoption c) (:source c) system (:request c)
                        (:inventory c) {writer-path (:writer-handle c)} writer-path))))
    (is (= 0 @fake-calls))
    (is (= 0 @(:calls c)))))

(deftest foreign-biff-adoption-handle-rejected-before-handler
  (let [c (scenario :invoke)]
    (is (= :unissued-host-adoption
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (Object.) (:source c) (:system c) (:request c)
                        (:inventory c) {writer-path (:writer-handle c)} writer-path))))
    (is (= 0 @(:calls c)))))

(deftest freshly-constructed-adoption-handle-is-not-issued
  (let [c (scenario :invoke)]
    (is (= :unissued-host-adoption
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (adoption/->HostAdoptionHandle) (:source c) (:system c)
                        (:request c) (:inventory c)
                        {writer-path (:writer-handle c)} writer-path))))
    (is (= 0 @(:calls c)))))

(deftest copied-contract-source-object-is-not-original-adoption
  (let [c (scenario :invoke)]
    (is (= :changed-host-adoption-source
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (:adoption c) (into {} (:source c)) (:system c)
                        (:request c) (:inventory c)
                        {writer-path (:writer-handle c)} writer-path))))
    (is (= 0 @(:calls c)))))

(deftest wrong-uri-does-not-borrow-a-certified-handler
  (let [c (scenario :invoke)]
    (doseq [uri ["/not-installed" "/operations/example/claim/"
                 "/operations/example/unrelated"]]
      (is (= :biff-route-not-literal-certified-or-wrong-operation
             (rejection #(observe c (assoc (:request c) :uri uri))))))
    (is (= 0 @(:calls c)))))

(deftest wrong-http-method-does-not-borrow-route-operation
  (let [c (scenario :invoke)]
    (is (= :biff-route-not-literal-certified-or-wrong-operation
           (rejection #(observe c (assoc (:request c) :request-method :get)))))
    (is (= 0 @(:calls c)))))

(deftest invalid-biff-observation-arguments-rejected-before-handler
  (let [c (scenario :invoke)]
    (doseq [[req handles path]
            [[nil {writer-path (:writer-handle c)} writer-path]
             [[] {writer-path (:writer-handle c)} writer-path]
             [{:request-method "POST" :uri "/operations/example/claim"}
              {writer-path (:writer-handle c)} writer-path]
             [{:request-method :post :uri 42}
              {writer-path (:writer-handle c)} writer-path]
             [(:request c) [] writer-path]
             [(:request c) {writer-path (:writer-handle c)} :unqualified]]]
      (is (= :invalid-biff-writer-observation-source
             (rejection #(coverage/observe-checked-biff-host-registered-writer!
                          (:adoption c) (:source c) (:system c) req
                          (:inventory c) handles path)))))
    (is (= 0 @(:calls c)))))

(deftest fabricated-writer-handle-cannot-authorize-dispatch
  (let [c (scenario :invoke)]
    (doseq [fake [(Object.) (coverage/->WriterHandle) nil]]
      (is (= :unknown-or-foreign-route-writer
             (rejection #(coverage/observe-checked-biff-host-registered-writer!
                          (:adoption c) (:source c) (:system c) (:request c)
                          (:inventory c) {writer-path fake} writer-path)))))
    (is (= 0 @(:calls c)))))

(deftest missing-or-wrong-path-for-issued-handle-is-rejected
  (let [c (scenario :invoke)]
    (is (= :unknown-or-foreign-route-writer
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (:adoption c) (:source c) (:system c) (:request c)
                        (:inventory c) {:host/other (:writer-handle c)} writer-path))))
    (is (= 0 @(:calls c)))))

(deftest foreign-issued-writer-is-not-substitutable-for-correct-callback
  (let [c (scenario :invoke) b (wc/seeded) other (wc/issue b)]
    (is (= :unverified-or-unaccounted-registered-writers
           (rejection #(coverage/observe-checked-biff-host-registered-writer!
                        (:adoption c) (:source c) (:system c) (:request c)
                        (:inventory c) {writer-path (:handle other)} writer-path))))
    (is (= 0 @(:calls c)))))

(deftest throwing-before-writing-does-not-manufacture-observation
  (let [c (scenario :throw-before)]
    (is (= :before-write (rejection #(observe c))))
    (is (= 0 (revision c)))))

(deftest throwing-after-writing-cannot-undo-physical-effects
  (let [c (scenario :throw-after)]
    (is (= :after-write (rejection #(observe c))))
    (is (= 1 (revision c)))))

(deftest repeated-observation-cannot-resubmit-one-shot-writer
  (let [c (scenario :invoke)]
    (is (= 200 (get-in (observe c) [:response :status])))
    (is (= 1 (revision c)))
    (is (= :one-shot-already-attempted (rejection #(observe c))))
    (is (= 1 (revision c)))
    (is (= 2 @(:calls c)))))

(deftest substituted-biff-adoption-recheck-is-rejected
  (let [c (scenario :invoke)]
    (with-redefs [adoption/require-current-biff-host-adoption!
                  (fn [& _] {:checked-certified-route-ids #{operation}})]
      (is (= :substituted-biff-host-adoption-boundary
             (rejection #(observe c)))))
    (is (= 0 @(:calls c)))))

(deftest substituted-ring-route-describer-rejected-before-dispatch
  (let [c (scenario :invoke)]
    (with-redefs [routes/describe-checked-ring-router
                  (fn [& _] {:route-ids #{operation}})]
      (is (= :substituted-route-observation-boundary
             (rejection #(observe c)))))
    (is (= 0 @(:calls c)))))

(deftest substituted-operation-summary-rejected-before-dispatch
  (let [c (scenario :invoke)]
    (with-redefs [app/operation-summary (fn [& _] {operation {:routes []}})]
      (is (= :substituted-route-observation-boundary
             (rejection #(observe c)))))
    (is (= 0 @(:calls c)))))

(deftest positive-observation-redacts-transaction-and-private-input
  (let [c (scenario :invoke) report (:report (observe c))
        printed (pr-str report)]
    (require-nonclaims! report)
    (doseq [private-text [sentinel (str (:xt/id (:before c)))
                          (str (:request-id (:request c)))]]
      (is (not (str/includes? printed private-text))))))

(deftest local-positive-observation-does-not-establish-server-or-writer-closure
  (let [c (scenario :invoke) report (:report (observe c))]
    (is (= :not-established (:host-deployment report)))
    (is (= :not-established (:host-writer-enumeration-exhaustiveness report)))
    (is (= :not-established (:other-xtdb-writer-bypasses report)))
    (is (= :not-established (:abstract-operation-correspondence report)))
    (is (= :not-established (:physical-commit-and-causality report)))
    (is (= :not-issued (:certificate report)))))

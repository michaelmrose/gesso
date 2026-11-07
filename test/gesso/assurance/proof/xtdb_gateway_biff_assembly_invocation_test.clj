(ns gesso.assurance.proof.xtdb-gateway-biff-assembly-invocation-test
  "v1558: permanent adversarial tests for the composed Biff host/writer
   assembly and one observed certified route dispatch, using a real Biff
   lifecycle and embedded XTDB. No finite survey proves writer closure,
   deployment identity, or refinement; post-dispatch rejection cannot undo
   previously executed writes."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.live.application-preflight :as application]
            [clojure.string :as str]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as biff-fixture]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [xtdb.api :as xt]))
(use-fixtures :once
  (fn [run]
    (let [f (first (:clojure.test/once-fixtures
                    (meta (the-ns 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (f run))))
(defn- context [behavior]
  ((var-get (ns-resolve 'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test 'scenario)) behavior))
(defn- args [c]
  [(:adoption c) (:source c) (:system c) (:inventory c)
   {:host/claim (:writer-handle c)}
   {:example/claim :host/claim} (:request c)])
(defn- observe [c] (apply assembly/observe-current-biff-registered-writer-assembly! (args c)))
(defn- reject [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :untyped))))
(defn- revision [c] (:revision (wc/physical (:before c))))
(deftest valid-composed-invocation
  (let [c (context :invoke) o (observe c) r (:report o)]
    (is (= 200 (get-in o [:response :status])))
    (is (= assembly/invocation-report-type (:type r)))
    (is (= assembly/invocation-report-version (:version r)))
    (is (= :conditional-observed-route-invoked-joined-one-shot-writer (:status r)))
    (is (= :only-this-observed-request (:invocation-coverage r)))
    (is (= :requires-new-one-shot (:issued-callback-for-next-request r)))
    (is (:observed-route-bound-to-exact-invoked-registered-writer? r))
    (is (:biff-handler-and-full-writer-assembly-rechecked-after-dispatch? r))
    (is (= 1 (:checked-certified-route-count r) (:covered-surveyed-writer-count r)
              (:observed-registered-callback-invocations r)))
    (is (= :not-established (:direct-and-external-xtdb-writer-closure r)))
    (is (= :not-established (:transaction-commit-and-physical-causality r)))
    (is (= :not-issued (:certificate r)))
    (is (= 1 (revision c)))
    (is (= 1 @(:calls c)))
    (is (not (str/includes? (pr-str r) "private-v1549-sentinel")))))
(deftest no-handler-dispatch-on-mismatched-request
  (let [c (context :invoke)]
    (is (= :request-not-one-source-checked-literal-bound-route
           (reject #(apply assembly/observe-current-biff-registered-writer-assembly!
                           (assoc (vec (args c)) 6 (assoc (:request c) :uri "/wrong"))))))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))
(deftest invalid-request-shape-rejected-before-dispatch
  (let [c (context :invoke)]
    (is (= :invalid-observed-biff-request
           (reject #(apply assembly/observe-current-biff-registered-writer-assembly!
                           (assoc (vec (args c)) 6 nil)))))
    (is (zero? @(:calls c)))))
(deftest incomplete-route-binding-rejected-before-dispatch
  (let [c (context :invoke)]
    (is (= :incomplete-or-mismatched-route-writer-join
           (reject #(apply assembly/observe-current-biff-registered-writer-assembly!
                           (assoc (vec (args c)) 5 {:example/claim :host/other})))))
    (is (zero? @(:calls c)))))
(deftest missing-callback-rejected-after-dispatch
  (let [c (context :omit)]
    (is (= :missing-extra-or-foreign-writer-invocation (reject #(observe c))))
    (is (= 1 @(:calls c)))
    (is (zero? (revision c)))))
(deftest direct-only-write-rejected-but-side-effect-not-undone
  (let [c (context :direct-only)]
    (is (= :missing-extra-or-foreign-writer-invocation (reject #(observe c))))
    (is (true? (:rogue (wc/physical (:before c)))))
    (is (zero? (revision c)))))
(deftest direct-plus-write-still-invisible
  (let [c (context :direct-plus) r (:report (observe c))]
    (is (= :only-this-observed-request (:invocation-coverage r)))
    (is (= :not-established (:direct-and-external-xtdb-writer-closure r)))
    (is (= 1 (revision c)))))
(deftest two-callback-calls-rejected-despite-first-commit
  (let [c (context :twice)]
    (is (= :missing-extra-or-foreign-writer-invocation (reject #(observe c))))
    (is (= 1 (revision c)))))
(deftest asynchronous-callback-not-synchronous-evidence
  (let [c (context :async)]
    (is (= :missing-extra-or-foreign-writer-invocation (reject #(observe c))))
    (is (= 1 (revision c)))))
(deftest host-registration-swap-after-writer-fails-closed
  (let [c (context :swap-after)]
    (is (not= :not-rejected (reject #(observe c))))
    (is (= 1 (revision c)))))
(deftest second-request-cannot-reuse-consumed-one-shot
  (let [c (context :invoke)]
    (observe c)
    (is (= 1 (revision c)))
    (is (not= :not-rejected (reject #(observe c))))
    (is (= 1 (revision c)))))
(deftest changed-host-registration-after-join-fails-before-dispatch
  (let [c (context :invoke)]
    (swap! (:registry c) assoc :host/raw
           {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)})
    (is (not= :not-rejected (reject #(observe c))))
    (is (zero? @(:calls c)))))
(deftest substituted-biff-observer-cannot-fabricate-a-composed-pass
  (let [c (context :invoke)]
    (with-redefs [coverage/observe-checked-biff-host-registered-writer!
                  (fn [& _] {:report {:type coverage/biff-observation-type}})]
      (is (= :substituted-biff-invocation-checker (reject #(observe c)))))
    (is (zero? @(:calls c)))))
(deftest preflight-is-nondispatching
  (let [c (context :invoke)]
    (apply assembly/require-current-biff-registered-writer-assembly! (butlast (args c)))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))


;; Permanent extension of v1557's 14 successful embedded-XTDB smoke cases.
;; Every case uses a fresh certified handler and locally issued writer.
(defn- altered [c index value]
  (apply assembly/observe-current-biff-registered-writer-assembly!
         (assoc (vec (args c)) index value)))

(deftest conditional-invocation-report-states-all-assurance-limits
  (let [c (context :invoke)
        r (:report (observe c))]
    (is (= :one-synchronous-process-local-biff-handler-dispatch (:scope r)))
    (is (true? (:all-source-checked-routes-locally-joined? r)))
    (is (true? (:all-surveyed-writer-paths-locally-joined? r)))
    (is (= :not-established (:other-routes-and-future-requests r)))
    (doseq [k [:host-enumerator-exhaustiveness
               :deployed-server-handler-identity
               :direct-and-external-xtdb-writer-closure
               :abstract-operation-refinement
               :transaction-commit-and-physical-causality]]
      (is (= :not-established (get r k)) (str "Not proven: " k)))
    (doseq [[k v] [[:certificate :not-issued]
                   [:admission :not-admitted]
                   [:transaction-authority :none]
                   [:authority :none]]]
      (is (= v (get r k))))
    (is (contains? (:open-obligations r)
                   :one-shot-callback-reissuance-for-subsequent-requests))
    (is (contains? (:open-obligations r)
                   :untested-routes-and-requests-still-need-invocation-observations))))

(deftest composed-report-never-exports-handles-physical-values-or-tx-data
  (let [c (context :invoke)
        report (:report (observe c))
        serialized (pr-str report)]
    (is (not-any? #(contains? report %)
                  [:node :tx-key :tx-ops :before :after :handler :writer
                   :writer-handle :inventory :source :system :request
                   :response :adoption :physical-document :document-id]))
    (is (not (str/includes? serialized "private-v1549-sentinel")))
    (is (not (str/includes? serialized "xt/id")))
    (is (not (str/includes? serialized "request-id")))))

(deftest wrong-http-method-rejected-without-handler-execution
  (let [c (context :invoke)]
    (is (= :request-not-one-source-checked-literal-bound-route
           (reject #(altered c 6 (assoc (:request c) :request-method :get)))))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))

(deftest malformed-url-not-accepted-as-request-route
  (let [c (context :invoke)]
    (is (= :invalid-observed-biff-request
           (reject #(altered c 6 (assoc (:request c) :uri 123)))))
    (is (zero? @(:calls c)))))

(deftest string-http-method-not-accepted-as-certified-method
  (let [c (context :invoke)]
    (is (= :invalid-observed-biff-request
           (reject #(altered c 6 (assoc (:request c)
                                         :request-method "POST")))))
    (is (zero? @(:calls c)))))

(deftest unknown-route-writer-identifier-rejected-before-dispatch
  (let [c (context :invoke)]
    (is (= :incomplete-or-mismatched-route-writer-join
           (reject #(altered c 5 {:example/claim :host/unknown}))))
    (is (zero? @(:calls c)))))

(deftest missing-issued-handle-rejected-before-dispatch
  (let [c (context :invoke)]
    (is (not= :not-rejected (reject #(altered c 4 {}))))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))

(deftest forged-issued-handle-rejected-before-dispatch
  (let [c (context :invoke)]
    (is (not= :not-rejected
              (reject #(altered c 4
                                {:host/claim (coverage/->WriterHandle)}))))
    (is (zero? @(:calls c)))))

(deftest independently-issued-handle-is-not-interchangeable
  (let [c (context :invoke) foreign (context :invoke)]
    (is (not= :not-rejected
              (reject #(altered c 4 {:host/claim (:writer-handle foreign)}))))
    (is (zero? @(:calls c)))
    (is (zero? @(:calls foreign)))))

(deftest altered-contract-source-is-not-the-installed-source
  (let [c (context :invoke)]
    (is (= :changed-host-adoption-source
           (reject #(altered c 1 (assoc (:source c)
                                         :unreviewed-source-change true)))))
    (is (zero? @(:calls c)))))

(deftest copied-contract-source-loses-issuance-identity
  (let [c (context :invoke)]
    (is (= :changed-host-adoption-source
           (reject #(altered c 1 (into {} (:source c))))))
    (is (zero? @(:calls c)))))

(deftest replaced-biff-handler-slot-rejected-before-dispatch
  (let [c (context :invoke)
        fake-calls (atom 0)
        fake (fn [_] (swap! fake-calls inc) {:status 200})
        tampered (assoc (:system c) application/biff-ring-handler-key fake)]
    (is (= :host-handler-slot-mismatch
           (reject #(altered c 2 tampered))))
    (is (zero? @fake-calls))
    (is (zero? @(:calls c)))))

(deftest forged-biff-host-adoption-handle-does-not-adopt
  (let [c (context :invoke)]
    (is (= :unissued-host-adoption
           (reject #(altered c 0 (adoption/->HostAdoptionHandle)))))
    (is (zero? @(:calls c)))))

(deftest changing-host-writer-callback-during-dispatch-is-rejected
  (let [c (context :swap-before)]
    (is (not= :not-rejected (reject #(observe c))))
    ;; The original real writer was invoked despite the registry substitution.
    ;; Rejection cannot undo an already committed physical change.
    (is (= 1 (revision c)))
    (is (= 1 @(:calls c)))))

(deftest throwing-route-before-writer-does-not-produce-invocation-evidence
  (let [c (context :throw-before)]
    (is (not= :not-rejected (reject #(observe c))))
    (is (= 1 @(:calls c)))
    (is (zero? (revision c)))))

(deftest throwing-route-after-writer-does-not-reverse-physical-write
  (let [c (context :throw-after)]
    (is (not= :not-rejected (reject #(observe c))))
    (is (= 1 @(:calls c)))
    (is (= 1 (revision c)))))

(deftest pre-dispatch-raw-registration-does-not-become-observed-coverage
  (let [c (context :invoke)]
    (swap! (:registry c) assoc :host/raw
           {:kind :raw-xtdb :operation-id nil :writer (fn [] :untracked)})
    (is (not= :not-rejected (reject #(observe c))))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))

(deftest wrapped-registered-callback-cannot-be-accepted
  (let [c (context :invoke) original (:writer c)]
    (swap! (:registry c) update :host/claim assoc
           :writer (fn [] (original)))
    (is (not= :not-rejected (reject #(observe c))))
    (is (zero? @(:calls c)))))

(deftest late-registration-replacement-after-clean-report-requires-reinspection
  (let [c (context :invoke)
        before (apply assembly/require-current-biff-registered-writer-assembly!
                      (butlast (args c)))]
    (is (= :conditional-current-biff-handler-and-issued-writers-joined
           (:status before)))
    (swap! (:registry c) assoc :host/rogue
           {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)})
    (is (not= :not-rejected (reject #(observe c))))
    (is (zero? @(:calls c)))))

(deftest reused-writer-with-new-request-id-is-still-one-shot
  (let [c (context :invoke)]
    (observe c)
    (is (= 1 (revision c)))
    (is (not= :not-rejected
              (reject #(altered c 6 (assoc (:request c)
                                            :request-id (random-uuid))))))
    (is (= 1 (revision c)))
    (is (= 2 @(:calls c)))))

(deftest request-observation-cannot-reissue-next-request-writer
  (let [c (context :invoke) r (:report (observe c))]
    (is (= :requires-new-one-shot (:issued-callback-for-next-request r)))
    (is (= :only-this-observed-request (:invocation-coverage r)))
    (is (not= :not-rejected (reject #(observe c))))
    (is (= 1 (revision c)))))

(deftest direct-xtdb-write-can-remain-invisible-while-composed-check-passes
  (let [c (context :direct-plus)
        o (observe c)]
    (is (= 200 (get-in o [:response :status])))
    (is (true? (:observed-route-bound-to-exact-invoked-registered-writer?
                (:report o))))
    (is (= :not-established
           (get-in o [:report :direct-and-external-xtdb-writer-closure])))
    (is (= 1 (revision c)))))

(deftest substituted-original-host-adoption-checker-refused
  (let [c (context :invoke)]
    (with-redefs [adoption/require-current-biff-host-adoption!
                  (fn [& _] {:type :forged})]
      (is (= :substituted-host-writer-or-assembly-checker
             (reject #(observe c)))))
    (is (zero? @(:calls c)))))

(deftest substituted-writer-registration-checker-refused
  (let [c (context :invoke)]
    (with-redefs [coverage/require-registered-writer-coverage!
                  (fn [& _] {:all-currently-surveyed-writers-locally-bound? true})]
      (is (= :substituted-host-writer-or-assembly-checker
             (reject #(observe c)))))
    (is (zero? @(:calls c)))))

(deftest observer-refuses-substitution-without-dispatch
  (let [c (context :invoke)]
    (with-redefs [coverage/observe-checked-biff-host-registered-writer!
                  (fn [& _] {:report {:status :forged-positive}})]
      (is (= :substituted-biff-invocation-checker
             (reject #(observe c)))))
    (is (zero? @(:calls c)))))

(deftest no-hidden-route-invocation-during-unified-assembly-inspection
  (let [c (context :invoke)]
    (doseq [_ (range 3)]
      (let [r (apply assembly/require-current-biff-registered-writer-assembly!
                     (butlast (args c)))]
        (is (= :not-established (:route-writer-invocation r)))))
    (is (zero? @(:calls c)))
    (is (zero? (revision c)))))

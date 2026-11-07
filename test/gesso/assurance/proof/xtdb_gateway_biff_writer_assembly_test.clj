(ns gesso.assurance.proof.xtdb-gateway-biff-writer-assembly-test
  "v1556: permanent adversarial embedded-XTDB tests for the fail-closed,
   non-dispatching join of an actually Biff-started checked handler, certified
   literal routes, and locally issued monotonic writer registrations.

   These tests deliberately distinguish an exact process-local join from
   route invocation, host enumeration completeness, command refinement,
   deployment authentication and physical writer closure."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.assurance.proof.xtdb-gateway-biff-writer-assembly :as assembly]
            [gesso.assurance.proof.checked-biff-host-adoption :as adoption]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-biff-host-observation-test :as biff-fixture]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as application]
            [xtdb.api :as xt]))

(def ^:private route-id :example/claim)
(def ^:private writer-path :host/claim)
(def ^:private original-binding {route-id writer-path})
(def ^:private secret "private-v1549-sentinel")

;; The existing v1554 fixture constructs the certified closed application,
;; genuine Biff startup and real XTDB node. Reuse the real fixture, not a
;; simulated adoption receipt or mocked writer registration.
(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                         (meta (the-ns
                                'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test))))]
      (fixture run))))

(defn- context []
  ((var-get (ns-resolve
             'gesso.assurance.proof.xtdb-gateway-biff-host-observation-test
             'scenario)) :invoke))

(defn- inputs [c]
  [(:adoption c) (:source c) (:system c)
   (:inventory c) {writer-path (:writer-handle c)} original-binding])

(defn- call-with [c modifications]
  (let [args (reduce-kv assoc (vec (inputs c)) modifications)]
    (apply assembly/require-current-biff-registered-writer-assembly! args)))

(defn- joined [c]
  (call-with c {}))

(defn- reason [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (or (:reason (ex-data e)) :untyped-rejection))))

(defn- expected-nonclaims! [r]
  (is (= assembly/report-type (:type r)))
  (is (= assembly/report-version (:version r)))
  (is (= :conditional-current-biff-handler-and-issued-writers-joined
         (:status r)))
  (is (= :one-returned-process-local-biff-system-and-host-writer-survey
         (:scope r)))
  (is (= 1 (:checked-certified-route-count r)
         (:matched-route-writer-binding-count r)
         (:covered-surveyed-writer-count r)))
  (doseq [k [:all-source-checked-routes-locally-joined?
             :all-surveyed-writer-paths-locally-joined?
             :same-biff-handler-slot-rechecked?
             :same-host-registrations-rechecked?]]
    (is (true? (get r k)) (str "Expected local join property " k)))
  (doseq [k [:route-writer-invocation
             :deployed-server-handler-identity
             :host-writer-enumerator-exhaustiveness
             :physical-execution-and-refinement]]
    (is (= :not-established (get r k)) (str "Open conclusion " k)))
  (doseq [[k v] [[:certificate :not-issued]
                 [:admission :not-admitted]
                 [:transaction-authority :none]
                 [:authority :none]]]
    (is (= v (get r k))))
  (doseq [obligation
          [:host-enumerator-exhaustiveness-and-after-check-stability
           :deployed-server-uses-this-biff-system-handler
           :route-invokes-its-declared-registered-writer
           :unregistered-direct-and-external-xtdb-writes
           :writer-command-refines-declared-abstract-operation
           :writer-history-monotonicity-outside-issued-gateways
           :one-shot-callback-lifetime-versus-repeated-http-requests
           :physical-refinement-and-deployment-certification]]
    (is (contains? (:open-obligations r) obligation)
        (str "Missing unresolved obligation " obligation)))
  (is (not-any? #(contains? r %)
                [:handler :node :system :writer :handle :xtdb-node
                 :tx-key :tx-ops :before :after :request :response
                 :route-writer-paths :physical-documents]))
  (is (not (str/includes? (pr-str r) secret))))

(deftest exact-genuine-biff-host-and-issued-writer-join
  (let [c (context) report (joined c)]
    (expected-nonclaims! report)
    (is (zero? @(:calls c)))
    (is (= 0 (:revision (wc/physical (:before c)))))))

(deftest local-join-may-be-inspected-repeatedly-without-consuming-writer
  (let [c (context) first-report (joined c) second-report (joined c)]
    (is (= first-report second-report))
    (is (= 0 @(:calls c)))
    (is (= 0 (:revision (wc/physical (:before c)))))
    (is (= :conditional-current-biff-handler-and-issued-writers-joined
           (:status second-report)))))

(deftest check-does-not-dispatch-a-ring-or-biff-request
  (let [c (context)]
    (joined c)
    (is (zero? @(:calls c)))
    (is (= 0 (:revision (wc/physical (:before c)))))
    (is (fn? (:writer c)))))

(deftest checked-join-does-not-consume-registered-one-shot-permit
  (let [c (context)]
    (joined c)
    (is (map? ((:writer c))))
    (is (= 1 (:revision (wc/physical (:before c)))))
    ;; A writer remains an issued registration after its one-shot is used,
    ;; but this check does not certify it is available for another request.
    (expected-nonclaims! (joined c))))

(deftest nil-route-bindings-rejected-before-dispatch
  (let [c (context)]
    (is (= :invalid-route-writer-bindings (reason #(call-with c {5 nil}))))
    (is (zero? @(:calls c)))))

(deftest empty-route-bindings-rejected
  (let [c (context)]
    (is (= :invalid-route-writer-bindings (reason #(call-with c {5 {}}))))))

(deftest list-route-bindings-rejected
  (let [c (context)]
    (is (= :invalid-route-writer-bindings
           (reason #(call-with c {5 (list [route-id writer-path])}))))))

(deftest metadata-bearing-route-map-rejected
  (let [c (context)]
    (is (= :invalid-route-writer-bindings
           (reason #(call-with c {5 (with-meta original-binding
                                     {:unreviewed true})}))))))

(deftest unqualified-route-identifier-rejected
  (let [c (context)]
    (is (= :invalid-route-writer-bindings
           (reason #(call-with c {5 {:claim writer-path}}))))))

(deftest unqualified-writer-path-rejected
  (let [c (context)]
    (is (= :invalid-route-writer-bindings
           (reason #(call-with c {5 {route-id :claim}}))))))

(deftest too-many-route-bindings-rejected
  (let [c (context)
        oversized (into {} (for [n (range 513)]
                             [(keyword "example" (str "extra" n)) writer-path]))]
    (is (= :invalid-route-writer-bindings
           (reason #(call-with c {5 oversized}))))
    (is (zero? @(:calls c)))))

(deftest source-route-binding-with-a-foreign-route-id-rejected
  (let [c (context)]
    (is (= :incomplete-or-mismatched-route-writer-join
           (reason #(call-with c {5 {:example/foreign writer-path}}))))))

(deftest extra-route-binding-not-derived-from-certified-assembly-rejected
  (let [c (context)]
    (is (= :incomplete-or-mismatched-route-writer-join
           (reason #(call-with c {5 (assoc original-binding
                                           :example/foreign writer-path)}))))))

(deftest wrong-issued-writer-path-in-route-join-rejected
  (let [c (context)]
    (is (= :incomplete-or-mismatched-route-writer-join
           (reason #(call-with c {5 {route-id :host/foreign}}))))))

(deftest missing-writer-handle-table-is-not-covered
  (let [c (context)]
    (is (not= :not-rejected (reason #(call-with c {4 {}}))))))

(deftest freshly-forged-writer-handle-cannot-join
  (let [c (context)]
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(call-with c {4 {writer-path (coverage/->WriterHandle)}}))))))

(deftest another-legitimately-issued-handle-cannot-borrow-a-callback
  (let [c (context) other (context)]
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(call-with c {4 {writer-path (:writer-handle other)}}))))))

(deftest foreign-registered-operation-identity-cannot-join
  (let [c (context)]
    (swap! (:registry c) update writer-path assoc :operation-id :example/foreign)
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))))

(deftest exact-callback-must-not-be-wrapped
  (let [c (context) original (:writer c)]
    (swap! (:registry c) update writer-path assoc
           :writer (fn [] (original)))
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))
    (is (zero? @(:calls c)))))

(deftest raw-xtdb-writer-added-to-host-survey-is-open
  (let [c (context)]
    (swap! (:registry c) assoc :host/direct
           {:kind :raw-xtdb :operation-id nil :writer (fn [] :direct)})
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))))

(deftest extra-host-path-with-issued-looking-but-foreign-writer-is-open
  (let [c (context)]
    (swap! (:registry c) assoc :host/background
           {:kind :issued-operation :operation-id route-id
            :writer (fn [] :outside-issued-registration)})
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))))

(deftest missing-host-registration-rejected
  (let [c (context)]
    (reset! (:registry c) {})
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))))

(deftest replaced-handler-slot-rejected-without-executing-it
  (let [c (context) calls (atom 0)
        fake (fn [_] (swap! calls inc) {:status 200})
        replaced (assoc (:system c) application/biff-ring-handler-key fake)]
    (is (= :host-handler-slot-mismatch
           (reason #(call-with c {2 replaced}))))
    (is (zero? @calls))
    (is (zero? @(:calls c)))))

(deftest missing-biff-handler-slot-rejected
  (let [c (context)]
    (is (= :host-handler-slot-mismatch
           (reason #(call-with c {2 (dissoc (:system c)
                                             application/biff-ring-handler-key)}))))))

(deftest fabricated-biff-adoption-handle-rejected
  (let [c (context)]
    (is (= :unissued-host-adoption
           (reason #(call-with c {0 (adoption/->HostAdoptionHandle)}))))))

(deftest foreign-biff-adoption-handle-is-not-transferrable
  (let [c (context) other (context)]
    (is (= :changed-host-adoption-source
           (reason #(call-with c {0 (:adoption other)}))))))

(deftest copied-contract-source-is-not-the-original-host-source
  (let [c (context)]
    (is (= :changed-host-adoption-source
           (reason #(call-with c {1 (into {} (:source c))}))))))

(deftest edited-contract-source-cannot-rewrite-checked-route-set
  (let [c (context)
        edited (assoc (:source c) :semantic-route-handlers {})]
    (is (= :changed-host-adoption-source
           (reason #(call-with c {1 edited}))))))

(deftest raw-writer-claiming-a-certified-operation-rejected
  (let [c (context)]
    (swap! (:registry c) update writer-path assoc :kind :raw-xtdb)
    (is (= :nonissued-writer-claims-operation
           (reason #(joined c))))))

(deftest registration-changes-after-positive-inspection-require-fresh-rejection
  (let [c (context) report (joined c)]
    (expected-nonclaims! report)
    (swap! (:registry c) assoc :host/new
           {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)})
    (is (= :unverified-or-unaccounted-registered-writers
           (reason #(joined c))))))

(deftest unstable-host-survey-is-not-accepted-as-a-stable-join
  (let [c (context) counter (atom 0)
        original (:enumerate-write-paths! (:inventory c))
        unstable (assoc (:inventory c) :enumerate-write-paths!
                        (fn []
                          (if (> (swap! counter inc) 6)
                            {writer-path {:kind :issued-operation
                                          :operation-id route-id
                                          :writer (fn [] :substituted)}}
                            (original))))]
    (is (contains? #{:host-or-writer-registration-changed-during-check
                     :unverified-or-unaccounted-registered-writers
                     :unstable-or-inconsistent-host-registrations}
                   (reason #(call-with c {3 unstable}))))))

(deftest throwing-host-enumerator-is-not-accepted
  (let [c (context)
        bad (assoc (:inventory c) :enumerate-write-paths!
                   (fn [] (throw (ex-info "enumerator unavailable" {}))))]
    (is (not= :not-rejected (reason #(call-with c {3 bad}))))))

(deftest missing-host-enumerator-is-not-accepted
  (let [c (context)
        bad (dissoc (:inventory c) :enumerate-write-paths!)]
    (is (not= :not-rejected (reason #(call-with c {3 bad}))))))

(deftest substituted-biff-host-checker-rejected-before-trusting-evidence
  (let [c (context)]
    (with-redefs [adoption/require-current-biff-host-adoption!
                  (fn [& _] {:checked-route-ids #{route-id}})]
      (is (= :substituted-host-writer-or-assembly-checker
             (reason #(joined c)))))))

(deftest substituted-writer-coverage-checker-rejected
  (let [c (context)]
    (with-redefs [coverage/require-registered-writer-coverage!
                  (fn [& _] {:all-currently-surveyed-writers-locally-bound? true})]
      (is (= :substituted-host-writer-or-assembly-checker
             (reason #(joined c)))))))

(deftest substituted-independent-operation-summary-rejected
  (let [c (context)]
    (with-redefs [application/operation-summary (fn [& _] {})]
      (is (= :substituted-host-writer-or-assembly-checker
             (reason #(joined c)))))))

(deftest outside-writer-omitted-from-host-enumerator-remains-invisible
  (let [c (context) original (:before c)]
    (xt/execute-tx wc/*node* [[:put-docs wc/table (assoc original :rogue true)]])
    (is (true? (:rogue (wc/physical original))))
    (expected-nonclaims! (joined c))
    (is (zero? @(:calls c)))))

(deftest host-enumerator-can-lie-about-an-extra-direct-writer
  (let [c (context) original (:enumerate-write-paths! (:inventory c))
        incomplete (assoc (:inventory c) :enumerate-write-paths!
                          (fn [] (select-keys (original) [writer-path])))]
    (swap! (:registry c) assoc :host/unseen
           {:kind :raw-xtdb :operation-id nil :writer (fn [] :unseen)})
    ;; The trusted host survey hides the new path. No closed-world conclusion.
    (let [r (call-with c {3 incomplete})]
      (is (= :conditional-current-biff-handler-and-issued-writers-joined
             (:status r)))
      (is (= :not-established (:host-writer-enumerator-exhaustiveness r)))
      (is (contains? (:open-obligations r)
                     :unregistered-direct-and-external-xtdb-writes)))))

(deftest copied-returned-system-map-does-not-authenticate-deployment
  (let [c (context) copied (into {} (:system c))
        report (call-with c {2 copied})]
    ;; Same local handler object is checkable in a copied system map.
    (is (= :conditional-current-biff-handler-and-issued-writers-joined
           (:status report)))
    (is (= :not-established (:deployed-server-handler-identity report)))))

(deftest invalid-argument-failure-leaks-no-physical-source
  (let [c (context)
        e (try (call-with c {5 {:example/secret-route :host/unissued}})
               nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo e))
    (is (not (str/includes? (str (.getMessage e) (ex-data e)) secret)))
    (is (= :incomplete-or-mismatched-route-writer-join
           (:reason (ex-data e))))))

(deftest assembled-writer-operation-is-still-only-a-host-label
  (let [c (context) report (joined c)]
    (is (= :not-established (:physical-execution-and-refinement report)))
    (is (contains? (:open-obligations report)
                   :writer-command-refines-declared-abstract-operation))
    (is (contains? (:open-obligations report)
                   :actual-request-and-principal-authentication))
    (is (zero? @(:calls c)))))

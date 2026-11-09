(ns gesso.assurance.proof.xtdb-gateway-biff-source-backed-target-request-test
  "v1581: permanent real-Biff/XTDB adversarial regression for v1580's
   source-backed target-bound request mediators.

   A complete static route-to-target declaration is resolved by v1578 from
   the current certified contract. The exact issued resolver is required at
   mediator construction and rechecked at startup and request dispatch.
   v1574 compares the selected operation and actual command/protected document
   with that independently declared target before a request-local writer is
   issued. Both bounded and unbounded paths are exercised with real XTDB.

   This proves neither independent authenticity of the application-supplied
   static targets nor abstract operation refinement, transaction causality,
   deployment-wide adoption, or exclusion of outside XTDB writers. Literal
   routes with fixed targets are the only covered provenance case."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test :as multi]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance :as provenance]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as application]))

(def ^:private private-token "private-v1581-target-should-not-leak")

(defn- real-certified-fixture [run]
  (let [fixture (first (:clojure.test/once-fixtures
                        (meta (the-ns
                               'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test))))]
    (when-not (fn? fixture)
      (throw (ex-info "Missing real certified two-route fixture"
                      {:reason :missing-real-two-route-fixture})))
    (fixture run)))

(use-fixtures :once real-certified-fixture)

(defn- original [symbol]
  (let [v (ns-resolve
           'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
           symbol)]
    (when-not v
      (throw (ex-info "Missing real Biff test helper"
                      {:reason :missing-real-biff-helper :symbol symbol})))
    (var-get v)))

(defn- fresh [] ((original 'build-case)))
(defn- request [which] ((original 'req) which))
(defn- revision [before] (:revision (wc/physical before)))
(defn- source-for-request [c req] ((original 'source-for-request) c req))

(defn- static-targets [c]
  {multi/claim-route {:command-operation :claim
                      :target [wc/table (:xt/id (:claim-before c))]}
   multi/cancel-route {:command-operation :claim
                       :target [wc/table (:xt/id (:cancel-before c))]}})

(defn- source-map []
  {:per-operation-writer-sources
   {multi/claim wc/*source*
    multi/cancel (:writer-source multi/*second*)}})

(defn- issue!
  ([c candidate! issuance] (issue! c candidate! issuance nil))
  ([c candidate! issuance policy]
   (let [inputs [(:adoption c) (:source c) (source-map)
                 multi/writer-bindings wc/*node* candidate! :revision]]
     (if policy
       (apply parallel/issue-source-backed-target-bound-bounded-isolated-parallel-biff-request-module!
              (conj inputs policy issuance))
       (apply parallel/issue-source-backed-target-bound-isolated-parallel-biff-request-module!
              (conj inputs issuance))))))

(def ^:private bounded-policy
  {:max-active-requests 2 :max-pending-requests 2
   :wait-timeout-ms 1000})

(defn- case-with!
  ([candidate! policy] (case-with! candidate! policy nil))
  ([candidate! policy declarations!]
   (let [c (fresh)
         built (atom [])
         candidate (fn [req]
                     (swap! built conj (:uri req))
                     (candidate! c req))
         table (if declarations! (declarations! c) (static-targets c))
         issuance (provenance/issue-certified-literal-route-target-resolver!
                   (:source c) table)
         mediator (issue! c candidate issuance policy)
         module (:module mediator)]
     ((:biff.core/init module) nil)
     (assoc c
            :candidate-calls built
            :target-issuance issuance
            :mediator mediator
            :final-system ((:biff.core/start module) (:system c))))))

(defn- normal-case!
  ([] (normal-case! nil))
  ([policy] (case-with! source-for-request policy)))

(defn- call! [c which]
  ((get (:final-system c) application/biff-ring-handler-key) (request which)))

(defn- observe [f]
  (try
    {:accepted (f)}
    (catch clojure.lang.ExceptionInfo e
      {:message (ex-message e) :details (ex-data e)})))

(defn- assert-rejected! [expected-reason f]
  (let [{:keys [details message] :as observation} (observe f)]
    (is (not (contains? observation :accepted))
        "A rejected construction/request must not return success")
    (is (= expected-reason (:reason details)))
    (is (some true? ((juxt :xtdb-gateway-biff-parallel-request-context/rejection
                           :xtdb-gateway-operation-target-binding/rejection
                           :xtdb-gateway-route-target-provenance/rejection) details)))
    (is (not (str/includes? (str message " " (pr-str details)) private-token)))))

(defn- assert-untouched! [c]
  (is (= 0 (revision (:claim-before c))) "Claim document remains unchanged")
  (is (= 0 (revision (:cancel-before c))) "Cancel document remains unchanged"))

(defn- report [c]
  (parallel/require-current-isolated-parallel-biff-request-module!
   (get-in c [:mediator :handle]) (:source c) (:final-system c)))

(deftest source-backed-issuer-refuses-arbitrary-host-callbacks
  (let [c (fresh)
        issuance (provenance/issue-certified-literal-route-target-resolver!
                  (:source c) (static-targets c))
        resolver (:expected-target! issuance)
        candidate #(source-for-request c %)]
    (assert-rejected! :requires-exact-issued-route-target-resolver
                      #(issue! c candidate resolver))
    (assert-rejected! :requires-exact-issued-route-target-resolver
                      #(issue! c candidate (assoc issuance :extra true)))
    (assert-rejected! :requires-exact-issued-route-target-resolver
                      #(issue! c candidate nil))
    (assert-rejected! :unissued-or-incompatible-route-target-resolver
                      #(issue! c candidate {:expected-target! (fn [req] (resolver req))}))
    (assert-rejected! :unissued-or-incompatible-route-target-resolver
                      #(issue! (assoc c :source (assoc (:source c) :unrelated true))
                               candidate issuance))
    (assert-untouched! c)))

(deftest source-backed-bounded-issuer-has-same-exact-issuance-restriction
  (let [c (fresh)
        issuance (provenance/issue-certified-literal-route-target-resolver!
                  (:source c) (static-targets c))
        candidate #(source-for-request c %)]
    (assert-rejected! :requires-exact-issued-route-target-resolver
                      #(issue! c candidate (:expected-target! issuance) bounded-policy))
    (assert-rejected! :unissued-or-incompatible-route-target-resolver
                      #(issue! c candidate {:expected-target! (fn [req]
                                                                ((:expected-target! issuance) req))}
                               bounded-policy))
    (assert-untouched! c)))

(deftest source-backed-report-asserts-only-conditional-evidence
  (let [c (normal-case!)
        r (report c)
        printed (pr-str r)]
    (is (true? (:source-backed-target-resolver-required? r)))
    (is (= :required-before-each-request-writer-issuance
           (:operation-target-binding-policy r)))
    (is (= :issued-source-linked-static-route-declaration-not-authenticated
           (:expected-target-provenance r)))
    (is (= :not-established (:operation-refinement r)))
    (is (= :not-established (:other-writer-closure r)))
    (is (= :not-established (:actual-http-server-adoption r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r) (:transaction-authority r)))
    (is (not (str/includes? printed private-token)))
    (is (empty? @(:candidate-calls c)))
    (assert-untouched! c)))

(deftest source-backed-unbounded-real-biff-writes-match-distinct-targets
  (let [c (normal-case!)]
    (is (= 200 (:status (call! c :claim))))
    (is (= 1 (revision (:claim-before c))))
    (is (= 0 (revision (:cancel-before c))))
    (is (= 200 (:status (call! c :cancel))))
    (is (= 1 (revision (:cancel-before c))))
    (is (= ["/operations/example/claim" "/operations/example/cancel"]
           @(:candidate-calls c)))))

(deftest source-backed-rejects-wrong-physical-document-before-write
  (let [c (case-with! (fn [case _] (wc/source (:cancel-before case))) nil)]
    (assert-rejected! :command-target-disagrees-with-expectation
                      #(call! c :claim))
    (is (= ["/operations/example/claim"] @(:candidate-calls c)))
    (assert-untouched! c)))

(deftest source-backed-rejects-protected-document-substitution
  (let [c (case-with! (fn [case req]
                        (assoc-in (source-for-request case req)
                                  [:protected-documents 0 :target]
                                  [wc/table (:xt/id (:cancel-before case))])) nil)]
    (assert-rejected! :protected-document-disagrees-with-expectation
                      #(call! c :claim))
    (assert-untouched! c)))

(deftest source-backed-route-declaration-cannot-silently-swap-target
  (let [c (case-with! source-for-request nil
                      (fn [case]
                        (assoc-in (static-targets case)
                                  [multi/claim-route :target]
                                  [wc/table (:xt/id (:cancel-before case))])))]
    (assert-rejected! :command-target-disagrees-with-expectation
                      #(call! c :claim))
    (assert-untouched! c)))

(deftest source-backed-unmatched-route-issues-no-writer
  (let [c (normal-case!)
        handler (get (:final-system c) application/biff-ring-handler-key)]
    (doseq [request [{:request-method :get :uri "/operations/example/claim"}
                     {:request-method :post :uri "/operations/example/unknown"}
                     nil]]
      (assert-rejected! :unmatched-parallel-request-route #(handler request)))
    (is (empty? @(:candidate-calls c)))
    (assert-untouched! c)))

(deftest source-backed-bounded-real-biff-writes-match-targets
  (let [c (normal-case! bounded-policy)
        r (report c)]
    (is (= :bounded-fair-permits-with-request-local-registries
           (:parallel-admission-policy r)))
    (is (true? (:source-backed-target-resolver-required? r)))
    (is (= 200 (:status (call! c :cancel))))
    (is (= 1 (revision (:cancel-before c))))
    (is (= 0 (revision (:claim-before c))))
    (is (= ["/operations/example/cancel"] @(:candidate-calls c)))))

(deftest source-backed-bounded-path-also-rejects-wrong-target
  (let [c (case-with! (fn [case _] (wc/source (:cancel-before case)))
                      bounded-policy)]
    (assert-rejected! :command-target-disagrees-with-expectation
                      #(call! c :claim))
    (assert-untouched! c)))

(deftest source-backed-late-provenance-checker-substitution-fails-closed
  (let [c (normal-case! bounded-policy)]
    (assert-rejected! :substituted-parallel-mediation-boundary
                      #(with-redefs
                         [provenance/require-current-certified-literal-route-target-resolver!
                          (fn [& _] {:status :forged})]
                         (call! c :claim)))
    (is (empty? @(:candidate-calls c))
        "The substituted checker must be rejected before source construction")
    (assert-untouched! c)
    (is (= 200 (:status (call! c :claim)))
        "Removing the substitution must restore the ordinary checked path")
    (is (= 1 (revision (:claim-before c))))
    (is (= 1 (count @(:candidate-calls c))))))

(deftest source-backed-valid-resolver-does-not-authenticate-wrong-host-data
  (let [c (normal-case!)
        resolver (get-in c [:target-issuance :expected-target!])
        request (request :claim)
        target (resolver request)]
    (is (= multi/claim (:operation-id target)))
    (is (= [wc/table (:xt/id (:claim-before c))] (:target target)))
    (is (= target
           (resolver (assoc request :params {:id (:xt/id (:cancel-before c))}
                                   :candidate-source (wc/source (:cancel-before c))))))
    (assert-untouched! c)))

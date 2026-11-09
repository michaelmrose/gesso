(ns gesso.assurance.proof.xtdb-gateway-biff-target-bound-request-test
  "v1577: permanent REAL Biff + embedded XTDB regression for the explicit
   v1576 target-bound per-request mediator. Independently derived expected
   operation/physical target must match the exact source before the selected
   guarded writer is issued. A rejected candidate must not modify either
   seeded document. The matched path must still execute the real writer.

   The fixture reuses the independently checked two-operation certified Biff
   application from v1570. Tests use actual XTDB writes, not mocked submit!
   or a replacement application handler. These conditional local checks do
   not establish independent host provenance, business-rule equivalence,
   complete deployed writer coverage, or transaction causality."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-request-context :as parallel]
            [gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test :as multi]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage-test :as wc]
            [gesso.live.application-preflight :as app]))

;; Reuse the REAL certified two-operation Biff fixture, including independent
;; operation inventories, a live XTDB node, Biff module startup and checked
;; route handlers. Every test creates fresh seeded documents and its own
;; target-bound mediator; none modifies the fixture's certified handlers.
(use-fixtures :once
  (fn [run]
    (let [fixture (first (:clojure.test/once-fixtures
                          (meta (the-ns
                                 'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test))))]
      (when-not (fn? fixture)
        (throw (ex-info "Missing real multi-route test fixture"
                        {:reason :missing-real-biff-fixture})))
      (fixture run))))

(defn- original [symbol]
  (let [v (ns-resolve
           'gesso.assurance.proof.xtdb-gateway-biff-parallel-multiroute-test
           symbol)]
    (when-not v
      (throw (ex-info "Missing original real-Biff test helper"
                      {:reason :missing-real-biff-helper :symbol symbol})))
    (var-get v)))

(defn- request [which] ((original 'req) which))
(defn- source-for-request [c request]
  ((original 'source-for-request) c request))
(defn- revision [before] (:revision (wc/physical before)))
(defn- rejections [f]
  (try
    (f)
    {:reason :not-rejected}
    (catch clojure.lang.ExceptionInfo e
      {:reason (:reason (ex-data e))
       :details (ex-data e)})))

(defn- expected-target
  "This expectation deliberately uses only independently seeded case identity
   and the Ring request. It never examines the candidate from source!. A
   production caller must establish the provenance of its own expectation."
  [c request]
  (let [claim? (= "/operations/example/claim" (:uri request))
        before (if claim? (:claim-before c) (:cancel-before c))]
    {:operation-id (if claim? multi/claim multi/cancel)
     :command-operation :claim
     :target [wc/table (:xt/id before)]}))

(defn- issue-case!
  "Issue and start a fresh mediator AFTER the common certified Biff startup.
   candidate! and expect! accept only c and the Ring request. A successful
   startup does not execute a transaction or grant generic XTDB authority."
  ([candidate! expect!] (issue-case! candidate! expect! nil))
  ([candidate! expect! policy]
   (let [c ((original 'build-case))
         source-calls (atom [])
         target-calls (atom [])
         sources {:per-operation-writer-sources
                  {multi/claim wc/*source*
                   multi/cancel (:writer-source multi/*second*)}}
         source! (fn [req]
                   (swap! source-calls conj (:uri req))
                   (candidate! c req))
         expected! (fn [req]
                     (swap! target-calls conj (:uri req))
                     (expect! c req))
         issued (if policy
                  (parallel/issue-target-bound-bounded-isolated-parallel-biff-request-module!
                   (:adoption c) (:source c) sources multi/writer-bindings
                   wc/*node* source! :revision policy expected!)
                  (parallel/issue-target-bound-isolated-parallel-biff-request-module!
                   (:adoption c) (:source c) sources multi/writer-bindings
                   wc/*node* source! :revision expected!))
         module (:module issued)]
     ((:biff.core/init module) nil)
     (assoc c
            :target-bound-issuance issued
            :target-bound-final ((:biff.core/start module) (:system c))
            :source-calls source-calls
            :target-calls target-calls))))

(defn- correct-case!
  ([] (correct-case! nil))
  ([policy]
   (issue-case! source-for-request expected-target policy)))

(defn- call! [c which]
  ((get (:target-bound-final c) app/biff-ring-handler-key)
   (request which)))

(defn- check-untouched! [c]
  (is (= 0 (revision (:claim-before c)))
      "A rejected command must not write the claim document")
  (is (= 0 (revision (:cancel-before c)))
      "A rejected command must not write the cancel document"))

(deftest installed-target-bound-report-remains-conditional
  (let [c (correct-case!)
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (get-in c [:target-bound-issuance :handle])
                (:source c) (:target-bound-final c))
        printed (pr-str report)]
    (is (= :required-before-each-request-writer-issuance
           (:operation-target-binding-policy report)))
    (is (= :independent-host-callback-not-authenticated
           (:expected-target-provenance report)))
    (is (= :not-established (:operation-refinement report)))
    (is (= :not-established (:actual-http-server-adoption report)))
    (is (= :not-established (:other-writer-closure report)))
    (is (= :none (:transaction-authority report)))
    (is (= :not-issued (:certificate report)))
    (is (not (str/includes? printed "private-v1549-sentinel")))
    (is (empty? @(:source-calls c)))
    (is (empty? @(:target-calls c)))
    (check-untouched! c)))

(deftest wrong-document-is-rejected-before-actual-xtdb-write
  (let [c (issue-case!
           (fn [case _] (wc/source (:cancel-before case)))
           expected-target)
        {:keys [reason details]} (rejections #(call! c :claim))]
    (is (= :command-target-disagrees-with-expectation reason))
    (is (true? (:xtdb-gateway-operation-target-binding/rejection details)))
    (is (= ["/operations/example/claim"] @(:source-calls c)))
    (is (= ["/operations/example/claim"] @(:target-calls c)))
    (is (not (str/includes? (pr-str details) "private-v1549-sentinel")))
    (check-untouched! c)))

(deftest matching-commands-commit-on-the-correct-distinct-documents
  (let [c (correct-case!)]
    (is (= 200 (:status (call! c :claim))))
    (is (= 1 (revision (:claim-before c))))
    (is (= 0 (revision (:cancel-before c))))
    (is (= 200 (:status (call! c :cancel))))
    (is (= 1 (revision (:claim-before c))))
    (is (= 1 (revision (:cancel-before c))))
    (is (= ["/operations/example/claim" "/operations/example/cancel"]
           @(:source-calls c)))
    (is (= @(:source-calls c) @(:target-calls c)))))

(deftest expected-target-cannot-be-quietly-substituted
  (let [c (issue-case!
           source-for-request
           (fn [case req]
             (assoc (expected-target case req)
                    :target [wc/table (:xt/id (:cancel-before case))])))]
    (is (= :command-target-disagrees-with-expectation
           (:reason (rejections #(call! c :claim)))))
    (check-untouched! c)))

(deftest wrong-route-operation-is-rejected
  (let [c (issue-case!
           source-for-request
           (fn [case req]
             (assoc (expected-target case req) :operation-id multi/cancel)))]
    (is (= :selected-operation-disagrees-with-expectation
           (:reason (rejections #(call! c :claim)))))
    (check-untouched! c)))

(deftest wrong-command-operation-is-rejected
  (let [c (issue-case!
           source-for-request
           (fn [case req]
             (assoc (expected-target case req) :command-operation :cancel)))]
    (is (= :command-operation-disagrees-with-expectation
           (:reason (rejections #(call! c :claim)))))
    (check-untouched! c)))

(deftest missing-or-malformed-expectation-is-rejected
  (doseq [expected! [(fn [_ _] nil)
                     (fn [case req] (dissoc (expected-target case req) :target))
                     (fn [case req] (assoc (expected-target case req) :extra true))]]
    (let [c (issue-case! source-for-request expected!)]
      (is (= :invalid-independent-physical-expectation
             (:reason (rejections #(call! c :claim)))))
      (check-untouched! c))))

(deftest thrown-target-builder-is-redacted-and-prevents-write
  (let [private-token "target-builder-private-test-secret"
        c (issue-case!
           source-for-request
           (fn [_ _]
             (throw (ex-info private-token {:secret private-token}))))
        {:keys [reason details]} (rejections #(call! c :claim))]
    (is (= :independent-target-derivation-failed reason))
    (is (not (str/includes? (pr-str details) private-token)))
    (check-untouched! c)))

(deftest protected-document-mismatch-is-rejected
  (let [c (issue-case!
           (fn [case req]
             (assoc-in (source-for-request case req)
                       [:protected-documents 0 :target]
                       [wc/table (:xt/id (:cancel-before case))]))
           expected-target)]
    (is (= :protected-document-disagrees-with-expectation
           (:reason (rejections #(call! c :claim)))))
    (check-untouched! c)))

(deftest unrelated-route-does-not-build-or-issue-a-writer
  (let [c (correct-case!)
        handler (get (:target-bound-final c) app/biff-ring-handler-key)]
    (doseq [req [{:request-method :get :uri "/operations/example/claim"}
                 {:request-method :post :uri "/operations/example/missing"}
                 nil]]
      (is (= :unmatched-parallel-request-route
             (:reason (rejections #(handler req))))))
    (is (empty? @(:source-calls c)))
    (is (empty? @(:target-calls c)))
    (check-untouched! c)))

(deftest bounded-target-bound-variant-also-rejects-substitution
  (let [policy {:max-active-requests 2
                :max-pending-requests 2
                :wait-timeout-ms 1000}
        c (issue-case!
           (fn [case _] (wc/source (:cancel-before case)))
           expected-target policy)
        report (parallel/require-current-isolated-parallel-biff-request-module!
                (get-in c [:target-bound-issuance :handle])
                (:source c) (:target-bound-final c))]
    (is (= :required-before-each-request-writer-issuance
           (:operation-target-binding-policy report)))
    (is (= :bounded-fair-permits-with-request-local-registries
           (:parallel-admission-policy report)))
    (is (= :command-target-disagrees-with-expectation
           (:reason (rejections #(call! c :claim)))))
    (check-untouched! c)))

(deftest bounded-target-bound-variant-can-commit-valid-command
  (let [policy {:max-active-requests 2
                :max-pending-requests 2
                :wait-timeout-ms 1000}
        c (correct-case! policy)]
    (is (= 200 (:status (call! c :cancel))))
    (is (= 0 (revision (:claim-before c))))
    (is (= 1 (revision (:cancel-before c))))
    (is (= ["/operations/example/cancel"] @(:source-calls c)))
    (is (= @(:source-calls c) @(:target-calls c)))))

(ns gesso.assurance.proof.physical-writer-inventory-test
  "v1490: permanent adversarial checks for v1489's process-local writer survey.

   The source fixture is the genuine v1367 independent induction and issued
   application construction. Its checked application is not replaced by a
   fabricated successful coverage report. Host writer tables are controlled
   test inputs, NEVER asserted to be exhaustive or physically sink-bound.

   Tests distinguish an honestly observed raw writer from an omitted or
   intentionally misclassified one. The latter two cannot be discovered by
   this API; even a clean identifier join never grants certification."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.physical-writer-inventory :as inventory]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as upstream]
            [gesso.assurance.proof.operation-writer-coverage-soundness-test]))

(def ^:private operation :example/claim)
(def ^:private primary-path :host/claim)
(def ^:dynamic *real-operation-source* nil)

(defn- fixture-var [symbol]
  (or (ns-resolve
       'gesso.assurance.proof.operation-writer-coverage-soundness-test
       symbol)
      (throw (ex-info "Required existing source-backed test fixture missing"
                      {:symbol symbol}))))

(use-fixtures
  :once
  (fn [run]
    (let [prior (-> (the-ns 'gesso.assurance.proof.operation-writer-coverage-soundness-test)
                    meta :clojure.test/once-fixtures first)]
      (when-not (fn? prior)
        (throw (ex-info "Expected source-backed v1367 fixture" {})))
      (prior (fn []
               (let [actual (var-get (fixture-var '*source*))]
                 (when-not (and (map? actual)
                                (= #{:induction-catalog :application-induction-source}
                                   (set (keys actual))))
                   (throw (ex-info "Unrecognized source-backed fixture" {})))
                 (binding [*real-operation-source* actual]
                   (run))))))))

(defn- writer
  ([] (writer (atom 0)))
  ([calls] (fn [& _] (swap! calls inc) :unexpected-invocation)))

(defn- issued
  ([f] (issued operation f))
  ([op f] {:kind :issued-operation :operation-id op :writer f}))

(defn- other [kind f]
  {:kind kind :operation-id nil :writer f})

(defn- host-source [enumerator]
  {:operation-writer-source *real-operation-source*
   :enumerate-write-paths! enumerator})

(defn- enumerated [paths]
  (host-source (fn [] paths)))

(defn- clean-source []
  (enumerated {primary-path (issued (writer))}))

(defn- observe [source]
  (inventory/describe-physical-writer-inventory! source))

(defn- fail [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-writer-inventory/rejection (ex-data e))
        (:reason (ex-data e))
        [:unexpected-exception-info (select-keys (ex-data e) [:reason :at])]))
    (catch Throwable e
      [:unexpected-exception (.getName (class e))])))

(defn- rejects! [expected f]
  (is (= expected (fail f)) (str "Expected physical writer rejection " expected)))

(defn- no-authority! [report]
  (doseq [[k expected]
          [[:host-writer-enumerator-exhaustiveness :not-established]
           [:writer-function-and-physical-sink-correspondence :not-established]
           [:complete-physical-writer-coverage :not-established]
           [:deployment-correspondence :not-established]
           [:physical-refinement :not-established]
           [:certificate :not-issued]
           [:admission :not-admitted]
           [:transaction-authority :none]
           [:authority :none]]]
    (is (= expected (get report k)) (str "Must not promote " k)))
  (is (every? (:open-obligations report)
              [:host-enumerator-exhaustiveness-and-stability
               :all-host-processes-and-external-writers-accounted-for
               :undeclared-and-raw-xtdb-bypasses-excluded
               :runtime-capability-identity-and-deployment
               :physical-execution-and-abstract-transition-correspondence
               :protected-domain-write-completeness
               :transaction-internal-prestate-and-guard-correspondence
               :universal-refinement-and-certification])))

(deftest genuine-issued-source-has-only-conditional-join
  (let [calls (atom 0)
        report (observe (enumerated {primary-path (issued (writer calls))}))]
    (is (= inventory/inventory-type (:type report)))
    (is (= inventory/inventory-version (:version report)))
    (is (= :conditional-observed-writer-path-id-join (:status report)))
    (is (:declared-to-observed-issued-ids-match? report))
    (is (= #{operation} (:independent-operation-ids report)))
    (is (= #{operation} (:issued-operation-ids report)))
    (is (= #{operation} (:observed-issued-operation-ids report)))
    (is (= #{primary-path} (:observed-writer-path-ids report)))
    (is (empty? (:observed-nonissued-writer-path-ids report)))
    (is (= :not-established
           (-> report :writer-rows first :physical-sink-binding)))
    (is (not (contains? (first (:writer-rows report)) :writer)))
    (is (= 0 @calls) "A writer callback must never be invoked by the survey")
    (no-authority! report)))

(deftest every-nonissued-kind-remains-open-and-visible
  (doseq [kind [:raw-xtdb :initializer :background-job :external-writer
                :other-writer :unknown]]
    (let [path (keyword "host" (name kind))
          report (observe (enumerated {primary-path (issued (writer))
                                       path (other kind (writer))}))]
      (is (= :open-nonissued-physical-write-paths (:status report)) (str kind))
      (is (= #{path} (:observed-nonissued-writer-path-ids report)) (str kind))
      (is (= #{path} (get (:observed-nonissued-paths-by-kind report) kind)))
      (is (contains? (:open-obligations report)
                     :observed-nonissued-writers-require-accounting))
      (is (:declared-to-observed-issued-ids-match? report)
          "Matching issued IDs do not erase observed bypasses")
      (no-authority! report))))

(deftest multiple-realistic-writer-kinds-are-not-conflated
  (let [paths {primary-path (issued (writer))
               :host/direct (other :raw-xtdb (writer))
               :host/setup (other :initializer (writer))
               :host/timer (other :background-job (writer))
               :host/remote (other :external-writer (writer))}
        report (observe (enumerated paths))]
    (is (= :open-nonissued-physical-write-paths (:status report)))
    (is (= (set (keys paths)) (:observed-writer-path-ids report)))
    (is (= #{:host/direct :host/setup :host/timer :host/remote}
           (:observed-nonissued-writer-path-ids report)))
    (is (= #{:host/setup}
           (get (:observed-nonissued-paths-by-kind report) :initializer)))
    (is (= #{:host/timer}
           (get (:observed-nonissued-paths-by-kind report) :background-job)))
    (is (= #{:host/remote}
           (get (:observed-nonissued-paths-by-kind report) :external-writer)))
    (no-authority! report)))

(deftest missing-issued-writer-remains-open-even-with-clean-source
  (let [report (observe (enumerated {}))]
    (is (= :open-observed-issued-writer-mismatch (:status report)))
    (is (= #{operation} (:missing-observed-issued-operation-ids report)))
    (is (empty? (:foreign-observed-issued-operation-ids report)))
    (is (false? (:declared-to-observed-issued-ids-match? report)))
    (is (contains? (:open-obligations report)
                   :declared-issued-writers-not-observed))
    (no-authority! report)))

(deftest foreign-writer-cannot-borrow-issued-status
  (let [report (observe (enumerated {primary-path
                                    (issued :example/not-admitted (writer))}))]
    (is (= :open-observed-issued-writer-mismatch (:status report)))
    (is (= #{operation} (:missing-observed-issued-operation-ids report)))
    (is (= #{:example/not-admitted}
           (:foreign-observed-issued-operation-ids report)))
    (is (contains? (:open-obligations report)
                   :observed-issued-writers-not-declared))
    (no-authority! report)))

(deftest duplicate-host-registrations-are-not-mistaken-for-exhaustiveness
  (let [report (observe (enumerated {primary-path (issued (writer))
                                    :host/claim-alias (issued (writer))}))]
    (is (= :conditional-observed-writer-path-id-join (:status report)))
    (is (= #{primary-path :host/claim-alias} (:observed-writer-path-ids report)))
    (is (= #{operation} (:observed-issued-operation-ids report)))
    (is (= 2 (count (:writer-rows report))))
    (no-authority! report)))

(deftest hidden-host-writer-is-invisible-to-a-nonexhaustive-enumerator
  (let [hidden-writers (atom {:host/secret (other :raw-xtdb (writer))})
        source (clean-source)
        report (observe source)]
    (is (= :conditional-observed-writer-path-id-join (:status report)))
    (swap! hidden-writers assoc :host/second-secret
           (other :background-job (writer)))
    (is (= report (inventory/require-current-physical-writer-inventory!
                   report source))
        "Identical surveyed paths cannot detect an unseen external registry")
    (is (= 2 (count @hidden-writers)))
    (no-authority! report)))

(deftest intentionally-misclassified-callable-is-not-authenticated
  (let [raw-call-count (atom 0)
        ;; The host lies: this callable is not actually an issued executor.
        source (enumerated {primary-path (issued (writer raw-call-count))})
        report (observe source)]
    (is (= :conditional-observed-writer-path-id-join (:status report)))
    (is (= 0 @raw-call-count))
    (is (= :not-established
           (:writer-function-and-physical-sink-correspondence report)))
    (no-authority! report)))

(deftest writer-function-substitution-is-outside-descriptor-recheck
  (let [actual-calls (atom 0)
        table (atom {primary-path (issued (writer actual-calls))})
        source (host-source (fn [] @table))
        before (observe source)]
    (swap! table assoc primary-path (issued (writer actual-calls)))
    (is (= before (inventory/require-current-physical-writer-inventory!
                   before source))
        "Callable identity is intentionally absent from report rows")
    (is (= 0 @actual-calls))
    (no-authority! before)))

(deftest survey-is-fresh-and-never-runs-the-writers
  (let [enumerations (atom 0)
        writer-calls (atom 0)
        table (atom {primary-path (issued (writer writer-calls))})
        source (host-source (fn [] (swap! enumerations inc) @table))
        before (observe source)]
    (is (= 1 @enumerations))
    (is (= 0 @writer-calls))
    (is (= before (inventory/require-current-physical-writer-inventory!
                   before source)))
    (is (= 2 @enumerations))
    (swap! table assoc :host/raw (other :raw-xtdb (writer writer-calls)))
    (rejects! :stale-or-substituted-writer-inventory
              #(inventory/require-current-physical-writer-inventory!
                before source))
    (is (= 3 @enumerations))
    (is (= 0 @writer-calls))
    (no-authority! (observe source))))

(deftest changed-paths-or-writer-classification-invalidate-a-copied-report
  (let [table (atom {primary-path (issued (writer))})
        source (host-source (fn [] @table))
        old (observe source)]
    (swap! table assoc :host/raw (other :raw-xtdb (writer)))
    (rejects! :stale-or-substituted-writer-inventory
              #(inventory/require-current-physical-writer-inventory! old source))
    (let [with-raw (observe source)]
      (swap! table assoc :host/raw (other :initializer (writer)))
      (rejects! :stale-or-substituted-writer-inventory
                #(inventory/require-current-physical-writer-inventory!
                  with-raw source)))
    (reset! table {primary-path (issued (writer))})
    (is (= old (inventory/require-current-physical-writer-inventory!
                 old source)))
    (no-authority! old)))

(deftest copied-or-upgraded-evidence-cannot-be-rechecked
  (let [source (clean-source)
        report (observe source)]
    (doseq [altered [(assoc report :status :certified)
                    (assoc report :complete-physical-writer-coverage :established)
                    (assoc report :host-writer-enumerator-exhaustiveness :established)
                    (assoc report :certificate :issued)
                    (assoc report :admission :admitted)
                    (assoc report :physical-refinement :proved)
                    (assoc report :open-obligations #{})
                    (dissoc report :physical-refinement)
                    (assoc report :surprise :new-field)
                    (with-meta report {:forged true})
                    [] nil]]
      (rejects! :stale-or-substituted-writer-inventory
                #(inventory/require-current-physical-writer-inventory!
                  altered source)))
    (no-authority! report)))

(deftest malformed-inventory-sources-reject-before-calling-enumerator
  (let [calls (atom 0)
        source (host-source (fn [] (swap! calls inc) {}))]
    (doseq [invalid [nil [] {} {:operation-writer-source *real-operation-source*}
                     (assoc source :unexpected true)
                     (dissoc source :operation-writer-source)
                     (with-meta source {:forged true})]]
      (rejects! :invalid-map-shape #(observe invalid)))
    (is (= 0 @calls))
    (rejects! :invalid-host-enumerator
              #(observe (assoc source :enumerate-write-paths! :not-callable)))
    (is (= 0 @calls))))

(deftest unsupported-or-malformed-enumerated-writer-tables-are-rejected
  (let [f (writer)]
    (doseq [table [nil [] #{:host/write} {:bare-id (issued f)}
                   {"host/write" (issued f)}
                   (with-meta {primary-path (issued f)} {:forge true})
                   (into {} (for [i (range 513)]
                              [(keyword "host" (str "writer-" i)) (issued f)]))]]
      (rejects! :invalid-host-writer-table #(observe (enumerated table))))))

(deftest writer-row-schemas-and-kinds-must-be-exact
  (let [f (writer)
        baseline (issued f)]
    (doseq [[row reason]
            [[nil :invalid-map-shape]
             [[] :invalid-map-shape]
             [(dissoc baseline :writer) :invalid-map-shape]
             [(assoc baseline :extra true) :invalid-map-shape]
             [(with-meta baseline {:forged true}) :invalid-map-shape]
             [(assoc baseline :kind :unknown-to-gesso) :unsupported-writer-kind]
             [(assoc baseline :writer :fake-function) :noncallable-writer]
             [(assoc baseline :writer nil) :noncallable-writer]
             [(assoc baseline :operation-id nil) :missing-issued-operation-id]
             [(assoc baseline :operation-id :unqualified) :missing-issued-operation-id]
             [(other :raw-xtdb f) nil]]]
      (if reason
        (rejects! reason #(observe (enumerated {primary-path row})))
        (is (= :open-observed-issued-writer-mismatch
               (:status (observe (enumerated {primary-path row})))))))))

(deftest nonissued-writers-cannot-claim-an-issued-operation-id
  (doseq [kind [:raw-xtdb :initializer :background-job :external-writer
                :other-writer :unknown]]
    (rejects! :nonissued-writer-claims-operation
              #(observe (enumerated {primary-path
                                     (assoc (other kind (writer))
                                            :operation-id operation)})))))

(deftest missing-or-invalid-registered-issued-id-rejects
  (doseq [id [nil :claim "example/claim" 123 {}]]
    (rejects! :missing-issued-operation-id
              #(observe (enumerated {primary-path (issued id (writer))})))))

(deftest substituted-upstream-coverage-checker-is-detected
  (let [source (clean-source)]
    (with-redefs [upstream/derive-observed-writer-coverage!
                  (fn [_] {:type upstream/report-type :status :certified})]
      (rejects! :substituted-source-checker #(observe source)))
    (is (= :conditional-observed-writer-path-id-join
           (:status (observe source))))))

(deftest unexpected-upstream-source-is-not-launched-as-physical-evidence
  (let [source (clean-source)
        changed (assoc-in source [:operation-writer-source :induction-catalog]
                          {:id :invalid/replacement})]
    ;; The induction layer itself should reject the bad catalog; no writer
    ;; classification or certificate can be obtained from invalid inputs.
    (is (not= :not-rejected (fail #(observe changed))))
    (no-authority! (observe source))))

(ns gesso.assurance.proof.xtdb-gateway-writer-coverage-test
  "v1550: permanent adversarial host-observed writer-registration coverage.
   Genuine source-derived operation inventory and embedded XTDB are used.
   Callback identity is checked locally, not deployment wide. A clean report
   MUST NOT be interpreted as complete writer closure, route invocation,
   abstract command equivalence, or physical refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as coverage]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution :as monotonic]
            [gesso.assurance.proof.operation-writer-coverage-soundness-test]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:dynamic *source* nil)
(def ^:dynamic *node* nil)
(def op :example/claim)
(def path :host/claim)
(def raw :host/raw)
(def table :gesso_v1549_coverage)
(def version {:revision-key :revision :created-at-key :created-at
              :updated-at-key :updated-at})

(use-fixtures :once
  (fn [run]
    (let [fixture (-> (the-ns 'gesso.assurance.proof.operation-writer-coverage-soundness-test)
                      meta :clojure.test/once-fixtures first)]
      (fixture
       (fn []
         (with-open [node (xtn/start-node {})]
           (binding [*node* node
                     *source* (var-get (ns-resolve
                                        'gesso.assurance.proof.operation-writer-coverage-soundness-test
                                        '*source*))]
             (run))))))))

(defn before []
  {:xt/id (str (random-uuid)) :revision 0 :created-at "created"
   :updated-at "before" :owner "open" :secret "private-v1549-sentinel"})
(defn after [b]
  (-> b (assoc :revision 1 :updated-at "after" :owner "claimed")))
(defn source [b]
  {:plan {:commands [(command/update-command table :claim b (after b) version)]
          :emit false}
   :protected-documents [{:target [table (:xt/id b)] :fields :all}]})
(defn seeded []
  (let [b (before)]
    (xt/execute-tx *node* [[:put-docs table b]]) b))
(defn physical [b]
  (first (xt/q *node* ["SELECT * FROM gesso_v1549_coverage WHERE _id = ?" (:xt/id b)])))
(defn issue [b]
  (coverage/issue-monotonic-registered-writer! path op *node* (source b) :revision))
(defn inv [host]
  {:operation-writer-source *source*
   :enumerate-write-paths! (fn [] @host)})
(defn registered [writer]
  {:kind :issued-operation :operation-id op :writer writer})
(defn invalid [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) :other-ex-info))))

(deftest a-exact-registered-writer-is-conditional-not-certified
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path (registered writer)})
        report (coverage/require-registered-writer-coverage! (inv host) {path handle})]
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots (:status report)))
    (is (true? (:all-currently-surveyed-writers-locally-bound? report)))
    (is (= 1 (:observed-writer-count report) (:matched-issued-monotonic-writer-count report)))
    (doseq [k [:all-xtdb-writers-mediated :writer-history-monotonicity
               :host-enumerator-exhaustiveness :physical-refinement]]
      (is (= :not-established (get report k))))
    (is (= :not-issued (:certificate report)))
    (is (= b (physical b)) "inspection does not run registered writer")
    (is (= :registered-writer-requires-zero-arguments (invalid #(writer :any))))
    (is (= b (physical b)))
    (is (map? (writer)))
    (is (= (after b) (physical b)))
    (is (= :one-shot-already-attempted (invalid writer)))
    (is (not (str/includes? (pr-str report) "private-v1549-sentinel")))))

(deftest b-raw-outside-writer-makes-coverage-open
  (let [b (seeded) {:keys [handle writer]} (issue b)
        other (fn [] :raw-writer)
        host (atom {path (registered writer)
                    raw {:kind :raw-xtdb :operation-id nil :writer other}})
        report (coverage/inspect-registered-writer-coverage! (inv host) {path handle})]
    (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))
    (is (= 1 (:nonissued-writer-count report)))
    (is (= 1 (:unbound-or-foreign-writer-count report)))
    (is (= :unverified-or-unaccounted-registered-writers
           (invalid #(coverage/require-registered-writer-coverage! (inv host) {path handle}))))))

(deftest c-callback-wrapper-is-not-originally-issued
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path (registered (fn [] (writer)))})
        report (coverage/inspect-registered-writer-coverage! (inv host) {path handle})]
    (is (= 0 (:matched-issued-monotonic-writer-count report)))
    (is (= 1 (:unbound-or-foreign-writer-count report)))))

(deftest d-copied-or-foreign-handle-is-not-accepted
  (let [b (seeded) a (issue b)
        b2 (seeded) other (issue b2)
        host (atom {path (registered (:writer a))})]
    (doseq [handles [{path (Object.)} {path (:handle other)} {}]]
      (is (= 1 (:unbound-or-foreign-writer-count
                (coverage/inspect-registered-writer-coverage! (inv host) handles)))))
    (is (= :invalid-issued-writer-handle-table
           (invalid #(coverage/inspect-registered-writer-coverage! (inv host) []))))))

(deftest e-extra-handle-is-open
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path (registered writer)})
        report (coverage/inspect-registered-writer-coverage! (inv host)
                                                            {path handle :host/ghost handle})]
    (is (= 1 (:issued-handle-not-registered-count report)))
    (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))))

(deftest f-host-registration-change-detected
  (let [b (seeded) {:keys [handle writer]} (issue b)
        other (fn [] :replaced)
        counter (atom 0)
        src {:operation-writer-source *source*
             :enumerate-write-paths!
             (fn [] {path (registered (if (>= (swap! counter inc) 3)
                                         other writer))})}]
    ;; Underlying inventory: call 1; repeated local observation: call 2, 3.
    (is (= :unstable-or-inconsistent-host-registrations
           (invalid #(coverage/inspect-registered-writer-coverage!
                      src {path handle}))))))

(deftest g-misclassified-operation-id-is-not-covered
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path {:kind :issued-operation
                          :operation-id :example/unknown
                          :writer writer}})
        report (coverage/inspect-registered-writer-coverage! (inv host) {path handle})]
    (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))
    (is (false? (:independent-issued-operation-ids-match? report)))
    (is (= 1 (:unbound-or-foreign-writer-count report)))))

(deftest h-foreign-direct-writer-unseen-by-enumerator-remains-unproven
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path (registered writer)})
        report (coverage/require-registered-writer-coverage! (inv host) {path handle})]
    (xt/execute-tx *node* [[:put-docs table (assoc b :revision 7)]])
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots (:status report)))
    (is (= 7 (:revision (physical b))))
    (is (= :not-established (:all-xtdb-writers-mediated report)))))

(deftest i-issuer-seam-replacement-rejected
  (with-redefs [monotonic/prepare-monotonic-revision-one-shot!
                (fn [& _] (throw (ex-info "should never run" {})))]
    (is (= :substituted-issuer-or-inventory
           (invalid #(coverage/issue-monotonic-registered-writer!
                      path op *node* {} :revision))))))

(deftest j-replaced-table-after-check-does-not-grant-deployment-coverage
  (let [b (seeded) {:keys [handle writer]} (issue b)
        host (atom {path (registered writer)})]
    (is (true? (:all-currently-surveyed-writers-locally-bound?
                (coverage/inspect-registered-writer-coverage! (inv host) {path handle}))))
    (reset! host {raw {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)}})
    (is (= :open-unverified-or-unaccounted-writer-paths
           (:status (coverage/inspect-registered-writer-coverage! (inv host) {path handle}))))))


(defn clean-context
  "Seed a genuine XTDB document, issue a guarded callback and register it."
  []
  (let [b (seeded)
        issued (issue b)
        host (atom {path (registered (:writer issued))})]
    {:before b :issued issued :host host}))

(defn- assert-open-boundary! [report]
  (is (= coverage/report-type (:type report)))
  (is (= coverage/report-version (:version report)))
  (doseq [k [:host-enumerator-exhaustiveness
             :actual-handler-calls-registered-callback
             :all-xtdb-writers-mediated
             :writer-history-monotonicity
             :declared-operation-to-prepared-command-semantics
             :physical-refinement]]
    (is (= :not-established (get report k)) (str "Unproven: " k)))
  (doseq [[k expected] [[:certificate :not-issued]
                        [:admission :not-admitted]
                        [:authority :none]
                        [:transaction-authority :none]
                        [:proof-status :conditional-process-local-writer-registration-only]]]
    (is (= expected (get report k)) (str "No authority: " k)))
  (is (contains? (:open-obligations report)
                 :deployed-route-invokes-the-registered-writer))
  (is (contains? (:open-obligations report)
                 :other-processes-and-external-xtdb-clients))
  (is (contains? (:open-obligations report)
                 :host-operation-id-matches-prepared-command-semantics))
  (is (not-any? #(contains? report %)
                [:node :plan :tx-ops :tx-key :before :after :writer
                 :handle :registry :physical-document :snapshot-token]))
  (is (not (str/includes? (pr-str report) "private-v1549-sentinel"))))

(deftest clean-survey-preserves-all-nonclaims-and-performs-no-write
  (let [{:keys [before issued host]} (clean-context)
        report (coverage/require-registered-writer-coverage!
                (inv host) {path (:handle issued)})]
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots
           (:status report)))
    (is (= :rederived-operation-sources-and-fresh-host-callback-identities
           (:checking report)))
    (is (= :current-process-enumerated-registered-writers-only (:scope report)))
    (is (= :exact-local-zero-argument-one-shot-monotonic-gateway
           (:registered-callback-execution-policy report)))
    (is (= 1 (:observed-writer-count report)))
    (is (= 1 (:matched-issued-monotonic-writer-count report)))
    (is (zero? (:unbound-or-foreign-writer-count report)))
    (assert-open-boundary! report)
    (is (= before (physical before)))))

(deftest wrong-callback-in-the-same-host-slot-is-open
  (let [{:keys [issued host]} (clean-context)
        other (fn [] :untracked)]
    (swap! host assoc path (registered other))
    (let [report (coverage/inspect-registered-writer-coverage!
                  (inv host) {path (:handle issued)})]
      (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))
      (is (= 1 (:unbound-or-foreign-writer-count report)))
      (is (zero? (:matched-issued-monotonic-writer-count report)))
      (assert-open-boundary! report))))

(deftest same-issuer-two-callbacks-never-authorize-each-other
  (let [b (seeded) old (issue b) current (issue b)
        host (atom {path (registered (:writer current))})]
    (is (= 1 (:unbound-or-foreign-writer-count
              (coverage/inspect-registered-writer-coverage!
               (inv host) {path (:handle old)}))))
    (is (true? (:all-currently-surveyed-writers-locally-bound?
                (coverage/inspect-registered-writer-coverage!
                 (inv host) {path (:handle current)}))))
    (is (= b (physical b)))))

(deftest synthetic-writer-handle-is-not-issued
  (let [{:keys [issued host]} (clean-context)
        synthetic (coverage/->WriterHandle)]
    (is (not (identical? synthetic (:handle issued))))
    (is (= 1 (:unbound-or-foreign-writer-count
              (coverage/inspect-registered-writer-coverage!
               (inv host) {path synthetic}))))))

(deftest registering-the-same-writer-at-different-path-does-not-match
  (let [{:keys [issued host]} (clean-context)]
    (reset! host {:host/different (registered (:writer issued))})
    (let [report (coverage/inspect-registered-writer-coverage!
                  (inv host) {:host/different (:handle issued)})]
      (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))
      (is (= 1 (:unbound-or-foreign-writer-count report)))
      (assert-open-boundary! report))))

(deftest two-distinct-issued-paths-can-match-one-declared-operation
  (let [b (seeded)
        a (issue b)
        other-path :host/claim-alias
        alias (coverage/issue-monotonic-registered-writer!
               other-path op *node* (source b) :revision)
        host (atom {path (registered (:writer a))
                    other-path (registered (:writer alias))})
        report (coverage/inspect-registered-writer-coverage!
                (inv host) {path (:handle a) other-path (:handle alias)})]
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots
           (:status report)))
    (is (= 2 (:observed-writer-count report)
           (:matched-issued-monotonic-writer-count report)))
    (is (= b (physical b)))
    (assert-open-boundary! report)))

(deftest an-alias-cannot-share-a-handle-even-if-its-writer-is-identical
  (let [{:keys [issued host]} (clean-context)]
    (swap! host assoc :host/alias (registered (:writer issued)))
    (let [report (coverage/inspect-registered-writer-coverage!
                  (inv host) {path (:handle issued)
                              :host/alias (:handle issued)})]
      (is (= 2 (:observed-writer-count report)))
      (is (= 1 (:matched-issued-monotonic-writer-count report)))
      (is (= 1 (:unbound-or-foreign-writer-count report)))
      (assert-open-boundary! report))))

(deftest no-registered-handles-can-be-accepted-even-with-genuine-host-callback
  (let [{:keys [issued host]} (clean-context)
        report (coverage/inspect-registered-writer-coverage! (inv host) {})]
    (is (= 1 (:unbound-or-foreign-writer-count report)))
    (is (= :unverified-or-unaccounted-registered-writers
           (invalid #(coverage/require-registered-writer-coverage!
                      (inv host) {}))))
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots
           (:status (coverage/inspect-registered-writer-coverage!
                     (inv host) {path (:handle issued)}))))))

(deftest foreign-path-handle-count-is-reported-separately
  (let [{:keys [issued host]} (clean-context)
        report (coverage/inspect-registered-writer-coverage!
                (inv host) {path (:handle issued)
                            :host/phantom (:handle issued)})]
    (is (= 1 (:issued-handle-not-registered-count report)))
    (is (zero? (:unbound-or-foreign-writer-count report)))
    (is (= :open-unverified-or-unaccounted-writer-paths (:status report)))))

(deftest all-physical-writer-kinds-other-than-issued-are-open
  (doseq [kind [:raw-xtdb :initializer :background-job :external-writer
                :other-writer :unknown]]
    (let [{:keys [issued host]} (clean-context)]
      (swap! host assoc :host/outside
             {:kind kind :operation-id nil :writer (fn [] :outside)})
      (let [report (coverage/inspect-registered-writer-coverage!
                    (inv host) {path (:handle issued)})]
        (is (= 1 (:nonissued-writer-count report)) (str kind))
        (is (= :open-unverified-or-unaccounted-writer-paths (:status report))
            (str kind))))))

(deftest host-enumerator-exception-after-upstream-snapshot-fails-closed
  (let [{:keys [issued host]} (clean-context)
        calls (atom 0)
        enumeration (fn [] (if (= 2 (swap! calls inc))
                             (throw (ex-info "private-enumerator-crash" {}))
                             @host))]
    (is (= :host-enumeration-failed
           (invalid #(coverage/inspect-registered-writer-coverage!
                      {:operation-writer-source *source*
                       :enumerate-write-paths! enumeration}
                      {path (:handle issued)}))))))

(deftest host-table-mutated-between-second-and-third-snapshot-fails-closed
  (let [{:keys [issued host]} (clean-context)
        calls (atom 0)
        enum! (fn []
                (if (= 3 (swap! calls inc))
                  (assoc @host :host/late
                         {:kind :raw-xtdb :operation-id nil :writer (fn [] nil)})
                  @host))]
    (is (= :unstable-or-inconsistent-host-registrations
           (invalid #(coverage/inspect-registered-writer-coverage!
                      {:operation-writer-source *source*
                       :enumerate-write-paths! enum!}
                      {path (:handle issued)}))))))

(deftest toggling-classification-during-local-survey-is-rejected
  (let [{:keys [issued host]} (clean-context)
        calls (atom 0)
        enum! (fn []
                (if (= 3 (swap! calls inc))
                  (assoc @host path {:kind :raw-xtdb :operation-id nil
                                     :writer (:writer issued)})
                  @host))]
    (is (= :unstable-or-inconsistent-host-registrations
           (invalid #(coverage/inspect-registered-writer-coverage!
                      {:operation-writer-source *source*
                       :enumerate-write-paths! enum!}
                      {path (:handle issued)}))))))

(deftest malformed-host-enumerator-shapes-never-produce-clean-evidence
  (let [{:keys [issued host]} (clean-context)]
    (doseq [bad [nil [] '() (with-meta @host {:tampered true})
                 {path {:kind :issued-operation :operation-id op}}
                 {path (assoc (registered (:writer issued)) :extra :untrusted)}
                 {:unqualified (registered (:writer issued))}]]
      (is (not= :no-error
                (invalid #(coverage/inspect-registered-writer-coverage!
                           {:operation-writer-source *source*
                            :enumerate-write-paths! (constantly bad)}
                           {path (:handle issued)})))
          (str "Host table rejected: " (type bad))))))

(deftest invalid-handle-table-shapes-fail-before-inventory-enumeration
  (let [{:keys [issued host]} (clean-context)
        source (inv host)]
    (doseq [bad [nil [] '() #{path} (with-meta {path (:handle issued)} {:x 1})
                 {:unqualified (:handle issued)}
                 (into {} (for [i (range 513)]
                            [(keyword "host" (str "extra-" i)) (:handle issued)]))]]
      (is (= :invalid-issued-writer-handle-table
             (invalid #(coverage/inspect-registered-writer-coverage! source bad)))))))

(deftest invalid-path-and-operation-ids-rejected-before-gateway-preparation
  (let [b (before)]
    ;; No prestate exists: acceptance would otherwise fail at XTDB acquisition.
    (doseq [[p o] [[nil op] [:not-qualified op] ["host/claim" op]
                   [path nil] [path :not-qualified] [path "example/claim"]]]
      (is (= :invalid-path-or-operation-id
             (invalid #(coverage/issue-monotonic-registered-writer!
                        p o *node* (source b) :revision)))))))

(deftest copied-callback-wrapper-cannot-borrow-issued-provenance
  (let [{:keys [issued host]} (clean-context)
        genuine (:writer issued)
        copy (fn [& args] (apply genuine args))]
    (reset! host {path (registered copy)})
    (is (= 1 (:unbound-or-foreign-writer-count
              (coverage/inspect-registered-writer-coverage!
               (inv host) {path (:handle issued)}))))
    (is (not (identical? genuine copy)))))

(deftest inspection-does-not-consume-a-one-shot-writer
  (let [{:keys [before issued host]} (clean-context)
        handles {path (:handle issued)}
        src (inv host)]
    (dotimes [_ 2]
      (is (true? (:all-currently-surveyed-writers-locally-bound?
                  (coverage/inspect-registered-writer-coverage! src handles))))
      (is (= before (physical before))))
    (is (= :registered-writer-requires-zero-arguments
           (invalid #((:writer issued) :bad-arg))))
    (is (= before (physical before))
        "Invalid arity must not consume the one-shot permit")
    (is (map? ((:writer issued))))
    (is (= (after before) (physical before)))
    (is (= :one-shot-already-attempted (invalid (:writer issued))))))

(deftest registration-report-does-not-imply-callback-still-has-unused-permit
  (let [{:keys [before issued host]} (clean-context)]
    (is (map? ((:writer issued))))
    (is (= (after before) (physical before)))
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots
           (:status (coverage/inspect-registered-writer-coverage!
                     (inv host) {path (:handle issued)}))))
    (is (= :one-shot-already-attempted (invalid (:writer issued))))
    ;; Registration is a provenance property, not readiness or liveness.
    (is (= :not-established
           (:actual-handler-calls-registered-callback
            (coverage/inspect-registered-writer-coverage!
             (inv host) {path (:handle issued)}))))))

(deftest direct-writer-can-change-physical-state-despite-clean-local-report
  (let [{:keys [before issued host]} (clean-context)
        report (coverage/require-registered-writer-coverage!
                (inv host) {path (:handle issued)})]
    (xt/execute-tx *node* [[:put-docs table (assoc before :revision 300)]])
    (is (= 300 (:revision (physical before))))
    (is (= :conditional-all-surveyed-writers-issued-monotonic-one-shots
           (:status report)))
    (is (= :not-established (:all-xtdb-writers-mediated report)))))

(deftest host-can-omit-a-real-writer-and-still-produce-only-local-coverage
  (let [{:keys [issued host]} (clean-context)
        hidden (atom {:host/unreported (fn [] :untracked)})
        report (coverage/require-registered-writer-coverage!
                (inv host) {path (:handle issued)})]
    (swap! hidden assoc :host/even-more-unreported (fn [] :untracked-too))
    (is (= 2 (count @hidden)))
    (is (true? (:all-currently-surveyed-writers-locally-bound? report)))
    (is (= :not-established (:host-enumerator-exhaustiveness report)))
    (is (contains? (:open-obligations report)
                   :host-enumerator-exhaustiveness-and-post-inspection-stability))))

(deftest post-survey-host-change-requires-fresh-inspection
  (let [{:keys [issued host]} (clean-context)
        handles {path (:handle issued)}
        old (coverage/inspect-registered-writer-coverage! (inv host) handles)]
    (swap! host assoc path (registered (fn [] :replacement)))
    (is (true? (:all-currently-surveyed-writers-locally-bound? old)))
    (is (false? (:all-currently-surveyed-writers-locally-bound?
                 (coverage/inspect-registered-writer-coverage! (inv host) handles))))
    (is (= :unverified-or-unaccounted-registered-writers
           (invalid #(coverage/require-registered-writer-coverage!
                      (inv host) handles))))))

(deftest issuer-and-inventory-var-seams-both-fail-closed
  (let [{:keys [issued host]} (clean-context)]
    (with-redefs [monotonic/prepare-monotonic-revision-one-shot!
                  (fn [& _] {:type :forged})]
      (is (= :substituted-issuer-or-inventory
             (invalid #(coverage/inspect-registered-writer-coverage!
                        (inv host) {path (:handle issued)})))))
    (with-redefs [gesso.assurance.proof.physical-writer-inventory/describe-physical-writer-inventory!
                  (fn [_] {:status :conditional-observed-writer-path-id-join})]
      (is (= :substituted-issuer-or-inventory
             (invalid #(coverage/inspect-registered-writer-coverage!
                        (inv host) {path (:handle issued)})))))))

(deftest host-operation-label-does-not-prove-command-abstract-semantics
  (let [b (seeded)
        ;; Same physical before/after, but an unrelated command operation
        ;; is advertised by the host as the declared :example/claim operation.
        mislabeled-source
        (assoc-in (source b) [:plan :commands 0]
                  (command/update-command table :different-action
                                          b (after b) version))
        issued (coverage/issue-monotonic-registered-writer!
                path op *node* mislabeled-source :revision)
        host (atom {path (registered (:writer issued))})
        report (coverage/inspect-registered-writer-coverage!
                (inv host) {path (:handle issued)})]
    (is (true? (:all-currently-surveyed-writers-locally-bound? report)))
    (is (= :not-established
           (:declared-operation-to-prepared-command-semantics report)))
    (assert-open-boundary! report)))

(deftest concurrent-survey-readers-do-not-submit-registered-writer
  (let [{:keys [before issued host]} (clean-context)
        src (inv host)
        handles {path (:handle issued)}
        reports (doall (map deref
                            (doall (repeatedly 3
                                             #(future (coverage/inspect-registered-writer-coverage!
                                                       src handles))))))]
    (is (every? :all-currently-surveyed-writers-locally-bound? reports))
    (is (= before (physical before)))))

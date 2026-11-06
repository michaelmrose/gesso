(ns gesso.assurance.example.physical-single-node-outcome-correspondence-test
  "v1500: Permanent adversarial checks of v1499's bounded outcome/readback
   agreement diagnostic. Exercise real embedded XTDB and the original typed
   reservation catalog, not a separately reimplemented abstract model.

   Agreement is NEVER an authenticated commit or a refinement certificate.
   In particular a foreign writer plus an unrelated transaction key can
   produce agreeing observations: this suite keeps that limitation explicit."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-outcome-correspondence :as subject]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.typed-presence-semantics :as typed]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private expected-obligations
  #{:actual-execute-callback-and-submission-not-source-authenticated
    :reported-transaction-key-and-commit-not-independently-authenticated
    :physical-read-node-and-read-timing-not-authenticated
    :concurrent-external-writer-interference-not-excluded
    :physical-to-abstract-mapping-not-independently-proved
    :issued-certified-executor-and-installed-route-not-established
    :complete-writer-coverage-not-established
    :transaction-internal-prestate-not-authenticated
    :universal-physical-refinement-not-established})

(defn- reason [f]
  (try
    (f)
    :unexpected-success
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-single-node-outcome-correspondence/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- put! [app row]
  (xt/execute-tx (:node app) [[:put-docs physical/table row]]))

(defn- expected-physical-after [row]
  (-> row
      (assoc :status "reserved" :claims 1
             :updated-at (str "v1494-revision-" (inc (:revision row))))
      (update :revision inc)))

(defn- assert-limited! [report op]
  (is (= subject/report-type (:type report)))
  (is (= subject/report-version (:version report)))
  (is (= :slot/reservation-lifecycle (:catalog-id report)))
  (is (= op (:operation-id report)))
  (is (true? (:abstract-guard-checked? report)))
  (is (true? (:declared-invariants-evaluated? report)))
  (is (= :one-caller-node-one-attempt-two-external-reads (:scope report)))
  (is (= :original-typed-operation-and-independent-physical-readback
         (:checking report)))
  (is (= :not-proved (:proof-status report)))
  (is (= :not-established (:physical-refinement report)))
  (is (= :not-established (:authenticated-commit report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= expected-obligations (:open-obligations report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report)))
  (is (not (contains? report :slot-id)))
  (is (not (contains? report :node)))
  (is (not (contains? report :tx-result)))
  (is (not (contains? report :physical-before)))
  (is (not (contains? report :physical-after))))

(deftest real-reserve-and-release-agree-only-in-the-bounded-sense
  (physical/with-proving-application!
   (fn [{:keys [read! abstract!] :as app}]
     (let [reserve (subject/observe-operation! app :slot/reserve)
           release (subject/observe-operation! app :slot/release)]
       (doseq [[op report] [[:slot/reserve reserve] [:slot/release release]]]
         (assert-limited! report op)
         (is (= :bounded-reported-commit-and-external-poststate-agreement
                (:status report)))
         (is (= :committed (:reported-commit-status report)))
         (is (true? (:public-transaction-key-shape? report)))
         (is (true? (:reported-outcome-and-observed-poststate-agree? report)))
         (is (true? (:physical-poststate-matches-declared-transition? report)))
         (is (true? (:abstract-poststate-matches-declared-transition? report))))
       (is (= [0 1] [(:observed-before-revision reserve)
                        (:observed-after-revision reserve)]))
       (is (= [1 2] [(:observed-before-revision release)
                        (:observed-after-revision release)]))
       (is (= 2 (:revision (read!))))
       (is (= {:slot/status :available :slot/active-claims 0
               :slot/audit-tag :preserved}
              (abstract!)))))))

(deftest one-attempt-causes-at-most-one-gesso-transaction
  (physical/with-proving-application!
   (fn [app]
     (let [original @#'tx/transact!
           calls (atom 0)
           result (with-redefs [tx/transact! (fn [ctx plan]
                                               (swap! calls inc)
                                               (original ctx plan))]
                    (subject/observe-operation! app :slot/reserve))]
       (is (= 1 @calls))
       (is (= 1 (:revision ((:read! app)))))
       (is (true? (:reported-outcome-and-observed-poststate-agree? result)))))))

(deftest forged-committed-map-without-write-never-agrees
  (physical/with-proving-application!
   (fn [app]
     (let [calls (atom 0)
           before ((:read! app))
           report (subject/observe-operation!
                   (assoc app :execute! (fn [_]
                                          (swap! calls inc)
                                          {:commit/status :committed}))
                   :slot/reserve)]
       (is (= 1 @calls))
       (is (= before ((:read! app))))
       (is (= :committed (:reported-commit-status report)))
       (is (false? (:public-transaction-key-shape? report)))
       (is (false? (:physical-poststate-matches-declared-transition? report)))
       (is (= :outcome-or-poststate-correspondence-not-established (:status report)))
       (is (false? (:reported-outcome-and-observed-poststate-agree? report)))
       (assert-limited! report :slot/reserve)))))

(deftest actual-write-without-reported-transaction-key-is-not-agreement
  (physical/with-proving-application!
   (fn [app]
     (let [execute! (:execute! app)
           report (subject/observe-operation!
                   (assoc app :execute! (fn [op]
                                          (dissoc (execute! op) :tx-result)))
                   :slot/reserve)]
       (is (= 1 (:revision ((:read! app)))))
       (is (true? (:physical-poststate-matches-declared-transition? report)))
       (is (true? (:abstract-poststate-matches-declared-transition? report)))
       (is (false? (:public-transaction-key-shape? report)))
       (is (= :outcome-or-poststate-correspondence-not-established (:status report)))
       (assert-limited! report :slot/reserve)))))

(deftest commit-unknown-after-a-real-write-is-nonauthoritative
  (physical/with-proving-application!
   (fn [app]
     (let [execute! (:execute! app)
           calls (atom 0)
           report (subject/observe-operation!
                   (assoc app :execute! (fn [op]
                                          (swap! calls inc)
                                          (execute! op)
                                          {:commit/status :commit-unknown}))
                   :slot/reserve)]
       (is (= 1 @calls))
       (is (= 1 (:revision ((:read! app)))))
       (is (= :nondefinitive (:reported-commit-status report)))
       (is (true? (:physical-poststate-matches-declared-transition? report)))
       (is (false? (:reported-outcome-and-observed-poststate-agree? report)))
       (assert-limited! report :slot/reserve)))))

(deftest nondefinitive-returns-never-claim-commit
  (doseq [outcome [nil {} {:commit/status :rejected}
                   {:commit/status :commit-unknown}
                   {:commit/status :already-incorporated}
                   {:commit/status :unknown}]]
    (testing (pr-str outcome)
      (physical/with-proving-application!
       (fn [app]
         (let [calls (atom 0)
               before ((:read! app))
               report (subject/observe-operation!
                       (assoc app :execute! (fn [_] (swap! calls inc) outcome))
                       :slot/reserve)]
           (is (= 1 @calls))
           (is (= before ((:read! app))))
           (is (= :nondefinitive (:reported-commit-status report)))
           (is (false? (:reported-outcome-and-observed-poststate-agree? report)))
           (is (= :outcome-or-poststate-correspondence-not-established (:status report)))
           (assert-limited! report :slot/reserve)))))))

(deftest execution-exception-preserves-original-throwable-and-no-retry
  (physical/with-proving-application!
   (fn [app]
     (let [failure (ex-info "synthetic failed operation" {:marker :v1500})
           calls (atom 0)
           before ((:read! app))
           thrown (try
                    (subject/observe-operation!
                     (assoc app :execute! (fn [_]
                                            (swap! calls inc)
                                            (throw failure)))
                     :slot/reserve)
                    nil
                    (catch Exception e e))]
       (is (identical? failure thrown))
       (is (= 1 @calls))
       (is (= before ((:read! app))))))))

(deftest ineligible-repeat-does-not-execute-at-all
  (physical/with-proving-application!
   (fn [app]
     ((:execute! app) :slot/reserve)
     (let [calls (atom 0)
           changed (assoc app :execute! (fn [_] (swap! calls inc) nil))]
       (is (= :abstract-operation-guard-does-not-hold
              (reason #(subject/observe-operation! changed :slot/reserve))))
       (is (zero? @calls))
       (is (= 1 (:revision ((:read! app)))))))))

(deftest release-before-reserve-does-not-execute
  (physical/with-proving-application!
   (fn [app]
     (let [calls (atom 0)]
       (is (= :abstract-operation-guard-does-not-hold
              (reason #(subject/observe-operation!
                        (assoc app :execute! (fn [_] (swap! calls inc) nil))
                        :slot/release))))
       (is (zero? @calls))))))

(deftest wrong-operation-id-or-metadata-is-rejected-before-execute
  (physical/with-proving-application!
   (fn [app]
     (let [calls (atom 0)
           quiet (assoc app :execute! (fn [_] (swap! calls inc) nil))]
       (doseq [changed [(assoc-in quiet [:report :certificate] :issued)
                        (assoc-in quiet [:report :authority] :verified)
                        (assoc-in quiet [:report :operation-ids] #{:slot/reserve})
                        (assoc-in quiet [:report :observed-writer-completeness] :complete)
                        (assoc-in quiet [:report :declared-example-writer-paths] #{:operation/reserve})
                        (assoc quiet :catalog (assoc (:catalog app) :id :other))
                        (assoc quiet :node nil)
                        (assoc quiet :slot-id "../escape")]]
         (is (= :invalid-application-operation-or-source
                (reason #(subject/observe-operation! changed :slot/reserve)))))
       (is (= :invalid-application-operation-or-source
              (reason #(subject/observe-operation! quiet :slot/missing))))
       (is (zero? @calls))))))

(deftest substituted-critical-source-seams-are-rejected
  (physical/with-proving-application!
   (fn [app]
     (let [calls (atom 0)
           quiet (assoc app :execute! (fn [_] (swap! calls inc) nil))]
       (with-redefs [pilot/reservation-catalog (fn [] :fake)]
         (is (= :substituted-source-or-physical-read-seam
                (reason #(subject/observe-operation! quiet :slot/reserve)))))
       (with-redefs [physical/physical->abstract (fn [_] {})]
         (is (= :substituted-source-or-physical-read-seam
                (reason #(subject/observe-operation! quiet :slot/reserve)))))
       (with-redefs [xt/q (fn [& _] [])]
         (is (= :substituted-source-or-physical-read-seam
                (reason #(subject/observe-operation! quiet :slot/reserve)))))
       (with-redefs [typed/evaluate-expression! (fn [& _] {})]
         (is (= :substituted-source-or-physical-read-seam
                (reason #(subject/observe-operation! quiet :slot/reserve)))))
       (is (zero? @calls))))))

(deftest unexpected-physical-fields-prevent-execution
  (physical/with-proving-application!
   (fn [app]
     (put! app (assoc ((:read! app)) :undeclared "extra"))
     (let [calls (atom 0)]
       (is (= :unsupported-physical-readback
              (reason #(subject/observe-operation!
                        (assoc app :execute! (fn [_] (swap! calls inc) nil))
                        :slot/reserve))))
       (is (zero? @calls))))))

(deftest missing-physical-document-prevents-execution
  (physical/with-proving-application!
   (fn [app]
     (xt/execute-tx (:node app)
                    [[:delete-docs physical/table (:slot-id app)]])
     (let [calls (atom 0)]
       (is (= :missing-or-ambiguous-physical-readback
              (reason #(subject/observe-operation!
                        (assoc app :execute! (fn [_] (swap! calls inc) nil))
                        :slot/reserve))))
       (is (zero? @calls))))))

(deftest physical-frame-modification-prevents-poststate-agreement
  (physical/with-proving-application!
   (fn [app]
     (let [execute! (:execute! app)
           report (subject/observe-operation!
                   (assoc app :execute!
                          (fn [op]
                            (let [result (execute! op)]
                              (put! app (assoc ((:read! app)) :audit-tag "changed"))
                              result)))
                   :slot/reserve)]
       (is (= :outcome-or-poststate-correspondence-not-established
              (:status report)))
       (is (false? (:physical-poststate-matches-declared-transition? report)))
       (is (false? (:abstract-poststate-matches-declared-transition? report))
           "Audit-field modification must violate the abstract frame")
       (assert-limited! report :slot/reserve)))))

(deftest unexpected-poststate-fields-reject-even-after-a-real-write
  (physical/with-proving-application!
   (fn [app]
     (let [execute! (:execute! app)
           calls (atom 0)]
       (is (= :unsupported-physical-readback
              (reason #(subject/observe-operation!
                        (assoc app :execute!
                               (fn [op]
                                 (swap! calls inc)
                                 (let [result (execute! op)]
                                   (put! app (assoc ((:read! app))
                                                    :undeclared "extra"))
                                   result)))
                        :slot/reserve))))
       (is (= 1 @calls))))))

(deftest misleading-physical-version-does-not-match
  (physical/with-proving-application!
   (fn [app]
     (let [execute! (:execute! app)
           report (subject/observe-operation!
                   (assoc app :execute!
                          (fn [op]
                            (let [result (execute! op)]
                              (put! app (assoc ((:read! app)) :revision 42))
                              result)))
                   :slot/reserve)]
       (is (false? (:physical-poststate-matches-declared-transition? report)))
       (is (= :outcome-or-poststate-correspondence-not-established
              (:status report)))
       (assert-limited! report :slot/reserve)))))

(deftest foreign-writer-and-valid-key-can-produce-only-conditional-agreement
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           calls (atom 0)
           report (subject/observe-operation!
                   (assoc app :execute!
                          (fn [_]
                            (swap! calls inc)
                            ;; Bypass the Gesso model command entirely. The
                            ;; transaction key is genuine, but not an issued
                            ;; or source-authenticated executor witness.
                            {:commit/status :committed
                             :tx-result (put! app (expected-physical-after before))}))
                   :slot/reserve)]
       (is (= 1 @calls))
       (is (= :bounded-reported-commit-and-external-poststate-agreement
              (:status report)))
       (is (true? (:reported-outcome-and-observed-poststate-agree? report)))
       (assert-limited! report :slot/reserve)
       (is (= :not-established (:authenticated-commit report)))
       (is (= :not-established (:complete-writer-coverage report)))
       (is (= :not-established (:physical-refinement report)))))))

(deftest a-success-report-is-redacted-and-cannot-be-a-certificate
  (physical/with-proving-application!
   "v1500_secretSlot_123"
   (fn [app]
     (let [report (subject/observe-operation! app :slot/reserve)
           serialized (pr-str report)]
       (assert-limited! report :slot/reserve)
       (is (not (.contains serialized "v1500_secretSlot_123")))
       (is (not (.contains serialized "v1494-created")))
       (is (not (.contains serialized "preserved")))
       (is (= :not-issued (:certificate report)))
       (is (= :not-admitted (:admission report)))))))

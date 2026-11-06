(ns gesso.assurance.example.single-node-pilot-checkpoint-test
  "v1451: executable acceptance checkpoint for the first declared-machine pilot.

   The reservation source (v1450) has two abstract operations and three
   shared invariants. The writer source below is a GENUINELY issued but
   DIFFERENT one-operation Gesso application, rather than a fake positive
   source report. Joining them must disclose incomplete correspondence.

   This is a test of the non-authoritative SOURCE/ISSUED/HOST reporting path,
   not a deployed reservation application, proof of all installed XTDB
   writers, or physical transaction refinement. The test also checks that
   source changes and newly observed host entries cannot inherit assurance."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.application-induction-test]
            [gesso.assurance.proof.operation-writer-coverage-soundness-test]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.application-assurance-report :as report]
            [gesso.assurance.proof.application-dependency :as dependency]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]))

(def ^:private issued-operation :example/claim)
(def ^:private unclassified-operation :example/direct-xtdb-writer)
(def ^:dynamic *catalog* nil)
(def ^:dynamic *graph* nil)
(def ^:dynamic *closure* nil)
(def ^:dynamic *source* nil)
(def ^:dynamic *host* nil)
(def ^:dynamic *baseline* nil)

(defn- rejection [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- issued-context []
  (let [f (ns-resolve 'gesso.assurance.application-induction-test 'make-context)]
    (when-not f
      (throw (ex-info "Original issued-executor fixture unavailable" {})))
    (f)))

(use-fixtures :once
  (fn [run]
    (let [{:keys [proof plan-source induction-source executor handler]}
          (issued-context)]
      (fixture/with-closed-application
        {:operations
         {issued-operation
          (fixture/trusted-operation
           issued-operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [snapshot (atom {issued-operation
                                {:kind :semantic-route :handler handler}})
                contract {:inductive-source proof
                          :application-assembly assembly
                          :semantic-route-handlers {issued-operation handler}
                          :certified-operations
                          {issued-operation
                           {:executor executor :current-source plan-source}}}
                issued {:authoritative-coverage-source
                        {:inductive-contract-source contract
                         :enumerate-entry-points! (fn [] @snapshot)}
                        :inductive-plans
                        {issued-operation
                         {:executor executor :current-source induction-source}}}
                ;; This original independently checked abstract catalog
                ;; agrees with the issued one-operation application, but it
                ;; deliberately does not agree with the reservation pilot.
                independent
                (let [f (ns-resolve
                         'gesso.assurance.proof.operation-writer-coverage-soundness-test
                         'abstract-catalog)]
                  (when-not f
                    (throw (ex-info "Original writer fixture unavailable" {})))
                  (f [issued-operation]))
                source {:induction-catalog independent
                        :application-induction-source issued}
                {:keys [catalog graph closure]}
                (pilot/assemble-abstract-assurance!)
                baseline (delay (report/describe-application-assurance!
                                  catalog graph closure source))]
            (binding [*catalog* catalog *graph* graph *closure* closure
                      *source* source *host* snapshot *baseline* baseline]
              ;; Force the ordinary baseline before any test is allowed to
              ;; mutate the host snapshot. Test order must be irrelevant.
              (force baseline)
              (run))))))))

(deftest source-derived-pilot-is-conditionally-inductive-not-certified
  (let [ops #{:slot/reserve :slot/release}
        invariants #{:slot/at-most-one-claim
                     :slot/status-domain :slot/nonnegative-claims}
        summary (dependency/describe-application-dependencies *closure*)]
    (is (= ops (:operation-ids summary)))
    (is (= invariants (:invariant-ids summary)))
    (is (= :not-established (:installed-writer-coverage summary)))
    (is (= :not-issued (:certificate summary)))
    (is (= :none (:authority summary)))))

(deftest real-issued-handler-does-not-make-foreign-pilot-complete
  (let [r @*baseline*
        accounting (:observed-writer-accounting r)]
    (is (= report/report-type (:type r)))
    (is (= #{:slot/reserve :slot/release} (:operation-ids r)))
    (is (= :open-observed-writer-coverage (:status r)))
    (is (false? (:exact-observed-operation-id-join? accounting)))
    (is (= #{issued-operation} (:issued-operation-ids accounting)))
    (is (= #{:slot/reserve :slot/release}
           (get-in accounting [:declared-vs-issued :missing-from-right])))
    (is (= #{issued-operation}
           (get-in accounting [:declared-vs-issued :unmodeled-in-right])))
    (is (contains? (:open-obligations r) :observed-writer-coverage-open))
    (is (contains? (:open-obligations r)
                   :independent-proof-kernel-soundness))
    (doseq [[field expected]
            [[:physical-writer-coverage :not-established]
             [:abstract-to-physical-correspondence :not-established]
             [:browser-correspondence :not-established]
             [:universal-proof-kernel-soundness :not-established]
             [:certificate :not-issued]
             [:admission :not-admitted]
             [:transaction-authority :none]
             [:authority :none]]]
      (is (= expected (get r field)) (str field)))))

(deftest unchanged-checkpoint-report-is-recheckable-but-not-authoritative
  (let [r @*baseline*
        again (report/require-current-application-assurance!
               r *catalog* *graph* *closure* *source*)]
    (is (= r again))
    (is (= :not-issued (:certificate again)))
    (is (= :none (:authority again)))))

(deftest changed-declared-rule-cannot-borrow-old-closure
  (let [changed (assoc-in *catalog*
                          [:operations 0 :construction :replacements
                           :slot/active-claims]
                          [:literal 0])
        e (rejection #(dependency/require-current-application-dependencies!
                       *closure* changed *graph*))]
    (is (map? e))
    (is (= :gesso.assurance.proof.application-dependency/invalid-application-dependencies
           (:error/type e)))))

(deftest unclassified-host-entry-remains-open
  (swap! *host* assoc unclassified-operation
         {:kind :unclassified :handler (fn [_] {:status 200})})
  (try
    (let [r (report/describe-application-assurance!
             *catalog* *graph* *closure* *source*)
          accounting (:observed-writer-accounting r)]
      (is (= :open-observed-writer-coverage (:status r)))
      (is (= #{unclassified-operation}
             (:observed-unclassified-entry-point-ids accounting)))
      (is (contains? (:open-obligations r)
                     :observed-entry-points-outside-declared-application))
      (is (not= (:fingerprint r) (:fingerprint @*baseline*)))
      (is (= :not-established (:physical-writer-coverage r)))
      (is (= :none (:authority r))))
    (finally
      (swap! *host* dissoc unclassified-operation))))

(deftest invalid-and-forged-original-writer-sources-reject
  (doseq [bad [nil {} {:induction-catalog {}}
               (assoc *source* :certificate :issued)]]
    (let [e (rejection #(report/describe-application-assurance!
                         *catalog* *graph* *closure* bad))]
      (is (map? e))
      (is (= :invalid-original-writer-source (:reason e))))))

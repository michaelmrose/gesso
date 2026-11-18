(ns gesso.assurance.ansatz-observation-gate-pilot
  "Opt-in v1736 source-derived, fail-closed ABSTRACT observer boundary.

   After the original reservation's Ansatz source/step bridge is kernel-checked
   and dependency-audited, validate EVERY observed matching transition against
   that SAME generated induction-step formula. Cross-check the original and
   independent Gesso abstract observers and refuse fabricated/mismatched
   reports. This makes the check a CONDITION of acceptance for all calls
   through this test-only boundary rather than relying on a finite sample.

   This is NOT a universal mathematical proof of either Clojure interpreter.
   It makes no assertion about calls that bypass this boundary, guard/assignment
   implementation correctness, installed handlers, XTDB writers or commits,
   initialization, or physical state. Return values are copyable and never
   carry admission, certification or runtime authority. No downstream type
   annotations or Ansatz source are needed."
  (:require [ansatz.core :as ansatz]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-transition-correspondence-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract]))

(def ^:private source-ns 'gesso.assurance.ansatz-transition-correspondence-pilot)
(def ^:private status-field :slot/status)
(def ^:private claims-field :slot/active-claims)
(def ^:private audit-field :slot/audit-tag)

(defn- from-source! [function & args]
  (let [v (ns-resolve source-ns function)]
    (when-not v
      (throw (ex-info "Missing original proof bridge" {:reason :pilot-contract-changed
                                                        :function function})))
    (apply @v args)))

(defn- fail! [reason details]
  (throw (ex-info "Unqualified Gesso abstract observation"
                  {:reason reason :details details})))

(defn- exact-report-shape! [result original independent-observation]
  (let [flags [:guard-holds? :assignments-hold? :frame-holds? :matches?]]
    (when-not (and (= :gesso.assurance.proof.transition/abstract-observation
                      (:type original))
                   (= :abstract-relation-observed (:checking original))
                   (= :gesso.assurance.proof.independent-transition-semantics/observation
                      (:type independent-observation))
                   (= :independent-typed-guarded-relation
                      (:checking independent-observation))
                   (= (:transition-fingerprint result)
                      (:transition-fingerprint original))
                   (= (:transition result)
                      (:source-declaration independent-observation))
                   (every? (fn [f]
                             (and (boolean? (get original f))
                                  (boolean? (get independent-observation f))
                                  (= (get original f)
                                     (get independent-observation f))))
                           flags))
      (fail! :observer-disagreement
             {:operation-id (:operation-id result)
              :original original :independent independent-observation}))
    (let [all? (every? true? (map original
                                  [:guard-holds? :assignments-hold? :frame-holds?]))]
      (when-not (= all? (:matches? original))
        (fail! :observer-acceptance-inconsistent
               {:operation-id (:operation-id result)})))))

(defn- audited-observation-boundary! [catalog]
  ;; This is a proof/check-time CONSTRUCTOR, not an HTTP-path proof invocation.
  ;; The three source-specific bridge theorems are checked BEFORE any witness
  ;; can be accepted. No dynamic application-supplied proof report is trusted.
  (let [bridge (from-source! 'check-original-bridge! catalog)
        {:keys [model sources]} (from-source! 'source-bridge-obligations! catalog)
        sources-by-id (into {} (map (juxt :operation-id identity)) sources)]
    (when-not (and (= :conditional-abstract-step-bridge (:assurance bridge))
                   (= 3 (:kernel-checked-bridge-count bridge))
                   (= 2 (count sources))
                   (= (:catalog-fingerprint bridge)
                      (:catalog-fingerprint model)))
      (fail! :incomplete-kernel-bridge bridge))
    {:checking :kernel-audited-abstract-observation-boundary-pilot
     :catalog-fingerprint (:catalog-fingerprint model)
     :observer-correspondence :per-invocation-checked-not-universally-proved
     :physical-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none
     :observe!
     (fn [operation-id before after]
       (let [{:keys [transition] :as source} (get sources-by-id operation-id)]
         (when-not source
           (fail! :unknown-source-operation {:operation-id operation-id}))
         ;; Both original interpreters validate the exact typed before/after
         ;; projection and reject malformed values; never accept caller reports.
         (let [original (abstract/observe-transition! transition before {} after)
               other (independent/observe-transition! transition before {} after)]
           (exact-report-shape! source original other)
           (let [matches? (:matches? original)
                 represented? (when matches?
                                (from-source! 'step-holds? model before after))]
             (when (and matches? (not represented?))
               (fail! :accepted-abstract-transition-outside-proved-step
                      {:operation-id operation-id :before before :after after}))
             {:operation-id operation-id
              :transition-fingerprint (:transition-fingerprint source)
              :matches? matches?
              :step-checked? (boolean matches?)
              :step-represented? (boolean represented?)
              :assurance :per-observation-abstract-step-enforcement
              :certificate :not-issued
              :admission :not-admitted
              :authority :none}))))}))

(deftest every-accepted-observation-at-this-boundary-satisfies-the-proved-step
  (let [boundary (audited-observation-boundary! (example/reservation-catalog))
        check! (:observe! boundary)
        states (vec (for [status [:available :reserved]
                          claims [-1 0 1 2]
                          audit [nil :audit/tag]]
                      {status-field status claims-field claims audit-field audit}))
        hits (atom 0)]
    (is (= :kernel-audited-abstract-observation-boundary-pilot (:checking boundary)))
    (is (= :per-invocation-checked-not-universally-proved
           (:observer-correspondence boundary)))
    (doseq [operation-id [:slot/reserve :slot/release]
            before states
            after states]
      (let [result (check! operation-id before after)]
        (is (boolean? (:matches? result)))
        (when (:matches? result)
          (swap! hits inc)
          (is (:step-checked? result))
          (is (:step-represented? result))
          (is (= :none (:authority result))))))
    (is (= 16 @hits))
    (is (= :not-issued (:certificate boundary)))
    (is (= :not-admitted (:admission boundary)))
    (is (= :none (:authority boundary)))))

(deftest observational-boundary-catches-false-acceptance-from-both-observers
  (let [check! (:observe! (audited-observation-boundary!
                           (example/reservation-catalog)))
        before {status-field :available claims-field 0 audit-field nil}
        bad {status-field :reserved claims-field 2 audit-field nil}
        valid {status-field :reserved claims-field 1 audit-field nil}
        primary abstract/observe-transition!
        secondary independent/observe-transition!
        fake-primary (fn [d b in a]
                       (assoc (primary d b in a)
                              :guard-holds? true :assignments-hold? true
                              :frame-holds? true :matches? true))
        fake-secondary (fn [d b in a]
                         (assoc (secondary d b in a)
                                :guard-holds? true :assignments-hold? true
                                :frame-holds? true :matches? true))
        reason (fn [f]
                 (try (f) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (is (:matches? (check! :slot/reserve before valid)))
    (is (false? (:matches? (check! :slot/reserve before bad))))
    (testing "A single lying observer cannot bypass an independent observer"
      (with-redefs [abstract/observe-transition! fake-primary]
        (is (= :observer-disagreement
               (reason #(check! :slot/reserve before bad))))))
    (testing "Even two colluding false-positive reports cannot bypass step check"
      (with-redefs [abstract/observe-transition! fake-primary
                   independent/observe-transition! fake-secondary]
        (is (= :accepted-abstract-transition-outside-proved-step
               (reason #(check! :slot/reserve before bad))))))
    (testing "Unknown operation and invalid source projection fail closed"
      (is (= :unknown-source-operation
             (reason #(check! :slot/unlisted before valid))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (check! :slot/reserve
                           (assoc before :slot/unregistered 1) valid))))))

(deftest per-observation-check-does-not-depend-on-a-bounded-integer-range
  (let [check! (:observe! (audited-observation-boundary!
                           (example/reservation-catalog)))
        before {status-field :available claims-field 1000000 audit-field nil}
        after {status-field :reserved claims-field 1 audit-field nil}
        mismatched (assoc after claims-field -1000000)]
    ;; Invalid initial invariant is irrelevant to this *conditional bridge*:
    ;; the guard/write match still implies the trace-step premise. A separate
    ;; initialization obligation is necessary for invariant safety.
    (is (:matches? (check! :slot/reserve before after)))
    (is (:step-represented? (check! :slot/reserve before after)))
    (is (false? (:matches? (check! :slot/reserve before mismatched))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests
                              'gesso.assurance.ansatz-observation-gate-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

(ns gesso.assurance.example.physical-single-node-initialization-test
  "v1498: permanent adversarial checks for the source-checked, bounded
   bootstrap observation introduced in v1497. Exercise real embedded XTDB
   readback and the ORIGINAL typed initial-constructor obligations.

   These tests do not certify an initializer or prove complete writer
   enumeration. In particular, successful observation after a separate
   undeclared write says nothing about that write's provenance."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-initialization :as subject]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private required-invariant-ids
  #{[:initial-state :slot/status-domain]
    [:initial-state :slot/nonnegative-claims]
    [:initial-state :slot/at-most-one-claim]})

(def ^:private required-open-obligations
  #{:actual-bootstrap-transaction-receipt-not-authenticated
    :physical-initializer-to-constructor-refinement-not-proved
    :complete-initializer-and-writer-coverage-not-established
    :physical-value-mapping-not-independently-certified
    :read-node-and-snapshot-provenance-not-authenticated
    :issued-executor-and-installed-handler-binding-not-established
    :transaction-internal-prestate-not-authenticated
    :universal-physical-refinement-not-established})

(defn- rejection-reason [f]
  (try
    (f)
    :unexpected-success
    (catch clojure.lang.ExceptionInfo e
      (if (:physical-single-node-initialization/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- put! [app row]
  (xt/execute-tx (:node app) [[:put-docs physical/table row]]))

(defn- require-readback-mismatch! [app row]
  (put! app row)
  (is (= :physical-bootstrap-readback-mismatch
         (rejection-reason #(subject/inspect-initialization! app)))))

(defn- assert-conditional-report! [report]
  (is (= subject/initialization-type (:type report)))
  (is (= subject/initialization-version (:version report)))
  (is (= subject/initializer-id (:initializer-id report)))
  (is (= :slot/reservation-lifecycle (:catalog-id report)))
  (is (string? (:case-fingerprint report)))
  (is (= 2 (count (:obligation-fingerprints report))))
  (is (every? string? (:obligation-fingerprints report)))
  (is (true? (:initial-condition-holds? report)))
  (is (= required-invariant-ids (:checked-initial-obligation-ids report)))
  (is (= 1 (:observed-document-count report)))
  (is (= 7 (:observed-declared-field-count report)))
  (is (true? (:bootstrap-writer-listed-in-example report)))
  (is (= :original-typed-init-and-fresh-external-physical-readback
         (:checking report)))
  (is (= :one-physical-bootstrap-document-at-one-read (:scope report)))
  (is (= :source-checked-bounded-initialization-observation
         (:proof-status report)))
  (is (= :not-established (:physical-initialization-coverage report)))
  (is (= :not-established (:complete-writer-coverage report)))
  (is (= :not-authenticated (:internal-transaction-prestate report)))
  (is (= required-open-obligations (:open-obligations report)))
  (is (= :not-issued (:certificate report)))
  (is (= :not-admitted (:admission report)))
  (is (= :none (:authority report))))

(deftest real-bootstrap-is-a-bounded-readback-not-an-issued-certificate
  (subject/with-checked-initialized-application!
   (fn [{:keys [read! abstract! initialization-report slot-id] :as app}]
     (assert-conditional-report! initialization-report)
     (is (= (physical/initial-document slot-id) (read!)))
     (is (= {:slot/status :available
             :slot/active-claims 0
             :slot/audit-tag :preserved}
            (abstract!)))
     (is (= initialization-report (subject/inspect-initialization! app))))))

(deftest named-slot-id-is-preserved-without-leaking-through-diagnostics
  (subject/with-checked-initialized-application!
   "v1498_privateSlot_017"
   (fn [{:keys [slot-id initialization-report]}]
     (is (= "v1498_privateSlot_017" slot-id))
     (is (not (re-find #"v1498_privateSlot_017"
                       (pr-str initialization-report))))
     (is (not (contains? initialization-report :node)))
     (is (not (contains? initialization-report :physical-document)))
     (is (not (contains? initialization-report :slot-id)))
     (assert-conditional-report! initialization-report))))

(deftest inspecting-again-does-not-submit-a-second-transaction
  (physical/with-proving-application!
   (fn [{:keys [read!] :as app}]
     (let [before (read!)
           calls (atom 0)]
       (with-redefs [tx/transact! (fn [& _]
                                    (swap! calls inc)
                                    (throw (ex-info "unexpected transaction" {})))]
         (assert-conditional-report! (subject/inspect-initialization! app))
         (assert-conditional-report! (subject/inspect-initialization! app)))
       (is (zero? @calls))
       (is (= before (read!)))))))

(deftest changed-business-fields-are-not-valid-initial-state
  (doseq [[field value] [[:status "reserved"]
                         [:claims 1]
                         [:audit-tag "changed"]]]
    (testing (str field " = " value)
      (physical/with-proving-application!
       (fn [{:keys [read!] :as app}]
         (require-readback-mismatch! app (assoc (read!) field value)))))))

(deftest changed-version-and-bootstrap-clock-are-not-initial-state
  (doseq [[field value] [[:revision 17]
                         [:created-at "external-created"]
                         [:updated-at "external-updated"]]]
    (testing (str field " = " value)
      (physical/with-proving-application!
       (fn [{:keys [read!] :as app}]
         (require-readback-mismatch! app (assoc (read!) field value)))))))

(deftest added-physical-fields-cannot-be-silent
  (physical/with-proving-application!
   (fn [{:keys [read!] :as app}]
     (require-readback-mismatch! app (assoc (read!) :undeclared-field "hidden")))))

(deftest removed-physical-fields-cannot-be-silent
  (doseq [field [:audit-tag :claims :created-at]]
    (testing (str "missing " field)
      (physical/with-proving-application!
       (fn [{:keys [read!] :as app}]
         (require-readback-mismatch! app (dissoc (read!) field)))))))

(deftest deleted-initial-document-cannot-pass
  (physical/with-proving-application!
   (fn [{:keys [node slot-id] :as app}]
     (xt/execute-tx node [[:delete-docs physical/table slot-id]])
     (is (= :missing-or-ambiguous-physical-initial-document
            (rejection-reason #(subject/inspect-initialization! app)))))))

(deftest mutation-after-a-successful-read-invalidates-a-fresh-inspection
  (physical/with-proving-application!
   (fn [{:keys [read!] :as app}]
     (let [earlier (subject/inspect-initialization! app)]
       (assert-conditional-report! earlier)
       (put! app (assoc (read!) :audit-tag "altered"))
       (is (= :physical-bootstrap-readback-mismatch
              (rejection-reason #(subject/inspect-initialization! app))))
       ;; The earlier report is NOT a continuing authorization to operate.
       (is (= :not-issued (:certificate earlier)))
       (is (= :not-established (:complete-writer-coverage earlier)))))))

(deftest committed-application-operations-do-not-masquerade-as-bootstrap
  (physical/with-proving-application!
   (fn [{:keys [execute! read!] :as app}]
     (is (= :committed (:commit/status (execute! :slot/reserve))))
     (is (= 1 (:revision (read!))))
     (is (= :physical-bootstrap-readback-mismatch
            (rejection-reason #(subject/inspect-initialization! app)))))))

(deftest undeclared-physical-writer-is-not-detected-as-complete-coverage
  (physical/with-proving-application!
   (fn [{:keys [node slot-id] :as app}]
     (xt/execute-tx node
                    [[:put-docs physical/table
                      (physical/initial-document (str slot-id "_other"))]])
     ;; A separate physical document lies outside the single-document read.
     ;; A successful read does not imply enumeration of the additional writer.
     (let [report (subject/inspect-initialization! app)]
       (assert-conditional-report! report)
       (is (= :not-established (:physical-initialization-coverage report)))
       (is (contains? (:open-obligations report)
                      :complete-initializer-and-writer-coverage-not-established))))))

(deftest a-tampered-declared-catalog-is-not-source-checked
  (physical/with-proving-application!
   (fn [{:keys [catalog] :as app}]
     (doseq [changed [(assoc catalog :id :slot/forged)
                      (dissoc catalog :operations)
                      (assoc catalog :declared-operation-ids #{:slot/reserve})]]
       (is (= :invalid-or-unissued-bootstrap-application
              (rejection-reason #(subject/inspect-initialization!
                                   (assoc app :catalog changed)))))))))

(deftest missing-or-substituted-physical-node-binding-is-rejected
  (physical/with-proving-application!
   (fn [app]
     (doseq [changed [(dissoc app :node)
                      (assoc-in app [:biff-context :biff.xtdb/node] (Object.))
                      (assoc app :slot-id nil)
                      (assoc app :execute! nil)]]
       (is (= :invalid-or-unissued-bootstrap-application
              (rejection-reason #(subject/inspect-initialization! changed))))))))

(deftest missing-bootstrap-or-operation-writers-are-rejected
  (physical/with-proving-application!
   (fn [app]
     (doseq [writers [#{}
                      #{:operation/reserve :operation/release}
                      #{:initializer/bootstrap :operation/reserve}
                      #{:initializer/bootstrap :operation/reserve
                        :operation/release :writer/undeclared}]]
       (is (= :invalid-or-unissued-bootstrap-application
              (rejection-reason
               #(subject/inspect-initialization!
                 (assoc-in app [:report :declared-example-writer-paths]
                           writers)))))))))

(deftest invented-certification-or-writer-completeness-is-rejected
  (physical/with-proving-application!
   (fn [app]
     (doseq [[path value] [[[:report :certificate] :issued]
                           [[:report :authority] :physical-certified]
                           [[:report :observed-writer-completeness] :established]
                           [[:report :operation-ids] #{:slot/reserve}]]]
       (is (= :invalid-or-unissued-bootstrap-application
              (rejection-reason
               #(subject/inspect-initialization!
                 (assoc-in app path value)))))))))

(deftest substituted-initial-constructor-and-catalog-are-rejected
  (physical/with-proving-application!
   (fn [app]
     (with-redefs [constructor/check-initial-constructor! (fn [& _] {:certificate :issued})]
       (is (= :substituted-initialization-source
              (rejection-reason #(subject/inspect-initialization! app)))))
     (with-redefs [pilot/reservation-catalog (fn [] {:id :slot/forged})]
       (is (= :substituted-initialization-source
              (rejection-reason #(subject/inspect-initialization! app))))))))

(deftest substituted-physical-seed-projection-or-read-hook-is-rejected
  (physical/with-proving-application!
   (fn [app]
     (with-redefs [physical/initial-document (fn [_] {:xt/id "forged"})]
       (is (= :substituted-initialization-source
              (rejection-reason #(subject/inspect-initialization! app)))))
     (with-redefs [physical/physical->abstract (fn [_] {:slot/status :available})]
       (is (= :substituted-initialization-source
              (rejection-reason #(subject/inspect-initialization! app)))))
     (with-redefs [xt/q (fn [& _] [(physical/initial-document (:slot-id app))])]
       (is (= :substituted-initialization-source
              (rejection-reason #(subject/inspect-initialization! app))))))))

(deftest malformed-applications-fail-before-physical-inspection
  (doseq [app [nil [] {} {:node (Object.)}]]
    (is (= :invalid-or-unissued-bootstrap-application
           (rejection-reason #(subject/inspect-initialization! app))))))

(deftest fixture-rejects-invalid-callbacks-and-identifiers
  (is (= :invalid-initialization-callback
         (rejection-reason #(subject/with-checked-initialized-application! nil))))
  (is (= :invalid-initialization-callback
         (rejection-reason #(subject/with-checked-initialized-application!
                             "v1498_valid" :not-a-function))))
  (is (= :unsupported-slot-id
         (try
           (subject/with-checked-initialized-application! "bad/id" identity)
           :unexpected-success
           (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))

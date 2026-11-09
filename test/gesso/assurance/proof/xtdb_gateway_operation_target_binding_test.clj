(ns gesso.assurance.proof.xtdb-gateway-operation-target-binding-test
  "v1575: permanent pure adversarial regression for v1574's read-only XTDB
   operation/physical-target comparison boundary.

   This checks local shape equality, rejection behavior, redaction, and honest
   assurance labels. It intentionally does NOT install the checker in a Biff
   request path, authenticate the expected target, run XTDB, or prove that a
   structurally matching command implements the declared business rule. Those
   are separate outstanding execution/correspondence obligations."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding :as binding]
            [gesso.model.command :as command])
  (:import (java.math BigInteger)
           (java.util UUID)))

(def ^:private selected-operation :example/claim)
(def ^:private command-operation :claim)
(def ^:private table :claims)
(def ^:private document-id "private-v1575-request-42")
(def ^:private second-id "private-v1575-other-request")
(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- before [id]
  {:xt/id id
   :revision 0
   :created-at "creation-time"
   :updated-at "old-time"
   :owner :unclaimed})

(defn- update-command
  ([id] (update-command id command-operation))
  ([id operation]
   (let [b (before id)
         a (assoc b :revision 1 :updated-at "new-time" :owner :claimed)]
     (command/update-command table operation b a version))))

(defn- source
  ([id] (source id command-operation))
  ([id operation]
   {:plan {:commands [(update-command id operation)]
           :emit false}
    :protected-documents [{:target [table id] :fields :all}]}))

(defn- expectation
  ([id]
   {:operation-id selected-operation
    :command-operation command-operation
    :target [table id]})
  ([id operation-id operation]
   {:operation-id operation-id
    :command-operation operation
    :target [table id]}))

(defn- inspect [f]
  (try
    {:accepted (f)}
    (catch clojure.lang.ExceptionInfo e
      {:message (ex-message e) :data (ex-data e)})))

(defn- assert-rejection! [expected-reason f]
  (let [{:keys [data message] :as observation} (inspect f)]
    (is (not (contains? observation :accepted)) "Must reject before admission")
    (is (= true (:xtdb-gateway-operation-target-binding/rejection data))
        "Rejection must be attributed to the local binding checker")
    (is (= expected-reason (:reason data)) "Rejection reason must be typed")
    (is (= #{:xtdb-gateway-operation-target-binding/rejection :reason}
           (set (keys data)))
        "Error metadata must contain no document/command/request payload")
    (is (= "Unsupported XTDB operation/physical-target binding." message)
        "Error text must not include private source details")))

(defn- checked [expected candidate]
  (binding/require-declared-operation-target!
   selected-operation expected candidate))

(deftest matching-command-and-protected-target-is-only-a-conditional-report
  (let [report (checked (expectation document-id) (source document-id))]
    (is (= binding/report-type (:type report)))
    (is (= binding/report-version (:version report)))
    (is (= :conditional-declared-command-target-match (:status report)))
    (is (true? (:selected-route-operation-checked? report)))
    (is (true? (:canonical-command-operation-checked? report)))
    (is (true? (:declared-command-and-protected-document-match-expected-target?
                report)))
    (is (false? (:source-submitted? report)))
    (is (= :caller-supplied-not-authenticated (:expected-target-provenance report)))
    (is (= :not-established (:abstract-operation-to-command-semantics report)))
    (is (= :not-established (:complete-deployed-writer-coverage report)))
    (is (= :not-established (:physical-transaction-causality report)))
    (is (= :local-structural-comparison-only (:proof-status report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:transaction-authority report) (:authority report)))
    (is (= #{:independent-request-to-expected-target-provenance
             :certified-operation-to-model-command-semantics
             :actual-issued-writer-must-use-this-validated-candidate
             :transaction-commit-causality-and-physical-refinement
             :unmediated-or-external-writers-not-excluded}
           (:open-obligations report)))
    (is (not (str/includes? (pr-str report) document-id)))
    (is (not (str/includes? (pr-str report) second-id)))
    (is (not-any? #(contains? report %)
                  [:source :expectation :target :document-id :before :after
                   :command :plan :protected-documents :node :request :writer
                   :tx-key :tx-ops :certificate-data]))
    (is (= report (checked (expectation document-id) (source document-id)))
        "Identical input must yield a deterministic, unprivileged diagnostic")))

(deftest the-known-wrong-document-counterexample-is-rejected
  (testing "Physical command targets a different document, despite valid UPDATE"
    (let [candidate (source second-id)]
      (is (command/update? (first (get-in candidate [:plan :commands]))))
      (assert-rejection! :command-target-disagrees-with-expectation
                         #(checked (expectation document-id) candidate))))
  (testing "Even matching declared protection cannot excuse the substituted command"
    (assert-rejection! :command-target-disagrees-with-expectation
                       #(checked (expectation document-id)
                                 (assoc-in (source second-id)
                                           [:protected-documents 0 :target]
                                           [table document-id]))))
  (testing "A command targeting the expected document cannot protect another"
    (assert-rejection! :protected-document-disagrees-with-expectation
                       #(checked (expectation document-id)
                                 (assoc-in (source document-id)
                                           [:protected-documents 0 :target]
                                           [table second-id])))))

(deftest a-self-consistent-forged-expectation-is-not-independent-proof
  ;; If a host simply derives both expected and actual targets from the same
  ;; substituted command, this pure matcher cannot detect the provenance bug.
  ;; Positive result MUST remain conditional and confer zero authority.
  (let [report (checked (expectation second-id) (source second-id))]
    (is (= :conditional-declared-command-target-match (:status report)))
    (is (= :caller-supplied-not-authenticated (:expected-target-provenance report)))
    (is (= :none (:transaction-authority report)))
    (is (= :not-admitted (:admission report)))
    (is (contains? (:open-obligations report)
                   :independent-request-to-expected-target-provenance))))

(deftest selection-and-operation-disagreement-reject-before-submission
  (testing "Selected route must agree with independently supplied expectation"
    (assert-rejection! :selected-operation-disagrees-with-expectation
                       #(binding/require-declared-operation-target!
                         :example/cancel (expectation document-id)
                         (source document-id))))
  (testing "The model command must have the expected operation keyword"
    (assert-rejection! :command-operation-disagrees-with-expectation
                       #(checked (expectation document-id)
                                 (source document-id :cancel))))
  (testing "A matching command operation cannot excuse a wrong expected one"
    (assert-rejection! :command-operation-disagrees-with-expectation
                       #(checked (expectation document-id selected-operation :cancel)
                                 (source document-id))))
  (testing "Canonical create is not a one-command UPDATE"
    (let [created (command/create table (before document-id) version)]
      (is (command/create? created))
      (assert-rejection! :not-one-canonical-update-command
                         #(checked (expectation document-id)
                                   (assoc-in (source document-id)
                                             [:plan :commands] [created])))))
  (testing "A structurally invalid UPDATE is never accepted"
    (assert-rejection! :not-one-canonical-update-command
                       #(checked (expectation document-id)
                                 (update-in (source document-id)
                                            [:plan :commands 0]
                                            dissoc :model/expected)))))

(deftest unsupported-source-shapes-reject-without-guessing-meaning
  (let [valid (source document-id)
        cmd (first (get-in valid [:plan :commands]))
        cases
        {"nil source" nil
         "list source" (list valid)
         "extra source key" (assoc valid :certified true)
         "missing plan" (dissoc valid :plan)
         "missing protection" (dissoc valid :protected-documents)
         "metadata-bearing source" (with-meta valid {:checked true})
         "sorted source map" (into (sorted-map) valid)
         "extra plan key" (assoc-in valid [:plan :signed] true)
         "emit true" (assoc-in valid [:plan :emit] true)
         "emit nil" (assoc-in valid [:plan :emit] nil)
         "missing emit" (update valid :plan dissoc :emit)
         "no commands" (assoc-in valid [:plan :commands] [])
         "two commands" (assoc-in valid [:plan :commands] [cmd cmd])
         "commands list" (assoc-in valid [:plan :commands] (list cmd))
         "commands vector metadata" (assoc-in valid [:plan :commands]
                                              (with-meta [cmd] {:tag :unsafe}))
         "no protected documents" (assoc valid :protected-documents [])
         "two protected documents" (assoc valid :protected-documents
                                          (conj (:protected-documents valid)
                                                (first (:protected-documents valid))))
         "protected documents list" (assoc valid :protected-documents
                                            (list (first (:protected-documents valid))))
         "protected documents metadata" (assoc valid :protected-documents
                                                (with-meta (:protected-documents valid)
                                                  {:tag :unsafe}))}]
    (doseq [[label altered] cases]
      (testing label
        (assert-rejection! :unsupported-physical-source-shape
                           #(checked (expectation document-id) altered))))))

(deftest independent-expectation-must-be-a-small-plain-declaration
  (let [valid (expectation document-id)
        cases
        {"nil expectation" nil
         "string expectation" "claims"
         "extra assurance assertion" (assoc valid :certified true)
         "missing selected ID" (dissoc valid :operation-id)
         "missing command operation" (dissoc valid :command-operation)
         "missing target" (dissoc valid :target)
         "unqualified operation ID" (assoc valid :operation-id :claim)
         "string operation ID" (assoc valid :operation-id "example/claim")
         "string command operation" (assoc valid :command-operation "claim")
         "metadata-bearing expectation" (with-meta valid {:tag :untrusted})
         "sorted expectation" (into (sorted-map) valid)
         "non-vector target" (assoc valid :target (list table document-id))
         "one-part target" (assoc valid :target [table])
         "three-part target" (assoc valid :target [table document-id :extra])
         "metadata-bearing target" (assoc valid :target
                                               (with-meta [table document-id]
                                                 {:tag :untrusted}))
         "namespaced physical table" (assoc valid :target [:app/claims document-id])
         "hyphenated physical table" (assoc valid :target [:claim-records document-id])
         "invalid numeric table" (assoc valid :target [:42claims document-id])
         "nil document ID" (assoc valid :target [table nil])
         "empty document ID" (assoc valid :target [table ""])
         "oversized document ID" (assoc valid :target [table (apply str (repeat 513 "x"))])
         "keyword document ID" (assoc valid :target [table :document-id])
         "map document ID" (assoc valid :target [table {:id document-id}])
         "big-integer document ID" (assoc valid :target [table (BigInteger/valueOf 1)])
         "Clojure bigint document ID" (assoc valid :target [table 1N])}]
    (doseq [[label altered] cases]
      (testing label
        (assert-rejection! :invalid-independent-physical-expectation
                           #(checked altered (source document-id)))))))

(deftest invalid-selected-route-identity-rejects
  (doseq [bad [nil :claim "example/claim" 42 [] {}]]
    (testing (str "Unsupported selected identity of type " (some-> bad class .getName))
      (assert-rejection! :invalid-selected-operation-id
                         #(binding/require-declared-operation-target!
                           bad (expectation document-id) (source document-id))))))

(deftest protected-document-declaration-must-match-the-command-exactly
  (let [valid (source document-id)
        cases
        {"no complete field protection" (assoc-in valid [:protected-documents 0 :fields]
                                                   [:owner])
         "nil protection fields" (assoc-in valid [:protected-documents 0 :fields]
                                            nil)
         "extra protection key" (assoc-in valid [:protected-documents 0 :proof] true)
         "missing protection key" (update-in valid [:protected-documents 0]
                                              dissoc :fields)
         "metadata-bearing protection" (assoc-in valid [:protected-documents 0]
                                                  (with-meta (first (:protected-documents valid))
                                                    {:tag :untrusted}))
         "non-map protection" (assoc valid :protected-documents [[table document-id]])
         "wrong protected table" (assoc-in valid [:protected-documents 0 :target]
                                           [:other document-id])
         "wrong protected ID" (assoc-in valid [:protected-documents 0 :target]
                                        [table second-id])}]
    (doseq [[label altered] cases]
      (testing label
        (assert-rejection! :protected-document-disagrees-with-expectation
                           #(checked (expectation document-id) altered))))))

(deftest supported-physical-id-forms-are-compared-exactly
  (let [uuid (UUID/fromString "a791371e-d868-4f87-99e1-279f26607d25")]
    (doseq [id [0 42 -1 uuid "x" (apply str (repeat 512 "x"))]]
      (testing (str "One supported physical ID of type " (.getName (class id)))
        (is (= :conditional-declared-command-target-match
               (:status (checked (expectation id) (source id)))))))))

(deftest mismatches-are-redacted-in-errors-and-success-reports
  (let [sensitive "private-v1575-substituted-secret"
        report (checked (expectation sensitive) (source sensitive))
        failed (inspect #(checked (expectation document-id) (source sensitive)))]
    (is (not (str/includes? (pr-str report) sensitive)))
    (is (not (str/includes? (pr-str failed) sensitive)))
    (is (not (str/includes? (pr-str failed) document-id)))
    (is (= :command-target-disagrees-with-expectation
           (get-in failed [:data :reason])))))

(deftest command-checker-var-substitution-rejects-and-restores
  ;; The guard detects ordinary Var root substitution in this JVM. It is not
  ;; code integrity, an isolation boundary, or protection from every mutation.
  (let [valid-source (source document-id)
        valid-expectation (expectation document-id)]
    (doseq [[label binding-var]
            [["update?" #'command/update?]
             ["target" #'command/target]
             ["operation" #'command/operation]]]
      (testing label
        (with-redefs-fn {binding-var (constantly :substituted)}
          (fn []
            (assert-rejection! :substituted-command-checker
                               #(checked valid-expectation valid-source))))))
    (is (= :conditional-declared-command-target-match
           (:status (checked valid-expectation valid-source)))
        "Var roots restored; the legitimate checker still accepts ordinary input")))

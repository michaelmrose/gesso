(ns gesso.assurance.proof.xtdb-gateway-poststate-observation-test
  "v1534: Permanent, bounded adversarial tests of the v1533 restricted-gateway
   poststate observation. Exercise real embedded XTDB submissions, pinned
   reads and ledger joins, source/projection binding, forged local returns,
   outside writers, and read failures. The observed state is NOT authenticated
   as the causal effect of the submitted transaction, and the source module
   does not issue any application proof or complete-writer certificate.

   All direct xtdb.api/execute-tx calls in these tests are deliberate external
   writers. They are not retrospectively attributed to the one-shot gateway."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:private table :gesso_v1534_poststate_probe)
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private typed
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types {:request/status {:type :enum :name :request/status
                                  :values #{:open :claimed :done}}
                 :request/owner {:type :keyword :optional? true}
                 :request/version {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})
(def ^:private obligation
  {:construction typed
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants {:request/status-valid
                {:readers #{:request/status}
                 :expression [:in [:read :request/status]
                              [:literal #{:open :claimed :done}]]}
                :request/version-valid
                {:readers #{:request/version}
                 :expression [:>= [:read :request/version] [:literal 0]]}}})

(defonce ^:private !node (atom nil))
(defn- node []
  (or @!node (throw (ex-info "XTDB fixture not active" {}))))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(defn- fresh-id [] (str "v1534-" (random-uuid)))
(defn- document [id]
  {:xt/id id :revision 0 :created-at "v1534-secret-created"
   :updated-at "v1534-secret-unchanged" :value "v1534-secret-note"})
(defn- projection-source [id]
  {:obligation-source obligation
   :protected-domain-source
   {:plan {:commands [(command/create table (document id) version-keys)]
           :emit false}
    :protected-documents [{:target [table id] :fields :all}]}
   :field-locations
   {:request/status {:target [table id] :physical-field :revision}
    :request/owner {:target [table id] :physical-field :created-at}
    :request/version {:target [table id] :physical-field :updated-at}
    :request/note {:target [table id] :physical-field :value}}})
(defn- prepare [id]
  (let [s (projection-source id)]
    {:source s :gate (gateway/prepare-one-shot!
                      (node) (:protected-domain-source s))}))
(defn- submitted [id]
  (let [{:keys [source gate]} (prepare id)]
    {:source source :gate gate :result ((:submit! gate))}))
(defn- observation [{:keys [source gate result]}]
  (gateway/require-local-poststate-observation! gate result source))
(defn- rejection [f]
  (try (f) :accepted
       (catch clojure.lang.ExceptionInfo e
         (if (:xtdb-restricted-submission-gateway/rejection (ex-data e))
           (:reason (ex-data e))
           (throw e)))))
(defn- rejects [reason f]
  (is (= reason (rejection f))))
(defn- raw-read [id]
  (first (xt/q (node)
               ["SELECT * FROM gesso_v1534_poststate_probe WHERE _id = ?" id])))
(defn- external-put! [doc]
  (xt/execute-tx (node) [[:put-docs table doc]]))
(defn- external-delete! [id]
  (xt/execute-tx (node) [[:delete-docs table id]]))
(defn- no-second-submission [gate]
  (rejects :one-shot-already-attempted (:submit! gate))
  (is (= 1 (:owned-execute-tx-call-entries
             (gateway/require-local-attempt! gate)))))

(def ^:private expected-nonclaims
  {:caller-node-authenticity :not-established
   :transaction-internal-prestate :not-authenticated
   :physical-poststate-causality :not-established
   :physical-frame-preservation :not-established
   :physical-value-type-correspondence :not-established
   :outside-gateway-writer-closure :not-established
   :principal-authorization :not-established
   :universal-physical-refinement :not-established
   :proof-status :bounded-external-poststate-and-ledger-diagnostic-only
   :admission :not-admitted
   :certificate :not-issued
   :authority :none})

(deftest genuine-one-shot-poststate-agreement
  (let [id (fresh-id) {:keys [gate result] :as submitted} (submitted id)
        report (observation submitted)
        attempt (gateway/require-local-attempt! gate)]
    (is (instance? TransactionKey (:tx-key result)))
    (is (= :returned-transaction-key (:status attempt)))
    (is (= :reported-key-matches-committed-ledger-row (:ledger-status report)))
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status report)))
    (is (= :snapshot-not-earlier-than-transaction
           (:poststate-basis-ordering report)))
    (is (true? (:poststate-and-ledger-shared-read-basis? report)))
    (is (true? (:poststate-at-or-after-public-transaction-time? report)))
    (is (= (:process-local-attempt-id attempt)
           (:process-local-attempt-id report)))
    (is (= (:process-local-source-binding attempt)
           (:process-local-source-binding report)))
    (is (= (document id) (raw-read id)))
    (no-second-submission gate)))

(deftest bounded-positive-report-never-certifies-causality
  (let [id (fresh-id) s (submitted id) report (observation s)]
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status report)))
    (doseq [[key expected] expected-nonclaims]
      (is (= expected (get report key)) (str "Escalated assertion: " key)))
    (is (= :read-only-pinned-xt-txs-on-submission-node
           (:ledger-observation-origin
            (gateway/require-local-ledger-observation!
             (:gate s) (:result s)))))
    (is (= :bounded-pinned-external-read-on-captured-gateway-node
           (:poststate-observation-origin report)))
    (doseq [forbidden [:node :plan :source :protected-documents :tx-key
                       :tx-ops :snapshot-token :physical-acquisition
                       :observed-rows :external-read-basis :principal
                       :sql :prestate :poststate :document :tx-id]]
      (is (not (contains? report forbidden)) (str "Raw evidence: " forbidden)))
    (is (not (str/includes? (pr-str report) "v1534-secret")))))

(deftest substituted-plan-and-protection-are-refused
  (let [id (fresh-id) {:keys [source gate result]} (submitted id)]
    (rejects :projection-not-bound-to-gateway-source
             #(gateway/require-local-poststate-observation!
               gate result (assoc-in source [:protected-domain-source :plan :emit] true)))
    (rejects :projection-not-bound-to-gateway-source
             #(gateway/require-local-poststate-observation!
               gate result (assoc-in source [:protected-domain-source
                                            :protected-documents 0 :fields]
                                     #{:value})))
    (rejects :projection-not-bound-to-gateway-source
             #(gateway/require-local-poststate-observation!
               gate result (assoc-in source [:protected-domain-source
                                            :plan :commands]
                                     [])))
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status (observation {:source source :gate gate :result result}))))))

(deftest incorrect-projection-schema-is-refused
  (let [id (fresh-id) {:keys [source gate result]} (submitted id)]
    (doseq [malformed [nil {} (assoc source :node (node))
                       (dissoc source :field-locations)]]
      (let [reason (rejection #(gateway/require-local-poststate-observation!
                               gate result malformed))]
        (is (#{:projection-not-bound-to-gateway-source
               :invalid-physical-projection} reason)
            (str "Malformed projection accepted: " reason))))))

(deftest structurally-admitted-but-unreadable-field-mapping-remains-unavailable
  (let [id (fresh-id) {:keys [source gate result]} (submitted id)
        altered (assoc-in source [:field-locations :request/status]
                          {:target [table id] :physical-field :impossible})
        report (gateway/require-local-poststate-observation!
                gate result altered)]
    ;; An introspectable projection declaration does not establish physical
    ;; readability; the bounded read fails rather than manufacturing evidence.
    (is (= :reported-key-matches-committed-ledger-row (:ledger-status report)))
    (is (= :poststate-acquisition-unavailable (:poststate-status report)))
    (is (false? (:poststate-and-ledger-shared-read-basis? report)))
    (is (false? (:poststate-at-or-after-public-transaction-time? report)))
    (is (= :not-established (:physical-poststate-causality report)))
    (is (= :not-issued (:certificate report)))))

(deftest copied-and-altered-returns-cannot-trigger-poststate-reads
  (let [id (fresh-id) {:keys [source gate result] :as s} (submitted id)]
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation!
               gate (into {} result) source))
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation!
               gate (assoc result :tx-key nil) source))
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation!
               gate (assoc result :certificate :issued) source))
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status (observation s))))))

(deftest foreign-gateway-result-and-mixed-functions-refused
  (let [a (submitted (fresh-id)) b (submitted (fresh-id))
        ga (:gate a) ra (:result a) sa (:source a)
        gb (:gate b) rb (:result b)]
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation! ga rb sa))
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation! gb ra (:source b)))
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation!
               (assoc ga :summary! (:summary! gb)) ra sa))
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status (observation a))))))

(deftest result-before-submission-and-preflight-rejection-do-not-observe
  (let [id (fresh-id) {:keys [source gate]} (prepare id)]
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation! gate {} source))
    (is (= :prepared (:status (gateway/require-local-attempt! gate))))
    (let [result ((:submit! gate))]
      (is (instance? TransactionKey (:tx-key result)))
      (is (= :bounded-poststate-and-committed-ledger-agreement
             (:poststate-status
              (gateway/require-local-poststate-observation! gate result source)))))))

(deftest substituted-execution-var-preflight-rejection-does-not-become-observation
  (let [id (fresh-id) {:keys [source gate]} (prepare id)]
    (with-redefs [xt/execute-tx (fn [& _] (throw (ex-info "forged" {})))]
      (rejects :substituted-gateway-or-upstream-seam (:submit! gate)))
    (is (= :preflight-rejected
           (:status (gateway/require-local-attempt! gate))))
    (is (= 0 (:owned-execute-tx-call-entries
              (gateway/require-local-attempt! gate))))
    (is (nil? (raw-read id)))
    (rejects :not-this-gateway-local-return
             #(gateway/require-local-poststate-observation! gate {} source))
    (rejects :one-shot-already-attempted (:submit! gate))))

(deftest closed-captured-node-cannot-be-replaced-with-another-node
  (let [id (fresh-id)
        s (projection-source id)
        observed (with-open [n (xtn/start-node {})]
                   (let [gate (gateway/prepare-one-shot!
                               n (:protected-domain-source s))
                         result ((:submit! gate))]
                     {:gate gate :result result}))]
    ;; The gateway retains the original, now closed node. It must not accept
    ;; a new node argument or infer a ledger match from a different node.
    (is (instance? TransactionKey (:tx-key (:result observed))))
    (rejects :no-corroborated-gateway-ledger-return
             #(gateway/require-local-poststate-observation!
               (:gate observed) (:result observed) s))
    (no-second-submission (:gate observed))))

(deftest foreign-writer-mutates-mapped-field-with-matching-ledger
  (let [id (fresh-id) {:keys [gate result] :as s} (submitted id)
        outsider (assoc (document id) :value "outsider-v1534")]
    (is (instance? TransactionKey (external-put! outsider)))
    (let [report (observation s)]
      (is (= :bounded-poststate-and-committed-ledger-agreement
             (:poststate-status report)))
      (is (= :reported-key-matches-committed-ledger-row (:ledger-status report)))
      (is (= outsider (raw-read id)))
      (doseq [[key expected] expected-nonclaims]
        (is (= expected (get report key))))
      (no-second-submission gate))))

(deftest foreign-writer-can-delete-the-observed-document
  (let [id (fresh-id) s (submitted id)]
    (is (instance? TransactionKey (external-delete! id)))
    (is (nil? (raw-read id)))
    (let [report (observation s)]
      (is (= :reported-key-matches-committed-ledger-row (:ledger-status report)))
      (is (= :poststate-acquisition-unavailable (:poststate-status report)))
      (is (= :not-established (:poststate-basis-ordering report)))
      (is (false? (:poststate-and-ledger-shared-read-basis? report)))
      (is (false? (:poststate-at-or-after-public-transaction-time? report)))
      (is (= :not-established (:physical-poststate-causality report))))))

(deftest foreign-writer-can-add-undeclared-field
  (let [id (fresh-id) s (submitted id)]
    (is (instance? TransactionKey
                   (external-put! (assoc (document id) :injected "unmapped"))))
    (let [report (observation s)]
      (is (= :reported-key-matches-committed-ledger-row (:ledger-status report)))
      (is (= :poststate-acquisition-unavailable (:poststate-status report)))
      (is (= :not-established (:physical-frame-preservation report)))
      (is (= :not-issued (:certificate report))))))

(deftest forged-report-is-not-an-authority-to-resubmit
  (let [id (fresh-id) {:keys [gate result] :as s} (submitted id)
        report (observation s)
        forged (assoc report :certificate :issued :physical-poststate-causality :proved)]
    (is (= :issued (:certificate forged)))
    (is (= :not-issued (:certificate (observation s))))
    (is (= :not-established (:physical-poststate-causality (observation s))))
    (is (= :none (:authority (observation s))))
    (is (= :returned-transaction-key (:status (gateway/require-local-attempt! gate))))
    (no-second-submission gate)
    (is (instance? TransactionKey (:tx-key result)))))

(deftest substituted-source-checking-or-read-code-is-rejected
  (let [id (fresh-id) {:keys [gate result source] :as s} (submitted id)]
    (with-redefs [projection/describe-physical-domain-projection!
                  (fn [& _] {:type projection/projection-type
                             :version projection/projection-version
                             :certificate :not-issued :authority :none
                             :proof-status :declarations-only})]
      (rejects :substituted-physical-observation-seam
               #(gateway/require-local-poststate-observation! gate result source)))
    (with-redefs [poststate/acquire-poststate-basis!
                  (fn [& _] {:report {:status :bounded-poststate-and-committed-ledger-agreement}
                             :handle (Object.)})]
      (rejects :substituted-physical-observation-seam
               #(gateway/require-local-poststate-observation! gate result source)))
    (is (= :bounded-poststate-and-committed-ledger-agreement
           (:poststate-status (observation s))))))

(deftest first-success-observation-does-not-freeze-later-poststate
  (let [id (fresh-id) {:keys [gate] :as s} (submitted id)
        before (observation s)
        _ (external-put! (assoc (document id) :value "outside-later"))
        after (observation s)]
    (is (= :bounded-poststate-and-committed-ledger-agreement (:poststate-status before)))
    (is (= :bounded-poststate-and-committed-ledger-agreement (:poststate-status after)))
    (is (= (:process-local-attempt-id before) (:process-local-attempt-id after)))
    (is (= :not-established (:physical-poststate-causality after)))
    (is (= "outside-later" (:value (raw-read id))))
    (no-second-submission gate)))

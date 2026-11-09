(ns gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback-test
  "v1615: Permanent real-XTDB adversarial checks for v1614's strictly
   process-local two-write submission-return to pinned-readback bridge.

   Identity of the submitted return, captured readback inputs, and a matching
   physical snapshot are evidence of co-observation, not transaction causality.
   Failed observation after a committed write must never authorize replay."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback :as sut]
            [gesso.assurance.proof.xtdb-gateway-atomic-readback :as readback]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as complete]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:dynamic *node* nil)
(def ^:private domain-table :gesso_v1615_atomic_submitted_readback)
(def ^:private secret-domain "PRIVATE-V1615-domain-must-not-escape")
(def ^:private secret-outcome "PRIVATE-V1615-outcome-must-not-escape")
(def ^:private secret-external "PRIVATE-V1615-external-mutation")
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private report-keys
  #{:type :version :status :process-local-return :submitted-gateway
    :readback-node-and-inputs :readback-status :domain-poststate-status
    :command-record-status :ledger-status :basis-order-status
    :three-observations-share-pinned-basis? :public-key-origin
    :backend-node-authenticity :transaction-causality :record-immutability
    :principal-and-command-authenticity :external-writer-closure
    :universal-refinement :retry-authorized? :live-settlement-authority
    :transaction-authority :certificate :admission :authority :proof-status})

(use-fixtures :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (binding [*node* node] (run)))))

(defn- before []
  {:xt/id (str (random-uuid)) :revision 0 :created-at "start"
   :updated-at "start" :owner "open" :note secret-domain})
(defn- after [b]
  (assoc b :revision 1 :updated-at "claimed" :owner "claimed"))
(defn- source [b]
  {:plan {:commands [(command/update-command
                      domain-table :request/claim b (after b) version-keys)]
          :emit false}
   :protected-documents [{:target [domain-table (:xt/id b)] :fields :all}]})
(defn- incorporation-input [b]
  {:binding {:command-id (identity/command-id (str (random-uuid)))
             :principal (identity/principal "helper-v1615")
             :operation :request/claim
             :arguments {:request-id (:xt/id b)}}
   :outcome {:status :claimed :detail secret-outcome}
   :now "2026-11-07T00:00:00Z"})
(defn- expected-command-record [input]
  (resolution/command-record (:binding input) (:outcome input) (:now input)))
(defn- physical [table id]
  (first (xt/q *node* [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))
(defn- actual-domain [b]
  (physical domain-table (:xt/id b)))
(defn- actual-command [input]
  (physical resolution/command-entity-type
            (resolution/record-id (:binding input))))
(defn- seed! [b]
  (xt/execute-tx *node* [[:put-docs domain-table b]]))
(defn- stage! []
  (let [b (before)
        input (incorporation-input b)]
    (seed! b)
    {:before b :input input
     :gateway (sut/prepare-source-bound-atomic-return-readback-one-shot!
               *node* (source b) input)}))
(defn- rejection [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (is (= true (:xtdb-gateway-atomic-submitted-readback/rejection
                      (ex-data e))))
         (is (= #{:xtdb-gateway-atomic-submitted-readback/rejection :reason}
                (set (keys (ex-data e)))))
         (let [printed (pr-str (ex-data e))]
           (is (not (str/includes? printed secret-domain)))
           (is (not (str/includes? printed secret-outcome))))
         (:reason (ex-data e)))))
(defn- rejects! [expected f]
  (is (= expected (rejection f)) (str "Expected typed rejection " expected)))
(defn- no-authority! [report]
  (is (= report-keys (set (keys report)))
      "The report must not grow a private input or an authority-bearing field")
  (is (= sut/report-type (:type report)))
  (is (= sut/report-version (:version report)))
  (is (true? (:three-observations-share-pinned-basis? report)))
  (is (= :exact-object-from-this-issued-submission (:process-local-return report)))
  (is (= :process-local-submission-return (:public-key-origin report)))
  (doseq [[k v] {:transaction-causality :not-established
                 :backend-node-authenticity :caller-supplied-unattested
                 :record-immutability :not-established
                 :principal-and-command-authenticity :application-premise
                 :external-writer-closure :not-established
                 :universal-refinement :not-established
                 :retry-authorized? false
                 :live-settlement-authority :none
                 :transaction-authority :none
                 :certificate :not-issued :admission :not-admitted
                 :authority :none
                 :proof-status :bounded-process-local-return-and-pinned-coobservation}]
    (is (= v (get report k)) (str "No stronger assurance for " k)))
  (let [printed (pr-str report)]
    (doseq [secret [secret-domain secret-outcome secret-external]]
      (is (not (str/includes? printed secret))))
    (is (not (re-find #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" printed)))))
(defn- consumed! [g]
  (rejects! :one-shot-already-attempted (:submit! g))
  (is (= :permit-consumed (:submission-status ((:summary! g))))))

(deftest cannot-observe-unsubmitted-or-invented-return
  (let [{:keys [before input gateway]} (stage!)
        initial ((:summary! gateway))]
    (is (= #{:type :version :submit! :observe-return! :summary!}
           (set (keys gateway))))
    (is (= sut/gateway-type (:type gateway)))
    (is (= sut/gateway-version (:version gateway)))
    (is (= :prepared (:submission-status initial)))
    (is (= :not-observed (:readback-status initial)))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) {:tx-key nil}))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) nil))
    (is (= :not-observed (:readback-status ((:summary! gateway)))))
    (is (= before (actual-domain before)))
    (is (nil? (actual-command input)))))

(deftest authenticated-return-and-three-pinned-physical-observations
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        report ((:observe-return! gateway) response)]
    (is (instance? TransactionKey (:tx-key response)))
    (is (= (after before) (actual-domain before)))
    (is (= (expected-command-record input) (actual-command input)))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status report)))
    (is (= :bounded-poststate-command-and-ledger-coobservation
           (:readback-status report)))
    (is (= :matches-source-after (:domain-poststate-status report)))
    (is (= :matches-canonical-record (:command-record-status report)))
    (is (= :matching-committed-ledger-row (:ledger-status report)))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:readback-status ((:summary! gateway)))))
    (no-authority! report)
    (consumed! gateway)
    (rejects! :readback-already-attempted
              #((:observe-return! gateway) response))))

(deftest equal-but-distinct-return-and-foreign-transaction-key-are-not-admitted
  (let [{:keys [gateway]} (stage!)
        response ((:submit! gateway))
        copy (into {} response)
        unrelated-key (xt/execute-tx
                       *node* [[:put-docs :gesso_v1615_unrelated
                                 {:xt/id (str (random-uuid)) :note secret-external}]])]
    (is (= response copy))
    (is (not (identical? response copy)))
    (is (instance? TransactionKey unrelated-key))
    (is (not= unrelated-key (:tx-key response)))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) copy))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) (assoc response :tx-key unrelated-key)))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) unrelated-key))
    (is (= :not-observed (:readback-status ((:summary! gateway)))))
    (let [report ((:observe-return! gateway) response)]
      (is (= :bounded-local-return-and-pinned-readback-coobservation
             (:status report)))
      (no-authority! report))))

(deftest a-second-gateway-cannot-observe-the-first-gateways-return
  (let [{g1 :gateway} (stage!)
        {g2 :gateway before2 :before input2 :input} (stage!)
        response ((:submit! g1))]
    (rejects! :not-this-submission-local-return
              #((:observe-return! g2) response))
    (is (= :not-observed (:readback-status ((:summary! g2)))))
    (is (= before2 (actual-domain before2)))
    (is (nil? (actual-command input2)))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status ((:observe-return! g1) response))))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status ((:observe-return! g2) ((:submit! g2))))))))

(deftest domain-mutation-after-commit-prevents-positive-correspondence
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        external (assoc before :revision 7 :owner secret-external
                        :updated-at "modified-after-commit")]
    (xt/execute-tx *node* [[:put-docs domain-table external]])
    (let [report ((:observe-return! gateway) response)]
      (is (= external (actual-domain before)))
      (is (= (expected-command-record input) (actual-command input)))
      (is (= :postsubmission-correspondence-not-established (:status report)))
      (is (= :diverges (:domain-poststate-status report)))
      (no-authority! report)
      (rejects! :readback-already-attempted
                #((:observe-return! gateway) response)))))

(deftest command-record-mutation-after-commit-prevents-positive-correspondence
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        mutated (assoc (expected-command-record input) :unexpected secret-external)]
    (xt/execute-tx *node* [[:put-docs resolution/command-entity-type mutated]])
    (let [report ((:observe-return! gateway) response)]
      (is (= (after before) (actual-domain before)))
      (is (= mutated (actual-command input)))
      (is (= :postsubmission-correspondence-not-established (:status report)))
      (is (not= :matches-canonical-record (:command-record-status report)))
      (no-authority! report))))

(deftest substitution-before-issuance-is-rejected-without-writing
  (doseq [target [:submission :readback]]
    (let [b (before) input (incorporation-input b)]
      (seed! b)
      (case target
        :submission
        (with-redefs [complete/prepare-complete-guard-command-incorporation-one-shot!
                      (fn [& _] (throw (ex-info "substituted" {})))]
          (rejects! :substituted-submission-or-readback-seam
                    #(sut/prepare-source-bound-atomic-return-readback-one-shot!
                      *node* (source b) input)))
        :readback
        (with-redefs [readback/acquire-atomic-command-readback!
                      (fn [& _] {:status :forged})]
          (rejects! :substituted-submission-or-readback-seam
                    #(sut/prepare-source-bound-atomic-return-readback-one-shot!
                      *node* (source b) input))))
      (is (= b (actual-domain b)))
      (is (nil? (actual-command input))))))

(deftest replacing-submission-seam-after-issuance-consumes-writer
  (let [{:keys [before input gateway]} (stage!)]
    (with-redefs [complete/prepare-complete-guard-command-incorporation-one-shot!
                  (fn [& _] {:forged true})]
      (rejects! :substituted-submission-or-readback-seam (:submit! gateway)))
    (consumed! gateway)
    (is (= :not-observed (:readback-status ((:summary! gateway)))))
    (is (= before (actual-domain before)))
    (is (nil? (actual-command input)))))

(deftest replacing-readback-seam-after-real-commit-yields-unknown-observation
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))]
    (is (= (after before) (actual-domain before)))
    (is (= (expected-command-record input) (actual-command input)))
    (with-redefs [readback/acquire-atomic-command-readback!
                  (fn [& _] {:type readback/report-type
                             :version readback/report-version
                             :status :forged})]
      (rejects! :substituted-submission-or-readback-seam
                #((:observe-return! gateway) response)))
    (is (= :outcome-unknown-or-unobserved
           (:readback-status ((:summary! gateway)))))
    (rejects! :readback-already-attempted
              #((:observe-return! gateway) response))
    (consumed! gateway)
    (is (= (after before) (actual-domain before)))
    (is (= (expected-command-record input) (actual-command input)))))

(deftest replacing-submission-seam-after-real-commit-blocks_observation
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))]
    (with-redefs [complete/prepare-complete-guard-command-incorporation-one-shot!
                  (fn [& _] {:forged true})]
      (rejects! :substituted-submission-or-readback-seam
                #((:observe-return! gateway) response)))
    (is (= :outcome-unknown-or-unobserved
           (:readback-status ((:summary! gateway)))))
    (rejects! :readback-already-attempted
              #((:observe-return! gateway) response))
    (consumed! gateway)
    (is (= (after before) (actual-domain before)))
    (is (= (expected-command-record input) (actual-command input)))))

(ns gesso.assurance.proof.xtdb-gateway-atomic-incorporation-test
  "v1601: Permanent adversarial real-XTDB regressions for v1600's opt-in
   two-write restricted command-incorporation gateway.

   Under XTDB's bounded ASSERT/PUT atomicity assumption, the guarded domain
   UPDATE and canonical durable command-record CREATE either become visible
   together or neither is applied. Tests intentionally include direct
   external writes to challenge that claim. A locally observed transaction
   key, domain state, or command record does not prove universal causality,
   authorization, backend semantics, or complete writer adoption. The v1593
   Biff path is NOT switched to this new opt-in gateway."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as sut]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:dynamic *node* nil)
(def ^:private domain-table :gesso_v1601_atomic_requests)
(def ^:private private-note "PRIVATE-V1601-domain-note")
(def ^:private private-outcome "PRIVATE-V1601-incorporated-outcome")
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})

(use-fixtures :once
  (fn [run]
    (with-open [n (xtn/start-node {})]
      (binding [*node* n] (run)))))

(defn- before []
  {:xt/id (str (random-uuid))
   :revision 0 :created-at "start" :updated-at "start"
   :owner "open" :note private-note})
(defn- after [b]
  (assoc b :revision (inc (:revision b))
         :updated-at "claimed" :owner "claimed"))
(defn- domain-source [b]
  {:plan {:commands [(command/update-command
                     domain-table :claim b (after b) version-keys)]
          :emit false}
   :protected-documents [{:target [domain-table (:xt/id b)] :fields :all}]})
(defn- command-input
  ([b] (command-input b (str (random-uuid))))
  ([b id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "helper-v1601")
              :operation :request/claim
              :arguments {:request-id (:xt/id b)}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))
(defn- command-record [input]
  (resolution/command-record (:binding input) (:outcome input) (:now input)))
(defn- physical [table id]
  (first (xt/q *node*
               [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))
(defn- domain [b] (physical domain-table (:xt/id b)))
(defn- record [input]
  (physical resolution/command-entity-type
            (resolution/record-id (:binding input))))
(defn- external-domain! [b] (xt/execute-tx *node* [[:put-docs domain-table b]]))
(defn- external-record! [input]
  (xt/execute-tx *node* [[:put-docs resolution/command-entity-type
                             (command-record input)]]))
(defn- prepare! [b input]
  (sut/prepare-complete-guard-command-incorporation-one-shot!
   *node* (domain-source b) input))
(defn- staged! []
  (let [b (before) input (command-input b)]
    (external-domain! b)
    {:before b :input input :gate (prepare! b input)}))
(defn- error-code [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           code
           (throw e)))))
(defn- rejects! [expected f]
  (is (= expected (error-code f)) (str "Expected rejection " expected)))
(defn- consumed! [gate]
  (rejects! :one-shot-already-attempted (:submit! gate))
  (is (= :permit-consumed (:status ((:summary! gate))))))

(def ^:private report-claims
  {:admitted-transaction-write-count 2
   :single-restricted-submission :one-derived-xtdb-vector
   :domain-update :complete-top-level-cas
   :command-record-create :canonical-absence-precondition-prepared
   :atomicity :conditional-on-xtdb-assert-and-put-semantics
   :durable-record-readback :not-observed
   :physical-commit-observation :not-observed
   :command-binding-authenticity :application-premise
   :outcome-and-time-authenticity :application-premise
   :physical-prestate-acquisition :not-performed
   :certified-plan-target-binding :not-established
   :monotonic-revision-policy :not-established
   :biff-registered-writer-adoption :not-established
   :record-immutability-after-commit :not-established
   :indeterminate-outcome-resolution :not-established
   :physical-causality :not-established
   :universal-refinement :not-established
   :proof-status :conditional-two-write-atomic-plan-submission
   :certificate :not-issued
   :admission :not-admitted
   :authority :one-shot-physical-submission-only})
(def ^:private report-obligations
  #{:join-certified-plan-target-and-external-prestate
    :join-exact-successor-monotonic-revision
    :join-registered-biff-request-writer
    :authenticate-principal-outcome-and-timestamp
    :verify-command-record-readback-and-retention
    :assume-backend-atomic-assert-and-put-semantics
    :establish-deployment-wide-writer-closure})
(def ^:private allowed-report-keys
  (into #{:type :version :status :underlying-gateway-status
          :owned-execute-tx-call-entries :open-obligations}
        (keys report-claims)))
(defn- scoped-report! [r status underlying entries]
  (is (= sut/incorporation-type (:type r)))
  (is (= sut/incorporation-version (:version r)))
  (is (= status (:status r)))
  (is (= underlying (:underlying-gateway-status r)))
  (is (= entries (:owned-execute-tx-call-entries r)))
  (is (= allowed-report-keys (set (keys r)))
      "The diagnostic must expose only its fixed, redacted schema")
  (doseq [[k v] report-claims]
    (is (= v (get r k)) (str "Incorrect or overstated assurance claim: " k)))
  (is (= report-obligations (:open-obligations r)))
  (is (not (str/includes? (pr-str r) private-note)))
  (is (not (str/includes? (pr-str r) private-outcome)))
  (is (not (re-find #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                    (pr-str r)))))

(deftest accepted-submission-incorporates-domain-and-command-together
  (let [{:keys [before input gate]} (staged!)
        early ((:summary! gate))]
    (is (= #{:type :version :submit! :summary!} (set (keys gate)))
        "No second gateway, raw node, plan, or independent record-writer escapes")
    (scoped-report! early :prepared :prepared 0)
    (is (= before (domain before)))
    (is (nil? (record input)))
    (let [result ((:submit! gate))]
      (is (instance? TransactionKey (:tx-key result)))
      (is (= (after before) (domain before)))
      (is (= (command-record input) (record input)))
      (is (= :incorporated (:gesso.command/status (record input))))
      (scoped-report! ((:summary! gate)) :permit-consumed
                      :returned-transaction-key 1))
    (consumed! gate)))

(deftest preexisting-command-id-aborts-domain-write
  (let [{:keys [before input gate]} (staged!)
        seeded (command-record input)]
    (external-record! input)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= before (domain before)))
    (is (= seeded (record input)))
    (scoped-report! ((:summary! gate)) :permit-consumed
                    :threw-outcome-unknown 1)
    (consumed! gate)))

(deftest stale-domain-prestate-cannot-create-durable-command-record
  (let [{:keys [before input gate]} (staged!)
        intervening (assoc before :note "external-same-revision")]
    (external-domain! intervening)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= intervening (domain before)))
    (is (nil? (record input)))
    (consumed! gate)))

(deftest field-shape-drift-also-aborts-command-incorporation
  (doseq [change [(fn [b] (assoc b :extra-field "late"))
                  (fn [b] (dissoc b :note))
                  (fn [b] (-> b (dissoc :note)
                              (assoc :replacement "different-keys")))]]
    (let [{:keys [before input gate]} (staged!)
          intervening (change before)]
      (external-domain! intervening)
      (rejects! :xtdb/assert-failed (:submit! gate))
      (is (= intervening (domain before)))
      (is (nil? (record input)))
      (consumed! gate))))

(deftest duplicate-command-id-across-different-domain-documents-is-atomic
  (let [a (before) b (before) id (str (random-uuid))
        input-a (command-input a id)
        input-b (command-input b id)]
    (external-domain! a)
    (external-domain! b)
    (let [ga (prepare! a input-a) gb (prepare! b input-b)]
      (is (instance? TransactionKey (:tx-key ((:submit! ga)))))
      (rejects! :xtdb/assert-failed (:submit! gb))
      (is (= (after a) (domain a)))
      (is (= b (domain b)))
      (is (= (command-record input-a) (record input-a)))
      (is (not= (:binding input-a) (:binding input-b)))
      (consumed! ga)
      (consumed! gb))))

(deftest same-document-competing-command-ids-do-not-create-loser-record
  (let [b (before) input-a (command-input b) input-b (command-input b)]
    (external-domain! b)
    (let [ga (prepare! b input-a) gb (prepare! b input-b)]
      (is (instance? TransactionKey (:tx-key ((:submit! ga)))))
      (rejects! :xtdb/assert-failed (:submit! gb))
      (is (= (after b) (domain b)))
      (is (= (command-record input-a) (record input-a)))
      (is (nil? (record input-b)))
      (consumed! ga)
      (consumed! gb))))

(deftest same-document-same-command-id-replay-is-not-reexecuted
  (let [{:keys [before input gate]} (staged!)
        other (prepare! before input)]
    (is (instance? TransactionKey (:tx-key ((:submit! gate)))))
    (rejects! :xtdb/assert-failed (:submit! other))
    (is (= (after before) (domain before)))
    (is (= (command-record input) (record input)))
    (consumed! other)))

(deftest malformed-inputs-reject-before-any-submission
  (let [b (before) input (command-input b)]
    (external-domain! b)
    (doseq [bad [(dissoc input :now)
                 (assoc input :now nil)
                 (assoc input :binding (assoc (:binding input)
                                              :command-id "untyped"))
                 (assoc input :binding (assoc (:binding input)
                                              :principal "untyped"))
                 (assoc input :outcome {})
                 (assoc input :outcome {:bad (Object.)})
                 (assoc input :plan {:commands []})]]
      (is (not= :no-error
                (error-code #(sut/prepare-complete-guard-command-incorporation-one-shot!
                              *node* (domain-source b) bad)))))
    (is (= b (domain b)))
    (is (nil? (record input)))))

(deftest invalid-domain-sources-are-rejected-without-creating-records
  (let [b (before) input (command-input b) source (domain-source b)]
    (external-domain! b)
    (doseq [bad [(assoc-in source [:protected-documents 0 :target]
                            [domain-table "other-document"])
                 (assoc-in source [:plan :emit] true)
                 (assoc-in source [:plan :extra-option] true)
                 (update-in source [:plan :commands] conj
                            (first (get-in source [:plan :commands])))
                 (assoc source :extra-transaction [])
                 (assoc-in source [:protected-documents 0 :fields] #{:revision})]]
      (is (not= :no-error
                (error-code #(sut/prepare-complete-guard-command-incorporation-one-shot!
                              *node* bad input)))))
    (is (= b (domain b)))
    (is (nil? (record input)))))

(deftest substituted-gateway-or-resolution-construction-fails-closed
  (let [b (before) input (command-input b)]
    (external-domain! b)
    (with-redefs [gateway/prepare-one-shot! (fn [& _] {:submit! (fn [] :forged)})]
      (rejects! :substituted-preparation-seam #(prepare! b input)))
    (with-redefs [resolution/incorporation-plan (fn [& _] {:plan {}})]
      (rejects! :substituted-command-incorporation-seam #(prepare! b input)))
    (with-redefs [resolution/require-record (fn [_] :forged)]
      (rejects! :substituted-command-incorporation-seam #(prepare! b input)))
    (with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
      (rejects! :substituted-preparation-seam #(prepare! b input)))
    (is (= b (domain b)))
    (is (nil? (record input)))))

(deftest altered-seam-after-preparation-consumes-permit-without-writing
  (let [{:keys [before input gate]} (staged!)]
    (with-redefs [resolution/incorporation-plan (fn [& _] {:plan {}})]
      (rejects! :substituted-command-incorporation-seam (:submit! gate)))
    (is (= before (domain before)))
    (is (nil? (record input)))
    (is (= :permit-consumed (:status ((:summary! gate)))))
    (is (= :prepared (:underlying-gateway-status ((:summary! gate)))))
    (consumed! gate)))

(deftest concurrent-same-command-id-incorporation-has-one-winner
  (let [a (before) b (before) id (str (random-uuid))
        ia (command-input a id) ib (command-input b id)]
    (external-domain! a)
    (external-domain! b)
    (let [ga (prepare! a ia) gb (prepare! b ib)
          release (promise)
          invoke (fn [g]
                   (future
                     @release
                     (try {:status :returned :result ((:submit! g))}
                          (catch Throwable e
                            {:status :threw
                             :code (or (:reason (ex-data e))
                                       (:xtdb.error/code (ex-data e)))}))))
          fa (invoke ga) fb (invoke gb)]
      (deliver release true)
      (let [ra (deref fa 30000 ::timeout) rb (deref fb 30000 ::timeout)
            results [ra rb] winners (filter #(= :returned (:status %)) results)]
        (is (not-any? #{::timeout} results))
        (is (= 1 (count winners)))
        (is (= 1 (count (filter #(= :xtdb/assert-failed (:code %)) results))))
        (is (= 1 (count (filter true? [(= (after a) (domain a))
                                           (= (after b) (domain b))]))))
        (is (contains? #{(command-record ia) (command-record ib)} (record ia)))
        (is (or (and (= (after a) (domain a)) (= b (domain b)))
                (and (= a (domain a)) (= (after b) (domain b)))))
        (consumed! ga)
        (consumed! gb)))))

(deftest aba-restoration-is-not-claimed-as-excluded
  (let [{:keys [before input gate]} (staged!)]
    (external-domain! (assoc before :note "temporarily-replaced"))
    (external-domain! before)
    (is (= before (domain before)))
    (is (instance? TransactionKey (:tx-key ((:submit! gate)))))
    (is (= (after before) (domain before)))
    (is (= (command-record input) (record input)))
    (is (= :not-established
           (:physical-causality ((:summary! gate)))))))

(deftest observed-record-after-lost-response-does-not-relicense-consumed-gateway
  (let [{:keys [before input gate]} (staged!)
        observed (try
                   ((:submit! gate))
                   (throw (ex-info "Simulated lost response after physical commit" {}))
                   (catch clojure.lang.ExceptionInfo _ :response-lost))]
    (is (= :response-lost observed))
    (is (= (after before) (domain before)))
    (is (= (command-record input) (record input)))
    (consumed! gate)
    (is (= :not-established
           (:indeterminate-outcome-resolution ((:summary! gate)))))))

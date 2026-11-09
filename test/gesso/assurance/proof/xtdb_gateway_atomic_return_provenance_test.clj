(ns gesso.assurance.proof.xtdb-gateway-atomic-return-provenance-test
  "v1613: Permanent adversarial regression for v1612's exact atomic
   submission-return and process-local attempt linkage.

   Genuine embedded XTDB tests exercise two-write incorporation and the
   lower restricted gateway's identity-registered return/attempt APIs.
   Deliberate post-submit verifier failure is fault injection: the physical
   mutation may already have committed. This is NOT transaction causality,
   authenticated origin, universal refinement, or retry permission."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as sut]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:dynamic *node* nil)
(def ^:private table :gesso_v1613_atomic_provenance)
(def ^:private private-note "PRIVATE-v1613-domain-note")
(def ^:private private-outcome "PRIVATE-v1613-command-outcome")
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})

(use-fixtures :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (binding [*node* node] (run)))))

(defn- before []
  {:xt/id (str (random-uuid)) :revision 0 :created-at "start"
   :updated-at "start" :owner "open" :note private-note})
(defn- after [b]
  (assoc b :revision 1 :updated-at "claimed" :owner "claimed"))
(defn- source [b]
  {:plan {:commands [(command/update-command
                      table :request/claim b (after b) version-keys)]
          :emit false}
   :protected-documents [{:target [table (:xt/id b)] :fields :all}]})
(defn- command-input [b]
  {:binding {:command-id (identity/command-id (str (random-uuid)))
             :principal (identity/principal "helper-v1613")
             :operation :request/claim
             :arguments {:request-id (:xt/id b)}}
   :outcome {:status :claimed :detail private-outcome}
   :now "2026-11-07T00:00:00Z"})
(defn- command-record [input]
  (resolution/command-record (:binding input) (:outcome input) (:now input)))
(defn- physical [entity id]
  (first (xt/q *node* [(str "SELECT * FROM " (name entity) " WHERE _id = ?") id])))
(defn- domain [b] (physical table (:xt/id b)))
(defn- record [input]
  (physical resolution/command-entity-type
            (resolution/record-id (:binding input))))
(defn- seed! [b] (xt/execute-tx *node* [[:put-docs table b]]))
(defn- prepare! [b input]
  (sut/prepare-complete-guard-command-incorporation-one-shot!
   *node* (source b) input))
(defn- staged! []
  (let [b (before) input (command-input b)]
    (seed! b)
    {:before b :input input :prepared (prepare! b input)}))

(defn- reason [f]
  (try (f) :not-rejected
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           code (throw e)))))
(defn- rejects! [expected f]
  (is (= expected (reason f)) (str "Expected rejection " expected)))
(defn- consumed! [g]
  (rejects! :one-shot-already-attempted (:submit! g))
  (is (= :permit-consumed (:status ((:summary! g))))))
(defn- no-unearned-claims! [r]
  (doseq [[k v] {:physical-causality :not-established
                 :durable-record-readback :not-observed
                 :physical-commit-observation :not-observed
                 :record-immutability-after-commit :not-established
                 :indeterminate-outcome-resolution :not-established
                 :universal-refinement :not-established
                 :certificate :not-issued
                 :admission :not-admitted}]
    (is (= v (get r k)) (str "Unjustified claim: " k)))
  (is (not (str/includes? (pr-str r) private-note)))
  (is (not (str/includes? (pr-str r) private-outcome)))
  (is (not (re-find #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                    (pr-str r)))))

(deftest atomic-wrapper-returns-unaltered-physical-transaction-result
  (let [{:keys [before input prepared]} (staged!)
        report-before ((:summary! prepared))]
    (is (= #{:type :version :submit! :summary!} (set (keys prepared))))
    (is (= sut/incorporation-type (:type prepared)))
    (is (= sut/incorporation-version (:version prepared)))
    (is (= :prepared (:status report-before)))
    (is (= 0 (:owned-execute-tx-call-entries report-before)))
    (is (= before (domain before)))
    (is (nil? (record input)))
    (let [response ((:submit! prepared))
          report ((:summary! prepared))]
      (is (instance? TransactionKey (:tx-key response)))
      (is (= :returned-transaction-key (get-in response [:diagnostic :status])))
      (is (= :returned-transaction-key (:underlying-gateway-status report)))
      (is (= 1 (:owned-execute-tx-call-entries report)))
      (is (= (after before) (domain before)))
      (is (= (command-record input) (record input)))
      (is (= :permit-consumed (:status report)))
      (no-unearned-claims! report))
    (consumed! prepared)))

(deftest lower-gateway-requires-exact-return-object-not-equal-map
  (let [b (before)]
    (seed! b)
    (let [g (gateway/prepare-one-shot! *node* (source b))
          response ((:submit! g))
          exact (gateway/require-local-return! g response)]
      (is (instance? TransactionKey (:tx-key response)))
      (is (= gateway/local-return-type (:type exact)))
      (is (= :exact-object-from-owned-call-site (:local-return-linkage exact)))
      (is (= :process-local-result-correspondence-only (:proof-status exact)))
      (is (= 1 (:owned-execute-tx-call-entries exact)))
      (is (not (identical? response (into {} response))))
      (is (= response (into {} response)))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return! g (into {} response)))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return!
                  g (assoc response :diagnostic (assoc (:diagnostic response) :status :forged))))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return! g (:tx-key response)))
      (is (= (after b) (domain b)))
      (rejects! :one-shot-already-attempted (:submit! g)))))

(deftest local-attempt-journal-matches-exact-return-and-source-binding
  (let [b (before)]
    (seed! b)
    (let [g (gateway/prepare-one-shot! *node* (source b))
          initial (gateway/require-local-attempt! g)]
      (is (= :prepared (:status initial)))
      (is (= [:prepared] (:observed-local-events initial)))
      (is (= 0 (:owned-execute-tx-call-entries initial)))
      (let [response ((:submit! g))
            returned (gateway/require-local-return! g response)
            attempt (gateway/require-local-attempt! g)]
        (is (= :returned-transaction-key (:status attempt)))
        (is (= [:prepared :checking :submitting :returned-transaction-key]
               (:observed-local-events attempt)))
        (is (= :public-transaction-key (:observed-result-kind returned)
               (:observed-result-kind attempt)))
        (is (= 1 (:owned-execute-tx-call-entries returned)
               (:owned-execute-tx-call-entries attempt)))
        (is (= (:process-local-attempt-id returned)
               (:process-local-attempt-id attempt)))
        (is (= (:process-local-source-binding returned)
               (:process-local-source-binding attempt)))
        (is (= :process-local-nonauthenticating
               (:source-binding-scope attempt)))
        (is (= :not-established (:committed-transaction attempt)))
        (is (= :not-checked (:public-key-to-ledger-join returned)
               (:public-key-to-ledger-join attempt)))))))

(deftest a-foreign-registered-gateway-cannot-claim-another-return
  (let [a (before) b (before)]
    (seed! a) (seed! b)
    (let [ga (gateway/prepare-one-shot! *node* (source a))
          gb (gateway/prepare-one-shot! *node* (source b))
          ra ((:submit! ga))]
      (is (instance? TransactionKey (:tx-key ra)))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return! gb ra))
      (rejects! :not-this-gateway-local-attempt
                #(gateway/require-local-attempt!
                  (assoc gb :summary! (:summary! ga))))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return!
                  (assoc ga :summary! (:summary! gb)) ra))
      (is (= (after a) (domain a)))
      (is (= b (domain b)))
      (is (= :prepared (:status (gateway/require-local-attempt! gb)))))))

(deftest forged-gateway-functions-and-manufactured-results-are-refused
  (let [b (before)]
    (seed! b)
    (let [g (gateway/prepare-one-shot! *node* (source b))
          fake (assoc g :submit! (fn [] :forged))]
      (rejects! :not-this-gateway-local-attempt
                #(gateway/require-local-attempt! fake))
      (rejects! :not-this-gateway-local-return
                #(gateway/require-local-return! g {:tx-key nil :diagnostic {}}))
      (is (= :prepared (:status (gateway/require-local-attempt! g))))
      (is (= b (domain b))))))

(deftest replacing-return-verifier-before-preparation-is-fail-closed
  (let [b (before) input (command-input b)]
    (seed! b)
    (with-redefs [gateway/require-local-return! (fn [& _] {:forged true})]
      (rejects! :substituted-command-incorporation-seam
                #(prepare! b input)))
    (is (= b (domain b)))
    (is (nil? (record input)))))

(deftest replacing-attempt-verifier-before-preparation-is-fail-closed
  (let [b (before) input (command-input b)]
    (seed! b)
    (with-redefs [gateway/require-local-attempt! (fn [& _] {:forged true})]
      (rejects! :substituted-command-incorporation-seam
                #(prepare! b input)))
    (is (= b (domain b)))
    (is (nil? (record input)))))

(deftest replacing-return-verifier-after-preparation-consumes-permit
  (let [{:keys [before input prepared]} (staged!)]
    (with-redefs [gateway/require-local-return! (fn [& _] {:forged true})]
      (rejects! :substituted-command-incorporation-seam (:submit! prepared)))
    (is (= before (domain before)))
    (is (nil? (record input)))
    (is (= :prepared
           (:underlying-gateway-status ((:summary! prepared)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))
    (consumed! prepared)))

(deftest replacing-attempt-verifier-after-preparation-consumes-permit
  (let [{:keys [before input prepared]} (staged!)]
    (with-redefs [gateway/require-local-attempt! (fn [& _] {:forged true})]
      (rejects! :substituted-command-incorporation-seam (:submit! prepared)))
    (is (= before (domain before)))
    (is (nil? (record input)))
    (is (= :prepared
           (:underlying-gateway-status ((:summary! prepared)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))
    (consumed! prepared)))

(deftest post-submit-return-verification-failure-is-outcome-unknown
  (let [{:keys [before input prepared]} (staged!)
        seam (ns-resolve
              'gesso.assurance.proof.xtdb-gateway-complete-guard-execution
              'require-atomic-local-return!)]
    (is (var? seam))
    (when (var? seam)
      ;; Controlled post-submit fault injection. We change ONLY the private
      ;; verification continuation, not the issued restricted gateway or the
      ;; original XTDB call. The failure must not undo or relicense the write.
      (rejects! :injected-post-submit-verification-failure
                #(with-redefs-fn
                   {seam (fn [& _]
                           (throw (ex-info "Injected after execute-tx return"
                                           {:reason :injected-post-submit-verification-failure})))}
                   (:submit! prepared))))
    (is (= (after before) (domain before)))
    (is (= (command-record input) (record input)))
    (let [r ((:summary! prepared))]
      (is (= :permit-consumed (:status r)))
      (is (= :returned-transaction-key (:underlying-gateway-status r)))
      (is (= 1 (:owned-execute-tx-call-entries r)))
      (no-unearned-claims! r))
    (consumed! prepared)))

(deftest genuine-xtdb-conflict-has-no-successful-local-return
  (let [{:keys [before input prepared]} (staged!)
        intervening (assoc before :note "external-same-revision")]
    (seed! intervening)
    (rejects! :xtdb/assert-failed (:submit! prepared))
    (is (= intervening (domain before)))
    (is (nil? (record input)))
    (let [r ((:summary! prepared))]
      (is (= :threw-outcome-unknown (:underlying-gateway-status r)))
      (is (= 1 (:owned-execute-tx-call-entries r)))
      (no-unearned-claims! r))
    (consumed! prepared)))

(deftest response-loss-after-real-commit-does-not-license-retry
  (let [{:keys [before input prepared]} (staged!)
        status (try
                 ((:submit! prepared))
                 (throw (ex-info "Simulated lost HTTP response" {}))
                 (catch clojure.lang.ExceptionInfo _ :response-lost))]
    (is (= :response-lost status))
    (is (= (after before) (domain before)))
    (is (= (command-record input) (record input)))
    (consumed! prepared)
    (no-unearned-claims! ((:summary! prepared)))))

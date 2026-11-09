(ns gesso.assurance.proof.xtdb-gateway-plan-bound-incorporation-test
  "v1603: Permanent real-XTDB adversarial regressions for the opt-in v1602
   certified-plan-bound monotonic domain/command incorporation gateway.

   These tests use the genuine certified-plan executor, issued target callback,
   closed application fixture, pinned XTDB prestate, and one physical two-write
   transaction. They do not establish target-table business truth, argument or
   principal authorization, record retention, transaction causality, external
   writer closure, Biff adoption, or universal refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as sut]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution
             :as complete]
            [gesso.assurance.proof.xtdb-gateway-route-target-provenance
             :as provenance]
            [gesso.assurance.proof.xtdb-gateway-operation-target-binding
             :as target-binding]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

;; Keep the fixture's certified-plan issuer and actual XTDB node; do not
;; manufacture a certificate or a permissive mock executor for this test.
(defn- helper [sym]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      sym)]
    (when-not v
      (throw (ex-info "Missing certified XTDB integration fixture"
                      {:reason :missing-certified-xtdb-fixture})))
    (var-get v)))

(defn- node [] (helper '*node*))
(defn- certified [] (helper '*case*))
(defn- before [] ((helper 'before)))
(defn- after [b] ((helper 'after) b))
(defn- candidate [b] ((helper 'candidate) b))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))
(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-note "v1587-secret-must-not-be-disclosed")
(def ^:private private-outcome "V1603-PRIVATE-OUTCOME")

(use-fixtures :once (fn [f] ((helper 'with-certified-xtdb) f)))

(defn- command-input
  ([] (command-input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "helper-v1603")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))

(defn- record [input]
  (first (xt/q (node)
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding input))])))
(defn- expected-record [input]
  (resolution/command-record (:binding input) (:outcome input) (:now input)))
(defn- prepare!
  ([source input] (prepare! source input :revision))
  ([source input revision-field]
   (let [{:keys [contract issuance]} (certified)]
     (sut/prepare-plan-bound-monotonic-command-incorporation-one-shot!
      (node) contract issuance operation request source revision-field input))))

(defn- code-of [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           code
           (throw e)))))
(defn- rejects! [expected f]
  (is (= expected (code-of f)) (str "Expected typed rejection: " expected)))
(defn- seed! []
  (let [b (before)] (put! b) b))
(defn- staged! []
  (let [b (seed!) input (command-input)]
    {:before b :input input :gateway (prepare! (candidate b) input)}))
(defn- consumed! [g]
  (rejects! :one-shot-already-attempted (:submit! g))
  (is (= :permit-consumed (:status ((:summary! g))))))

(def ^:private claims
  {:physical-submission-gateways-prepared 1
   :admitted-transaction-write-count 2
   :domain-update :complete-top-level-cas
   :command-record-create :canonical-absence-precondition-prepared
   :atomicity :conditional-on-xtdb-assert-and-put-semantics
   :revision-policy :source-checked-exactly-one-nonwrapping-successor
   :original-revision-version-guard :required-and-source-matched
   :plan-target-correspondence :exact-current-issued-callback-checked-at-preparation
   :pre-submit-plan-target-recheck :required
   :external-prestate :same-node-pinned-read-matched-at-preparation
   :pre-submit-revision-recheck :required
   :command-record-operation :bound-to-selected-route
   :command-arguments-target-correspondence :not-established
   :application-target-table-truth :not-established
   :principal-authorization :not-established
   :outcome-and-time-authenticity :application-premise
   :physical-commit-causality :not-established
   :durable-record-readback :not-observed
   :record-immutability-after-commit :not-established
   :external-writer-aba :not-excluded
   :transaction-internal-prestate :not-authenticated
   :complete-deployment-writer-coverage :not-established
   :registered-biff-writer-adoption :not-established
   :abstract-operation-refinement :not-established
   :proof-status :conditional-plan-bound-atomic-incorporation
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :one-shot-submission-capability})
(def ^:private obligations
  #{:authenticate-principal-outcome-and-timestamp
    :check-command-arguments-against-selected-target
    :prove-static-target-table-business-correctness
    :verify-command-record-readback-and-retention
    :establish-deployment-wide-writer-closure
    :integrate-identity-registered-biff-request-writer
    :assume-xtdb-atomic-assert-and-put-semantics
    :establish-physical-commit-causality})

(defn- report! [r status]
  (is (= sut/plan-bound-incorporation-type (:type r)))
  (is (= sut/plan-bound-incorporation-version (:version r)))
  (is (= status (:status r)))
  (is (= (conj (set (keys claims))
               :type :version :status :underlying-gateway-status
               :owned-execute-tx-call-entries :open-obligations)
         (set (keys r)))
      "The diagnostic must not leak gateway internals or extra authority")
  (doseq [[k expected] claims]
    (is (= expected (get r k)) (str "Assurance overstated: " k)))
  (is (= obligations (:open-obligations r)))
  (is (not (str/includes? (pr-str r) private-id)))
  (is (not (str/includes? (pr-str r) private-note)))
  (is (not (str/includes? (pr-str r) private-outcome)))
  (is (not (re-find #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                    (pr-str r)))))

(deftest one-submission-incorporates-both-documents
  (let [{:keys [before input gateway]} (staged!)]
    (is (= #{:type :version :submit! :summary!} (set (keys gateway))))
    (report! ((:summary! gateway)) :prepared)
    (is (= :prepared (:underlying-gateway-status ((:summary! gateway)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! gateway)))))
    (is (= before (physical)))
    (is (nil? (record input)))
    (is (instance? TransactionKey (:tx-key ((:submit! gateway)))))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))
    (report! ((:summary! gateway)) :permit-consumed)
    (is (= :returned-transaction-key
           (:underlying-gateway-status ((:summary! gateway)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! gateway)))))
    (consumed! gateway)))

(deftest command-operation-binding-and-candidate-target-must-match
  (let [b (seed!) input (command-input) source (candidate b)]
    (rejects! :incorporation-binding-operation-disagrees-with-route
              #(prepare! source (assoc-in input [:binding :operation] :example/cancel)))
    (rejects! :command-or-protection-disagrees-with-plan-target
              #(prepare! (assoc-in source [:plan :commands 0 :model/operation] :cancel)
                         input))
    (rejects! :command-or-protection-disagrees-with-plan-target
              #(prepare! (assoc-in source [:protected-documents 0 :target]
                                          [:requests "different-request"])
                         input))
    (rejects! :command-or-protection-disagrees-with-plan-target
              #(prepare! (candidate (assoc b :xt/id "different-request")) input))
    (is (= b (physical)))
    (is (nil? (record input)))))

(deftest monotonic-policy-rejects-unjustified-revisions
  (let [b (seed!) input (command-input) source (candidate b)]
    (rejects! :revision-not-exact-successor
              #(prepare! (assoc-in source [:plan :commands 0 :model/after :revision] 9)
                         input))
    (rejects! :revision-not-exact-successor
              #(prepare! (update-in source [:plan :commands 0 :model/after]
                                    dissoc :revision) input))
    (rejects! :revision-not-originally-version-guarded
              #(prepare! (update-in source [:plan :commands 0 :model/expected :model/checks]
                                   (fn [checks]
                                     (vec (remove (fn [check] (= :revision (first check))) checks))))
                         input))
    (is (= b (physical)))
    (is (nil? (record input)))))

(deftest pinned-xtdb-before-rejects-forgery
  (let [b (seed!) input (command-input) source (candidate b)]
    (rejects! :incorporation-before-disagrees-with-external-read
              #(prepare! (assoc-in source [:plan :commands 0 :model/before :note]
                                          "forged") input))
    (rejects! :incorporation-before-disagrees-with-external-read
              #(prepare! (assoc-in source [:plan :commands 0 :model/before :extra]
                                          "invented") input))
    (is (= b (physical)))
    (is (nil? (record input)))))

(deftest duplicate-command-record-blocks-domain-write-atomically
  (let [{:keys [before input gateway]} (staged!)]
    (is (instance? TransactionKey (:tx-key ((:submit! gateway)))))
    (let [b2 (physical)
          repeat-gateway (prepare! (candidate b2) input)]
      (rejects! :xtdb/assert-failed (:submit! repeat-gateway))
      (is (= b2 (physical)))
      (is (= (expected-record input) (record input)))
      (consumed! repeat-gateway))))

(deftest stale-physical-before-prevents-command-incorporation
  (let [{:keys [before input gateway]} (staged!)
        outside (assoc before :note "outside-replaced-with-same-revision")]
    (put! outside)
    (rejects! :xtdb/assert-failed (:submit! gateway))
    (is (= outside (physical)))
    (is (nil? (record input)))
    (consumed! gateway)))

(deftest one-shot-permit-is-consumed-even-if-revalidation-fails
  (let [{:keys [before input gateway]} (staged!)]
    (with-redefs [target-binding/require-declared-operation-target!
                  (fn [& _] {:type :fake})]
      (rejects! :substituted-incorporation-correspondence-seam
                (:submit! gateway)))
    (is (= before (physical)))
    (is (nil? (record input)))
    (is (= :prepared (:underlying-gateway-status ((:summary! gateway)))))
    (consumed! gateway)))

(deftest substituted-source-bound-seams-reject-before-authority
  (let [b (seed!) input (command-input) source (candidate b)]
    (with-redefs [complete/prepare-complete-guard-command-incorporation-one-shot!
                  (fn [& _] {:submit! (fn [] :forged)})]
      (rejects! :substituted-incorporation-correspondence-seam
                #(prepare! source input)))
    (with-redefs [provenance/require-current-certified-literal-route-target-resolver!
                  (fn [& _] {:type :fake})]
      (rejects! :substituted-incorporation-correspondence-seam
                #(prepare! source input)))
    (with-redefs [xt/q (fn [& _] [{:snapshot-token "forged"}])]
      (rejects! :substituted-incorporation-correspondence-seam
                #(prepare! source input)))
    (is (= b (physical)))
    (is (nil? (record input)))))

(deftest issued-resolver-must-be-current-and-plan-bound
  (let [b (seed!) input (command-input) source (candidate b)
        {:keys [contract issuance]} (certified)]
    (rejects! :unissued-or-changed-target-resolver
              #(sut/prepare-plan-bound-monotonic-command-incorporation-one-shot!
                (node) contract {:expected-target! (fn [_] [:requests private-id])}
                operation request source :revision input))
    (rejects! :invalid-incorporation-target-input
              #(sut/prepare-plan-bound-monotonic-command-incorporation-one-shot!
                (node) contract (assoc issuance :extra true)
                operation request source :revision input))
    (is (= b (physical)))
    (is (nil? (record input)))))

(deftest response-loss-does-not-relicense-original-permit
  (let [{:keys [before input gateway]} (staged!)
        outcome (try
                  ((:submit! gateway))
                  (throw (ex-info "Simulated lost response" {}))
                  (catch clojure.lang.ExceptionInfo _ :response-lost))]
    (is (= :response-lost outcome))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))
    (consumed! gateway)
    (is (= :not-established (:physical-commit-causality ((:summary! gateway)))))))

(deftest competing-submissions-have-one-winner-and-no-orphan-record
  (let [b (seed!) a (command-input) b-input (command-input)
        ga (prepare! (candidate b) a)
        gb (prepare! (candidate b) b-input)
        start (promise)
        go! (fn [g]
              (future
                @start
                (try {:kind :returned :result ((:submit! g))}
                     (catch Throwable e
                       {:kind :threw
                        :code (or (:reason (ex-data e))
                                  (:xtdb.error/code (ex-data e)))}))))
        fa (go! ga) fb (go! gb)]
    (deliver start true)
    (let [ra (deref fa 30000 ::timeout)
          rb (deref fb 30000 ::timeout)
          rs [ra rb]]
      (is (not-any? #{::timeout} rs))
      (is (= 1 (count (filter #(= :returned (:kind %)) rs))))
      (is (= 1 (count (filter #(= :xtdb/assert-failed (:code %)) rs))))
      (is (= (after b) (physical)))
      (let [stored (filter some? [(record a) (record b-input)])]
        (is (= 1 (count stored)))
        (is (contains? #{(expected-record a) (expected-record b-input)}
                       (first stored))))
      (consumed! ga)
      (consumed! gb))))

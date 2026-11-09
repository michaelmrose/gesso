(ns gesso.assurance.proof.xtdb-gateway-unknown-command-reconciliation-test
  "v1599: Permanent real-XTDB, read-only regressions for v1598's
   independently pinned durable-command-record observer.

   A visible compatible record is not proof that the previous failed
   request wrote it. A missing record is not proof of noncommit. Neither
   observation authorizes a retry, a Live settlement or a physical write.
   The v1593 single-UPDATE path still does not atomically create command
   records; these tests deliberately seed command records independently and
   never infer their causal origin from the domain document.

   The redacted diagnostic is intended for trusted inspection only:
   exposing :unresolved to an untrusted client may leak record existence."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-unknown-command-reconciliation
             :as sut]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [xtdb.node :as xtn]))

(def ^:private secret "PRIVATE_V1599_COMMAND_OUTCOME")
(def ^:private secret-id "PRIVATE_V1599_COMMAND_ID")
(def ^:private typed-statuses
  {:replay :compatible-durable-record-observed-attempt-unattributed
   :unresolved :no-command-record-visible-noncommit-unproved
   :unavailable :record-disclosure-unavailable
   :incompatible :record-binding-incompatible})
(def ^:private permitted-report-keys
  #{:type :version :status :canonical-resolution-status
    :command-record-read :compatible-durable-record-observed?
    :prior-attempt-commit-attributed? :no-record-proves-noncommit?
    :domain-document-match-proves-command-incorporation?
    :unified-single-update-record-atomicity
    :command-binding-source-authenticity :submitted-payload-attribution
    :command-record-immutability :snapshot-and-node-authenticity
    :external-writer-closure :transaction-causality :retry-authorized?
    :automatic-retry :live-settlement-authority :transaction-authority
    :proof-status :certificate :admission :authority})

(def ^:dynamic *node* nil)

(defn- with-real-xtdb [f]
  (with-open [node (xtn/start-node {})]
    (binding [*node* node]
      (f))))

(use-fixtures :once with-real-xtdb)

(defn- binding-for
  ([] (binding-for (str (random-uuid))))
  ([id]
   {:command-id (identity/command-id id)
    :principal (identity/principal "test-helper")
    :operation :request/claim
    :arguments {:request-id "request-42"}}))

(defn- observe
  ([binding] (observe binding (fn [_ _] true)))
  ([binding may-disclose?]
   (sut/acquire-unknown-command-reconciliation!
    *node* binding may-disclose?)))

(defn- seed-command!
  ([binding] (seed-command! binding {:result :claimed :private secret}))
  ([binding outcome]
   (xt/execute-tx
    *node*
    [[:put-docs :gesso_commands
      (resolution/command-record binding outcome "2026-11-07T00:00:00Z")]])))

(defn- rejection [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- reject! [reason f]
  (let [r (rejection f)]
    (is (= {:xtdb-gateway-unknown-command-reconciliation/rejection true
            :reason reason}
           r)
        (str "Expected redacted rejection " reason))
    (is (not (str/includes? (pr-str r) secret)))
    (is (not (str/includes? (pr-str r) secret-id)))))

(defn- scoped-report! [report canonical-status]
  (is (= sut/report-type (:type report)))
  (is (= sut/report-version (:version report)))
  (is (= (typed-statuses canonical-status) (:status report)))
  (is (= canonical-status (:canonical-resolution-status report)))
  (is (= permitted-report-keys (set (keys report)))
      "Only the deliberately redacted fixed report schema may escape")
  (is (= :same-node-pinned-external-read-on-caller-node
         (:command-record-read report)))
  (is (= (= :replay canonical-status)
         (:compatible-durable-record-observed? report)))
  (doseq [k [:prior-attempt-commit-attributed?
             :no-record-proves-noncommit?
             :domain-document-match-proves-command-incorporation?
             :retry-authorized?]]
    (is (false? (get report k)) (str k " must remain false")))
  (doseq [k [:unified-single-update-record-atomicity
             :command-binding-source-authenticity
             :submitted-payload-attribution
             :command-record-immutability
             :snapshot-and-node-authenticity
             :external-writer-closure
             :transaction-causality]]
    (is (= :not-established (get report k))
        (str k " must remain unproved")))
  (doseq [[k v] {:automatic-retry :forbidden-by-this-observation
                 :live-settlement-authority :none
                 :transaction-authority :none
                 :proof-status :bounded-read-only-command-record-observation
                 :certificate :not-issued
                 :admission :not-admitted
                 :authority :none}]
    (is (= v (get report k)) (str "No authority: " k)))
  (is (not (str/includes? (pr-str report) secret)))
  (is (not (str/includes? (pr-str report) secret-id)))
  (is (not (contains? report :snapshot-token)))
  (is (not (contains? report :outcome)))
  (is (not (contains? report :record-present?))))

(deftest missing-command-record-does-not-establish-rejection-or-retry
  (testing "The pinned read's absent record remains unresolved"
    (let [binding (binding-for)
          calls (atom 0)
          r (observe binding (fn [_ _] (swap! calls inc) true))]
      (scoped-report! r :unresolved)
      (is (zero? @calls)
          "An absent record never evaluates the disclosure predicate"))))

(deftest matching-command-record-is-unattributed-and-does-not-disclose-outcome
  (let [binding (binding-for)
        checked (atom [])
        _ (seed-command! binding)
        r (observe binding (fn [b record]
                             (swap! checked conj [b record])
                             true))]
    (scoped-report! r :replay)
    (is (= 1 (count @checked)))
    (is (= binding (ffirst @checked)))
    (is (= {:result :claimed :private secret}
           (:gesso.command/outcome (second (first @checked))))
        "The trusted predicate sees the true record, but the report does not")
    (is (= r (observe binding (fn [_ _] true)))
        "Read-only observation is repeatable without issuing a permit")))

(deftest inaccessible-record-is-unavailable-not-replay
  (let [binding (binding-for)
        _ (seed-command! binding)
        r (observe binding (fn [_ _] false))]
    (scoped-report! r :unavailable)))

(deftest disclosure-predicate-must-return-literal-true
  (let [binding (binding-for)
        _ (seed-command! binding)]
    (doseq [v [nil false 0 :allowed "yes" [] {}]]
      (scoped-report! (observe binding (fn [_ _] v)) :unavailable))))

(deftest same-command-id-with-different-binding-is-incompatible
  (let [binding (binding-for)
        _ (seed-command! binding)]
    (doseq [candidate [(assoc binding :principal (identity/principal "someone-else"))
                       (assoc binding :operation :request/cancel)
                       (assoc binding :arguments {:request-id "request-43"})]]
      (scoped-report! (observe candidate) :incompatible)))
  (testing "Disclosure denial takes precedence over incompatibility"
    (let [binding (binding-for)
          _ (seed-command! binding)
          other (assoc binding :principal (identity/principal "different"))]
      (scoped-report! (observe other (fn [_ _] false)) :unavailable))))

(deftest a-different-command-record-does-not-satisfy-this-command
  (let [stored (binding-for)
        requested (binding-for)
        _ (seed-command! stored)]
    (scoped-report! (observe requested) :unresolved)
    (scoped-report! (observe stored) :replay)))

(deftest a-domain-write-does-not-establish-command-incorporation
  (let [binding (binding-for)
        _ (xt/execute-tx *node* [[:put-docs :requests
                                 {:xt/id (str (random-uuid))
                                  :status :claimed
                                  :note secret}]])]
    (scoped-report! (observe binding) :unresolved)))

(deftest malformed-persisted-command-record-fails-closed
  (let [binding (binding-for)
        record (assoc (resolution/command-record
                       binding {:result :claimed :private secret} "t1")
                      :gesso.command/revision 1)
        _ (xt/execute-tx *node* [[:put-docs :gesso_commands record]])]
    (reject! :invalid-record-or-disclosure-result #(observe binding))))

(deftest malformed-inputs-and-failed-disclosure-are-redacted
  (let [binding (binding-for secret-id)
        _ (seed-command! binding)]
    (reject! :missing-node-or-disclosure-predicate
             #(sut/acquire-unknown-command-reconciliation! nil binding (fn [_ _] true)))
    (doseq [bad [nil true {}]]
      (reject! :missing-node-or-disclosure-predicate
               #(observe binding bad)))
    (doseq [bad [(assoc binding :operation nil)
                 (assoc binding :arguments {:bad (Object.)})
                 (assoc binding :command-id (identity/command-id ""))
                 (dissoc binding :principal)]]
      (reject! :invalid-command-binding #(observe bad)))
    (reject! :invalid-record-or-disclosure-result
             #(observe binding
                       (fn [_ _]
                         (throw (ex-info "Private disclosure failure"
                                         {:secret secret})))))))

(deftest unreadable-node-rejects-instead-of-inventing-a-result
  (reject! :snapshot-unavailable
           #(sut/acquire-unknown-command-reconciliation!
             (Object.) (binding-for) (fn [_ _] true))))

(deftest substituted-implementation-seams-fail-closed
  (let [binding (binding-for)]
    (with-redefs [resolution/reconcile (fn [& _] {:status :replay})]
      (reject! :substituted-command-reconciliation-seam
               #(observe binding)))
    (with-redefs [resolution/require-binding (fn [b] b)]
      (reject! :substituted-command-reconciliation-seam
               #(observe binding)))
    (with-redefs [resolution/record-id (fn [_] "substituted")]
      (reject! :substituted-command-reconciliation-seam
               #(observe binding)))
    (with-redefs [xt/q (fn [& _] [])]
      (reject! :substituted-command-reconciliation-seam
               #(observe binding)))
    (with-redefs [basis/<-time-basis-str (fn [& _] {})]
      (reject! :substituted-command-reconciliation-seam
               #(observe binding)))))

(ns gesso.assurance.proof.xtdb-gateway-atomic-readback-test
  "v1609: Permanent real-XTDB adversarial checks for v1608's read-only,
   pinned-basis atomic domain/command/ledger co-observation.

   Matching physical poststate, canonical command record, and a committed
   ledger row do not establish that the supplied transaction created those
   documents. Even a completely matching result grants no retry, browser
   settlement, application admission, or transaction authority."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-atomic-readback :as sut]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]))

(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-note "v1587-secret-must-not-be-disclosed")
(def ^:private private-outcome "PRIVATE-V1609-command-outcome")
(def ^:private private-other "PRIVATE-V1609-external-mutation")
(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private permitted-report-keys
  #{:type :version :status :domain-poststate-status :command-record-status
    :ledger-status :basis-order-status :three-observations-share-pinned-basis?
    :observed-record-exactly-canonical? :observed-domain-exactly-source-after?
    :observed-ledger-matches-public-key? :key-attributed-to-command-creation?
    :submitted-transaction-contents-authenticated?
    :application-command-inputs-authenticated? :command-record-immutability
    :node-and-transaction-key-origin :transaction-causality
    :external-writer-closure :universal-operation-refinement
    :retry-authorized? :automatic-retry :transaction-authority
    :live-settlement-authority :certificate :admission :authority
    :proof-status})

(defn- fixture-helper [sym]
  (or (some-> (ns-resolve
               'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
               sym) var-get)
      (throw (ex-info "Missing certified XTDB fixture"
                      {:reason :missing-certified-fixture}))))

(use-fixtures :once (fn [run] ((fixture-helper 'with-certified-xtdb) run)))

(defn- node [] (fixture-helper '*node*))
(defn- certified [] (fixture-helper '*case*))
(defn- before [] ((fixture-helper 'before)))
(defn- after [b] ((fixture-helper 'after) b))
(defn- source [b] ((fixture-helper 'candidate) b))
(defn- seed-domain! [b] ((fixture-helper 'put!) b))
(defn- physical [] ((fixture-helper 'physical)))
(defn- persisted-command [input]
  (first (xt/q (node)
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding input))])))
(defn- incorporation-input
  ([] (incorporation-input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "v1609-test-principal")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :note private-outcome}
    :now "2026-11-07T00:00:00Z"}))
(defn- canonical-record [input]
  (resolution/command-record
   (:binding input) (:outcome input) (:now input)))
(defn- real-atomic-submit! [s input]
  (let [{:keys [contract issuance]} (certified)
        gate (monotonic/prepare-plan-bound-monotonic-command-incorporation-one-shot!
              (node) contract issuance operation request s :revision input)]
    (:tx-key ((:submit! gate)))))
(defn- observe [s input key]
  (sut/acquire-atomic-command-readback! (node) s input key))
(defn- unrelated-tx! []
  (xt/execute-tx
   (node) [[:put-docs :misc {:xt/id (str (random-uuid)) :note private-other}]]))
(defn- error-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- reject! [reason f]
  (let [data (error-data f)]
    (is (= {:xtdb-gateway-atomic-readback/rejection true
            :reason reason} data)
        (str "Expected redacted typed rejection " reason))
    (is (not (str/includes? (pr-str data) private-id)))
    (is (not (str/includes? (pr-str data) private-outcome)))
    (is (not (str/includes? (pr-str data) private-other)))))

(defn- scoped-report! [r]
  (is (= sut/report-type (:type r)))
  (is (= sut/report-version (:version r)))
  (is (= permitted-report-keys (set (keys r)))
      "Only the v1608 fixed, redacted diagnostic keys may escape")
  (is (contains? #{:bounded-poststate-command-and-ledger-coobservation
                   :postsubmission-correspondence-not-established}
                 (:status r)))
  (is (true? (:three-observations-share-pinned-basis? r)))
  (is (= (= :matches-canonical-record (:command-record-status r))
         (:observed-record-exactly-canonical? r)))
  (is (= (= :matches-source-after (:domain-poststate-status r))
         (:observed-domain-exactly-source-after? r)))
  (is (= (= :matching-committed-ledger-row (:ledger-status r))
         (:observed-ledger-matches-public-key? r)))
  (doseq [k [:key-attributed-to-command-creation?
             :submitted-transaction-contents-authenticated?
             :application-command-inputs-authenticated?
             :retry-authorized?]]
    (is (false? (get r k)) (str "Unproved claim: " k)))
  (doseq [k [:command-record-immutability
             :transaction-causality :external-writer-closure
             :universal-operation-refinement]]
    (is (= :not-established (get r k)) (str "Unproved: " k)))
  (doseq [[k expected]
          {:node-and-transaction-key-origin :caller-supplied-unattested
           :automatic-retry :forbidden-by-this-observation
           :transaction-authority :none :live-settlement-authority :none
           :certificate :not-issued :admission :not-admitted
           :authority :none
           :proof-status :bounded-postsubmission-read-only-coobservation}]
    (is (= expected (get r k)) (str "Incorrect authority: " k)))
  (doseq [secret [private-id private-note private-outcome private-other]]
    (is (not (str/includes? (pr-str r) secret))))
  (is (not-any? #(contains? r %)
                [:domain-id :command-id :record :domain :before :after :tx-key
                 :transaction-key :snapshot-token :node :source :binding
                 :outcome :system-time :transaction-ops :basis])))

(defn- staged! []
  (let [b (before) s (source b) input (incorporation-input)]
    (seed-domain! b)
    {:before b :source s :input input}))

(deftest real-atomic-incorporation-has-three-matching-observations
  (let [{:keys [before source input]} (staged!)
        key (real-atomic-submit! source input)
        r (observe source input key)]
    (is (= (after before) (physical)))
    (is (= (canonical-record input) (persisted-command input)))
    (scoped-report! r)
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status r)))
    (is (= :matches-source-after (:domain-poststate-status r)))
    (is (= :matches-canonical-record (:command-record-status r)))
    (is (= :matching-committed-ledger-row (:ledger-status r)))
    (is (= :basis-not-before-reported-transaction (:basis-order-status r)))
    (is (= r (observe source input key))
        "Readback is repeatable and never consumes or issues a write permit")))

(deftest genuine-unrelated-ledger-key-does-not-prove-causality
  (let [{:keys [source input]} (staged!)
        original (real-atomic-submit! source input)
        foreign (unrelated-tx!)
        r (observe source input foreign)]
    (is (not= original foreign))
    (scoped-report! r)
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status r)))
    (is (= :matching-committed-ledger-row (:ledger-status r)))
    (is (false? (:key-attributed-to-command-creation? r)))
    (is (= :not-established (:transaction-causality r)))))

(deftest independent-domain-and-record-writes-can-fool-coobservation
  (let [{:keys [before source input]} (staged!)
        domain-key (xt/execute-tx (node) [[:put-docs :requests (after before)]])
        record-key (xt/execute-tx (node) [[:put-docs :gesso_commands
                                            (canonical-record input)]])
        foreign-key (unrelated-tx!)
        r (observe source input foreign-key)]
    (is (not= domain-key record-key))
    (is (not= record-key foreign-key))
    (is (= (after before) (physical)))
    (is (= (canonical-record input) (persisted-command input)))
    (scoped-report! r)
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status r)))
    (is (false? (:submitted-transaction-contents-authenticated? r)))
    (is (= :not-established (:transaction-causality r)))))

(deftest matching-domain-and-foreign-ledger-cannot-replace-missing-record
  (let [{:keys [before source input]} (staged!)
        _ (xt/execute-tx (node) [[:put-docs :requests (after before)]])
        key (unrelated-tx!)
        r (observe source input key)]
    (scoped-report! r)
    (is (= :matches-source-after (:domain-poststate-status r)))
    (is (= :not-visible (:command-record-status r)))
    (is (= :matching-committed-ledger-row (:ledger-status r)))
    (is (= :postsubmission-correspondence-not-established (:status r)))
    (is (false? (:observed-record-exactly-canonical? r)))))

(deftest matching-record-with-unchanged-domain-is-not-positive
  (let [{:keys [before source input]} (staged!)
        _ (xt/execute-tx (node) [[:put-docs :gesso_commands
                                     (canonical-record input)]])
        key (unrelated-tx!)
        r (observe source input key)]
    (scoped-report! r)
    (is (= before (physical)))
    (is (= :diverges (:domain-poststate-status r)))
    (is (= :matches-canonical-record (:command-record-status r)))
    (is (= :postsubmission-correspondence-not-established (:status r)))))

(deftest missing-both-poststates-remains-unestablished
  (let [{:keys [source input]} (staged!)
        key (unrelated-tx!)
        r (observe source input key)]
    (scoped-report! r)
    (is (= :diverges (:domain-poststate-status r)))
    (is (= :not-visible (:command-record-status r)))
    (is (= :postsubmission-correspondence-not-established (:status r)))))

(deftest later-domain-update-breaks-a-previously-positive-observation
  (let [{:keys [before source input]} (staged!)
        key (real-atomic-submit! source input)
        initial (observe source input key)
        changed (assoc (after before) :note private-other)]
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status initial)))
    (xt/execute-tx (node) [[:put-docs :requests changed]])
    (let [r (observe source input key)]
      (scoped-report! r)
      (is (= changed (physical)))
      (is (= :diverges (:domain-poststate-status r)))
      (is (= :matches-canonical-record (:command-record-status r)))
      (is (= :postsubmission-correspondence-not-established (:status r))))))

(deftest later-command-record-mutation-breaks-previous-observation
  (let [{:keys [source input]} (staged!)
        key (real-atomic-submit! source input)
        initial (observe source input key)]
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status initial)))
    (xt/execute-tx (node) [[:put-docs :gesso_commands
                            (assoc (canonical-record input)
                                   :gesso.command/outcome {:status :tampered
                                                           :note private-other})]])
    (let [r (observe source input key)]
      (scoped-report! r)
      (is (= :matches-source-after (:domain-poststate-status r)))
      (is (= :diverges-or-invalid (:command-record-status r)))
      (is (= :postsubmission-correspondence-not-established (:status r))))))

(deftest malformed-persisted-command-record-is-not-canonical
  (let [{:keys [source input]} (staged!)
        key (real-atomic-submit! source input)]
    (xt/execute-tx (node) [[:put-docs :gesso_commands
                            (assoc (canonical-record input)
                                   :gesso.command/revision 4711)]])
    (let [r (observe source input key)]
      (scoped-report! r)
      (is (= :diverges-or-invalid (:command-record-status r)))
      (is (= :postsubmission-correspondence-not-established (:status r))))))

(deftest another-command-record-cannot-stand-in-for-requested-command
  (let [{:keys [source input]} (staged!)
        key (real-atomic-submit! source input)
        second-input (incorporation-input)
        r (observe source second-input key)]
    (scoped-report! r)
    (is (= :matches-source-after (:domain-poststate-status r)))
    (is (= :not-visible (:command-record-status r)))
    (is (= :postsubmission-correspondence-not-established (:status r)))))

(deftest invalid-key-node-and-source-reject-without-disclosing-data
  (let [{:keys [source input]} (staged!)
        valid-key (unrelated-tx!)]
    (reject! :missing-observation-node
             #(sut/acquire-atomic-command-readback! nil source input valid-key))
    (doseq [key [nil :not-a-key {} "transaction-1"]]
      (reject! :invalid-public-transaction-key #(observe source input key)))
    (doseq [bad [(dissoc source :protected-documents)
                 (assoc source :protected-documents [])
                 (assoc-in source [:protected-documents 0 :fields] :subset)
                 (assoc-in source [:protected-documents 0 :target]
                           [:requests "not-the-target"])
                 (assoc-in source [:plan :emit] true)
                 (assoc-in source [:plan :commands 0 :model/entity-type]
                           :unsafe/schema)
                 (assoc-in source [:plan :commands 0 :model/after :xt/id]
                           "wrong-id")]]
      (is (= :unsupported-domain-or-command-source
             (:reason (error-data #(observe bad input valid-key))))
          "Malformed physical source must not produce a matching readback"))
    (doseq [bad [(dissoc input :now)
                 (assoc input :now nil)
                 (assoc input :binding (dissoc (:binding input) :principal))
                 (assoc input :outcome {:secret (Object.)})]]
      (is (contains? #{:unsupported-domain-or-command-source
                       :invalid-canonical-command-record}
                     (:reason (error-data #(observe source bad valid-key))))
          "Malformed incorporation input must fail before physical readback"))))

(deftest command-operation-authenticity-is-not-established-by-readback
  (let [{:keys [before source input]} (staged!)
        key (real-atomic-submit! source input)
        altered (assoc-in source [:plan :commands 0 :model/operation]
                          :example/cancel)
        r (observe altered input key)]
    (is (= (after before) (physical)))
    (scoped-report! r)
    (is (= :bounded-poststate-command-and-ledger-coobservation (:status r))
        "Readback is a physical co-observer, not a certified-operation checker")
    (is (false? (:application-command-inputs-authenticated? r)))
    (is (= :not-established (:universal-operation-refinement r)))))

(deftest substituted-physical-or-model-observers-are-rejected
  (let [{:keys [source input]} (staged!)
        valid-key (unrelated-tx!)]
    (doseq [[label f]
            [[:query #(with-redefs [xt/q (fn [& _] [])]
                        (observe source input valid-key))]
             [:command-update #(with-redefs [command/update? (fn [_] true)]
                                 (observe source input valid-key))]
             [:record-builder #(with-redefs [resolution/command-record
                                             (fn [& _] {})]
                                 (observe source input valid-key))]
             [:record-verifier #(with-redefs [resolution/require-record
                                              (fn [& _] {})]
                                  (observe source input valid-key))]
             [:binding-checker #(with-redefs [resolution/require-binding
                                              (fn [_] nil)]
                                  (observe source input valid-key))]
             [:token-decoder #(with-redefs [basis/<-time-basis-str (fn [_] nil)]
                                (observe source input valid-key))]]]
      (testing (name label)
        (reject! :substituted-observation-seam f)))))

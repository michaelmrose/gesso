(ns gesso.assurance.proof.xtdb-gateway-scoped-atomic-readback-test
  "v1619: Permanent real-XTDB adversarial regression for v1618's issued,
   identity-registered, request-scoped atomic writer and exact-return readback.

   These are finite execution observations using a genuine certified plan,
   source-issued target resolver and embedded XTDB. They do NOT prove
   authorization, business-target authenticity, physical transaction
   causality, outside-writer closure, or universal refinement. A lost response
   or post-commit observer failure never authorizes transaction replay."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as sut]
            [gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback :as submitted]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

(def ^:private operation :example/claim)
(def ^:private path :host/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-note "v1587-secret-must-not-be-disclosed")
(def ^:private private-outcome "v1619-private-command-outcome")

;; The pre-existing fixture issues a real certified executor and exact
;; plan-target callback, closes a Gesso application, and starts embedded XTDB.
;; Its private helpers are test-only; no forged assurance report is supplied.
(defn- helper [sym]
  (or (some-> (ns-resolve
               'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
               sym) var-get)
      (throw (ex-info "Missing genuine certified XTDB fixture"
                      {:reason :missing-certified-xtdb-fixture}))))
(defn- node [] (helper '*node*))
(defn- certified [] (helper '*case*))
(defn- before [] ((helper 'before)))
(defn- after [b] ((helper 'after) b))
(defn- candidate [b] ((helper 'candidate) b))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))

(use-fixtures :once (fn [run] ((helper 'with-certified-xtdb) run)))

(defn- input
  ([] (input (str (random-uuid))))
  ([command-id]
   {:binding {:command-id (identity/command-id command-id)
              :principal (identity/principal "v1619-helper")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))

(defn- record [i]
  (first (xt/q (node)
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding i))])))
(defn- expected-record [i]
  (resolution/command-record (:binding i) (:outcome i) (:now i)))

(defn- issue!
  ([source i] (issue! path operation source i))
  ([path-id operation-id source i]
   (let [{:keys [contract issuance]} (certified)]
     (sut/issue-request-scoped-plan-bound-atomic-readback-writer!
      path-id operation-id (node) contract issuance request source :revision i))))
(defn- stage! []
  (let [b (before) i (input)]
    (put! b)
    {:before b :input i :issued (issue! (candidate b) i)}))
(defn- scope [issued] ((:request-scope-status! issued)))

(defn- reason [f]
  (try (f) :not-rejected
       (catch Throwable e
         (if-let [r (or (:reason (ex-data e))
                        (:xtdb.error/code (ex-data e)))]
           (do
             (when (:xtdb-gateway-writer-coverage/rejection (ex-data e))
               (is (= #{:reason :xtdb-gateway-writer-coverage/rejection}
                      (set (keys (ex-data e))))
                   "Writer rejection data must not expose private inputs")
               (doseq [secret [private-id private-note private-outcome]]
                 (is (not (str/includes?
                           (str (ex-message e) (pr-str (ex-data e))) secret)))))
             r)
           (throw e)))))
(defn- reject! [expected f]
  (is (= expected (reason f)) (str "Expected typed rejection " expected)))

(defn- nonclaims! [report]
  (is (= submitted/report-type (:type report)))
  (is (= submitted/report-version (:version report)))
  (doseq [[k v] {:process-local-return :exact-object-from-this-issued-submission
                 :submitted-gateway
                 :certified-plan-bound-monotonic-atomic-two-write-one-shot
                 :readback-node-and-inputs :same-captured-submission-inputs
                 :three-observations-share-pinned-basis? true
                 :public-key-origin :process-local-submission-return
                 :transaction-causality :not-established
                 :backend-node-authenticity :caller-supplied-unattested
                 :record-immutability :not-established
                 :principal-and-command-authenticity :application-premise
                 :external-writer-closure :not-established
                 :universal-refinement :not-established
                 :retry-authorized? false
                 :live-settlement-authority :none
                 :transaction-authority :none
                 :authority :none
                 :certificate :not-issued
                 :admission :not-admitted
                 :proof-status :bounded-process-local-return-and-pinned-coobservation}]
    (is (= v (get report k)) (str "Unestablished authority: " k)))
  (doseq [secret [private-id private-note private-outcome]]
    (is (not (str/includes? (pr-str report) secret))))
  (is (not-any? (set (keys report))
                [:source :node :before :after :target :raw-transaction
                 :command-input :record :tx-key :gateway :submit! :writer])))

(deftest issuance-has-opaque-identity-and-no-physical-effects
  (let [{:keys [before input issued]} (stage!)]
    (is (= #{:handle :writer :with-request! :request-scope-status!
             :observe-return!}
           (set (keys issued))))
    (is (= "#<gesso.assurance/issued-xtdb-writer>" (str (:handle issued))))
    (is (every? fn? [(:writer issued) (:with-request! issued)
                     (:request-scope-status! issued) (:observe-return! issued)]))
    (is (= :available (scope issued)))
    (is (= before (physical)))
    (is (nil? (record input)))
    (reject! :writer-outside-owning-request-scope (:writer issued))
    (reject! :readback-outside-owning-request-scope
             #((:observe-return! issued) {:tx-key nil}))
    (is (= :available (scope issued)))
    (is (= before (physical)))))

(deftest exact-result-submits-and-observes-once-without-changing-return
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer observe-return! with-request!]} issued]
    (let [response (with-request!
                     (fn []
                       (is (= :active (scope issued)))
                       (let [r (writer) report (observe-return! r)]
                         (is (instance? TransactionKey (:tx-key r)))
                         (is (= :bounded-local-return-and-pinned-readback-coobservation
                                (:status report)))
                         (is (= :matches-source-after
                                (:domain-poststate-status report)))
                         (is (= :matches-canonical-record
                                (:command-record-status report)))
                         (is (= :matching-committed-ledger-row
                                (:ledger-status report)))
                         (nonclaims! report)
                         (reject! :readback-already-attempted
                                  #(observe-return! r))
                         (reject! :one-shot-already-attempted writer)
                         r)))]
      (is (instance? TransactionKey (:tx-key response)))
      (is (= :closed (scope issued)))
      (reject! :writer-outside-owning-request-scope writer)
      (reject! :readback-outside-owning-request-scope
               #(observe-return! response))
      (reject! :request-scope-already-used-or-active
               #(with-request! writer))
      (is (= (after before) (physical)))
      (is (= (expected-record input) (record input))))))

(deftest foreign-and-copied-returns-do-not-consume-readback
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (reject! :not-the-registered-writer-return
                 #(observe-return! {:tx-key nil}))
        (let [r (writer)
              copied (into {} r)
              external (xt/execute-tx
                        (node) [[:put-docs :gesso_v1619_foreign
                                 {:xt/id (str (random-uuid)) :type :foreign}]])
              forged (assoc r :tx-key external)]
          (is (= r copied))
          (is (not (identical? r copied)))
          (is (instance? TransactionKey external))
          (is (not= external (:tx-key r)))
          (doseq [invalid [nil copied forged external]]
            (reject! :not-the-registered-writer-return
                     #(observe-return! invalid)))
          (is (= :bounded-local-return-and-pinned-readback-coobservation
                 (:status (observe-return! r)))))))
    (is (= :closed (scope issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))))

(deftest reader-requires-an-actual-return-from-this-registered-writer
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (reject! :not-the-registered-writer-return
                 #(observe-return! {:tx-key nil}))
        (is (= before (physical)))
        (let [r (writer)]
          (is (= :bounded-local-return-and-pinned-readback-coobservation
                 (:status (observe-return! r)))))))
    (is (= :closed (scope issued)))
    (is (= (expected-record input) (record input)))))

(deftest a-separate-issued-writer-cannot-use-anothers-result
  (let [{b :before i :input issued-a :issued} (stage!)
        other-i (input)
        issued-b (issue! (candidate b) other-i)
        response ((:with-request! issued-a) (:writer issued-a))]
    (is (= (after b) (physical)))
    (is (= (expected-record i) (record i)))
    ((:with-request! issued-b)
     (fn []
       (reject! :not-the-registered-writer-return
                #((:observe-return! issued-b) response))
       (is (= :active (scope issued-b)))
       (is (nil? (record other-i)))))
    (is (= :closed (scope issued-b)))
    (reject! :readback-outside-owning-request-scope
             #((:observe-return! issued-a) response))
    (is (nil? (record other-i)))))

(deftest cross-thread-writer-and-readback-do-not-use-owner-permits
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (is (= :writer-outside-owning-request-scope
               (deref (future (reason writer)) 30000 :timeout)))
        (let [r (writer)]
          (is (= :readback-outside-owning-request-scope
                 (deref (future (reason #(observe-return! r))) 30000 :timeout)))
          (is (= :bounded-local-return-and-pinned-readback-coobservation
                 (:status (observe-return! r)))))))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))))

(deftest invalid-arity-nesting-and-unused-scope-do-not-grant-authority
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (reject! :invalid-request-scope-dispatch #(with-request! nil))
    (is (= :available (scope issued)))
    (with-request!
      (fn []
        (reject! :request-scope-already-used-or-active
                 #(with-request! writer))
        (reject! :registered-writer-requires-zero-arguments #(writer :extra))
        (is (= before (physical)))
        (is (= :bounded-local-return-and-pinned-readback-coobservation
               (:status (let [result (writer)] (observe-return! result)))))))
    (is (= :closed (scope issued)))
    (is (= (expected-record input) (record input)))))

(deftest unused-or-throwing-request-closes-both-permits
  (doseq [throw? [false true]]
    (let [{:keys [before input issued]} (stage!)
          {:keys [writer with-request! observe-return!]} issued]
      (if throw?
        (reject! :simulated-handler-error
                 #(with-request!
                    (fn [] (throw (ex-info "Simulated handler failure"
                                           {:reason :simulated-handler-error})))))
        (is (= :handler-did-not-write
               (with-request! (fn [] :handler-did-not-write)))))
      (is (= :closed (scope issued)))
      (reject! :writer-outside-owning-request-scope writer)
      (reject! :readback-outside-owning-request-scope
               #(observe-return! {:tx-key nil}))
      (is (= before (physical)))
      (is (nil? (record input))))))

(deftest post-commit-response-loss-closes-scope-but-preserves-both-writes
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (reject! :lost-response-after-commit
             #(with-request!
                (fn []
                  (let [r (writer)]
                    (is (instance? TransactionKey (:tx-key r)))
                    (throw (ex-info "Simulated response loss"
                                    {:reason :lost-response-after-commit}))))))
    (is (= :closed (scope issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))
    (reject! :writer-outside-owning-request-scope writer)
    (reject! :readback-outside-owning-request-scope
             #(observe-return! {:tx-key nil}))))

(deftest post-commit-observer-seam-failure-consumes-readback-only
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (let [r (writer)]
          (with-redefs
            [monotonic/prepare-plan-bound-monotonic-command-incorporation-one-shot!
             (fn [& _] {:substituted true})]
            (reject! :substituted-incorporation-writer-issuer
                     #(observe-return! r)))
          (reject! :readback-already-attempted #(observe-return! r))
          (reject! :one-shot-already-attempted writer))))
    (is (= :closed (scope issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))))

(deftest later-domain-mutation-yields-no-positive-coobservation
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (let [r (writer)
              changed (assoc (after before) :note "foreign-post-commit-update")]
          (put! changed)
          (let [report (observe-return! r)]
            (is (= :postsubmission-correspondence-not-established
                   (:status report)))
            (is (= :diverges (:domain-poststate-status report)))
            (nonclaims! report)
            (reject! :readback-already-attempted #(observe-return! r))
            (is (= changed (physical)))))))
    (is (= (expected-record input) (record input)))))

(deftest later-command-record-mutation-yields-no-positive-coobservation
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued]
    (with-request!
      (fn []
        (let [r (writer)
              changed (assoc (expected-record input) :tampered "foreign")]
          (xt/execute-tx (node)
                         [[:put-docs resolution/command-entity-type changed]])
          (let [report (observe-return! r)]
            (is (= :postsubmission-correspondence-not-established
                   (:status report)))
            (is (not= :matches-canonical-record
                      (:command-record-status report)))
            (nonclaims! report)
            (reject! :readback-already-attempted #(observe-return! r))))))
    (is (= (after before) (physical)))))

(deftest stale-prestate-conflict-does-not-create-command-record
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request! observe-return!]} issued
        changed (assoc before :note "outside-same-revision-change")]
    (put! changed)
    (with-request!
      (fn []
        (reject! :xtdb/assert-failed writer)
        (reject! :one-shot-already-attempted writer)
        (reject! :not-the-registered-writer-return
                 #(observe-return! {:tx-key nil}))))
    (is (= changed (physical)))
    (is (nil? (record input)))))

(deftest foreign-or-forged-issuance-inputs-never-produce-writers
  (let [b (before) i (input) {:keys [contract issuance]} (certified)]
    (put! b)
    (reject! :invalid-path-or-operation-id
             #(issue! :unqualified operation (candidate b) i))
    (reject! :incorporation-binding-operation-disagrees-with-route
             #(issue! (candidate b)
                      (assoc-in i [:binding :operation] :example/cancel)))
    (reject! :incorporation-before-disagrees-with-external-read
             #(issue! (candidate (assoc b :note "forged")) i))
    (reject! :command-or-protection-disagrees-with-plan-target
             #(issue! (assoc-in (candidate b)
                                [:protected-documents 0 :target]
                                [:requests "wrong-document"]) i))
    (reject! :unissued-or-changed-target-resolver
             #(sut/issue-request-scoped-plan-bound-atomic-readback-writer!
               path operation (node) contract {:expected-target! (fn [_] [:requests private-id])}
               request (candidate b) :revision i))
    (is (= b (physical)))
    (is (nil? (record i)))))

(deftest source-substitution-is-rejected-before-and-during-request
  (let [{:keys [before input issued]} (stage!)
        {:keys [writer with-request!]} issued]
    (with-redefs [submitted/prepare-plan-bound-atomic-return-readback-one-shot!
                  (fn [& _] {:counterfeit true})]
      (reject! :substituted-atomic-return-readback-issuer
               #(issue! (candidate before) input))
      (reject! :substituted-atomic-return-readback-issuer
               #(with-request! writer)))
    (is (= :available (scope issued)))
    (is (= before (physical)))
    (is (nil? (record input)))
    (is (instance? TransactionKey (:tx-key (with-request! writer))))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))))

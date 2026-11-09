(ns gesso.assurance.proof.xtdb-gateway-atomic-writer-test
  "v1605: permanent adversarial tests for the v1604 issued request-scoped,
   plan-bound, single-transaction domain UPDATE and durable command CREATE
   writer. A genuine certified application fixture and embedded XTDB exercise
   physical effects; the writer is NOT yet installed by a Biff mediator.
   Observations do not establish authorization, command-argument correctness,
   physical commit causality, external-writer closure, or universal refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-writer-coverage :as sut]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

;; The fixture's private helpers are intentionally accessed by name. It
;; creates the genuine certified plan, issued plan-target callback, checked
;; application and XTDB node. No forged certificate or substitute node.
(defn- helper [name]
  (let [v (ns-resolve
           'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
           name)]
    (when-not v
      (throw (ex-info "Missing certified XTDB fixture"
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
(def ^:private path :host/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-note "v1587-secret-must-not-be-disclosed")
(def ^:private private-outcome "v1605-private-command-result")

(use-fixtures :once (fn [run] ((helper 'with-certified-xtdb) run)))

(defn- input
  ([] (input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "v1605-helper")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))

(defn- record [command-input]
  (first (xt/q (node)
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding command-input))])))
(defn- expected-record [command-input]
  (resolution/command-record
   (:binding command-input) (:outcome command-input) (:now command-input)))

(defn- issue!
  ([source command-input] (issue! path operation source command-input))
  ([path-id operation-id source command-input]
   (let [{:keys [contract issuance]} (certified)]
     (sut/issue-request-scoped-plan-bound-atomic-incorporation-writer!
      path-id operation-id (node) contract issuance request source :revision
      command-input))))

(defn- reason [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [r (or (:reason (ex-data e))
                        (:xtdb.error/code (ex-data e)))]
           r
           (throw e)))))
(defn- reject! [expected f]
  (is (= expected (reason f)) (str "Expected typed rejection: " expected)))
(defn- seed! []
  (let [b (before)] (put! b) b))
(defn- staged! []
  (let [b (seed!) command-input (input)]
    {:before b :command-input command-input
     :issued (issue! (candidate b) command-input)}))
(defn- scope! [issued] ((:request-scope-status! issued)))

(deftest genuine-issuance-is-opaque-and-does-not-write
  (let [{:keys [before command-input issued]} (staged!)]
    (is (= #{:handle :writer :with-request! :request-scope-status!}
           (set (keys issued))))
    (is (= "#<gesso.assurance/issued-xtdb-writer>"
           (str (:handle issued))))
    (is (fn? (:writer issued)))
    (is (fn? (:with-request! issued)))
    (is (= :available (scope! issued)))
    (is (= before (physical)))
    (is (nil? (record command-input)))
    (reject! :writer-outside-owning-request-scope (:writer issued))
    (is (= :available (scope! issued)))
    (is (= before (physical)))))

(deftest one-scope-atomically-incorporates-domain-and-command-record
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (is (instance? TransactionKey (:tx-key (with-request! writer))))
    (is (= :closed (scope! issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))
    (reject! :writer-outside-owning-request-scope writer)
    (reject! :request-scope-already-used-or-active
             #(with-request! writer))
    (is (= (expected-record command-input) (record command-input)))))

(deftest repeat-in-original-owning-scope-is-consumed-not-reexecuted
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (is (= :one-shot-already-attempted
           (with-request! #(do (writer) (reason writer)))))
    (is (= :closed (scope! issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))))

(deftest invalid-arity-does-not-consume-the-writer
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (with-request!
      (fn []
        (reject! :registered-writer-requires-zero-arguments #(writer :extra))
        (is (= before (physical)))
        (is (nil? (record command-input)))
        (is (instance? TransactionKey (:tx-key (writer))))))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))))

(deftest unused-or-throwing-scope-closes-without-writing
  (doseq [throw? [false true]]
    (let [{:keys [before command-input issued]} (staged!)
          {:keys [writer with-request!]} issued]
      (if throw?
        (reject! :request-aborted
                 #(with-request!
                    (fn [] (throw (ex-info "simulated response error"
                                           {:reason :request-aborted})))))
        (is (= :nothing-to-write (with-request! (fn [] :nothing-to-write)))))
      (is (= :closed (scope! issued)))
      (reject! :writer-outside-owning-request-scope writer)
      (reject! :request-scope-already-used-or-active
               #(with-request! writer))
      (is (= before (physical)))
      (is (nil? (record command-input))))))

(deftest nested-scope-and-invalid-dispatch-do-not-bypass-ownership
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (reject! :invalid-request-scope-dispatch #(with-request! nil))
    (is (= :available (scope! issued)))
    (is (instance? TransactionKey
                   (:tx-key
                    (with-request!
                      (fn []
                        (is (= :active (scope! issued)))
                        (reject! :request-scope-already-used-or-active
                                 #(with-request! writer))
                        (writer))))))
    (is (= :closed (scope! issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))))

(deftest owning-thread-is-mandatory-even-inside-live-scope
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (is (= :writer-outside-owning-request-scope
           (with-request! #(deref (future (reason writer))))))
    (is (= :closed (scope! issued)))
    (is (= before (physical)))
    (is (nil? (record command-input)))))

(deftest operation-target-and-prestate-rejections-never-create-records
  (let [b (seed!) command-input (input)]
    (reject! :incorporation-binding-operation-disagrees-with-route
             #(issue! (candidate b)
                      (assoc-in command-input [:binding :operation]
                                :example/cancel)))
    (reject! :incorporation-before-disagrees-with-external-read
             #(issue! (candidate (assoc b :owner "forged")) command-input))
    (reject! :incorporation-before-disagrees-with-external-read
             #(issue! (candidate (assoc b :note "forged")) command-input))
    (reject! :invalid-path-or-operation-id
             #(issue! :unqualified operation (candidate b) command-input))
    (reject! :invalid-path-or-operation-id
             #(issue! path :unqualified (candidate b) command-input))
    (is (= b (physical)))
    (is (nil? (record command-input)))))

(deftest wrong-physical-document-and-protected-target-fail-closed
  (let [b (seed!) command-input (input)
        wrong (assoc b :xt/id (str (random-uuid)))
        source (candidate b)]
    (is (not= :no-error (reason #(issue! (candidate wrong) command-input))))
    (is (not= :no-error
              (reason #(issue! (assoc-in source [:protected-documents 0 :target]
                                         [:requests (:xt/id wrong)]) command-input))))
    (is (= b (physical)))
    (is (nil? (record command-input)))
    (is (nil? (first (xt/q (node)
                            ["SELECT * FROM requests WHERE _id = ?"
                             (:xt/id wrong)]))))))

(deftest intervening-physical-write-conflicts-without-partial-incorporation
  (let [{:keys [before command-input issued]} (staged!)
        changed (assoc before :note "independent-writer-same-revision")
        {:keys [writer with-request!]} issued]
    (put! changed)
    (is (= :one-shot-already-attempted
           (with-request! #(do
                            (reject! :xtdb/assert-failed writer)
                            (reason writer)))))
    ;; The second invocation above is attempted after the conflict and must
    ;; be rejected as consumed, not permitted to issue another transaction.
    (is (= changed (physical)))
    (is (nil? (record command-input)))
    (is (= :closed (scope! issued)))))

(deftest duplicate-command-id-cannot-advance-second-domain-transaction
  (let [b (seed!) id (str (random-uuid))
        a (input id)
        first-issued (issue! (candidate b) a)
        second-issued (issue! (candidate b) a)]
    (is (instance? TransactionKey
                   (:tx-key ((:with-request! first-issued) (:writer first-issued)))))
    (is (= (after b) (physical)))
    (is (= (expected-record a) (record a)))
    (is (= :xtdb/assert-failed
           (reason #((:with-request! second-issued) (:writer second-issued)))))
    (is (= :closed (scope! second-issued)))
    (is (= (after b) (physical)))
    (is (= (expected-record a) (record a)))))

(deftest throw-after-physical-commit-preserves-durable-record-and-closes-scope
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued]
    (reject! :lost-http-response
             #(with-request!
                (fn [] (writer)
                  (throw (ex-info "simulated response loss"
                                  {:reason :lost-http-response})))))
    (is (= :closed (scope! issued)))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))
    (reject! :writer-outside-owning-request-scope writer)))

(deftest replaced-gateway-constructor-fails-before-and-during-scope
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request!]} issued
        replacement (fn [& _] :substituted)]
    (with-redefs [monotonic/prepare-plan-bound-monotonic-command-incorporation-one-shot!
                  replacement]
      (reject! :substituted-incorporation-writer-issuer
               #(issue! (candidate before) (input)))
      (reject! :substituted-incorporation-writer-issuer
               #(with-request! writer)))
    (is (= :available (scope! issued))
        "A substituted seam must not run the request or burn its scope")
    (is (= before (physical)))
    (is (nil? (record command-input)))
    (is (instance? TransactionKey
                   (:tx-key (with-request! writer))))
    (is (= (after before) (physical)))
    (is (= (expected-record command-input) (record command-input)))))

(deftest exception-data-is-redacted-and-not-a-certificate
  (let [{:keys [before command-input issued]} (staged!)
        {:keys [writer with-request! handle]} issued
        error (try (writer) nil (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= #{:xtdb-gateway-writer-coverage/rejection :reason}
           (set (keys (ex-data error)))))
    (is (= :writer-outside-owning-request-scope
           (:reason (ex-data error))))
    (is (= "#<gesso.assurance/issued-xtdb-writer>" (str handle)))
    (doseq [secret [private-id private-note private-outcome]]
      (is (not (str/includes? (str (ex-message error) (pr-str (ex-data error)))
                             secret))))
    (is (= before (physical)))
    (is (nil? (record command-input)))
    (is (not-any? (set (keys issued))
                  [:certificate :admission :tx-key :node :raw-gateway
                   :submit! :target :command-record]))))

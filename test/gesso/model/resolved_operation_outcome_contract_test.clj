(ns gesso.model.resolved-operation-outcome-contract-test
  "Permanent adversarial regressions for the optional closed command-outcome
   contract. Outcomes are independently declared before domain construction;
   equality is not an authentication or business-correctness proof."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private request-id "private-v1058-request")
(def ^:private helper-id "private-v1058-helper")
(def ^:private before
  {:xt/id request-id :revision 0 :created-at "t0" :updated-at "t0" :owner nil})
(def ^:private after
  (assoc before :revision 1 :updated-at "t1" :owner helper-id))
(def ^:private command-binding
  {:command-id (identity/command-id "v1058-command")
   :principal (identity/principal "private-v1058-principal")
   :operation :request/claim
   :arguments {:request-id request-id}})
(def ^:private allowed-post-state
  #{[:requests request-id :xt/id request-id]
    [:requests request-id :revision 1]
    [:requests request-id :created-at "t0"]
    [:requests request-id :updated-at "t1"]
    [:requests request-id :owner helper-id]})
(def ^:private claimed {:status :claimed})

(defn- request-plan []
  {:commands [(command/update-command :requests :claim before after version)]
   :guards []
   :emit false})

(defn- configuration
  ([] (configuration claimed))
  ([outcome]
   {:operation :request/claim
    :read-record! (fn [_ _] nil)
    :may-disclose? (fn [& _] true)
    :construct! (fn [_ _] {:now "t1" :plan (request-plan) :outcome outcome})
    :construction-contract
    {:required-guard-tables #{} :allowed-guard-tables #{}
     :required-write-tables #{:requests :gesso_commands}
     :allowed-write-tables #{:requests :gesso_commands}}
    :target-contract!
    (fn [_ cmd]
      {:required-guard-targets #{} :allowed-guard-targets #{}
       :required-write-targets #{[:requests (get-in cmd [:arguments :request-id])]}
       :allowed-write-targets #{[:requests (get-in cmd [:arguments :request-id])]}})
    :command-operation-contract!
    (fn [_ cmd]
      {:required-command-operations #{[:requests (get-in cmd [:arguments :request-id]) :claim]}
       :allowed-command-operations #{[:requests (get-in cmd [:arguments :request-id]) :claim]}})
    :post-state-contract!
    (fn [_ _]
      {:required-post-state-values #{[:requests request-id :owner helper-id]}
       :allowed-post-state-values allowed-post-state})}))

(defn- with-outcome [cfg expected]
  (assoc cfg :outcome-contract! (fn [_ _] {:expected-outcome expected})))

(defn- attempt
  ([cfg] (attempt cfg :submit nil))
  ([cfg mode] (attempt cfg mode nil))
  ([cfg mode existing]
   (let [sent (atom [])
         observed (atom [])
         steps (atom [])
         construct (:construct! cfg)
         outcome-declare (:outcome-contract! cfg)
         wrapped (cond->
                  (assoc cfg :read-record! (fn [_ _] existing)
                         :construct! (fn [ctx cmd]
                                       (swap! steps conj :construction)
                                       (construct ctx cmd)))
                   outcome-declare
                   (assoc :outcome-contract!
                          (fn [ctx cmd]
                            (swap! steps conj :outcome-declaration)
                            (outcome-declare ctx cmd))))
         op (resolved/build-operation wrapped)]
     (with-redefs [tx/transact! (fn [_ plan]
                                 (swap! sent conj plan)
                                 {:commit/status :committed
                                  :tx-result {:tx-id 1058}})]
       {:result (try {:value (op {resolved/observation-context-key
                                  #(swap! observed conj %)}
                                 {:binding command-binding :mode mode})}
                     (catch clojure.lang.ExceptionInfo e {:error e}))
        :sent @sent
        :observed @observed
        :steps @steps
        :metadata (meta op)}))))

(defn- kind [attempt-result]
  (some-> attempt-result :result :error ex-data :error/kind))

(defn- details [attempt-result]
  (ex-data (get-in attempt-result [:result :error])))

(defn- rejected-before-send [a expected]
  (is (= expected (kind a)))
  (is (empty? (:sent a)))
  (is (empty? (:observed a))))

(defn- config-kind [cfg]
  (try (resolved/build-operation cfg)
       nil
       (catch clojure.lang.ExceptionInfo e
         (:error/kind (ex-data e)))))

(deftest matching-outcome-is-allowed-and-contract-is-scoped
  (let [a (attempt (with-outcome (configuration) claimed))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= claimed (get-in a [:result :value :outcome])))
    (is (= 1 (count (:sent a))))
    (is (= [:prepared :physical-outcome] (mapv :phase (:observed a))))
    (is (= [:outcome-declaration :construction] (:steps a)))
    (is (= resolved/outcome-contract-scope
           (get-in a [:metadata :gesso.model/outcome-contract-scope])))
    (is (contains? (:checked resolved/outcome-contract-scope)
                   :exact-durable-outcome-equals-independently-declared-outcome))
    (is (contains? (:not-established resolved/outcome-contract-scope)
                   :correctness-of-declared-business-result))))

(deftest different-but-portable-outcome-is-rejected-before-ledger-write
  (doseq [constructed [{:status :cancelled}
                       {:status :claimed :helper "other"}
                       {:status "claimed"}
                       {:status :claimed :request-id request-id}]]
    (let [a (attempt (with-outcome (configuration constructed) claimed))]
      (rejected-before-send a :construction-outcome-mismatch)
      (is (= [:outcome-declaration :construction] (:steps a))))))

(deftest independent-declaration-can-express-complex-portable-outcome
  (let [expected {:status :claimed
                  :request {:id request-id :revision 1}
                  :notifications [:creator :helper]
                  :cleared nil
                  :attempt 17}
        a (attempt (with-outcome (configuration expected) expected))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= expected (get-in a [:result :value :outcome])))
    (is (= 1 (count (:sent a))))))

(deftest expected-outcome-declaration-is-not-sourced-from-construction
  (let [order (atom [])
        cfg (-> (configuration)
                (assoc :outcome-contract!
                       (fn [_ _]
                         (swap! order conj :independent-declaration)
                         {:expected-outcome claimed}))
                (assoc :construct!
                       (fn [_ _]
                         (swap! order conj :domain-construction)
                         {:now "t1" :plan (request-plan) :outcome claimed})))
        a (attempt cfg)]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= [:independent-declaration :domain-construction] @order))))

(deftest invalid-shapes-fail-before-domain-construction
  (doseq [bad [nil {} []
               {:expected-outcome nil}
               {:expected-outcome {}}
               {:expected-outcome []}
               {:expected-outcome claimed :extra "private-leak"}
               {:wrong-key claimed}
               {:expected-outcome {:status #{:claimed}}}
               {:expected-outcome {:status :claimed :unstable (Object.)}}
               {:expected-outcome {:status :claimed :mutable (java.util.concurrent.atomic.AtomicLong. 5)}}
               {:expected-outcome {:status :claimed :date (java.util.Date.)}}
               {:expected-outcome {:status :claimed :nan Double/NaN}}
               {:expected-outcome {:status :claimed :inf Double/POSITIVE_INFINITY}}]]
    (let [a (attempt (assoc (configuration)
                            :outcome-contract! (fn [_ _] bad)))]
      (rejected-before-send a :invalid-outcome-contract)
      (is (not (some #{:construction} (:steps a))))
      (is (not (str/includes? (pr-str (details a)) "private-leak"))))))

(deftest invalid-outcome-contract-registration-is-rejected
  (doseq [bad [nil false [] {} 3]]
    (is (= :invalid-outcome-contract
           (config-kind (assoc (configuration) :outcome-contract! bad))))))

(deftest required-write-and-post-state-contracts-cannot-be-omitted
  (doseq [missing [:command-operation-contract! :post-state-contract!]]
    (is (= :invalid-configuration
           (config-kind (dissoc (with-outcome (configuration) claimed)
                                missing))))))

(deftest immutable-portable-scalar-outcomes-are-supported
  (doseq [value [nil true false "hello" :ready 17 17N 12.5M
                 2.5 (java.util.UUID/randomUUID)]]
    (let [result {:status :claimed :value value}
          a (attempt (with-outcome (configuration result) result))]
      (is (= :incorporated (get-in a [:result :value :status])))
      (is (= 1 (count (:sent a)))))))

(deftest mutable-and-nonfinite-constructed-outcomes-fail-before-submission
  (doseq [invalid [(java.util.concurrent.atomic.AtomicLong. 7)
                   (java.util.concurrent.atomic.AtomicInteger. 7)
                   (java.util.Date.)
                   Double/NEGATIVE_INFINITY
                   Float/NaN
                   #{:unsupported}
                   (Object.)]]
    (let [a (attempt (with-outcome
                     (configuration {:status :claimed :value invalid})
                     claimed))]
      (rejected-before-send a :construction-outcome-mismatch))))

(deftest declaration-is-never-read-when-reconciling
  (let [a (attempt (assoc (configuration)
                          :outcome-contract! (fn [& _]
                                               (throw (ex-info "called" {}))))
                   :reconcile)]
    (is (= :unresolved (get-in a [:result :value :status])))
    (is (empty? (:steps a)))
    (is (empty? (:sent a)))
    (is (empty? (:observed a)))))

(deftest absent-contract-retains-existing-operation-behavior
  (let [a (attempt (configuration {:status :cancelled}))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= {:status :cancelled} (get-in a [:result :value :outcome])))
    (is (= 1 (count (:sent a))))
    (is (not (contains? (:metadata a) :gesso.model/outcome-contract-scope)))
    (is (not (contains? (:checked resolved/assurance-scope)
                        :exact-durable-outcome-equals-independently-declared-outcome)))))

(deftest failure-is-redacted
  (let [secret "private-v1058-do-not-disclose"
        a (attempt (with-outcome (configuration {:status :claimed :secret secret})
                     claimed))
        reported (pr-str (details a))]
    (rejected-before-send a :construction-outcome-mismatch)
    (doseq [v [secret request-id helper-id "v1058-command"
               "private-v1058-principal" ":claimed"]]
      (is (not (str/includes? reported v))))))

(deftest malformed-declaration-errors-remain-redacted
  (let [secret "private-v1058-declaration-secret"
        a (attempt (assoc (configuration)
                          :outcome-contract!
                          (fn [_ _]
                            {:expected-outcome claimed :private-value secret})))
        reported (pr-str (details a))]
    (rejected-before-send a :invalid-outcome-contract)
    (is (not (str/includes? reported secret)))
    (is (not (str/includes? reported request-id)))))

(deftest trusted-callback-failure-does-not-submit
  (let [a (attempt (assoc (configuration)
                          :outcome-contract!
                          (fn [_ _]
                            (throw (ex-info "private-declaration-failed" {})))))]
    (is (some? (get-in a [:result :error])))
    (is (empty? (:sent a)))
    (is (empty? (:observed a)))
    (is (not (some #{:construction} (:steps a))))))

(deftest complete-contract-remains-noncertifying
  (let [a (attempt (with-outcome (configuration) claimed))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (contains? (:not-established resolved/outcome-contract-scope)
                   :application-certification))
    (is (contains? (:assumed resolved/outcome-contract-scope)
                   :trusted-outcome-requirements-callback))
    (is (not (contains? (:checked resolved/outcome-contract-scope)
                        :complete-business-rules-or-authorization)))))

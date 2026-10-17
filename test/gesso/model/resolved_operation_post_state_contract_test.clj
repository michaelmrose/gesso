(ns gesso.model.resolved-operation-post-state-contract-test
  "Permanent adversarial regressions for the opt-in closed post-state contract.
   Expectations come from independently declared trusted inputs; agreement
   does not prove that an application declared the correct business rules."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private request-id "private-v1055-request")
(def ^:private helper-id "private-v1055-helper")
(def ^:private before
  {:xt/id request-id :revision 0 :created-at "t0" :updated-at "t0" :owner nil})
(def ^:private after
  (assoc before :revision 1 :updated-at "t1" :owner helper-id))
(def ^:private command-binding
  {:command-id (identity/command-id "v1055-command")
   :principal (identity/principal "private-v1055-principal")
   :operation :request/claim
   :arguments {:request-id request-id}})
(def ^:private allowed-fields
  #{[:requests request-id :xt/id request-id]
    [:requests request-id :revision 1]
    [:requests request-id :created-at "t0"]
    [:requests request-id :updated-at "t1"]
    [:requests request-id :owner helper-id]})

(defn- request-plan
  ([document] (request-plan document :claim))
  ([document operation]
   {:commands [(command/update-command :requests operation before document version)]
    :guards [] :emit false}))

(defn- post-state-declaration [_ctx _binding]
  {:required-post-state-values #{[:requests request-id :owner helper-id]}
   :allowed-post-state-values allowed-fields})

(defn- configuration [plan]
  {:operation :request/claim
   :read-record! (fn [_ _] nil)
   :may-disclose? (fn [& _] true)
   :construct! (fn [_ _] {:now "t1" :plan plan :outcome {:status :claimed}})
   :construction-contract
   {:required-guard-tables #{} :allowed-guard-tables #{}
    :required-write-tables #{:requests :gesso_commands}
    :allowed-write-tables #{:requests :gesso_commands}}
   :target-contract!
   (fn [_ctx cmd]
     {:required-guard-targets #{}
      :allowed-guard-targets #{}
      :required-write-targets #{[:requests (get-in cmd [:arguments :request-id])]}
      :allowed-write-targets #{[:requests (get-in cmd [:arguments :request-id])]}})
   :command-operation-contract!
   (fn [_ctx cmd]
     {:required-command-operations #{[:requests (get-in cmd [:arguments :request-id]) :claim]}
      :allowed-command-operations #{[:requests (get-in cmd [:arguments :request-id]) :claim]}})
   :post-state-contract! post-state-declaration})

(defn- attempt
  ([cfg] (attempt cfg :submit))
  ([cfg mode]
   (let [sent (atom [])
         observed (atom [])
         constructed (atom 0)
         steps (atom [])
         original-construct (:construct! cfg)
         original-declare (:post-state-contract! cfg)
         wrapped (cond-> (assoc cfg :construct!
                                (fn [ctx b]
                                  (swap! constructed inc)
                                  (swap! steps conj :construction)
                                  (original-construct ctx b)))
                   original-declare
                   (assoc :post-state-contract!
                          (fn [ctx b]
                            (swap! steps conj :declaration)
                            (original-declare ctx b))))
         op (resolved/build-operation wrapped)]
     (with-redefs [tx/transact! (fn [_ plan]
                                 (swap! sent conj plan)
                                 {:commit/status :committed
                                  :tx-result {:tx-id 1055}})]
       {:result (try {:value (op {resolved/observation-context-key
                                  #(swap! observed conj %)}
                                 {:binding command-binding :mode mode})}
                     (catch clojure.lang.ExceptionInfo e {:error e}))
        :sent @sent
        :observed @observed
        :constructed @constructed
        :steps @steps
        :metadata (meta op)}))))

(defn- kind [result]
  (some-> result :result :error ex-data :error/kind))

(defn- diagnostic [result]
  (pr-str (ex-data (get-in result [:result :error]))))

(defn- rejected-before-send [result expected-kind]
  (is (= expected-kind (kind result)))
  (is (empty? (:sent result)))
  (is (empty? (:observed result))))

(deftest matching-exact-values-allow-incorporation
  (let [a (attempt (configuration (request-plan after)))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 1 (count (:sent a))))
    (is (= [:prepared :physical-outcome] (mapv :phase (:observed a))))
    (is (= [:declaration :construction] (:steps a)))
    (is (= resolved/post-state-contract-scope
           (get-in a [:metadata :gesso.model/post-state-contract-scope])))))

(deftest wrong-owner-on-the-right-document-fails-before-submission
  (let [a (attempt (configuration (request-plan (assoc after :owner "private-wrong-owner"))))]
    (rejected-before-send a :construction-post-state-mismatch)
    (is (= 1 (get-in (ex-data (get-in a [:result :error]))
                     [:error/details :missing-post-state-value-count])))
    (is (= 1 (get-in (ex-data (get-in a [:result :error]))
                     [:error/details :unexpected-post-state-value-count])))))

(deftest undeclared-added-post-state-field-fails-closed
  (let [a (attempt (configuration (request-plan (assoc after :admin true))))]
    (rejected-before-send a :construction-post-state-mismatch)
    (is (= 1 (get-in (ex-data (get-in a [:result :error]))
                     [:error/details :unexpected-post-state-value-count])))))

(deftest wrong-updated-at-is-detected-even-when-owner-is-correct
  (let [a (attempt (configuration (request-plan (assoc after :updated-at "unexpected-timestamp"))))]
    (rejected-before-send a :construction-post-state-mismatch)))

(deftest missing-required-value-fails-closed
  (let [required #{[:requests request-id :owner helper-id]
                   [:requests request-id :status :claimed]}
        cfg (assoc (configuration (request-plan after))
                   :post-state-contract! (fn [_ _]
                                           {:required-post-state-values required
                                            :allowed-post-state-values
                                            (conj allowed-fields [:requests request-id :status :claimed])}))
        a (attempt cfg)]
    (rejected-before-send a :construction-post-state-mismatch)
    (is (= 1 (get-in (ex-data (get-in a [:result :error]))
                     [:error/details :missing-post-state-value-count])))))

(deftest allowed-extra-field-can-be-included
  (let [cfg (assoc (configuration (request-plan (assoc after :status :claimed)))
                   :post-state-contract! (fn [_ _]
                                           (update (post-state-declaration nil nil)
                                                   :allowed-post-state-values
                                                   conj [:requests request-id :status :claimed])))
        a (attempt cfg)]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 1 (count (:sent a))))))

(deftest nil-is-a-valid-explicit-field-value
  (let [expected (-> allowed-fields
                     (disj [:requests request-id :owner helper-id])
                     (conj [:requests request-id :owner nil]))
        cfg (assoc (configuration (request-plan (assoc after :owner nil)))
                   :post-state-contract! (fn [_ _]
                                           {:required-post-state-values #{[:requests request-id :owner nil]}
                                            :allowed-post-state-values expected}))
        a (attempt cfg)]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 1 (count (:sent a))))))

(deftest invalid-declaration-shapes-fail-before-domain-construction
  (let [good (post-state-declaration nil nil)
        malformed [nil {} []
                   (dissoc good :allowed-post-state-values)
                   (assoc good :extra "private-data")
                   (assoc good :required-post-state-values [])
                   (assoc good :allowed-post-state-values [[:requests request-id :owner helper-id]])
                   (assoc good :required-post-state-values #{[:requests request-id]})
                   (assoc good :required-post-state-values #{[:requests nil :owner helper-id]})
                   (assoc good :required-post-state-values #{[:requests request-id "owner" helper-id]})
                   (assoc good :required-post-state-values #{[:requests request-id :owner "wrong"]})
                   (assoc good :allowed-post-state-values
                          (conj allowed-fields [:requests request-id :owner "other-owner"]))
                   (assoc good :allowed-post-state-values
                          (conj allowed-fields [:gesso_commands "ledger" :status :ok]))
                   (assoc good :required-post-state-values
                          #{[:requests "private-other-request" :owner helper-id]})]]
    (doseq [decl malformed]
      (let [a (attempt (assoc (configuration (request-plan after))
                              :post-state-contract! (fn [_ _] decl)))]
        (rejected-before-send a :invalid-post-state-contract)
        (is (zero? (:constructed a)))))))

(deftest required-declaration-must-include-every-required-write-target
  (let [cfg (-> (configuration (request-plan after))
                (assoc :post-state-contract!
                       (fn [_ _] {:required-post-state-values #{}
                                  :allowed-post-state-values allowed-fields})))
        a (attempt cfg)]
    (rejected-before-send a :invalid-post-state-contract)
    (is (zero? (:constructed a)))))

(deftest callback-is-invoked-before-construction
  (let [a (attempt (configuration (request-plan after)))]
    (is (= [:declaration :construction] (:steps a)))
    (is (= 1 (:constructed a)))))

(deftest callback-must-be-a-function
  (doseq [bad [nil false {} []]]
    (is (= :invalid-post-state-contract
           (try (resolved/build-operation
                 (assoc (configuration (request-plan after)) :post-state-contract! bad))
                nil
                (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e))))))))

(deftest post-state-contract-requires-table-and-target-contracts
  (doseq [omit [:construction-contract :target-contract!]]
    (is (= :invalid-configuration
           (try (resolved/build-operation
                 (dissoc (configuration (request-plan after)) omit))
                nil
                (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e))))))))

(deftest reconciliation-skips-construction-and-declaration
  (let [a (attempt (configuration (request-plan after)) :reconcile)]
    (is (= :unresolved (get-in a [:result :value :status])))
    (is (empty? (:steps a)))
    (is (zero? (:constructed a)))
    (is (empty? (:sent a)))
    (is (empty? (:observed a)))))

(deftest opting-out-does-not-claim-post-state-assurance
  (let [cfg (dissoc (configuration (request-plan (assoc after :owner "different")))
                    :post-state-contract!)
        a (attempt cfg)]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 1 (count (:sent a))))
    (is (nil? (get-in a [:metadata :gesso.model/post-state-contract-scope])))
    (is (not (contains? (:checked resolved/assurance-scope)
                        :required-and-allowed-domain-post-state-values-match-final-plan)))))

(deftest errors-redact-document-identities-and-confidential-values
  (let [a (attempt (configuration (request-plan (assoc after :owner "private-wrong-owner"))))
        message (diagnostic a)]
    (rejected-before-send a :construction-post-state-mismatch)
    (doseq [secret [request-id helper-id "private-wrong-owner"
                    "private-v1055-principal" "v1055-command"]]
      (is (not (str/includes? message secret))))
    (is (= #{:missing-post-state-value-count :unexpected-post-state-value-count}
           (set (keys (:error/details (ex-data (get-in a [:result :error])))))))))

(deftest successful-contract-is-not-authorization-or-certification
  (let [a (attempt (configuration (request-plan after)))
        scope (get-in a [:metadata :gesso.model/post-state-contract-scope])]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (contains? (:not-established scope) :correct-meaning-of-declared-post-state-values))
    (is (contains? (:not-established scope) :authorization-or-policy-correctness))
    (is (contains? (:not-established scope) :xtdb-physical-refinement))
    (is (contains? (:not-established scope) :application-certification))))

(deftest post-state-agreement-alone-does-not-check-command-operation
  (let [cfg (-> (configuration (request-plan after :cancel))
                (dissoc :command-operation-contract!))
        a (attempt cfg)]
    ;; The exact same final field values do not establish that the correct
    ;; semantic command operation was issued; that check is a separate opt-in.
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 1 (count (:sent a))))
    (is (nil? (get-in a [:metadata :gesso.model/command-operation-contract-scope])))))

(deftest closed-declarations-ignore-gesso-owned-ledger-values
  (let [a (attempt (configuration (request-plan after)))]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= 2 (count (get-in (first (:sent a)) [:commands]))))))

(deftest empty-optional-allowed-targets-cannot-smuggle-post-state
  (let [cfg (-> (configuration (request-plan after))
                (assoc :post-state-contract!
                       (fn [_ _]
                         {:required-post-state-values #{[:requests request-id :owner helper-id]}
                          :allowed-post-state-values
                          (conj allowed-fields [:other-table "private-other" :x true])})))
        a (attempt cfg)]
    (rejected-before-send a :invalid-post-state-contract)
    (is (zero? (:constructed a)))))

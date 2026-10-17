(ns gesso.model.resolved-operation-construction-contract-test
  "Permanent adversarial tests for v1046 optional closed-table construction
   coverage at the resolved-operation boundary. These regressions establish
   rejection before submitting a transaction, not independent discovery of
   business authorization dependencies, valid row identities, constructor
   purity, or a theorem about XTDB physical behavior."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private before
  {:xt/id "private-request-id" :revision 0 :created-at "t0"
   :updated-at "t0" :owner "unclaimed"})
(def ^:private after
  (assoc before :revision 1 :updated-at "t1" :owner "private-helper"))
(def ^:private membership
  {:xt/id "private-membership-id" :revision 0 :created-at "t0"
   :updated-at "t0" :active true})
(def ^:private command-binding
  {:command-id (identity/command-id "v1047-command")
   :principal (identity/principal "v1047-principal")
   :operation :request/claim
   :arguments {:request-id (:xt/id before)}})
(def ^:private outcome {:status :claimed})
(def ^:private contract
  {:required-guard-tables #{:memberships}
   :allowed-guard-tables #{:memberships}
   :required-write-tables #{:requests :gesso_commands}
   :allowed-write-tables #{:requests :gesso_commands}})

(defn- plan []
  {:commands [(command/update-command :requests :claim before after version)]
   :guards [(command/guard :memberships membership version)]
   :emit false})

(defn- registration
  ([p] (registration p nil))
  ([p stored]
   {:operation :request/claim
    :read-record! (fn [_ctx _id] stored)
    :may-disclose? (fn [_ctx _binding _record] true)
    :construct! (fn [_ctx _binding]
                  {:now "t1" :plan p :outcome outcome})}))

(defn- registered [p selected-contract]
  (resolved/build-operation
   (assoc (registration p) :construction-contract selected-contract)))

(defn- exception [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- error-kind [f]
  (some-> (exception f) ex-data :error/kind))

(defn- invoke
  ([operation] (invoke operation {} :submit))
  ([operation context mode]
   (operation context {:binding command-binding :mode mode})))

(defn- instrumented-submit [operation]
  (let [submitted (atom [])
        events (atom [])
        context {resolved/observation-context-key #(swap! events conj %)}]
    (with-redefs [tx/transact!
                  (fn [_ctx p]
                    (swap! submitted conj p)
                    {:commit/status :committed :tx-result {:tx-id 41}})]
      {:result (try {:value (invoke operation context :submit)}
                    (catch clojure.lang.ExceptionInfo e {:exception e}))
       :submitted @submitted
       :events @events})))

(deftest valid-contract-checks-incorporated-ledger-before-commit-test
  (let [{:keys [result submitted events]} (instrumented-submit
                                           (registered (plan) contract))]
    (is (= :incorporated (get-in result [:value :status])))
    (is (= 1 (count submitted)))
    (is (= #{:requests :gesso_commands}
           (set (map (comp first tx/command-target)
                     (:commands (first submitted))))))
    (is (= #{:memberships}
           (set (map (comp first tx/guard-target)
                     (:guards (first submitted))))))
    (is (= [:prepared :physical-outcome] (mapv :phase events)))))

(deftest registration-exposes-only-opt-in-contract-metadata-test
  (let [opted (registered (plan) contract)
        old (resolved/build-operation (registration (plan)))
        opted-meta (meta opted)
        old-meta (meta old)]
    (is (= contract (:gesso.model/construction-contract opted-meta)))
    (is (= resolved/construction-contract-scope
           (:gesso.model/construction-contract-scope opted-meta)))
    (is (not (contains? old-meta :gesso.model/construction-contract)))
    (is (not (contains? old-meta :gesso.model/construction-contract-scope)))
    (is (= resolved/assurance-scope
           (:gesso.model/assurance-scope opted-meta)
           (:gesso.model/assurance-scope old-meta)))
    (is (not (contains? (:checked resolved/assurance-scope)
                        :opt-in-closed-construction-table-coverage)))
    (is (contains? (:not-established resolved/construction-contract-scope)
                   :independent-business-rule-discovery))
    (is (contains? (:not-established resolved/construction-contract-scope)
                   :correct-row-identities-within-declared-tables))
    (is (contains? (:not-established resolved/construction-contract-scope)
                   :application-certification))))

(deftest missing-required-guard-fails-before-submission-or-prepared-observation-test
  (let [{:keys [result submitted events]}
        (instrumented-submit (registered (assoc (plan) :guards []) contract))]
    (is (= :construction-contract-mismatch
           (some-> (:exception result) ex-data :error/kind)))
    (is (= #{:memberships}
           (get-in (ex-data (:exception result))
                   [:error/details :missing-guard-tables])))
    (is (empty? submitted))
    (is (empty? events))))

(deftest missing-domain-write-fails-before-commit-test
  (let [p (assoc (plan) :commands
                 [(command/update-command :other :claim before after version)])
        {:keys [result submitted events]} (instrumented-submit (registered p contract))]
    (is (= :construction-contract-mismatch
           (some-> (:exception result) ex-data :error/kind)))
    (is (= #{:requests}
           (get-in (ex-data (:exception result))
                   [:error/details :missing-write-tables])))
    (is (= #{:other}
           (get-in (ex-data (:exception result))
                   [:error/details :unexpected-write-tables])))
    (is (empty? submitted))
    (is (empty? events))))

(deftest undeclared-guard-table-fails-closed-test
  (let [p (update (plan) :guards conj (command/guard :secret membership version))
        attempt (instrumented-submit (registered p contract))]
    (is (= :construction-contract-mismatch
           (some-> attempt :result :exception ex-data :error/kind)))
    (is (= #{:secret}
           (get-in (ex-data (get-in attempt [:result :exception]))
                   [:error/details :unexpected-guard-tables])))
    (is (empty? (:submitted attempt)))))

(deftest undeclared-write-table-fails-closed-test
  (let [p (update (plan) :commands conj
                  (command/update-command :secret :claim before after version))
        attempt (instrumented-submit (registered p contract))]
    (is (= :construction-contract-mismatch
           (some-> attempt :result :exception ex-data :error/kind)))
    (is (= #{:secret}
           (get-in (ex-data (get-in attempt [:result :exception]))
                   [:error/details :unexpected-write-tables])))
    (is (empty? (:submitted attempt)))))

(deftest allowed-but-not-required-guard-can-be-absent-or-present-test
  (let [c (update contract :allowed-guard-tables conj :optional)
        absent (instrumented-submit (registered (plan) c))
        present (instrumented-submit
                 (registered (update (plan) :guards conj
                                     (command/guard :optional membership version)) c))]
    (is (= :incorporated (get-in absent [:result :value :status])))
    (is (= :incorporated (get-in present [:result :value :status])))
    (is (= 1 (count (:submitted absent))))
    (is (= 1 (count (:submitted present))))))

(deftest allowed-but-not-required-write-can-be-absent-or-present-test
  (let [c (update contract :allowed-write-tables conj :audit)
        absent (instrumented-submit (registered (plan) c))
        present (instrumented-submit
                 (registered (update (plan) :commands conj
                                     (command/update-command :audit :claim
                                                             before after version)) c))]
    (is (= :incorporated (get-in absent [:result :value :status])))
    (is (= :incorporated (get-in present [:result :value :status])))
    (is (= 1 (count (:submitted absent))))
    (is (= 1 (count (:submitted present))))))

(deftest custom-sql-assertion-is-rejected-rather-than-implicitly-covered-test
  (let [p (assoc (plan) :assertions
                 [{:assert [:= 1 {:select [[[:count '*]]] :from 'secret_table}]}])
        {:keys [result submitted events]} (instrumented-submit (registered p contract))]
    (is (= :construction-contract-mismatch
           (some-> (:exception result) ex-data :error/kind)))
    (is (true? (get-in (ex-data (:exception result))
                       [:error/details :opaque-assertions?])))
    (is (empty? submitted))
    (is (empty? events))))

(deftest invalid-contract-shapes-are-rejected-at-registration-test
  (doseq [[description bad]
          [[:not-map nil]
           [:unknown-keys (assoc contract :extra :data)]
           [:missing-required-set (dissoc contract :required-guard-tables)]
           [:missing-allowed-set (dissoc contract :allowed-write-tables)]
           [:wrong-set-type (assoc contract :allowed-write-tables [:requests :gesso_commands])]
           [:non-keyword (update contract :allowed-guard-tables conj "memberships")]
           [:bad-guard-subset (assoc contract :allowed-guard-tables #{})]
           [:bad-write-subset (assoc contract :allowed-write-tables #{:gesso_commands})]
           [:not-required-ledger (assoc contract :required-write-tables #{:requests})]
           [:ledger-only-allowed (-> contract
                                     (assoc :required-write-tables #{:requests})
                                     (assoc :allowed-write-tables #{:requests :gesso_commands}))]
           [:wrong-required-type (assoc contract :required-guard-tables [])]]]
    (testing (name description)
      (is (= :invalid-construction-contract
             (error-kind #(registered (plan) bad)))))))

(deftest nil-contract-does-not-silently-disable-an-opted-in-gate-test
  (is (= :invalid-construction-contract
         (error-kind #(resolved/build-operation
                        (assoc (registration (plan)) :construction-contract nil))))))

(deftest uncontracted-operation-remains-backward-compatible-test
  (let [p (assoc (plan) :guards [])
        attempt (instrumented-submit
                 (resolved/build-operation (registration p)))]
    (is (= :incorporated (get-in attempt [:result :value :status])))
    (is (= 1 (count (:submitted attempt))))
    (is (= [:prepared :physical-outcome] (mapv :phase (:events attempt))))))

(deftest already-recorded-command-does-not-run-construction-contract-test
  (let [record (resolution/command-record command-binding outcome "t1")
        built (resolved/build-operation
               (assoc (registration (assoc (plan) :guards []) record)
                      :construction-contract contract))
        attempts (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! attempts inc)
                                {:commit/status :committed})]
      (is (= :replay (:status (invoke built {} :submit))))
      (is (= :replay (:status (invoke built {} :reconcile))))
      (is (zero? @attempts)))))

(deftest absent-record-reconcile-does-not-construct-or-transact-test
  (let [built (registered (assoc (plan) :guards []) contract)
        attempts (atom 0)]
    (with-redefs [tx/transact! (fn [& _] (swap! attempts inc)
                                {:commit/status :committed})]
      (is (= :unresolved (:status (invoke built {} :reconcile))))
      (is (zero? @attempts)))))

(deftest rejected-plan-does-not-publish-a-prepared-attempt-test
  (let [prepared (atom [])
        inputs (atom [])
        commits (atom 0)
        built (registered (assoc (plan) :guards []) contract)
        ctx {resolved/observation-context-key #(swap! prepared conj %)
             resolved/construction-input-context-key #(swap! inputs conj %)}]
    (with-redefs [tx/transact! (fn [& _] (swap! commits inc)
                                {:commit/status :committed})]
      (is (= :construction-contract-mismatch
             (error-kind #(invoke built ctx :submit))))
      (is (= [:construction-input] (mapv :phase @inputs)))
      (is (empty? @prepared))
      (is (zero? @commits)))))

(deftest mismatch-report-contains-tables-but-not-sensitive-record-values-test
  (let [sensitive "private-helper" 
        p (assoc (plan) :guards [])
        error (exception #(invoke (registered p contract)))]
    (is (= :construction-contract-mismatch (:error/kind (ex-data error))))
    (is (= #{:memberships}
           (get-in (ex-data error) [:error/details :missing-guard-tables])))
    (is (not (str/includes? (pr-str (ex-data error)) sensitive)))
    (is (not (str/includes? (pr-str (ex-data error)) "private-request-id")))))

(deftest table-coverage-alone-does-not-validate-row-identity-or-policy-test
  ;; Replacing the guarded membership with a DIFFERENT versioned membership
  ;; retains the same table footprint; this should NOT be called authorization
  ;; proof or independent dependency discovery.
  (let [wrong-membership (assoc membership :xt/id "unrelated-membership")
        p (assoc (plan) :guards [(command/guard :memberships wrong-membership version)])
        attempt (instrumented-submit (registered p contract))]
    (is (= :incorporated (get-in attempt [:result :value :status])))
    (is (= 1 (count (:submitted attempt))))
    (is (contains? (:not-established resolved/construction-contract-scope)
                   :correct-row-identities-within-declared-tables))))

(deftest no-mandatory-policy-on-legacy-registrations-test
  (let [config (registration (plan))]
    (is (fn? (resolved/build-operation config)))
    (is (= :invalid-configuration
           (error-kind #(resolved/build-operation (assoc config :unrelated true)))))
    (is (= :invalid-configuration
           (error-kind #(resolved/build-operation (dissoc config :construct!)))))))

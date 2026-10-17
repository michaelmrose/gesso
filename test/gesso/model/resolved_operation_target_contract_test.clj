(ns gesso.model.resolved-operation-target-contract-test
  "Permanent adversarial coverage for optional exact document-target contracts.

   These tests check the REGISTERED caller/context-derived contract against the
   final incorporated plan before transaction submission. They do not prove
   that the callback is independent, its business dependencies are complete,
   or its document identities are authorized by the domain."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private request-id "private-request-v1049")
(def ^:private membership-id "private-membership-v1049")
(def ^:private before
  {:xt/id request-id :revision 0 :created-at "t0" :updated-at "t0" :owner nil})
(def ^:private after
  (assoc before :revision 1 :updated-at "t1" :owner "helper-v1049"))
(def ^:private membership
  {:xt/id membership-id :revision 0 :created-at "t0" :updated-at "t0" :active true})
(def ^:private command-binding-fixture
  {:command-id (identity/command-id "v1049-cmd")
   :principal (identity/principal "v1049-principal")
   :operation :request/claim
   :arguments {:request-id request-id}})
(def ^:private outcome {:status :claimed})
(def ^:private closed-tables
  {:required-guard-tables #{:memberships}
   :allowed-guard-tables #{:memberships}
   :required-write-tables #{:requests :gesso_commands}
   :allowed-write-tables #{:requests :gesso_commands}})

(defn- plan []
  {:commands [(command/update-command :requests :claim before after version)]
   :guards [(command/guard :memberships membership version)]
   :emit false})

(defn- trusted-targets [ctx command-binding]
  (let [r (get-in command-binding [:arguments :request-id])
        m (:membership-id ctx)]
    {:required-guard-targets #{[:memberships m]}
     :allowed-guard-targets #{[:memberships m]}
     :required-write-targets #{[:requests r]}
     :allowed-write-targets #{[:requests r]}}))

(defn- config
  ([p] (config p trusted-targets))
  ([p declare!]
   {:operation :request/claim
    :read-record! (fn [_ _] nil)
    :may-disclose? (fn [_ _ _] true)
    :construct! (fn [_ _] {:now "t1" :plan p :outcome outcome})
    :construction-contract closed-tables
    :target-contract! declare!}))

(defn- error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))
(defn- kind [f]
  (some-> (error f) ex-data :error/kind))

(defn- attempt
  ([configuration] (attempt configuration {:membership-id membership-id} command-binding-fixture :submit))
  ([configuration ctx command-binding mode]
   (let [submitted (atom [])
         observed (atom [])
         constructed (atom 0)
         declared (atom 0)
         original-construct! (:construct! configuration)
         original-target! (:target-contract! configuration)
         configuration (cond-> configuration
                         original-construct!
                         (assoc :construct! (fn [c b]
                                              (swap! constructed inc)
                                              (original-construct! c b)))
                         original-target!
                         (assoc :target-contract! (fn [c b]
                                                   (swap! declared inc)
                                                   (original-target! c b))))
         instance (resolved/build-operation configuration)
         ctx (assoc ctx resolved/observation-context-key #(swap! observed conj %))]
     (with-redefs [tx/transact! (fn [_ p]
                                  (swap! submitted conj p)
                                  {:commit/status :committed :tx-result {:tx-id 1049}})]
       {:result (try {:value (instance ctx {:binding command-binding :mode mode})}
                     (catch clojure.lang.ExceptionInfo e {:exception e}))
        :submitted @submitted
        :observed @observed
        :constructed @constructed
        :declared @declared
        :metadata (meta instance)}))))

(deftest valid-target-contract-and-derived-ledger-are-admitted-test
  (let [a (attempt (config (plan)))
        actual (first (:submitted a))
        ledger [:gesso_commands (resolution/record-id command-binding-fixture)]]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= #{[:memberships membership-id]}
           (set (map tx/guard-target (:guards actual)))))
    (is (= #{[:requests request-id] ledger}
           (set (map tx/command-target (:commands actual)))))
    (is (= 1 (:declared a)))
    (is (= 1 (:constructed a)))
    (is (= [:prepared :physical-outcome] (mapv :phase (:observed a))))
    (is (= resolved/target-contract-scope
           (get-in a [:metadata :gesso.model/target-contract-scope])))
    (is (not (contains? (:checked resolved/assurance-scope)
                        :required-and-allowed-document-target-coverage)))
    (is (contains? (:not-established resolved/target-contract-scope)
                   :independent-discovery-of-authorization-subjects))))

(deftest target-declaration-precedes-domain-construction-test
  (let [order (atom [])
        declared (fn [ctx b] (swap! order conj :declare)
                   (trusted-targets ctx b))
        c (assoc (config (plan) declared)
                 :construct! (fn [_ _] (swap! order conj :construct)
                               {:now "t1" :plan (plan) :outcome outcome}))
        a (attempt c)]
    (is (= :incorporated (get-in a [:result :value :status])))
    (is (= [:declare :construct] @order))))

(deftest correct-tables-wrong-membership-id-rejected-before-submit-test
  (let [wrong (assoc membership :xt/id "private-wrong-membership-v1049")
        p (assoc (plan) :guards [(command/guard :memberships wrong version)])
        a (attempt (config p))]
    (is (= :construction-target-mismatch (some-> a :result :exception ex-data :error/kind)))
    (is (= 1 (get-in (ex-data (get-in a [:result :exception]))
                     [:error/details :missing-guard-target-count])))
    (is (= 1 (get-in (ex-data (get-in a [:result :exception]))
                     [:error/details :unexpected-guard-target-count])))
    (is (empty? (:submitted a)))
    (is (empty? (:observed a)))))

(deftest correct-tables-wrong-request-id-rejected-before-submit-test
  (let [wrong-before (assoc before :xt/id "private-wrong-request-v1049")
        wrong-after (assoc after :xt/id "private-wrong-request-v1049")
        p (assoc (plan) :commands
                 [(command/update-command :requests :claim wrong-before wrong-after version)])
        a (attempt (config p))]
    (is (= :construction-target-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (= 1 (get-in (ex-data (get-in a [:result :exception]))
                     [:error/details :missing-write-target-count])))
    (is (= 1 (get-in (ex-data (get-in a [:result :exception]))
                     [:error/details :unexpected-write-target-count])))
    (is (empty? (:submitted a)))
    (is (empty? (:observed a)))))

(deftest wrong-context-identity-is-a-rejection-not-an-authenticated-claim-test
  (let [a (attempt (config (plan)) {:membership-id "different-member"} command-binding-fixture :submit)]
    (is (= :construction-target-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (empty? (:submitted a)))))

(deftest command-binding-argument-controls-exact-target-test
  (let [changed (assoc-in command-binding-fixture [:arguments :request-id] "other-request")
        a (attempt (config (plan)) {:membership-id membership-id} changed :submit)]
    (is (= :construction-target-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (empty? (:submitted a)))))

(deftest missing-guard-identity-is-rejected-before-submit-test
  (let [a (attempt (config (assoc (plan) :guards [])))]
    ;; The old closed-table contract catches this before the document contract.
    (is (= :construction-contract-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (empty? (:submitted a)))))

(deftest missing-required-document-with-table-still-present-test
  (let [allowed-membership (assoc membership :xt/id "extra")
        p (assoc (plan) :guards [(command/guard :memberships allowed-membership version)])
        a (attempt (config p))]
    (is (= :construction-target-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (empty? (:submitted a)))))

(deftest document-id-leaks-neither-from-mismatch-nor-invalid-declaration-test
  (let [wrong (assoc membership :xt/id "sensitive-wrong-member")
        p (assoc (plan) :guards [(command/guard :memberships wrong version)])
        a (attempt (config p))
        invalid (attempt (config (plan)
                                 (fn [_ _]
                                   (assoc (trusted-targets {:membership-id membership-id} command-binding-fixture)
                                          :unrecognized "secret-stashed-value"))))]
    (is (= :construction-target-mismatch
           (some-> a :result :exception ex-data :error/kind)))
    (is (= :invalid-target-contract
           (some-> invalid :result :exception ex-data :error/kind)))
    (doseq [one [a invalid]]
      (let [msg (pr-str (ex-data (get-in one [:result :exception])))]
        (is (not (str/includes? msg request-id)))
        (is (not (str/includes? msg membership-id)))
        (is (not (str/includes? msg "sensitive-wrong-member")))
        (is (not (str/includes? msg "secret-stashed-value")))
        (is (empty? (:submitted one)))))))

(deftest invalid-target-contract-shape-fails-closed-test
  (let [good (trusted-targets {:membership-id membership-id} command-binding-fixture)
        variants [nil
                  {}
                  (assoc good :surplus true)
                  (dissoc good :allowed-write-targets)
                  (assoc good :required-guard-targets [[:memberships membership-id]])
                  (assoc good :allowed-write-targets #{[:requests nil]})
                  (assoc good :allowed-write-targets #{[:requests]})
                  (assoc good :required-guard-targets #{["memberships" membership-id]})
                  (assoc good :required-write-targets #{[:requests request-id :extra]})
                  (assoc good :allowed-guard-targets #{})
                  (assoc good :allowed-write-targets #{})
                  (assoc good :required-guard-targets #{})
                  (assoc good :required-write-targets #{})
                  (update good :required-guard-targets conj [:gesso_commands "forged"])
                  (update good :required-write-targets conj [:gesso_commands "forged"])]]
    (doseq [bad variants]
      (let [a (attempt (config (plan) (fn [_ _] bad)))]
        (is (= :invalid-target-contract
               (some-> a :result :exception ex-data :error/kind))
            (pr-str bad))
        (is (empty? (:submitted a)))))))

(deftest target-declaration-must-be-consistent-with-closed-table-footprint-test
  (let [good (trusted-targets {:membership-id membership-id} command-binding-fixture)
        variants [(update good :allowed-guard-targets conj [:secret "s"])
                  (update good :allowed-write-targets conj [:secret "s"])] ]
    (doseq [bad variants]
      (let [a (attempt (config (plan) (fn [_ _] bad)))]
        (is (= :invalid-target-contract
               (some-> a :result :exception ex-data :error/kind)))
        (is (empty? (:submitted a)))))))

(deftest target-contract-cannot-be-enabled-without-table-contract-test
  (let [configuration (dissoc (config (plan)) :construction-contract)]
    (is (= :invalid-configuration
           (kind #(resolved/build-operation configuration))))
    (is (= :invalid-target-contract
           (kind #(resolved/build-operation
                    (assoc (config (plan)) :target-contract! nil)))))))

(deftest declared-optional-document-targets-admitted-only-when-present-test
  (let [extra (assoc membership :xt/id "optional-member")
        extra-target [:memberships "optional-member"]
        declared (fn [ctx b]
                   (update (trusted-targets ctx b) :allowed-guard-targets conj extra-target))
        absent (attempt (config (plan) declared))
        present (attempt (config (update (plan) :guards conj
                                        (command/guard :memberships extra version)) declared))]
    (is (= :incorporated (get-in absent [:result :value :status])))
    (is (= :incorporated (get-in present [:result :value :status])))
    (is (= 1 (count (:submitted absent))))
    (is (= 1 (count (:submitted present))))))

(deftest duplicate-guard-target-does-not-create-false-success-test
  ;; A duplicate target is not a substitute for required coverage. The
  ;; existing canonical model's duplicate-target rejection may fire first.
  (let [wrong (assoc membership :xt/id "wrong")
        p (assoc (plan) :guards [(command/guard :memberships wrong version)
                                 (command/guard :memberships wrong version)])
        a (attempt (config p))]
    (is (some? (get-in a [:result :exception])))
    (is (empty? (:submitted a)))))

(deftest command-ledger-is-owned-by-framework-not-domain-contract-test
  (let [good (trusted-targets {:membership-id membership-id} command-binding-fixture)
        spoof (assoc good :allowed-write-targets
                     (conj (:allowed-write-targets good)
                           [:gesso_commands "some-other-command-id"]))
        a (attempt (config (plan) (fn [_ _] spoof)))]
    (is (= :invalid-target-contract
           (some-> a :result :exception ex-data :error/kind)))
    (is (empty? (:submitted a)))))

(deftest existing-record-and-reconcile-bypass-new-construction-contract-test
  (let [calls (atom 0)
        c (-> (config (plan))
              (assoc :read-record! (fn [_ _]
                                     (resolution/command-record command-binding-fixture outcome "t1")))
              (assoc :target-contract! (fn [& _] (swap! calls inc)
                                         (throw (ex-info "must not run" {})))))
        x (attempt c {:membership-id membership-id} command-binding-fixture :submit)
        y (attempt c {:membership-id membership-id} command-binding-fixture :reconcile)]
    (is (= :replay (get-in x [:result :value :status])))
    (is (= :replay (get-in y [:result :value :status])))
    (is (zero? @calls))
    (is (empty? (:submitted x)))
    (is (empty? (:submitted y)))))

(deftest old-table-only-registrations-are-unchanged-test
  (let [c (dissoc (config (plan)) :target-contract!)
        x (attempt c)]
    (is (= :incorporated (get-in x [:result :value :status])))
    (is (not (contains? (:metadata x) :gesso.model/target-contract-scope)))
    (is (= 1 (count (:submitted x))))))

(deftest target-contract-never-proves-domain-policy-test
  (is (contains? (:not-established resolved/target-contract-scope)
                 :authorization-policy-correctness))
  (is (contains? (:assumed resolved/target-contract-scope)
                 :complete-declaration-of-relevant-document-dependencies))
  (is (not (contains? (:checked resolved/target-contract-scope)
                      :complete-declaration-of-relevant-document-dependencies))))

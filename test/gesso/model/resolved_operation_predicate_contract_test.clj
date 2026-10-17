(ns gesso.model.resolved-operation-predicate-contract-test
  "Permanent adversarial regressions for optional closed expected-value contracts.

   The predicate declarations are computed from trusted invocation inputs before
   domain construction, then checked against the final incorporated plan.
   These tests deliberately do not claim independent business-policy discovery,
   complete authorization, or proof of XTDB's physical semantics."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx :as tx]))

(def ^:private request-id "private-request-v1051")
(def ^:private membership-id "private-membership-v1051")
(def ^:private command-binding
  {:command-id (identity/command-id "v1051-command")
   :principal (identity/principal "private-v1051-principal")
   :operation :request/claim
   :arguments {:request-id request-id}})
(def ^:private before
  {:xt/id request-id :revision 0 :created-at "t0" :updated-at "t0" :owner "open"})
(def ^:private after
  (assoc before :revision 1 :updated-at "t1" :owner "helper"))
(def ^:private member
  {:xt/id membership-id :revision 7 :created-at "t0" :updated-at "t0" :active true})
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private guarded-version
  (assoc version :compare-keys [:revision :updated-at :active]))
(def ^:private tables
  {:required-guard-tables #{:memberships}
   :allowed-guard-tables #{:memberships}
   :required-write-tables #{:requests :gesso_commands}
   :allowed-write-tables #{:requests :gesso_commands}})

(defn- targets [ctx command-binding]
  {:required-guard-targets #{[:memberships (:membership-id ctx)]}
   :allowed-guard-targets #{[:memberships (:membership-id ctx)]}
   :required-write-targets #{[:requests (get-in command-binding [:arguments :request-id])]}
   :allowed-write-targets #{[:requests (get-in command-binding [:arguments :request-id])]}})

(defn- predicates [ctx command-binding]
  (let [m (:membership-id ctx)
        r (get-in command-binding [:arguments :request-id])]
    {:required-guard-checks #{[[:memberships m] :active true]}
     :allowed-guard-checks #{[[:memberships m] :active true]
                             [[:memberships m] :revision 7]
                             [[:memberships m] :updated-at "t0"]}
     :required-update-checks #{[[:requests r] :revision 0]}
     :allowed-update-checks #{[[:requests r] :revision 0]
                              [[:requests r] :updated-at "t0"]}}))

(defn- plan
  ([] (plan member guarded-version version))
  ([member-document member-version request-version]
   {:guards [(command/guard :memberships member-document member-version)]
    :commands [(command/update-command :requests :claim before after request-version)]
    :emit false}))

(defn- configuration
  ([p] (configuration p predicates))
  ([p predicate!]
   {:operation :request/claim
    :read-record! (fn [_ _] nil)
    :may-disclose? (fn [_ _ _] true)
    :construct! (fn [_ _] {:now "t1" :plan p :outcome {:status :claimed}})
    :construction-contract tables
    :target-contract! targets
    :predicate-contract! predicate!}))

(defn- error-kind [e]
  (some-> e ex-data :error/kind))

(defn- execute
  ([config] (execute config {:membership-id membership-id} command-binding :submit))
  ([config context command-binding mode]
   (let [submitted (atom [])
         observations (atom [])
         calls (atom [])
         orig-construct (:construct! config)
         orig-target (:target-contract! config)
         orig-predicate (:predicate-contract! config)
         orig-read (:read-record! config)
         wrapped (cond-> config
                   orig-read (assoc :read-record!
                                    (fn [ctx id]
                                      (swap! calls conj :read)
                                      (orig-read ctx id)))
                   orig-construct (assoc :construct!
                                         (fn [ctx b]
                                           (swap! calls conj :construct)
                                           (orig-construct ctx b)))
                   orig-target (assoc :target-contract!
                                      (fn [ctx b]
                                        (swap! calls conj :targets)
                                        (orig-target ctx b)))
                   orig-predicate (assoc :predicate-contract!
                                         (fn [ctx b]
                                           (swap! calls conj :predicates)
                                           (orig-predicate ctx b))))
         operation (resolved/build-operation wrapped)
         ctx (assoc context resolved/observation-context-key
                    #(swap! observations conj %))]
     (with-redefs [tx/transact! (fn [_ p]
                                  (swap! calls conj :transact)
                                  (swap! submitted conj p)
                                  {:commit/status :committed
                                   :tx-result {:tx-id 1051}})]
       (let [result (try {:value (operation ctx {:binding command-binding
                                                :mode mode})}
                         (catch clojure.lang.ExceptionInfo e
                           {:exception e}))]
         {:result result
          :exception (:exception result)
          :kind (error-kind (:exception result))
          :details (some-> result :exception ex-data :error/details)
          :status (get-in result [:value :status])
          :submitted @submitted
          :observed @observations
          :calls @calls
          :metadata (meta operation)})))))

(defn- rejected-before-send [a kind]
  (is (= kind (:kind a)))
  (is (empty? (:submitted a)))
  (is (empty? (:observed a)))
  (is (not (some #{:transact} (:calls a)))))

(deftest valid-registered-predicates-are-accepted-test
  (let [a (execute (configuration (plan)))]
    (is (= :incorporated (:status a)))
    (is (= 1 (count (:submitted a))))
    (is (= [:read :targets :predicates :construct :transact] (:calls a)))
    (is (= [:prepared :physical-outcome] (mapv :phase (:observed a))))
    (is (= resolved/predicate-contract-scope
           (get-in a [:metadata :gesso.model/predicate-contract-scope])))))

(deftest declared-predicate-inputs-precede-domain-construction-test
  (let [a (execute (configuration (plan)))]
    (is (< (.indexOf (:calls a) :predicates)
           (.indexOf (:calls a) :construct)))
    (is (< (.indexOf (:calls a) :targets)
           (.indexOf (:calls a) :predicates)))
    (is (contains? (:assumed resolved/predicate-contract-scope)
                   :requirements-independent-of-constructed-plan))))

(deftest same-document-without-active-condition-is-rejected-test
  (let [a (execute (configuration (plan member version version)))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (= 1 (get-in a [:details :missing-guard-check-count])))
    (is (zero? (get-in a [:details :unexpected-guard-check-count])))))

(deftest same-document-with-false-active-condition-is-rejected-test
  (let [a (execute (configuration (plan (assoc member :active false)
                                        guarded-version version)))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (= 1 (get-in a [:details :missing-guard-check-count])))
    (is (= 1 (get-in a [:details :unexpected-guard-check-count])))))

(deftest same-document-with-wrong-version-value-is-rejected-test
  (let [a (execute (configuration (plan (assoc member :revision 6)
                                        guarded-version version)))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (pos? (get-in a [:details :unexpected-guard-check-count])))))

(deftest correct-membership-guard-with-missing-request-update-check-is-rejected-test
  (let [a (execute (configuration (plan member guarded-version
                                        (assoc version :compare-keys [:updated-at]))))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (= 1 (get-in a [:details :missing-update-check-count])))))

(deftest correct-request-target-with-wrong-update-prestate-is-rejected-test
  (let [wrong-before (assoc before :updated-at "PRIVATE-UNEXPECTED-TIME")
        altered {:guards [(command/guard :memberships member guarded-version)]
                 :commands [(command/update-command
                             :requests :claim wrong-before after version)]
                 :emit false}
        a (execute (configuration altered))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (pos? (get-in a [:details :unexpected-update-check-count])))))

(deftest optional-checks-may-be-omitted-test
  (let [permissive (update (predicates {:membership-id membership-id} command-binding)
                           :required-update-checks (constantly #{}))
        p (plan member guarded-version (assoc version :compare-keys [:updated-at]))
        a (execute (configuration p (fn [_ _] permissive)))]
    (is (= :incorporated (:status a)))
    (is (= 1 (count (:submitted a))))))

(deftest extra-guard-check-not-allowed-is-rejected-test
  (let [p (plan (assoc member :extra-secret "PRIVATE-EXTRA")
                (assoc guarded-version :compare-keys
                       [:revision :updated-at :active :extra-secret])
                version)
        a (execute (configuration p))]
    (rejected-before-send a :construction-predicate-mismatch)
    (is (= 1 (get-in a [:details :unexpected-guard-check-count])))))

(deftest missing-required-guard-and-update-declaration-fails-test
  (let [declared (predicates {:membership-id membership-id} command-binding)
        bad (assoc declared
                   :required-guard-checks #{[[:memberships membership-id] :unknown 1]})
        a (execute (configuration (plan) (fn [_ _] bad)))]
    (rejected-before-send a :invalid-predicate-contract)))

(deftest malformed-declaration-shapes-fail-before-construction-test
  (let [good (predicates {:membership-id membership-id} command-binding)
        bads [nil {} []
              (assoc good :surprise "PRIVATE-SURPRISE")
              (dissoc good :allowed-guard-checks)
              (assoc good :required-guard-checks [])
              (assoc good :required-guard-checks #{[[:memberships membership-id] :active]})
              (assoc good :required-guard-checks #{[[:memberships membership-id] :active true :extra]})
              (assoc good :required-guard-checks #{[[:memberships membership-id] :active nil]})
              (assoc good :required-guard-checks #{[[:memberships nil] :active true]})
              (assoc good :required-guard-checks #{[["memberships" membership-id] :active true]})
              (assoc good :allowed-guard-checks #{[[:other "PRIVATE"] :active true]})
              (assoc good :allowed-update-checks #{[[:other "PRIVATE"] :revision 0]})
              (assoc good :allowed-guard-checks #{[[:gesso_commands "PRIVATE"] :active true]})
              (assoc good :allowed-update-checks #{[[:gesso_commands "PRIVATE"] :revision 0]})
              (assoc good :allowed-update-checks #{})
              (assoc good :allowed-guard-checks #{})]]
    (doseq [bad bads]
      (let [a (execute (configuration (plan) (fn [_ _] bad)))]
        (rejected-before-send a :invalid-predicate-contract)
        (is (not (some #{:construct} (:calls a))))
        (is (not (str/includes? (pr-str (ex-data (:exception a))) "PRIVATE-SURPRISE")))
        (is (not (str/includes? (pr-str (ex-data (:exception a))) membership-id)))))))

(deftest predicate-requires-closed-target-contract-test
  (let [base (configuration (plan))
        no-target (dissoc base :target-contract!)
        no-tables (dissoc base :construction-contract)]
    (doseq [c [no-target no-tables]]
      (is (= :invalid-configuration
             (try (resolved/build-operation c) nil
                  (catch clojure.lang.ExceptionInfo e (error-kind e))))))))

(deftest invalid-predicate-registration-is-rejected-test
  (doseq [bad [nil {} true :not-a-function]]
    (is (= :invalid-predicate-contract
           (try (resolved/build-operation
                 (assoc (configuration (plan)) :predicate-contract! bad))
                nil
                (catch clojure.lang.ExceptionInfo e (error-kind e)))))))

(deftest closed-table-and-target-gates-still-precede-predicate-gate-test
  (let [wrong-member (assoc member :xt/id "PRIVATE-WRONG-MEMBER")
        a (execute (configuration (plan wrong-member guarded-version version)))]
    (rejected-before-send a :construction-target-mismatch)))

(deftest old-operations-without-predicate-contract-remain-compatible-test
  (let [c (dissoc (configuration (plan member version version))
                  :predicate-contract!)
        a (execute c)]
    (is (= :incorporated (:status a)))
    (is (nil? (get-in a [:metadata :gesso.model/predicate-contract-scope])))
    (is (= 1 (count (:submitted a))))))

(deftest operation-with-only-base-configuration-remains-compatible-test
  (let [c (dissoc (configuration (plan))
                  :predicate-contract! :target-contract! :construction-contract)
        a (execute c)]
    (is (= :incorporated (:status a)))
    (is (= 1 (count (:submitted a))))
    (is (= [:read :construct :transact] (:calls a)))))

(deftest reconciliation-skips-domain-and-contract-callbacks-test
  (let [a (execute (configuration (plan)) {:membership-id membership-id}
                   command-binding :reconcile)]
    (is (= :unresolved (:status a)))
    (is (= [:read] (:calls a)))
    (is (empty? (:submitted a)))
    (is (empty? (:observed a)))))

(deftest existing-record-prevents-constructing-and-rechecking-test
  (let [c (assoc (configuration (plan)) :read-record!
                 (fn [_ _] {:xt/id "existing" :command/id (:command-id command-binding)
                            :command/operation :request/claim
                            :command/principal (:principal command-binding)
                            :command/arguments (:arguments command-binding)
                            :command/outcome {:status :claimed}}))
        a (execute c)]
    ;; Existing-record reconciliation must occur before any new plan is built.
    (is (not (some #{:construct :predicates :transact} (:calls a))))
    (is (empty? (:submitted a)))
    (is (empty? (:observed a)))))

(deftest predicate-report-does-not-claim-authorization-or-certification-test
  (let [scope resolved/predicate-contract-scope]
    (is (= :gesso.model.resolved-operation/closed-expected-checks-v1
           (:construction scope)))
    (is (contains? (:not-established scope)
                   :independent-authorization-policy-discovery))
    (is (contains? (:not-established scope)
                   :complete-authorization-or-business-predicates))
    (is (contains? (:not-established scope)
                   :application-certification))
    (is (not (contains? (:checked resolved/assurance-scope)
                        :exact-guard-and-update-expected-check-coverage)))))

(deftest errors-do-not-reveal-secret-values-test
  (let [a (execute (configuration
                    (plan (assoc member :active false)
                          guarded-version version)))
        encoded (pr-str (ex-data (:exception a)))]
    (is (= :construction-predicate-mismatch (:kind a)))
    (doseq [private-value [request-id membership-id "private-v1051-principal"]]
      (is (not (str/includes? encoded private-value))))))

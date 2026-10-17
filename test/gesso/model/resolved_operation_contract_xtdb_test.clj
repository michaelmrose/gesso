(ns gesso.model.resolved-operation-contract-xtdb-test
  "v1056 genuine embedded-XTDB adversarial downstream-operation tests.

   Every attempt uses the real gesso.model.tx/transact! path, not an
   intercepted mock. Trusted construction declarations come from request,
   membership, and principal inputs independently of :construct!. The tests
   establish only rejection of the deliberately incorrect constructed plans
   BEFORE any physical send; they do not establish correctness or completeness
   of the independently declared business rules, XTDB refinement, or proof."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))
(defn- with-real-xtdb [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-real-xtdb)

(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private membership-version
  (assoc version :compare-keys [:revision :updated-at :active]))
(def ^:private now "v1056-time-1")
(def ^:private operation :request/claim)

(defn- read-doc [table id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- fixture []
  (let [suffix (str (random-uuid))
        request-id (str "private-v1056-request-" suffix)
        member-id (str "private-v1056-membership-" suffix)
        helper-id (str "private-v1056-helper-" suffix)
        before {:xt/id request-id :revision 0 :created-at "t0"
                :updated-at "t0" :owner "unclaimed"}
        membership {:xt/id member-id :revision 0 :created-at "t0"
                    :updated-at "t0" :active true}
        after (assoc before :revision 1 :updated-at now :owner helper-id)
        binding {:command-id (identity/command-id (str "v1056-command-" suffix))
                 :principal (identity/principal helper-id)
                 :operation operation
                 :arguments {:request-id request-id}}]
    (xt/execute-tx @!node [[:put-docs :requests before]
                            [:put-docs :memberships membership]])
    {:before before :membership membership :after after
     :request-id request-id :member-id member-id :helper-id helper-id
     :binding binding}))

(defn- domain-plan [{:keys [before after membership]}]
  {:commands [(command/update-command :requests :claim before after version)]
   :guards [(command/guard :memberships membership membership-version)]
   :emit false})

(defn- expected-post-state [{:keys [after request-id]}]
  (into #{} (map (fn [[field value]] [:requests request-id field value]) after)))

(defn- configuration [f plan]
  (let [{:keys [request-id member-id]} f
        requested-post-state (expected-post-state f)]
    {:operation operation
     :read-record! (fn [_ id] (read-doc :gesso_commands id))
     :may-disclose? (fn [_ _ _] true)
     :construct! (fn [_ _] {:now now :plan plan :outcome {:status :claimed}})
     :construction-contract
     {:required-guard-tables #{:memberships}
      :allowed-guard-tables #{:memberships}
      :required-write-tables #{:requests :gesso_commands}
      :allowed-write-tables #{:requests :gesso_commands}}
     :target-contract!
     (fn [_ _]
       {:required-guard-targets #{[:memberships member-id]}
        :allowed-guard-targets #{[:memberships member-id]}
        :required-write-targets #{[:requests request-id]}
        :allowed-write-targets #{[:requests request-id]}})
     :predicate-contract!
     (fn [_ _]
       {:required-guard-checks #{[:memberships member-id :active true]}
        :allowed-guard-checks #{[:memberships member-id :active true]
                                [:memberships member-id :revision 0]
                                [:memberships member-id :updated-at "t0"]}
        :required-update-checks #{[:requests request-id :revision 0]}
        :allowed-update-checks #{[:requests request-id :revision 0]
                                 [:requests request-id :updated-at "t0"]}})
     :command-operation-contract!
     (fn [_ _]
       {:required-command-operations #{[:requests request-id :claim]}
        :allowed-command-operations #{[:requests request-id :claim]}})
     :post-state-contract!
     (fn [_ _]
       {:required-post-state-values #{[:requests request-id :owner (:helper-id f)]}
        :allowed-post-state-values requested-post-state})}))

(defn- attempt [f plan]
  (let [observed (atom [])
        resolved (resolved/build-operation (configuration f plan))
        ctx {:biff.xtdb/node @!node
             resolved/observation-context-key #(swap! observed conj %)}
        outcome (try {:value (resolved ctx {:binding (:binding f) :mode :submit})}
                     (catch clojure.lang.ExceptionInfo e {:error e}))]
    {:outcome outcome :phases (mapv :phase @observed)
     :metadata (meta resolved)}))

(defn- error-kind [result]
  (some-> result :outcome :error ex-data :error/kind))

(defn- assert-pre-submit-rejection! [f changed expected-kind]
  (let [result (attempt f changed)
        error-data (ex-data (get-in result [:outcome :error]))]
    (is (= expected-kind (error-kind result)))
    (is (empty? (:phases result)) "No Gesso prepared-send or physical-outcome event")
    (is (= (:before f) (read-doc :requests (:request-id f))))
    (is (nil? (read-doc :gesso_commands
                        (resolution/record-id (:binding f)))))
    (doseq [private-value [(:request-id f) (:member-id f) (:helper-id f)]]
      (is (not (str/includes? (pr-str error-data) private-value))
          "Rejection diagnostics must omit private document identities"))))

(deftest correct-declared-claim-commits-to-real-xtdb
  (let [f (fixture)
        result (attempt f (domain-plan f))]
    (is (= :incorporated (get-in result [:outcome :value :status])))
    (is (= :committed (get-in result [:outcome :value :transaction :commit/status])))
    (is (= [:prepared :physical-outcome] (:phases result)))
    (is (= (:after f) (read-doc :requests (:request-id f))))
    (is (some? (read-doc :gesso_commands
                        (resolution/record-id (:binding f)))))
    (doseq [k [:gesso.model/construction-contract-scope
               :gesso.model/target-contract-scope
               :gesso.model/predicate-contract-scope
               :gesso.model/command-operation-contract-scope
               :gesso.model/post-state-contract-scope]]
      (is (contains? (:metadata result) k)))
    (let [again (attempt f (domain-plan f))]
      (is (= :replay (get-in again [:outcome :value :status])))
      (is (empty? (:phases again))))))

(deftest missing-membership-guard-is-detected-before-real-xtdb-send
  (let [f (fixture)]
    (assert-pre-submit-rejection!
     f (assoc (domain-plan f) :guards []) :construction-contract-mismatch)))

(deftest right-table-wrong-membership-is-detected-before-real-xtdb-send
  (let [f (fixture)
        wrong (assoc (:membership f) :xt/id (str "private-other-" (random-uuid)))]
    (assert-pre-submit-rejection!
     f (assoc (domain-plan f) :guards [(command/guard :memberships wrong membership-version)])
     :construction-target-mismatch)))

(deftest right-membership-missing-active-guard-is-detected-before-real-xtdb-send
  (let [f (fixture)]
    (assert-pre-submit-rejection!
     f (assoc (domain-plan f) :guards [(command/guard :memberships (:membership f) version)])
     :construction-predicate-mismatch)))

(deftest right-request-wrong-business-operation-is-detected-before-real-xtdb-send
  (let [f (fixture)
        wrong (assoc-in (domain-plan f) [:commands 0 :model/operation] :cancel)]
    (assert-pre-submit-rejection! f wrong :construction-command-operation-mismatch)))

(deftest right-request-wrong-owner-is-detected-before-real-xtdb-send
  (let [f (fixture)
        changed (assoc-in (domain-plan f) [:commands 0 :model/after :owner]
                          "private-unauthorized-helper")]
    (assert-pre-submit-rejection! f changed :construction-post-state-mismatch)))

(deftest undeclared-post-state-field-is-detected-before-real-xtdb-send
  (let [f (fixture)
        changed (assoc-in (domain-plan f) [:commands 0 :model/after :admin] true)]
    (assert-pre-submit-rejection! f changed :construction-post-state-mismatch)))

(deftest declared-contracts-are-not-general-authorization-proofs
  (let [f (fixture)
        result (attempt f (domain-plan f))
        scope (get-in result [:metadata :gesso.model/post-state-contract-scope])]
    (is (= :incorporated (get-in result [:outcome :value :status])))
    (is (contains? (:assumed scope) :complete-declaration-of-domain-post-state-semantics))
    (is (contains? (:not-established scope) :authorization-or-policy-correctness))
    (is (contains? (:not-established scope) :independent-business-rule-discovery))))

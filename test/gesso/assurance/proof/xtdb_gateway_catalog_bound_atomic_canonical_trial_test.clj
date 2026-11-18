(ns gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial-test
  "v1775: Original catalog is the ONLY declaration/invariant source used by
   one real-XTDB typed canonical execution trial. Still not installed policy."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.set :as set]
            [gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial
             :as catalog-bound]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial-test]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private model-ns 'gesso.model.resolved-operation-test)
(def ^:private typed-ns
  'gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test)
(defn- helper [ns-name sym]
  (var-get (or (ns-resolve ns-name sym)
               (throw (ex-info "Missing fixture helper" {:ns ns-name :name sym})))))
(defn- model [s] (helper model-ns s))
(defn- typed [s] (helper typed-ns s))
(use-fixtures :once (model 'with-real-xtdb))

(defn- original-catalog []
  (let [d (typed 'declaration)
        originals (typed 'premises)
        construction (-> d
                         (dissoc :assignments)
                         (assoc :replacements (:assignments d)
                                :frame (set/difference
                                        (set (keys (:state-types d)))
                                        (:writes d))))]
    {:id :request/catalog-bound-full-atomic
     :declared-operation-ids #{:request/claim}
     :initial-condition
     {:readers #{:doc/owner}
      :expression [:not= [:read :doc/owner] [:literal ""]]}
     :invariants (:invariants originals)
     :operations [{:id :request/claim :construction construction}]}))
(defn- source []
  {:catalog (original-catalog) :physical-field-map (typed 'mapping)})
(defn- issue
  ([] (issue (source) {:input/owner "helper-a"}))
  ([original inputs]
   (catalog-bound/issue-catalog-bound-canonical-trial!
    original :request/claim inputs)))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))
(defn- data [] ((model 'fixture-data)))
(defn- environment [d opts] ((model 'real-environment) d opts))
(defn- submit [e mode] ((model 'real-call) e mode))
(defn- seed! [& args] (apply (model 'seed!) args))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))

(deftest checked-original-catalog-derives-full-typed-source-not-copied-premises
  (let [catalog (original-catalog)
        checked (operation-set/check-operation-set! catalog)
        issued (issue)
        summary ((:summary! issued))]
    (is (= operation-set/report-type (:type checked)))
    (is (= :closed-declared-abstract-operation-set-only (:scope checked)))
    (is (= catalog-bound/report-type (:type issued) (:type summary)))
    (is (= :not-attempted (:status summary)))
    (is (= :original-checked-catalog-construction
           (:declaration-source summary)))
    (is (= :same-original-checked-catalog (:invariant-inventory summary)))
    (is (= :derived-from-original-invariant-rules (:read-premises summary)))
    (is (= :not-established
           (:catalog-provenance-in-deployed-application summary)))
    (is (= :not-established (:authentic-business-policy summary)))
    (is (= :not-issued (:certificate summary)))
    (is (not-any? #(contains? summary %)
                  [:catalog :declaration :invariants :read-premises-source
                   :inputs :field-map :plan :tx-ops :submit! :executor]))))

(deftest catalog-bound-original-trial-commits-domain-and-ledger-on-xtdb
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue)]
    (seed! [:requests request])
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :bounded-catalog-bound-atomic-acknowledgment (:status r)))
      (is (= :bounded-catalog-bound-atomic-acknowledgment
             (:status ((:summary! issued)))))
      (is (= :single-original-model-transaction-acknowledged
             (:actual-canonical-execution r)))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record
              binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= :replay (:status (submit e :reconcile))))
      (is (= 1 @(:constructs e)))
      (is (= :not-established (:certified-executor-adoption r)))
      (is (= :none (:authority r) (:retry-authority r)))
      (is (= :catalog-bound-trial-already-attempted
             (reason (fn [] ((:observe-once! issued) (fn [] (submit e :submit))))))))))

(deftest catalog-source-mutating-typed-guard-cannot-authorize-physical-write
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        bad (assoc-in (source) [:catalog :operations 0 :construction
                                :guard :expression]
                      [:= [:read :doc/owner] [:literal "different-owner"]])
        issued (issue bad {:input/owner "helper-a"})]
    (seed! [:requests request])
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :catalog-bound-typed-check-not-established (:status r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :not-established (:actual-canonical-execution r)))
      (is (= :not-issued (:certificate r))))))

(deftest missing-or-incorrect-catalog-invariant-is-not-a-submittable-source
  (let [bad-source (update-in (source) [:catalog :invariants]
                              (constantly {}))]
    (is (= :invalid-original-operation-set
           (reason #(issue bad-source {:input/owner "helper-a"})))))
  (let [bad (update-in (source) [:catalog :invariants]
                       (constantly
                        {:doc/other-not-empty
                         {:readers #{:doc/created}
                          :expression [:not= [:read :doc/created]
                                        [:literal ""]]}}))]
    (is (= :selected-original-declaration-or-premises-diverged
           (reason #(issue bad {:input/owner "helper-a"}))))))

(deftest altered-mapping-or-unexpected-operation-is-rejected
  (let [bad (assoc-in (source) [:physical-field-map :doc/owner] :revision)]
    (is (= :selected-original-declaration-or-premises-diverged
           (reason #(issue bad {:input/owner "helper-a"})))))
  (is (= :catalog-selected-operation-or-rules-not-established
         (reason #(catalog-bound/issue-catalog-bound-canonical-trial!
                   (source) :request/cancel {:input/owner "helper-a"})))))

(deftest incorrect-input-is-blocked-at-the-real-canonical-boundary
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue (source) {:input/owner "wrong-owner"})]
    (seed! [:requests request])
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :catalog-bound-typed-check-not-established (:status r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding)))))))

(deftest stale-xtdb-guard-rejects-catalog-bound-complete-atomic-write
  (let [{:keys [request membership binding] :as d} (data)
        e (environment d {:guard-membership? true})
        issued (issue)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {}
                                        {:commands [((model 'revoke-command)
                                                     membership)]
                                         :emit false})))
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :catalog-bound-canonical-rejected (:status r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority r))))))

(deftest substituted-original-catalog-checker-is-not-a-proof-source
  (let [issued (issue)]
    (with-redefs [operation-set/check-operation-set!
                  (fn [& _] {:scope :forged :authority :all})]
      (is (= :substituted-catalog-or-trial-boundary
             (reason #((:observe-once! issued) (fn [] :fake))))))
    (is (= :catalog-bound-trial-already-attempted
           (reason #((:observe-once! issued) (fn [] :fake)))))))

(deftest weakened-but-checked-catalog-remains-unauthed
  (let [weak (assoc-in (source) [:catalog :invariants :doc/owner-not-blank
                               :expression]
                       [:not= [:read :doc/owner] [:literal "not-present"]])
        issued (issue weak {:input/owner "helper-a"})
        summary ((:summary! issued))]
    (is (= :same-original-checked-catalog (:invariant-inventory summary)))
    (is (= :not-established (:authentic-business-policy summary)))
    (is (= :not-established
           (:catalog-provenance-in-deployed-application summary)))
    (is (= :not-issued (:certificate summary)))))

(deftest lost-acknowledgment-is-not-qualified-by-a-catalog-match
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue)
        original tx/transact!
        calls (atom 0)]
    (seed! [:requests request])
    (let [r (with-redefs [tx/transact!
                         (fn [ctx plan]
                           (swap! calls inc)
                           (original ctx plan)
                           (throw (ex-info "physical acknowledgment lost" {})))]
              ((:observe-once! issued) #(submit e :submit)))]
      (is (= :not-qualified-or-uncertain (:status r)))
      (is (= :not-established (:actual-canonical-execution r)))
      (is (= 1 @calls))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority r))))))

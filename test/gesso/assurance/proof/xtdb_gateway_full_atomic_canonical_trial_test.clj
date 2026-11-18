(ns gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial-test
  "v1774: the original canonical Gesso writer may submit only after the exact
   in-invocation source-relative typed FULL atomic preparation passes. All
   physical observations are from the real embedded XTDB fixture."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial :as trial]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation :as full]
            [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private model-fixtures 'gesso.model.resolved-operation-test)
(def ^:private typed-fixtures
  'gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation-test)
(defn- helper [n s]
  (var-get (or (ns-resolve n s)
               (throw (ex-info "Missing fixture" {:namespace n :name s})))))
(defn- model [s] (helper model-fixtures s))
(defn- typed [s] (helper typed-fixtures s))
(use-fixtures :once (model 'with-real-xtdb))

(defn- data [] ((model 'fixture-data)))
(defn- environment [d opts] ((model 'real-environment) d opts))
(defn- submit [e mode] ((model 'real-call) e mode))
(defn- seed! [& args] (apply (model 'seed!) args))
(defn- read-request [id] ((model 'read-request) id))
(defn- read-command [id] ((model 'read-command) id))
(defn- issue
  ([] (issue :request/claim (typed 'declaration) {:input/owner "helper-a"}))
  ([operation declaration inputs]
   (trial/issue-typed-full-atomic-canonical-trial!
    operation declaration inputs (typed 'mapping) (typed 'premises))))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest acknowledged-typed-domain-plus-atomic-ledger-on-real-xtdb
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue)]
    (seed! [:requests request])
    (is (= :not-attempted (:status ((:summary! issued)))))
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= trial/report-type (:type r)))
      (is (= trial/report-version (:version r)))
      (is (= :bounded-typed-full-atomic-canonical-acknowledgment
             (:status r) (:status ((:summary! issued)))))
      (is (= :checked-before-model-transaction
             (:typed-preparation-at-canonical-submit r)))
      (is (= :original-and-independent-source-relative
             (:typed-domain-semantics r)))
      (is (= :one-guarded-incorporated-plan (:domain-and-command-ledger r)))
      (is (= :original-model-tx-transact (:canonical-writer r)))
      (is (= :not-established (:typed-complete-cas-gateway-adoption r)))
      (is (= :not-established (:authenticated-xtdb-receipt-and-durability r)))
      (is (= :not-established (:actual-live-delivery r)))
      (is (= :not-issued (:certificate r)))
      (is (= :none (:retry-authority r) (:authority r)))
      (is (not-any? #(contains? r %)
                    [:plan :tx-ops :request :source :binding :record :result
                     :execution :submit! :transaction :receipt :node :witness]))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record
              binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= :replay (:status (submit e :reconcile))))
      (is (= 1 @(:constructs e)))
      (is (= :typed-canonical-trial-already-attempted
             (reason (fn [] ((:observe-once! issued) (fn [] (submit e :submit))))))))))

(deftest changed-typed-guard-rejects-before-canonical-model-write
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        bad (assoc-in (typed 'declaration) [:guard :expression]
                      [:= [:read :doc/owner] [:literal "not-unclaimed"]])
        issued (issue :request/claim bad {:input/owner "helper-a"})]
    (seed! [:requests request])
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :typed-preparation-not-established-before-submit (:status r)))
      (is (= :not-established (:typed-preparation-at-canonical-submit r)))
      (is (= :not-established (:canonical-writer r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= 1 @(:constructs e)))
      (is (= :unresolved (:status (submit e :reconcile))))
      (is (= :not-issued (:certificate r))))))

(deftest incorrect-typed-input-rejects-before-canonical-writer
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue :request/claim (typed 'declaration)
                      {:input/owner "different-owner"})]
    (seed! [:requests request])
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :typed-preparation-not-established-before-submit (:status r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :none (:transaction-authority r))))))

(deftest stale-dependency-cas-rejects-whole-typed-atomic-operation
  (let [{:keys [request membership binding] :as d} (data)
        e (environment d {:guard-membership? true})
        issued (issue)]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((model 'database))
                   (:tx-ops (tx/prepare {} {:commands [((model 'revoke-command)
                                                        membership)]
                                            :emit false})))
    (let [r ((:observe-once! issued) #(submit e :submit))]
      (is (= :typed-full-atomic-canonical-submission-rejected (:status r)))
      (is (= :checked-before-model-transaction
             (:typed-preparation-at-canonical-submit r)))
      (is (= :original-model-tx-transact (:canonical-writer r)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :unresolved (:status (submit e :reconcile))))
      (is (= :none (:retry-authority r))))))

(deftest no-submit-or-wrong-operation-never-produces-typed-physical-proof
  (doseq [[operation mode] [[:request/claim :reconcile]
                            [:request/cancel :submit]]]
    (let [{:keys [request binding] :as d} (data)
          e (environment d nil)
          issued (issue operation (typed 'declaration) {:input/owner "helper-a"})]
      (seed! [:requests request])
      (let [r ((:observe-once! issued) #(submit e mode))]
        (is (= :no-canonical-submission-observed (:status r)))
        (is (= :not-established (:typed-preparation-at-canonical-submit r)))
        (is (= request (read-request (:xt/id request))))
        (is (nil? (read-command (resolution/record-id binding))))
        (is (= :not-issued (:certificate r)))))))

(deftest substituted-typed-preparer-is-fail-closed
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        issued (issue)]
    (seed! [:requests request])
    (with-redefs [full/issue-full-atomic-typed-preparer!
                  (fn [& _]
                    {:prepare-once! (fn [] {:status :synthetic-success})})]
      (is (= :substituted-typed-or-canonical-trial-boundary
             (reason (fn [] ((:observe-once! issued)
                             (fn [] (submit e :submit))))))))
    (is (= request (read-request (:xt/id request))))
    (is (nil? (read-command (resolution/record-id binding))))
    (is (= :typed-canonical-trial-already-attempted
           (reason (fn [] ((:observe-once! issued) (fn [] (submit e :submit)))))))))

(deftest lost-acknowledgment-is-never-promoted-to-committed
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)
        original tx/transact!
        calls (atom 0)
        issued (issue)]
    (seed! [:requests request])
    (let [r (with-redefs [tx/transact! (fn [ctx plan]
                                        (swap! calls inc)
                                        (original ctx plan)
                                        (throw (ex-info "acknowledgment lost" {})))]
              ((:observe-once! issued) #(submit e :submit)))]
      (is (= :not-qualified-or-uncertain (:status r)))
      (is (= :checked-before-model-transaction
             (:typed-preparation-at-canonical-submit r)))
      (is (= 1 @calls))
      (is (= ((model 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (some? (read-command (resolution/record-id binding))))
      (is (= :none (:retry-authority r))))))

(deftest normal-canonical-writer-remains-unmodified-without-trial
  (let [{:keys [request binding] :as d} (data)
        e (environment d nil)]
    (seed! [:requests request])
    (is (= :incorporated (:status (submit e :submit))))
    (is (= ((model 'next-version) request {:owner "helper-a"})
           (read-request (:xt/id request))))
    (is (some? (read-command (resolution/record-id binding))))))

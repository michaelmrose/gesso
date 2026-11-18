(ns gesso.model.guarded-atomic-canonical-trial-test
  "v1772: One actual canonical transaction, with both domain and durable
   ledger, under guarded pre-submit qualification. No separate writer used."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.model.guarded-atomic-canonical-trial :as trial]
            [gesso.model.guarded-atomic-incorporation-preparation :as guarded]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]))

(def ^:private originals 'gesso.model.resolved-operation-test)
(defn- helper [sym]
  (var-get (or (ns-resolve originals sym)
               (throw (ex-info "Missing original fixture" {:name sym})))))
(use-fixtures :once (helper 'with-real-xtdb))

(defn- seed! [& pairs] (apply (helper 'seed!) pairs))
(defn- data [] ((helper 'fixture-data)))
(defn- env [source opts] ((helper 'real-environment) source opts))
(defn- submit [environment mode] ((helper 'real-call) environment mode))
(defn- read-request [id] ((helper 'read-request) id))
(defn- read-command [id] ((helper 'read-command) id))
(defn- read-membership [id] ((helper 'read-membership) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest real-xtdb-canonical-atomic-write-has-guarded-preparation
  (let [{:keys [request binding] :as d} (data)
        e (env d nil)
        issued (trial/issue-guarded-canonical-trial! :request/claim)]
    (seed! [:requests request])
    (is (= :not-attempted (:status ((:summary! issued)))))
    (let [report ((:observe-once! issued) #(submit e :submit))]
      (is (= trial/report-type (:type report)))
      (is (= trial/report-version (:version report)))
      (is (= :bounded-guarded-atomic-canonical-acknowledgment (:status report)))
      (is (= :checked-before-model-transaction
             (:canonical-guarded-atomic-preparation report)))
      (is (= :one-guarded-incorporated-plan (:domain-and-ledger report)))
      (is (= :original-model-tx-transact (:canonical-writer report)))
      (is (= :not-established (:physical-durability-and-receipt report)))
      (is (= :not-established (:typed-complete-cas-gateway-adoption report)))
      (is (= :not-established (:actual-live-delivery report)))
      (is (= :not-issued (:certificate report)))
      (is (= :none (:retry-authority report)))
      (is (not-any? #(contains? report %)
                    [:plan :tx-ops :ctx :binding :record :result :request
                     :execution :witness :transaction :handler :submit!]))
      (is (= ((helper 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= :replay (:status (submit e :reconcile))))
      (is (= 1 @(:constructs e)))
      (is (= :one-shot-canonical-trial-already-attempted
             (reason (fn [] ((:observe-once! issued) (fn [] (submit e :submit))))))))))

(deftest stale-xtdb-dependency-guard-preserves-domain-and-ledger
  (let [{:keys [request membership binding] :as d} (data)
        e (env d {:guard-membership? true})]
    (seed! [:requests request] [:memberships membership])
    (xt/execute-tx ((helper 'database))
                   (:tx-ops (tx/prepare {} {:commands [((helper 'revoke-command) membership)]
                                            :emit false})))
    (let [issued (trial/issue-guarded-canonical-trial! :request/claim)
          report ((:observe-once! issued) #(submit e :submit))]
      (is (= :guarded-canonical-submission-rejected (:status report)))
      (is (= :checked-before-model-transaction
             (:canonical-guarded-atomic-preparation report)))
      (is (= request (read-request (:xt/id request))))
      (is (false? (:active (read-membership (:xt/id membership)))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :unresolved (:status (submit e :reconcile))))
      (is (= :not-issued (:certificate report)))
      (is (= :none (:retry-authority report))))))

(deftest reconcile-does-not-qualify-as-canonical-submission
  (let [{:keys [request] :as d} (data)
        e (env d nil)
        issued (trial/issue-guarded-canonical-trial! :request/claim)]
    (seed! [:requests request])
    (let [report ((:observe-once! issued) #(submit e :reconcile))]
      (is (= :no-canonical-submission-observed (:status report)))
      (is (= :not-established (:canonical-guarded-atomic-preparation report)))
      (is (= request (read-request (:xt/id request))))
      (is (zero? @(:constructs e))))))

(deftest wrong-selected-operation-blocks-before-physical-tx
  (let [{:keys [request binding] :as d} (data)
        e (env d nil)
        issued (trial/issue-guarded-canonical-trial! :request/cancel)]
    (seed! [:requests request])
    (let [report ((:observe-once! issued) #(submit e :submit))]
      (is (= :not-qualified-or-uncertain (:status report)))
      (is (= :not-established (:canonical-guarded-atomic-preparation report)))
      (is (= request (read-request (:xt/id request))))
      (is (nil? (read-command (resolution/record-id binding))))
      (is (= :not-issued (:certificate report))))))

(deftest substituted-guarded-preparer-fails-closed-before-physical-write
  (let [{:keys [request binding] :as d} (data)
        e (env d nil)
        issued (trial/issue-guarded-canonical-trial! :request/claim)]
    (seed! [:requests request])
    (with-redefs [guarded/issue-guarded-atomic-preparer!
                  (fn [_] {:prepare-once! (fn [] {:status :forged})})]
      (is (= :substituted-canonical-trial-boundary
             (reason (fn [] ((:observe-once! issued)
                             (fn [] (submit e :submit))))))))
    (is (= request (read-request (:xt/id request))))
    (is (nil? (read-command (resolution/record-id binding))))
    (is (zero? @(:constructs e)))))

(deftest lost-ack-cannot-be-promoted-or-safely-retried
  (let [{:keys [request binding] :as d} (data)
        e (env d nil)
        issued (trial/issue-guarded-canonical-trial! :request/claim)
        original tx/transact!
        physical-writes (atom 0)]
    (seed! [:requests request])
    (let [report
          (with-redefs [tx/transact!
                        (fn [ctx plan]
                          (swap! physical-writes inc)
                          (original ctx plan)
                          (throw (ex-info "ack lost" {})))]
            ((:observe-once! issued) #(submit e :submit)))]
      (is (= :not-qualified-or-uncertain (:status report)))
      (is (= :checked-before-model-transaction
             (:canonical-guarded-atomic-preparation report)))
      (is (= 1 @physical-writes))
      (is (= ((helper 'next-version) request {:owner "helper-a"})
             (read-request (:xt/id request))))
      (is (= (resolution/command-record binding {:status :claimed :owner "helper-a"} "t1")
             (read-command (resolution/record-id binding))))
      (is (= :replay (:status (submit e :reconcile))))
      (is (= :none (:retry-authority report))))))

(deftest diagnostic-failure-after-real-commit-never-makes-it-uncommitted
  (let [{:keys [request binding] :as d} (data)
        e (env d nil)]
    (seed! [:requests request])
    (with-redefs [trial/after-canonical-submit!
                  (fn [& _] (throw (ex-info "diagnostic broken" {})))]
      (is (= :incorporated (:status (submit e :submit)))))
    (is (= ((helper 'next-version) request {:owner "helper-a"})
           (read-request (:xt/id request))))
    (is (some? (read-command (resolution/record-id binding))))))

(deftest callback-only-side-effect-does-not-masquerade-as-canonical-submit
  (let [e ((helper 'fake-environment))
        configuration (:configuration e)
        separate (atom 0)
        operation (resolved/build-operation
                   (assoc configuration :construct!
                          (fn [_ _]
                            (swap! separate inc)
                            (throw (ex-info "before canonical" {})))))
        issued (trial/issue-guarded-canonical-trial! :request/claim)
        report ((:observe-once! issued)
                #(operation {} {:binding (:binding e) :mode :submit}))]
    (is (= :no-canonical-submission-observed (:status report)))
    (is (= 1 @separate))
    (is (= :not-issued (:certificate report)))))

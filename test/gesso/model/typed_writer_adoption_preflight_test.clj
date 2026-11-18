(ns gesso.model.typed-writer-adoption-preflight-test
  "v1769: A one-document typed gateway is NOT a safe substitute for a
   resolved operation's atomic domain + durable command-result transaction."
  (:require [clojure.test :refer [deftest is]]
            [gesso.model.typed-writer-adoption-preflight :as adoption]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]))

(def ^:private originals 'gesso.model.resolved-operation-test)
(defn- helper [sym]
  (var-get (or (ns-resolve originals sym)
               (throw (ex-info "Missing test fixture helper" {:name sym})))))
(defn- e [] ((helper 'fake-environment)))
(defn- call [env mode] ((helper 'call) env mode))
(defn- candidate [data]
  ;; The current typed complete-CAS gateway admits exactly one domain update
  ;; and no command ledger; this is only its public shape, NOT verified source.
  {:plan {:commands [(first (:commands ((helper 'domain-plan) (:request data))))]
          :emit false}})

(deftest typed-single-document-writer-is-blocked-before-canonical-tx
  (let [data (e)
        physical-calls (atom 0)]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! physical-calls inc)
                                 {:commit/status :committed})]
      (let [{:keys [execution report]}
            (adoption/inspect-typed-writer-adoption!
             :request/claim (candidate data) #(call data :submit))]
        (is (= :threw (:outcome execution)))
        (is (= :typed-single-update-omits-atomic-command-ledger
               (:reason (ex-data (:error execution)))))
        (is (= :incompatible-before-canonical-model-write (:status report)))
        (is (= :typed-single-update-omits-atomic-command-ledger
               (:blocked-reason report)))
        (is (= :yes (:atomic-command-ledger-required report)))
        (is (= :one-domain-update-only (:current-typed-gateway-scope report)))
        (is (= :not-established (:typed-gateway-adopted report)))
        (is (= :not-submitted-at-canonical-callsite
               (:canonical-tx-submitted report)))
        (is (= :not-issued (:certificate report)))
        (is (= :none (:authority report)))
        (is (zero? @physical-calls))
        (is (not-any? #(contains? report %)
                      [:result :error :execution :plan :binding :record
                       :candidate :document :operation-input :transaction]))))))

(deftest regular-canonical-writer-is-left-intact
  (let [env (e) sent (atom [])]
    (with-redefs [tx/transact! (fn [_ plan]
                                 (swap! sent conj plan)
                                 {:commit/status :committed})]
      (is (= :incorporated (:status (call env :submit))))
      (is (= 1 (count @sent)))
      (let [cmds (:commands (first @sent))
            ledgers (filter #(= resolution/command-entity-type
                                (:model/entity-type %)) cmds)]
        (is (= 2 (count cmds)))
        (is (= 1 (count ledgers)))
        (is (= :create (:model/operation (first ledgers))))))))

(deftest no-canonical-submit-in-reconcile-does-not-claim-blockage
  (let [env (e)
        {:keys [execution report]}
        (adoption/inspect-typed-writer-adoption!
         :request/claim (candidate env) #(call env :reconcile))]
    (is (= :returned (:outcome execution)))
    (is (= :no-canonical-submit-reached (:status report)))
    (is (= :not-established (:blocked-reason report)))
    (is (= :not-established (:typed-gateway-adopted report)))))

(deftest invalid-candidate-and-wrong-operation-fail-before-physical-tx
  (let [env (e) physical-calls (atom 0)
        single-command (first (get-in (candidate env) [:plan :commands]))]
    (with-redefs [tx/transact! (fn [& _]
                                 (swap! physical-calls inc)
                                 {:commit/status :committed})]
      (doseq [[operation candidate-source expected]
              [[:request/other (candidate env) :wrong-canonical-operation]
               [:request/claim {:plan {:commands [] :emit false}}
                :unsupported-typed-candidate-shape]
               [:request/claim {:plan {:commands [single-command single-command]
                                        :emit false}}
                :unsupported-typed-candidate-shape]]]
        (let [{:keys [execution report]}
              (adoption/inspect-typed-writer-adoption!
               operation candidate-source #(call env :submit))]
          (is (= :threw (:outcome execution)))
          (is (= expected (:blocked-reason report)))
          (is (= :incompatible-before-canonical-model-write (:status report)))))
      (is (zero? @physical-calls)))))

(deftest nested-inspections-are-rejected
  (let [env (e)
        {:keys [execution report]}
        (adoption/inspect-typed-writer-adoption!
         :request/claim (candidate env)
         (fn [] (adoption/inspect-typed-writer-adoption!
                 :request/claim (candidate env) #(call env :submit))))]
    (is (= :threw (:outcome execution)))
    (is (= :nested-adoption-inspection
           (:reason (ex-data (:error execution)))))
    (is (= :no-canonical-submit-reached (:status report)))))

(deftest embedded-xtdb-rejects-replacement-without-writing-domain-or-ledger
  ((helper 'with-real-xtdb)
   (fn []
     (let [data ((helper 'fixture-data))
           env ((helper 'real-environment) data nil)
           request (:request data)
           binding (:binding data)]
       ((helper 'seed!) [:requests request])
       (let [{:keys [execution report]}
             (adoption/inspect-typed-writer-adoption!
              :request/claim (candidate data)
              #((helper 'real-call) env :submit))]
         (is (= :threw (:outcome execution)))
         (is (= :incompatible-before-canonical-model-write (:status report)))
         (is (= :typed-single-update-omits-atomic-command-ledger
                (:blocked-reason report)))
         (is (= request ((helper 'read-request) (:xt/id request))))
         (is (nil? ((helper 'read-command) (resolution/record-id binding))))
         (is (= :unresolved (:status ((helper 'real-call) env :reconcile))))
         (is (= 1 @(:constructs env))))))))

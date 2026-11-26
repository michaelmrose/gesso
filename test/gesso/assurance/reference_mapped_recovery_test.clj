(ns gesso.assurance.reference-mapped-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.assurance.reference-recovery-trajectory-test :as recovery]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]))

(defn- step [n source targets docs journal record puts spec]
  (let [id (:xt/id record)
        prefix (frame/formatted-prestate-assertions!
                (plan/check-target-mapping! source targets) docs journal)
        tx (fixture/tx n (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]]
                              (concat prefix puts)))]
    (cond-> {:original source :targets targets :docs docs :journal journal
             :expectation tx :prefix-count (count prefix) :command-id id}
      spec (assoc :publication-ids [] :residual-ops [] :abstract-spec spec))))

(defn- example []
  (let [docs {:gesso_requests [fixture/row] :gesso_assignments []}
        loser-targets (assoc fixture/targets :example/outcome [:journal-slot "helper-b" :example/claim])
        after-docs {:gesso_requests [(assoc fixture/row :amount 3)]
                    :gesso_assignments [{:xt/id "a1" :owner "helper-a"}]}
        refusal-source (assoc fixture/source
                              :input-types {} :reads #{:example/owner}
                              :guard {:readers #{:example/owner} :expression [:= [:read :example/owner] [:literal "helper-a"]]}
                              :writes #{:example/outcome} :replacements {:example/outcome [:literal :refused]}
                              :frame #{:example/count :example/enabled :example/owner})
        steps [(step 1 fixture/source fixture/targets docs [] recovery/winner fixture/puts fixture/spec)
               (step 2 (assoc-in fixture/source [:replacements :example/owner] [:literal "helper-b"])
                     loser-targets docs [] recovery/candidate
                     [[:put-docs :gesso_requests (assoc fixture/row :amount 3)]
                      [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-b"}]
                      (recovery/journal-put recovery/candidate)] nil)
               (step 3 refusal-source loser-targets after-docs [] recovery/refusal
                     [(recovery/journal-put recovery/refusal)] {:inputs {} :outcome-key :status})]
        history (into [fixture/initial] (map :expectation steps))]
    {:steps steps :history history :sample {:status :observed :replay-inputs history}
     :tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence :gesso_live_outbox}}))

(deftest recovery-comparison-shares-the-retained-sample
  (let [{:keys [steps history sample tables]} (example)
        predicted (temporal/replay (mapv #(dissoc % :tx-id) history))
        failure-index (:failed-assertion-index (nth (:history predicted) 2))
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 4
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        run! (fn [samples scope selected expected abort-index]
               (let [remaining (atom samples)]
                 (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (let [s (first @remaining)] (swap! remaining next) s))
                                  #'provider/compare-source-bound-execution! (fn [_ inputs _] (is (= history inputs)) comparison)
                                  #'xt/q (fn [_ query] (if (= 2 (last query))
                                                        [{:committed false :error (ex-info "abort" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx abort-index})}]
                                                        [{:committed true}]))}
                   #(provider/compare-local-processed-retained-mapped-recovery! nil scope selected expected))))
        stable (repeat 8 sample)
        matched (run! stable tables steps recovery/expected failure-index)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= provider/mapped-recovery-profile (:mapped-recovery-profile matched)))
    (is (= [1 2 3] (:selected-tx-ids matched)))
    (is (= [:committed :aborted :committed] (:selected-commitments matched)))
    (is (= [:matched-retained-projection :not-committed :matched-retained-projection] (:abstract-transitions matched)))
    (is (= :matched-retained-journal-trajectory (:recovery-linkage matched)))
    (is (= :none (:authority matched)))
    (is (= :not-issued (:certificate matched)))
    (is (= :not-established (:provider-refinement matched)))
    (doseq [[r reason] [[(run! (assoc (vec stable) 3 (assoc sample :changed true)) tables steps recovery/expected failure-index) :source-log-changed-during-recovery-comparison]
                        [(run! (assoc (vec stable) 2 (assoc sample :changed true)) tables steps recovery/expected failure-index) :source-log-changed-during-comparison]
                        [(run! (assoc (vec stable) 7 (assoc sample :changed true)) tables steps recovery/expected failure-index) :source-log-changed-during-recovery-comparison]
                        [(run! stable (disj tables :gesso_assignments) steps recovery/expected failure-index) :incomplete-mapped-recovery-scope]
                        [(run! stable tables (assoc-in steps [0 :abstract-spec :inputs :example/next-count] 4) recovery/expected failure-index) :selected-retained-abstract-transition-mismatch]
                        [(run! stable tables steps recovery/expected 0) :selected-retained-abort-error-mismatch]
                        [(run! stable tables (assoc-in steps [1 :command-id] "wrong") recovery/expected failure-index) :invalid-mapped-recovery-source]]]
      (is (= reason (:reason r)) (pr-str r))
      (is (not-any? #(contains? r %) [:selected-tx-ids :recovery-linkage :selected-commitments :abstract-transitions])))))

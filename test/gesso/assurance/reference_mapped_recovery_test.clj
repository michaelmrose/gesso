(ns gesso.assurance.reference-mapped-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.assurance.reference-recovery-trajectory-test :as recovery]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.xtdb-envelope :as envelope]
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

(defn- complete-example []
  (let [{:keys [steps tables]} (example)
        fp (str "sha256:" (apply str (repeat 64 "a")))
        context {:catalog-fingerprint fp :induction-fingerprint fp :selectors {}}
        declarations {:example/claim nil}
        complete (mapv
                  (fn [{:keys [original targets docs journal expectation prefix-count abstract-spec] :as step} record]
                    (let [spec (assoc (or abstract-spec fixture/spec)
                                      :effects (if (= :refused (get-in record [:gesso.command/outcome :status]))
                                                 {} {[:document :gesso_requests "r1"] {}
                                                     [:document :gesso_assignments "a1"] {}})
                                      :binding (:gesso.command/binding record)
                                      :outcome (:gesso.command/outcome record)
                                      :now (if (= record recovery/refusal) "t2" "t1"))
                          prefix (subvec (:ops expectation) 1 (inc prefix-count))
                          witness (evidence/derive-record! original targets docs journal prefix spec context)
                          ops (envelope/bind-transition-envelope!
                               (conj (:ops expectation) [:put-docs :gesso_transition_evidence witness]))]
                      (assoc (select-keys step [:original :targets :docs :journal :prefix-count])
                             :expectation (assoc expectation :ops ops) :spec spec :context context
                             :source-id "source-a" :declarations declarations :residual-declarations declarations)))
                  steps (mapv recovery/expected [:winner :abort :recovery]))
        history (into [fixture/initial] (map :expectation complete))]
    {:steps complete :history history :tables tables}))

(deftest complete-executor-recovery-composition
  (let [{:keys [steps history tables]} (complete-example)
        sample {:status :observed :replay-inputs history}
        failure-index (:failed-assertion-index (nth (:history (temporal/replay (mapv #(dissoc % :tx-id) history))) 2))
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 4
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        observed (atom 0)
        run! (fn [s scope]
               (reset! observed 0)
               (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (swap! observed inc) sample)
                                #'provider/compare-source-bound-execution! (constantly comparison)
                                #'xt/q (fn [_ query] (if (= 2 (last query))
                                                      [{:committed false :error (ex-info "abort" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx failure-index})}]
                                                      [{:committed true}]))}
                 #(evidence/compare-local-processed-retained-mapped-executor-recovery! nil scope s)))
        matched (run! steps tables)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= 8 @observed))
    (is (= evidence/executor-recovery-profile (:executor-recovery-profile matched)))
    (is (= [:matched-source-relative-record :matched-uncommitted-record :matched-source-relative-record]
           (:executor-envelopes matched)))
    (is (= [1 2 3] (:selected-tx-ids matched)))
    (is (= :not-established (:provider-refinement matched)))
    (is (= :none (:authority matched)))
    (is (= :not-issued (:certificate matched)))
    (doseq [i (range 3)]
      (let [ops (get-in steps [i :expectation :ops])
            corrupted (assoc-in steps [i :expectation :ops (dec (count ops)) 2 :profile-version] 99)
            r (run! corrupted tables)]
        (is (= :transition-evidence-record-mismatch (:reason r)) (pr-str r))
        (is (zero? @observed))
        (is (not (contains? r :executor-envelopes)))))
    (doseq [bad [(assoc-in steps [1 :source-id] "another-source")
                 (assoc-in steps [2 :context :catalog-fingerprint] "another-catalog")
                 (assoc-in steps [1 :declarations] {})
                 (assoc-in steps [1 :command-id] "unrelated")]]
      (let [r (run! bad tables)]
        (is (= :invalid-executor-recovery-source (:reason r)))
        (is (zero? @observed))))
    (let [r (run! steps (disj tables :gesso_assignments))]
      (is (= :incomplete-mapped-recovery-scope (:reason r)))
      (is (not (contains? r :executor-envelopes))))))

(deftest activation-root-bracket-constrains-nested-recovery
  (let [{:keys [steps history sample tables]} (example)
        manifest (select-keys (first history) [:tx-id :ops])
        initializer (second history)
        successor (:tx-id (nth history 2))
        changed (assoc sample :changed true)
        calls (atom 0)
        run! (fn [samples m i successor-id compare!]
               (let [remaining (atom samples)]
                 (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (let [s (first @remaining)] (swap! remaining next) s))}
                   #(provider/compare-with-retained-root! nil m i successor-id compare!))))
        matched! (fn [] (swap! calls inc) {:status :matched :example-positive true})]
    (is (= {:status :matched :example-positive true}
           (run! [sample sample] manifest initializer successor matched!)))
    (doseq [[m i id] [[(assoc manifest :ops []) initializer successor]
                      [manifest (assoc initializer :ops []) successor]
                      [manifest (assoc initializer :system-time nil) successor]
                      [manifest initializer -1]]]
      (is (= :selected-activation-root-mismatch (:reason (run! [sample] m i id matched!)))))
    (is (= 1 @calls))
    (let [r (run! [sample changed] manifest initializer successor matched!)]
      (is (= :source-log-changed-during-recovery-comparison (:reason r)))
      (is (not (contains? r :example-positive))))
    (let [r (run! [sample changed sample] manifest initializer successor
                  #(provider/compare-local-processed-retained-mapped-recovery! nil tables steps recovery/expected))]
      (is (= :source-log-changed-during-recovery-comparison (:reason r)))
      (is (not (contains? r :recovery-linkage))))))

(deftest worker-interleaved-provider-comparison-shares-the-full-history
  (let [{:keys [steps tables]} (example)
        {worker-history :history id :id} (#'recovery/worker-history)
        [w a r] steps
        pending (peek (:ops (nth worker-history 1)))
        w (update-in w [:expectation :ops] conj pending)
        a (update a :expectation #(fixture/tx 3 (:ops %)))
        r (update r :expectation #(fixture/tx 5 (:ops %)))
        steps [w a r]
        history [fixture/initial (:expectation w) (nth worker-history 2)
                 (:expectation a) (nth worker-history 4) (:expectation r)]
        sample {:status :observed :replay-inputs history}
        failure-index (:failed-assertion-index (nth (:history (temporal/replay (mapv #(dissoc % :tx-id) history))) 3))
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 6
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        observed (atom [])
        run! (fn [samples scope source-id adjacent? result]
               (reset! observed [])
               (let [remaining (atom samples)]
                 (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (let [s (first @remaining)] (swap! remaining next) s))
                                  #'provider/compare-source-bound-execution! (fn [_ inputs scope] (swap! observed conj [inputs scope]) result)
                                  #'xt/q (fn [_ query] (if (= 3 (last query))
                                                        [{:committed false :error (ex-info "abort" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx failure-index})}]
                                                        [{:committed true}]))}
                   #(if adjacent?
                      (provider/compare-local-processed-retained-mapped-recovery! nil scope steps recovery/expected)
                      (provider/compare-local-processed-retained-mapped-worker-interleaved-recovery! nil scope steps recovery/expected source-id)))))
        stable (vec (repeat 8 sample))
        matched (run! stable tables "s" false comparison)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= provider/mapped-interleaved-recovery-profile (:mapped-recovery-profile matched)))
    (is (= [1 3 5] (:selected-tx-ids matched)))
    (is (= [:committed :aborted :committed] (:selected-commitments matched)))
    (is (= [:matched-retained-projection :not-committed :matched-retained-projection] (:abstract-transitions matched)))
    (is (= [{:tx-id 2 :maintenance :lease-acquisition :intent-id id}
            {:tx-id 4 :maintenance :dispatch-acknowledgment :intent-id id}]
           (:intervening-maintenance matched)))
    (is (= 6 (:replayed-transaction-count matched)))
    (is (= (vec (repeat 3 [history tables])) @observed))
    (is (= :not-established (:provider-refinement matched)))
    (is (= :none (:authority matched)))
    (is (= :not-issued (:certificate matched)))
    (doseq [[samples scope source-id adjacent? result reason]
            [[stable tables "s" true comparison :nonadjacent-recovery-selection]
             [stable tables nil false comparison :invalid-recovery-linkage-source]
             [stable tables "wrong" false comparison :unqualified-intervening-worker]
             [(vec (repeat 8 (update-in sample [:replay-inputs 2 :ops] #(into [["ASSERT TRUE"]] %))))
              tables "s" false comparison :unqualified-intervening-worker]
             [stable (disj tables :gesso_live_outbox) "s" false comparison :incomplete-mapped-recovery-scope]
             [(assoc stable 3 (assoc sample :changed true)) tables "s" false comparison :source-log-changed-during-recovery-comparison]
             [(assoc stable 7 (assoc sample :changed true)) tables "s" false comparison :source-log-changed-during-recovery-comparison]
             [stable tables "s" false (assoc comparison :transaction-count 4) :invalid-retained-replay-execution-comparison]]]
      (let [r (run! samples scope source-id adjacent? result)]
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:selected-tx-ids :recovery-linkage :intervening-maintenance
                                        :replayed-transaction-count :abstract-transitions]))))))

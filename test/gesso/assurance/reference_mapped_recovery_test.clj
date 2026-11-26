(ns gesso.assurance.reference-mapped-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.assurance.reference-recovery-trajectory-test :as recovery]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-decoder :as decoder]
            [gesso.model.inductive-plan :as induction]
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

(defn- complete-worker-example []
  (let [{:keys [steps tables]} (complete-example)
        declarations {:example/claim {:purpose :example/refresh
                                      :contributions [{:target [:document :gesso_requests "r1"] :topic :example/request :field :xt/id}
                                                      {:target [:document :gesso_assignments "a1"] :topic :example/assignment :field :xt/id}]}
                      :example/refuse nil}
        residuals {:example/claim nil :example/refuse nil}
        complete (mapv (fn [i step]
                         (let [{:keys [original targets docs journal expectation prefix-count spec context source-id]} step
                               original (if (= 2 i) (assoc-in original [:subject :id] :example/refuse) original)
                               spec (-> spec (dissoc :outcome-key)
                                        (assoc :alternative (get-in original [:subject :id])
                                               :outcome-cases {["helper-a" :example/claim {:request-id "r1"} {:status :claimed}] :claimed
                                                               ["helper-b" :example/claim {:request-id "r1"} {:status :claimed}] :claimed
                                                               ["helper-b" :example/claim {:request-id "r1"} {:status :refused}] :refused}))
                               {:keys [guard-ops put-ops]} (publication/derive-publication! original targets docs journal spec source-id declarations)
                               ops (:ops expectation)
                               prefix (subvec ops 1 (inc prefix-count))
                               witness (evidence/derive-record! original targets docs journal prefix spec context)
                               ops (envelope/bind-transition-envelope!
                                    (vec (concat (subvec ops 0 (inc prefix-count)) guard-ops
                                                 (subvec ops (inc prefix-count) (dec (count ops))) put-ops
                                                 [[:put-docs :gesso_transition_evidence witness]])))]
                           (assoc step :original original :spec spec :declarations declarations :residual-declarations residuals
                                  :expectation (fixture/tx (inc (* 2 i)) ops)))) (range 3) steps)
        [w a r] (mapv :expectation complete)
        pending (nth (first (filter #(= [:put-docs :gesso_live_outbox] (vec (take 2 %))) (:ops w))) 2)
        id (:xt/id pending)
        worker-history (:history (#'recovery/worker-history))
        acquisition (nth worker-history 2) acknowledgment (nth worker-history 4)
        lease-fields [:gesso.live.outbox/status :gesso.live.outbox/lease-id :gesso.live.outbox/lease-owner :gesso.live.outbox/lease-until]
        ack-fields (into lease-fields [:gesso.live.outbox/dispatch-receipt-id :gesso.live.outbox/dispatch-adapter])
        acquire (-> acquisition (assoc-in [:ops 0 1] id)
                    (update-in [:ops 3 2] #(merge pending (select-keys % lease-fields))))
        ack (-> acknowledgment (assoc-in [:ops 0 1] id)
                (update-in [:ops 1 2] #(merge pending (select-keys % ack-fields))))]
    {:steps complete :tables tables :history [fixture/initial w acquire a ack r] :id id}))

(deftest complete-worker-interleaved-executor-composition
  (let [{:keys [steps history tables id]} (complete-worker-example)
        sample {:status :observed :replay-inputs history}
        failure-index (:failed-assertion-index (nth (:history (temporal/replay (mapv #(dissoc % :tx-id) history))) 3))
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 6
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        observations (atom 0) compared (atom [])
        run! (fn [s h scope adjacent?]
               (reset! observations 0) (reset! compared [])
               (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (swap! observations inc) (assoc sample :replay-inputs h))
                                #'provider/compare-source-bound-execution! (fn [_ inputs scope] (swap! compared conj [inputs scope]) comparison)
                                #'xt/q (fn [_ query] (if (= 3 (last query))
                                                      [{:committed false :error (ex-info "abort" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx failure-index})}]
                                                      [{:committed true}]))}
                 #(if adjacent?
                    (evidence/compare-local-processed-retained-mapped-executor-recovery! nil scope s)
                    (evidence/compare-local-processed-retained-mapped-executor-worker-interleaved-recovery! nil scope s))))
        matched (run! steps history tables false)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= evidence/executor-interleaved-recovery-profile (:executor-recovery-profile matched)))
    (is (= provider/mapped-interleaved-recovery-profile (:mapped-recovery-profile matched)))
    (is (= [1 3 5] (:selected-tx-ids matched)))
    (is (= [:matched-source-relative-record :matched-uncommitted-record :matched-source-relative-record] (:executor-envelopes matched)))
    (is (= [{:tx-id 2 :maintenance :lease-acquisition :intent-id id}
            {:tx-id 4 :maintenance :dispatch-acknowledgment :intent-id id}] (:intervening-maintenance matched)))
    (is (= 6 (:replayed-transaction-count matched)))
    (is (= (vec (repeat 3 [history tables])) @compared))
    (is (= 8 @observations))
    (is (= :none (:authority matched)))
    (is (= :not-issued (:certificate matched)))
    (is (= :not-established (:provider-refinement matched)))
    (doseq [i (range 3)]
      (let [ops (get-in steps [i :expectation :ops])
            corrupted (assoc-in steps [i :expectation :ops (dec (count ops)) 2 :profile-version] 99)
            r (run! corrupted history tables false)]
        (is (= :transition-evidence-record-mismatch (:reason r)) (pr-str r))
        (is (zero? @observations))
        (is (not (contains? r :executor-envelopes)))))
    (let [ops (get-in steps [0 :expectation :ops])
          index (dec (- (count ops) 1))
          corrupted (assoc-in steps [0 :expectation :ops index 2 :gesso.live.outbox/topic] :example/wrong)
          r (run! corrupted history tables false)]
      (is (= :publication-fragment-mismatch (:reason r)) (pr-str r))
      (is (zero? @observations)))
    (doseq [[s h scope adjacent? reason]
            [[steps history tables true :nonadjacent-recovery-selection]
             [(assoc-in steps [1 :source-id] "different") history tables false :invalid-executor-recovery-source]
             [steps (assoc-in history [2 :ops 3 2 :gesso.live.outbox/source-id] "different") tables false :unqualified-intervening-worker]
             [steps history (disj tables :gesso_live_outbox) false :incomplete-mapped-recovery-scope]]]
      (let [r (run! s h scope adjacent?)]
        (is (= reason (:reason r)) (pr-str r))
        (is (not-any? #(contains? r %) [:executor-envelopes :intervening-maintenance :recovery-linkage :selected-tx-ids]))))))

(defn- issued-worker-example []
  (let [{:keys [steps history tables id]} (complete-worker-example)
        targets (assoc fixture/targets :example/loser-outcome [:journal-slot "helper-b" :example/claim])
        alternatives [:example/claim :example/compete :example/refuse]
        declarations (assoc (:declarations (first steps)) :example/compete (get-in steps [0 :declarations :example/claim]))
        residuals (zipmap alternatives (repeat nil))
        selectors {:example/claim {:example/actor [:principal]}
                   :example/compete {:example/actor [:principal]} :example/refuse {}}
        concrete (into {} (map (fn [id {:keys [spec]}]
                                 [id {:arguments (get-in spec [:binding :arguments]) :outcome (:outcome spec)}]) alternatives steps))
        steps (mapv (fn [i {:keys [original spec docs expectation context source-id] :as step}]
                      (let [id (nth alternatives i)
                            original (-> original (assoc-in [:subject :id] id)
                                         (assoc-in [:state-types :example/loser-outcome] {:type :keyword :nullable? true})
                                         (update :frame conj :example/loser-outcome))
                            original (if (zero? i) original
                                         (-> original (update :writes disj :example/outcome) (update :writes conj :example/loser-outcome)
                                             (update :frame disj :example/loser-outcome) (update :frame conj :example/outcome)
                                             (update :replacements #(-> % (assoc :example/loser-outcome (:example/outcome %)) (dissoc :example/outcome)))))
                            original (if (= i 2) original
                                         (-> original (assoc :input-types {:example/actor {:type :string}})
                                             (assoc :reads #{:example/actor :example/owner})
                                             (assoc-in [:replacements :example/count] [:literal 3])
                                             (assoc-in [:replacements :example/owner] [:read :example/actor])))
                            journal (if (= i 2) [recovery/winner] [])
                            spec (assoc spec :alternative id :inputs (if (= i 2) {} {:example/actor (if (zero? i) "helper-a" "helper-b")}))
                            context (assoc context :selectors (get selectors id))
                            prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)
                            {:keys [guard-ops put-ops]} (publication/derive-publication! original targets docs journal spec source-id declarations)
                            witness (evidence/derive-record! original targets docs journal prefix spec context)
                            ops (envelope/bind-transition-envelope!
                                 (vec (concat [(first (:ops expectation))] prefix guard-ops
                                              (writes/derive-puts! original targets docs journal spec) put-ops
                                              [[:put-docs :gesso_transition_evidence witness]])))]
                        (assoc step :original original :targets targets :journal journal :spec spec :context context
                               :declarations declarations :residual-declarations residuals :prefix-count (count prefix)
                               :expectation (assoc expectation :ops ops)))) (range 3) steps)
        source {:inductive-source {:catalog {:operations (mapv (fn [id step] {:id id :construction (:original step)}) alternatives steps)}}
                :target-map targets :input-derivations selectors}
        source (assoc source :decode-outcome! (decoder/build-closed! source concrete))
        description (merge (select-keys (:context (first steps)) [:catalog-fingerprint :induction-fingerprint])
                           {:operation-mappings (zipmap alternatives (repeat {})) :target-map targets :input-derivations selectors
                            :document-effects (zipmap alternatives (map #(get-in % [:spec :effects]) steps))
                            :publication-derivations declarations :residual-assertion-derivations residuals})
        history (reduce (fn [h step] (assoc h (get-in step [:expectation :tx-id]) (:expectation step))) history steps)]
    {:steps steps :history history :tables tables :id id :source source :concrete concrete :description description}))

(deftest issued-worker-interleaved-recovery-composes-all-source-checks
  (let [{:keys [steps history tables id source concrete description]} (issued-worker-example)
        admission (Object.)
        sample {:status :observed :replay-inputs history}
        failure-index (:failed-assertion-index (nth (:history (temporal/replay (mapv #(dissoc % :tx-id) history))) 3))
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 6
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        acquired (atom 0) compared (atom [])
        run! (fn [s src declarations desc adjacent?]
               (reset! acquired 0) (reset! compared [])
               (with-redefs-fn {#'induction/describe-mapped-admission (constantly desc)
                                #'induction/require-current-mapped-admission! (fn [a s]
                                                                              (if (and (identical? a admission) (identical? (:decode-outcome! s) (:decode-outcome! source)))
                                                                                desc (throw (ex-info "Wrong registration" {:reason :changed-mapped-admission-registration}))))
                                #'provider/observe-replay-history! (fn [_] (swap! acquired inc) sample)
                                #'provider/compare-source-bound-execution! (fn [_ inputs scope] (swap! compared conj [inputs scope]) comparison)
                                #'xt/q (fn [_ query] (if (= 3 (last query))
                                                      [{:committed false :error (ex-info "abort" {:xtdb.error/code :xtdb/assert-failed :tx-op-idx failure-index})}]
                                                      [{:committed true}]))}
                 #(if adjacent?
                    (evidence/compare-local-processed-retained-issued-executor-recovery! nil tables admission src declarations s)
                    (evidence/compare-local-processed-retained-issued-executor-worker-interleaved-recovery! nil tables admission src declarations s))))
        r (run! steps source concrete description false)]
    (is (= :matched (:status r)) (pr-str r))
    (is (= evidence/issued-executor-interleaved-recovery-profile (:issued-executor-recovery-profile r)))
    (is (= evidence/executor-outcome-interleaved-recovery-profile (:executor-outcome-recovery-profile r)))
    (is (= evidence/executor-input-interleaved-recovery-profile (:executor-input-recovery-profile r)))
    (is (= evidence/executor-interleaved-recovery-profile (:executor-recovery-profile r)))
    (is (= :matched-issued-closed-construction (:closed-decoder-binding r)))
    (is (= :matched-issued-declarations (:issued-admission-binding r)))
    (is (= (vec (repeat 3 :matched-source-relative-selectors)) (:input-derivations r)))
    (is (= (vec (repeat 3 :matched-declared-journal-assignment)) (:outcome-derivations r)))
    (is (= [1 3 5] (:selected-tx-ids r)))
    (is (= [2 4] (mapv :tx-id (:intervening-maintenance r))))
    (is (= 6 (:replayed-transaction-count r)))
    (is (= (vec (repeat 3 [history tables])) @compared))
    (is (= 8 @acquired))
    (is (= :not-established (:source-provenance r) (:outcome-provenance r) (:provider-refinement r)))
    (is (= :none (:authority r)))
    (is (= :not-issued (:certificate r)))
    (doseq [[s src d desc reason]
            [[(assoc-in steps [1 :spec :inputs :example/actor] "helper-a") source concrete description :selected-input-valuation-mismatch]
             [(update-in steps [2 :spec :outcome-cases] dissoc (first (keys (get-in steps [2 :spec :outcome-cases])))) source concrete description :selected-outcome-cases-mismatch]
             [(assoc-in steps [0 :context :catalog-fingerprint] "wrong") source concrete description :issued-executor-recovery-declaration-mismatch]
             [steps (assoc source :decode-outcome! (fn [_] :claimed)) concrete description :unregistered-closed-decoder]
             [steps source (dissoc concrete :example/refuse) description :changed-closed-decoder-construction]]]
      (let [bad (run! s src d desc false)]
        (is (= reason (:reason bad)) (pr-str bad))
        (is (zero? @acquired))
        (is (not-any? #(contains? bad %) [:issued-admission-binding :closed-decoder-binding :input-derivations :outcome-derivations :executor-envelopes :intervening-maintenance]))))
    (let [bad (run! steps source concrete description true)]
      (is (= :nonadjacent-recovery-selection (:reason bad)))
      (is (not (contains? bad :issued-admission-binding))))))

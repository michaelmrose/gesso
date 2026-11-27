(ns gesso.assurance.reference-recovery-invariant-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-recovery-expression :as expression]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.rule-eval :as production]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]
            [xtdb.db-catalog :as db])
  (:import [java.time Instant Duration]))

(def ^:private types {:fixture/value {:type :integer}})
(def ^:private coordinates {:fixture/value [:document :fixture_rows "row" :value]})
(defn- construction [id before after]
  {:subject {:kind :operation :id id :interface {:operation id}}
   :state-types types :input-types {}
   :guard {:readers #{:fixture/value} :expression [:= [:read :fixture/value] [:literal before]]}
   :reads #{:fixture/value} :writes #{:fixture/value} :frame #{}
   :replacements {:fixture/value [:literal after]}})
(def ^:private accept (construction :fixture/accept 0 1))
(def ^:private refuse (construction :fixture/refuse 1 2))
(def ^:private catalog
  {:id :fixture/catalog
   :initial-condition {:readers #{:fixture/value} :expression [:= [:read :fixture/value] [:literal 0]]}
   :invariants {:fixture/nonnegative {:readers #{:fixture/value}
                                      :expression [:>= [:read :fixture/value] [:literal 0]]}}
   :operations [{:id :fixture/accept :construction accept}
                {:id :fixture/refuse :construction refuse}]})
(defn- t [i] (.plusSeconds (Instant/parse "2026-11-26T20:00:00Z") i))
(defn- put-value [v] [:put-docs :fixture_rows {:xt/id "row" :value v}])
(def ^:private inputs
  (mapv (fn [i ops] {:tx-id i :system-time (t i) :ops ops}) (range)
        [[[:put-docs :manifest {:xt/id "root" :value 1}]]
         [(put-value 0)] [(put-value 1)] [[:put-docs :maintenance {:xt/id "worker" :value 1}]]
         [(put-value 9) ["ASSERT FALSE"]] [(put-value 2)] [(put-value -1)]]))
(def ^:private comparison
  {:manifest-tx-id 0 :initialization-tx-id 1 :selected-tx-ids [2 4 5]
   :selected-commitments [:committed :aborted :committed]
   :intervening-maintenance [{:tx-id 3}]})
(def ^:private steps
  (mapv (fn [id op] {:expectation (nth inputs id) :original op
                     :spec {:alternative (get-in op [:subject :id]) :inputs {}}})
        [2 4 5] [accept accept refuse]))

(deftest finite-selected-invariant-boundaries-are-actually-checked
  (let [audit #'invariant/audit-replay!
        run #(audit %1 coordinates {} {:fixture/value 0} %2 comparison %3)
        r (run catalog inputs steps)]
    (is (= :checked (:status r)) (pr-str r))
    (is (= :declared-invariants-hold-on-selected-replay-projections (:conclusion r)))
    (is (= [1 2 3 4 5] (mapv :tx-id (:checked-boundaries r))))
    (is (= [:checked-initial-projection :committed-abstract-transition :maintenance-stutter
            :aborted-stutter :committed-abstract-transition] (mapv :relation (:checked-boundaries r))))
    (is (= #{:fixture/nonnegative} (:invariant-ids r)))
    (let [evidence (:expression-correspondence r)]
      (is (= invariant/expression-profile (:profile evidence)))
      (is (= :matched (:status evidence)))
      (is (= #{:fixture/nonnegative} (:invariant-ids evidence)))
      (is (= [[1 :initial] [2 :prestate] [2 :poststate] [3 :prestate] [3 :poststate]
              [4 :prestate] [4 :poststate] [5 :prestate] [5 :poststate]]
             (mapv (juxt :tx-id :side) (:checked-points evidence))))
      (is (= :not-established (:universal-evaluator-correspondence evidence)))
      (is (= :not-performed (:proof-discharge evidence))))
    (is (= :not-established (:physical-invariant-theorem r) (:universal-refinement r)))
    (is (= r (run catalog (pop inputs) steps))) ;; suffix deliberately excluded
    (let [bad-refuse (construction :fixture/refuse 1 -1)
          bad (run (assoc-in catalog [:operations 1 :construction] bad-refuse)
                   (assoc-in inputs [5 :ops] [(put-value -1)])
                   (assoc-in steps [2 :original] bad-refuse))]
      (is (= :selected-projection-invariant-violation (:reason bad)))
      (is (not (contains? bad :conclusion))))
    (doseq [[changed reason] [[(assoc-in inputs [1 :ops] [(put-value 1)]) :initial-projection-invariant-mismatch]
                              [(assoc-in inputs [3 :ops] [(put-value 2)]) :selected-invariant-transition-mismatch]
                              [(assoc-in inputs [4 :ops] [(put-value 1)]) :selected-invariant-transition-mismatch]
                              [(assoc-in inputs [5 :ops] [(put-value 3)]) :selected-invariant-transition-mismatch]
                              [(vec (remove #(= 3 (:tx-id %)) inputs)) :invalid-selected-invariant-sequence]
                              [(assoc-in inputs [2 :ops]
                                         [(put-value 1)
                                          [:put-docs {:into :fixture_rows :valid-from (t 3)} {:xt/id "row" :value 3}]])
                               :selected-projection-continuity-mismatch]]]
      (let [bad (run catalog changed steps)]
        (is (= reason (:reason bad)) (pr-str bad))
        (is (not (contains? bad :conclusion)))))
    (with-redefs [poststate/project-visible! (constantly {:fixture/value 0})]
      (is (not (invariant/boundaries-current?)))
      (is (= :invariant-boundary-replaced
             (:reason (invariant/audit-issued-selection! nil nil nil nil nil nil nil nil)))))))

(deftest original-expression-evaluators-agree-on-selected-values
  (let [evaluate #'invariant/corresponding-value!
        rule (get-in catalog [:invariants :fixture/nonnegative])
        original production/evaluate-rule!
        candidate (fn [alter]
                    (with-redefs-fn {#'invariant/original-production-evaluate
                                     (fn [& args] (alter (apply original args)))}
                      #(try (evaluate types {:fixture/value 0} rule)
                            (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))]
    (is (true? (evaluate types {:fixture/value 0} rule)))
    (is (false? (evaluate types {:fixture/value -1} rule)))
    (doseq [alter [#(assoc % :value false)
                   #(assoc % :value :truthy)
                   #(assoc % :expression [:literal true])
                   #(assoc % :readers #{})
                   #(assoc % :proof-status :proved)]]
      (is (= :invariant-expression-correspondence-mismatch (candidate alter))))
    (let [optional {:fixture/value {:type :integer :optional? true :nullable? true}}
          guarded {:readers #{:fixture/value}
                   :expression [:and [:present [:read :fixture/value]]
                                [:not= [:read :fixture/value] [:literal nil]]
                                [:>= [:read :fixture/value] [:literal 0]]]}]
      (doseq [[state expected] [[{} false] [{:fixture/value nil} false]
                               [{:fixture/value -1} false] [{:fixture/value 0} true]]]
        (is (= expected (evaluate optional state guarded)))))
    (with-redefs [production/evaluate-rule! (constantly {:value true})]
      (is (not (invariant/boundaries-current?)))
      (is (= :invariant-boundary-replaced
             (:reason (invariant/audit-issued-selection! nil nil nil nil nil nil nil nil)))))))

(deftest independently-read-selected-physical-coordinates
  (let [checked (#'invariant/audit-replay! catalog coordinates {} {:fixture/value 0}
                                             inputs comparison steps)]
    (with-open [node (xtn/start-node {})]
      (doseq [tx inputs]
        (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
          (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 15))))
      (let [run #( #'invariant/audit-physical! node coordinates {} % checked)
            matched (run inputs)
            points (:checked-coordinates matched)]
        (is (= :matched (:status matched)) (pr-str matched))
        (is (= [[1 :initial] [2 :prestate] [2 :poststate] [3 :prestate] [3 :poststate]
                [4 :prestate] [4 :poststate] [5 :prestate] [5 :poststate]]
               (mapv (juxt :tx-id :side) points)))
        (let [evidence (:projection-correspondence matched)]
          (is (= projection/profile (:profile evidence)))
          (is (= :matched (:status evidence)))
          (is (= points (:checked-coordinates evidence)))
          (is (= coordinates (:field-coordinates evidence))))
        (with-redefs-fn {#'invariant/original-independent-project (constantly {:fixture/value 999})}
          #(is (= :independent-retained-projection-mismatch (:reason (run inputs)))))
        (let [snapshot (:snapshot-correspondence matched)]
          (is (= invariant/snapshot-profile (:profile snapshot)))
          (is (= :matched (:status snapshot)))
          (is (= (t 6) (:snapshot-system-time snapshot)))
          (is (= :matched-before-and-after (:token-echo snapshot)))
          (is (= :matched-prior-poststate-to-next-prestate (:valid-time-continuity snapshot)))
          (is (= points (:checked-coordinates snapshot)))
          (is (= [0 1 2 3 4 5] (mapv :tx-id (:ledger snapshot))))
          (is (= [true true true true false true] (mapv :committed? (:ledger snapshot))))
          (is (= :not-established (:provider-refinement snapshot))))
        (let [query @#'invariant/original-query]
          (doseq [[alter reason]
                  [[(fn [sql rows] (if (and (vector? sql) (= 4 (second sql))
                                           (= "SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?" (first sql)))
                                    (mapv #(assoc % :committed true) rows) rows)) :selected-pinned-ledger-mismatch]
                   [(fn [sql rows] (if (= sql "SHOW SNAPSHOT_TOKEN") [{:snapshot-token "foreign"}] rows))
                    :selected-pinned-token-mismatch]]]
            (with-redefs-fn {#'invariant/original-query
                             (fn ([n sql] (query n sql))
                               ([n sql opts] (alter sql (query n sql opts))))}
              #(let [bad (run inputs)]
                 (is (= reason (:reason bad)))
                 (is (not (contains? bad :snapshot-correspondence)))))))
        (is (= (t 1) (:system-time (second points))))
        (is (= (t 2) (:valid-time (second points))))
        (is (= matched (run (pop inputs))))
        (let [derive #( #'invariant/derive-physical-invariants %1 %2 inputs)
              conclusion (derive checked matched)]
          (is (= :conditional (:status conclusion)))
          (is (= :declared-invariants-hold-on-selected-physical-projections (:conclusion conclusion)))
          (is (= points (:checked-coordinates conclusion)))
          (is (= (:invariant-ids checked) (:invariant-ids conclusion)))
          (is (= :not-performed (:proof-discharge conclusion) (:premise-acceptance conclusion)))
          (is (= :not-established (:physical-invariant-theorem conclusion) (:universal-refinement conclusion)))
          (is (= #{:faithful-pinned-provider-readbacks :faithful-authoritative-state-projection
                   :faithful-invariant-expression-semantics} (:conditional-on conclusion)))
          (doseq [[replay physical] [[(dissoc checked :conclusion) matched]
                                     [(assoc checked :projection-continuity :unknown) matched]
                                     [(assoc checked :status :mismatch) matched]
                                     [checked nil]
                                     [checked (assoc matched :status :mismatch)]
                                     [checked (assoc matched :status :unavailable)]
                                     [checked (assoc matched :profile :other)]
                                     [checked (dissoc matched :snapshot-correspondence)]
                                     [checked (assoc-in matched [:snapshot-correspondence :snapshot-system-time] (t 4))]
                                     [checked (assoc-in matched [:snapshot-correspondence :token-echo] :unknown)]
                                     [checked (assoc-in matched [:snapshot-correspondence :ledger 4 :committed?] true)]
                                     [checked (update-in matched [:snapshot-correspondence :ledger] pop)]
                                     [checked (update-in matched [:snapshot-correspondence :checked-coordinates] pop)]
                                     [checked (dissoc matched :projection-correspondence)]
                                     [checked (assoc-in matched [:projection-correspondence :status] :mismatch)]
                                     [checked (update-in matched [:projection-correspondence :checked-coordinates] pop)]
                                     [checked (assoc matched :invariant-ids #{:fixture/other})]
                                     [checked (update matched :checked-coordinates pop)]
                                     [checked (update matched :checked-coordinates #(vec (reverse %)))]
                                     [checked (assoc-in matched [:checked-coordinates 1 :valid-time] (t 1))]
                                     [checked (assoc-in matched [:checked-coordinates 1 :system-time] (t 2))]]]
            (let [failure (derive replay physical)]
              (is (= :not-established (:status failure)))
              (is (not (contains? failure :conclusion)))
              (is (not (contains? failure :checked-coordinates))))))
        (let [mismatch (run (assoc-in inputs [5 :ops] [(put-value 3)]))]
          (is (= :mismatch (:status mismatch)) (pr-str mismatch))
          (is (not (contains? mismatch :checked-coordinates)))
          (is (= :not-established
                 (:status (#'invariant/derive-physical-invariants checked mismatch inputs))))))))
  (with-redefs [xt/q (constantly [])]
    (is (not (invariant/boundaries-current?)))
    (is (= :invariant-boundary-replaced
           (:reason (invariant/audit-issued-selection! nil nil nil nil nil nil nil nil))))))

(deftest pinned-snapshot-excludes-later-transaction
  (with-open [node (xtn/start-node {})]
    (let [submit! (fn [tx]
                    (let [id (:tx-id (xt/submit-tx node (:ops tx) {:system-time (:system-time tx)}))]
                      (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 15))))
          selected (subvec inputs 0 6)]
      (doseq [tx (pop selected)] (submit! tx))
      (let [older (#'invariant/snapshot-token! (xt/q node "SHOW SNAPSHOT_TOKEN"))]
        (submit! (peek selected))
        (let [token (#'invariant/snapshot-token! (xt/q node "SHOW SNAPSHOT_TOKEN"))
              basis {:snapshot-token token :current-time (Instant/now)}]
          (submit! (peek inputs))
          (is (not= token (#'invariant/snapshot-token! (xt/q node "SHOW SNAPSHOT_TOKEN"))))
          (is (nil? (#'invariant/confirm-basis! node basis)))
          (is (= (t 5) (#'invariant/snapshot-point! token selected)))
          (is (= :selected-snapshot-does-not-cover-cut
                 (try (#'invariant/snapshot-point! older selected)
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
          (is (empty? (xt/q node ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?" 6] basis)))
          (is (= 2 (get-in (#'invariant/read-physical-visible! node basis #{:fixture_rows} (t 5) (t 6))
                           [:fixture_rows "row" :value])))
          (is (= -1 (get-in (#'invariant/read-physical-visible! node {:snapshot-token (#'invariant/snapshot-token! (xt/q node "SHOW SNAPSHOT_TOKEN"))
                                                                     :current-time (Instant/now)} #{:fixture_rows} (t 6) (t 6))
                            [:fixture_rows "row" :value]))))))))

(defn check-judgment! [result scope original]
  (let [checked (:selected-invariant-evidence result)
        ids (:selected-tx-ids original)
        initial (:initialization-tx-id original)
        maintenance (mapv :tx-id (:intervening-maintenance original))
        boundaries (:checked-boundaries checked)]
    (let [physical (:physical-readbacks checked)
          points (:checked-coordinates physical)]
      (is (= invariant/physical-profile (:profile physical)))
      (is (= :matched (:status physical)) (pr-str physical))
      (is (= :matched-selected-physical-projections (:correspondence physical)))
      (is (= (:invariant-ids checked) (:invariant-ids physical)))
      (is (= (inc (* 2 (dec (count boundaries)))) (count points)))
      (is (= (mapv :tx-id boundaries) (mapv :tx-id (remove #(= :prestate (:side %)) points))))
      (is (= :not-established (:physical-invariant-theorem physical) (:universal-refinement physical)))
      (is (= :not-performed (:proof-discharge physical)))
      (let [snapshot (:snapshot-correspondence physical)]
        (is (= invariant/snapshot-profile (:profile snapshot)))
        (is (= :matched (:status snapshot)))
        (is (= :matched-before-and-after (:token-echo snapshot)))
        (is (= :matched-prior-poststate-to-next-prestate (:valid-time-continuity snapshot)))
        (is (= points (:checked-coordinates snapshot)))
        (is (= (into [(:manifest-tx-id original)] (map :tx-id boundaries)) (mapv :tx-id (:ledger snapshot))))
        (is (= (into [true] (map #(not= :aborted-stutter (:relation %)) boundaries)) (mapv :committed? (:ledger snapshot))))
        (is (= :not-established (:provider-refinement snapshot))))
      (let [evidence (:projection-correspondence physical)]
        (is (= projection/profile (:profile evidence)))
        (is (= :matched (:status evidence)))
        (is (= points (:checked-coordinates evidence)))
        (is (= 7 (count (:field-coordinates evidence))))
        (is (= 4 (:closed-outcome-case-count evidence)))
        (is (= :not-established (:provider-refinement evidence) (:universal-projection evidence)))
        (is (= :not-performed (:proof-discharge evidence)))))
    (let [conclusion (:conditional-physical-invariant checked)]
      (is (= invariant/conditional-physical-profile (:profile conclusion)))
      (is (= :conditional (:status conclusion)) (pr-str conclusion))
      (is (= :declared-invariants-hold-on-selected-physical-projections (:conclusion conclusion)))
      (is (= (:invariant-ids checked) (:invariant-ids conclusion)))
      (is (= (get-in checked [:physical-readbacks :checked-coordinates]) (:checked-coordinates conclusion)))
      (is (= :not-established (:physical-invariant-theorem conclusion) (:universal-refinement conclusion)))
      (is (= :not-performed (:proof-discharge conclusion) (:premise-acceptance conclusion)))
      (is (= :none (:authority conclusion)))
      (is (= :not-issued (:certificate conclusion))))
    (let [evidence (:expression-correspondence checked)]
      (is (= invariant/expression-profile (:profile evidence)))
      (is (= :matched (:status evidence)))
      (is (= (:invariant-ids checked) (:invariant-ids evidence)))
      (is (= :matched-true-at-initial-projection (:initial-condition evidence)))
      (is (= (mapv #(select-keys % [:tx-id :side]) (get-in checked [:physical-readbacks :checked-coordinates]))
             (:checked-points evidence)))
      (is (= :not-established (:universal-evaluator-correspondence evidence)))
      (is (= :not-performed (:proof-discharge evidence))))
    (let [catalog (:catalog-expression-refinement checked)
          support (:invariant-expression-refinement result)
          fp (:premise-fingerprint support)]
      (is (= expression/profile (:profile catalog)))
      (is (= :conditional-catalog-refinement (:status catalog)) (pr-str catalog))
      (is (= 819 (:cases-checked catalog)))
      (is (= expression/binding-profile (:profile support)))
      (is (= :conditional-premise-support (:status support)) (pr-str support))
      (is (= (:catalog-fingerprint checked) (:catalog-fingerprint support)))
      (is (= (:recovery-cut-ref checked) (:recovery-cut-ref support)))
      (is (= (:invariant-ids checked) (:invariant-ids support)))
      (is (= (:conditions catalog) (:remaining-conditions support)))
      (is (= fp (get-in result [:physical-invariant-transfer :condition-premise-fingerprints
                                :faithful-invariant-expression-semantics])))
      (is (contains? (get-in result [:obligation-inventory :unaccepted-premise-fingerprints]) fp))
      (is (= :open (:premise-state support)))
      (is (= :not-performed (:proof-discharge support) (:premise-acceptance support))))
    (let [mapping (:catalog-projection-correspondence checked)
          support (:authoritative-projection-refinement result)
          transfer (:physical-invariant-transfer result)]
      (is (= projection/catalog-profile (:profile mapping)))
      (is (= :matched (:status mapping)) (pr-str mapping))
      (is (= 8 (count (:operation-mappings mapping))))
      (is (= 4 (count (:closed-outcome-cases mapping))))
      (is (= projection/binding-profile (:profile support)))
      (is (= :conditional-premise-support (:status support)) (pr-str support))
      (is (= (:recovery-cut-ref checked) (:recovery-cut-ref support)))
      (is (= (:catalog-fingerprint checked) (:catalog-fingerprint support)))
      (is (= (get-in transfer [:condition-premise-fingerprints :faithful-authoritative-state-projection])
             (:premise-fingerprint support)))
      (is (= (get-in transfer [:condition-premise-fingerprints :faithful-pinned-provider-readbacks])
             (:snapshot-premise-fingerprint support)))
      (is (= (:remaining-conditions mapping) (:remaining-conditions support)))
      (is (= :open (:premise-state support)))
      (is (= :not-performed (:proof-discharge support) (:premise-acceptance support))))
    (is (= invariant/profile (:profile checked)))
    (is (= :checked (:status checked)) (pr-str checked))
    (is (= :declared-invariants-hold-on-selected-replay-projections (:conclusion checked)))
    (is (= :original-selected-transaction-boundaries-only (:scope checked)))
    (is (= :finite-direct-evaluation-not-inductive-theorem (:derivation checked)))
    (is (= :transaction-system-and-valid-time (:poststate-coordinate checked)))
    (is (= :preceding-system-current-transaction-valid-time (:prestate-coordinate checked)))
    (is (= (:declared-invariant-ids (:abstract-induction result)) (:invariant-ids checked)))
    (is (= (:judgment-subject result) (:judgment-subject checked)))
    (is (= (:catalog-fingerprint (:judgment-subject result)) (:catalog-fingerprint checked)))
    (is (= (:historical-cut scope) (:historical-cut checked)))
    (is (= (get-in scope [:historical-cut :payload-fingerprint]) (:retained-payload-fingerprint checked)))
    (is (= (get-in result [:command-authentication-premises :scope :command-observation-ref]) (:recovery-cut-ref checked)))
    (is (= ids (:selected-tx-ids checked)))
    (is (= (into [initial] (sort (into ids maintenance))) (mapv :tx-id boundaries)))
    (is (= :checked-initial-projection (:relation (first boundaries))))
    (is (= [(second ids)] (mapv :tx-id (filter #(= :aborted-stutter (:relation %)) boundaries))))
    (is (= maintenance (mapv :tx-id (filter #(= :maintenance-stutter (:relation %)) boundaries))))
    (is (= [(first ids) (peek ids)] (mapv :tx-id (filter #(= :committed-abstract-transition (:relation %)) boundaries))))
    (is (= :not-established (:physical-invariant-theorem checked) (:universal-refinement checked)))
    (is (= :not-performed (:proof-discharge checked)))
    (is (= :none (:authority checked)))
    (is (= :not-issued (:certificate checked)))))

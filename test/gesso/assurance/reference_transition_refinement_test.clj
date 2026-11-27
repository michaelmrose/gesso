(ns gesso.assurance.reference-transition-refinement-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- cases []
  (:cases (outcome/derive-cases! {:inductive-source {:catalog (claim/catalog)}
                                :target-map claim/target-map}
                               (claim/outcome-declarations))))

(defn- seed []
  {:requests {"r1" {:xt/id "r1" :status :open :revision 0 :extra "preserve"}}
   :request_assignments {}
   :memberships {"helper-1" {:xt/id "helper-1" :active true}
                 "helper-2" {:xt/id "helper-2" :active true}}
   :gesso_commands {}})

(defn- prepare [image helper branch cases]
  (let [original (claim/source helper branch)
        docs (into {} (map (fn [[k rows]] [k (vec (vals rows))])) (dissoc image :gesso_commands))
        journal (vec (vals (:gesso_commands image)))
        declaration (get (claim/outcome-declarations) (claim/alternative-id helper branch))
        spec {:inputs {} :effects (if (= :accept branch)
                                     {[:document :requests "r1"] {} [:document :request_assignments "primary-r1"] {}}
                                     {})
              :binding {:command-id (identity/command-id (str "command-" helper))
                        :principal (identity/principal helper) :operation :request/claim
                        :arguments (:arguments declaration)}
              :outcome (:outcome declaration) :now "fixture-time"
              :alternative (claim/alternative-id helper branch) :outcome-cases cases}
        suffix (writes/derive-puts! original claim/target-map docs journal spec)]
    {:original original :docs docs :journal journal :spec spec :suffix suffix}))

(defn- apply-puts [image puts]
  (reduce (fn [m [_ table & rows]]
            (reduce #(assoc-in %1 [table (:xt/id %2)] %2) m rows)) image puts))

(defn- check [{:keys [original docs journal spec suffix]} commitment before after]
  (refinement/check-step! original claim/target-map docs journal spec suffix commitment before after))

(deftest both-claim-orders-refine-commit-abort-and-recovery
  (let [cases (cases)]
    (doseq [[winner loser] [["helper-1" "helper-2"] ["helper-2" "helper-1"]]]
      (let [initial (seed)
            win (prepare initial winner :accept cases)
            stale (prepare initial loser :accept cases)
            won (apply-puts initial (:suffix win))
            recovery (prepare won loser :refuse cases)
            recovered (apply-puts won (:suffix recovery))
            results [(check win :committed initial won)
                     (check stale :aborted won won)
                     (check recovery :committed won recovered)]]
        (doseq [r results]
          (is (= :conditional-step-correspondence (:status r)) (pr-str r))
          (is (= :not-established (:provider-refinement r) (:writer-coverage r) (:universal-refinement r)))
          (is (= :not-performed (:proof-discharge r))))
        (is (= [:committed-abstract-transition :aborted-stutter :committed-abstract-transition] (mapv :relation results)))
        (is (= (get-in (first results) [:after :assignment/helper]) winner))
        (is (= (:before (second results)) (:after (second results))))
        (is (= (:after (first results)) (:before (last results))))
        (is (= winner (get-in (last results) [:after :assignment/helper])))
        (is (= 2 (count (:gesso_commands recovered))))
        ;; A successful abstract guard is not a proof that the provider committed.
        (is (= :aborted-stutter (:relation (check win :aborted initial initial))))))))

(deftest reject-incomplete-atomic-effects-and-stale-commits
  (let [cases (cases) initial (seed)
        win (prepare initial "helper-1" :accept cases)
        stale (prepare initial "helper-2" :accept cases)
        won (apply-puts initial (:suffix win))
        run #(check win :committed initial %)]
    (doseq [bad [initial (assoc-in won [:requests "r1" :extra] "wrong")
                 (assoc-in won [:memberships "helper-2" :active] false)
                 (assoc won :gesso_commands {})
                 (assoc won :request_assignments {})]]
      (is (= :committed-row-effect-mismatch (:reason (run bad)))))
    (is (= :committed-prestate-guard-mismatch (:reason (check stale :committed won won))))
    (is (= :aborted-row-effect-mismatch (:reason (check stale :aborted won (assoc-in won [:requests "r1" :extra] "partial")))))
    (doseq [suffix [(pop (:suffix win))
                    (conj (:suffix win) (first (:suffix win)))
                    (conj (:suffix win) [:put-docs :memberships {:xt/id "helper-2" :active false}])]]
      (let [bad (check (assoc win :suffix suffix) :committed initial won)]
        (is (= :not-established (:status bad)))
        (is (not (contains? bad :relation)))))
    (with-redefs [writes/derive-transition! (constantly {})]
      (is (= :unqualified-transition-boundary (:reason (run won)))))
    (is (= :conditional-step-correspondence (:status (run won))))))

(defn- selection-fixture []
  (let [cases (cases) initial (seed)
        winner (prepare initial "helper-1" :accept cases)
        loser (prepare initial "helper-2" :accept cases)
        won (apply-puts initial (:suffix winner))
        recovery (prepare won "helper-2" :refuse cases)
        recovered (apply-puts won (:suffix recovery))
        tx (fn [id ops] {:tx-id id :system-time (.plusSeconds java.time.Instant/EPOCH id) :ops ops})
        envelope (fn [id step] (tx id (into [["ASSERT TRUE"]] (:suffix step))))
        inputs [(tx 0 []) (tx 1 []) (envelope 2 winner)
                (tx 3 [[:put-docs :gesso_live_outbox {:xt/id "worker" :status :ready}]])
                (envelope 4 loser) (envelope 5 recovery)]
        steps (mapv (fn [step id] (assoc step :targets claim/target-map :expectation (inputs id)))
                    [winner loser recovery] [2 4 5])
        ledger (mapv #(assoc (dissoc % :ops) :committed? (not= 4 (:tx-id %))) inputs)
        points (#'refinement/selected-points ledger)
        images (zipmap points (map #(hash-map :retained % :physical %) [initial initial won won won won won won recovered]))
        comparison {:status :matched :manifest-tx-id 0 :initialization-tx-id 1
                    :selected-tx-ids [2 4 5] :selected-commitments [:committed :aborted :committed]
                    :issued-admission-binding :matched-issued-declarations
                    :closed-decoder-binding :matched-issued-closed-construction
                    :activation-root-binding :matched-local-activated-retained-root
                    :submission-recovery-linkage :matched-local-observed-retained-steps
                    :executor-envelopes [:matched-source-relative-record :matched-uncommitted-record :matched-source-relative-record]
                    :intervening-maintenance [{:tx-id 3}]}
        checked {:status :checked :selected-tx-ids [2 4 5]
                 :checked-boundaries [{:tx-id 1 :relation :checked-initial-projection}
                                      {:tx-id 2 :relation :committed-abstract-transition :alternative-id (get-in winner [:spec :alternative])}
                                      {:tx-id 3 :relation :maintenance-stutter}
                                      {:tx-id 4 :relation :aborted-stutter}
                                      {:tx-id 5 :relation :committed-abstract-transition :alternative-id (get-in recovery [:spec :alternative])}]}]
    {:catalog (claim/catalog) :coordinates claim/target-map :cases cases :inputs inputs
     :checked checked :comparison comparison :steps steps :ledger ledger :points points :images images}))

(deftest selected-envelope-and-full-row-refinement
  (let [f (selection-fixture) r (refinement/check-selected! f) points (:points f)
        maintenance-post (points 4) abort-post (points 6)]
    (is (= :conditional-selected-transition-correspondence (:status r)) (pr-str r))
    (is (= [2 3 4 5] (mapv :tx-id (:steps r))))
    (is (= [:committed-abstract-transition :maintenance-stutter :aborted-stutter :committed-abstract-transition]
           (mapv :relation (:steps r))))
    (is (= [:exact-source-derived-puts :exact-mapped-row-stutter :exact-row-stutter :exact-source-derived-puts]
           (mapv :checked-row-effect (:steps r))))
    (is (= points (:checked-coordinates r)))
    (is (= :not-established (:provider-refinement r) (:universal-refinement r) (:writer-coverage r)))
    (is (= :not-performed (:proof-discharge r)))
    (is (not (contains? r :images)))
    (doseq [[bad reason]
            [[(assoc-in f [:images (points 2) :physical :requests "r1" :extra] "foreign")
              :selected-complete-mapped-row-mismatch]
             [(reduce #(assoc-in %1 [:images maintenance-post %2 :requests "r1" :extra] "foreign") f [:retained :physical])
              :selected-maintenance-row-stutter-mismatch]
             [(reduce #(assoc-in %1 [:images abort-post %2 :requests "r1" :extra] "partial") f [:retained :physical])
              :aborted-row-effect-mismatch]
             [(update-in f [:steps 0 :expectation :ops] pop) :selected-retained-command-envelope-mismatch]
             [(assoc-in f [:steps 1 :spec :outcome-cases] {}) :selected-retained-command-envelope-mismatch]
             [(assoc-in f [:comparison :executor-envelopes] []) :unqualified-selected-transition-sequence]
             [(assoc-in f [:ledger 4 :committed?] true) :unqualified-selected-transition-sequence]
             [(update f :images dissoc (points 1)) :unqualified-selected-transition-sequence]]]
      (let [bad (refinement/check-selected! bad)]
        (is (= :not-established (:status bad)))
        (is (= reason (:reason bad)) (pr-str bad))
        (is (not (contains? bad :steps)))))))

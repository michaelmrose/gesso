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

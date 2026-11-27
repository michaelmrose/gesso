(ns gesso.assurance.mapped-guard-atomicity-test
  "Bounded live-provider experiments, not a universal XTDB semantics proof.
   Deliberately reordered transactions are probes, not admitted commands."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.proof.mapped-prestate-semantics :as semantics]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-transition-refinement :as refinement]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]))

(defn- seed []
  {:requests {"r1" {:xt/id "r1" :status :open :revision 0 :extra "preserve"}}
   :request_assignments {}
   :memberships {"helper-1" {:xt/id "helper-1" :active true}
                 "helper-2" {:xt/id "helper-2" :active true}}
   :gesso_commands {}})

(defn- rows [image]
  [(into {} (map (fn [[table rs]] [table (vec (vals rs))])) (dissoc image :gesso_commands))
   (vec (vals (:gesso_commands image)))])

(defn- prefix! [original image]
  (let [[docs journal] (rows image)
        mapping (plan/check-target-mapping! original claim/target-map)
        prefix (frame/formatted-prestate-assertions! mapping docs journal)
        report (lowering/check-prefix! original claim/target-map docs journal prefix)]
    (is (= :checked-exact-prefix (:status report)))
    (is (= :independent-exact-sql-vectors (:formatter-correspondence report)))
    prefix))

(defn- prepare [image helper branch cases]
  (let [original (claim/source helper branch)
        [docs journal] (rows image)
        d (get (claim/outcome-declarations) (claim/alternative-id helper branch))
        spec {:inputs {} :effects (if (= :accept branch)
                                     {[:document :requests "r1"] {} [:document :request_assignments "primary-r1"] {}}
                                     {})
              :binding {:command-id (identity/command-id (str "command-" helper))
                        :principal (identity/principal helper) :operation :request/claim
                        :arguments (:arguments d)}
              :outcome (:outcome d) :now "fixture-time"
              :alternative (claim/alternative-id helper branch) :outcome-cases cases}
        suffix (writes/derive-puts! original claim/target-map docs journal spec)]
    {:original original :docs docs :journal journal :spec spec :suffix suffix
     :prefix (prefix! original image)}))

(defn- apply-puts [image puts]
  (reduce (fn [m [_ table & rs]]
            (reduce #(assoc-in %1 [table (:xt/id %2)] %2) m rs)) image puts))

(defn- read-image [node basis]
  (into {} (map (fn [table]
                  [table (into {} (map (juxt :xt/id identity))
                               (xt/q node (str "SELECT * FROM " (name table)) basis))]))
        [:requests :request_assignments :memberships :gesso_commands]))

(defn- pin [node]
  {:snapshot-token (:snapshot-token (first (xt/q node "SHOW SNAPSHOT_TOKEN")))
   :current-time (Instant/now)})

(defn- ledger [node basis]
  (xt/q node "SELECT _id, committed FROM xt.txs ORDER BY _id" basis))

(defn- execute! [node ops expected before after]
  ;; Fresh isolated node, no other submitting clients. Check the exact
  ;; appended ledger row and independently pinned before/after images.
  (let [old (pin node) old-ledger (ledger node old)
        result (try {:status :committed :key (xt/execute-tx node ops)}
                    (catch Exception e
                      (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
                        {:status :assert-failed} (throw e))))
        new (pin node) new-ledger (ledger node new)
        tx-id (inc (:xt/id (peek old-ledger)))]
    (is (= expected (:status result)))
    (is (= (conj old-ledger {:xt/id tx-id :committed (= :committed expected)}) new-ledger))
    (when (= :committed expected) (is (= tx-id (get-in result [:key :tx-id]))))
    (is (= before (read-image node old)))
    (is (= after (read-image node new)))
    (is (= old-ledger (ledger node old)))
    (is (= (:snapshot-token old) (:snapshot-token (first (xt/q node "SHOW SNAPSHOT_TOKEN" old)))))
    (is (= before (read-image node old)))
    new))

(defn- semantic-match? [original prepared actual]
  (let [[docs journal] (rows prepared) [actual-docs actual-journal] (rows actual)
        description (semantics/describe! original claim/target-map docs journal)]
    (:matches? (semantics/observe! original claim/target-map docs journal description actual-docs actual-journal))))

(defn- check-step [step commitment before after]
  (let [{:keys [original docs journal spec suffix]} step]
    (refinement/check-step! original claim/target-map docs journal spec suffix commitment before after)))

(deftest exact-claim-prefix-observes-statement-order-and-atomic-rollback
  (let [cases (:cases (outcome/derive-cases! {:inductive-source {:catalog (claim/catalog)}
                                            :target-map claim/target-map}
                                           (claim/outcome-declarations)))]
    (doseq [[winner loser] [["helper-1" "helper-2"] ["helper-2" "helper-1"]]]
      (with-open [node (xtn/start-node {})]
        (let [initial (seed)
              _ (xt/execute-tx node (into [] (keep (fn [[table rs]]
                                                   (when (seq rs) (into [:put-docs table] (vals rs))))) initial))
              win (prepare initial winner :accept cases)
              stale (prepare initial loser :accept cases)
              won (apply-puts initial (:suffix win))
              recovery (prepare won loser :refuse cases)
              recovered (apply-puts won (:suffix recovery))]
          (is (= 3 (count (:suffix win))))
          ;; Independently qualify the exact prefix for each intermediate
          ;; row image. Probe every nonempty prefix of the put suffix.
          (doseq [n (range 1 (inc (count (:suffix win))))]
            (let [partial (subvec (:suffix win) 0 n)
                  intermediate (apply-puts initial partial)
                  intermediate-prefix (prefix! (:original win) intermediate)]
              (is (semantic-match? (:original win) intermediate intermediate))
              (is (false? (semantic-match? (:original win) initial intermediate)))
              ;; New assertions must see these tentative writes, otherwise
              ;; the probe would fail before reaching the old prefix. The
              ;; positive control below separately commits that exact order.
              (execute! node (into (into (:prefix win) partial) intermediate-prefix)
                        :committed initial intermediate)
              ;; Reset only the modified closed rows by ordinary puts or
              ;; deletes before the rollback probe; this is test setup.
              (let [restore (into [] (mapcat (fn [[table rs]]
                                             (let [old (get initial table)
                                                   added (remove #(contains? old %) (keys rs))]
                                               (concat (when (seq added) [(into [:delete-docs table] added)])
                                                       (when (seq old) [(into [:put-docs table] (vals old))]))))) intermediate)]
                (execute! node restore :committed intermediate initial))
              (execute! node (into (into (into (:prefix win) partial) intermediate-prefix) (:prefix win))
                        :assert-failed initial initial)))
          (execute! node (into (:prefix win) (:suffix win)) :committed initial won)
          (is (= :committed-abstract-transition (:relation (check-step win :committed initial won))))
          ;; The original prepared loser prefix cannot authorize writes
          ;; against the new authoritative state, irrespective of its old basis.
          (is (false? (semantic-match? (:original stale) initial won)))
          (execute! node (into (:prefix stale) (:suffix stale)) :assert-failed won won)
          (is (= :aborted-stutter (:relation (check-step stale :aborted won won))))
          ;; A refusal still writes its journal record; a failing late
          ;; assertion must undo it, then a normal retry can commit.
          (let [post-prefix (prefix! (:original recovery) recovered)]
            (execute! node (into (into (into (:prefix recovery) (:suffix recovery)) post-prefix) (:prefix recovery))
                      :assert-failed won won))
          (execute! node (into (:prefix recovery) (:suffix recovery)) :committed won recovered)
          (is (= :committed-abstract-transition (:relation (check-step recovery :committed won recovered)))))))))

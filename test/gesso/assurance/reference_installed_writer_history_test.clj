(ns gesso.assurance.reference-installed-writer-history-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-installed-writer-history :as history]
            [gesso.assurance.reference-installed-writer-classification :as installed]))

(defn- fixture []
  (let [identity {:alternative-id :claim/accept :catalog-fingerprint "c"}
        root {:source-id "s" :manifest-tx-id 1 :initialization-tx-id 3}
        classification {:status :conditional-current-installed-writer-classification
                        :activation-root root :catalog-fingerprint "c" :induction-fingerprint "i"
                        :alternative-identities {:claim/accept identity}
                        :reviewed-policy-branches {:mapped-operation {:effect :conditional-abstract-transition}
                                                   :lease-reclaim {:effect :conditional-mapped-stutter}
                                                   :dispatch-acknowledgment {:effect :conditional-mapped-stutter}}}
        steps (mapv (fn [id] {:original {:subject {:id :claim/accept} :writes #{:field}}
                              :source-id "s" :spec {:alternative :claim/accept}
                              :context {:catalog-fingerprint "c" :induction-fingerprint "i"}
                              :expectation {:tx-id id}}) [7 17 21])
        maintenance [{:tx-id 9 :intent-id "a" :maintenance :expired-lease-reclamation}
                     {:tx-id 13 :intent-id "a" :maintenance :dispatch-acknowledgment}]
        compared {:status :matched :recovery-cut-binding :matched-local-recovery-cut
                  :activation-root-binding :matched-local-activated-retained-root
                  :local-acquisition-binding :matched-issued-origin-and-retained-sample
                  :worker-observation-provenance :matched-local-issued-host-capture
                  :manifest-tx-id 1 :initialization-tx-id 3
                  :selected-tx-ids [7 17 21] :selected-commitments [:committed :aborted :committed]
                  :intervening-maintenance maintenance
                  :worker-correspondence {:maintenance-correspondence :matched-issued-worker-retained-transactions
                                          :retained-payload-fingerprint "p" :selected-tx-ids [7 17 21]
                                          :selected-maintenance (mapv #(assoc % :committed? true :worker-origin {:source-id "s"}) maintenance)}}
        cut {:transaction-ids [1 3 7 9 13 17 21] :payload-fingerprint "p"}]
    [classification steps compared cut]))

(deftest classify-exact-selected-transaction-partition
  (let [[c s r cut :as args] (fixture)
        run (fn [c s r cut] (try (#'history/partition! c s r cut)
                                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        result (apply run args)]
    (is (= [7 9 13 17 21] (mapv :tx-id result)))
    (is (= [:committed-mapped :committed-worker :committed-worker :aborted-attempt :committed-mapped] (mapv :class result)))
    (is (= :lease-reclaim (get-in result [1 :policy-branch])))
    (doseq [ids [[1 3 7 9 13 17 21 23] [1 3 7 9 11 13 17 21] [1 3 7 13 17 21]]]
      (is (= :unclassified-retained-transaction (run c s r (assoc cut :transaction-ids ids)))))
    (is (= :installed-history-evidence-mismatch (run c s r (assoc cut :payload-fingerprint "other"))))
    (is (= :installed-history-evidence-mismatch (run c s (assoc r :initialization-tx-id 2) cut)))
    (is (= :installed-history-evidence-mismatch (run c s (assoc r :selected-commitments [:committed :committed :committed]) cut)))
    (is (= :installed-history-evidence-mismatch (run c s (assoc-in r [:worker-correspondence :selected-tx-ids] [7 17 22]) cut)))
    (is (= :installed-history-maintenance-mismatch (run c s (assoc-in r [:worker-correspondence :selected-maintenance 0 :committed?] false) cut)))
    (is (= :installed-history-maintenance-mismatch (run c s (assoc-in r [:worker-correspondence :selected-maintenance 0 :worker-origin :source-id] "other") cut)))
    (is (= :installed-history-alternative-mismatch (run c (assoc-in s [0 :original :writes] #{}) r cut)))
    (is (= :installed-history-alternative-mismatch (run c (assoc-in s [0 :context :induction-fingerprint] "other") r cut)))
    (with-redefs [installed/derive! (constantly c)]
      (let [bad (history/derive! {} #{} nil {} {} "c" {} [] nil nil nil)]
        (is (= :installed-history-boundary-replaced (:reason bad)))
        (is (not (contains? bad :recovery)))))))

(ns gesso.assurance.reference-mapped-abstract-bracket-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-poststate-test :as fixture]
            [gesso.assurance.reference-mapped-poststate :as poststate]
            [gesso.assurance.reference-provider :as provider]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]))

(deftest abstract-check-shares-the-commitment-bracket
  ;; Stub the acquisition boundary only: real mapped-prefix checking,
  ;; historical replay, commitment selection and abstract comparison run.
  (let [docs {:gesso_requests [fixture/row] :gesso_assignments []}
        prefix (frame/formatted-prestate-assertions!
                (plan/check-target-mapping! fixture/source fixture/targets) docs [])
        ops (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" "c1"]]
                  (concat prefix fixture/puts))
        selected (fixture/tx 1 ops)
        history [fixture/initial selected fixture/later]
        sample {:status :observed :replay-inputs history}
        tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 3
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        check! (fn [samples scope spec committed?]
                 (let [remaining (atom samples)]
                   (with-redefs-fn {#'provider/observe-replay-history! (fn [_] (let [s (first @remaining)] (swap! remaining rest) s))
                                    #'provider/compare-source-bound-execution! (fn [_ inputs _] (is (= history inputs)) comparison)
                                    #'xt/q (fn [& _] [{:committed committed?}])}
                     #(provider/compare-local-processed-retained-mapped-publication-absence!
                       nil scope fixture/source fixture/targets docs [] selected (count prefix) "c1" [] [] :publication-first true spec))))
        run! #(check! [sample sample] tables % true)
        matched (run! fixture/spec)]
    (is (= :matched (:status matched)) (pr-str matched))
    (is (= :committed (:selected-transaction-commitment matched)))
    (is (= poststate/profile (:selected-abstract-transition-profile matched)))
    (is (= :matched-retained-projection (:abstract-transition matched)))
    (is (= :not-established (:provider-refinement matched)))
    (doseq [[report reason] [[(run! (assoc-in fixture/spec [:inputs :example/next-count] 4)) :selected-retained-abstract-transition-mismatch]
                             [(check! [sample (assoc sample :changed true)] tables fixture/spec true) :source-log-changed-during-comparison]
                             [(check! [sample sample] (disj tables :gesso_assignments) fixture/spec true) :incomplete-abstract-projection-scope]
                             [(check! [sample sample] tables fixture/spec false) :selected-retained-commitment-outcome-mismatch]]]
      (is (= reason (:reason report)) (pr-str report))
      (is (not (contains? report :abstract-transition)))
      (is (not (contains? report :selected-transaction-commitment))))
    (let [legacy (run! nil)]
      (is (= :matched (:status legacy)))
      (is (= :committed (:selected-transaction-commitment legacy)))
      (is (not (contains? legacy :abstract-transition))))))

(deftest complete-envelope-composes-abstract-check
  (let [docs {:gesso_requests [fixture/row] :gesso_assignments []}
        prefix (frame/formatted-prestate-assertions!
                (plan/check-target-mapping! fixture/source fixture/targets) docs [])
        spec (assoc fixture/spec :effects {[:document :gesso_requests "r1"] {}
                                           [:document :gesso_assignments "a1"] {}}
                    :binding fixture/command-binding :outcome {:status :claimed} :now "t1")
        fp (str "sha256:" (apply str (repeat 64 "a")))
        context {:catalog-fingerprint fp :induction-fingerprint fp :selectors {}}
        declarations {:example/claim nil}
        witness (evidence/derive-record! fixture/source fixture/targets docs [] prefix spec context)
        ops (envelope/bind-transition-envelope!
             (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" "c1"]]
                   (concat prefix fixture/puts [[:put-docs :gesso_transition_evidence witness]])))
        selected (fixture/tx 1 ops)
        history [fixture/initial selected]
        sample {:status :observed :replay-inputs history}
        tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        comparison {:status :matched :profile provider/profile :acquisition-profile provider/acquisition-profile
                    :source-binding-profile provider/source-binding-profile :transaction-count 2
                    :checks {:ledger true :retained-envelopes true :selected-history true}
                    :authority :none :certificate :not-issued :provider-refinement :not-established}
        check! (fn [compare scope]
                 (compare nil scope fixture/source fixture/targets docs [] selected (count prefix)
                          spec context "source-a" declarations declarations))]
    (with-redefs-fn {#'provider/observe-replay-history! (constantly sample)
                     #'provider/compare-source-bound-execution! (constantly comparison)
                     #'xt/q (fn [& _] [{:committed true}])}
      (fn []
        (let [r (check! evidence/compare-local-processed-retained-mapped-executor-abstract! tables)]
          (is (= :matched (:status r)) (pr-str r))
          (is (= :selected-mapped-retained-executor-abstract-v1 (:mapped-retained-evidence-profile r)))
          (is (= :matched-retained-projection (:abstract-transition r)))
          (is (= :matched-source-relative-record (:transition-evidence r)))
          (is (= :committed (:selected-transaction-commitment r)))
          (is (= :not-established (:provider-refinement r))))
        (let [r (check! evidence/compare-local-processed-retained-mapped-executor-commit! tables)]
          (is (= :matched (:status r)))
          (is (not (contains? r :abstract-transition))))
        (let [r (check! evidence/compare-local-processed-retained-mapped-executor-abstract!
                        (disj tables :gesso_assignments))]
          (is (= :incomplete-abstract-projection-scope (:reason r)))
          (is (not-any? #(contains? r %) [:abstract-transition :transition-evidence :selected-transaction-commitment])))))))

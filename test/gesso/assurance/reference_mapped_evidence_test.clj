(ns gesso.assurance.reference-mapped-evidence-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-writes-test :as fixture]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.subject :as subject]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.mapped-frame :as frame]))

(deftest derive-v4-witness-without-candidate-record
  (let [source @#'fixture/source targets @#'fixture/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        spec {:inputs {:example/next-count 3} :effects {[:document :gesso_requests "r1"] {:revision 1}
                                                       [:document :gesso_assignments "a1"] {:revision 0}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping docs [])
        fp (fn [id] (:fingerprint (subject/describe-subject {:kind :rule :id id :interface {:fixture true}})))
        context {:catalog-fingerprint (fp :example/catalog) :induction-fingerprint (fp :example/induction)
                 :selectors {:example/next-count [:argument :count]}}
        before {:example/count 2 :example/enabled true :example/owner nil :example/outcome nil}
        after {:example/count 3 :example/enabled true :example/owner "helper-a" :example/outcome :claimed}
        actual (#'inductive/transition-evidence
                (assoc (dissoc context :selectors) :input-derivations {:example/claim (:selectors context)})
                :example/claim mapping "c1" before (:inputs spec) after (frame/prefix-evidence! prefix))
        check #(evidence/check-record! source targets docs [] prefix spec context %)
        derived (writes/derive-transition! source targets docs [] spec)]
    (is (= before (:before derived)))
    (is (= after (:after derived)))
    (is (= actual (evidence/derive-record! source targets docs [] prefix spec context)))
    (is (= :matched (:status (check actual))))
    (is (= :source-relative-unbound-record (:scope (check actual))))
    (is (= :none (:authority (check actual))))
    (is (= :not-issued (:certificate (check actual))))
    (doseq [k (keys actual)]
      (let [r (check (dissoc actual k))]
        (is (= :mismatch (:status r)) (str k))
        (is (not (contains? r :scope)))))
    (doseq [bad [(assoc actual :xt/id "c2")
                 (update actual :prestate-witness #(vec (reverse %)))
                 (assoc-in actual [:input-witness 0 3] "03")
                 (assoc actual :poststate-fingerprint (fp :example/other))
                 (assoc actual :envelope-fingerprint (fp :example/envelope))]]
      (is (= :mismatch (:status (check bad)))))
    (doseq [[p s c] [[(pop prefix) spec context]
                     [prefix (assoc-in spec [:inputs :example/next-count] "3") context]
                     [prefix spec (assoc context :catalog-fingerprint "bad")]
                     [prefix spec (assoc context :extra true)]]]
      (is (= :unsupported (:status (evidence/check-record! source targets docs [] p s c actual)))))
    (is (= :mismatch (:status (evidence/check-record! source targets docs [] prefix
                                                     (assoc-in spec [:inputs :example/next-count] 4) context actual))))
    (is (= :mismatch (:status (evidence/check-record! source targets docs [] prefix spec
                                                     (assoc context :selectors {}) actual))))))

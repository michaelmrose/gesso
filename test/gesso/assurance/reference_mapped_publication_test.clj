(ns gesso.assurance.reference-mapped-publication-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-mapped-writes-test :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]))

(deftest derive-declared-publication-without-candidate-outbox
  (let [source @#'fixture/source targets @#'fixture/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        spec {:inputs {:example/next-count 3} :effects {[:document :gesso_requests "r1"] {:revision 1}
                                                       [:document :gesso_assignments "a1"] {:revision 0}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        source-id "source-🚀"
        contributions [{:target [:document :gesso_requests "r1"] :topic :example/revision :field :revision}
                       {:target [:document :gesso_assignments "a1"] :topic :example/revision :field :revision}
                       {:target [:document :gesso_requests "r1"] :topic :example/request :field :xt/id}]
        declaration {:purpose :example/refresh :contributions contributions}
        declarations {:example/claim declaration}
        derive #(publication/derive-publication! source targets docs [] spec source-id %)
        actual (outbox/prepare! {:intent-ids (mapv #(outbox/derived-intent-id source-id :example/claim "helper-a" "c1" %) (range 3))
                                 :source-id source-id :operation :example/claim :purpose :example/refresh :command-id "c1"}
                                [{:topic :example/revision :id 0} {:topic :example/revision :id 1}
                                 {:topic :example/request :id "r1"}])
        fragment {:guard-ops (:guard-ops actual) :put-ops [(:tx-op actual)]}
        check #(publication/check-publication! source targets docs [] spec source-id declarations %)]
    (is (= (assoc fragment :intents (:intents actual)) (derive declarations)))
    (is (= [0 1 "r1"] (mapv :gesso.live.outbox/subject-id (:intents (derive declarations)))))
    (is (= 3 (count (set (map :xt/id (:intents (derive declarations)))))))
    (is (every? #(and (= 8 (.version ^java.util.UUID %)) (= 2 (.variant ^java.util.UUID %)))
                (map :xt/id (:intents (derive declarations)))))
    (is (= :matched (:status (check fragment))))
    (is (= :source-relative-publication-fragment (:scope (check fragment))))
    (is (= :none (:authority (check fragment))))
    (is (= :not-issued (:certificate (check fragment))))
    (is (= :not-established (:provider-refinement (check fragment))))
    (is (= {:intents [] :guard-ops [] :put-ops []} (derive {:example/claim nil})))
    (is (= :matched (:status (publication/check-publication! source targets docs [] spec source-id
                                                             {:example/claim nil} {:guard-ops [] :put-ops []}))))
    (doseq [bad [{} {:example/claim {}} {:example/claim (assoc declaration :extra true)}
                 {:example/claim (assoc declaration :purpose :unqualified)}
                 {:example/claim (assoc declaration :contributions [])}
                 {:example/claim (assoc declaration :contributions [(first contributions)])}
                 {:example/claim (update declaration :contributions conj (first contributions))}
                 {:example/claim (assoc-in declaration [:contributions 1 :field] :owner)}
                 {:example/claim (assoc-in declaration [:contributions 1 :target] [:document :other "r1"])}
                 {:example/claim (assoc-in declaration [:contributions 2 :field] :missing)}]]
      (is (thrown? clojure.lang.ExceptionInfo (derive bad)) (pr-str bad)))
    (doseq [bad [(update fragment :guard-ops pop)
                 (update fragment :guard-ops conj (first (:guard-ops fragment)))
                 (update fragment :guard-ops #(vec (reverse %)))
                 (assoc-in fragment [:guard-ops 0 1] (java.util.UUID. 0 1))
                 (assoc-in fragment [:put-ops 0 2 :gesso.live.outbox/subject-id] 9)
                 (assoc-in fragment [:put-ops 0 2 :gesso.live.outbox/status] :delivered)
                 (assoc-in fragment [:put-ops 0 2 :extra] true)
                 (update-in fragment [:put-ops 0] pop)
                 (update-in fragment [:put-ops 0] conj (first (:intents actual)))
                 (assoc-in fragment [:put-ops 0 2 :xt/id] (java.util.UUID. 0 1))
                 (assoc-in fragment [:put-ops 0 2 :gesso.live.outbox/version] 1N)
                 (update fragment :guard-ops #(apply list %))
                 (with-meta fragment {:untrusted true})]]
      (let [r (check bad)]
        (is (= :mismatch (:status r)))
        (is (not (contains? r :scope)))))
    (is (= :unsupported (:status (publication/check-publication! source targets docs [] spec "" declarations fragment))))
    (let [reordered (derive {:example/claim (assoc declaration :contributions (vec (reverse contributions)))})]
      (is (= ["r1" 0 1] (mapv :gesso.live.outbox/subject-id (:intents reordered))))
      (is (= (mapv :xt/id (:intents actual)) (mapv :xt/id (:intents reordered))))
      (is (not= (:intents actual) (:intents reordered))))))

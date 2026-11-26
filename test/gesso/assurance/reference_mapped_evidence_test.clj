(ns gesso.assurance.reference-mapped-evidence-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-writes-test :as fixture]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

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

(deftest compose-bound-evidence-with-retained-predecessor
  (let [root (Files/createTempDirectory "gesso-retained-mapped-evidence-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        source @#'fixture/source targets @#'fixture/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve"}
        docs {:gesso_requests [row] :gesso_assignments []}
        binding {:command-id (identity/command-id "c1") :principal (identity/principal "helper-a")
                 :operation :example/claim :arguments {:request-id "r1"}}
        spec {:inputs {:example/next-count 3} :effects {[:document :gesso_requests "r1"] {:revision 1}
                                                       [:document :gesso_assignments "a1"] {:revision 0}}
              :binding binding :outcome {:status :claimed} :now "t1" :outcome-key :status}
        fresh (assoc-in spec [:binding :command-id] (identity/command-id "c2"))
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping docs [])
        fp (fn [id] (:fingerprint (subject/describe-subject {:kind :rule :id id :interface {:fixture true}})))
        context {:catalog-fingerprint (fp :example/catalog) :induction-fingerprint (fp :example/induction)
                 :selectors {:example/next-count [:argument :count]}}
        before {:example/count 2 :example/enabled true :example/owner nil :example/outcome nil}
        after {:example/count 3 :example/enabled true :example/owner "helper-a" :example/outcome :claimed}
        make-ops (fn [s]
                   (let [id (resolution/record-id (:binding s))
                         record (#'inductive/transition-evidence
                                 (assoc (dissoc context :selectors) :input-derivations {:example/claim (:selectors context)})
                                 :example/claim mapping id before (:inputs s) after (frame/prefix-evidence! prefix))]
                     (envelope/bind-transition-envelope!
                      (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]]
                            (concat prefix [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" id]
                                            [:put-docs :gesso_requests (assoc row :amount 3 :revision 1)]
                                            [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a" :revision 0}]
                                            [:put-docs :gesso_commands (resolution/command-record (:binding s) (:outcome s) (:now s))]
                                            [:put-docs :gesso_transition_evidence record]])))))
        a (make-ops spec) b (make-ops fresh)
        ;; Deliberately seed an orphan evidence record; the mapped slot and
        ;; command ID remain absent, so only the evidence guard rejects c1.
        transactions [[[:put-docs :gesso_requests row] (peek a)] a b]
        tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence}
        check! (fn [node expectation s scope]
                 (evidence/compare-local-processed-retained-mapped-evidence!
                  node scope source targets docs [] expectation (count prefix) s context))
        matched! (fn [r]
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= :matched-source-relative-record (:transition-evidence r)))
                   (is (= :matched (:transition-envelope-binding r)))
                   (is (= :absent-at-retained-predecessor (:global-command-id r)))
                   (is (= :absent-at-retained-predecessor (:global-evidence-id r)))
                   (is (= :matched-retained-predecessor (:prepared-prestate r)))
                   (is (= :not-asserted (:selected-transaction-commitment r)))
                   (is (= :none (:authority r)))
                   (is (= :not-issued (:certificate r)))
                   (is (= :not-established (:provider-refinement r))))
        rejected! (fn [r status reason]
                    (is (= status (:status r)) (pr-str r))
                    (is (= reason (:reason r)))
                    (is (not-any? #(contains? r %) [:transition-evidence :transition-envelope-binding :global-evidence-id])))
        rebind (fn [ops]
                 (envelope/bind-transition-envelope!
                  (update-in ops [(dec (count ops)) 2] #(apply dissoc % envelope/binding-keys))))]
    (try
      (let [history (with-open [node (xtn/start-node config)]
                      (let [database (db/primary-db node)
                            history (mapv (fn [i ops]
                                            (let [time (.plusSeconds t i)
                                                  id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                                              (.awaitTxBlocking database id (Duration/ofSeconds 30))
                                              {:tx-id id :system-time time :ops ops})) (range) transactions)
                            expected (peek history)
                            last-index (dec (count b))]
                        (.awaitSourceBlocking database (:tx-id expected) (Duration/ofSeconds 30))
                        (is (= [true false true]
                               (mapv (fn [tx] (:committed (first (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id tx)])))) history)))
                        (is (= [(peek (peek a))] (xt/q node ["SELECT * FROM gesso_transition_evidence WHERE _id = ?" "c1"])))
                        (is (empty? (xt/q node ["SELECT * FROM gesso_commands WHERE _id = ?" "c1"])))
                        (rejected! (check! node (second history) spec tables) :mismatch :selected-retained-evidence-id-present)
                        (matched! (check! node expected fresh tables))
                        (doseq [[ops reason] [[(assoc-in b [last-index 2 :envelope-fingerprint] (fp :example/altered)) :transition-evidence-envelope-binding-mismatch]
                                              [(rebind (assoc-in b [last-index 2 :poststate-fingerprint] (fp :example/altered))) :transition-evidence-record-mismatch]
                                              [(rebind (assoc-in b [(+ 2 (count prefix)) 2 :amount] 99)) :selected-retained-write-suffix-mismatch]
                                              [(rebind (let [i (+ 2 (count prefix))] (assoc b i (nth b (inc i)) (inc i) (nth b i)))) :selected-retained-transaction-mismatch]]]
                          (rejected! (check! node (assoc expected :ops ops) fresh tables) :mismatch reason))
                        (doseq [ops [(subvec b 1) (assoc-in b [0 1] "other") (pop b)
                                     (assoc-in b [(inc (count prefix)) 1] "other")]]
                          (rejected! (check! node (assoc expected :ops ops) fresh tables) :unsupported :invalid-mapped-evidence-envelope))
                        (is (= :unsupported (:status (check! node expected fresh (disj tables :gesso_transition_evidence)))))
                        history))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id (peek history)) (Duration/ofSeconds 30))
          (matched! (check! node (peek history) fresh tables))
          (rejected! (check! node (second history) spec tables) :mismatch :selected-retained-evidence-id-present)))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

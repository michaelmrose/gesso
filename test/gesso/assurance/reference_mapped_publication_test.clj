(ns gesso.assurance.reference-mapped-publication-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-mapped-writes-test :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox :as outbox]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.assurance.subject :as subject]
            [gesso.model.certified-plan :as plan]
            [gesso.model.inductive-plan :as inductive]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

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

(deftest publication-envelope-and-historical-identity-absence
  (let [root (Files/createTempDirectory "gesso-retained-publication-envelope-" (make-array FileAttribute 0))
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
        source-id "source-a"
        declarations {:example/claim {:purpose :example/refresh
                                       :contributions [{:target [:document :gesso_requests "r1"] :topic :example/revision :field :revision}
                                                       {:target [:document :gesso_assignments "a1"] :topic :example/revision :field :revision}]}}
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping docs [])
        fp (fn [id] (:fingerprint (subject/describe-subject {:kind :rule :id id :interface {:fixture true}})))
        context {:catalog-fingerprint (fp :example/catalog) :induction-fingerprint (fp :example/induction)
                 :selectors {:example/next-count [:argument :count]}}
        before {:example/count 2 :example/enabled true :example/owner nil :example/outcome nil}
        after {:example/count 3 :example/enabled true :example/owner "helper-a" :example/outcome :claimed}
        make-ops (fn [s publish?]
                   (let [id (resolution/record-id (:binding s))
                         record (#'inductive/transition-evidence
                                 (assoc (dissoc context :selectors) :input-derivations {:example/claim (:selectors context)})
                                 :example/claim mapping id before (:inputs s) after (frame/prefix-evidence! prefix))
                         out (when publish?
                               (outbox/prepare! {:intent-ids (mapv #(outbox/derived-intent-id source-id :example/claim "helper-a" id %) (range 2))
                                                 :source-id source-id :operation :example/claim :purpose :example/refresh :command-id id}
                                                [{:topic :example/revision :id 0} {:topic :example/revision :id 1}]))]
                     (envelope/bind-transition-envelope!
                      (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]]
                            (concat prefix (:guard-ops out)
                                    [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands WHERE _id = ?)" id]
                                     [:put-docs :gesso_requests (assoc row :amount 3 :revision 1)]
                                     [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a" :revision 0}]
                                     [:put-docs :gesso_commands (resolution/command-record (:binding s) (:outcome s) (:now s))]]
                                    (when out [(:tx-op out)]) [[:put-docs :gesso_transition_evidence record]])))))
        a (make-ops spec true) b (make-ops fresh true) empty-ops (make-ops fresh false)
        occupied (nth (nth a (- (count a) 2)) 2)
        seed [[:put-docs :gesso_requests row] [:put-docs :gesso_live_outbox occupied]]
        tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        check! (fn [node tx s ds scope]
                 (evidence/compare-local-processed-retained-mapped-publication!
                  node scope source targets docs [] tx (count prefix) s context source-id ds))
        submit! (fn [node n ops]
                  (let [time (.plusSeconds t n) database (db/primary-db node)
                        id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                    (.awaitTxBlocking database id (Duration/ofSeconds 30))
                    (.awaitSourceBlocking database id (Duration/ofSeconds 30))
                    {:tx-id id :system-time time :ops ops}))
        matched! (fn [r]
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= :matched-source-relative-publication (:publication-fragment r)))
                   (is (= :absent-at-retained-predecessor (:publication-ids r)))
                   (is (= :matched (:transition-envelope-binding r)))
                   (is (= :not-asserted (:selected-transaction-commitment r)))
                   (is (= :none (:authority r)))
                   (is (= :not-issued (:certificate r))))
        rejected! (fn [r status reason]
                    (is (= status (:status r)) (pr-str r))
                    (is (= reason (:reason r)))
                    (is (not-any? #(contains? r %) [:publication-fragment :publication-ids :transition-evidence])))
        rebind (fn [ops] (envelope/bind-transition-envelope!
                          (update-in ops [(dec (count ops)) 2] #(apply dissoc % envelope/binding-keys))))]
    (try
      (let [[collision committed]
            (with-open [node (xtn/start-node config)]
              (submit! node 0 seed)
              (let [collision (submit! node 1 a) committed (submit! node 2 b)
                    guard-index (inc (count prefix)) put-index (- (count b) 2)]
                (is (= [{:committed false}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id collision)])))
                (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id committed)])))
                (is (= [occupied] (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" (:xt/id occupied)])))
                (is (= 3 (count (xt/q node "SELECT * FROM gesso_live_outbox"))))
                (rejected! (check! node collision spec declarations tables) :mismatch :selected-retained-publication-id-present)
                (matched! (check! node committed fresh declarations tables))
                (doseq [ops [(assoc-in b [guard-index 1] (java.util.UUID. 0 1))
                             (assoc b guard-index (nth b (inc guard-index)) (inc guard-index) (nth b guard-index))
                             (assoc-in b [put-index 2 :gesso.live.outbox/subject-id] 9)
                             (update-in b [put-index] pop)]]
                  (rejected! (check! node (assoc committed :ops (rebind ops)) fresh declarations tables)
                             :mismatch :publication-fragment-mismatch))
                (is (= :unsupported (:status (check! node committed fresh declarations (disj tables :gesso_live_outbox)))))
                (is (= :unsupported (:status (check! node committed fresh {} tables))))
                [collision committed]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id committed) (Duration/ofSeconds 30))
          (matched! (check! node committed fresh declarations tables))
          (rejected! (check! node collision spec declarations tables) :mismatch :selected-retained-publication-id-present)))
      (with-open [node (xtn/start-node {:log [:local {:path (str root "/empty-log")}]
                                       :storage [:local {:path (str root "/empty-storage")}]})]
        (submit! node 0 seed)
        (let [tx (submit! node 1 empty-ops)]
          (matched! (check! node tx fresh {:example/claim nil} tables))
          (is (= [occupied] (xt/q node "SELECT * FROM gesso_live_outbox")))
          (is (= :unsupported (:status (check! node tx fresh {} tables))))
          (is (= :unsupported (:status (check! node tx fresh {:example/claim nil} (disj tables :gesso_live_outbox)))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

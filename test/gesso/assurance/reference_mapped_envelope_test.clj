(ns gesso.assurance.reference-mapped-envelope-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
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
            [gesso.model.mapped-residual :as residual]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Duration Instant)))

(deftest residual-publication-envelope-correspondence
  (let [root (Files/createTempDirectory "gesso-retained-publication-envelope-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        t (Instant/parse "2026-01-01T00:00:00Z")
        fields [:example/count :example/owner :example/outcome]
        source (-> @#'fixture/source
                   (assoc :guard {:readers (set fields)
                                  :expression [:and [:= [:read :example/count] [:literal 2]]
                                               [:= [:read :example/owner] [:literal nil]]
                                               [:= [:read :example/outcome] [:literal nil]]]})
                   (update :reads into fields))
        targets @#'fixture/targets
        row {:xt/id "r1" :amount 2 :enabled true :revision 0 :extra "preserve" :status :open :updated-at "t0" :request/location-id "l1" :request.detail/last-kind :example/open :location-id "other"}
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
        residual-declaration {:document-layout {[:gesso_requests "r1"] [[:prestate :revision] [:state :example/count] [:prestate :status] [:prestate :updated-at]]}
                              :subject-checks [[:gesso_requests "r1" :request/location-id]]}
        residual-declarations {:example/claim residual-declaration}
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping docs [])
        fp (fn [id] (:fingerprint (subject/describe-subject {:kind :rule :id id :interface {:fixture true}})))
        context {:catalog-fingerprint (fp :example/catalog) :induction-fingerprint (fp :example/induction)
                 :selectors {:example/next-count [:argument :count]}}
        before {:example/count 2 :example/enabled true :example/owner nil :example/outcome nil}
        after {:example/count 3 :example/enabled true :example/owner "helper-a" :example/outcome :claimed}
        make-ops (fn [s publish? residual?]
                   (let [id (resolution/record-id (:binding s))
                         record (#'inductive/transition-evidence
                                 (assoc (dissoc context :selectors) :input-derivations {:example/claim (:selectors context)})
                                 :example/claim mapping id before (:inputs s) after (frame/prefix-evidence! prefix))
                         assertions (when residual? (residual/reconstruct! source mapping residual-declaration (:binding s) docs))
                         out (when publish?
                               (outbox/prepare! {:intent-ids (mapv #(outbox/derived-intent-id source-id :example/claim "helper-a" id %) (range 2))
                                                 :source-id source-id :operation :example/claim :purpose :example/refresh :command-id id}
                                                [{:topic :example/revision :id 0} {:topic :example/revision :id 1}]))]
                     (envelope/bind-transition-envelope!
                      (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" id]]
                            (concat prefix assertions (:guard-ops out)
                                    [[:put-docs :gesso_requests (assoc row :amount 3 :revision 1)]
                                     [:put-docs :gesso_assignments {:xt/id "a1" :owner "helper-a" :revision 0}]
                                     [:put-docs :gesso_commands (resolution/command-record (:binding s) (:outcome s) (:now s))]]
                                    (when out [(:tx-op out)]) [[:put-docs :gesso_transition_evidence record]])))))
        a (make-ops spec true true) b (make-ops fresh true true) empty-ops (make-ops fresh false false)
        occupied (nth (nth a (- (count a) 2)) 2)
        seed [[:put-docs :gesso_requests row] [:put-docs :gesso_live_outbox occupied]]
        tables #{:gesso_requests :gesso_assignments :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        check! (fn [node tx s ds scope rs]
                 (evidence/compare-local-processed-retained-mapped-residual!
                  node scope source targets docs [] tx (count prefix) s context source-id ds rs))
        submit! (fn [node n ops]
                  (let [time (.plusSeconds t n) database (db/primary-db node)
                        id (:tx-id (xt/submit-tx node ops {:system-time time}))]
                    (.awaitTxBlocking database id (Duration/ofSeconds 30))
                    (.awaitSourceBlocking database id (Duration/ofSeconds 30))
                    {:tx-id id :system-time time :ops ops}))
        matched! (fn [r]
                   (is (= :matched (:status r)) (pr-str r))
                   (is (= :matched-source-relative-publication (:publication-fragment r)))
                   (is (= :matched-source-relative-residual (:residual-fragment r)))
                   (is (= :absent-at-retained-predecessor (:publication-ids r)))
                   (is (= :matched (:transition-envelope-binding r)))
                   (is (= :not-asserted (:selected-transaction-commitment r)))
                   (is (= :none (:authority r)))
                   (is (= :not-issued (:certificate r))))
        rejected! (fn [r status reason]
                    (is (= status (:status r)) (pr-str r))
                    (is (= reason (:reason r)))
                    (is (not-any? #(contains? r %) [:publication-fragment :publication-ids :transition-evidence :residual-fragment])))
        rebind (fn [ops] (envelope/bind-transition-envelope!
                          (update-in ops [(dec (count ops)) 2] #(apply dissoc % envelope/binding-keys))))]
    (try
      (let [[collision committed]
            (with-open [node (xtn/start-node config)]
              (submit! node 0 seed)
              (let [collision (submit! node 1 a) committed (submit! node 2 b)
                    residual-index (inc (count prefix)) guard-index (+ residual-index 5) put-index (- (count b) 2)]
                (is (= [{:committed false}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id collision)])))
                (is (= [{:committed true}] (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" (:tx-id committed)])))
                (is (= [occupied] (xt/q node ["SELECT * FROM gesso_live_outbox WHERE _id = ?" (:xt/id occupied)])))
                (is (= 3 (count (xt/q node "SELECT * FROM gesso_live_outbox"))))
                (rejected! (check! node collision spec declarations tables residual-declarations) :mismatch :selected-retained-publication-id-present)
                (matched! (check! node committed fresh declarations tables residual-declarations))
                (is (= :matched (:command-id-absence-guard (check! node committed fresh declarations tables residual-declarations))))
                (is (= :unsupported (:status (check! node committed fresh declarations tables {}))))
                (doseq [ops [(assoc-in b [residual-index 2] "other")
                             (assoc b residual-index (nth b (inc residual-index)) (inc residual-index) (nth b residual-index))
                             (into (subvec b 0 residual-index) (subvec b (inc residual-index)))
                             (into (conj (subvec b 0 residual-index) (nth b residual-index)) (subvec b residual-index))]]
                  (rejected! (check! node (assoc committed :ops (rebind ops)) fresh declarations tables residual-declarations)
                             :mismatch :residual-assertion-fragment-mismatch))
                (doseq [ops [(assoc-in b [guard-index 1] (java.util.UUID. 0 1))
                             (assoc b guard-index (nth b (inc guard-index)) (inc guard-index) (nth b guard-index))
                             (assoc-in b [put-index 2 :gesso.live.outbox/subject-id] 9)
                             (update-in b [put-index] pop)]]
                  (rejected! (check! node (assoc committed :ops (rebind ops)) fresh declarations tables residual-declarations)
                             :mismatch :publication-fragment-mismatch))
                (is (= :unsupported (:status (check! node committed fresh declarations (disj tables :gesso_live_outbox) residual-declarations))))
                (is (= :unsupported (:status (check! node committed fresh {} tables residual-declarations))))
                [collision committed]))]
        (with-open [node (xtn/start-node config)]
          (.awaitSourceBlocking (db/primary-db node) (:tx-id committed) (Duration/ofSeconds 30))
          (matched! (check! node committed fresh declarations tables residual-declarations))
          (rejected! (check! node collision spec declarations tables residual-declarations) :mismatch :selected-retained-publication-id-present)))
      (with-open [node (xtn/start-node {:log [:local {:path (str root "/empty-log")}]
                                       :storage [:local {:path (str root "/empty-storage")}]})]
        (submit! node 0 seed)
        (let [tx (submit! node 1 empty-ops)]
          (let [r (check! node tx fresh {:example/claim nil} tables {:example/claim nil})]
            (matched! r)
            (is (= :not-declared (:command-id-absence-guard r))))
          (is (= :unsupported (:status (check! node tx fresh {:example/claim nil} tables {}))))
          (is (= [occupied] (xt/q node "SELECT * FROM gesso_live_outbox")))
          (is (= :unsupported (:status (check! node tx fresh {} tables residual-declarations))))
          (is (= :unsupported (:status (check! node tx fresh {:example/claim nil} (disj tables :gesso_live_outbox) {:example/claim nil}))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

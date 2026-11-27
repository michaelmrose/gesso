(ns gesso.assurance.reference-source-envelope-argument-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-envelope-argument :as argument]
            [gesso.assurance.reference-source-transition-argument :as transition]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.assurance.reference-mapped-publication :as publication]
            [gesso.assurance.reference-mapped-residual :as residual]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [gesso.live.outbox.claim-semantic-fixture :as claim]))

(defn- fixture []
  (let [original (claim/catalog)
        source (claim/physical-mapped-admission-source {:catalog original})
        declarations (claim/outcome-declarations)
        fp (:fingerprint (catalog/check-operation-set! original))
        cases (:cases (outcome/derive-cases! source declarations))]
    [source declarations fp cases]))

(defn- bind [admitted witness]
  (envelope/bind-transition-envelope!
   (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" (:xt/id witness)]]
         (conj admitted [:put-docs :gesso_transition_evidence witness]))))

(defn- prepared [source declarations fp cases helper branch docs journal]
  (let [id (claim/alternative-id helper branch)
        original (claim/source helper branch) targets (:target-map source)
        declaration (declarations id)
        spec {:inputs {} :effects (claim/document-effects id)
              :binding {:command-id (identity/command-id (str "command-" helper))
                        :principal (identity/principal helper) :operation :request/claim
                        :arguments (:arguments declaration)}
              :outcome (:outcome declaration) :now "t1" :alternative id :outcome-cases cases}
        context {:catalog-fingerprint fp :induction-fingerprint fp :selectors {}}
        source-id "source-1"
        prefix (frame/formatted-prestate-assertions! (plan/check-target-mapping! original targets) docs journal)
        pub (publication/derive-publication! original targets docs journal spec source-id (:publication-derivations source))
        res (residual/derive-assertions! original targets docs journal (:binding spec) (:residual-assertion-derivations source))
        puts (writes/derive-puts! original targets docs journal spec)
        admitted (vec (concat prefix (:guard-ops pub) res puts (:put-ops pub)))
        witness (evidence/derive-record! original targets docs journal prefix spec context)
        bound (bind admitted witness)]
    {:docs docs :journal journal :spec spec :context context :source-id source-id :prefix-count (count prefix)
     :admitted admitted :witness witness :bound bound :consumed bound}))

(defn- initial []
  {:requests [{:xt/id "r1" :status :open :revision 0 :updated-at "t0" :request/location-id "l1"}]
   :request_assignments []
   :memberships [{:xt/id "helper-1" :active true :revision 0 :updated-at "t0"}
                 {:xt/id "helper-2" :active true :revision 0 :updated-at "t0"}]})

(defn- after-writes [docs admitted]
  (reduce (fn [m [_ table & rows]]
            (if (contains? m table)
              (reduce (fn [acc row]
                        (update acc table (fn [rs] (conj (filterv #(not= (:xt/id row) (:xt/id %)) rs) row)))) m rows)
              m)) docs (filter #(= :put-docs (first %)) admitted)))

(deftest source-envelope-schema-and-all-writing-alternatives
  (let [[source declarations fp cases :as args] (fixture)
        schema (apply argument/derive! args)
        original (apply transition/derive! args)]
    (is (= :conditional-source-envelope-schema (:status schema)) (pr-str schema))
    (is (= (set (keys declarations)) (set (keys (:alternatives schema)))))
    (is (= (:outside-fragment original) (:outside-fragment schema)))
    (is (every? (:remaining-conditions schema) (:remaining-conditions original)))
    (doseq [[winner loser] [["helper-1" "helper-2"] ["helper-2" "helper-1"]]]
      (let [win (prepared source declarations fp cases winner :accept (initial) [])
            won (after-writes (initial) (:admitted win))
            journal (vec (mapcat #(drop 2 %) (filter #(= :gesso_commands (second %)) (:admitted win))))
            refusal (prepared source declarations fp cases loser :refuse won journal)]
        (doseq [instance [win refusal]]
          (let [id (get-in instance [:spec :alternative])
                result (argument/check-instance! source declarations fp cases id instance)]
            (is (= :conditional-source-envelope-instance (:status result)) (pr-str result))
            (is (= (get-in schema [:alternatives id :identity]) (:identity result)))
            (is (= (get-in schema [:alternatives id :source-equation-fact]) (:source-equation-fact result)))
            (is (= :independent-exact-sql-vectors (:guard-correspondence result)))
            (is (= :matched-source-relative-selectors (:input-correspondence result)))
            (is (= :not-established (:induction-fingerprint-provenance result)))
            (is (= :matched-supplied-vector-only (:consumption-correspondence result)))
            (is (= :not-established (:provider-refinement result) (:universal-refinement result) (:writer-coverage result)))
            (is (= :not-performed (:proof-discharge result)))
            (is (every? (:remaining-conditions result) (:remaining-conditions schema)))))))))

(deftest changed-source-and-envelope-reject
  (let [[source declarations fp cases :as args] (fixture)
        id (claim/alternative-id "helper-1" :accept)
        instance (prepared source declarations fp cases "helper-1" :accept (initial) [])
        check #(argument/check-instance! source declarations fp cases id %)
        admitted (:admitted instance) witness (:witness instance)
        write-index (first (keep-indexed #(when (= :requests (second %2)) %1) admitted))
        rebind (fn [ops w] (let [b (bind ops w)] (assoc instance :admitted ops :witness w :bound b :consumed b)))
        altered (assoc-in admitted [write-index 2 :status] :open)
        changed (bind altered witness)]
    (is (= :conditional-source-envelope-instance (:status (check instance))))
    (doseq [bad [(assoc instance :consumed (pop (:consumed instance)))
                 (assoc-in instance [:context :catalog-fingerprint] "foreign")
                 (assoc-in instance [:spec :outcome-cases] {})
                 (assoc-in instance [:spec :effects [:document :requests "r1"] :updated-at] "foreign")
                 (assoc-in instance [:context :selectors] {:foreign/input [:principal]})
                 (assoc-in instance [:spec :alternative] (claim/alternative-id "helper-2" :accept))
                 (assoc-in instance [:spec :binding :principal] (identity/principal "helper-2"))
                 (assoc instance :bound changed :consumed changed)
                 (rebind altered witness)
                 (rebind (assoc admitted 0 ["ASSERT TRUE"]) witness)
                 (rebind admitted (assoc witness :poststate-fingerprint fp))
                 (rebind (conj admitted [:put-docs :requests {:xt/id "foreign" :status :open}]) witness)]]
      (let [r (check bad)]
        (is (= :not-established (:status r)) (pr-str r))
        (is (not (contains? r :source-equation-fact)))))
    (is (= :not-established (:status (argument/check-instance! source declarations fp cases (claim/alternative-id "helper-1" :replay-accepted) instance))))
    (with-redefs [evidence/check-executor-envelope (constantly {:status :matched})]
      (is (= :not-established (:status (apply argument/derive! args))))
      (is (= :not-established (:status (check instance)))))
    (with-redefs [argument/derive! (constantly (apply argument/derive! args))]
      (is (= :source-envelope-boundary-replaced (:reason (check instance)))))
    (is (= :conditional-source-envelope-instance (:status (check instance))))))

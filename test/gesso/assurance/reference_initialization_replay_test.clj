(ns gesso.assurance.reference-initialization-replay-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-replay-input :as input]
            [gesso.assurance.reference-transaction :as current]))

(defn- project [clocked? sql args]
  (#'input/project-operation clocked? {:kind :sql :query sql :args args}))

(deftest selected-initialization-absence-projection
  (let [marker "ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = $1)"
        marker-op ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = ?)" "source"]
        tables [:requests :request_assignments :memberships :gesso_commands
                :gesso_transition_evidence :gesso_live_outbox]]
    (is (= marker-op (project false marker ["source"])))
    (is (= #{:gesso_initializations} (current/operation-tables marker-op)))
    (is (nil? (project true marker ["source"])))
    (doseq [table tables
            :let [sql (str "ASSERT 0 = (SELECT COUNT(*) FROM " (name table) ")")]]
      (is (= [sql] (project false sql [])))
      (is (= #{table} (current/operation-tables [sql])))
      (is (nil? (project true sql [])))
      (is (nil? (project false sql ["extra"]))))
    (doseq [[sql args] [[marker []] [marker [""]] [marker [1]] [marker ["s" "extra"]]
                        ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = $2)" ["s"]]
                        ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = $01)" ["s"]]
                        ["ASSERT 1 = (SELECT COUNT(*) FROM requests)" []]
                        ["ASSERT 0 = (SELECT COUNT(*) FROM requests) -- comment" []]
                        ["ASSERT 0 = (SELECT COUNT(*) FROM unrelated)" []]
                        ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_other)" []]]]
      (is (nil? (project false sql args)) (pr-str [sql args])))))

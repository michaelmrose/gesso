(ns gesso.live.outbox.http-aleph-optin-test
  "Offline configuration invariants for optional Aleph qualification. These
   checks do NOT stand in for execution of the real Aleph listener tests."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]))

(deftest aleph-qualifier-does-not-change-default-dependency-graph-test
  (let [project (edn/read-string (slurp "deps.edn"))
        alias (get-in project [:aliases :aleph-integration])]
    (is (not (contains? (:deps project) 'aleph/aleph)))
    (is (= {:mvn/version "0.9.11"}
           (get-in alias [:extra-deps 'aleph/aleph])))
    (is (= ["integration"] (:extra-paths alias)))
    (is (not-any? #{"integration"} (get-in project [:aliases :test :extra-paths])))
    (is (not-any? #{"integration"} (:paths project)))
    (is (not (contains? (get-in project [:aliases :test :extra-deps])
                        'aleph/aleph)))))

(deftest opt-in-runner-requires-explicit-test-execution-test
  (let [tasks (edn/read-string (slurp "bb.edn"))
        entry (pr-str (get-in tasks [:tasks 'test:outbox:aleph :task]))]
    (is (str/includes? entry "-M:aleph-integration"))
    (is (str/includes? entry "-m"))
    (is (str/includes? entry "gesso.live.outbox.http-aleph-qualification-main"))
    (is (not (str/includes? entry "-n"))
        "The Cognitect -n test selector may silently run zero integration tests")))

(ns gesso.assurance.journal-predicate-partition-test
  "Provider feasibility only; do not use this experimental partitioner for
   production admission or independent retained replay."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private selector
  "ASSERT 1 = (SELECT COUNT(*) FROM gesso_commands WHERE (gesso$command$binding).principal.gesso$choreo$identity$value = ? AND (gesso$command$binding).operation = ?")

(defn- predicate-pairs [[sql _ _ & args]]
  (let [prefix (str selector " AND ")]
    (assert (and (str/starts-with? sql prefix) (str/ends-with? sql ")")))
    (let [clauses (str/split (subs sql (count prefix) (dec (count sql))) #" AND " -1)]
      (assert (= (count clauses) (count args)))
      (mapv vector clauses args))))

(defn- partition-prefix [prefix]
  ;; Test-only transformation of the existing exact generated conjunction.
  ;; Preserve slot uniqueness and field count, every predicate and operand,
  ;; and execute all chunks in the same transaction without intervening writes.
  (let [[_ principal operation :as guard] (peek prefix)]
    (into (pop prefix)
          (map (fn [pairs]
                 (into [(str selector " AND " (str/join " AND " (map first pairs)) ")")
                        principal operation]
                       (map second pairs))))
          (partition-all 12 (predicate-pairs guard)))))

(defn- attempt! [node ops]
  (try (xt/execute-tx node ops) :committed
       (catch Exception e
         (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
           :assert-failed (throw e)))))

(deftest partitioned-wide-journal-predicate-feasibility
  (let [binding {:command-id (identity/command-id "c1")
                 :principal (identity/principal "helper-a")
                 :operation :request/claim :arguments {:request-id "r"}}
        items (into [1 :claim/open "open"] (range 3 32))
        row (resolution/command-record binding {:status :claimed :details {:empty {} :items items}} "t1")
        mapping {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}}
        original (frame/formatted-prestate-assertions! mapping {} [row])
        prefix (partition-prefix original)
        chunks (subvec prefix (dec (count original)))
        item-path [:gesso.command/outcome :details :items]
        mutations [(assoc-in row (conj item-path 0) 1.0)
                   (assoc-in row (conj item-path 0) 1M)
                   (assoc-in row (conj item-path 1) "claim/open")
                   (assoc-in row (conj item-path 2) :open)
                   (assoc-in row (conj item-path 16) 999)
                   (assoc-in row (conj item-path 31) 999)
                   (update-in row item-path pop)
                   (update-in row item-path conj 32)
                   (assoc-in row [:gesso.command/outcome :details :empty] {:extra true})
                   (assoc-in row [:gesso.command/binding :arguments :request-id] "other")
                   (assoc-in row [:gesso.command/outcome :status] "claimed")]
        put (fn [r] [:put-docs :gesso_commands r])
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        duplicate (resolution/command-record (assoc binding :command-id (identity/command-id "c2"))
                                             (:gesso.command/outcome row) "t1")]
    (is (= 32 (count items)))
    (is (> (count (predicate-pairs (peek original))) 100))
    (is (= (pop original) (subvec prefix 0 (dec (count original)))))
    (is (= (predicate-pairs (peek original)) (vec (mapcat predicate-pairs chunks))))
    (is (every? #(<= (count (predicate-pairs %)) 12) chunks))
    (is (every? #(= (subvec (peek original) 1 3) (subvec % 1 3)) chunks))
    (with-open [node (xtn/start-node {})]
      (let [written #(xt/q node "SELECT * FROM guard_probe")
            journal #(xt/q node "SELECT * FROM gesso_commands")
            checked (into [probe] prefix)]
        (is (= :assert-failed (attempt! node checked)))
        (is (empty? (written)))
        (xt/execute-tx node [(put row)])
        (is (= :committed (attempt! node prefix)))
        (doseq [changed mutations]
          ;; Candidate and marker are tentative; any failed chunk must roll
          ;; both back, leaving the exact original row visible.
          (is (= :assert-failed (attempt! node (into [(put changed)] checked)))
              (pr-str changed))
          (is (= [row] (journal)))
          (is (empty? (written))))
        (is (= :assert-failed (attempt! node (into [(put duplicate)] checked))))
        (is (= [row] (journal)))
        (is (empty? (written)))
        (is (= :committed (attempt! node (conj prefix probe))))
        (is (= [{:xt/id "probe" :written true}] (written)))))))

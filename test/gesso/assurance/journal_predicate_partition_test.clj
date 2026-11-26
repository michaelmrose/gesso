(ns gesso.assurance.journal-predicate-partition-test
  "Production wide-guard lowering and pinned-provider regression."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.certified-plan :as plan]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
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

(def ^:private source
  {:subject {:kind :operation :id :example/journal :interface {:operation :example/journal}}
   :state-types {:example/outcome {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {} :frame #{:example/outcome}})

(def ^:private targets {:example/outcome [:journal-slot "helper-a" :request/claim]})

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
        mapping (plan/check-target-mapping! source targets)
        prefix (frame/formatted-prestate-assertions! mapping {} [row])
        chunks (subvec prefix 2)
        constraints (vec (#'lowering/journal-constraints row))
        check #(lowering/check-prefix! source targets {} [row] %)
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
    (is (> (count constraints) 100))
    (is (= [(str selector ")") "helper-a" :request/claim] (first prefix)))
    (is (= constraints (vec (mapcat predicate-pairs chunks))))
    (is (= :checked-exact-prefix (:status (check prefix))))
    (is (= 5 (:version (check prefix))))
    (is (= :not-established (:sql-refinement (check prefix))))
    (doseq [bad [(pop prefix) (assoc prefix 2 (peek prefix))
                 (into (subvec prefix 0 2) (reverse chunks))
                 (assoc-in prefix [2 3] "wrong-value")]]
      (is (thrown? clojure.lang.ExceptionInfo (check bad))))
    (let [narrow (assoc-in row item-path [1 :claim/open])
          p (frame/formatted-prestate-assertions! mapping {} [narrow])]
      (is (= 3 (count p)))
      (is (= :checked-exact-prefix (:status (lowering/check-prefix! source targets {} [narrow] p)))))
    (is (every? #(<= (count (predicate-pairs %)) 12) chunks))
    (is (every? #(= (subvec (first prefix) 1 3) (subvec % 1 3)) chunks))
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

(ns gesso.assurance.reference-partitioned-journal-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transaction :as reference]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defn- attempt! [node ops]
  (try (xt/execute-tx node ops) :committed
       (catch Exception e
         (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
           :assert-failed (throw e)))))

(deftest independently-interpret-wide-journal-guards
  (let [binding {:command-id (identity/command-id "c1")
                 :principal (identity/principal "helper-a")
                 :operation :request/claim :arguments {:request-id "r"}}
        items (into [1 :claim/open "open"] (range 3 32))
        row (resolution/command-record binding {:status :claimed :details {:empty {} :items items}} "t1")
        prefix (frame/formatted-prestate-assertions!
                {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}} {} [row])
        state {:gesso_commands {(:xt/id row) row}}
        put (fn [r] [:put-docs :gesso_commands r])
        probe [:put-docs :guard_probe {:xt/id "probe" :written true}]
        item-path [:gesso.command/outcome :details :items]
        changed [(assoc-in row [:gesso.command/binding :arguments :request-id] "other")
                 (assoc-in row (conj item-path 16) 999)
                 (update-in row item-path pop)
                 (assoc-in row [:gesso.command/outcome :details :empty] {:extra true})
                 (assoc-in row [:gesso.command/outcome :status] :changed)]
        run #(reference/execute state %)]
    (is (> (count prefix) 9))
    (is (= :predicted (:status (run prefix))))
    (is (true? (:committed? (run prefix))))
    (is (= state (:state (run prefix))))
    (is (= 0 (:failed-assertion-index (reference/execute {} prefix))))
    (let [duplicate (assoc row :xt/id "other")
          r (run (into [(put duplicate)] prefix))]
      (is (= 1 (:failed-assertion-index r)))
      (is (= state (:state r))))
    (let [narrow-row (assoc-in row item-path [1 :claim/open])
          narrow (frame/formatted-prestate-assertions!
                  {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}} {} [narrow-row])
          r (run (into [(put narrow-row)] (conj narrow (peek narrow))))]
      (is (= 3 (count narrow)))
      (is (= :predicted (:status r)))
      (is (true? (:committed? r))))
    ;; Partial physical comparisons have not been qualified across domains.
    (let [r (run (into [(put (assoc-in row (conj item-path 1) "claim/open"))] prefix))]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :committed?))))
    (doseq [bad [(subvec prefix 2)
                 (assoc prefix 3 (update (nth prefix 3) 0 str/replace "STR(ARRAY[?])" "STR(?)"))
                 (assoc prefix 3 (pop (nth prefix 3)))
                 (assoc prefix 3 (assoc (nth prefix 3) 1 "other"))]]
      (let [r (run (into [["ASSERT FALSE"]] bad))]
        (is (= :unsupported (:status r)) (pr-str r))
        (is (not (contains? r :state)))))
    (with-open [node (xtn/start-node {})]
      (is (= :committed (attempt! node [(put row)])))
      (is (= :committed (attempt! node prefix)))
      (let [indices
            (mapv (fn [candidate]
                    (let [r (run (into [(put candidate) probe] prefix))]
                      (is (= :predicted (:status r)) (pr-str r))
                      (is (false? (:committed? r)))
                      (is (= state (:state r)))
                      (is (= :assert-failed (attempt! node (into [(put candidate) probe] prefix))))
                      (is (= [row] (xt/q node "SELECT * FROM gesso_commands")))
                      (is (empty? (xt/q node "SELECT * FROM guard_probe")))
                      ;; Probe actual physical assertions in order, not the
                      ;; abstract leaf or the unsplit conjunction. Restore the
                      ;; original journal after the successful probes.
                      (let [i (first (keep-indexed (fn [i guard]
                                                     (when (= :assert-failed (attempt! node [(put candidate) guard])) i))
                                                   prefix))]
                        (is (some? i))
                        (is (= (+ 2 i) (:failed-assertion-index r)))
                        (is (= :committed (attempt! node [(put row)])))
                        i))) changed)]
        (is (>= (count (distinct indices)) 3) (pr-str indices))))))

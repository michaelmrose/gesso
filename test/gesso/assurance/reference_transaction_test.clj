(ns gesso.assurance.reference-transaction-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-transaction :as reference]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.model.tx :as tx]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-frame :as frame]
            [xtdb.api :as xt]
            [xtdb.db-catalog :as db]
            [xtdb.node :as xtn])
  (:import (java.time Duration)))

(def ^:private a {:xt/id "a" :value 1 :old true})
(def ^:private b {:xt/id "b" :value 2})
(def ^:private replacement {:xt/id "a" :value 3})
(def ^:private seed {:refprobe {"a" a} :refother {"a" b}})
;; Use matching keys for the canonical state, including same id across tables.
(def ^:private initial (assoc-in seed [:refother "a"] (assoc b :xt/id "a")))
(def ^:private put-a [:put-docs :refprobe replacement])
(def ^:private put-b [:put-docs :refprobe b])
(def ^:private put-other [:put-docs :refother {:xt/id "a" :value 9}])

(deftest independent-current-state-execution
  (let [run #(reference/execute initial %)
        committed (run [put-a ["ASSERT TRUE"] put-b put-other])
        aborted (run [put-a put-other ["ASSERT FALSE"] put-b])]
    (is (= :predicted (:status committed)))
    (is (true? (:committed? committed)))
    (is (= {:refprobe {"a" replacement "b" b}
            :refother {"a" {:xt/id "a" :value 9}}} (:state committed)))
    (is (= replacement (get-in (:state committed) [:refprobe "a"])))
    (is (= :predicted (:status aborted)))
    (is (false? (:committed? aborted)))
    (is (= 2 (:failed-assertion-index aborted)))
    (is (= initial (:state aborted)))
    (is (= initial (:state (run [["ASSERT TRUE"]]))))
    (is (= 0 (:failed-assertion-index (run [["ASSERT FALSE"] put-a]))))
    (is (= a (get-in (:state (run [put-a [:put-docs :refprobe a]])) [:refprobe "a"])))
    (is (= :none (:authority committed)))
    (is (= :not-issued (:certificate committed)))))

(def ^:private uuid-a #uuid "00000000-0000-0000-0000-000000000001")
(def ^:private uuid-b #uuid "00000000-0000-0000-0000-000000000002")

(def ^:private equality-cases
  [[0 0 true] [Long/MIN_VALUE Long/MAX_VALUE false]
   [Long/MAX_VALUE Long/MAX_VALUE true] [-1 1 false]
   [true true true] [false false true] [true false false]
   ["" "" true] ["open" "Open" false] ["café" "café" true]
   ["café" "cafe\u0301" false] ["a" "a " false]
   [:open :open true] [:open :closed false]
   [:claim/status :claim/status true] [:claim/status :other/status false]
   [uuid-a uuid-a true] [uuid-a uuid-b false]])

(deftest independent-parameterized-equality
  (doseq [[left right expected] equality-cases]
    (let [assertion ["ASSERT ? = ?" left right]
          predicted (reference/execute initial [put-a assertion put-other])]
      (is (= :predicted (:status predicted)) (pr-str assertion))
      (is (= expected (:committed? predicted)) (pr-str assertion))
      (is (= (if expected (-> initial
                              (assoc-in [:refprobe "a"] replacement)
                              (assoc-in [:refother "a"] {:xt/id "a" :value 9}))
                 initial) (:state predicted)))
      (is (= (when-not expected 1) (:failed-assertion-index predicted)))
      (is (= :none (:authority predicted)))
      (is (= :not-issued (:certificate predicted))))))

(defn- read-guard [table id field expected]
  [(str "ASSERT EXISTS (SELECT 1 FROM \"" (name table)
        "\" WHERE _id = ? AND \"" (name field) "\" = ?)") id expected])

(def ^:private read-cases
  ;; Each case establishes its own relevant starting value.
  (let [reset-a [:put-docs :refprobe a]
        g #(read-guard :refprobe "a" :value %)]
    [[true [reset-a (g 1) put-a (g 3) put-other]]
     [false [reset-a put-a (g 1) put-other]]
     [true [reset-a put-a (g 3) reset-a (g 1)]]
     [false [reset-a put-a (g 3) reset-a (g 3)]]
     [true [put-b (read-guard :refprobe "b" :value 2) put-other]]
     [false [put-a (read-guard :refprobe "absent" :value 3) put-other]]
     [false [reset-a put-a (read-guard :refprobe "a" :old true)]]
     [true [reset-a (read-guard :refprobe "a" :old true) put-a]]
     [true [put-other put-a (read-guard :refother "a" :value 9) (g 3)]]
     [false [put-other put-a (read-guard :refother "a" :value 3)]]
     [true [[:put-docs :refprobe {:xt/id "a" :value "café"}] (g "café")]]
     [false [[:put-docs :refprobe {:xt/id "a" :value "café"}] (g "cafe\u0301")]]
     [true [[:put-docs :refprobe {:xt/id "a" :value false}] (g false)]]
     [false [[:put-docs :refprobe {:xt/id "a" :value false}] (g true)]]]))

(deftest independent-transaction-internal-reads
  (doseq [[expected ops] read-cases]
    (let [r (reference/execute initial ops)]
      (is (= :predicted (:status r)) (pr-str ops))
      (is (= expected (:committed? r)) (pr-str ops))
      (if expected
        (is (not (contains? r :failed-assertion-index)))
        (do (is (= initial (:state r)))
            (is (string? (first (nth ops (:failed-assertion-index r)))))))
      (is (= :none (:authority r)))
      (is (= :not-issued (:certificate r)))))
  (let [r (reference/execute initial [put-a (read-guard :refprobe "a" :value 3)])]
    (is (= replacement (get-in (:state r) [:refprobe "a"]))))
  (let [r (reference/execute initial [put-a (read-guard :refprobe "a" :value 1)])]
    (is (= 1 (:failed-assertion-index r)))))

(def ^:private card-table :gesso_refcard)
(def ^:private card-a [:put-docs card-table a])
(def ^:private card-b [:put-docs card-table b])
(def ^:private card-replacement [:put-docs card-table replacement])
(def ^:private card-c [:put-docs card-table {:xt/id "c" :value 4}])

(defn- table-count [n]
  [(str "ASSERT " n " = (SELECT COUNT(*) FROM gesso_refcard)")])

(defn- id-count [n id]
  [(str "ASSERT " n " = (SELECT COUNT(*) FROM gesso_refcard WHERE _id = ?)") id])

(defn- field-count [n id]
  ;; Real constructor/formatter only in the differential harness; the
  ;; independent interpreter must not call either to recognize the guard.
  (tx/format-assertion (tx/assert-document-field-count card-table id n)))

(def ^:private cardinality-cases
  ;; These run as a sequence, starting without the cardinality table.
  [[true [(table-count 0) (id-count 0 "a")]]
   [true [card-a (table-count 1) (id-count 1 "a") (field-count 3 "a")]]
   [true [card-b (table-count 2) (id-count 1 "b") (field-count 2 "b")]]
   [true [card-replacement (field-count 2 "a") (table-count 2)]]
   [false [card-a (field-count 2 "a") put-other]]
   [false [card-c (table-count 2) put-other]]
   [false [card-c (id-count 1 "c") (id-count 0 "c")]]
   [true [(id-count 0 "c") (table-count 2) (field-count 2 "a")]]
   [false [(id-count 0 "a") put-other]]
   [false [(id-count 1 "c")]]
   [false [(field-count 1 "c")]]
   [true [card-c ["ASSERT ? = (SELECT COUNT(*) FROM \"gesso_refcard\")" 3]
          (tx/format-assertion (tx/assert-document-exists card-table "c"))]]
   [false [(tx/format-assertion (tx/assert-document-absent card-table "c"))]]
   [true [(tx/format-assertion (tx/assert-document-absent card-table "absent"))
          ["ASSERT 1 = (SELECT COUNT(*) FROM \"refother\" WHERE _id = ?)" "a"]]]])

(deftest independent-document-cardinality
  (let [states (reductions (fn [state [expected ops]]
                             (let [r (reference/execute state ops)]
                               (is (= :predicted (:status r)) (pr-str ops))
                               (is (= expected (:committed? r)) (pr-str ops))
                               (if expected
                                 (is (not (contains? r :failed-assertion-index)))
                                 (do (is (= state (:state r)))
                                     (is (integer? (:failed-assertion-index r)))))
                               (is (= :none (:authority r)))
                               (is (= :not-issued (:certificate r)))
                               (:state r))) initial cardinality-cases)
        final (last states)]
    (is (= {"a" replacement "b" b "c" {:xt/id "c" :value 4}}
           (get final card-table)))
    (is (= (:refother initial) (:refother final)))
    (is (= 1 (:failed-assertion-index
              (reference/execute initial [card-a (field-count 2 "a")]))))))

(def ^:private mapped-table :gesso_refmapped)

(defn- mapped-put [doc]
  [:put-docs mapped-table (assoc doc :xt/id "a")])

(defn- mapped-guard [checks]
  (tx/format-assertion
   (tx/assert-document-decoded-current mapped-table "a" checks)))

(def ^:private mapped-cases
  [[true [(mapped-put {:value :open :active true})
          (mapped-guard [[:value :open] [:active true]])]]
   [false [(mapped-guard [[:value :closed]]) put-other]]
   [false [(mapped-put {:value :claim/status})
           (mapped-guard [[:value :other/status]])]]
   [true [(mapped-put {:value :claim/status})
          (mapped-guard [[:value :claim/status]])]]
   [false [(mapped-put {:value "open"}) (mapped-guard [[:value :open]])]]
   [false [(mapped-put {:value :open}) (mapped-guard [[:value "open"]])]]
   [true [(mapped-put {:value "café"}) (mapped-guard [[:value "café"]])]]
   [false [(mapped-guard [[:value "cafe\u0301"]])]]
   [true [(mapped-put {:value Long/MIN_VALUE :active false})
          (mapped-guard [[:value Long/MIN_VALUE] [:active false]])]]
   [false [(mapped-guard [[:value Long/MAX_VALUE]])]]
   [false [(mapped-put {:value 3}) (mapped-guard [[:active false]])]]
   [true [(mapped-put {:value :open}) (mapped-guard [[:value :open]])
          (tx/format-assertion (tx/assert-document-field-count mapped-table "a" 2))]]
   [false [(mapped-put {:value :open :active true}) (mapped-guard [[:value :open]])
           (tx/format-assertion (tx/assert-document-field-count mapped-table "a" 2))]]])

(deftest independent-mapped-scalar-guards
  (let [final (reduce (fn [state [expected ops]]
                        (let [r (reference/execute state ops)]
                          (is (= :predicted (:status r)) (pr-str ops))
                          (is (= expected (:committed? r)) (pr-str ops))
                          (if expected
                            (is (not (contains? r :failed-assertion-index)))
                            (is (= state (:state r))))
                          (:state r))) initial mapped-cases)]
    (is (= {:xt/id "a" :value :open} (get-in final [mapped-table "a"]))))
  (let [guard (mapped-guard [[:value :open]])]
    (doseq [ops [[(assoc guard 2 "other")]
                 [(assoc guard 5 :closed)]
                 [(assoc guard 1 0)]
                 [(pop guard)]
                 [(assoc guard 0 (str/replace (first guard) "STR(ARRAY[value])" "STR(value)"))]
                 [(mapped-put {:value true}) guard]
                 [["ASSERT FALSE"] (pop guard)]]]
      (let [r (reference/execute initial ops)]
        (is (= :unsupported (:status r)) (pr-str ops))
        (is (not (contains? r :state)))
        (is (not (contains? r :committed?)))))))

(def ^:private nested-table :gesso_refnested)
(def ^:private nested-doc
  {:xt/id "a"
   :gesso.command/binding {:principal {:gesso.choreo.identity/value uuid-a}
                           :operation :claim/request
                           :arguments {:nested {:value "original"}
                                       :items [1 :open "open" false uuid-b]}}
   :empty {} :items []})
(def ^:private nested-replacement
  {:xt/id "a" :gesso.command/binding {:operation :claim/request
                                      :arguments {:items [uuid-a]}}})
(def ^:private nested-cases
  [[true [[:put-docs nested-table nested-doc]]]
   [true [(tx/format-assertion (tx/assert-document-field-count nested-table "a" 4))]]
   [false [[:put-docs nested-table nested-replacement] ["ASSERT FALSE"]]]
   [true [[:put-docs nested-table nested-replacement]]]
   [false [[:put-docs nested-table nested-doc]
           (tx/format-assertion (tx/assert-document-field-count nested-table "a" 2))]]
   [true [[:put-docs nested-table nested-doc] [:put-docs nested-table nested-replacement]]]
   [true [[:put-docs nested-table {:xt/id "b" :value uuid-b}]]]
   [true [[:put-docs nested-table nested-doc]]]])

(deftest independent-nested-document-replacement
  (let [final (reduce (fn [state [expected ops]]
                        (let [r (reference/execute state ops)]
                          (is (= :predicted (:status r)) (pr-str ops))
                          (is (= expected (:committed? r)) (pr-str ops))
                          (when-not expected (is (= state (:state r))))
                          (:state r))) initial nested-cases)]
    (is (= {"a" nested-doc "b" {:xt/id "b" :value uuid-b}}
           (get final nested-table))))
  (let [r (reference/execute {nested-table {"a" nested-doc}}
                             [[:put-docs nested-table nested-replacement]])]
    (is (= nested-replacement (get-in (:state r) [nested-table "a"]))))
  (doseq [v [{:nested nil} {:nested [1 nil]} {:nested [[:open]]}
             {:nested #{:open}} {:xt/id "nested"} {:bad_key 1}
             {:nested (with-meta {:value 1} {:tag true})}
             (nth (iterate #(hash-map :nested %) 10) 10)
             (vec (repeat 257 1))]]
    (let [r (reference/execute initial [[:put-docs nested-table {:xt/id "a" :value v}]])]
      (is (= :unsupported (:status r)) (pr-str v))
      (is (not (contains? r :state)))
      (is (not (contains? r :committed?))))))

(defn- journal-binding [id principal operation]
  {:command-id (identity/command-id id) :principal (identity/principal principal)
   :operation operation :arguments {:request-id "r"}})

(def ^:private slot-binding (journal-binding "c1" "helper-a" :request/claim))
(def ^:private slot-row (resolution/command-record slot-binding {:status :claimed} "t1"))
(def ^:private slot-duplicate
  (resolution/command-record (journal-binding "c2" "helper-a" :request/claim)
                             {:status :claimed} "t2"))
(def ^:private slot-other-principal
  (resolution/command-record (journal-binding "c3" "helper-b" :request/claim)
                             {:status :claimed} "t2"))
(def ^:private slot-other-operation
  (resolution/command-record (journal-binding "c4" "helper-a" :request/release)
                             {:status :released} "t2"))
(def ^:private slot-empty (resolution/principal-operation-slot-assertion slot-binding))
(def ^:private slot-occupied
  (first (frame/formatted-prestate-assertions!
          {:target-map {:outcome [:journal-slot "helper-a" :request/claim]}}
          {} [slot-row])))
(defn- journal-put [row] [:put-docs :gesso_commands row])
(def ^:private slot-cases
  [[true [slot-empty]]
   [false [slot-occupied]]
   [true [(journal-put slot-other-principal) (journal-put slot-other-operation) slot-empty]]
   [true [(journal-put slot-row) slot-occupied]]
   [false [(journal-put slot-duplicate) slot-occupied put-other]]
   [false [slot-empty put-other]]
   [true [slot-occupied]]
   [true [(journal-put (assoc slot-other-principal :xt/id "c1")) slot-empty]]
   [false [slot-occupied]]
   [true [(journal-put slot-row) slot-occupied]]
   [true [(journal-put (assoc slot-row :gesso.command/outcome {:status :changed})) slot-occupied]]
   [false [(journal-put slot-duplicate) slot-empty]]
   [true [(journal-put slot-duplicate)]]
   [false [slot-occupied]]
   [false [slot-empty]]])

(deftest independent-journal-slot-selection
  (let [final (reduce (fn [state [expected ops]]
                        (let [r (reference/execute state ops)]
                          (is (= :predicted (:status r)) (pr-str ops))
                          (is (= expected (:committed? r)) (pr-str ops))
                          (when-not expected (is (= state (:state r))))
                          (:state r))) initial slot-cases)]
    (is (= #{"c1" "c2" "c3" "c4"} (set (keys (:gesso_commands final)))))
    (is (= (:refother initial) (:refother final))))
  (doseq [ops [[(assoc slot-empty 1 nil)] [(assoc slot-empty 2 "request/claim")]
               [(conj slot-empty :extra)] [(pop slot-empty)]
               [[(str/replace (first slot-empty) "ASSERT 0" "ASSERT 2") "helper-a" :request/claim]]
               [[(str/replace (first slot-empty) " AND " " OR ") "helper-a" :request/claim]]
               [["ASSERT FALSE"] (assoc slot-empty 2 1)]
               [(journal-put (assoc-in slot-row [:gesso.command/binding :operation] "request/claim")) slot-empty]
               [(journal-put (assoc-in slot-row [:gesso.command/binding :principal] "helper-a")) slot-empty]]]
    (let [r (reference/execute initial ops)]
      (is (= :unsupported (:status r)) (pr-str ops))
      (is (not (contains? r :state)))
      (is (not (contains? r :committed?))))))

(def ^:private recursive-binding (journal-binding "recursive" "helper-recursive" :request/claim))
(def ^:private recursive-row
  (resolution/command-record recursive-binding
                             {:status :claimed :details {:empty {} :owner uuid-a
                                                        :nested {:value "a"}}} "t1"))
(def ^:private recursive-guards
  (frame/formatted-prestate-assertions!
   {:target-map {:outcome [:journal-slot "helper-recursive" :request/claim]}}
   {} [recursive-row]))
(def ^:private recursive-value-guard (last recursive-guards))
(def ^:private recursive-cases
  (let [out :gesso.command/outcome
        restore (journal-put recursive-row)
        changed (fn [path value] (journal-put (assoc-in recursive-row (into [out] path) value)))]
    [[false recursive-guards]
     [true (into [restore] recursive-guards)]
     [false [(changed [:details :nested :value] "b") recursive-value-guard put-other]]
     [false [(changed [:details :owner] uuid-b) recursive-value-guard]]
     [false [(changed [:status] "claimed") recursive-value-guard]]
     [false [(changed [:details :empty] {:extra true}) recursive-value-guard]]
     [false [(changed [:details :nested] {:value "a" :extra true}) recursive-value-guard]]
     [false [(changed [:details :nested] {:other "a"}) recursive-value-guard]]
     [true recursive-guards]
     [true [restore recursive-value-guard]]]))

(deftest independent-recursive-journal-map-guards
  (let [final (reduce (fn [state [expected ops]]
                        (let [r (reference/execute state ops)]
                          (is (= :predicted (:status r)) (pr-str r))
                          (is (= expected (:committed? r)) (pr-str ops))
                          (when-not expected (is (= state (:state r))))
                          (:state r))) initial recursive-cases)]
    (is (= recursive-row (get-in final [:gesso_commands "recursive"]))))
  (doseq [guard [(pop recursive-value-guard)
                 (conj recursive-value-guard :extra)
                 (assoc recursive-value-guard 1 nil)
                 (assoc recursive-value-guard 2 "request/claim")
                 (update recursive-value-guard 0 str/replace " LIKE ?" " = ?")
                 (update recursive-value-guard 0 str/replace "STR(ARRAY[?])" "STR(?)")]]
    (let [r (reference/execute initial [["ASSERT FALSE"] guard])]
      (is (= :unsupported (:status r)))
      (is (not (contains? r :state)))
      (is (not (contains? r :committed?))))))

(def ^:private vector-binding (journal-binding "vectors" "helper-vectors" :request/claim))
(def ^:private vector-items [1 :claim/open "claim/open" false uuid-a])
(def ^:private vector-row
  (resolution/command-record vector-binding {:details {:items vector-items :empty []}} "t1"))
(def ^:private vector-guards
  (frame/formatted-prestate-assertions!
   {:target-map {:outcome [:journal-slot "helper-vectors" :request/claim]}}
   {} [vector-row]))
(def ^:private vector-guard (last vector-guards))
(def ^:private vector-cases
  (let [restore (journal-put vector-row)
        changed (fn [field v]
                  (journal-put (assoc-in vector-row [:gesso.command/outcome :details field] v)))]
    [[false vector-guards]
     [true (into [restore] vector-guards)]
     [false [(changed :items (assoc vector-items 0 2)) vector-guard put-other]]
     [false [(changed :items (assoc vector-items 1 "claim/open")) vector-guard]]
     [false [(changed :items (assoc vector-items 2 :claim/open)) vector-guard]]
     [false [(changed :items (assoc vector-items 3 true)) vector-guard]]
     [false [(changed :items (assoc vector-items 4 uuid-b)) vector-guard]]
     [false [(changed :items (pop vector-items)) vector-guard]]
     [false [(changed :items (conj vector-items 1)) vector-guard]]
     [false [(changed :items [1 "claim/open" :claim/open false uuid-a]) vector-guard]]
     [false [(changed :empty [1]) vector-guard]]
     [true vector-guards]
     [true [restore vector-guard]]]))

(deftest independent-journal-vector-guards
  (let [final (reduce (fn [state [expected ops]]
                        (let [r (reference/execute state ops)]
                          (is (= :predicted (:status r)) (pr-str r))
                          (is (= expected (:committed? r)) (pr-str ops))
                          (when-not expected (is (= state (:state r))))
                          (:state r))) initial vector-cases)]
    (is (= vector-row (get-in final [:gesso_commands "vectors"]))))
  (let [vector-index (first (keep-indexed #(when (= vector-items %2) %1) vector-guard))]
    (is (integer? vector-index))
    (doseq [guard [(update vector-guard 0 str/replace "[1]" "[0]")
                   (update vector-guard 0 str/replace "[1]" "[2]")
                   (assoc vector-guard (inc vector-index) 42)
                   (assoc vector-guard vector-index (assoc vector-items 0 2))
                   (assoc vector-guard vector-index [nil])
                   (assoc vector-guard vector-index [[1]])
                   (pop vector-guard)]]
      (let [r (reference/execute initial [["ASSERT FALSE"] guard])]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :state)))
        (is (not (contains? r :committed?)))))))

(deftest unsupported-reference-input-does-not-predict
  (doseq [ops [[] [(table-count -1)] [(table-count 16385)]
               [["ASSERT ? = (SELECT COUNT(*) FROM gesso_refcard)" nil]]
               [["ASSERT ? = (SELECT COUNT(*) FROM gesso_refcard)" 1.0]]
               [["ASSERT ? = (SELECT COUNT(*) FROM gesso_refcard)" 1 2]]
               [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_refcard)" "a"]]
               [(id-count 0 "")] [(id-count 0 nil)] [(id-count 0 1)]
               [(pop (id-count 0 "a"))]
               [["ASSERT 0 = (SELECT COUNT(*) FROM xt_txs)"]]
               [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_refcard WHERE value = ?)" 1]]
               [["ASSERT 0 = (SELECT LENGTH(NEST_ONE (SELECT * FROM gesso_refcard WHERE _id = ?)))" "a"]]
               [(assoc (field-count 2 "a") 1 0)]
               [(assoc (field-count 2 "a") 1 8193)]
               [["ASSERT FALSE"] (id-count 0 nil)]
               [(read-guard :refprobe "a" :value nil)]
               [(read-guard :refprobe "a" :value "1")]
               [(read-guard :refprobe "a" :value :open)]
               [(read-guard :xt_txs "a" :value 1)]
               [(read-guard :refprobe "a" :bad_field 1)]
               [(read-guard :refprobe "" :value 1)]
               [(pop (read-guard :refprobe "a" :value 1))]
               [["ASSERT FALSE"] (read-guard :refprobe "a" :value nil)]
               [["ASSERT ? = ?" 1]] [["ASSERT ? = ?" 1 1 1]]
               [["ASSERT ? = ?" 1 "1"]] [["ASSERT ? = ?" :open "open"]]
               [["ASSERT ? = ?" nil nil]] [["ASSERT ? = ?" 1.0 1.0]]
               [["ASSERT ? = ?" 1N 1N]] [["ASSERT ? = ?" [1] [1]]]
               [["ASSERT ? = ?" (apply str (repeat 4097 "a")) ""]]
               [["ASSERT ? = ?" (str (char 0xd800)) "?"]]
               [["ASSERT FALSE"] ["ASSERT ? = ?" 1 nil]]
               [["ASSERT ? = ?" 1 2] ["ASSERT ? = ?" [] []]]
               [["ASSERT FALSE"] ["ASSERT arbitrary"]]
               [[:delete-docs :refprobe "a"]]
               [[:put-docs {:into :refprobe :valid-from :now} a]]
               [[:put-docs :refprobe (assoc a :xt/valid-to 1)]]
               [[:put-docs :refprobe (assoc a :value nil)]]
               [[:put-docs :refprobe (assoc a :value 1N)]]
               [[:put-docs :refprobe (assoc a :value (str (char 0xd800)))]]
               [[:put-docs :refprobe (assoc a :value (keyword "bad space"))]]
               [[:put-docs :refprobe (assoc a :value {:nested nil})]]
               [[:put-docs :refprobe (assoc a :xt/id 1)]]
               [[:put-docs :refprobe a replacement]]
               [[:put-docs :refprobe]] [[:put-docs :xt_txs a]]
               [[:put-docs :refprobe (assoc a :alias_key 1)]]]]
    (let [r (reference/execute initial ops)]
      (is (= :unsupported (:status r)) (pr-str ops))
      (is (not (contains? r :committed?)))
      (is (not (contains? r :state)))))
  (doseq [state [seed {:refprobe {}} {:refprobe {"a" (assoc a :value nil)}} nil]]
    (is (= :unsupported-reference-prestate
           (:reason (reference/execute state [["ASSERT TRUE"]]))))))

(defn- provider-state [node]
  (into {} (keep (fn [table]
                   (let [rows (xt/q node (str "SELECT * FROM " (name table)))]
                     (when (seq rows)
                       (let [indexed (into {} (map (juxt :xt/id identity)) rows)]
                         (is (= (count rows) (count indexed)) "No duplicate visible document IDs")
                         [table indexed])))))
        [:refprobe :refother card-table mapped-table nested-table :gesso_commands]))

(deftest reference-predictions-match-selected-real-xtdb-executions
  (with-open [node (xtn/start-node {})]
    (let [ops [[:put-docs :refprobe a] [:put-docs :refother (assoc b :xt/id "a")]]
          start (reference/execute {} ops)
          _ (xt/execute-tx node ops)]
      (is (= initial (:state start) (provider-state node)))
      ;; Each reference prediction uses its OWN previous predicted state,
      ;; never a readback from the provider. Compare complete row maps.
      (loop [state (:state start)
             cases (into [[["ASSERT TRUE"]]
                    [put-a put-b put-other]
                    [[:put-docs :refprobe a] put-a]
                    [["ASSERT FALSE"] put-a]
                    [[:put-docs :refprobe {:xt/id "c" :value 4}] put-other ["ASSERT FALSE"]]
                    [[:put-docs :refprobe a b] ["ASSERT TRUE"]]
                    [put-a ["ASSERT FALSE"] [:put-docs :refother {:xt/id "z" :value 7}]]
                    [put-other ["ASSERT TRUE"] put-a]]
                         (concat
                          (map-indexed
                           (fn [i [left right _]]
                             ;; Distinct writes around the assertion expose
                             ;; wrong commits and incomplete rollback.
                             [[:put-docs :refprobe {:xt/id "a" :value (+ 100 i)}]
                              ["ASSERT ? = ?" left right]
                              [:put-docs :refother {:xt/id "a" :value (+ 200 i)}]])
                           equality-cases)
                          (map second read-cases)
                          (map second cardinality-cases)
                          (map second mapped-cases)
                          (map second nested-cases)
                          (map second slot-cases)
                          (map second recursive-cases)
                          (map second vector-cases)))]
        (when-let [ops (first cases)]
          (let [predicted (reference/execute state ops)
                id (:tx-id (xt/submit-tx node ops))
                _ (.awaitTxBlocking (db/primary-db node) id (Duration/ofSeconds 10))
                outcomes (xt/q node ["SELECT committed FROM xt.txs WHERE _id = ?" id])]
            (is (= :predicted (:status predicted)))
            (is (= [{:committed (:committed? predicted)}] outcomes) (pr-str ops))
            (is (= (:state predicted) (provider-state node)) (pr-str ops))
            (is (= :matched (:status (envelope/check-envelope! node id ops (:committed? predicted)))))
            (recur (:state predicted) (next cases))))))))

(deftest uuid-identities-do-not-extend-string-selectors
  (let [doc {:xt/id uuid-a :value 1}
        r (reference/execute {} [[:put-docs :refprobe doc]])]
    (is (= {:refprobe {uuid-a doc}} (:state r)))
    (doseq [op [(read-guard :refprobe "a" :value 1)
                ["ASSERT 0 = (SELECT COUNT(*) FROM \"refprobe\" WHERE _id = ?)" "a"]]]
      (let [result (reference/execute (:state r) [op])]
        (is (= :unsupported (:status result)))
        (is (not (contains? result :state)))))))

(deftest admitted-operation-table-dependencies
  (doseq [[op expected] [[put-a #{:refprobe}]
                         [["ASSERT TRUE"] #{}]
                         [["ASSERT FALSE"] #{}]
                         [["ASSERT ? = ?" 1 2] #{}]
                         [(read-guard :refother "a" :value 2) #{:refother}]
                         [(table-count 0) #{card-table}]
                         [(id-count 0 "a") #{card-table}]
                         [(field-count 2 "a") #{card-table}]
                         [(mapped-guard [[:value :open]]) #{mapped-table}]
                         [slot-empty #{:gesso_commands}]
                         [recursive-value-guard #{:gesso_commands}]
                         [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)" uuid-a] #{:gesso_live_outbox}]
                         [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" "source"] #{:gesso_live_outbox}]
                         [["ASSERT 0 = (SELECT COUNT(*) FROM \"probe\") -- comment"] nil]
                         [(pop (mapped-guard [[:value :open]])) nil]
                         [[:unknown] nil]]]
    (is (= expected (reference/operation-tables op)) (pr-str op))))

(deftest mapped-canonical-qualified-column-identities
  (let [doc {:xt/id "a" :updated-at "t0" :request/location-id "l1"
             :request.detail/last-kind :example/open :location-id "unqualified"}
        state {mapped-table {"a" doc}}
        checks (mapv #(vector % (get doc %)) [:updated-at :request/location-id :request.detail/last-kind :location-id])
        guard (mapped-guard checks)
        check! (fn [ops] (reference/execute state ops))]
    (is (str/includes? (first guard) "(updated_at = ?)"))
    (is (str/includes? (first guard) "(\"request$location_id\" = ?)"))
    (is (str/includes? (first guard) "(\"request$detail$last_kind\" = ?)"))
    (is (= #{mapped-table} (reference/operation-tables guard)))
    (is (true? (:committed? (check! [guard]))))
    (doseq [changed [(assoc doc :updated-at "t1")
                     (assoc doc :request/location-id "unqualified")
                     (assoc doc :location-id "l1")
                     (assoc doc :request.detail/last-kind :example/closed)
                     (dissoc doc :request/location-id)]]
      (let [r (check! [[:put-docs mapped-table changed] put-other guard])]
        (is (= :predicted (:status r)))
        (is (false? (:committed? r)))
        (is (= 2 (:failed-assertion-index r)))
        (is (= state (:state r)))))
    (doseq [bad [(update guard 0 str/replace "updated_at" "updated__at")
                 (update guard 0 str/replace "\"request$location_id\"" "request$location_id")
                 (update guard 0 str/replace "\"request$location_id\"" "\"request$$location_id\"")
                 (update guard 0 str/replace "\"request$location_id\"" "\"xt$location_id\"")
                 (update guard 0 str/replace "\"request$location_id\"" "\"xt$detail$location_id\"")
                 (update guard 0 str/replace "\"request$location_id\"" "location_id")
                 (update guard 0 str/replace-first "STR(\"request$location_id\")" "STR(location_id)")
                 (update guard 0 str/replace "\"request$location_id\"" "\"request$1\"")]]
      (is (nil? (reference/operation-tables bad)) (pr-str bad))
      (let [r (check! [["ASSERT FALSE"] bad])]
        (is (= :unsupported (:status r)))
        (is (not (contains? r :committed?)))))))

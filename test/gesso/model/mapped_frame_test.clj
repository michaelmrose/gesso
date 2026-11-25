(ns gesso.model.mapped-frame-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.certified-plan :as plan]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private mapping
  (plan/check-target-mapping!
   {:subject {:kind :operation :id :example/claim :interface {:operation :example/claim}}
    :state-types {:request/status {:type :keyword}
                  :assignment/helper {:type :string :nullable? true}}
    :input-types {} :guard {:readers #{} :expression [:literal true]}
    :reads #{} :writes #{:request/status :assignment/helper}
    :replacements {:request/status [:literal :request/claimed]
                   :assignment/helper [:literal "p1"]}
    :frame #{}}
   {:request/status [:document :requests "r1" :status]
    :assignment/helper [:nullable-document :assignments "a1" :helper]}))

(def ^:private before
  {:xt/id "r1" :status :request/open :revision 0
   :request/location-id "l1" :hidden "keep" :enabled false})
(def ^:private rows {:requests [before] :assignments []})

(defn- guards [docs]
  (mapv tx/format-assertion (frame/prestate-assertions! mapping docs)))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest complete-frame-lowering-is-exact-and-fails-closed
  (let [g (guards rows)]
    (is (= 3 (count g)))
    (is (= g (guards (update-in rows [:requests 0] #(into (sorted-map) %)))))
    (is (frame/require-prestate-assertions! mapping rows (conj g ["ASSERT TRUE"])))
    (doseq [missing g]
      (is (= :missing-frame-prestate-assertion
             (reason #(frame/require-prestate-assertions! mapping rows
                                                         (filterv (partial not= missing) g))))))
    (is (= :missing-frame-prestate-assertion
           (reason #(frame/require-prestate-assertions! mapping (assoc-in rows [:requests 0 :hidden] "different") g)))))
  (doseq [[docs expected]
          [[(assoc rows :requests []) :missing-required-frame-row]
           [(assoc rows :requests [before before]) :duplicate-frame-row]
           [(dissoc rows :assignments) :invalid-frame-row-collection]
           [(assoc-in rows [:requests 0 :hidden] nil) :unsupported-frame-field]
           [(assoc-in rows [:requests 0 :hidden] {:nested true}) :unsupported-frame-field]
           [(assoc-in rows [:requests 0 :request/location_id] "l1") :aliased-document-key]]]
    (is (= expected (reason #(guards docs))))))

(defn- execute-result [node ops]
  (try (xt/execute-tx node ops) :committed
       (catch Exception e
         (if (= :xtdb/assert-failed (:xtdb.error/code (ex-data e)))
           :assert-failed (throw e)))))

(deftest complete-frame-guards-refuse-intervening-physical-changes
  (with-open [node (xtn/start-node {})]
    (let [after (assoc before :status :request/claimed :revision 1)
          assignment {:xt/id "a1" :helper "p1"}
          candidate (into (guards rows) [[:put-docs :requests after]
                                         [:put-docs :assignments assignment]])
          request-row #(first (xt/q node ["SELECT * FROM requests WHERE _id = ?" "r1"]))
          assignment-row #(first (xt/q node ["SELECT * FROM assignments WHERE _id = ?" "a1"]))]
      (doseq [changed [(assoc before :hidden "intervening")
                       (assoc before :late "new")
                       (dissoc before :hidden)
                       (-> before (dissoc :hidden) (assoc :late "keep"))]]
        (xt/execute-tx node [[:put-docs :requests changed]])
        (is (= :assert-failed (execute-result node candidate)))
        (is (= changed (request-row)))
        (is (nil? (assignment-row))))
      (xt/execute-tx node [[:put-docs :requests before]
                           [:put-docs :assignments (assoc assignment :helper "other")]])
      (is (= :assert-failed (execute-result node candidate)))
      (is (= before (request-row)))
      (is (= "other" (:helper (assignment-row))))
      (xt/execute-tx node [[:delete-docs :assignments "a1"]])
      (is (= :committed (execute-result node candidate)))
      (is (= after (request-row)))
      (is (= assignment (assignment-row))))))

(deftest read-only-mapped-documents-are-guarded-before-any-write
  (let [mapping (plan/check-target-mapping!
                 {:subject {:kind :operation :id :example/change :interface {:operation :example/change}}
                  :state-types {:example/status {:type :keyword}
                                :example/active {:type :boolean}
                                :example/other {:type :string :nullable? true}}
                  :input-types {} :guard {:readers #{:example/active}
                                         :expression [:= [:read :example/active] [:literal true]]}
                  :reads #{:example/active} :writes #{:example/status}
                  :replacements {:example/status [:literal :claimed]}
                  :frame #{:example/active :example/other}}
                 {:example/status [:document :requests "r1" :status]
                  :example/active [:document :memberships "m1" :active]
                  :example/other [:nullable-document :assignments "a1" :helper]})
        request {:xt/id "r1" :status :open}
        member {:xt/id "m1" :active true :revision 0}
        docs {:requests [request] :memberships [member] :assignments []}
        prefix (mapv tx/format-assertion (frame/prestate-assertions! mapping docs))
        after (assoc request :status :claimed)
        candidate (conj prefix [:put-docs :requests after])]
    (is (= #{[:document :requests "r1"]} (:write-targets mapping)))
    (is (= 5 (count prefix)))
    (is (= :missing-required-frame-row
           (reason #(frame/prestate-assertions! mapping (assoc docs :memberships [])))))
    (with-open [node (xtn/start-node {})]
      (xt/execute-tx node [[:put-docs :requests request] [:put-docs :memberships member]])
      (doseq [changed [(assoc member :active false) (assoc member :revision 1)]]
        (xt/execute-tx node [[:put-docs :memberships changed]])
        (is (= :assert-failed (execute-result node candidate)))
        (is (= [request] (xt/q node ["SELECT * FROM requests"]))))
      (xt/execute-tx node [[:put-docs :memberships member]
                           [:put-docs :assignments {:xt/id "a1" :helper "p2"}]])
      (is (= :assert-failed (execute-result node candidate)))
      (is (= [request] (xt/q node ["SELECT * FROM requests"])))
      (xt/execute-tx node [[:delete-docs :assignments "a1"]])
      (is (= :committed (execute-result node candidate)))
      (is (= [after] (xt/q node ["SELECT * FROM requests"])))
      (is (= [member] (xt/q node ["SELECT * FROM memberships"]))))))

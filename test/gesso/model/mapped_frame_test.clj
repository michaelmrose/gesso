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

(ns gesso.model.mapped-projection-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-projection :as projection]))

(def ^:private source
  {:subject {:kind :operation :id :example/projection :interface {:operation :example/projection}}
   :state-types {:example/count {:type :integer}
                 :example/enabled {:type :boolean}
                 :example/owner {:type :string :nullable? true}
                 :example/one {:type :keyword :nullable? true}
                 :example/two {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {}
   :frame #{:example/count :example/enabled :example/owner :example/one :example/two}})

(def ^:private targets
  {:example/count [:document :requests "r1" :item_count]
   :example/enabled [:document :requests "r1" :enabled]
   :example/owner [:nullable-document :assignments "a1" :owner]
   :example/one [:journal-slot "p1" :example/command]
   :example/two [:journal-slot "p2" :example/command]})

(def ^:private rows {:requests [{:xt/id "r1" :item-count 2 :enabled false}]
                     :assignments []})

(defn- record [id principal]
  (resolution/command-record {:command-id (identity/command-id id)
                              :principal (identity/principal principal)
                              :operation :example/command :arguments {}}
                             {:result :accepted} "t1"))

(defn- project [docs journal]
  (projection/project-state! source targets docs journal #(get-in % [:gesso.command/outcome :result])))

(deftest mapped-projection-decodes-closed-row-sets
  (is (= {:example/count 2 :example/enabled false :example/owner nil
          :example/one nil :example/two nil} (project rows [])))
  (is (= {:example/count 2 :example/enabled false :example/owner "p1"
          :example/one :accepted :example/two nil}
         (project (assoc rows :assignments [{:xt/id "a1" :owner "p1"}]) [(record "c1" "p1")])))
  (is (nil? (:example/owner (project (assoc rows :assignments [{:xt/id "a1" :owner nil}]) []))))
  (is (= :accepted (:example/two (project rows [(record "c2" "p2")])))))

(deftest mapped-projection-refuses-ambiguous-incomplete-or-ill-typed-rows
  (let [a (record "c1" "p1")
        reason (fn [docs journal]
                 (try (project docs journal) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (doseq [[docs journal expected]
            [[(dissoc rows :assignments) [] :invalid-projection-input]
             [(assoc rows :extra []) [] :invalid-projection-input]
             [(assoc rows :requests []) [] :missing-required-document]
             [(assoc rows :requests (vec (repeat 2 (first (:requests rows))))) [] :duplicate-row]
             [(update rows :requests conj {:xt/id "r2"}) [] :unexpected-row]
             [(update-in rows [:requests 0] dissoc :enabled) [] :missing-mapped-field]
             [(assoc rows :assignments [{:xt/id "a1"}]) [] :missing-mapped-field]
             [(assoc-in rows [:requests 0 :enabled] nil) [] :ill-typed-present-value]
             [(assoc-in rows [:requests 0 :item-count] "2") [] :ill-typed-present-value]
             [rows [a (record "c2" "p1")] :duplicate-row]
             [rows [a (record "c1" "p2")] :duplicate-command-id]
             [rows [(record "c3" "p3")] :unexpected-row]]]
      (is (= expected (reason docs journal)) (pr-str expected)))
    (is (thrown? clojure.lang.ExceptionInfo (project rows [(assoc a :xt/id "wrong")])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (projection/project-state! source targets rows [a] (constantly nil))))
    (is (= :optional-field-unsupported
           (try (projection/project-state! (assoc-in source [:state-types :example/count :optional?] true)
                                           targets rows [] (constantly :accepted))
                nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

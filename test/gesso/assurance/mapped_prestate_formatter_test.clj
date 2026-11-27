(ns gesso.assurance.mapped-prestate-formatter-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.model.certified-plan :as plan]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.tx :as tx]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity]))

(def ^:private source
  {:subject {:kind :operation :id :fixture/guard :interface {:operation :fixture/guard}}
   :state-types {:fixture/status {:type :keyword}
                 :fixture/helper {:type :string :nullable? true}
                 :fixture/journal {:type :keyword :nullable? true}}
   :input-types {} :guard {:readers #{} :expression [:literal true]}
   :reads #{} :writes #{} :replacements {}
   :frame #{:fixture/status :fixture/helper :fixture/journal}})

(def ^:private targets
  {:fixture/status [:document :request_rows "r1" :status]
   :fixture/helper [:nullable-document :assignments "a1" :helper]
   :fixture/journal [:journal-slot "helper" :request/claim]})

(def ^:private docs
  {:request_rows [{:xt/id "r1" :status :request/open :item-count 2 :active false
                   :model/extra "quoted' value"}]
   :assignments []})

(defn- record []
  (resolution/command-record {:command-id (identity/command-id "c1")
                              :principal (identity/principal "helper")
                              :operation :request/claim :arguments {:request-id "r1"}}
                             {:outcome :request/claimed :nested {:empty {} :flags [true false]}} "t1"))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest independent-document-and-journal-sql-vectors
  (let [mapping (plan/check-target-mapping! source targets)
        disabled (fn [& _] (throw (ex-info "Shared formatter invoked" {})))
        check #(lowering/check-prefix! source targets %1 %2 %3)]
    (doseq [d [docs (assoc docs :assignments [{:xt/id "a1" :helper "helper"}])]
            journal [[] [(record)]]]
      (let [prefix (frame/formatted-prestate-assertions! mapping d journal)
            r (with-redefs [tx/format-assertion disabled
                            tx/assert-document-absent disabled
                            tx/assert-document-decoded-current disabled
                            tx/assert-document-field-count disabled
                            frame/formatted-prestate-assertions! disabled]
                (check d journal prefix))]
        (is (= :checked-exact-prefix (:status r)))
        (is (= :independent-exact-sql-vectors (:formatter-correspondence r)))
        (is (= prefix (into [] (mapcat :assertions) (:groups r))))
        (is (= 5 (:version r)))
        (is (= :not-established (:sql-refinement r)))
        (is (contains? (:open-obligations r) :identifier-normalization-refinement))
        (is (contains? (:open-obligations r) :transaction-internal-read-basis))))
    (let [prefix (frame/formatted-prestate-assertions! mapping docs [(record)])
          format-original tx/format-assertion
          ;; Corrupt the actual production formatter, not the independent
          ;; reference. The same corruption used to affect both paths.
          altered (with-redefs [tx/format-assertion
                                (fn [assertion]
                                  (let [v (format-original assertion)]
                                    (if (= "ASSERT ? = (SELECT LENGTH(NEST_ONE (SELECT * FROM request_rows WHERE _id = ?)))" (first v))
                                      (update v 1 dec) v)))]
                    (frame/formatted-prestate-assertions! mapping docs [(record)]))]
      (is (not= prefix altered))
      (is (= :noncanonical-lowering-prefix (reason #(check docs [(record)] altered))))
      (is (= :checked-exact-prefix (:status (check docs [(record)] prefix))))
      (doseq [bad [(assoc-in prefix [0 1] 1)
                   (update-in prefix [1 0] #(str/replace % "STR(ARRAY[_id])" "STR(_id)"))
                   (assoc-in prefix [1 2] "wrong")]]
        (is (= :noncanonical-lowering-prefix (reason #(check docs [(record)] bad))))))))

(ns gesso.model.mapped-guard-test
  (:require [clojure.test :refer [deftest is]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-guard :as guard]
            [gesso.model.tx :as tx]))

(def ^:private types {:state/status {:type :keyword}
                      :state/active {:type :boolean}
                      :state/owner {:type :string :nullable? true}
                      :state/journal {:type :keyword :nullable? true}})
(def ^:private fields (set (keys types)))
(def ^:private targets {:state/status [:document :requests "r" :status]
                        :state/active [:document :requests "r" :active_flag]
                        :state/owner [:nullable-document :assignments "a" :owner]
                        :state/journal [:journal-slot "p" :request/claim]})
(def ^:private command-binding {:command-id (identity/command-id "c")
                         :principal (identity/principal "p")
                         :operation :request/claim :arguments {}})
(defn- eq [field value] [:= [:read field] [:literal value]])
(defn- source [expr readers]
  {:subject {:kind :operation :id :example/claim :interface {:operation :example/claim}}
   :state-types types :input-types {} :guard {:readers readers :expression expr}
   :reads readers :writes #{} :replacements {} :frame fields})
(def ^:private terms [(eq :state/status :request/open) (eq :state/active false)
                      (eq :state/owner nil) (eq :state/journal nil)])
(def ^:private original (source (into [:and] terms) fields))
(def ^:private layout {[:requests "r"] [[:state :state/status] [:extra :updated-at "t0"] [:state :state/active]]})
(defn- lower [s l] (guard/lower-assertions! s targets command-binding l))
(defn- reason [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest literal-lowering-preserves-bound-data-and-strengthens-absence
  (let [assertions (lower original layout)
        current (tx/format-assertion (tx/assert-document-current :requests
                      {:model/id "r" :model/checks [[:status :request/open] [:updated-at "t0"] [:active_flag false]]}))]
    (is (= 4 (count assertions)))
    (is (= #{current (tx/format-assertion (tx/assert-document-absent :assignments "a"))
             (tx/format-assertion (tx/assert-document-absent :gesso_commands "c"))
             (resolution/principal-operation-slot-assertion command-binding)} (set assertions)))
    (is (= [1 "r" :request/open "t0" false] (vec (rest current))))
    (is (= assertions (lower (source (into [:and] (reverse terms)) fields) layout)))
    (is (not= assertions (lower (source (assoc (into [:and] terms) 1 (eq :state/status :request/closed)) fields) layout)))))

(deftest restricted-lowering-refuses-unsupported-or-incomplete-guards
  (is (= :incomplete-document-layout (reason #(lower original {}))))
  (is (= :incomplete-document-layout (reason #(lower original (update layout [:requests "r"] pop)))))
  (is (= :incomplete-document-layout (reason #(lower original (update layout [:requests "r"] conj [:state :state/active])))))
  (is (= :invalid-extra-check (reason #(lower original (update layout [:requests "r"] conj [:extra :active-flag true])))))
  (is (= :invalid-extra-check (reason #(lower original (update layout [:requests "r"] conj [:extra :xt/id "r"])))))
  (is (= :unsupported-guard-term (reason #(lower (source (into [:or] terms) fields) layout))))
  (is (= :duplicate-or-empty-guard (reason #(lower (source (into [:and] (conj terms (first terms))) fields) layout))))
  (is (= :unsupported-nullable-document-guard (reason #(lower (source (into [:and] (assoc terms 2 (eq :state/owner "p"))) fields) layout))))
  (is (= :unsupported-journal-guard (reason #(lower (source (into [:and] (assoc terms 3 (eq :state/journal :accepted))) fields) layout))))
  (is (= :unsupported-journal-guard (reason #(guard/lower-assertions! original targets
                                           (assoc command-binding :principal (identity/principal "other")) layout)))))

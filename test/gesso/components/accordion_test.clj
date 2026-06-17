(ns gesso.components.accordion-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [gesso.components.accordion.core :as accordion]
   [gesso.components.accordion.scripts :as scripts]))

;; -----------------------------------------------------------------------------
;; Hiccup helpers
;; -----------------------------------------------------------------------------

(defn- hiccup-node?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defn- node-attrs
  [node]
  (if (and (hiccup-node? node)
           (map? (second node)))
    (second node)
    {}))

(defn- node-children
  [node]
  (if (and (hiccup-node? node)
           (map? (second node)))
    (nnext node)
    (next node)))

(defn- all-nodes
  [x]
  (lazy-seq
   (cond
     (hiccup-node? x)
     (cons x (mapcat all-nodes (node-children x)))

     (sequential? x)
     (mapcat all-nodes x)

     :else
     nil)))

(defn- nodes-by-tag
  [tree tag]
  (filter #(= tag (first %)) (all-nodes tree)))

(defn- first-node-by-tag
  [tree tag]
  (first (nodes-by-tag tree tag)))

(defn- script-attr
  [node]
  (let [attrs (node-attrs node)]
    (or (get attrs :_)
        (get attrs "_"))))

(defn- rendered-details
  [tree]
  (vec (nodes-by-tag tree :details)))

(defn- rendered-summaries
  [tree]
  (vec (nodes-by-tag tree :summary)))

(defn- rendered-root
  [tree]
  (first-node-by-tag tree :div))

(defn- test-items
  []
  [{:value "one"
    :title "One"
    :content [:p "One body"]
    :content-opts {}}
   {:value "two"
    :title "Two"
    :content [:p "Two body"]
    :content-opts {}}])

;; -----------------------------------------------------------------------------
;; Script generation
;; -----------------------------------------------------------------------------

(deftest single-accordion-script-keeps-single-open-and-chevron-behavior
  (let [script (scripts/accordion-script
                {:type :single
                 :collapsible? true})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "closest <div[data-accordion-root]/>"))
    (is (str/includes? script "for d in <details/> in root"))
    (is (str/includes? script "set d.open to false"))
    (is (str/includes? script "svg[data-accordion-chevron]"))
    (is (str/includes? script "rotate(180deg)"))
    (is (str/includes? script "rotate(0deg)"))))

(deftest accordion-script-does-not-own-submitted-open-state
  (let [script (scripts/accordion-script
                {:type :single
                 :collapsible? true
                 :state-input "#board-state input[name=selected]"
                 :state-name "selected"})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "svg[data-accordion-chevron]"))

    (testing "server/form selected-state plumbing is not accordion behavior"
      (is (not (str/includes? script "stateInput")))
      (is (not (str/includes? script "me.dataset.accordionValue")))
      (is (not (str/includes? script "on submit from <form/> in me")))
      (is (not (str/includes? script "form.appendChild(input)")))
      (is (not (str/includes? script "input[name='selected']")))
      (is (not (str/includes? script "first <#board-state input[name=selected]/> in document"))))))

(deftest multiple-accordion-script-keeps-chevron-behavior
  (let [script (scripts/accordion-script
                {:type :multiple
                 :state-input "#board-state input[name=selected]"
                 :state-name "selected"})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "svg[data-accordion-chevron]"))
    (is (str/includes? script "rotate(180deg)"))
    (is (str/includes? script "rotate(0deg)"))

    (testing "multiple accordions do not own hidden selected-state either"
      (is (not (str/includes? script "stateInput")))
      (is (not (str/includes? script "on submit from <form/> in me")))
      (is (not (str/includes? script "form.appendChild(input)"))))))

;; -----------------------------------------------------------------------------
;; Map-form rendering
;; -----------------------------------------------------------------------------

(deftest map-form-renders-default-open-item
  (let [tree    (accordion/accordion
                 {:type :single
                  :default-value "two"
                  :items (test-items)})
        details (rendered-details tree)]
    (is (= 2 (count details)))
    (is (= ["one" "two"]
           (mapv #(get (node-attrs %) :data-accordion-value) details)))
    (is (= [false true]
           (mapv #(true? (get (node-attrs %) :open)) details)))))

(deftest map-form-renders-root-and-details-contract
  (let [tree    (accordion/accordion
                 {:type :single
                  :items (test-items)})
        root    (rendered-root tree)
        details (rendered-details tree)]
    (is root)
    (is (contains? (node-attrs root) :data-accordion-root))
    (is (= 2 (count details)))
    (is (every? #(= :details (first %)) details))
    (is (= ["one" "two"]
           (mapv #(get (node-attrs %) :data-accordion-value) details)))))

(deftest map-form-attaches-behavior-script-to-each-item
  (let [tree    (accordion/accordion
                 {:type :single
                  :default-value "two"
                  :items (test-items)})
        details (rendered-details tree)
        scripts (mapv script-attr details)]
    (is (= 2 (count details)))
    (is (every? some? scripts))
    (is (every? #(str/includes? % "on toggle") scripts))
    (is (every? #(str/includes? % "closest <div[data-accordion-root]/>") scripts))
    (is (every? #(str/includes? % "svg[data-accordion-chevron]") scripts))

    (testing "accordion behavior scripts do not inject form state"
      (is (not-any? #(str/includes? % "stateInput") scripts))
      (is (not-any? #(str/includes? % "on submit from <form/> in me") scripts))
      (is (not-any? #(str/includes? % "form.appendChild(input)") scripts)))))

(deftest map-form-state-include-appends-root-hx-include
  (let [tree (accordion/accordion
              {:type :single
               :state-input "#board-state input[name=selected]"
               :state-name "selected"
               :state-include? true
               :attrs {:id "test-accordion"
                       :hx-include "#other-state"
                       :data-extra "extra"}
               :items (test-items)})
        root (rendered-root tree)
        a    (node-attrs root)]
    (is (= "test-accordion" (:id a)))
    (is (= "#other-state, #board-state input[name=selected]"
           (:hx-include a)))
    (is (= "extra" (:data-extra a)))))

(deftest map-form-state-include-adds-root-hx-include-when-absent
  (let [tree (accordion/accordion
              {:type :single
               :state-input "#board-state input[name=selected]"
               :state-name "selected"
               :state-include? true
               :items (test-items)})
        root (rendered-root tree)]
    (is (= "#board-state input[name=selected]"
           (get (node-attrs root) :hx-include)))))

(deftest map-form-does-not-add-hx-include-unless-requested
  (let [tree (accordion/accordion
              {:type :single
               :state-input "#board-state input[name=selected]"
               :state-name "selected"
               :items (test-items)})
        root (rendered-root tree)]
    (is (nil? (get (node-attrs root) :hx-include)))))

;; -----------------------------------------------------------------------------
;; Long-form rendering
;; -----------------------------------------------------------------------------

(deftest long-form-renders-details-children
  (let [tree    (accordion/accordion
                 {:type :single}
                 (accordion/accordion-item
                  {:value "one"}
                  [:summary "One"]
                  [:section "One body"])
                 (accordion/accordion-item
                  {:value "two"}
                  [:summary "Two"]
                  [:section "Two body"]))
        details (rendered-details tree)]
    (is (= 2 (count details)))
    (is (= ["one" "two"]
           (mapv #(get (node-attrs %) :data-accordion-value) details)))))

(deftest long-form-attaches-behavior-script-to-details-children
  (let [tree    (accordion/accordion
                 {:type :single}
                 (accordion/accordion-item
                  {:value "one"}
                  [:summary "One"]
                  [:section "One body"])
                 (accordion/accordion-item
                  {:value "two"}
                  [:summary "Two"]
                  [:section "Two body"]))
        details (rendered-details tree)
        scripts (mapv script-attr details)]
    (is (= 2 (count details)))
    (is (every? some? scripts))
    (is (every? #(str/includes? % "on toggle") scripts))
    (is (every? #(str/includes? % "closest <div[data-accordion-root]/>") scripts))
    (is (every? #(str/includes? % "svg[data-accordion-chevron]") scripts))

    (testing "long-form accordion items do not inject submitted selected state"
      (is (not-any? #(str/includes? % "stateInput") scripts))
      (is (not-any? #(str/includes? % "on submit from <form/> in me") scripts))
      (is (not-any? #(str/includes? % "form.appendChild(input)") scripts)))))

(deftest long-form-state-include-adds-root-hx-include
  (let [tree (accordion/accordion
              {:type :single
               :state-input "#board-state input[name=selected]"
               :state-name "selected"
               :state-include? true}
              (accordion/accordion-item
               {:value "one"}
               [:summary "One"]
               [:section "One body"]))
        root (rendered-root tree)]
    (is (= "#board-state input[name=selected]"
           (get (node-attrs root) :hx-include)))))

(deftest long-form-preserves-summary-and-content
  (let [tree (accordion/accordion
              {:type :single}
              (accordion/accordion-item
               {:value "one"}
               [:summary "One"]
               [:section "One body"])
              (accordion/accordion-item
               {:value "two"}
               [:summary "Two"]
               [:section "Two body"]))]
    (is (= 2 (count (rendered-summaries tree))))
    (is (some #(= [:summary "One"] %) (all-nodes tree)))
    (is (some #(= [:summary "Two"] %) (all-nodes tree)))
    (is (some #(= [:section "One body"] %) (all-nodes tree)))
    (is (some #(= [:section "Two body"] %) (all-nodes tree)))))

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

(deftest single-accordion-script-syncs-state-on-toggle
  (let [script (scripts/accordion-script
                {:type :single
                 :collapsible? true
                 :state-input "#board-state input[name=selected]"})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "closest <div[data-accordion-root]/>"))
    (is (str/includes? script "first <#board-state input[name=selected]/> in document"))
    (is (str/includes? script "me.dataset.accordionValue"))
    (is (str/includes? script "stateInput.value"))
    (is (str/includes? script "stateInput.value == value"))))

(deftest single-accordion-script-syncs-descendant-form-submit
  (let [script (scripts/accordion-script
                {:type :single
                 :collapsible? true
                 :state-input "#board-state input[name=selected]"
                 :state-name "selected"})]
    (is (str/includes? script "on submit from <form/> in me"))
    (is (str/includes? script "set form to event.target"))
    (is (str/includes? script "me.dataset.accordionValue"))
    (is (str/includes? script "first <input[name='selected']/> in form"))
    (is (str/includes? script "make an <input/> called input"))
    (is (str/includes? script "input.type"))
    (is (str/includes? script "input.name"))
    (is (str/includes? script "form.appendChild(input)"))
    (is (str/includes? script "input.value"))))

(deftest accordion-script-without-state-options-keeps-existing-behavior-only
  (let [script (scripts/accordion-script
                {:type :single
                 :collapsible? true})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "closest <div[data-accordion-root]/>"))
    (is (str/includes? script "svg[data-accordion-chevron]"))
    (is (not (str/includes? script "stateInput")))
    (is (not (str/includes? script "on submit from <form/> in me")))
    (is (not (str/includes? script "form.appendChild(input)")))))

(deftest multiple-accordion-does-not-enable-single-state-sync-yet
  (let [script (scripts/accordion-script
                {:type :multiple
                 :state-input "#board-state input[name=selected]"
                 :state-name "selected"})]
    (is (str/includes? script "on toggle"))
    (is (str/includes? script "svg[data-accordion-chevron]"))
    (is (not (str/includes? script "stateInput")))
    (is (not (str/includes? script "on submit from <form/> in me")))))

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

(deftest map-form-attaches-state-script-to-each-item
  (let [tree    (accordion/accordion
                 {:type :single
                  :default-value "two"
                  :state-input "#board-state input[name=selected]"
                  :state-name "selected"
                  :items (test-items)})
        details (rendered-details tree)
        scripts (mapv script-attr details)]
    (is (= 2 (count details)))
    (is (every? some? scripts))
    (is (every? #(str/includes? % "first <#board-state input[name=selected]/> in document")
                scripts))
    (is (every? #(str/includes? % "on submit from <form/> in me")
                scripts))
    (is (every? #(str/includes? % "first <input[name='selected']/> in form")
                scripts))))

(deftest map-form-state-include-appends-root-hx-include
  (let [tree (accordion/accordion
              {:type :single
               :state-input "#board-state input[name=selected]"
               :state-name "selected"
               :state-include? true
               :attrs {:hx-include "#other-state"}
               :items (test-items)})
        root (rendered-root tree)]
    (is (= "#other-state, #board-state input[name=selected]"
           (get (node-attrs root) :hx-include)))))

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

(deftest long-form-attaches-state-script-to-details-children
  (let [tree    (accordion/accordion
                 {:type :single
                  :state-input "#board-state input[name=selected]"
                  :state-name "selected"}
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
    (is (= ["one" "two"]
           (mapv #(get (node-attrs %) :data-accordion-value) details)))
    (is (every? some? scripts))
    (is (every? #(str/includes? % "first <#board-state input[name=selected]/> in document")
                scripts))
    (is (every? #(str/includes? % "on submit from <form/> in me")
                scripts))))

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

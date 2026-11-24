(ns gesso.components.bars-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.components.bars.core :as bars]
            [gesso.components.dropdown-menu.core :as dropdown]
            [rum.core :as rum]))

(defn- elements [tree]
  (filter #(and (vector? %) (keyword? (first %)) (map? (second %)))
          (tree-seq sequential? seq tree)))

(defn- items [tree attr]
  (filter #(get (second %) attr) (elements tree)))

(deftest native-dropdown-navigation-test
  (let [href "/settings?q=one&next=\"two\""
        short (dropdown/dropdown-menu-item {:text "Settings" :href href})
        long (dropdown/dropdown-menu-item {:href href :attrs {:href "/other" :target "_blank"}}
                                          [:span "Settings"])
        disabled (dropdown/dropdown-menu-item {:text "Unavailable" :href href :disabled? true
                                               :attrs {:href "/other" "href" "/other"}})
        action (dropdown/dropdown-menu-item {:text "Save" :attrs {:hx-post "/save"}})]
    (doseq [node [short long]]
      (is (= :a (first node)))
      (is (= href (get-in node [1 :href])))
      (is (= "menuitem" (get-in node [1 :role])))
      (is (not (contains? (second node) :type)))
      (is (str/includes? (get-in node [1 :_]) "dropdownOpen"))
      (is (not (contains? (second node) :onclick))))
    (is (= "_blank" (get-in long [1 :target])))
    (is (= :button (first disabled)))
    (is (true? (get-in disabled [1 :disabled])))
    (is (not-any? #(contains? (second disabled) %) [:href "href"]))
    (is (= :button (first action)))
    (is (= "button" (get-in action [1 :type])))
    (is (= "/save" (get-in action [1 :hx-post])))
    (is (str/includes? (rum/render-static-markup short) "&amp;next=&quot;two&quot;"))))

(deftest bars-navigation-uses-native-links-in-every-layout-test
  (let [tree (bars/bars {:menus [{:id "home" :home-region :leftmost
                                 :items [{:text "Home" :href "/home" :current? true}]}
                                {:id "account" :label "Account" :home-region :rightmost
                                 :items [{:text "Profile" :href "/profile" :current? true}
                                         {:text "Disabled" :href "/disabled" :disabled? true}
                                         {:text "Save" :attrs {:hx-post "/save"}}]}
                                {:id "sidebar" :home-region :sidebar
                                 :items [{:text "Help" :href "/help"}]}]})
        nodes (elements tree)
        dropdown-items (items tree :data-dropdown-item)
        ordinary-items (items tree :data-bars-menu-item)]
    (is (= 3 (count dropdown-items)))
    (is (seq ordinary-items))
    (is (every? #(not (contains? (second %) :onclick)) nodes))
    (doseq [href ["/home" "/profile" "/help"]]
      (is (some #(and (= :a (first %)) (= href (get-in % [1 :href]))) nodes)))
    (is (some #(and (= :a (first %)) (= "/profile" (get-in % [1 :href]))
                    (= "page" (get-in % [1 :aria-current]))) dropdown-items))
    (is (some #(and (= :button (first %)) (true? (get-in % [1 :disabled]))) dropdown-items))
    (is (not-any? #(= "/disabled" (get-in % [1 :href])) nodes))
    (is (some #(and (= :button (first %)) (= "/save" (get-in % [1 :hx-post]))) dropdown-items))
    (is (not (str/includes? (rum/render-static-markup tree) "onclick=")))))

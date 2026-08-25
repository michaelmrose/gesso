(ns gesso.live.browser.continuity-test
  (:require
   [clojure.string :as str]
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.dom :as dom]))

;; =============================================================================
;; DOM/test helpers
;; =============================================================================

(defn- element
  ([tag]
   (element tag nil))
  ([tag attrs]
   (let [node (.createElement js/document tag)]
     (doseq [[attribute value] attrs]
       (.setAttribute node (dom/attr-name attribute) (str value)))
     node)))

(defn- append!
  [parent child]
  (.appendChild parent child)
  child)

(defn- text!
  [node value]
  (set! (.-textContent node) value)
  node)

(defn- config-json
  [config]
  (.stringify js/JSON (clj->js config)))

(defn- sandbox
  []
  (let [root (element "div"
                      {:data-gesso-continuity-test
                       (str "test-" (random-uuid))})]
    (.appendChild (.-body js/document) root)
    root))

(defn- with-sandbox*
  [f]
  (let [root (sandbox)]
    (try
      (f root)
      (finally
        (.remove root)))))

(defn- runtime
  ([]
   (continuity/create))
  ([options]
   (continuity/create options)))

(defn- immediate-runtime
  ([]
   (immediate-runtime nil))
  ([options]
   (runtime
    (merge {:request-animation-frame! (fn [f] (f) 1)}
           options))))

(defn- continuity-tree
  ([sandbox-root config]
   (continuity-tree sandbox-root "continuity-target" config))
  ([sandbox-root target-id config]
   (let [root (append!
               sandbox-root
               (element
                "section"
                {continuity/continuity-attr "true"
                 continuity/continuity-fragment-attr-key target-id
                 continuity/continuity-config-attr-key (config-json config)}))
         target (append!
                 root
                 (element "div" {:id target-id}))]
     {:root root
      :target target})))

(defn- replacement-target
  [id]
  (element "div" {:id id}))

(defn- replace-target!
  [old-target new-target]
  (.replaceWith old-target new-target)
  new-target)

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-kind
  [f]
  (some-> (thrown f) ex-data :error/kind))

(defn- promise->done!
  [promise done assertions]
  (.then promise
         (fn [value]
           (try
             (assertions value)
             (done)
             (catch :default error
               (is false (str "Unexpected assertion error: " error))
               (done))))
         (fn [error]
           (is false (str "Promise rejected: " error))
           (done))))

;; =============================================================================
;; Runtime identity and validation
;; =============================================================================

(deftest runtime-and-wire-identity-test
  (let [rt (runtime)]
    (is (= 3 continuity/runtime-version))
    (is (= :gesso.live.browser.continuity/runtime
           continuity/runtime-type))
    (is (= :gesso.live.browser.continuity/resource
           continuity/resource-type))
    (is (= :data-gesso-live-continuity
           continuity/continuity-attr))
    (is (= "[data-gesso-live-continuity='true']"
           continuity/continuity-root-selector))
    (is (= "data-gesso-live-continuity-config"
           continuity/continuity-config-attr))
    (is (= "data-gesso-live-continuity-fragment"
           continuity/continuity-fragment-attr))
    (is (continuity/runtime? rt))
    (is (= continuity/runtime-version (:version rt)))))

(deftest create-validates-options-test
  (is (= :invalid-map
         (thrown-kind #(continuity/create []))))
  (is (= :unknown-options
         (thrown-kind #(continuity/create {:invented true}))))
  (is (= :invalid-callable
         (thrown-kind #(continuity/create {:on-diagnostic :no}))))
  (is (= :invalid-callable
         (thrown-kind #(continuity/create {:now-ms :no}))))
  (is (= :invalid-callable
         (thrown-kind #(continuity/create {:request-animation-frame! :no}))))
  (is (= :invalid-map
         (thrown-kind #(continuity/create {:boxes []})))))

(deftest runtime-registry-is-per-runtime-test
  (let [a (runtime)
        b (runtime)]
    (continuity/register-box!
     a
     :application-only
     {:capture (fn [_runtime _root _target _box] 1)
      :restore (fn [_runtime _root _target _box _state] true)})
    (is (some #{"application-only"}
              (continuity/registered-box-types a)))
    (is (not (some #{"application-only"}
                   (continuity/registered-box-types b))))))

(deftest built-in-boxes-are-installed-per-runtime-test
  (let [types (set (continuity/registered-box-types (runtime)))]
    (is (= #{"anchor-scroll"
             "details-open"
             "event"
             "focus"
             "hyperscript"
             "inputs"
             "js"
             "raw-scroll"}
           types))))

(deftest handlers-close-over-one-runtime-test
  (let [rt (runtime)
        handlers (continuity/handlers rt)]
    (is (= #{:continuity-capture!
             :continuity-restore!
             :continuity-release!}
           (set (keys handlers))))
    (is (every? fn? (vals handlers)))))

(deftest diagnostics-contains-no-captured-resources-test
  (let [rt (runtime)
        data (continuity/diagnostics rt)]
    (is (= continuity/runtime-type
           (:gesso.live.browser.continuity/type data)))
    (is (= continuity/runtime-version (:version data)))
    (is (vector? (:registered-box-types data)))
    (is (not (contains? data :root)))
    (is (not (contains? data :resource)))
    (is (not (contains? data :slots)))))

;; =============================================================================
;; Generic helpers / configuration
;; =============================================================================

(deftest js-function-and-node-containment-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [child (append! sandbox-root (element "span"))
            outsider (element "span")]
        (is (continuity/js-function? (fn [])))
        (is (false? (continuity/js-function? {})))
        (is (continuity/contains-node? sandbox-root sandbox-root))
        (is (continuity/contains-node? sandbox-root child))
        (is (false? (continuity/contains-node? sandbox-root outsider)))
        (is (false? (continuity/contains-node? nil child)))))))

(deftest parse-json-and-malformed-json-test
  (with-sandbox*
    (fn [root]
      (let [events (atom [])
            diagnostics (atom [])
            rt (runtime {:on-diagnostic #(swap! diagnostics conj %)})]
        (.addEventListener
         root
         "gesso:live-continuity:error"
         (fn [event]
           (swap! events conj (.-detail event))))
        (is (= {:enabled true
                :preserve {:inputs true}}
               (continuity/parse-json
                rt root
                "{\"enabled\":true,\"preserve\":{\"inputs\":true}}"
                "test")))
        (is (nil? (continuity/parse-json rt root "" "test")))
        (is (nil? (continuity/parse-json rt root nil "test")))
        (is (nil? (continuity/parse-json rt root "{bad json" "attr")))
        (is (= 1 (count @events)))
        (is (= "parse-config" (aget (first @events) "phase")))
        (is (contains? (set (map :kind @diagnostics))
                       :invalid-config-json))))))

(deftest emit-prefixes-dom-event-and-diagnostics-test
  (with-sandbox*
    (fn [root]
      (let [seen (atom nil)
            diagnostics (atom [])
            rt (runtime {:on-diagnostic #(swap! diagnostics conj %)})
            detail #js {:value 42}]
        (.addEventListener
         root
         "gesso:live-continuity:captured"
         (fn [event]
           (reset! seen (.-detail event))))
        (is (identical? detail
                        (continuity/emit! rt root "captured" detail)))
        (is (identical? detail @seen))
        (is (= :dom-event (:kind (last @diagnostics))))
        (is (= "captured" (:name (last @diagnostics))))))))

(deftest after-layout-uses-two-animation-frames-test
  (async done
    (let [frames (atom [])
          called (atom 0)
          rt (runtime {:request-animation-frame!
                       (fn [f]
                         (swap! frames conj f)
                         (count @frames))})
          promise (continuity/after-layout!
                   rt
                   (fn []
                     (swap! called inc)
                     :complete))]
      (is (= 1 (count @frames)))
      (is (= 0 @called))
      ((first @frames))
      (is (= 2 (count @frames)))
      (is (= 0 @called))
      ((second @frames))
      (promise->done!
       promise done
       (fn [value]
         (is (= :complete value))
         (is (= 1 @called)))))))

(deftest parse-config-prefers-attribute-and-falls-back-to-script-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [rt (runtime)
            {:keys [root]} (continuity-tree
                            sandbox-root
                            {:enabled true
                             :preserve {:inputs true}})
            script (append!
                    root
                    (element "script"
                             {:type "application/json"
                              continuity/continuity-config-attr-key ""}))]
        (set! (.-textContent script)
              (config-json {:enabled false}))
        (is (= {:enabled true
                :preserve {:inputs true}}
               (continuity/parse-config rt root)))
        (.removeAttribute root continuity/continuity-config-attr)
        (is (= {:enabled false}
               (continuity/parse-config rt root)))))))

(deftest enabled-config-semantics-test
  (is (not (continuity/enabled? nil)))
  (is (false? (continuity/enabled? {:enabled false})))
  (is (continuity/enabled? {}))
  (is (continuity/enabled? {:enabled true})))

;; =============================================================================
;; Stable root / target / element identity
;; =============================================================================

(deftest root-and-target-resolution-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [{:keys [root target]}
            (continuity-tree sandbox-root {:enabled true})
            nested (append! target (element "span"))]
        (is (identical? root (continuity/root root)))
        (is (identical? root (continuity/root target)))
        (is (identical? root (continuity/root nested)))
        (is (= "continuity-target" (continuity/target-id root)))
        (is (identical? target (continuity/target root)))))))

(deftest target-id-falls-back-to-simple-hx-target-test
  (let [root (element "section" {continuity/continuity-attr "true"
                                  :hx-target "#fragment-7"})]
    (is (= "fragment-7" (continuity/target-id root)))
    (.setAttribute root "hx-target" "closest div")
    (is (nil? (continuity/target-id root)))))

(deftest target-prefers-descendant-before-global-element-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [outside (append! sandbox-root (element "div" {:id "same-id"}))
            root (append! sandbox-root
                          (element "section"
                                   {continuity/continuity-attr "true"
                                    continuity/continuity-fragment-attr-key "same-id"}))
            inside (append! root (element "div" {:id "same-id"}))]
        (is (identical? inside (continuity/target root)))
        (is (not (identical? outside (continuity/target root))))))))

(deftest key-for-element-precedence-and-fallbacks-test
  (let [node (element "input"
                      {:id "id-1"
                       :name "name-1"
                       :data-key "data-key-1"
                       :data-gesso-continuity-key "continuity-key-1"
                       :data-app-key "app-key-1"})]
    (is (= {:kind :attr :attr "data-app-key" :value "app-key-1"}
           (continuity/key-for-element node {:key-attr :data-app-key})))
    (is (= {:kind :id :value "id-1"}
           (continuity/key-for-element node)))
    (.removeAttribute node "id")
    (is (= {:kind :attr
            :attr "data-gesso-continuity-key"
            :value "continuity-key-1"}
           (continuity/key-for-element node)))
    (.removeAttribute node "data-gesso-continuity-key")
    (is (= {:kind :attr :attr "data-key" :value "data-key-1"}
           (continuity/key-for-element node)))
    (.removeAttribute node "data-key")
    (is (= {:kind :name :value "name-1"}
           (continuity/key-for-element node)))))

(deftest find-by-key-supports-id-attr-name-and-index-test
  (let [root (element "div")
        by-id (append! root (element "input" {:id "id-a"}))
        by-attr (append! root (element "input" {:data-key "attr-a"}))
        by-name (append! root (element "input" {:name "name-a"}))
        indexed (append! root (element "input"))]
    (is (identical? by-id
                    (continuity/find-by-key root {:kind :id :value "id-a"})))
    (is (identical? by-attr
                    (continuity/find-by-key root {:kind :attr
                                                  :attr "data-key"
                                                  :value "attr-a"})))
    (is (identical? by-name
                    (continuity/find-by-key root {:kind :name :value "name-a"})))
    (is (identical? indexed
                    (continuity/find-by-key root {:kind :index
                                                  :selector "input"
                                                  :value 3})))))

;; =============================================================================
;; Input state
;; =============================================================================

(deftest input-value-covers-text-checkbox-radio-and-multiple-select-test
  (let [text (element "input" {:type "text"})
        checkbox (element "input" {:type "checkbox"})
        radio (element "input" {:type "radio"})
        select (element "select" {:multiple "multiple"})
        a (append! select (element "option" {:value "a"}))
        b (append! select (element "option" {:value "b"}))]
    (set! (.-value text) "hello")
    (set! (.-checked checkbox) true)
    (set! (.-checked radio) false)
    (set! (.-selected a) true)
    (set! (.-selected b) false)
    (is (= {:value "hello"} (continuity/input-value text)))
    (is (= {:checked true} (continuity/input-value checkbox)))
    (is (= {:checked false} (continuity/input-value radio)))
    (is (= {:selected-values ["a"]} (continuity/input-value select)))))

(deftest restore-input-value-covers-supported-input-kinds-test
  (let [text (element "input")
        checkbox (element "input" {:type "checkbox"})
        select (element "select" {:multiple "multiple"})
        a (append! select (element "option" {:value "a"}))
        b (append! select (element "option" {:value "b"}))]
    (continuity/restore-input-value! text {:value "restored"})
    (continuity/restore-input-value! checkbox {:checked true})
    (continuity/restore-input-value! select {:selected-values ["b"]})
    (is (= "restored" (.-value text)))
    (is (true? (.-checked checkbox)))
    (is (false? (.-selected a)))
    (is (true? (.-selected b)))))

(deftest capture-and-restore-inputs-survives-node-replacement-test
  (let [old-target (element "div")
        old-input (append! old-target (element "input" {:id "query"}))
        new-target (element "div")
        new-input (append! new-target (element "input" {:id "query"}))]
    (set! (.-value old-input) "user typed")
    (let [state (continuity/capture-inputs old-target nil)]
      (set! (.-value new-input) "server value")
      (continuity/restore-inputs! new-target state)
      (is (= "user typed" (.-value new-input))))))

(deftest capture-inputs-falls-back-to-position-for-keyless-elements-test
  (let [old-target (element "div")
        old-a (append! old-target (element "input"))
        old-b (append! old-target (element "input"))
        new-target (element "div")
        new-a (append! new-target (element "input"))
        new-b (append! new-target (element "input"))]
    (set! (.-value old-a) "a-user")
    (set! (.-value old-b) "b-user")
    (let [state (continuity/capture-inputs old-target nil)]
      (continuity/restore-inputs! new-target state)
      (is (= "a-user" (.-value new-a)))
      (is (= "b-user" (.-value new-b))))))

;; =============================================================================
;; Details state
;; =============================================================================

(deftest details-elements-includes-target-when-target-is-details-test
  (let [target (element "details")
        nested (append! target (element "details"))]
    (is (= [target nested]
           (continuity/details-elements target nil)))))

(deftest details-state-survives-replacement-test
  (let [old-target (element "div")
        old-a (append! old-target (element "details" {:id "a"}))
        old-b (append! old-target (element "details" {:id "b"}))
        new-target (element "div")
        new-a (append! new-target (element "details" {:id "a"}))
        new-b (append! new-target (element "details" {:id "b"}))]
    (set! (.-open old-a) true)
    (set! (.-open old-b) false)
    (set! (.-open new-a) false)
    (set! (.-open new-b) true)
    (let [state (continuity/capture-details old-target nil false)]
      (continuity/restore-details! new-target nil state)
      (is (true? (.-open new-a)))
      (is (false? (.-open new-b))))))

(deftest single-details-mode-opens-at-most-one-test
  (let [target (element "div")
        a (append! target (element "details" {:id "a"}))
        b (append! target (element "details" {:id "b"}))]
    (set! (.-open a) true)
    (set! (.-open b) true)
    (continuity/restore-details!
     target nil
     {:single? true
      :items [{:key {:kind :id :value "a"} :open? true}
              {:key {:kind :id :value "b"} :open? true}]})
    (is (true? (.-open a)))
    (is (false? (.-open b)))))

;; =============================================================================
;; Focus and caret
;; =============================================================================

(deftest capture-focus-requires-editable-active-element-inside-target-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append! sandbox-root (element "section"))
            target (append! root (element "div"))
            input (append! target (element "input" {:id "focus-input"}))
            button (append! target (element "button" {:id "focus-button"}))]
        (.focus input)
        (is (= {:kind :id :value "focus-input"}
               (:key (continuity/capture-focus root target {}))))
        (.focus button)
        (is (nil? (continuity/capture-focus root target {})))
        (is (= {:kind :id :value "focus-button"}
               (:key (continuity/capture-focus
                      root target {:allow-non-editable true}))))))))

(deftest focus-caret-survives-target-replacement-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append! sandbox-root (element "section"))
            old-target (append! root (element "div" {:id "focus-target"}))
            old-input (append! old-target (element "input" {:id "editor"}))]
        (set! (.-value old-input) "abcdef")
        (.focus old-input)
        (.setSelectionRange old-input 2 5 "forward")
        (let [state (continuity/capture-focus root old-target {})
              new-target (replacement-target "focus-target")
              new-input (append! new-target (element "input" {:id "editor"}))]
          (set! (.-value new-input) "abcdef")
          (replace-target! old-target new-target)
          (continuity/restore-focus! root new-target state)
          (is (identical? new-input (.-activeElement js/document)))
          (is (= 2 (.-selectionStart new-input)))
          (is (= 5 (.-selectionEnd new-input))))))))

;; =============================================================================
;; Raw and anchor scroll
;; =============================================================================

(deftest explicit-scroll-container-selector-wins-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append! sandbox-root (element "div"))
            scroller (append! root (element "div" {:id "scroller"}))
            target (append! scroller (element "div"))]
        (is (identical?
             scroller
             (continuity/scroll-container-for
              target root {:container-selector "#scroller"})))))))

(deftest capture-and-restore-element-scroll-survives-scroller-replacement-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append! sandbox-root (element "div"))
            old-scroller (append! root
                                  (element "div"
                                           {:id "scroller"
                                            :style "height:40px;width:40px;overflow:auto;"}))
            old-target (append! old-scroller
                                (element "div"
                                         {:id "target"
                                          :style "height:200px;width:200px;"}))]
        (set! (.-scrollTop old-scroller) 27)
        (set! (.-scrollLeft old-scroller) 4)
        (let [state (continuity/capture-raw-scroll
                     root old-target {:container-selector "#scroller"})
              new-scroller (element "div"
                                    {:id "scroller"
                                     :style "height:40px;width:40px;overflow:auto;"})
              new-target (append! new-scroller
                                  (element "div"
                                           {:id "target"
                                            :style "height:200px;width:200px;"}))]
          (.replaceWith old-scroller new-scroller)
          (set! (.-scrollTop new-scroller) 0)
          (set! (.-scrollLeft new-scroller) 0)
          (is (continuity/restore-raw-scroll! root state))
          (is (= 27 (.-scrollTop new-scroller)))
          (is (= 4 (.-scrollLeft new-scroller)))
          (is new-target))))))

(deftest anchor-capture-falls-back-without-selector-or-key-test
  (let [root (element "div")
        target (append! root (element "div"))
        no-selector (continuity/capture-anchor-scroll root target {})
        anchor (append! target (element "div" {:class "anchor"}))
        no-key (continuity/capture-anchor-scroll
                root target {:selector ".anchor"})]
    (is (:raw-only? no-selector))
    (is (= :no-selector (:reason no-selector)))
    (is anchor)
    (is (:raw-only? no-key))
    (is (= :no-anchor-key (:reason no-key)))))

(deftest anchor-scroll-adjusts-element-scroller-by-layout-delta-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append! sandbox-root (element "div"))
            scroller (append! root
                              (element "div"
                                       {:id "scroller"
                                        :style "height:40px;overflow:auto;"}))
            target (append! scroller
                            (element "div" {:style "height:240px;"}))
            anchor (append! target (element "div" {:id "anchor"}))]
        (set! (.-scrollTop scroller) 100)
        (with-redefs [continuity/scroll-container-for
                      (fn [_element _root _box] scroller)]
          (let [state {:key {:kind :id :value "anchor"}
                       :top 20
                       :raw {:kind :element
                             :key {:kind :id :value "scroller"}
                             :top 100
                             :left 0}}
                original (.-getBoundingClientRect anchor)]
            (set! (.-getBoundingClientRect anchor)
                  (fn [] #js {:top 35 :bottom 45}))
            (is (continuity/restore-anchor-scroll!
                 root target {:container-selector "#scroller"} state))
            (is (= 115 (.-scrollTop scroller)))
            (set! (.-getBoundingClientRect anchor) original)))))))

;; =============================================================================
;; Boxes / custom extension behavior
;; =============================================================================

(deftest box-normalization-and-config-expansion-test
  (is (= {:type "inputs"}
         (continuity/normalize-box :inputs)))
  (is (= {:type "focus"}
         (continuity/normalize-box "focus")))
  (let [boxes (continuity/boxes-from-config
               {:preserve {:inputs true
                           :focus {:selector "input.keep"}
                           :scroll {:mode "raw"}}
                :boxes [{:type :details-open}]})]
    (is (= ["details-open" "raw-scroll" "focus" "inputs"]
           (mapv :type boxes)))
    (is (= "input.keep" (:selector (nth boxes 2))))))

(deftest custom-box-capture-and-restore-roundtrip-test
  (let [seen (atom [])
        rt (runtime
            {:boxes
             {:custom
              {:capture (fn [_runtime _root _target box]
                          {:captured (:value box)})
               :restore (fn [_runtime _root _target box state]
                          (swap! seen conj [box state]))}}})
        root (element "div")
        target (append! root (element "div"))
        captured (continuity/capture-box
                  rt root target {:type "custom" :value 7})]
    (is (= "custom" (:type captured)))
    (is (= {:captured 7} (:state captured)))
    (continuity/restore-box! rt root target captured)
    (is (= [[{:type "custom" :value 7}
             {:captured 7}]]
           @seen))))

(deftest unknown-and-throwing-boxes-are-nonfatal-test
  (with-sandbox*
    (fn [root]
      (let [diagnostics (atom [])
            rt (runtime
                {:on-diagnostic #(swap! diagnostics conj %)
                 :boxes {:boom {:capture (fn [& _]
                                           (throw (js/Error. "boom")))
                                :restore (fn [& _]
                                           (throw (js/Error. "restore boom")))}}})
            target (append! root (element "div"))]
        (is (nil? (continuity/capture-box
                   rt root target {:type "missing"})))
        (is (nil? (continuity/capture-box
                   rt root target {:type "boom"})))
        (continuity/restore-box!
         rt root target
         {:type "boom" :box {:type "boom"} :state {}})
        (is (contains? (set (map :kind @diagnostics))
                       :unknown-box-type))
        (is (contains? (set (map :kind @diagnostics))
                       :box-capture-failed))
        (is (contains? (set (map :kind @diagnostics))
                       :box-restore-failed))))))

(deftest event-box-roundtrip-uses-dom-events-test
  (with-sandbox*
    (fn [root]
      (let [rt (runtime)
            target (append! root (element "div"))
            captured-state (atom nil)
            restored-state (atom nil)]
        (.addEventListener
         root
         "gesso:live-continuity:capture-box:application"
         (fn [event]
           (aset (.-detail event)
                 "state"
                 #js {:token "captured"})))
        (.addEventListener
         root
         "gesso:live-continuity:restore-box:application"
         (fn [event]
           (reset! restored-state
                   (js->clj (aget (.-detail event) "state")
                            :keywordize-keys true))))
        (reset! captured-state
                (continuity/capture-event-box
                 rt root target {:type "event" :name "application"}))
        (is (= {:token "captured"} @captured-state))
        (continuity/restore-event-box!
         rt root target
         {:type "event" :name "application"}
         @captured-state)
        (is (= {:token "captured"} @restored-state))))))

(deftest js-box-resolves-global-functions-test
  (with-sandbox*
    (fn [root]
      (let [target (append! root (element "div"))
            restored (atom nil)
            rt (runtime)]
        (aset js/window "gessoContinuityTest"
              #js {:capture (fn [_root _target _box]
                              #js {:value 9})
                   :restore (fn [_root _target _box state]
                              (reset! restored
                                      (js->clj state :keywordize-keys true)))})
        (try
          (let [captured (continuity/capture-box
                          rt root target
                          {:type "js"
                           :capture "gessoContinuityTest.capture"
                           :restore "gessoContinuityTest.restore"})]
            (is (= {:value 9} (:state captured)))
            (continuity/restore-box! rt root target captured)
            (is (= {:value 9} @restored)))
          (finally
            (js-delete js/window "gessoContinuityTest")))))))

;; =============================================================================
;; Opaque resource lifecycle
;; =============================================================================

(deftest capture-disabled-or-malformed-config-returns-disabled-resource-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [rt (runtime {:now-ms (constantly 42)})
            {:keys [root]}
            (continuity-tree sandbox-root {:enabled false})
            disabled (continuity/capture!
                      rt {:physical {:fragment-root root}})]
        (is (continuity/resource? disabled))
        (is (false? (:enabled? disabled)))
        (is (= 42 (:captured-at disabled)))
        (.setAttribute root continuity/continuity-config-attr "{bad")
        (let [malformed (continuity/capture!
                         rt {:physical {:fragment-root root}})]
          (is (continuity/resource? malformed))
          (is (false? (:enabled? malformed))))))))

(deftest capture-with-missing-target-returns-disabled-resource-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [root (append!
                  sandbox-root
                  (element "section"
                           {continuity/continuity-attr "true"
                            continuity/continuity-fragment-attr-key "missing"
                            continuity/continuity-config-attr-key
                            (config-json {:enabled true
                                          :preserve {:inputs true}})}))
            resource (continuity/capture!
                      (runtime)
                      {:physical {:fragment-root root}})]
        (is (continuity/resource? resource))
        (is (false? (:enabled? resource)))
        (is (= "missing" (:target-id resource)))
        (is (= [] (:captured resource)))))))

(deftest resource-summary-is-host-reference-free-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [{:keys [root target]}
            (continuity-tree sandbox-root
                             {:enabled true
                              :preserve {:inputs true}})
            input (append! target (element "input" {:id "x"}))
            _ (set! (.-value input) "value")
            resource (continuity/capture!
                      (immediate-runtime {:now-ms (constantly 99)})
                      {:physical {:fragment-root root}})
            summary (continuity/resource-summary resource)]
        (is (= continuity/resource-type
               (:gesso.live.browser.continuity/type summary)))
        (is (= true (:enabled? summary)))
        (is (= "continuity-target" (:target-id summary)))
        (is (= 99 (:captured-at summary)))
        (is (= ["inputs"] (:box-types summary)))
        (is (not (contains? summary :root)))
        (is (not (contains? summary :config)))
        (is (not (contains? summary :captured)))))))

(deftest release-restores-height-lock-and-emits-release-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [{:keys [root target]}
            (continuity-tree sandbox-root {:enabled true})
            rt (runtime)
            seen (atom 0)]
        (set! (.-style.height root) "40px")
        (set! (.-style.display root) "block")
        (text! target "content")
        (.addEventListener
         root
         "gesso:live-continuity:released"
         (fn [_]
           (swap! seen inc)))
        (let [resource (continuity/capture!
                        rt {:physical {:fragment-root root}})]
          (continuity/release! rt {:resource resource})
          (is (= 1 @seen))
          (is (= "" (.-minHeight (.-style root)))))))))

(deftest restore-invalid-or-disabled-resource-resolves-successfully-test
  (async done
    (with-sandbox*
      (fn [sandbox-root]
        (let [{:keys [root]}
              (continuity-tree sandbox-root {:enabled false})
              rt (runtime)
              disabled (continuity/capture!
                        rt {:physical {:fragment-root root}})]
          (-> (continuity/restore! rt {:resource nil})
              (.then (fn [value]
                       (is (= true value))
                       (continuity/restore! rt {:resource disabled})))
              (.then (fn [value]
                       (is (= true value))
                       (.remove sandbox-root)
                       (done)))
              (.catch (fn [error]
                        (is false (str error))
                        (.remove sandbox-root)
                        (done)))))))))

(deftest full-capture-replacement-restore-preserves-user-state-not-server-structure-test
  (async done
    (let [sandbox-root (sandbox)
          rt (immediate-runtime)
          {:keys [root target]}
          (continuity-tree
           sandbox-root
           {:enabled true
            :preserve {:inputs true
                       :focus true}
            :boxes [{:type "details-open"}]})
          old-input (append! target (element "input" {:id "query"}))
          old-details (append! target (element "details" {:id "advanced"}))
          old-server-only (append! target (element "div" {:id "old-server-only"}))]
      (text! old-server-only "obsolete")
      (set! (.-value old-input) "user edit")
      (set! (.-open old-details) true)
      (.focus old-input)
      (.setSelectionRange old-input 2 5 "forward")
      (let [resource (continuity/capture!
                      rt {:physical {:fragment-root root}})
            new-target (replacement-target "continuity-target")
            new-input (append! new-target (element "input" {:id "query"}))
            new-details (append! new-target (element "details" {:id "advanced"}))
            new-server-only (append! new-target (element "div" {:id "new-server-only"}))]
        (set! (.-value new-input) "server canonical")
        (set! (.-open new-details) false)
        (text! new-server-only "new structure")
        (replace-target! target new-target)
        (promise->done!
         (continuity/restore! rt {:resource resource})
         done
         (fn [value]
           (is (= true value))
           (is (= "user edit" (.-value new-input)))
           (is (true? (.-open new-details)))
           (is (identical? new-input (.-activeElement js/document)))
           (is (= 2 (.-selectionStart new-input)))
           (is (= 5 (.-selectionEnd new-input)))
           (is (nil? (.querySelector root "#old-server-only")))
           (is (= "new structure" (.-textContent new-server-only)))
           (.remove sandbox-root)))))))

(deftest restore-resolves-target-again-after-layout-test
  (async done
    (let [sandbox-root (sandbox)
          frames (atom [])
          rt (runtime {:request-animation-frame!
                       (fn [f]
                         (swap! frames conj f)
                         (count @frames))})
          {:keys [root target]}
          (continuity-tree
           sandbox-root
           {:enabled true
            :preserve {:inputs true}})
          old-input (append! target (element "input" {:id "query"}))]
      (set! (.-value old-input) "captured")
      (let [resource (continuity/capture!
                      rt {:physical {:fragment-root root}})
            initial-target (replacement-target "continuity-target")
            initial-input (append! initial-target (element "input" {:id "query"}))]
        (set! (.-value initial-input) "first")
        (replace-target! target initial-target)
        (let [promise (continuity/restore! rt {:resource resource})]
          (is (= 1 (count @frames)))
          ((first @frames))
          (is (= 2 (count @frames)))
          ;; Replace again between layout frames. Final restore must resolve the
          ;; current target instead of retaining the first replacement node.
          (let [latest-target (replacement-target "continuity-target")
                latest-input (append! latest-target (element "input" {:id "query"}))]
            (set! (.-value latest-input) "latest-server")
            (replace-target! initial-target latest-target)
            ((second @frames))
            (promise->done!
             promise done
             (fn [value]
               (is (= true value))
               (is (= "captured" (.-value latest-input)))
               (is (not (.-isConnected initial-target)))
               (.remove sandbox-root)))))))))

(deftest captured-and-restored-events-bracket-resource-lifecycle-test
  (async done
    (let [sandbox-root (sandbox)
          rt (immediate-runtime)
          {:keys [root target]}
          (continuity-tree
           sandbox-root
           {:enabled true
            :preserve {:inputs true}})
          input (append! target (element "input" {:id "query"}))
          events (atom [])]
      (set! (.-value input) "user")
      (.addEventListener root "gesso:live-continuity:captured"
                         (fn [_] (swap! events conj :captured)))
      (.addEventListener root "gesso:live-continuity:restored"
                         (fn [_] (swap! events conj :restored)))
      (let [resource (continuity/capture!
                      rt {:physical {:fragment-root root}})
            new-target (replacement-target "continuity-target")
            new-input (append! new-target (element "input" {:id "query"}))]
        (replace-target! target new-target)
        (promise->done!
         (continuity/restore! rt {:resource resource})
         done
         (fn [_]
           (is (= [:captured :restored] @events))
           (is (= "user" (.-value new-input)))
           (.remove sandbox-root)))))))

(deftest capture-resource-is-opaque-physical-data-not-runtime-owned-slot-test
  (with-sandbox*
    (fn [sandbox-root]
      (let [rt (runtime)
            {:keys [root]}
            (continuity-tree sandbox-root {:enabled true})
            resource-a (continuity/capture!
                        rt {:physical {:fragment-root root}})
            resource-b (continuity/capture!
                        rt {:physical {:fragment-root root}})]
        (is (continuity/resource? resource-a))
        (is (continuity/resource? resource-b))
        (is (not (identical? resource-a resource-b)))
        (is (not (contains? rt :slots)))
        (is (not (contains? rt :resources)))
        (is (identical? root (:root resource-a)))
        (is (identical? root (:root resource-b)))))))


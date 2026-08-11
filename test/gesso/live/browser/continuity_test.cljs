(ns gesso.live.browser.continuity-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.dom :as dom]))

;; -----------------------------------------------------------------------------
;; DOM/test helpers
;; -----------------------------------------------------------------------------

(defn- element
  ([tag]
   (element tag nil))
  ([tag attrs]
   (let [node
         (.createElement
          js/document
          tag)]
     (doseq [[attribute value]
             attrs]
       (.setAttribute
        node
        (dom/attr-name attribute)
        (str value)))
     node)))

(defn- append!
  [parent child]
  (.appendChild
   parent
   child)
  child)

(defn- text!
  [node value]
  (set!
   (.-textContent node)
   value)
  node)

(defn- config-json
  [config]
  (.stringify
   js/JSON
   (clj->js config)))

(defn- sandbox
  []
  (let [root
        (element
         "div"
         {:data-gesso-continuity-test
          (str
           "test-"
           (random-uuid))})]
    (.appendChild
     (.-body js/document)
     root)
    root))

(defn- with-sandbox*
  [f]
  (let [root
        (sandbox)]
    (try
      (reset!
       continuity/slots
       {})
      (continuity/register-built-in-boxes!)
      (f root)
      (finally
        (reset!
         continuity/slots
         {})
        (continuity/register-built-in-boxes!)
        (.remove root)))))

(defn- continuity-tree
  ([sandbox-root config]
   (continuity-tree
    sandbox-root
    "continuity-target"
    config))
  ([sandbox-root target-id config]
   (let [root
         (append!
          sandbox-root
          (element
           "section"
           {continuity/continuity-attr
            "true"

            continuity/continuity-fragment-attr-key
            target-id

            continuity/continuity-config-attr-key
            (config-json config)}))

         target
         (append!
          root
          (element
           "div"
           {:id target-id}))]

     {:root root
      :target target})))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some->
   (thrown f)
   ex-data))

(defn- event
  ([name detail]
   (js/CustomEvent.
    name
    #js {:bubbles true
         :detail detail}))
  ([detail]
   (event
    "test"
    detail)))

(defn- set-event-detail!
  [event name value]
  (aset
   (.-detail event)
   name
   value)
  event)

;; -----------------------------------------------------------------------------
;; Wire/config identity
;; -----------------------------------------------------------------------------

(deftest continuity-wire-identity-test
  (is (= :data-gesso-live-continuity
         continuity/continuity-attr))

  (is (= :data-gesso-live-continuity-config
         continuity/continuity-config-attr-key))

  (is (= :data-gesso-live-continuity-fragment
         continuity/continuity-fragment-attr-key))

  (is (= "[data-gesso-live-continuity='true']"
         continuity/continuity-root-selector))

  (is (= "data-gesso-live-continuity-config"
         continuity/continuity-config-attr))

  (is (= "script[type='application/json'][data-gesso-live-continuity-config]"
         continuity/continuity-config-script-selector))

  (is (= "data-gesso-live-continuity-fragment"
         continuity/continuity-fragment-attr))

  (is (= "gesso:live-continuity:"
         continuity/continuity-event-prefix)))

;; -----------------------------------------------------------------------------
;; Generic helpers
;; -----------------------------------------------------------------------------

(deftest now-ms-is-number-test
  (is (number?
       (continuity/now-ms))))

(deftest js-function-predicate-test
  (is (continuity/js-function?
       (fn [])))

  (is (false?
       (continuity/js-function?
        {})))

  (is (false?
       (continuity/js-function?
        nil))))

(deftest contains-node-test
  (let [root
        (element "div")

        child
        (append!
         root
         (element "span"))

        other
        (element "span")]

    (is (continuity/contains-node?
         root
         root))

    (is (continuity/contains-node?
         root
         child))

    (is (false?
         (continuity/contains-node?
          root
          other)))

    (is (false?
         (continuity/contains-node?
          nil
          child)))))

;; -----------------------------------------------------------------------------
;; JSON configuration
;; -----------------------------------------------------------------------------

(deftest parse-json-test
  (let [root
        (element "div")]

    (is (= {:enabled true
            :preserve
            {:inputs true}}
           (continuity/parse-json
            root
            "{\"enabled\":true,\"preserve\":{\"inputs\":true}}"
            "test")))

    (is (nil?
         (continuity/parse-json
          root
          ""
          "test")))

    (is (nil?
         (continuity/parse-json
          root
          nil
          "test")))))

(deftest malformed-json-emits-error-and-returns-nil-test
  (let [root
        (element "div")

        seen
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:error"
     (fn [event]
       (reset!
        seen
        (.-detail event))))

    (is (nil?
         (continuity/parse-json
          root
          "{bad json"
          "attr")))

    (is (= "parse-config"
           (aget
            @seen
            "phase")))

    (is (= "attr"
           (aget
            @seen
            "source")))

    (is (= "{bad json"
           (aget
            @seen
            "raw")))))

;; -----------------------------------------------------------------------------
;; Custom events
;; -----------------------------------------------------------------------------

(deftest custom-event-test
  (let [detail
        #js {:value 42}

        event
        (continuity/custom-event
         "gesso:test"
         detail)]

    (is (= "gesso:test"
           (.-type event)))

    (is (true?
         (.-bubbles event)))

    (is (false?
         (.-cancelable event)))

    (is (identical?
         detail
         (.-detail event)))))

(deftest dispatch-test
  (let [root
        (element "div")

        detail
        #js {:value 42}

        seen
        (atom nil)]

    (.addEventListener
     root
     "gesso:test"
     (fn [event]
       (reset!
        seen
        (.-detail event))))

    (is (identical?
         detail
         (continuity/dispatch!
          root
          "gesso:test"
          detail)))

    (is (identical?
         detail
         @seen))))

(deftest emit-prefixes-continuity-event-name-test
  (let [root
        (element "div")

        detail
        #js {:phase "test"}

        seen
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:captured"
     (fn [event]
       (reset!
        seen
        (.-detail event))))

    (continuity/emit!
     root
     "captured"
     detail)

    (is (identical?
         detail
         @seen))))

;; -----------------------------------------------------------------------------
;; Deferred layout boundary
;; -----------------------------------------------------------------------------

(deftest after-layout-uses-two-animation-frames-test
  (let [callbacks
        (atom [])

        ran?
        (atom false)

        original
        (aget
         js/window
         "requestAnimationFrame")]

    (try
      (aset
       js/window
       "requestAnimationFrame"
       (fn [callback]
         (swap!
          callbacks
          conj
          callback)
         (count
          @callbacks)))

      (continuity/after-layout!
       #(reset!
         ran?
         true))

      (is (= 1
             (count @callbacks)))

      (is (false?
           @ran?))

      ((first
        @callbacks))

      (is (= 2
             (count @callbacks)))

      (is (false?
           @ran?))

      ((second
        @callbacks))

      (is (true?
           @ran?))

      (finally
        (aset
         js/window
         "requestAnimationFrame"
         original)))))

;; -----------------------------------------------------------------------------
;; Global function lookup
;; -----------------------------------------------------------------------------

(deftest resolve-global-path-test
  (aset
   js/window
   "__gessoContinuityTest"
   #js {:nested
        #js {:value 42}})

  (try
    (is (= 42
           (continuity/resolve-global-path
            "__gessoContinuityTest.nested.value")))

    (is (nil?
         (continuity/resolve-global-path
          "__gessoContinuityTest.missing.value")))

    (is (nil?
         (continuity/resolve-global-path
          "")))

    (finally
      (js-delete
       js/window
       "__gessoContinuityTest"))))

;; -----------------------------------------------------------------------------
;; Element lookup identity
;; -----------------------------------------------------------------------------

(deftest css-escape-test
  (is (string?
       (continuity/css-escape
        "request:1")))

  (is (not-empty
       (continuity/css-escape
        "request:1"))))

(deftest find-by-id-includes-root-test
  (let [root
        (element
         "div"
         {:id "root-id"})

        child
        (append!
         root
         (element
          "div"
          {:id "child-id"}))]

    (is (identical?
         root
         (continuity/find-by-id
          root
          "root-id")))

    (is (identical?
         child
         (continuity/find-by-id
          root
          "child-id")))

    (is (nil?
         (continuity/find-by-id
          root
          "missing")))))

(deftest find-by-attr-test
  (let [root
        (element "div")

        child
        (append!
         root
         (element
          "div"
          {:data-key "alpha"}))]

    (is (identical?
         child
         (continuity/find-by-attr
          root
          :data-key
          "alpha")))

    (is (nil?
         (continuity/find-by-attr
          root
          :data-key
          "missing")))))

(deftest key-for-element-precedence-test
  (let [node
        (element
         "input"
         {:id "id-key"
          :name "name-key"
          :data-gesso-continuity-key "gesso-key"
          :data-key "data-key"
          :data-app-key "application-key"})]

    (testing "explicit application key wins"
      (is (= {:kind :attr
              :attr :data-app-key
              :value "application-key"}
             (continuity/key-for-element
              node
              {:key-attr :data-app-key}))))

    (testing "DOM id wins without an explicit key"
      (is (= {:kind :id
              :value "id-key"}
             (continuity/key-for-element
              node))))

    (.removeAttribute
     node
     "id")

    (testing "Gesso continuity key wins after id"
      (is (= {:kind :attr
              :attr "data-gesso-continuity-key"
              :value "gesso-key"}
             (continuity/key-for-element
              node))))

    (.removeAttribute
     node
     "data-gesso-continuity-key")

    (testing "data-key precedes name"
      (is (= {:kind :attr
              :attr "data-key"
              :value "data-key"}
             (continuity/key-for-element
              node))))

    (.removeAttribute
     node
     "data-key")

    (is (= {:kind :name
            :value "name-key"}
           (continuity/key-for-element
            node)))))

(deftest key-for-element-supports-option-aliases-test
  (let [node
        (element
         "div"
         {:data-app "key"})]

    (doseq [opts
            [{:key-attr :data-app}
             {:key-attribute :data-app}
             {:keyAttr :data-app}
             {:keyAttribute :data-app}]]
      (is (= {:kind :attr
              :attr :data-app
              :value "key"}
             (continuity/key-for-element
              node
              opts))))))

(deftest key-for-element-may-have-no-stable-key-test
  (is (nil?
       (continuity/key-for-element
        (element "div"))))

  (is (nil?
       (continuity/key-for-element
        nil))))

(deftest find-by-key-test
  (let [root
        (element "div")

        by-id
        (append!
         root
         (element
          "input"
          {:id "by-id"}))

        by-attr
        (append!
         root
         (element
          "input"
          {:data-key "by-attr"}))

        by-name
        (append!
         root
         (element
          "input"
          {:name "by-name"}))

        first-index
        (append!
         root
         (element
          "textarea"
          {:data-position "first"}))

        second-index
        (append!
         root
         (element
          "textarea"
          {:data-position "second"}))]

    (is (identical?
         by-id
         (continuity/find-by-key
          root
          {:kind :id
           :value "by-id"})))

    (is (identical?
         by-attr
         (continuity/find-by-key
          root
          {:kind :attr
           :attr "data-key"
           :value "by-attr"})))

    (is (identical?
         by-name
         (continuity/find-by-key
          root
          {:kind :name
           :value "by-name"})))

    (is (identical?
         first-index
         (continuity/find-by-key
          root
          {:kind :index
           :selector "textarea"
           :value 0})))

    (is (identical?
         second-index
         (continuity/find-by-key
          root
          {:kind :index
           :selector "textarea"
           :value 1})))

    (is (nil?
         (continuity/find-by-key
          root
          {:kind :unknown})))))

;; -----------------------------------------------------------------------------
;; Raw scroll
;; -----------------------------------------------------------------------------

(deftest window-scroll-state-shape-test
  (let [state
        (continuity/window-scroll-state)]

    (is (= :window
           (:kind state)))

    (is (number?
         (:x state)))

    (is (number?
         (:y state)))))

(deftest restore-window-scroll-test
  (let [calls
        (atom [])

        original
        (aget
         js/window
         "scrollTo")]

    (try
      (aset
       js/window
       "scrollTo"
       (fn [x y]
         (swap!
          calls
          conj
          [x y])))

      (is (true?
           (continuity/restore-window-scroll!
            {:kind :window
             :x 12
             :y 34})))

      (is (= [[12 34]]
             @calls))

      (is (nil?
           (continuity/restore-window-scroll!
            {:kind :element
             :top 1})))

      (finally
        (aset
         js/window
         "scrollTo"
         original)))))

(deftest explicit-scroll-container-selector-wins-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           scroller
           (append!
            root
            (element
             "div"
             {:id "scroller"}))

           target
           (append!
            scroller
            (element "div"))]

       (is (identical?
            scroller
            (continuity/scroll-container-for
             target
             root
             {:container-selector
              "#scroller"})))

       (is (identical?
            scroller
            (continuity/scroll-container-for
             target
             root
             {:containerSelector
              "#scroller"})))))))

(deftest capture-element-scroll-state-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           scroller
           (append!
            root
            (element
             "div"
             {:id "scroll-box"}))

           target
           (append!
            scroller
            (element "div"))]

       (set!
        (.-scrollTop scroller)
        27)

       (set!
        (.-scrollLeft scroller)
        9)

       (let [state
             (continuity/capture-raw-scroll
              root
              target
              {:container "#scroll-box"})]

         (is (= :element
                (:kind state)))

         (is (= {:kind :id
                 :value "scroll-box"}
                (:key state)))

         (is (= 27
                (:top state)))

         (is (= 9
                (:left state)))

         (is (identical?
              scroller
              (:transient-element state))))))))

(deftest restore-element-scroll-finds-replacement-by-key-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           replacement
           (append!
            root
            (element
             "div"
             {:id "scroll-box"}))]

       (continuity/restore-raw-scroll!
        root
        {:kind :element
         :key {:kind :id
               :value "scroll-box"}
         :top 31
         :left 7})

       (is (= 31
              (.-scrollTop replacement)))

       (is (= 7
              (.-scrollLeft replacement)))))))

(deftest restore-element-scroll-may-use-surviving-transient-element-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           scroller
           (append!
            root
            (element "div"))]

       (continuity/restore-raw-scroll!
        root
        {:kind :element
         :key nil
         :top 51
         :left 2
         :transient-element scroller})

       (is (= 51
              (.-scrollTop scroller)))

       (is (= 2
              (.-scrollLeft scroller)))))))

;; -----------------------------------------------------------------------------
;; Anchor scroll
;; -----------------------------------------------------------------------------

(deftest capture-anchor-scroll-falls-back-without-selector-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        raw
        {:kind :window
         :x 1
         :y 2}]

    (with-redefs
     [continuity/capture-raw-scroll
      (fn [_root _target _box]
        raw)]

      (is (= {:raw-only? true
              :raw raw
              :reason :no-selector}
             (continuity/capture-anchor-scroll
              root
              target
              {}))))))

(deftest capture-anchor-scroll-falls-back-when-anchor-has-no-key-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        anchor
        (append!
         target
         (element
          "div"
          {:class "anchor"}))

        raw
        {:kind :window
         :x 0
         :y 10}]

    (with-redefs
     [continuity/visible-anchor
      (fn [_elements _scroller]
        anchor)

      continuity/capture-raw-scroll
      (fn [_root _target _box]
        raw)]

      (is (= {:raw-only? true
              :raw raw
              :reason :no-anchor-key}
             (continuity/capture-anchor-scroll
              root
              target
              {:selector ".anchor"}))))))

(deftest restore-anchor-falls-back-when-key-disappears-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        raw
        {:kind :window
         :x 3
         :y 4}

        calls
        (atom [])]

    (with-redefs
     [continuity/restore-raw-scroll!
      (fn [actual-root actual-state]
        (swap!
         calls
         conj
         [actual-root actual-state])
        true)]

      (is (true?
           (continuity/restore-anchor-scroll!
            root
            target
            {:selector ".anchor"}
            {:key {:kind :id
                   :value "missing"}
             :top 100
             :raw raw})))

      (is (= [[root raw]]
             @calls)))))

(deftest restore-anchor-adjusts-element-scroller-by-layout-delta-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           scroller
           (append!
            root
            (element
             "div"
             {:id "scroller"}))

           target
           (append!
            scroller
            (element "div"))

           anchor
           (append!
            target
            (element
             "div"
             {:id "anchor"}))]

       (set!
        (.-scrollTop scroller)
        20)

       (with-redefs
        [continuity/scroll-container-for
         (fn [_element _root _box]
           scroller)]

        (set!
         (.-getBoundingClientRect anchor)
         (fn []
           #js {:top 150
                :bottom 170}))

        (is (true?
             (continuity/restore-anchor-scroll!
              root
              target
              {:container "#scroller"}
              {:key {:kind :id
                     :value "anchor"}
               :top 100
               :raw {:kind :element}})))

        (is (= 70
               (.-scrollTop scroller))))))))

;; -----------------------------------------------------------------------------
;; Input values
;; -----------------------------------------------------------------------------

(deftest input-value-text-test
  (let [input
        (element
         "input"
         {:type "text"})]

    (set!
     (.-value input)
     "draft")

    (is (= {:value "draft"}
           (continuity/input-value
            input)))))

(deftest input-value-checkbox-and-radio-test
  (doseq [type
          ["checkbox"
           "radio"]]

    (let [input
          (element
           "input"
           {:type type})]

      (set!
       (.-checked input)
       true)

      (is (= {:checked true}
             (continuity/input-value
              input)))

      (set!
       (.-checked input)
       false)

      (is (= {:checked false}
             (continuity/input-value
              input))))))

(deftest input-value-multiple-select-test
  (let [select
        (element
         "select"
         {:multiple "multiple"})

        one
        (append!
         select
         (element
          "option"
          {:value "one"}))

        two
        (append!
         select
         (element
          "option"
          {:value "two"}))

        three
        (append!
         select
         (element
          "option"
          {:value "three"}))]

    (set!
     (.-selected one)
     true)

    (set!
     (.-selected two)
     false)

    (set!
     (.-selected three)
     true)

    (is (= {:selected-values
            ["one"
             "three"]}
           (continuity/input-value
            select)))))

(deftest restore-input-value-test
  (let [text
        (element
         "input"
         {:type "text"})

        checkbox
        (element
         "input"
         {:type "checkbox"})

        select
        (element
         "select"
         {:multiple "multiple"})

        one
        (append!
         select
         (element
          "option"
          {:value "one"}))

        two
        (append!
         select
         (element
          "option"
          {:value "two"}))]

    (continuity/restore-input-value!
     text
     {:value "restored"})

    (continuity/restore-input-value!
     checkbox
     {:checked true})

    (continuity/restore-input-value!
     select
     {:selected-values
      ["two"]})

    (is (= "restored"
           (.-value text)))

    (is (true?
         (.-checked checkbox)))

    (is (false?
         (.-selected one)))

    (is (true?
         (.-selected two)))))

(deftest capture-and-restore-inputs-by-stable-key-test
  (let [before
        (element "div")

        before-input
        (append!
         before
         (element
          "input"
          {:id "draft"}))

        _set-before
        (set!
         (.-value before-input)
         "typed value")

        captured
        (continuity/capture-inputs
         before
         nil)

        after
        (element "div")

        after-input
        (append!
         after
         (element
          "input"
          {:id "draft"}))]

    (set!
     (.-value after-input)
     "server default")

    (continuity/restore-inputs!
     after
     captured)

    (is (= "typed value"
           (.-value after-input)))))

(deftest capture-and-restore-inputs-by-position-fallback-test
  (let [before
        (element "div")

        first-before
        (append!
         before
         (element
          "input"
          {:type "text"}))

        second-before
        (append!
         before
         (element
          "input"
          {:type "text"}))

        _set-first
        (set!
         (.-value first-before)
         "first")

        _set-second
        (set!
         (.-value second-before)
         "second")

        captured
        (continuity/capture-inputs
         before
         "input")

        after
        (element "div")

        first-after
        (append!
         after
         (element
          "input"
          {:type "text"}))

        second-after
        (append!
         after
         (element
          "input"
          {:type "text"}))]

    (continuity/restore-inputs!
     after
     captured)

    (is (= "first"
           (.-value first-after)))

    (is (= "second"
           (.-value second-after)))))

;; -----------------------------------------------------------------------------
;; <details> state
;; -----------------------------------------------------------------------------

(deftest details-elements-includes-target-itself-test
  (let [target
        (element
         "details"
         {:id "outer"})

        inner
        (append!
         target
         (element
          "details"
          {:id "inner"}))]

    (is (= [target inner]
           (continuity/details-elements
            target
            nil)))))

(deftest capture-and-restore-details-test
  (let [before
        (element "div")

        first
        (append!
         before
         (element
          "details"
          {:id "first"}))

        second
        (append!
         before
         (element
          "details"
          {:id "second"}))

        _open
        (set!
         (.-open first)
         true)

        captured
        (continuity/capture-details
         before
         nil
         false)

        after
        (element "div")

        first-after
        (append!
         after
         (element
          "details"
          {:id "first"}))

        second-after
        (append!
         after
         (element
          "details"
          {:id "second"}))]

    (set!
     (.-open first-after)
     false)

    (set!
     (.-open second-after)
     true)

    (continuity/restore-details!
     after
     nil
     captured)

    (is (true?
         (.-open first-after)))

    (is (false?
         (.-open second-after)))))

(deftest single-details-restoration-opens-at-most-one-test
  (let [target
        (element "div")

        first
        (append!
         target
         (element
          "details"
          {:id "first"}))

        second
        (append!
         target
         (element
          "details"
          {:id "second"}))

        third
        (append!
         target
         (element
          "details"
          {:id "third"}))]

    (doseq [node
            [first second third]]
      (set!
       (.-open node)
       true))

    (continuity/restore-details!
     target
     nil
     {:single? true
      :items
      [{:key {:kind :id
              :value "first"}
        :open? false}

       {:key {:kind :id
              :value "second"}
        :open? true}

       {:key {:kind :id
              :value "third"}
        :open? true}]})

    (is (false?
         (.-open first)))

    (is (true?
         (.-open second)))

    (is (false?
         (.-open third)))))

;; -----------------------------------------------------------------------------
;; Focus and caret
;; -----------------------------------------------------------------------------

(deftest capture-focus-requires-active-element-inside-root-and-target-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           target
           (append!
            root
            (element "div"))

           input
           (append!
            target
            (element
             "input"
             {:id "focus-input"
              :type "text"}))]

       (.focus input)

       (let [state
             (continuity/capture-focus
              root
              target
              {})]

         (is (= {:kind :id
                 :value "focus-input"}
                (:key state)))

         (is (= :window
                (get-in
                 state
                 [:window-scroll
                  :kind]))))

       (is (nil?
            (continuity/capture-focus
             root
             (element "div")
             {})))))))

(deftest capture-focus-captures-caret-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           target
           (append!
            root
            (element "div"))

           input
           (append!
            target
            (element
             "input"
             {:id "caret"
              :type "text"}))]

       (set!
        (.-value input)
        "abcdef")

       (.focus input)

       (.setSelectionRange
        input
        2
        5
        "forward")

       (let [state
             (continuity/capture-focus
              root
              target
              {})]

         (is (= 2
                (:selection-start state)))

         (is (= 5
                (:selection-end state)))

         (is (= "forward"
                (:selection-direction state))))))))

(deftest capture-focus-default-excludes-button-like-controls-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           target
           (append!
            root
            (element "div"))

           button
           (append!
            target
            (element
             "button"
             {:id "button"}))]

       (.focus button)

       (is (nil?
            (continuity/capture-focus
             root
             target
             {})))

       (is (= {:kind :id
               :value "button"}
              (:key
               (continuity/capture-focus
                root
                target
                {:include-buttons true}))))))))

(deftest capture-focus-respects-selector-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           target
           (append!
            root
            (element "div"))

           input
           (append!
            target
            (element
             "input"
             {:id "draft"
              :class "draft"
              :type "text"}))]

       (.focus input)

       (is (some?
            (continuity/capture-focus
             root
             target
             {:selector ".draft"})))

       (is (nil?
            (continuity/capture-focus
             root
             target
             {:selector ".other"})))))))

(deftest restore-focus-finds-replacement-and-restores-caret-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element "div"))

           target
           (append!
            root
            (element "div"))

           input
           (append!
            target
            (element
             "input"
             {:id "draft"
              :type "text"}))

           raf-calls
           (atom [])]

       (set!
        (.-value input)
        "abcdef")

       (let [original
             (aget
              js/window
              "requestAnimationFrame")]

         (try
           (aset
            js/window
            "requestAnimationFrame"
            (fn [callback]
              (swap!
               raf-calls
               conj
               callback)
              1))

           (with-redefs
            [continuity/restore-window-scroll!
             (fn [_state]
               true)]

             (continuity/restore-focus!
              root
              target
              {:key {:kind :id
                     :value "draft"}
               :selection-start 1
               :selection-end 4
               :selection-direction "forward"
               :window-scroll
               {:kind :window
                :x 0
                :y 10}})

             (is (identical?
                  input
                  (.-activeElement js/document)))

             (is (= 1
                    (.-selectionStart input)))

             (is (= 4
                    (.-selectionEnd input)))

             (is (= 1
                    (count @raf-calls))))

           (finally
             (aset
              js/window
              "requestAnimationFrame"
              original))))))))

;; -----------------------------------------------------------------------------
;; Event-mediated custom boxes
;; -----------------------------------------------------------------------------

(deftest capture-event-box-roundtrip-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        seen
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:capture-box:selection"
     (fn [event]
       (reset!
        seen
        (.-detail event))

       (aset
        (.-detail event)
        "state"
        #js {:selectedId "request-2"})))

    (is (= {:selectedId
            "request-2"}
           (continuity/capture-event-box
            root
            target
            {:type "event"
             :name "selection"})))

    (is (identical?
         root
         (aget
          @seen
          "root")))

    (is (identical?
         target
         (aget
          @seen
          "target")))))

(deftest restore-event-box-roundtrip-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        seen
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:restore-box:selection"
     (fn [event]
       (reset!
        seen
        (.-detail event))))

    (continuity/restore-event-box!
     root
     target
     {:type "event"
      :name "selection"}
     {:selectedId
      "request-2"})

    (is (= "request-2"
           (aget
            (aget
             @seen
             "state")
            "selectedId")))))

;; -----------------------------------------------------------------------------
;; Box registry
;; -----------------------------------------------------------------------------

(deftest initialize-registers-built-in-boxes-test
  (reset!
   continuity/boxes
   {})

  (is (true?
       (continuity/initialize!)))

  (is (= #{"raw-scroll"
           "anchor-scroll"
           "focus"
           "inputs"
           "details-open"
           "js"
           "event"
           "hyperscript"}
         (set
          (keys
           @continuity/boxes)))))

(deftest register-box-normalizes-type-and-js-implementation-test
  (reset!
   continuity/boxes
   {})

  (let [capture
        (fn [_root _target _box]
          {:captured true})

        restore
        (fn [_root _target _box _state]
          true)]

    (is (true?
         (continuity/register-box!
          :custom
          {:capture capture
           :restore restore})))

    (is (identical?
         capture
         (get-in
          @continuity/boxes
          ["custom" :capture])))

    (is (true?
         (continuity/register-box!
          "js-object"
          #js {:capture capture
               :restore restore})))

    (is (identical?
         restore
         (get-in
          @continuity/boxes
          ["js-object" :restore])))))

(deftest register-box-nil-is-no-op-test
  (reset!
   continuity/boxes
   {})

  (is (true?
       (continuity/register-box!
        nil
        {:capture identity})))

  (is (= {}
         @continuity/boxes)))

;; -----------------------------------------------------------------------------
;; Box normalization/config expansion
;; -----------------------------------------------------------------------------

(deftest normalize-box-test
  (continuity/register-built-in-boxes!)

  (is (= {:type "inputs"}
         (continuity/normalize-box
          "inputs")))

  (is (= {:type "focus"}
         (continuity/normalize-box
          :focus)))

  (is (= {:type "details-open"
          :single? true}
         (continuity/normalize-box
          {:type :details-open
           :single? true})))

  (is (= {:name :focus
          :type "focus"}
         (continuity/normalize-box
          {:name :focus})))

  (is (= {:name "selection"
          :type "event"}
         (continuity/normalize-box
          {:name "selection"})))

  (is (nil?
       (continuity/normalize-box
        42))))

(deftest boxes-from-config-expands-preserve-shorthand-test
  (is (= [{:type "custom"}
          {:type "raw-scroll"}
          {:selector ".draft"
           :type "focus"}
          {:selector "input"
           :type "inputs"}]
         (continuity/boxes-from-config
          {:boxes
           [{:type "custom"}]

           :preserve
           {:scroll true
            :focus
            {:selector ".draft"}
            :inputs
            {:selector "input"}}}))))

(deftest boxes-from-config-normalizes-anchor-scroll-test
  (is (= [{:mode "anchor"
           :selector "[data-row]"
           :type "anchor-scroll"}]
         (continuity/boxes-from-config
          {:preserve
           {:scroll
            {:mode "anchor"
             :selector "[data-row]"}}}))))

(deftest boxes-from-config-normalizes-raw-scroll-aliases-test
  (doseq [mode
          ["raw"
           "position"
           "scroll"
           "raw-scroll"]]

    (is (= "raw-scroll"
           (:type
            (first
             (continuity/boxes-from-config
              {:preserve
               {:scroll
                {:mode mode}}})))))))

(deftest boxes-from-config-accepts-single-explicit-box-test
  (is (= [{:type "details-open"}]
         (continuity/boxes-from-config
          {:boxes
           :details-open}))))

;; -----------------------------------------------------------------------------
;; Generic box execution
;; -----------------------------------------------------------------------------

(deftest capture-box-test
  (reset!
   continuity/boxes
   {"custom"
    {:capture
     (fn [_root _target box]
       {:value
        (:value box)})}})

  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))]

    (is (= {:type "custom"
            :name "named"
            :box
            {:type "custom"
             :name "named"
             :value 42}
            :state
            {:value 42}}
           (continuity/capture-box
            root
            target
            {:type "custom"
             :name "named"
             :value 42})))))

(deftest unknown-box-capture-is-nonfatal-and-emits-error-test
  (reset!
   continuity/boxes
   {})

  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        detail
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:error"
     (fn [event]
       (reset!
        detail
        (.-detail event))))

    (is (nil?
         (continuity/capture-box
          root
          target
          {:type "missing"})))

    (is (= "capture"
           (aget
            @detail
            "phase")))

    (is (= "unknown-box-type"
           (aget
            @detail
            "reason")))))

(deftest throwing-box-capture-is-nonfatal-test
  (reset!
   continuity/boxes
   {"explode"
    {:capture
     (fn [& _]
       (throw
        (js/Error.
         "boom")))}})

  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))

        detail
        (atom nil)]

    (.addEventListener
     root
     "gesso:live-continuity:error"
     (fn [event]
       (reset!
        detail
        (.-detail event))))

    (is (nil?
         (continuity/capture-box
          root
          target
          {:type "explode"})))

    (is (= "capture"
           (aget
            @detail
            "phase")))))

(deftest restore-box-test
  (let [calls
        (atom [])]

    (reset!
     continuity/boxes
     {"custom"
      {:restore
       (fn [root target box state]
         (swap!
          calls
          conj
          [root target box state]))}})

    (let [root
          (element "div")

          target
          (append!
           root
           (element "div"))

          captured
          {:type "custom"
           :box {:type "custom"
                 :value 1}
           :state {:captured true}}]

      (is (identical?
           target
           (continuity/restore-box!
            root
            target
            captured)))

      (is (= [[root
               target
               {:type "custom"
                :value 1}
               {:captured true}]]
             @calls)))))

(deftest unknown-or-throwing-restore-is-nonfatal-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))]

    (reset!
     continuity/boxes
     {})

    (is (identical?
         target
         (continuity/restore-box!
          root
          target
          {:type "missing"})))

    (reset!
     continuity/boxes
     {"explode"
      {:restore
       (fn [& _]
         (throw
          (js/Error.
           "boom")))}})

    (is (identical?
         target
         (continuity/restore-box!
          root
          target
          {:type "explode"
           :box {:type "explode"}
           :state {}})))))

;; -----------------------------------------------------------------------------
;; Config lookup
;; -----------------------------------------------------------------------------

(deftest child-config-script-test
  (let [root
        (element "div")

        unrelated
        (append!
         root
         (element
          "script"
          {:type "application/json"}))

        matching
        (append!
         root
         (element
          "script"
          {:type "application/json"
           continuity/continuity-config-attr-key
           "true"}))]

    (is (identical?
         matching
         (continuity/child-config-script
          root)))

    (is (not
         (identical?
          unrelated
          (continuity/child-config-script
           root))))))

(deftest parse-config-prefers-root-attribute-test
  (let [root
        (element
         "div"
         {continuity/continuity-config-attr-key
          (config-json
           {:enabled true
            :source "attr"})})

        script
        (append!
         root
         (element
          "script"
          {:type "application/json"
           continuity/continuity-config-attr-key
           "true"}))]

    (set!
     (.-textContent script)
     (config-json
      {:enabled true
       :source "script"}))

    (is (= {:enabled true
            :source "attr"}
           (continuity/parse-config
            root)))))

(deftest parse-config-falls-back-to-child-script-test
  (let [root
        (element "div")

        script
        (append!
         root
         (element
          "script"
          {:type "application/json"
           continuity/continuity-config-attr-key
           "true"}))]

    (set!
     (.-textContent script)
     (config-json
      {:enabled true
       :preserve
       {:inputs true}}))

    (is (= {:enabled true
            :preserve
            {:inputs true}}
           (continuity/parse-config
            root)))))

(deftest enabled-test
  (is (true?
       (boolean
        (continuity/enabled?
         {:enabled true}))))

  (is (true?
       (boolean
        (continuity/enabled?
         {}))))

  (is (false?
       (boolean
        (continuity/enabled?
         {:enabled false}))))

  (is (false?
       (boolean
        (continuity/enabled?
         nil)))))

;; -----------------------------------------------------------------------------
;; Stable roots and targets
;; -----------------------------------------------------------------------------

(deftest root-finds-nearest-continuity-owner-test
  (let [outer
        (element
         "section"
         {continuity/continuity-attr
          "true"})

        inner
        (append!
         outer
         (element "div"))

        button
        (append!
         inner
         (element "button"))]

    (is (identical?
         outer
         (continuity/root
          button)))

    (is (identical?
         outer
         (continuity/root
          outer)))

    (is (nil?
         (continuity/root
          (element "div"))))

    (is (nil?
         (continuity/root
          nil)))))

(deftest target-id-prefers-explicit-continuity-fragment-id-test
  (let [root
        (element
         "section"
         {continuity/continuity-fragment-attr-key
          "continuity-target"

          :hx-target
          "#htmx-target"})]

    (is (= "continuity-target"
           (continuity/target-id
            root)))))

(deftest target-id-falls-back-to-simple-hx-target-id-test
  (is (= "fragment"
         (continuity/target-id
          (element
           "section"
           {:hx-target
            "#fragment"}))))

  (is (nil?
       (continuity/target-id
        (element
         "section"
         {:hx-target
          "closest [data-card]"})))))

(deftest target-prefers-descendant-before-document-global-id-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [outside
           (append!
            sandbox-root
            (element
             "div"
             {:id "same-target"}))

           owner
           (append!
            sandbox-root
            (element
             "section"
             {continuity/continuity-fragment-attr-key
              "same-target"}))

           inside
           (append!
            owner
            (element
             "div"
             {:id "same-target"}))]

       (is (identical?
            inside
            (continuity/target
             owner)))

       (is (not
            (identical?
             outside
             (continuity/target
              owner))))))))

(deftest target-may-be-global-document-element-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [target
           (append!
            sandbox-root
            (element
             "div"
             {:id "global-target"}))

           owner
           (append!
            sandbox-root
            (element
             "section"
             {continuity/continuity-fragment-attr-key
              "global-target"}))]

       (is (identical?
            target
            (continuity/target
             owner)))))))

(deftest root-for-target-id-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root]}
           (continuity-tree
            sandbox-root
            "request-list"
            {:enabled true})]

       (is (identical?
            root
            (continuity/root-for-target-id
             "request-list")))

       (is (nil?
            (continuity/root-for-target-id
             "missing")))))))

;; -----------------------------------------------------------------------------
;; Height stability
;; -----------------------------------------------------------------------------

(deftest lock-height-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))]

    (set!
     (.. root -style -minHeight)
     "5px")

    (set!
     (.-getBoundingClientRect root)
     (fn []
       #js {:height 120}))

    (let [lock
          (continuity/lock-height!
           root
           target)]

      (is (= {:height 120
              :previous "5px"}
             lock))

      (is (= "120px"
             (.. root -style -minHeight)))

      (is (identical?
           root
           (continuity/release-height-lock!
            root
            lock)))

      (is (= "5px"
             (.. root -style -minHeight))))))

(deftest zero-height-does-not-lock-test
  (let [root
        (element "div")

        target
        (append!
         root
         (element "div"))]

    (set!
     (.-getBoundingClientRect root)
     (fn []
       #js {:height 0}))

    (is (nil?
         (continuity/lock-height!
          root
          target)))))

;; -----------------------------------------------------------------------------
;; Slot lifecycle
;; -----------------------------------------------------------------------------

(deftest capture-disabled-config-does-nothing-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root]}
           (continuity-tree
            sandbox-root
            {:enabled false
             :preserve
             {:inputs true}})]

       (is (nil?
            (continuity/capture!
             root)))

       (is (= {}
              @continuity/slots))))))

(deftest capture-requires-resolvable-target-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [root
           (append!
            sandbox-root
            (element
             "section"
             {continuity/continuity-attr
              "true"

              continuity/continuity-fragment-attr-key
              "missing"

              continuity/continuity-config-attr-key
              (config-json
               {:enabled true
                :preserve
                {:inputs true}})}))]

       (is (nil?
            (continuity/capture!
             root)))

       (is (= {}
              @continuity/slots))))))

(deftest capture-stores-browser-owned-state-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :boxes
             [{:type "details-open"}]
             :preserve
             {:inputs true}})

           details
           (append!
            target
            (element
             "details"
             {:id "request-1"}))

           input
           (append!
            details
            (element
             "input"
             {:id "draft"
              :type "text"}))

           source
           (element "button")]

       (set!
        (.-open details)
        true)

       (set!
        (.-value input)
        "typed")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (let [slot
              (continuity/capture!
               root
               source)]

          (is (= "continuity-target"
                 (:target-id slot)))

          (is (identical?
               source
               (:source slot)))

          (is (identical?
               root
               (:root slot)))

          (is (= {:enabled true
                  :boxes
                  [{:type "details-open"}]
                  :preserve
                  {:inputs true}}
                 (:config slot)))

          (is (= ["details-open"
                  "inputs"]
                 (mapv
                  :type
                  (:captured slot))))

          (is (= {:kind :window
                  :x 0
                  :y 0}
                 (:fallback-scroll slot)))

          (is (identical?
               slot
               (continuity/captured-slot
                root)))

          (is (identical?
               slot
               (continuity/captured-slot
                "continuity-target")))))))))

(deftest recapture-replaces-prior-slot-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           source-a
           (element
            "button"
            {:id "a"})

           source-b
           (element
            "button"
            {:id "b"})]

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (let [first-slot
              (continuity/capture!
               root
               source-a)

              second-slot
              (continuity/capture!
               root
               source-b)]

          (is (not
               (identical?
                first-slot
                second-slot)))

          (is (identical?
               source-b
               (:source
                (continuity/captured-slot
                 root))))

          (is (= 1
                 (count
                  @continuity/slots)))))))))

(deftest release-slot-removes-slot-and-height-lock-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           released
           (atom [])

           slot
           {:root root
            :target-id "continuity-target"
            :height-lock
            {:height 10}}]

       (reset!
        continuity/slots
        {"continuity-target"
         slot})

       (with-redefs
        [continuity/release-height-lock!
         (fn [actual-root actual-lock]
           (swap!
            released
            conj
            [actual-root
             actual-lock])
           actual-root)]

        (is (identical?
             slot
             (continuity/release-slot!
              slot)))

        (is (= {}
               @continuity/slots))

        (is (= [[root
                 {:height 10}]]
               @released)))))))

;; -----------------------------------------------------------------------------
;; Two-phase restoration
;; -----------------------------------------------------------------------------

(deftest restore-immediate-restores-only-details-state-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :boxes
             [{:type "details-open"}]
             :preserve
             {:inputs true}})

           details
           (append!
            target
            (element
             "details"
             {:id "request-1"}))

           input
           (append!
            details
            (element
             "input"
             {:id "draft"
              :type "text"}))]

       (set!
        (.-open details)
        true)

       (set!
        (.-value input)
        "typed")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (let [slot
              (continuity/capture!
               root)]

          (set!
           (.-open details)
           false)

          (set!
           (.-value input)
           "server")

          (is (identical?
               slot
               (continuity/restore-immediate!
                root)))

          (testing "details state is visible immediately"
            (is (true?
                 (.-open details))))

          (testing "input state waits for the layout phase"
            (is (= "server"
                   (.-value input))))

          (testing "the slot remains live until full restoration"
            (is (identical?
                 slot
                 (continuity/captured-slot
                  root))))))))))

(deftest restore-after-layout-restores-all-boxes-and-consumes-slot-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :boxes
             [{:type "details-open"}]
             :preserve
             {:inputs true}})

           details
           (append!
            target
            (element
             "details"
             {:id "request-1"}))

           input
           (append!
            details
            (element
             "input"
             {:id "draft"
              :type "text"}))]

       (set!
        (.-open details)
        true)

       (set!
        (.-value input)
        "typed")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (let [slot
              (continuity/capture!
               root)]

          (set!
           (.-open details)
           false)

          (set!
           (.-value input)
           "server")

          (is (identical?
               slot
               (continuity/restore-after-layout!
                root)))

          (is (true?
               (.-open details)))

          (is (= "typed"
                 (.-value input)))

          (is (nil?
               (continuity/captured-slot
                root)))))))))

(deftest restore-missing-target-falls-back-to-window-scroll-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           calls
           (atom [])]

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 10
            :y 20})

         continuity/lock-height!
         (fn [_root _target]
           nil)

         continuity/restore-window-scroll!
         (fn [state]
           (swap!
            calls
            conj
            state)
           true)]

        (continuity/capture!
         root)

        (.remove target)

        (continuity/restore-immediate!
         root)

        (continuity/restore-after-layout!
         root)

        (is (= [{:kind :window
                 :x 10
                 :y 20}

                {:kind :window
                 :x 10
                 :y 20}]
               @calls))

        (is (nil?
             (continuity/captured-slot
              root))))))))

(deftest restore-validates-completion-callback-test
  (let [data
        (thrown-data
         #(continuity/restore!
           nil
           :not-callable))]

    (is (= :gesso.live.continuity/invalid-completion-callback
           (:error/type data)))

    (is (= :not-callable
           (:callback data)))))

(deftest restore-callback-runs-after-layout-boundary-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           callback-value
           (atom nil)

           scheduled
           (atom nil)]

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)

         continuity/after-layout!
         (fn [f]
           (reset!
            scheduled
            f))]

        (let [slot
              (continuity/capture!
               root)]

          (is (identical?
               slot
               (continuity/restore!
                root
                #(reset!
                  callback-value
                  %))))

          (is (nil?
               @callback-value))

          (is (ifn?
               @scheduled))

          (@scheduled)

          (is (identical?
               root
               (:root
                @callback-value)))

          (is (identical?
               target
               (:target
                @callback-value)))

          (is (identical?
               slot
               (:slot
                @callback-value)))

          (is (nil?
               (continuity/captured-slot
                root)))))))))

(deftest restore-callback-runs-even-with-no-slot-test
  (let [root
        (element "div")

        callback-value
        (atom nil)]

    (with-redefs
     [continuity/after-layout!
      (fn [f]
        (f))]

      (is (nil?
           (continuity/restore!
            root
            #(reset!
              callback-value
              %))))

      (is (identical?
           root
           (:root
            @callback-value)))

      (is (nil?
           (:slot
            @callback-value))))))

;; -----------------------------------------------------------------------------
;; Capture/restored framework events
;; -----------------------------------------------------------------------------

(deftest capture-emits-captured-event-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           seen
           (atom nil)

           source
           (element "button")]

       (.addEventListener
        root
        "gesso:live-continuity:captured"
        (fn [event]
          (reset!
           seen
           (.-detail event))))

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (continuity/capture!
         root
         source)

        (is (identical?
             source
             (aget
              @seen
              "source")))

        (is (identical?
             root
             (aget
              @seen
              "root")))

        (is (identical?
             target
             (aget
              @seen
              "target")))

        (is (= "continuity-target"
               (aget
                @seen
                "targetId")))

        (is (= 0
               (aget
                @seen
                "count"))))))))

(deftest full-restore-emits-restored-event-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true})

           seen
           (atom nil)]

       (.addEventListener
        root
        "gesso:live-continuity:restored"
        (fn [event]
          (reset!
           seen
           (.-detail event))))

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (continuity/capture!
         root)

        (continuity/restore-after-layout!
         root)

        (is (identical?
             root
             (aget
              @seen
              "root")))

        (is (identical?
             target
             (aget
              @seen
              "target")))

        (is (= "continuity-target"
               (aget
                @seen
                "targetId"))))))))

;; -----------------------------------------------------------------------------
;; HTMX/SSE event adaptation
;; -----------------------------------------------------------------------------

(deftest event-detail-and-field-test
  (let [detail
        #js {:target "value"}

        e
        (event detail)]

    (is (identical?
         detail
         (continuity/event-detail e)))

    (is (= "value"
           (continuity/detail-field
            e
            "target")))))

(deftest event-elements-collects-distinct-dom-elements-test
  (let [target
        (element "div")

        elt
        (element "button")

        source
        (element "form")

        request-element
        (element "input")

        e
        (event
         #js {:target target
              :elt elt
              :source source
              :requestConfig
              #js {:elt request-element}})]

    (is (= [target
            elt
            source
            request-element]
           (continuity/event-elements
            e)))))

(deftest event-source-precedence-test
  (let [elt
        (element
         "button"
         {:id "elt"})

        source
        (element
         "button"
         {:id "source"})

        request-element
        (element
         "button"
         {:id "request"})

        e
        (event
         #js {:elt elt
              :source source
              :requestConfig
              #js {:elt request-element}})]

    (is (identical?
         elt
         (continuity/event-source
          e)))))

(deftest event-source-falls-back-through-known-fields-test
  (let [source
        (element "button")

        request-element
        (element "button")]

    (is (identical?
         source
         (continuity/event-source
          (event
           #js {:source source}))))

    (is (identical?
         request-element
         (continuity/event-source
          (event
           #js {:requestConfig
                #js {:elt
                     request-element}}))))))

(deftest event-target-ids-accept-elements-and-hash-selectors-test
  (let [target
        (element
         "div"
         {:id "target-a"})

        swapped
        (element
         "div"
         {:id "target-b"})

        e
        (event
         #js {:target target
              :oobTarget "#target-c"
              :swappedElement swapped})]

    (is (= #{"target-a"
             "target-b"
             "target-c"}
           (continuity/event-target-ids
            e)))))

(deftest roots-from-event-resolves-direct-document-and-captured-roots-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{direct-root :root
            direct-target :target}
           (continuity-tree
            sandbox-root
            "direct-target"
            {:enabled true})

           {captured-root :root}
           (continuity-tree
            sandbox-root
            "captured-target"
            {:enabled true})

           detached-old-target
           (element
            "div"
            {:id "captured-target"})

           e
           (event
            #js {:target direct-target
                 :oobTarget "#captured-target"})]

       (reset!
        continuity/slots
        {"captured-target"
         {:root captured-root
          :target-id "captured-target"}})

       (.remove
        (continuity/target
         captured-root))

       (let [roots
             (continuity/roots-from-event
              e)]

         (is (some
              #(identical?
                direct-root
                %)
              roots))

         (is (some
              #(identical?
                captured-root
                %)
              roots))

         (is (= 2
                (count roots))))

       ;; Keep the local binding used so accidental compiler warnings do not
       ;; obscure the test's intent.
       (is (= "captured-target"
              (.-id detached-old-target)))))))

(deftest capture-from-event-captures-each-affected-root-test
  (let [root-a
        (element "div")

        root-b
        (element "div")

        source
        (element "button")

        calls
        (atom [])

        e
        (event
         #js {:elt source})]

    (with-redefs
     [continuity/roots-from-event
      (fn [_event]
        [root-a
         root-b])

      continuity/capture!
      (fn [root actual-source]
        (swap!
         calls
         conj
         [root
          actual-source]))]

      (is (true?
           (continuity/capture-from-event!
            e)))

      (is (= [[root-a source]
              [root-b source]]
             @calls)))))

(deftest restore-event-adapters-call-each-root-test
  (let [root-a
        (element "div")

        root-b
        (element "div")

        e
        (event #js {})

        immediate
        (atom [])

        after
        (atom [])

        scheduled
        (atom [])]

    (with-redefs
     [continuity/roots-from-event
      (fn [_event]
        [root-a
         root-b])

      continuity/restore-immediate!
      (fn [root]
        (swap!
         immediate
         conj
         root))

      continuity/restore-after-layout!
      (fn [root]
        (swap!
         after
         conj
         root))

      continuity/after-layout!
      (fn [f]
        (swap!
         scheduled
         conj
         f))]

      (is (true?
           (continuity/restore-immediate-from-event!
            e)))

      (is (= [root-a
              root-b]
             @immediate))

      (is (true?
           (continuity/restore-after-layout-from-event!
            e)))

      (is (= 2
             (count
              @scheduled)))

      (doseq [f
              @scheduled]
        (f))

      (is (= [root-a
              root-b]
             @after)))))

(deftest restore-from-event-runs-both-adapters-test
  (let [e
        (event #js {})

        calls
        (atom [])]

    (with-redefs
     [continuity/restore-immediate-from-event!
      (fn [actual]
        (swap!
         calls
         conj
         [:immediate
          actual])
        true)

      continuity/restore-after-layout-from-event!
      (fn [actual]
        (swap!
         calls
         conj
         [:after
          actual])
        true)]

      (is (true?
           (continuity/restore-from-event!
            e)))

      (is (= [[:immediate e]
              [:after e]]
             @calls)))))

;; -----------------------------------------------------------------------------
;; Cleanup
;; -----------------------------------------------------------------------------

(deftest cleanup-element-releases-root-and-nested-root-slots-test
  (let [outer
        (element
         "section"
         {continuity/continuity-attr
          "true"

          continuity/continuity-fragment-attr-key
          "outer-target"})

        nested
        (append!
         outer
         (element
          "section"
          {continuity/continuity-attr
           "true"

           continuity/continuity-fragment-attr-key
           "nested-target"}))

        released
        (atom [])]

    (reset!
     continuity/slots
     {"outer-target"
      {:target-id "outer-target"
       :root outer}

      "nested-target"
      {:target-id "nested-target"
       :root nested}})

    (with-redefs
     [continuity/release-slot!
      (fn [slot]
        (swap!
         released
         conj
         (:target-id slot))
        (swap!
         continuity/slots
         dissoc
         (:target-id slot))
        slot)]

      (is (true?
           (continuity/cleanup-element!
            outer)))

      (is (= #{"outer-target"
               "nested-target"}
             (set
              @released)))

      (is (= {}
             @continuity/slots)))))

(deftest cleanup-element-without-continuity-is-no-op-test
  (reset!
   continuity/slots
   {"other"
    {:target-id "other"}})

  (is (true?
       (continuity/cleanup-element!
        (element "div"))))

  (is (= {"other"
          {:target-id "other"}}
         @continuity/slots)))

;; -----------------------------------------------------------------------------
;; Diagnostics
;; -----------------------------------------------------------------------------

(deftest slot-summaries-are-dom-light-test
  (let [button
        (element "button")]

    (reset!
     continuity/slots
     {"target-a"
      {:target-id "target-a"
       :captured-at 123
       :source button
       :captured
       [{:type "inputs"}
        {:type "focus"}]
       :height-lock
       {:height 50}}

      "target-b"
      {:target-id "target-b"
       :captured-at 456
       :source nil
       :captured []
       :height-lock nil}})

    (is (= #{{:target-id "target-a"
              :captured-at 123
              :source "button"
              :box-types
              ["inputs"
               "focus"]
              :height-locked? true}

             {:target-id "target-b"
              :captured-at 456
              :source nil
              :box-types []
              :height-locked? false}}
           (set
            (continuity/slot-summaries))))))

;; -----------------------------------------------------------------------------
;; JS continuity box
;; -----------------------------------------------------------------------------

(deftest built-in-js-box-resolves-global-capture-and-restore-functions-test
  (continuity/register-built-in-boxes!)

  (let [capture-calls
        (atom [])

        restore-calls
        (atom [])

        root
        (element "div")

        target
        (append!
         root
         (element "div"))]

    (aset
     js/window
     "__gessoContinuityFunctions"
     #js {:capture
          (fn [actual-root actual-target box]
            (swap!
             capture-calls
             conj
             [actual-root
              actual-target
              (js->clj
               box
               :keywordize-keys true)])

            #js {:value 42})

          :restore
          (fn [actual-root actual-target box state]
            (swap!
             restore-calls
             conj
             [actual-root
              actual-target
              (js->clj
               box
               :keywordize-keys true)
              (js->clj
               state
               :keywordize-keys true)]))})

    (try
      (let [captured
            (continuity/capture-box
             root
             target
             {:type "js"
              :capture
              "__gessoContinuityFunctions.capture"
              :restore
              "__gessoContinuityFunctions.restore"
              :name "custom"})]

        (is (= {:value 42}
               (:state captured)))

        (continuity/restore-box!
         root
         target
         captured)

        (is (= 1
               (count @capture-calls)))

        (is (= 1
               (count @restore-calls)))

        (is (= {:value 42}
               (nth
                (first
                 @restore-calls)
                3))))

      (finally
        (js-delete
         js/window
         "__gessoContinuityFunctions")))))

;; -----------------------------------------------------------------------------
;; Integration: preservation across target replacement
;; -----------------------------------------------------------------------------

(deftest input-and-details-state-survive-authoritative-target-replacement-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :boxes
             [{:type "details-open"}]
             :preserve
             {:inputs true}})

           details
           (append!
            target
            (element
             "details"
             {:id "request-1"}))

           input
           (append!
            details
            (element
             "input"
             {:id "draft"
              :type "text"}))]

       (set!
        (.-open details)
        true)

       (set!
        (.-value input)
        "customer draft")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (continuity/capture!
         root)

        (let [replacement
              (element
               "div"
               {:id "continuity-target"})

              replacement-details
              (append!
               replacement
               (element
                "details"
                {:id "request-1"}))

              replacement-input
              (append!
               replacement-details
               (element
                "input"
                {:id "draft"
                 :type "text"}))]

          (set!
           (.-open replacement-details)
           false)

          (set!
           (.-value replacement-input)
           "authoritative default")

          (.replaceWith
           target
           replacement)

          (testing "details restoration happens before the layout boundary"
            (continuity/restore-immediate!
             root)

            (is (true?
                 (.-open replacement-details)))

            (is (= "authoritative default"
                   (.-value replacement-input))))

          (testing "input restoration follows after layout"
            (continuity/restore-after-layout!
             root)

            (is (= "customer draft"
                   (.-value replacement-input)))

            (is (nil?
                 (continuity/captured-slot
                  root))))))))))

(deftest continuity-preserves-user-state-not-server-structure-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :preserve
             {:inputs true}})

           input
           (append!
            target
            (element
             "input"
             {:id "draft"
              :type "text"}))

           old-label
           (append!
            target
            (text!
             (element
              "span"
              {:id "status"})
             "Old authoritative status"))]

       (set!
        (.-value input)
        "typed locally")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (continuity/capture!
         root)

        (let [replacement
              (element
               "div"
               {:id "continuity-target"})

              replacement-input
              (append!
               replacement
               (element
                "input"
                {:id "draft"
                 :type "text"}))

              new-label
              (append!
               replacement
               (text!
                (element
                 "span"
                 {:id "status"})
                "New authoritative status"))]

          (set!
           (.-value replacement-input)
           "server default")

          (.replaceWith
           target
           replacement)

          (continuity/restore-after-layout!
           root)

          (testing "browser-owned input state survives"
            (is (= "typed locally"
                   (.-value replacement-input))))

          (testing "server-owned structure/content remains authoritative"
            (is (= "New authoritative status"
                   (.-textContent new-label)))

            (is (nil?
                 (.querySelector
                  replacement
                  "#old-only"))))

          (is (= "Old authoritative status"
                 (.-textContent old-label)))))))))

;; -----------------------------------------------------------------------------
;; Integration: callback means restoration completed
;; -----------------------------------------------------------------------------

(deftest completion-callback-observes-restored-input-state-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            {:enabled true
             :preserve
             {:inputs true}})

           input
           (append!
            target
            (element
             "input"
             {:id "draft"
              :type "text"}))

           observed
           (atom nil)]

       (set!
        (.-value input)
        "before")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)

         continuity/after-layout!
         (fn [f]
           (f))]

        (continuity/capture!
         root)

        (set!
         (.-value input)
         "after replacement")

        (continuity/restore!
         root
         (fn [{:keys [target slot]}]
           (reset!
            observed
            {:value
             (.-value
              (.querySelector
               target
               "#draft"))

             :slot
             slot})))

        (is (= "before"
               (:value @observed)))

        (is (map?
             (:slot @observed)))

        (is (nil?
             (continuity/captured-slot
              root))))))))

;; -----------------------------------------------------------------------------
;; Integration: stable root may survive target replacement
;; -----------------------------------------------------------------------------

(deftest slot-is-keyed-by-stable-target-id-not-old-node-identity-test
  (with-sandbox*
   (fn [sandbox-root]
     (let [{:keys [root target]}
           (continuity-tree
            sandbox-root
            "replaceable"
            {:enabled true
             :preserve
             {:inputs true}})

           input
           (append!
            target
            (element
             "input"
             {:id "draft"
              :type "text"}))]

       (set!
        (.-value input)
        "preserve me")

       (with-redefs
        [continuity/window-scroll-state
         (fn []
           {:kind :window
            :x 0
            :y 0})

         continuity/lock-height!
         (fn [_root _target]
           nil)]

        (let [slot
              (continuity/capture!
               root)

              replacement
              (element
               "div"
               {:id "replaceable"})

              replacement-input
              (append!
               replacement
               (element
                "input"
                {:id "draft"
                 :type "text"}))]

          (.replaceWith
           target
           replacement)

          (is (not
               (identical?
                target
                (continuity/target
                 root))))

          (is (identical?
               slot
               (continuity/captured-slot
                root)))

          (continuity/restore-after-layout!
           root)

          (is (= "preserve me"
                 (.-value replacement-input)))))))))

;; -----------------------------------------------------------------------------
;; Initialization is deliberately listener-free
;; -----------------------------------------------------------------------------

(deftest initialize-only-installs-boxes-test
  (reset!
   continuity/boxes
   {})

  (let [before
        (.-onclick js/document)]

    (is (true?
         (continuity/initialize!)))

    (is (= before
           (.-onclick js/document)))

    (is (contains?
         @continuity/boxes
         "inputs"))

    (is (contains?
         @continuity/boxes
         "focus"))))

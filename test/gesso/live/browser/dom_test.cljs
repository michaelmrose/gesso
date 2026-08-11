(ns gesso.live.browser.dom-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.dom :as dom]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Test DOM helpers
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

(defn- text!
  [node value]
  (set! (.-textContent node)
        value)
  node)

(defn- append!
  [parent child]
  (.appendChild parent child)
  child)

(defn- sandbox
  []
  (let [root
        (element
         "div"
         {:data-gesso-dom-test
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
      (f root)
      (finally
        (.remove root)))))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f)
          ex-data))

(defn- child-tags
  [node]
  (mapv
   #(.-tagName %)
   (array-seq
    (.-children node))))

(defn- child-text
  [node]
  (mapv
   #(.-textContent %)
   (array-seq
    (.-children node))))

(defn- canonical-attrs
  ([scope revision]
   (canonical-attrs
    scope
    revision
    true))
  ([scope revision canonical]
   {(dom/attr-name protocol/canonical-attr)
    (if canonical "true" "false")

    (dom/attr-name protocol/scope-attr)
    scope

    (dom/attr-name protocol/revision-attr)
    (protocol/revision->wire revision)}))

(defn- canonical-element
  ([tag scope revision]
   (canonical-element
    tag
    scope
    revision
    nil))
  ([tag scope revision attrs]
   (element
    tag
    (merge
     (canonical-attrs
      scope
      revision)
     attrs))))

;; -----------------------------------------------------------------------------
;; Attribute names
;; -----------------------------------------------------------------------------

(deftest attr-name-test
  (is (= "data-gesso-optimistic-canonical"
         (dom/attr-name
          :data-gesso-optimistic-canonical)))

  (is (= "data-test"
         (dom/attr-name
          "data-test")))

  (is (= "42"
         (dom/attr-name
          42))))

(deftest transport-only-attrs-test
  (is (= #{"hx-swap-oob"
           "data-hx-swap-oob"}
         dom/transport-only-attrs)))

;; -----------------------------------------------------------------------------
;; Node predicates
;; -----------------------------------------------------------------------------

(deftest element-predicate-test
  (let [node
        (element "div")

        text
        (.createTextNode
         js/document
         "hello")]

    (is (dom/element?
         node))

    (is (false?
         (dom/element?
          text)))

    (is (false?
         (dom/element?
          nil)))))

(deftest template-predicate-test
  (is (dom/template?
       (element "template")))

  (is (false?
       (dom/template?
        (element "div")))))

(deftest connected-predicate-test
  (with-sandbox*
   (fn [root]
     (let [child
           (append!
            root
            (element "div"))

           detached
           (element "div")]

       (is (dom/connected?
            root))

       (is (dom/connected?
            child))

       (is (false?
            (dom/connected?
             detached)))

       (is (false?
            (dom/connected?
             nil)))))))

;; -----------------------------------------------------------------------------
;; Lookup
;; -----------------------------------------------------------------------------

(deftest by-id-test
  (with-sandbox*
   (fn [root]
     (let [node
           (append!
            root
            (element
             "div"
             {:id "gesso-dom-by-id"}))]

       (is (identical?
            node
            (dom/by-id
             "gesso-dom-by-id")))

       (is (nil?
            (dom/by-id
             "missing-gesso-dom-id")))

       (is (nil?
            (dom/by-id
             "")))

       (is (nil?
            (dom/by-id
             nil)))))))

(deftest query-one-test
  (with-sandbox*
   (fn [root]
     (let [first-node
           (append!
            root
            (element
             "span"
             {:data-kind "first"}))

           _second-node
           (append!
            root
            (element
             "span"
             {:data-kind "second"}))]

       (is (identical?
            first-node
            (dom/query-one
             root
             "[data-kind='first']")))

       (is (nil?
            (dom/query-one
             root
             "[data-kind='missing']")))

       (testing "invalid selectors are optional lookup failures"
         (is (nil?
              (dom/query-one
               root
               "["))))

       (is (nil?
            (dom/query-one
             root
             "")))

       (is (nil?
            (dom/query-one
             nil
             "span")))))))

(deftest query-all-test
  (with-sandbox*
   (fn [root]
     (append!
      root
      (element
       "span"
       {:data-kind "x"}))

     (append!
      root
      (element
       "span"
       {:data-kind "x"}))

     (is (= 2
            (count
             (dom/query-all
              root
              "[data-kind='x']"))))

     (is (= []
            (dom/query-all
             root
             "[data-kind='missing']")))

     (testing "invalid selectors are represented as an empty result"
       (is (= []
              (dom/query-all
               root
               "["))))

     (is (= []
            (dom/query-all
             nil
             "span"))))))

(deftest require-element-test
  (let [node
        (element "div")]

    (is (identical?
         node
         (dom/require-element!
          node)))

    (let [data
          (thrown-data
           #(dom/require-element!
             nil))]
      (is (= :gesso.live.browser.dom/missing-element
             (:error/type data)))
      (is (nil?
           (:element data))))

    (let [error
          (thrown
           #(dom/require-element!
             "Custom missing target."
             nil
             {:execution-id "e-1"}))]
      (is (= "Custom missing target."
             (ex-message error)))
      (is (= :gesso.live.browser.dom/missing-element
             (:error/type
              (ex-data error))))
      (is (= "e-1"
             (:execution-id
              (ex-data error)))))))

(deftest require-connected-test
  (with-sandbox*
   (fn [root]
     (let [connected
           (append!
            root
            (element "div"))

           detached
           (element "div")]

       (is (identical?
            connected
            (dom/require-connected!
             connected
             {:execution-id "e-1"})))

       (let [data
             (thrown-data
              #(dom/require-connected!
                detached
                {:execution-id "e-2"}))]
         (is (= :gesso.live.browser.dom/detached-target
                (:error/type data)))
         (is (= "e-2"
                (:execution-id data)))
         (is (identical?
              detached
              (:element data))))))))

(deftest require-query-one-test
  (with-sandbox*
   (fn [root]
     (let [node
           (append!
            root
            (element
             "button"
             {:data-action "claim"}))]

       (is (identical?
            node
            (dom/require-query-one!
             root
             "[data-action='claim']")))

       (doseq [selector
               ["[data-action='missing']"
                "["]]
         (let [data
               (thrown-data
                #(dom/require-query-one!
                  root
                  selector))]
           (is (= :gesso.live.browser.dom/missing-target
                  (:error/type data)))
           (is (= selector
                  (:selector data)))
           (is (identical?
                root
                (:root data)))))))))

;; -----------------------------------------------------------------------------
;; Selector helpers
;; -----------------------------------------------------------------------------

(deftest matches-test
  (let [node
        (element
         "button"
         {:data-action "claim"
          :class "primary"})]

    (is (dom/matches?
         node
         "button.primary[data-action='claim']"))

    (is (false?
         (dom/matches?
          node
          "div")))

    (is (false?
         (dom/matches?
          node
          "[")))

    (is (false?
         (dom/matches?
          nil
          "button")))))

(deftest closest-test
  (let [outer
        (element
         "article"
         {:data-card "request-1"})

        middle
        (append!
         outer
         (element "div"))

        button
        (append!
         middle
         (element "button"))]

    (is (identical?
         outer
         (dom/closest
          button
          "[data-card]")))

    (is (identical?
         button
         (dom/closest
          button
          "button")))

    (is (nil?
         (dom/closest
          button
          "[data-missing]")))

    (is (nil?
         (dom/closest
          button
          "[")))))

(deftest closest-with-attr-test
  (let [outer
        (element
         "article"
         {:data-card "request-1"})

        middle
        (append!
         outer
         (element "div"))

        button
        (append!
         middle
         (element
          "button"
          {:data-action "claim"}))]

    (is (identical?
         button
         (dom/closest-with-attr
          button
          :data-action)))

    (is (identical?
         outer
         (dom/closest-with-attr
          button
          :data-card)))

    (is (nil?
         (dom/closest-with-attr
          button
          :data-missing)))))

;; -----------------------------------------------------------------------------
;; Attribute helpers
;; -----------------------------------------------------------------------------

(deftest attribute-read-write-test
  (let [node
        (element "div")]

    (is (nil?
         (dom/attr
          node
          :data-state)))

    (is (false?
         (dom/has-attr?
          node
          :data-state)))

    (is (identical?
         node
         (dom/set-attr!
          node
          :data-state
          :pending)))

    (is (= "pending"
           (dom/attr
            node
            :data-state)))

    (is (dom/has-attr?
         node
         :data-state))

    (is (identical?
         node
         (dom/remove-attr!
          node
          :data-state)))

    (is (nil?
         (dom/attr
          node
          :data-state)))))

(deftest set-attr-nil-removes-attribute-test
  (let [node
        (element
         "div"
         {:data-state "pending"})]

    (dom/set-attr!
     node
     :data-state
     nil)

    (is (false?
         (dom/has-attr?
          node
          :data-state)))))

(deftest attribute-helpers-are-safe-on-non-elements-test
  (is (nil?
       (dom/attr
        nil
        :data-x)))

  (is (false?
       (dom/has-attr?
        nil
        :data-x)))

  (is (nil?
       (dom/set-attr!
        nil
        :data-x
        "x")))

  (is (nil?
       (dom/remove-attr!
        nil
        :data-x))))

(deftest truthy-attr-test
  (doseq [value
          ["" "true" "TRUE" "1" "yes-actually"
           "data-enabled"]]
    (let [node
          (element
           "div"
           {:data-enabled value})]
      (is (true?
           (dom/truthy-attr?
            node
            :data-enabled))
          (str "Expected truthy marker for " (pr-str value)))))

  (doseq [value
          ["false" "FALSE" "0" "no" "NO"]]
    (let [node
          (element
           "div"
           {:data-enabled value})]
      (is (false?
           (dom/truthy-attr?
            node
            :data-enabled))
          (str "Expected false marker for " (pr-str value)))))

  (is (nil?
       (dom/truthy-attr?
        (element "div")
        :data-enabled))))

;; -----------------------------------------------------------------------------
;; Canonical metadata
;; -----------------------------------------------------------------------------

(deftest canonical-authority-is-explicit-test
  (let [scope-wire
        "e:[:request \"request-1\"]"

        merely-scoped
        (element
         "details"
         {(dom/attr-name protocol/scope-attr)
          scope-wire

          (dom/attr-name protocol/revision-attr)
          "i:7"})

        canonical
        (canonical-element
         "details"
         scope-wire
         7)

        explicitly-false
        (element
         "details"
         (canonical-attrs
          scope-wire
          7
          false))]

    (is (false?
         (dom/canonical?
          merely-scoped)))

    (is (dom/canonical?
         canonical))

    (is (false?
         (dom/canonical?
          explicitly-false)))

    (is (= scope-wire
           (dom/scope canonical)))

    (is (= "i:7"
           (dom/revision-wire
            canonical)))

    (is (= 7
           (dom/revision
            canonical)))))

(deftest opaque-revision-decodes-as-string-test
  (let [node
        (canonical-element
         "div"
         "scope"
         "opaque-revision")]

    (is (= "s:opaque-revision"
           (dom/revision-wire node)))

    (is (= "opaque-revision"
           (dom/revision node)))))

(deftest malformed-revision-is-not-silently-accepted-test
  (let [node
        (element
         "div"
         {(dom/attr-name protocol/canonical-attr)
          "true"

          (dom/attr-name protocol/revision-attr)
          "bogus"})]

    (is (some?
         (thrown
          #(dom/revision node))))))

(deftest canonical-for-scope-test
  (let [node
        (canonical-element
         "div"
         "scope-a"
         1)]

    (is (dom/canonical-for-scope?
         node
         "scope-a"))

    (is (false?
         (dom/canonical-for-scope?
          node
          "scope-b")))

    (dom/remove-attr!
     node
     protocol/canonical-attr)

    (is (false?
         (dom/canonical-for-scope?
          node
          "scope-a")))))

;; -----------------------------------------------------------------------------
;; Canonical lookup and ordering
;; -----------------------------------------------------------------------------

(deftest canonical-elements-includes-root-and-descendants-test
  (let [root
        (canonical-element
         "section"
         "scope-a"
         1)

        same-scope
        (append!
         root
         (canonical-element
          "div"
          "scope-a"
          2))

        other-scope
        (append!
         root
         (canonical-element
          "div"
          "scope-b"
          9))

        noncanonical
        (append!
         root
         (element
          "div"
          {(dom/attr-name protocol/scope-attr)
           "scope-a"}))]

    (is (= #{root
             same-scope
             other-scope}
           (set
            (dom/canonical-elements
             root))))

    (is (= #{root
             same-scope}
           (set
            (dom/canonical-elements
             root
             "scope-a"))))

    (is (not
         (contains?
          (set
           (dom/canonical-elements
            root))
          noncanonical)))

    (is (= []
           (dom/canonical-elements
            nil)))))

(deftest compare-element-revisions-test
  (let [one
        (canonical-element
         "div"
         "scope"
         1)

        two
        (canonical-element
         "div"
         "scope"
         2)

        opaque-a
        (canonical-element
         "div"
         "scope"
         "a")

        opaque-b
        (canonical-element
         "div"
         "scope"
         "b")]

    (is (= :older
           (dom/compare-element-revisions
            one
            two)))

    (is (= :newer
           (dom/compare-element-revisions
            two
            one)))

    (is (= :same
           (dom/compare-element-revisions
            one
            one)))

    (is (= :incomparable
           (dom/compare-element-revisions
            opaque-a
            opaque-b)))))

(deftest newest-canonical-none-test
  (is (= {:status :none}
         (dom/newest-canonical
          [])))

  (is (= {:status :none}
         (dom/newest-canonical
          [(element "div")
           (element "span")]))))

(deftest newest-canonical-one-test
  (let [node
        (canonical-element
         "div"
         "scope"
         1)]

    (is (= {:status :selected
            :element node}
           (dom/newest-canonical
            [node])))))

(deftest newest-canonical-selects-largest-numeric-revision-test
  (let [one
        (canonical-element
         "div"
         "scope"
         1)

        three
        (canonical-element
         "div"
         "scope"
         3)

        two
        (canonical-element
         "div"
         "scope"
         2)

        result
        (dom/newest-canonical
         [one
          three
          two])]

    (is (= :selected
           (:status result)))

    (is (identical?
         three
         (:element result)))))

(deftest newest-canonical-duplicate-revision-is-ambiguous-test
  (let [left
        (canonical-element
         "div"
         "scope"
         7)

        right
        (canonical-element
         "div"
         "scope"
         7)

        result
        (dom/newest-canonical
         [left
          right])]

    (is (= :ambiguous
           (:status result)))

    (is (= :duplicate-revision
           (:reason result)))

    (is (= [left right]
           (:elements result)))))

(deftest newest-canonical-same-node-repeated-is-not-ambiguous-test
  (let [node
        (canonical-element
         "div"
         "scope"
         7)

        result
        (dom/newest-canonical
         [node
          node])]

    (is (= :selected
           (:status result)))

    (is (identical?
         node
         (:element result)))))

(deftest newest-canonical-distinct-opaque-revisions-are-ambiguous-test
  (let [left
        (canonical-element
         "div"
         "scope"
         "alpha")

        right
        (canonical-element
         "div"
         "scope"
         "beta")

        result
        (dom/newest-canonical
         [left
          right])]

    (is (= :ambiguous
           (:status result)))

    (is (= :incomparable-revisions
           (:reason result)))

    (is (= [left right]
           (:elements result)))))

(deftest incoming-revision-status-test
  (let [current
        (canonical-element
         "div"
         "scope"
         7)

        newer
        (canonical-element
         "div"
         "scope"
         8)

        same
        (canonical-element
         "div"
         "scope"
         7)

        older
        (canonical-element
         "div"
         "scope"
         6)

        opaque
        (canonical-element
         "div"
         "scope"
         "opaque")

        noncanonical
        (element
         "div"
         {(dom/attr-name protocol/revision-attr)
          "i:9"})]

    (is (= :no-current
           (dom/incoming-revision-status
            nil
            newer)))

    (is (= :no-current
           (dom/incoming-revision-status
            (element "div")
            newer)))

    (is (= :not-canonical
           (dom/incoming-revision-status
            current
            noncanonical)))

    (is (= :newer
           (dom/incoming-revision-status
            current
            newer)))

    (is (= :same
           (dom/incoming-revision-status
            current
            same)))

    (is (= :older
           (dom/incoming-revision-status
            current
            older)))

    (is (= :incomparable
           (dom/incoming-revision-status
            current
            opaque)))))

;; -----------------------------------------------------------------------------
;; Root compatibility
;; -----------------------------------------------------------------------------

(deftest tag-name-test
  (is (= "DETAILS"
         (dom/tag-name
          (element "details"))))

  (is (nil?
       (dom/tag-name
        nil))))

(deftest same-root-tag-test
  (is (dom/same-root-tag?
       (element "details")
       (element "details")))

  (is (false?
       (dom/same-root-tag?
        (element "details")
        (element "div"))))

  (is (false?
       (dom/same-root-tag?
        nil
        (element "div")))))

(deftest compatible-root-test
  (let [current
        (element "details")

        replacement
        (element "details")]

    (is (identical?
         replacement
         (dom/assert-compatible-root!
          current
          replacement)))))

(deftest root-tag-change-is-framework-error-test
  (let [current
        (element "button")

        replacement
        (element "details")

        data
        (thrown-data
         #(dom/assert-compatible-root!
           current
           replacement))]

    (is (= :gesso.live.browser.dom/root-tag-mismatch
           (:error/type data)))

    (is (= "BUTTON"
           (:current-tag data)))

    (is (= "DETAILS"
           (:replacement-tag data)))))

(deftest compatible-root-requires-elements-test
  (let [data
        (thrown-data
         #(dom/assert-compatible-root!
           nil
           (element "div")))]

    (is (= :gesso.live.browser.dom/invalid-root
           (:error/type data)))))

;; -----------------------------------------------------------------------------
;; One-root parsing
;; -----------------------------------------------------------------------------

(deftest one-element-root-accepts-whitespace-around-element-test
  (let [container
        (element "div")]

    (.appendChild
     container
     (.createTextNode
      js/document
      " \n\t "))

    (let [child
          (append!
           container
           (element "details"))]

      (.appendChild
       container
       (.createTextNode
        js/document
        "\n"))

      (is (identical?
           child
           (dom/one-element-root
            container))))))

(deftest one-element-root-rejects-no-root-test
  (let [container
        (element "div")

        data
        (thrown-data
         #(dom/one-element-root
           container))]

    (is (= :gesso.live.browser.dom/not-one-root
           (:error/type data)))

    (is (= 0
           (:child-count data)))))

(deftest one-element-root-rejects-multiple-elements-test
  (let [container
        (element "div")]

    (append!
     container
     (element "div"))

    (append!
     container
     (element "span"))

    (let [data
          (thrown-data
           #(dom/one-element-root
             container))]
      (is (= :gesso.live.browser.dom/not-one-root
             (:error/type data)))
      (is (= 2
             (:child-count data))))))

(deftest one-element-root-rejects-non-whitespace-text-test
  (let [container
        (element "div")]

    (.appendChild
     container
     (.createTextNode
      js/document
      "text"))

    (append!
     container
     (element "div"))

    (let [data
          (thrown-data
           #(dom/one-element-root
             container))]
      (is (= :gesso.live.browser.dom/not-one-root
             (:error/type data)))
      (is (= 2
             (:child-count data))))))

(deftest one-element-root-rejects-comment-plus-element-test
  (let [container
        (element "div")]

    (.appendChild
     container
     (.createComment
      js/document
      "comment"))

    (append!
     container
     (element "div"))

    (let [data
          (thrown-data
           #(dom/one-element-root
             container))]
      (is (= :gesso.live.browser.dom/not-one-root
             (:error/type data)))
      (is (= 2
             (:child-count data))))))

;; -----------------------------------------------------------------------------
;; Templates
;; -----------------------------------------------------------------------------

(deftest template-root-test
  (let [template
        (element "template")]

    (set!
     (.-innerHTML template)
     "<details data-card='request-1'><summary>Pending</summary></details>")

    (let [first-root
          (dom/template-root
           template)

          second-root
          (dom/template-root
           template)]

      (is (= "DETAILS"
             (.-tagName first-root)))

      (is (= "request-1"
             (.getAttribute
              first-root
              "data-card")))

      (is (= "Pending"
             (.-textContent first-root)))

      (testing "each lookup is a fresh detached clone"
        (is (not
             (identical?
              first-root
              second-root)))

        (.setAttribute
         first-root
         "data-mutated"
         "true")

        (is (nil?
             (.getAttribute
              second-root
              "data-mutated")))))))

(deftest template-root-requires-template-test
  (let [data
        (thrown-data
         #(dom/template-root
           (element "div")))]

    (is (= :gesso.live.browser.dom/not-template
           (:error/type data)))))

(deftest template-root-requires-one-element-root-test
  (let [template
        (element "template")]

    (set!
     (.-innerHTML template)
     "<div>A</div><div>B</div>")

    (is (= :gesso.live.browser.dom/not-one-root
           (:error/type
            (thrown-data
             #(dom/template-root
               template)))))))

(deftest templates-and-template-by-name-test
  (with-sandbox*
   (fn [root]
     (let [template-a
           (append!
            root
            (element
             "template"
             {(dom/attr-name protocol/template-attr)
              "claim"}))

           template-b
           (append!
            root
            (element
             "template"
             {(dom/attr-name protocol/template-attr)
              "cancel"}))

           unrelated
           (append!
            root
            (element "template"))]

       (is (= #{template-a
                template-b}
              (set
               (dom/templates
                root))))

       (is (identical?
            template-a
            (dom/template-by-name
             root
             "claim")))

       (is (identical?
            template-b
            (dom/template-by-name
             root
             :cancel)))

       (is (nil?
            (dom/template-by-name
             root
             "missing")))

       (is (not
            (contains?
             (set
              (dom/templates
               root))
             unrelated)))))))

(deftest duplicate-template-name-is-framework-error-test
  (with-sandbox*
   (fn [root]
     (append!
      root
      (element
       "template"
       {(dom/attr-name protocol/template-attr)
        "claim"}))

     (append!
      root
      (element
       "template"
       {(dom/attr-name protocol/template-attr)
        "claim"}))

     (let [data
           (thrown-data
            #(dom/template-by-name
              root
              "claim"))]

       (is (= :gesso.live.browser.dom/duplicate-template
              (:error/type data)))

       (is (= "claim"
              (:template-name data)))

       (is (= 2
              (:count data)))))))

;; -----------------------------------------------------------------------------
;; HTML parsing
;; -----------------------------------------------------------------------------

(deftest parse-one-root-test
  (let [node
        (dom/parse-one-root
         "  <details id='request-1' open><summary>Claimed</summary></details>  ")]

    (is (= "DETAILS"
           (.-tagName node)))

    (is (= "request-1"
           (.-id node)))

    (is (true?
         (.-open node)))

    (is (false?
         (dom/connected?
          node)))))

(deftest parse-one-root-requires-string-test
  (let [data
        (thrown-data
         #(dom/parse-one-root
           [:div]))]

    (is (= :gesso.live.browser.dom/invalid-html
           (:error/type data)))

    (is (= [:div]
           (:value data)))))

(deftest parse-one-root-rejects-multiple-roots-test
  (is (= :gesso.live.browser.dom/not-one-root
         (:error/type
          (thrown-data
           #(dom/parse-one-root
             "<div>A</div><div>B</div>"))))))

(deftest parse-one-root-rejects-significant-text-test
  (is (= :gesso.live.browser.dom/not-one-root
         (:error/type
          (thrown-data
           #(dom/parse-one-root
             "text<div>A</div>"))))))

;; -----------------------------------------------------------------------------
;; Transport sanitization
;; -----------------------------------------------------------------------------

(deftest remove-attrs-test
  (let [node
        (element
         "div"
         {:a "1"
          :b "2"
          :c "3"})]

    (is (identical?
         node
         (dom/remove-attrs!
          node
          [:a :c])))

    (is (nil?
         (dom/attr
          node
          :a)))

    (is (= "2"
           (dom/attr
            node
            :b)))

    (is (nil?
         (dom/attr
          node
          :c)))))

(deftest sanitize-transport-attrs-removes-root-and-descendant-markers-test
  (let [root
        (element
         "div"
         {:hx-swap-oob "outerHTML"
          :data-hx-swap-oob "true"
          :data-keep "root"})

        child
        (append!
         root
         (element
          "section"
          {:hx-swap-oob "beforeend"
           :data-keep "child"}))

        grandchild
        (append!
         child
         (element
          "span"
          {:data-hx-swap-oob "true"}))]

    (is (identical?
         root
         (dom/sanitize-transport-attrs!
          root)))

    (doseq [node
            [root child grandchild]

            attr
            dom/transport-only-attrs]

      (is (false?
           (dom/has-attr?
            node
            attr))))

    (is (= "root"
           (dom/attr
            root
            :data-keep)))

    (is (= "child"
           (dom/attr
            child
            :data-keep)))))

(deftest sanitize-transport-attrs-requires-element-test
  (is (= :gesso.live.browser.dom/invalid-sanitize-root
         (:error/type
          (thrown-data
           #(dom/sanitize-transport-attrs!
             nil))))))

;; -----------------------------------------------------------------------------
;; Structural snapshots
;; -----------------------------------------------------------------------------

(deftest snapshot-captures-structural-metadata-test
  (let [node
        (canonical-element
         "details"
         "scope-a"
         7
         {:id "request-1"
          :open "open"})

        child
        (append!
         node
         (text!
          (element "span")
          "Original"))

        snapshot
        (dom/snapshot
         node)]

    (is (= "DETAILS"
           (:tag-name snapshot)))

    (is (= "scope-a"
           (:scope snapshot)))

    (is (= "i:7"
           (:revision-wire snapshot)))

    (is (true?
         (:canonical? snapshot)))

    (is (dom/element?
         (:node snapshot)))

    (is (not
         (identical?
          node
          (:node snapshot))))

    (is (= "Original"
           (.-textContent
            (:node snapshot))))

    (text!
     child
     "Mutated")

    (is (= "Original"
           (.-textContent
            (:node snapshot))))))

(deftest snapshot-requires-element-test
  (is (= :gesso.live.browser.dom/invalid-snapshot-root
         (:error/type
          (thrown-data
           #(dom/snapshot
             nil))))))

(deftest snapshot-node-is-fresh-each-time-test
  (let [snapshot
        (dom/snapshot
         (text!
          (element "div")
          "Original"))

        first-node
        (dom/snapshot-node
         snapshot)

        second-node
        (dom/snapshot-node
         snapshot)]

    (is (not
         (identical?
          first-node
          second-node)))

    (text!
     first-node
     "Mutated")

    (is (= "Original"
           (.-textContent
            second-node)))))

(deftest snapshot-node-validates-snapshot-test
  (doseq [snapshot
          [nil
           {}
           {:node nil}
           {:node "not-an-element"}]]

    (is (= :gesso.live.browser.dom/invalid-snapshot
           (:error/type
            (thrown-data
             #(dom/snapshot-node
               snapshot)))))))

;; -----------------------------------------------------------------------------
;; Attribute and child copying
;; -----------------------------------------------------------------------------

(deftest attributes-test
  (let [node
        (element
         "div"
         {:id "target"
          :data-state "pending"
          :aria-label "Example"})

        pairs
        (set
         (dom/attributes
          node))]

    (is (= #{["id" "target"]
             ["data-state" "pending"]
             ["aria-label" "Example"]}
           pairs))))

(deftest attributes-requires-element-test
  (is (= :gesso.live.browser.dom/invalid-attribute-root
         (:error/type
          (thrown-data
           #(dom/attributes
             nil))))))

(deftest restore-attributes-replaces-complete-set-test
  (let [target
        (element
         "div"
         {:id "old"
          :data-old "yes"})]

    (is (identical?
         target
         (dom/restore-attributes!
          target
          [["id" "new"]
           ["data-new" "yes"]])))

    (is (= "new"
           (.-id target)))

    (is (= "yes"
           (dom/attr
            target
            :data-new)))

    (is (false?
         (dom/has-attr?
          target
          :data-old)))))

(deftest restore-attributes-requires-element-target-test
  (is (= :gesso.live.browser.dom/invalid-attribute-target
         (:error/type
          (thrown-data
           #(dom/restore-attributes!
             nil
             []))))))

(deftest replace-children-from-deep-clones-source-children-test
  (let [target
        (element "div")

        old-child
        (append!
         target
         (text!
          (element "span")
          "Old"))

        source
        (element "div")

        source-child
        (append!
         source
         (text!
          (element "strong")
          "New"))]

    (is (identical?
         target
         (dom/replace-children-from!
          target
          source)))

    (is (= ["STRONG"]
           (child-tags target)))

    (is (= ["New"]
           (child-text target)))

    (is (not
         (identical?
          source-child
          (.-firstElementChild target))))

    (is (nil?
         (.-parentNode old-child)))

    (text!
     source-child
     "Changed source")

    (is (= "New"
           (.-textContent
            (.-firstElementChild target))))))

(deftest replace-children-from-validates-both-roots-test
  (is (= :gesso.live.browser.dom/invalid-copy-root
         (:error/type
          (thrown-data
           #(dom/replace-children-from!
             nil
             (element "div"))))))

  (is (= :gesso.live.browser.dom/invalid-copy-root
         (:error/type
          (thrown-data
           #(dom/replace-children-from!
             (element "div")
             nil))))))

;; -----------------------------------------------------------------------------
;; Stable DOM identity
;; -----------------------------------------------------------------------------

(deftest stable-identity-allows-same-id-test
  (is (true?
       (dom/assert-stable-identity!
        (element
         "div"
         {:id "request-1"})
        (element
         "div"
         {:id "request-1"})))))

(deftest stable-identity-allows-source-without-id-test
  (is (true?
       (dom/assert-stable-identity!
        (element
         "div"
         {:id "request-1"})
        (element
         "div")))))

(deftest stable-identity-allows-target-without-id-test
  (is (true?
       (dom/assert-stable-identity!
        (element "div")
        (element
         "div"
         {:id "request-1"})))))

(deftest stable-identity-rejects-id-change-test
  (let [data
        (thrown-data
         #(dom/assert-stable-identity!
           (element
            "div"
            {:id "request-1"})
           (element
            "div"
            {:id "request-2"})))]

    (is (= :gesso.live.browser.dom/target-id-mismatch
           (:error/type data)))

    (is (= "request-1"
           (:target-id data)))

    (is (= "request-2"
           (:source-id data)))))

;; -----------------------------------------------------------------------------
;; In-place copy
;; -----------------------------------------------------------------------------

(deftest copy-element-into-keeps-target-object-alive-test
  (with-sandbox*
   (fn [root]
     (let [target
           (append!
            root
            (element
             "details"
             {:id "request-1"
              :data-old "yes"}))

           _old-child
           (append!
            target
            (text!
             (element "summary")
             "Old"))

           source
           (element
            "details"
            {:id "request-1"
             :data-new "yes"
             :open "open"})

           _new-child
           (append!
            source
            (text!
             (element "summary")
             "New"))

           returned
           (dom/copy-element-into!
            target
            source)]

       (is (identical?
            target
            returned))

       (is (dom/connected?
            target))

       (is (identical?
            target
            (.-firstElementChild root)))

       (is (= "yes"
              (dom/attr
               target
               :data-new)))

       (is (false?
            (dom/has-attr?
             target
             :data-old)))

       (is (= "New"
              (.-textContent
               (.-firstElementChild target))))

       (is (true?
            (.-open target)))))))

(deftest copy-element-into-preserves-existing-id-when-source-omits-it-test
  (let [target
        (element
         "div"
         {:id "stable-id"
          :data-old "yes"})

        source
        (element
         "div"
         {:data-new "yes"})]

    (dom/copy-element-into!
     target
     source)

    (is (= "stable-id"
           (.-id target)))

    (is (= "yes"
           (dom/attr
            target
            :data-new)))))

(deftest copy-element-into-copies-source-id-when-target-has-none-test
  (let [target
        (element "div")

        source
        (element
         "div"
         {:id "source-id"})]

    (dom/copy-element-into!
     target
     source)

    (is (= "source-id"
           (.-id target)))))

(deftest copy-element-into-rejects-root-tag-change-before-mutation-test
  (let [target
        (text!
         (element
          "button"
          {:data-old "yes"})
         "Old")

        source
        (text!
         (element
          "details"
          {:data-new "yes"})
         "New")

        data
        (thrown-data
         #(dom/copy-element-into!
           target
           source))]

    (is (= :gesso.live.browser.dom/root-tag-mismatch
           (:error/type data)))

    (testing "the target remains untouched"
      (is (= "yes"
             (dom/attr
              target
              :data-old)))

      (is (= "Old"
             (.-textContent target))))))

(deftest copy-element-into-rejects-id-change-before-mutation-test
  (let [target
        (text!
         (element
          "div"
          {:id "old"
           :data-old "yes"})
         "Old")

        source
        (text!
         (element
          "div"
          {:id "new"
           :data-new "yes"})
         "New")

        data
        (thrown-data
         #(dom/copy-element-into!
           target
           source))]

    (is (= :gesso.live.browser.dom/target-id-mismatch
           (:error/type data)))

    (is (= "old"
           (.-id target)))

    (is (= "Old"
           (.-textContent target)))))

(deftest details-open-property-follows-source-test
  (let [target
        (element
         "details"
         {:open "open"})

        source
        (element "details")]

    (is (true?
         (.-open target)))

    (is (false?
         (.-open source)))

    (dom/copy-element-into!
     target
     source)

    (is (false?
         (.-open target)))))

;; -----------------------------------------------------------------------------
;; Canonical in-place copy
;; -----------------------------------------------------------------------------

(deftest copy-canonical-into-requires-explicit-authority-test
  (let [target
        (element "div")

        source
        (element
         "div"
         {(dom/attr-name protocol/scope-attr)
          "scope"})]

    (is (= :gesso.live.browser.dom/not-canonical
           (:error/type
            (thrown-data
             #(dom/copy-canonical-into!
               target
               source)))))))

(deftest copy-canonical-into-sanitizes-before-copy-test
  (let [target
        (element
         "div"
         {:id "target"})

        source
        (canonical-element
         "div"
         "scope"
         2
         {:id "target"
          :hx-swap-oob "outerHTML"})

        child
        (append!
         source
         (element
          "span"
          {:data-hx-swap-oob "true"
           :data-child "yes"}))]

    (dom/copy-canonical-into!
     target
     source)

    (is (dom/canonical?
         target))

    (is (= "scope"
           (dom/scope target)))

    (is (= 2
           (dom/revision target)))

    (is (false?
         (dom/has-attr?
          target
          :hx-swap-oob)))

    (let [installed-child
          (.-firstElementChild target)]

      (is (= "yes"
             (dom/attr
              installed-child
              :data-child)))

      (is (false?
           (dom/has-attr?
            installed-child
            :data-hx-swap-oob))))

    (testing "the detached canonical source is itself sanitized"
      (is (false?
           (dom/has-attr?
            source
            :hx-swap-oob)))

      (is (false?
           (dom/has-attr?
            child
            :data-hx-swap-oob))))))

;; -----------------------------------------------------------------------------
;; Whole-node replacement
;; -----------------------------------------------------------------------------

(deftest replace-installs-detached-compatible-node-test
  (with-sandbox*
   (fn [root]
     (let [current
           (append!
            root
            (text!
             (element "details")
             "Current"))

           replacement
           (text!
            (element "details")
            "Replacement")

           installed
           (dom/replace!
            current
            replacement)]

       (is (identical?
            replacement
            installed))

       (is (identical?
            replacement
            (.-firstElementChild root)))

       (is (false?
            (dom/connected?
             current)))

       (is (dom/connected?
            replacement))

       (is (= "Replacement"
              (.-textContent replacement)))))))

(deftest replace-rejects-detached-current-test
  (let [current
        (element "div")

        replacement
        (element "div")

        data
        (thrown-data
         #(dom/replace!
           current
           replacement))]

    (is (= :gesso.live.browser.dom/detached-target
           (:error/type data)))

    (is (identical?
         current
         (:current data)))))

(deftest replace-rejects-root-tag-change-test
  (with-sandbox*
   (fn [root]
     (let [current
           (append!
            root
            (element "button"))

           replacement
           (element "details")

           data
           (thrown-data
            #(dom/replace!
              current
              replacement))]

       (is (= :gesso.live.browser.dom/root-tag-mismatch
              (:error/type data)))

       (is (identical?
            current
            (.-firstElementChild root)))))))

;; -----------------------------------------------------------------------------
;; Canonical whole-node replacement
;; -----------------------------------------------------------------------------

(deftest replace-canonical-requires-explicit-canonical-marker-test
  (with-sandbox*
   (fn [root]
     (let [current
           (append!
            root
            (element "div"))

           replacement
           (element "div")]

       (is (= :gesso.live.browser.dom/not-canonical
              (:error/type
               (thrown-data
                #(dom/replace-canonical!
                  current
                  replacement)))))

       (is (identical?
            current
            (.-firstElementChild root)))))))

(deftest replace-canonical-sanitizes-installed-tree-test
  (with-sandbox*
   (fn [root]
     (let [current
           (append!
            root
            (element "div"))

           replacement
           (canonical-element
            "div"
            "scope"
            9
            {:hx-swap-oob "outerHTML"})

           child
           (append!
            replacement
            (element
             "span"
             {:data-hx-swap-oob "true"}))

           installed
           (dom/replace-canonical!
            current
            replacement)]

       (is (identical?
            replacement
            installed))

       (is (dom/canonical?
            installed))

       (is (false?
            (dom/has-attr?
             installed
             :hx-swap-oob)))

       (is (false?
            (dom/has-attr?
             child
             :data-hx-swap-oob)))

       (is (identical?
            installed
            (.-firstElementChild root)))))))

;; -----------------------------------------------------------------------------
;; Snapshot restoration
;; -----------------------------------------------------------------------------

(deftest restore-snapshot-installs-fresh-clone-test
  (with-sandbox*
   (fn [root]
     (let [original
           (append!
            root
            (text!
             (element
              "details"
              {:id "request-1"})
             "Original"))

           snapshot
           (dom/snapshot
            original)

           replacement
           (text!
            (element
             "details"
             {:id "request-1"})
            "Optimistic")

           _installed
           (dom/replace!
            original
            replacement)

           restored
           (dom/restore-snapshot!
            replacement
            snapshot)]

       (is (= "Original"
              (.-textContent restored)))

       (is (= "request-1"
              (.-id restored)))

       (is (dom/connected?
            restored))

       (is (not
            (identical?
             restored
             (:node snapshot))))

       (is (identical?
            restored
            (.-firstElementChild root)))))))

(deftest restore-snapshot-preserves-snapshot-for-later-recovery-attempt-test
  (with-sandbox*
   (fn [root]
     (let [original
           (append!
            root
            (text!
             (element "div")
             "Original"))

           snapshot
           (dom/snapshot
            original)

           optimistic
           (text!
            (element "div")
            "Optimistic")

           _first-install
           (dom/replace!
            original
            optimistic)

           first-restore
           (dom/restore-snapshot!
            optimistic
            snapshot)

           second-current
           (text!
            (element "div")
            "Second optimistic")

           _second-install
           (dom/replace!
            first-restore
            second-current)

           second-restore
           (dom/restore-snapshot!
            second-current
            snapshot)]

       (is (= "Original"
              (.-textContent first-restore)))

       (is (= "Original"
              (.-textContent second-restore)))

       (is (not
            (identical?
             first-restore
             second-restore)))))))

;; -----------------------------------------------------------------------------
;; Canonical ancestry
;; -----------------------------------------------------------------------------

(deftest canonical-ancestor-test
  (let [outer
        (canonical-element
         "article"
         "scope-a"
         1)

        middle
        (append!
         outer
         (canonical-element
          "section"
          "scope-b"
          2))

        inner
        (append!
         middle
         (element "button"))]

    (testing "nearest canonical ancestor wins without scope restriction"
      (is (identical?
           middle
           (dom/canonical-ancestor
            inner))))

    (testing "scope restriction may deliberately skip a nearer canonical node"
      (is (identical?
           outer
           (dom/canonical-ancestor
            inner
            "scope-a")))

      (is (identical?
           middle
           (dom/canonical-ancestor
            inner
            "scope-b"))))

    (is (nil?
         (dom/canonical-ancestor
          inner
          "missing-scope")))))

(deftest canonical-ancestor-includes-self-test
  (let [node
        (canonical-element
         "details"
         "scope"
         1)]

    (is (identical?
         node
         (dom/canonical-ancestor
          node)))

    (is (identical?
         node
         (dom/canonical-ancestor
          node
          "scope")))))

;; -----------------------------------------------------------------------------
;; Canonical selection within roots
;; -----------------------------------------------------------------------------

(deftest canonical-in-root-selects-newest-for-scope-test
  (let [root
        (element "div")

        older
        (append!
         root
         (canonical-element
          "section"
          "scope-a"
          1))

        newer
        (append!
         root
         (canonical-element
          "section"
          "scope-a"
          2))

        _other
        (append!
         root
         (canonical-element
          "section"
          "scope-b"
          100))

        result
        (dom/canonical-in-root
         root
         "scope-a")]

    (is (= :selected
           (:status result)))

    (is (identical?
         newer
         (:element result)))

    (is (not
         (identical?
          older
          (:element result))))))

(deftest canonical-in-root-reports-none-test
  (let [root
        (element "div")]

    (append!
     root
     (canonical-element
      "section"
      "other"
      1))

    (is (= {:status :none}
           (dom/canonical-in-root
            root
            "scope")))))

(deftest canonical-in-root-preserves-ambiguity-test
  (let [root
        (element "div")

        left
        (append!
         root
         (canonical-element
          "section"
          "scope"
          "left"))

        right
        (append!
         root
         (canonical-element
          "section"
          "scope"
          "right"))

        result
        (dom/canonical-in-root
         root
         "scope")]

    (is (= :ambiguous
           (:status result)))

    (is (= :incomparable-revisions
           (:reason result)))

    (is (= #{left right}
           (set
            (:elements result))))))

;; -----------------------------------------------------------------------------
;; Regression contract for the currently observed browser exception
;; -----------------------------------------------------------------------------

(deftest optimistic-projection-root-must-match-semantic-target-test
  (let [request-card
        (element
         "details"
         {:data-humanhelp-request-card
          "request-1"})

        projected-card
        (element
         "details"
         {:data-humanhelp-request-card
          "request-1"})

        htmx-source
        (element
         "button"
         {:data-action
          "claim"})]

    (testing "the semantic request-card projection is mechanically compatible"
      (is (identical?
           projected-card
           (dom/assert-compatible-root!
            request-card
            projected-card))))

    (testing "an incidental HTMX button is correctly rejected as the replacement root"
      (let [data
            (thrown-data
             #(dom/assert-compatible-root!
               htmx-source
               projected-card))]

        (is (= :gesso.live.browser.dom/root-tag-mismatch
               (:error/type data)))

        (is (= "BUTTON"
               (:current-tag data)))

        (is (= "DETAILS"
               (:replacement-tag data)))))))

;; -----------------------------------------------------------------------------
;; Authority/mechanics separation
;; -----------------------------------------------------------------------------

(deftest raw-replace-does-not-require-canonical-authority-test
  (with-sandbox*
   (fn [root]
     (let [current
           (append!
            root
            (element "div"))

           projected
           (element
            "div"
            {(dom/attr-name protocol/scope-attr)
             "scope"})]

       (is (identical?
            projected
            (dom/replace!
             current
             projected)))

       (is (false?
            (dom/canonical?
             projected)))))))

(deftest copy-element-does-not-invent-canonical-authority-test
  (let [target
        (canonical-element
         "div"
         "scope"
         1)

        projection
        (element
         "div"
         {(dom/attr-name protocol/scope-attr)
          "scope"})]

    (dom/copy-element-into!
     target
     projection)

    (testing "ordinary projection copies the source exactly; it does not retain old authority"
      (is (false?
           (dom/canonical?
            target)))

      (is (= "scope"
             (dom/scope
              target)))

      (is (nil?
           (dom/revision-wire
            target))))))

(deftest canonical-copy-does-not-decide-revision-authority-test
  (let [target
        (canonical-element
         "div"
         "scope"
         10)

        older
        (canonical-element
         "div"
         "scope"
         1)]

    (testing "DOM mechanics copy what the caller authorizes, even if older"
      (dom/copy-canonical-into!
       target
       older)

      (is (= 1
             (dom/revision target))))

    (testing "revision authority belongs to the caller/protocol layer"
      (is (= :same
             (dom/incoming-revision-status
              target
              older))))))

;; -----------------------------------------------------------------------------
;; Detached-vs-connected semantics
;; -----------------------------------------------------------------------------

(deftest parsed-and-template-roots-are-detached-test
  (let [parsed
        (dom/parse-one-root
         "<div>Parsed</div>")

        template
        (element "template")]

    (set!
     (.-innerHTML template)
     "<div>Template</div>")

    (let [projected
          (dom/template-root
           template)]

      (is (false?
           (dom/connected?
            parsed)))

      (is (false?
           (dom/connected?
            projected))))))

(deftest in-place-copy-preserves-connected-target-test
  (with-sandbox*
   (fn [root]
     (let [target
           (append!
            root
            (element "div"))

           source
           (text!
            (element "div")
            "New")]

       (dom/copy-element-into!
        target
        source)

       (is (dom/connected?
            target))

       (is (false?
            (dom/connected?
             source)))

       (is (= "New"
              (.-textContent target)))))))

;; -----------------------------------------------------------------------------
;; Mutation ordering: validate before changing DOM
;; -----------------------------------------------------------------------------

(deftest incompatible-copy-does-not-partially-restore-attributes-test
  (let [target
        (element
         "button"
         {:id "button-1"
          :data-state "original"})

        source
        (element
         "details"
         {:id "button-1"
          :data-state "replacement"})]

    (is (= :gesso.live.browser.dom/root-tag-mismatch
           (:error/type
            (thrown-data
             #(dom/copy-element-into!
               target
               source)))))

    (is (= "button-1"
           (.-id target)))

    (is (= "original"
           (dom/attr
            target
            :data-state)))))

(deftest incompatible-canonical-copy-does-not-partially-mutate-target-test
  (let [target
        (text!
         (element
          "button"
          {:id "button-1"
           :data-state "original"})
         "Original")

        source
        (canonical-element
         "details"
         "scope"
         2
         {:id "button-1"
          :data-state "replacement"
          :hx-swap-oob "outerHTML"})]

    (is (= :gesso.live.browser.dom/root-tag-mismatch
           (:error/type
            (thrown-data
             #(dom/copy-canonical-into!
               target
               source)))))

    (is (= "original"
           (dom/attr
            target
            :data-state)))

    (is (= "Original"
           (.-textContent target)))

    (testing "transport sanitization occurs before root compatibility validation"
      (is (false?
           (dom/has-attr?
            source
            :hx-swap-oob))))))

;; -----------------------------------------------------------------------------
;; Small composition smoke test
;; -----------------------------------------------------------------------------

(deftest projection-snapshot-canonical-composition-test
  (with-sandbox*
   (fn [root]
     (let [target
           (append!
            root
            (canonical-element
             "details"
             "request-1"
             7
             {:id "request-1"}))

           _initial-child
           (append!
            target
            (text!
             (element "summary")
             "Unclaimed"))

           snapshot
           (dom/snapshot
            target)

           projection
           (element
            "details"
            {:id "request-1"
             (dom/attr-name protocol/scope-attr)
             "request-1"})

           _projection-child
           (append!
            projection
            (text!
             (element "summary")
             "Claiming…"))

           _projection-copy
           (dom/copy-element-into!
            target
            projection)

           canonical
           (canonical-element
            "details"
            "request-1"
            8
            {:id "request-1"
             :hx-swap-oob "outerHTML"})

           _canonical-child
           (append!
            canonical
            (text!
             (element "summary")
             "Claimed"))

           _canonical-copy
           (dom/copy-canonical-into!
            target
            canonical)]

       (testing "the snapshot remains the pre-projection authoritative structure"
         (is (= "Unclaimed"
                (.-textContent
                 (.-firstElementChild
                  (:node snapshot)))))

         (is (dom/canonical?
              (:node snapshot)))

         (is (= 7
                (dom/revision
                 (:node snapshot)))))

       (is (= "Claimed"
              (.-textContent
               (.-firstElementChild target))))

       (is (dom/canonical?
            target))

       (is (= 8
              (dom/revision target)))

       (is (false?
            (dom/has-attr?
             target
             :hx-swap-oob)))))))

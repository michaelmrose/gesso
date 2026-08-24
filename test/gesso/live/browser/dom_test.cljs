(ns gesso.live.browser.dom-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.dom :as dom]))

;; =============================================================================
;; Physical DOM test helpers
;; =============================================================================

(defn- element
  ([tag]
   (element tag nil))
  ([tag attrs]
   (let [node (.createElement js/document tag)]
     (doseq [[attribute value] attrs]
       (.setAttribute node
                      (dom/attr-name attribute)
                      (str value)))
     node)))

(defn- text!
  [node value]
  (set! (.-textContent node) value)
  node)

(defn- append!
  [parent child]
  (.appendChild parent child)
  child)

(defn- sandbox
  []
  (let [root (element "div"
                      {:data-gesso-dom-test
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

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some-> (thrown f) ex-data))

(defn- error-kind
  [f]
  (:error/kind (thrown-data f)))

(defn- child-tags
  [node]
  (mapv #(.-tagName %)
        (array-seq (.-children node))))

(defn- child-text
  [node]
  (mapv #(.-textContent %)
        (array-seq (.-children node))))

;; =============================================================================
;; Public identity and attribute names
;; =============================================================================

(deftest public-identity-test
  (is (= 2 dom/dom-version))
  (is (= :gesso.live.browser.dom/structural-snapshot
         dom/structural-snapshot-type)))

(deftest attr-name-is-mechanical-test
  (is (= "data-test" (dom/attr-name :data-test)))
  (is (= "data-test" (dom/attr-name 'data-test)))
  (is (= "data-test" (dom/attr-name "data-test")))
  (is (= "42" (dom/attr-name 42))))

;; =============================================================================
;; Node predicates and lookup
;; =============================================================================

(deftest node-predicates-test
  (let [div (element "div")
        template (element "template")
        text (.createTextNode js/document "hello")]
    (is (true? (dom/element? div)))
    (is (true? (dom/template? template)))
    (is (false? (dom/template? div)))
    (is (false? (dom/element? text)))
    (is (false? (dom/element? nil)))))

(deftest connected-predicate-test
  (with-sandbox*
   (fn [root]
     (let [child (append! root (element "span"))
           detached (element "span")]
       (is (true? (dom/connected? root)))
       (is (true? (dom/connected? child)))
       (is (false? (dom/connected? detached)))
       (is (false? (dom/connected? nil)))))))

(deftest by-id-supports-global-and-explicit-document-test
  (with-sandbox*
   (fn [root]
     (let [node (append! root (element "div" {:id "dom-by-id"}))]
       (is (identical? node (dom/by-id "dom-by-id")))
       (is (identical? node (dom/by-id js/document "dom-by-id")))
       (is (nil? (dom/by-id js/document "missing")))
       (is (nil? (dom/by-id js/document "")))
       (is (nil? (dom/by-id nil "dom-by-id")))))))

(deftest query-one-is-safe-optional-lookup-test
  (with-sandbox*
   (fn [root]
     (let [node (append! root (element "button" {:data-action "claim"}))]
       (is (identical? node
                       (dom/query-one root "button[data-action='claim']")))
       (is (nil? (dom/query-one root "[data-missing]")))
       (is (nil? (dom/query-one root "[")))
       (is (nil? (dom/query-one root "")))
       (is (nil? (dom/query-one nil "button")))))))

(deftest query-all-is-safe-optional-lookup-test
  (with-sandbox*
   (fn [root]
     (append! root (element "span" {:data-kind "x"}))
     (append! root (element "span" {:data-kind "x"}))
     (is (= 2 (count (dom/query-all root "[data-kind='x']"))))
     (is (= [] (dom/query-all root "[data-missing]")))
     (is (= [] (dom/query-all root "[")))
     (is (= [] (dom/query-all nil "span"))))))

(deftest require-element-has-uniform-error-shape-test
  (let [node (element "div")]
    (is (identical? node (dom/require-element! node)))
    (let [error (thrown #(dom/require-element!
                          "Missing physical node."
                          nil
                          {:execution-id "execution-1"}))
          data (ex-data error)]
      (is (= "Missing physical node." (ex-message error)))
      (is (= :gesso.live.browser.dom/error (:error/type data)))
      (is (= :missing-element (:error/kind data)))
      (is (= "execution-1" (:execution-id data)))
      (is (nil? (:element data))))))

(deftest require-connected-rejects-detached-elements-test
  (with-sandbox*
   (fn [root]
     (let [connected (append! root (element "div"))
           detached (element "div")]
       (is (identical? connected (dom/require-connected! connected)))
       (let [data (thrown-data #(dom/require-connected!
                                 detached
                                 {:fragment-id "fragment-1"}))]
         (is (= :detached-target (:error/kind data)))
         (is (= "fragment-1" (:fragment-id data)))
         (is (identical? detached (:element data))))))))

(deftest require-query-one-turns-optional-miss-into-physical-error-test
  (with-sandbox*
   (fn [root]
     (let [node (append! root (element "button" {:data-action "claim"}))]
       (is (identical? node
                       (dom/require-query-one! root
                                               "button[data-action='claim']")))
       (doseq [selector ["[data-missing]" "["]]
         (let [data (thrown-data #(dom/require-query-one! root selector))]
           (is (= :missing-target (:error/kind data)))
           (is (= selector (:selector data)))
           (is (identical? root (:root data)))))))))

(deftest selector-helpers-test
  (let [outer (element "article" {:data-card "request-1"})
        middle (append! outer (element "div"))
        button (append! middle (element "button" {:data-action "claim"}))]
    (is (true? (dom/matches? button "button[data-action='claim']")))
    (is (false? (dom/matches? button "div")))
    (is (false? (dom/matches? button "[")))
    (is (identical? outer (dom/closest button "[data-card]")))
    (is (identical? button (dom/closest button "button")))
    (is (nil? (dom/closest button "[")))
    (is (identical? button (dom/closest-with-attr button :data-action)))
    (is (identical? outer (dom/closest-with-attr button :data-card)))
    (is (nil? (dom/closest-with-attr button :data-missing)))))

(deftest contains-node-includes-root-itself-test
  (let [root (element "div")
        child (append! root (element "span"))
        other (element "span")]
    (is (true? (dom/contains-node? root root)))
    (is (true? (dom/contains-node? root child)))
    (is (false? (dom/contains-node? root other)))
    (is (false? (dom/contains-node? nil child)))))

;; =============================================================================
;; Attribute mechanics
;; =============================================================================

(deftest attribute-read-write-test
  (let [node (element "div")]
    (is (nil? (dom/attr node :data-state)))
    (is (false? (dom/has-attr? node :data-state)))
    (is (identical? node (dom/set-attr! node :data-state "pending")))
    (is (= "pending" (dom/attr node :data-state)))
    (is (true? (dom/has-attr? node :data-state)))
    (is (identical? node (dom/remove-attr! node :data-state)))
    (is (nil? (dom/attr node :data-state)))))

(deftest nil-attribute-value-means-remove-test
  (let [node (element "div" {:data-state "pending"})]
    (dom/set-attr! node :data-state nil)
    (is (false? (dom/has-attr? node :data-state)))))

(deftest tolerant-attribute-helpers-ignore-non-elements-test
  (is (nil? (dom/attr nil :data-x)))
  (is (false? (dom/has-attr? nil :data-x)))
  (is (nil? (dom/set-attr! nil :data-x "x")))
  (is (nil? (dom/remove-attr! nil :data-x))))

(deftest truthy-marker-is-generic-html-mechanics-test
  (doseq [value ["" "true" "TRUE" "1" "yes" "anything"]]
    (is (true? (dom/truthy-attr?
                (element "div" {:data-enabled value})
                :data-enabled))
        (str "expected truthy marker for " (pr-str value))))
  (doseq [value ["false" "FALSE" "0" "no" "NO"]]
    (is (false? (dom/truthy-attr?
                 (element "div" {:data-enabled value})
                 :data-enabled))
        (str "expected false-like marker for " (pr-str value))))
  (is (nil? (dom/truthy-attr? (element "div") :data-enabled))))

(deftest attributes-and-restoration-are-complete-set-operations-test
  (let [source (element "div"
                        {:id "new"
                         :data-new "yes"
                         :aria-label "Example"})
        target (element "div"
                        {:id "old"
                         :data-old "yes"})
        pairs (dom/attributes source)]
    (is (= #{["id" "new"]
             ["data-new" "yes"]
             ["aria-label" "Example"]}
           (set pairs)))
    (is (identical? target (dom/restore-attributes! target pairs)))
    (is (= "new" (.-id target)))
    (is (= "yes" (dom/attr target :data-new)))
    (is (= "Example" (dom/attr target :aria-label)))
    (is (false? (dom/has-attr? target :data-old)))))

(deftest strict-attribute-collection-requires-elements-test
  (is (= :missing-element (error-kind #(dom/attributes nil))))
  (is (= :missing-element (error-kind #(dom/restore-attributes! nil []))))
  (is (= :missing-element (error-kind #(dom/remove-attrs! nil [:x])))))

(deftest remove-attrs-is-caller-directed-test
  (let [node (element "div" {:a "1" :b "2" :c "3"})]
    (is (identical? node (dom/remove-attrs! node [:a :c])))
    (is (nil? (dom/attr node :a)))
    (is (= "2" (dom/attr node :b)))
    (is (nil? (dom/attr node :c)))))

(deftest remove-attrs-deep-mutates-only-requested-attributes-test
  (let [root (element "div" {:data-drop "root" :data-keep "root"})
        child (append! root (element "section" {:data-drop "child"
                                                 :data-keep "child"}))
        grandchild (append! child (element "span" {:data-drop "grandchild"}))]
    (is (identical? root (dom/remove-attrs-deep! root [:data-drop])))
    (doseq [node [root child grandchild]]
      (is (false? (dom/has-attr? node :data-drop))))
    (is (= "root" (dom/attr root :data-keep)))
    (is (= "child" (dom/attr child :data-keep)))))

(deftest policy-looking-attributes-are-uninterpreted-data-test
  (let [source (element "section"
                        {:id "new-id"
                         :data-gesso-optimistic-canonical "false"
                         :data-gesso-optimistic-scope "scope-x"
                         :data-gesso-optimistic-revision "opaque"
                         :hx-swap-oob "outerHTML"})
        target (element "section" {:id "old-id"})]
    (dom/copy-element-into! target source)
    (is (= "new-id" (.-id target)))
    (is (= "false" (dom/attr target :data-gesso-optimistic-canonical)))
    (is (= "scope-x" (dom/attr target :data-gesso-optimistic-scope)))
    (is (= "opaque" (dom/attr target :data-gesso-optimistic-revision)))
    (is (= "outerHTML" (dom/attr target :hx-swap-oob)))
    (testing "generic DOM copying neither promotes nor sanitizes policy metadata"
      (is (= (set (dom/attributes source))
             (set (dom/attributes target)))))))

;; =============================================================================
;; Root shape and parsing
;; =============================================================================

(deftest tag-name-and-root-tag-test
  (is (= "DETAILS" (dom/tag-name (element "details"))))
  (is (nil? (dom/tag-name nil)))
  (is (true? (dom/same-root-tag? (element "details")
                                 (element "details"))))
  (is (false? (dom/same-root-tag? (element "details")
                                  (element "section"))))
  (is (false? (dom/same-root-tag? nil (element "section")))))

(deftest same-root-tag-is-required-only-for-in-place-copy-test
  (let [target (element "button")
        source (element "details")
        data (thrown-data #(dom/require-same-root-tag! target source))]
    (is (= :incompatible-copy-root (:error/kind data)))
    (is (= "BUTTON" (:target-tag data)))
    (is (= "DETAILS" (:source-tag data))))
  (is (= :missing-element
         (error-kind #(dom/require-same-root-tag! nil (element "div"))))))

(deftest one-element-root-accepts-only-whitespace-around-root-test
  (let [container (element "div")]
    (.appendChild container (.createTextNode js/document " \n\t "))
    (let [child (append! container (element "details"))]
      (.appendChild container (.createTextNode js/document "\n"))
      (is (identical? child (dom/one-element-root container))))))

(deftest one-element-root-rejects-non-single-root-shapes-test
  (is (= :missing-parent (error-kind #(dom/one-element-root nil))))
  (let [empty (element "div")]
    (is (= :not-one-root (error-kind #(dom/one-element-root empty)))))
  (let [multi (element "div")]
    (append! multi (element "div"))
    (append! multi (element "span"))
    (let [data (thrown-data #(dom/one-element-root multi))]
      (is (= :not-one-root (:error/kind data)))
      (is (= 2 (:child-count data)))))
  (let [text-plus-element (element "div")]
    (.appendChild text-plus-element (.createTextNode js/document "text"))
    (append! text-plus-element (element "div"))
    (is (= :not-one-root
           (error-kind #(dom/one-element-root text-plus-element)))))
  (let [comment-plus-element (element "div")]
    (.appendChild comment-plus-element (.createComment js/document "comment"))
    (append! comment-plus-element (element "div"))
    (is (= :not-one-root
           (error-kind #(dom/one-element-root comment-plus-element))))))

(deftest template-root-returns-fresh-detached-clones-test
  (let [template (element "template")]
    (set! (.-innerHTML template)
          "<details id='request-1'><summary>Pending</summary></details>")
    (let [first-root (dom/template-root template)
          second-root (dom/template-root template)]
      (is (= "DETAILS" (dom/tag-name first-root)))
      (is (= "request-1" (.-id first-root)))
      (is (= "Pending" (.-textContent first-root)))
      (is (false? (dom/connected? first-root)))
      (is (not (identical? first-root second-root)))
      (.setAttribute first-root "data-mutated" "true")
      (is (nil? (.getAttribute second-root "data-mutated"))))))

(deftest template-root-validates-physical-shape-test
  (is (= :not-template
         (error-kind #(dom/template-root (element "div")))))
  (let [template (element "template")]
    (set! (.-innerHTML template) "<div>A</div><div>B</div>")
    (is (= :not-one-root
           (error-kind #(dom/template-root template))))))

(deftest parse-one-root-returns-detached-browser-node-test
  (let [node (dom/parse-one-root
              "  <details id='request-1' open><summary>Claimed</summary></details>  ")]
    (is (= "DETAILS" (dom/tag-name node)))
    (is (= "request-1" (.-id node)))
    (is (true? (.-open node)))
    (is (false? (dom/connected? node)))))

(deftest parse-one-root-validates-input-and-shape-test
  (let [data (thrown-data #(dom/parse-one-root [:div]))]
    (is (= :invalid-html (:error/kind data)))
    (is (= [:div] (:value data))))
  (is (= :missing-document
         (error-kind #(dom/parse-one-root nil "<div></div>"))))
  (is (= :not-one-root
         (error-kind #(dom/parse-one-root "<div>A</div><span>B</span>"))))
  (is (= :not-one-root
         (error-kind #(dom/parse-one-root "text<div>A</div>")))))

;; =============================================================================
;; Structural snapshots are physical resources only
;; =============================================================================

(deftest structural-snapshot-is-policy-free-detached-clone-test
  (let [source (element "details"
                        {:id "request-1"
                         :data-gesso-optimistic-canonical "true"
                         :data-gesso-optimistic-scope "scope-a"
                         :data-gesso-optimistic-revision "7"})
        child (append! source (text! (element "span") "Original"))
        snapshot (dom/snapshot source)]
    (is (true? (dom/snapshot? snapshot)))
    (is (= #{:gesso.live.browser.dom/type :node}
           (set (keys snapshot))))
    (is (= dom/structural-snapshot-type
           (:gesso.live.browser.dom/type snapshot)))
    (is (dom/element? (:node snapshot)))
    (is (not (identical? source (:node snapshot))))
    (is (= "Original" (.-textContent (:node snapshot))))
    (is (= "true"
           (dom/attr (:node snapshot) :data-gesso-optimistic-canonical)))
    (text! child "Mutated")
    (is (= "Original" (.-textContent (:node snapshot))))))

(deftest snapshot-node-is-fresh-for-every-restoration-attempt-test
  (let [snapshot (dom/snapshot (text! (element "article") "Original"))
        first-node (dom/snapshot-node snapshot)
        second-node (dom/snapshot-node snapshot)]
    (is (not (identical? first-node second-node)))
    (text! first-node "Mutated")
    (is (= "Original" (.-textContent second-node)))
    (is (= "Original" (.-textContent (:node snapshot))))))

(deftest snapshot-validation-test
  (is (= :missing-element (error-kind #(dom/snapshot nil))))
  (doseq [value [nil
                 {}
                 {:gesso.live.browser.dom/type dom/structural-snapshot-type}
                 {:gesso.live.browser.dom/type :wrong :node (element "div")}
                 {:gesso.live.browser.dom/type dom/structural-snapshot-type
                  :node "not-an-element"}]]
    (is (false? (dom/snapshot? value)))
    (is (= :invalid-snapshot (error-kind #(dom/require-snapshot! value))))
    (is (= :invalid-snapshot (error-kind #(dom/snapshot-node value))))))

;; =============================================================================
;; Physical copying
;; =============================================================================

(deftest replace-children-deep-clones-source-test
  (let [target (element "div")
        old-child (append! target (text! (element "span") "Old"))
        source (element "div")
        source-child (append! source (text! (element "strong") "New"))]
    (is (identical? target (dom/replace-children-from! target source)))
    (is (= ["STRONG"] (child-tags target)))
    (is (= ["New"] (child-text target)))
    (is (not (identical? source-child (.-firstElementChild target))))
    (is (nil? (.-parentNode old-child)))
    (text! source-child "Changed source")
    (is (= "New" (.-textContent (.-firstElementChild target))))))

(deftest replace-children-validates-both-roots-test
  (is (= :missing-element
         (error-kind #(dom/replace-children-from! nil (element "div")))))
  (is (= :missing-element
         (error-kind #(dom/replace-children-from! (element "div") nil)))))

(deftest copy-element-into-preserves-target-object-but-copies-source-identity-test
  (let [target (element "section" {:id "old-id" :data-old "yes"})
        source (element "section" {:id "new-id" :data-new "yes"})
        source-child (append! source (text! (element "strong") "New body"))]
    (is (identical? target (dom/copy-element-into! target source)))
    (is (= "new-id" (.-id target)))
    (is (= "yes" (dom/attr target :data-new)))
    (is (false? (dom/has-attr? target :data-old)))
    (is (= "New body" (.-textContent target)))
    (is (not (identical? source-child (.-firstElementChild target))))))

(deftest copy-element-into-copies-details-open-property-test
  (let [target (element "details")
        source (element "details")]
    (set! (.-open target) false)
    (set! (.-open source) true)
    (dom/copy-element-into! target source)
    (is (true? (.-open target)))
    (set! (.-open source) false)
    (dom/copy-element-into! target source)
    (is (false? (.-open target)))))

(deftest copy-element-into-rejects-root-tag-change-test
  (let [target (element "button")
        source (element "details")
        data (thrown-data #(dom/copy-element-into! target source))]
    (is (= :incompatible-copy-root (:error/kind data)))
    (is (= "BUTTON" (:target-tag data)))
    (is (= "DETAILS" (:source-tag data)))))

;; =============================================================================
;; Whole-node replacement
;; =============================================================================

(deftest whole-node-replacement-may-change-root-tag-and-id-test
  (with-sandbox*
   (fn [root]
     (let [current (append! root (text! (element "button" {:id "old"}) "Old"))
           replacement (text! (element "details" {:id "new"}) "New")
           installed (dom/replace! current replacement)]
       (is (identical? replacement installed))
       (is (identical? replacement (.-firstElementChild root)))
       (is (= "DETAILS" (dom/tag-name installed)))
       (is (= "new" (.-id installed)))
       (is (= "New" (.-textContent installed)))
       (is (false? (dom/connected? current)))
       (is (true? (dom/connected? installed)))))))

(deftest replacement-rejects-invalid-or-detached-current-test
  (is (= :missing-element
         (error-kind #(dom/replace! nil (element "div")))))
  (is (= :missing-element
         (error-kind #(dom/replace! (element "div") nil))))
  (let [current (element "div")
        replacement (element "section")
        data (thrown-data #(dom/replace! current replacement))]
    (is (= :detached-target (:error/kind data)))
    (is (identical? current (:current data)))))

(deftest restore-snapshot-is-mechanical-cross-tag-replacement-test
  (with-sandbox*
   (fn [root]
     (let [snapshot-source (text! (element "article" {:id "restored"})
                                  "Original")
           snapshot (dom/snapshot snapshot-source)
           current (append! root (text! (element "details" {:id "current"})
                                        "Current"))
           restored (dom/restore-snapshot! current snapshot)]
       (is (= "ARTICLE" (dom/tag-name restored)))
       (is (= "restored" (.-id restored)))
       (is (= "Original" (.-textContent restored)))
       (is (identical? restored (.-firstElementChild root)))
       (is (false? (dom/connected? current)))
       (is (not (identical? restored (:node snapshot))))))))

(deftest repeated-snapshot-restoration-does-not-consume-snapshot-test
  (with-sandbox*
   (fn [root]
     (let [snapshot (dom/snapshot (text! (element "article") "Original"))
           first-current (append! root (text! (element "div") "First"))
           first-restored (dom/restore-snapshot! first-current snapshot)
           second-current (text! (element "section") "Second")]
       (dom/replace! first-restored second-current)
       (let [second-restored (dom/restore-snapshot! second-current snapshot)]
         (is (= "Original" (.-textContent first-restored)))
         (is (= "Original" (.-textContent second-restored)))
         (is (not (identical? first-restored second-restored)))
         (is (= "Original" (.-textContent (:node snapshot)))))))))

(ns gesso.live.oob-test
  (:require
   [clojure.test :refer [deftest is]]
   [gesso.live.oob :as oob]))

;; -----------------------------------------------------------------------------
;; Selectors
;; -----------------------------------------------------------------------------

(deftest id-selector-builds-simple-id-selector-test
  (is (= "#request-1"
         (oob/id-selector "request-1")))
  (is (= "#request_1"
         (oob/id-selector :request_1)))
  (is (= "#request-1"
         (oob/id-selector "#request-1"))))

(deftest id-selector-rejects-blank-or-unsafe-values-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must not be blank"
       (oob/id-selector "")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"CSS-id-safe"
       (oob/id-selector "request 1")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"CSS-id-safe"
       (oob/id-selector "1-request")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"CSS-id-safe"
       (oob/id-selector "request#1"))))

(deftest selector-allows-complex-selectors-test
  (is (= "#request-panel .item[data-x='1']"
         (oob/selector "#request-panel .item[data-x='1']")))
  (is (= "request-panel"
         (oob/selector :request-panel))))

(deftest selector-rejects-blank-values-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must not be blank"
       (oob/selector ""))))

;; -----------------------------------------------------------------------------
;; Swap values
;; -----------------------------------------------------------------------------

(deftest normalize-swap-handles-valid-swap-values-test
  (is (= "true" (oob/normalize-swap nil)))
  (is (= "true" (oob/normalize-swap true)))
  (is (= "true" (oob/normalize-swap :true)))
  (is (= "outerHTML" (oob/normalize-swap :outerHTML)))
  (is (= "innerHTML" (oob/normalize-swap 'innerHTML)))
  (is (= "beforebegin" (oob/normalize-swap "beforebegin")))
  (is (= "afterbegin" (oob/normalize-swap :afterbegin)))
  (is (= "beforeend" (oob/normalize-swap :beforeend)))
  (is (= "afterend" (oob/normalize-swap :afterend)))
  (is (= "delete" (oob/normalize-swap :delete)))
  (is (= "none" (oob/normalize-swap :none))))

(deftest normalize-swap-rejects-invalid-values-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unsupported gesso.live OOB swap style"
       (oob/normalize-swap :banana)))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unsupported gesso.live OOB swap style"
       (oob/normalize-swap "banana")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must not be blank"
       (oob/normalize-swap "")))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unsupported gesso.live OOB swap style"
       (oob/normalize-swap 42))))

(deftest swap-value-builds-htmx-oob-values-test
  (is (= "true"
         (oob/swap-value)))
  (is (= "outerHTML"
         (oob/swap-value :outerHTML)))
  (is (= "outerHTML:#request-panel"
         (oob/swap-value :outerHTML "#request-panel")))
  (is (= "beforeend:#messages"
         (oob/swap-value :beforeend "#messages")))
  (is (= "outerHTML:#request-panel"
         (oob/swap-value true "#request-panel")))
  (is (= "outerHTML:#request-panel"
         (oob/swap-value :true "#request-panel"))))

(deftest oob-attrs-builds-attribute-map-test
  (is (= {:hx-swap-oob "true"}
         (oob/oob-attrs)))
  (is (= {:hx-swap-oob "innerHTML:#panel"}
         (oob/oob-attrs {:swap :innerHTML
                         :target "#panel"}))))

;; -----------------------------------------------------------------------------
;; Hiccup node marking
;; -----------------------------------------------------------------------------

(deftest with-oob-adds-oob-attribute-to-node-with-attrs-test
  (is (= [:div {:id "panel"
                :class "card"
                :hx-swap-oob "outerHTML"}
          "Hello"]
         (oob/with-oob
           [:div {:id "panel"
                  :class "card"}
            "Hello"]
           {:swap :outerHTML}))))

(deftest with-oob-adds-oob-attribute-to-node-without-attrs-test
  (is (= [:div {:hx-swap-oob "true"} "Hello"]
         (oob/with-oob [:div "Hello"]))))

(deftest with-oob-supports-targeted-swap-test
  (is (= [:div {:class "item"
                :hx-swap-oob "beforeend:#items"}
          "Item"]
         (oob/with-oob
           [:div {:class "item"} "Item"]
           {:swap :beforeend
            :target "#items"}))))

(deftest with-oob-rejects-malformed-hiccup-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"expected a Hiccup node vector"
       (oob/with-oob {:not :hiccup})))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"expected a Hiccup node vector"
       (oob/with-oob []))))

(deftest without-oob-removes-oob-attribute-test
  (is (= [:div {:id "panel"} "Hello"]
         (oob/without-oob
           [:div {:id "panel"
                  :hx-swap-oob "outerHTML"}
            "Hello"]))))

(deftest oob-predicate-and-value-read-oob-attrs-test
  (let [node (oob/with-oob [:div {:id "panel"} "Hello"]
                           {:swap :innerHTML
                            :target "#panel"})]
    (is (oob/oob? node))
    (is (= "innerHTML:#panel"
           (oob/oob-value node))))
  (is (not (oob/oob? [:div {:id "panel"} "Hello"])))
  (is (nil? (oob/oob-value [:div {:id "panel"} "Hello"]))))

(deftest ensure-id-adds-explicit-id-test
  (is (= [:div {:class "card"
                :id "request-1"}
          "Hello"]
         (oob/ensure-id
           [:div {:class "card"} "Hello"]
           "request-1"))))

(deftest ensure-id-rejects-unsafe-id-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"CSS-id-safe"
       (oob/ensure-id [:div "Hello"] "bad id"))))

;; -----------------------------------------------------------------------------
;; Common OOB operations
;; -----------------------------------------------------------------------------

(deftest replace-oob-marks-node-for-outer-html-swap-test
  (is (= [:div {:id "panel"
                :hx-swap-oob "outerHTML"}
          "Updated"]
         (oob/replace-oob
           [:div {:id "panel"} "Updated"]))))

(deftest replace-oob-supports-target-test
  (is (= [:div {:class "replacement"
                :hx-swap-oob "outerHTML:#panel"}
          "Updated"]
         (oob/replace-oob
           "#panel"
           [:div {:class "replacement"} "Updated"]))))

(deftest inner-oob-marks-node-for-inner-html-swap-test
  (is (= [:div {:hx-swap-oob "innerHTML:#panel"} "Inner"]
         (oob/inner-oob "#panel" [:div "Inner"]))))

(deftest append-oob-marks-node-for-beforeend-swap-test
  (is (= [:li {:hx-swap-oob "beforeend:#items"} "Item"]
         (oob/append-oob "#items" [:li "Item"]))))

(deftest prepend-oob-marks-node-for-afterbegin-swap-test
  (is (= [:li {:hx-swap-oob "afterbegin:#items"} "Item"]
         (oob/prepend-oob "#items" [:li "Item"]))))

(deftest before-oob-marks-node-for-beforebegin-swap-test
  (is (= [:div {:hx-swap-oob "beforebegin:#panel"} "Before"]
         (oob/before-oob "#panel" [:div "Before"]))))

(deftest after-oob-marks-node-for-afterend-swap-test
  (is (= [:div {:hx-swap-oob "afterend:#panel"} "After"]
         (oob/after-oob "#panel" [:div "After"]))))

(deftest delete-oob-builds-delete-marker-test
  (is (= [:div {:hx-swap-oob "delete:#panel"}]
         (oob/delete-oob "#panel"))))

(deftest no-op-oob-builds-none-marker-test
  (is (= [:div {:hx-swap-oob "none"}]
         (oob/no-op-oob))))

;; -----------------------------------------------------------------------------
;; Fragment collections
;; -----------------------------------------------------------------------------

(deftest fragments-drops-nil-values-test
  (let [a (oob/replace-oob [:div {:id "a"}])
        b (oob/append-oob "#items" [:li "b"])]
    (is (= [a b]
           (oob/fragments a nil b nil)))))

(deftest ensure-all-oob-returns-original-collection-when-valid-test
  (let [nodes [(oob/replace-oob [:div {:id "a"}])
               (oob/append-oob "#items" [:li "b"])]]
    (is (= nodes
           (oob/ensure-all-oob! nodes)))))

(deftest ensure-all-oob-rejects-non-oob-node-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Expected every gesso.live OOB fragment"
       (oob/ensure-all-oob!
         [(oob/replace-oob [:div {:id "a"}])
          [:div {:id "b"}]]))))

(deftest only-oob-filters-nodes-test
  (let [a (oob/replace-oob [:div {:id "a"}])
        b [:div {:id "b"}]
        c (oob/append-oob "#items" [:li "c"])]
    (is (= [a c]
           (oob/only-oob [a b c])))))

;; -----------------------------------------------------------------------------
;; Opaque metadata attrs
;; -----------------------------------------------------------------------------

(deftest with-live-id-adds-live-id-data-attr-test
  (is (= [:div {:id "panel"
                :data-gesso-live-id "request-1"}
          "Hello"]
         (oob/with-live-id
           [:div {:id "panel"} "Hello"]
           :request-1))))

(deftest with-consistency-token-adds-token-data-attr-test
  (is (= [:div {:id "panel"
                :data-gesso-consistency-token "opaque-token"}
          "Hello"]
         (oob/with-consistency-token
           [:div {:id "panel"} "Hello"]
           "opaque-token"))))

(deftest with-consistency-token-rejects-blank-token-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"must not be blank"
       (oob/with-consistency-token [:div "Hello"] ""))))

(deftest with-trigger-adds-trigger-data-attr-test
  (is (= [:div {:id "panel"
                :data-gesso-trigger "live-update"}
          "Hello"]
         (oob/with-trigger
           [:div {:id "panel"} "Hello"]
           :live-update))))

(deftest metadata-attrs-compose-with-oob-test
  (is (= [:div {:id "panel"
                :hx-swap-oob "outerHTML"
                :data-gesso-live-id "request-1"
                :data-gesso-consistency-token "opaque-token"
                :data-gesso-trigger "live-update"}
          "Hello"]
         (-> [:div {:id "panel"} "Hello"]
             (oob/replace-oob)
             (oob/with-live-id :request-1)
             (oob/with-consistency-token "opaque-token")
             (oob/with-trigger :live-update)))))

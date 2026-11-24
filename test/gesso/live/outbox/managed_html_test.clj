(ns gesso.live.outbox.managed-html-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.http :as http]
            [gesso.live.outbox.managed-html :as managed-html]))

(defn- failure [node]
  (try (managed-html/html-response node) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest selected-content-rejects-executable-markup-before-serialization-test
  (doseq [[kind node] [[:forbidden-element [:script "alert(1)"]]
                       [:forbidden-element ["SCRIPT#demo" {:src "/asset.js"}]]
                       [:forbidden-element [(symbol "script")]]
                       [:forbidden-element [:iframe]]
                       [:inline-event-handler [:button {:on-click "alert(1)"}]]
                       [:inline-event-handler [:img {"OnError" "alert(1)"}]]
                       [:raw-html [:div {:dangerouslySetInnerHTML {:__html "<script>bad</script>"}}]]
                       [:raw-html [:div {:src-doc "<script>bad</script>"}]]
                       [:executable-url [:a {:href "JaVa\nScRiPt:alert(1)"}]]
                       [:executable-url [:use {:xlink-href "javascript:alert(1)"}]]
                       [:invalid-element-selector ["div#x\" onclick=\"bad"]]
                       [:invalid-attribute-name [:div {"x\" onclick" "bad"}]]
                       [:invalid-style-key [:div {:style {"color\" onclick" "bad"}}]]
                       [:unsupported-render-node (rum.server_render.JSComponent. "<script>bad</script>")]]]
    (testing (name kind)
      (let [result (failure [:section (list node)])]
        (is (= :gesso.live.outbox.managed-html/invalid-content (:error/type result)))
        (is (= kind (:error/kind result)))
        (is (= [1 0] (:render-path result)))))))

(deftest checked-response-preserves-data-and-existing-preflight-test
  (let [seen (atom nil)
        node (with-meta [:section#panel {:class ["a" "b"] :style {:min-height "12px"}}
                         [:a {:href "/requests"} "Requests"]
                         (map identity [[:span "<script>literal</script>"]
                                        [:div {:id "email-error"}
                                         [:span {:data-validation-error-message true} "Bad"]]])]
               {:example/marker :owned})
        result (http/with-html-response-preflight
                (fn [body] (reset! seen body) true)
                #(managed-html/html-response node))]
    (is (= 200 (:status result)))
    (is (= (http/html-response node) result))
    (is (= (meta node) (meta @seen)))
    (is (str/includes? (:body result) "&lt;script&gt;literal&lt;/script&gt;"))
    (is (not (str/includes? (:body result) "<script>")))
    ;; Full-document/unmanaged serialization remains deliberately unchanged.
    (is (str/includes? (:body (http/html-response [:script {:src "/asset.js"}])) "<script"))))

(deftest selected-content-rejects-undeclared-secondary-effects-test
  (doseq [[attribute node path]
          [["hx-swap-oob" [:div#foreign {:hx-swap-oob true}] []]
           ["hx-swap-oob" [:div {:hx-swap-oob "innerHTML:#foreign"}] []]
           ["data-hx-swap-oob" [:div {"data-hx-swap-oob" "delete:#foreign"}] []]
           ["hx-swap-oob" [:section [:div {"HX-SWAP-OOB" "beforeend:#foreign"}]] [1]]
           ["data-hx-swap-oob" [:template (list [:div {:data-hx-swap-oob "outerHTML"}])] [1 0]]
           ["hx-swap-oob" [:<> [:span "ordinary"] [:div {:hx-swap-oob "false"}]] [2]]]]
    (let [result (failure node)]
      (is (= :gesso.live.outbox.managed-html/invalid-content (:error/type result)))
      (is (= :undeclared-oob-effect (:error/kind result)))
      (is (= attribute (:attribute result)))
      (is (= path (:render-path result)))))
  (is (= 200 (:status (managed-html/html-response
                      [:section [:button {:hx-swap "innerHTML"} "Ordinary"]]))))
  ;; Separate unmanaged/validation response paths have not been qualified by
  ;; this restriction and retain their existing serialization behavior.
  (is (str/includes? (:body (http/html-response [:div {:hx-swap-oob "innerHTML"}]))
                    "hx-swap-oob")))

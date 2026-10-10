(ns gesso.live.application-preflight.rendered-request-test
  "Direct, effect-free regression tests for physical Hiccup request inspection.

   These tests intentionally avoid whole-application construction. This module
   enumerates explicit requests and reports ambiguous HTML attributes; it
   does not decide whether a request is an admitted Choreo operation."
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.application-preflight.rendered-request :as request]
   [gesso.live.ui :as ui]))

(def ^:private htmx-methods
  [[:hx-get :get]
   [:hx-post :post]
   [:hx-put :put]
   [:hx-patch :patch]
   [:hx-delete :delete]])

(defn- coordinates
  [hiccup]
  (request/request-coordinates hiccup))

(defn- only-coordinate
  [hiccup]
  (let [found (coordinates hiccup)]
    (is (= 1 (count found))
        (str "Expected exactly one explicit request: " (pr-str found)))
    (first found)))

(deftest all-htmx-methods-accept-keyword-and-string-attributes
  (doseq [[attribute method] htmx-methods
          attribute-key [attribute (name attribute)]]
    (testing (str "HTMX " (name method) " via " (pr-str attribute-key))
      (is (= {:method method
              :path "/resource/42?source=ui"
              :request-source attribute
              :render-path []
              :choreo-operation-declared? false}
             (only-coordinate
              [:button {attribute-key "/resource/42?source=ui"} "Go"]))))))

(deftest multiple-requests-on-one-node-remain-distinct
  (is (= [{:method :post
           :path "/claim"
           :request-source :hx-post
           :render-path []
           :choreo-operation-declared? false}
          {:method :delete
           :path "/cancel"
           :request-source :hx-delete
           :render-path []
           :choreo-operation-declared? false}]
         (coordinates
          [:button {:hx-post "/claim"
                    "hx-delete" "/cancel"}
           "Two requests"]))))

(deftest absent-blank-and-non-string-htmx-targets-are-not-requests
  (doseq [invalid [nil "" "  \n " :not-a-url 42 false]]
    (testing (str "Invalid request target " (pr-str invalid))
      (let [attrs (into {} (for [[attribute _] htmx-methods]
                             [attribute invalid]))]
        (is (empty? (coordinates [:section [:button attrs "Go"]]))))))
  (is (= :get
         (:method (only-coordinate [:a {:hx-get "/valid"} "Read"]))))
  (is (= "/valid"
         (:path (only-coordinate [:a {"hx-get" "/valid"} "Read"])))))

(deftest render-paths-identify-original-hiccup-occurrences
  (let [found (coordinates
               [:main {:id "page"}
                [:div {:class "container"}
                 [:button {:hx-post "/claim"} "Claim"]]
                [:aside [:p "Intro"]
                 [:a {"hx-get" "/refresh"} "Refresh"]]])]
    (is (= [[:post "/claim" [2 2]]
            [:get "/refresh" [3 2]]]
           (mapv (juxt :method :path :render-path) found)))
    (is (= [:hx-post :hx-get] (mapv :request-source found)))))

(deftest framework-metadata-is-reported-without-granting-authority
  (let [canonical (with-meta
                    [:button {:hx-post "/claim"} "Claim"]
                    {ui/choreo-affordance-metadata-key :request/claim})
        rendered [:section
                  canonical
                  [:button {:hx-post "/claim"} "Anonymous"]]
        found (coordinates rendered)]
    (is (= 2 (count found)))
    (is (= [[1] [2]] (mapv :render-path found)))
    (is (= [true false] (mapv :choreo-operation-declared? found)))
    (is (= ["/claim" "/claim"] (mapv :path found)))))

(deftest native-forms-expose-only-explicit-post-actions
  (doseq [attrs [{:method "post" :action "/claim"}
                 {"method" "POST" "action" "/claim"}
                 {:method :post "action" "/claim"}
                 {"method" :POST :action "/claim"}]]
    (testing (pr-str attrs)
      (is (= {:method :post
              :path "/claim"
              :request-source :native-form
              :render-path []
              :choreo-operation-declared? false}
             (only-coordinate [:form attrs [:input {:type "text"}]])))))
  (doseq [attrs [{:action "/search"}
                 {:method "get" :action "/search"}
                 {:method "invalid" :action "/search"}
                 {:method "dialog" :action "/claim"}
                 {:method "post"}
                 {:method "post" :action "   "}]]
    (testing (str "Not an explicit POST: " (pr-str attrs))
      (is (empty? (coordinates [:form attrs [:input {:type "text"}]]))))))

(deftest submitter-overrides-resolve-inherited-action-and-method
  (let [rendered
        [:section
         [:form {:id "owner" :method "post" :action "/claim"}
          [:button {:formaction "/cancel"} "Cancel"]
          [:button {:formmethod "get"} "Read"]
          [:button {:formmethod "dialog"} "Close"]
          [:button {:type "reset" :formaction "/ignored"} "Reset"]]
         [:form {:method "get" :action "/search"}
          [:input {:type "submit" :formmethod "post"}]]]
        found (coordinates rendered)]
    (is (= [[:post "/claim" :native-form [1]]
            [:post "/cancel" :native-submitter [1 2]]
            [:get "/claim" :native-submitter [1 3]]
            [:post "/search" :native-submitter [2 2]]]
           (mapv (juxt :method :path :request-source :render-path) found)))
    (is (= [[1] [1]]
           (mapv :form-render-path (filterv #(= :native-submitter (:request-source %))
                                             (take 3 found)))))
    (is (= [#{:formaction} #{:formmethod} #{:formmethod}]
           (mapv :submitter-overrides
                 (filterv #(= :native-submitter (:request-source %)) found))))))

(deftest detached-submitters-resolve-hiccup-shorthand-form-ids
  (doseq [form-tag [:form#owner "form#owner"]]
    (let [found (coordinates
                 [:main
                  [form-tag {:method "get" :action "/lookup"}]
                  [:aside
                   [:button {"form" "owner"
                             "formmethod" "post"
                             "formaction" "/claim"}
                    "Claim"]]])]
      (testing (str "Form tag " (pr-str form-tag))
        (is (= 1 (count found)))
        (is (= [:post "/claim" :native-submitter [2 1] [1]]
               ((juxt :method :path :request-source :render-path :form-render-path)
                (first found))))
        (is (= #{:formaction :formmethod}
               (:submitter-overrides (first found)))))))
  (let [found (coordinates
               [:main
                [:form {:id "owner" :method "post" :action "/first"}]
                [:form {"id" "owner" :method "post" :action "/second"}]
                [:button {:form "owner" :formmethod "post"} "Submit"]])]
    (is (= ["/first" "/second" "/first"] (mapv :path found)))
    (is (= [1] (:form-render-path (last found))))))

(deftest explicit-form-association-overrides-enclosing-form
  (let [found (coordinates
               [:main
                [:form#remote {:action "/remote"}]
                [:form {:method "post" :action "/local"}
                 [:button {:form "remote" :formmethod "post"} "Submit"]]])]
    (is (= ["/local" "/remote"] (mapv :path found)))
    (is (= [1] (:form-render-path (last found))))
    (is (= [2 2] (:render-path (last found))))))

(deftest submit-button-defaults-and-input-types-follow-html-semantics
  (doseq [tag [:button "button"]
          button-type [nil "" "mystery" :unknown 99]]
    (testing (str "Button type " (pr-str button-type))
      (let [attrs (cond-> {:formmethod "post"}
                    (some? button-type) (assoc :type button-type))]
        (is (= :post
               (:method
                (only-coordinate
                 [:form {:action "/claim"}
                  [tag attrs "Submit"]])))))))
  (doseq [tag [:button :input]
          button-type (if (= tag :button)
                        ["button" "reset"]
                        [nil "text" "button" "reset" "hidden" "invalid"])]
    (testing (str "Non-submit " (pr-str [tag button-type]))
      (is (empty?
           (coordinates
            [:form {:action "/claim"}
             [tag {:type button-type :formmethod "post"}]])))))
  (doseq [button-type ["submit" "image"]]
    (is (= :post
           (:method
            (only-coordinate
             [:form {:action "/claim"}
              [:input {"type" button-type "formmethod" "post"}]])))))
  (is (empty?
       (coordinates
        [:form {:action "/claim"}
         [:button {:type "submit"} "Implicit submit"]]))))

(deftest submitters-need-an-explicit-form-owner-and-resolved-action
  (doseq [hiccup [[:button {:formaction "/claim" :formmethod "post"}]
                  [:button {:form "missing"
                            :formaction "/claim"
                            :formmethod "post"}]
                  [:form#owner
                   [:button {:formmethod "post"}]]
                  [:form {:method "post"}
                   [:button {:formmethod "post"}]]
                  [:form {:action "/claim"}
                   [:button {:formmethod "dialog"}]]]]
    (testing (pr-str hiccup)
      (is (empty? (coordinates hiccup)))))
  (is (= "/claim"
         (:path
          (only-coordinate
           [:form {:method "get"}
            [:button {:formaction "/claim" :formmethod "post"}]])))))

(deftest keyword-string-attribute-conflicts-are-local-and-deterministic
  (let [rendered
        [:section
         [:button {:hx-post "/claim" "hx-post" "/claim"
                   :type "submit" "type" "submit"}]
         [:form {:method "post" "method" "post"
                 :action "/claim" "action" "/claim"}]
         [:input {:formaction "/claim" "formaction" "/claim"}]]
        errors (request/attribute-conflicts rendered :request-board)]
    (is (= [:action :formaction :hx-post :method :type]
           (sort (map :attribute errors))))
    (is (= [[:hx-post [1]]
            [:type [1]]
            [:action [2]]
            [:method [2]]
            [:formaction [3]]]
           (mapv (juxt :attribute :render-path) errors)))
    (is (every? #(= :rendered-ambiguous-html-attribute (:kind %)) errors))
    (is (every? #(= :request-board (:surface %)) errors))
    (is (every? #(and (string? (:message %)) (seq (:message %))) errors))))

(deftest every-scanner-relevant-attribute-rejects-duplicate-spellings
  (let [attributes [:hx-get :hx-post :hx-put :hx-patch :hx-delete
                    :id :method :action :form :formaction :formmethod :type]]
    (doseq [attribute attributes]
      (testing (name attribute)
        (let [errors (request/attribute-conflicts
                      [:button {attribute "value" (name attribute) "value"}]
                      :example)]
          (is (= 1 (count errors)))
          (is (= attribute (:attribute (first errors))))
          (is (= [] (:render-path (first errors)))))))))

(deftest irrelevant-duplicate-attributes-and-ordinary-content-are-ignored
  (is (empty?
       (request/attribute-conflicts
        [:main
         [:div {:data-state "open" "data-state" "closed"} "Content"]
         [:span {:class "x" "class" "y"} "Text"]]
        :ordinary)))
  (is (empty? (coordinates [:main [:p "Nothing submits"] [:a {:href "/page"} "Go"]])))
  (is (empty? (request/attribute-conflicts "A string" :plain)))
  (is (empty? (coordinates nil))))

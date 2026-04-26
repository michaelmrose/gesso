(ns gesso.components.form.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.components.form.attr :as form-attr]
   [gesso.components.form.scripts :as form-scripts]
   [gesso.core :as g]
   [gesso.test.hiccup :as h]
   [gesso.validation.malli :as vmalli]
   [malli.core :as m]))

;; -----------------------------------------------------------------------------
;; Schemas
;; -----------------------------------------------------------------------------

(def ^:private signup-schema
  [:map
   [:username
    [:string {:min 3
              :max 24
              :re #"^[a-z0-9_-]+$"
              :gesso.html/pattern "^[a-z0-9_-]+$"
              :gesso.error/min "Username must be at least 3 characters."
              :gesso.error/max "Username must be at most 24 characters."
              :gesso.error/pattern "Use lowercase letters, numbers, underscores, or hyphens."
              :gesso.error/required "Username is required."}]]

   [:email
    [:and
     [:string {:min 5
               :max 120
               :gesso.html/pattern ".+@.+"
               :gesso.error/pattern "Enter an email address."
               :gesso.error/required "Email is required."}]
     [:re #".+@.+"]]]

   [:age
    [:int {:min 18
           :max 120
           :gesso.error/min "You must be at least 18."
           :gesso.error/max "Age must be 120 or less."}]]

   [:displayName {:optional true}
    [:string {:max 80
              :gesso.error/max "Display name must be at most 80 characters."}]]

   [:bio {:optional true}
    [:maybe [:string {:max 240
                      :gesso.error/max "Bio must be 240 characters or fewer."}]]]

   [:notes {:optional true}
    [:string]]])

(def ^:private unsafe-regex-schema
  [:map
   [:name
    [:string {:re #"(?i)^[a-z]+$"}]]])

;; -----------------------------------------------------------------------------
;; Validation extraction and facade tests
;; -----------------------------------------------------------------------------

(deftest validation-extraction-test
  (testing "string constraints become HTML5 string validation attrs"
    (let [{:keys [rules required messages type nilable?]}
          (vmalli/extract-constraints signup-schema :username)]
      (is (= :string type))
      (is (= false nilable?))
      (is (= true required))
      (is (= {:minlength 3
              :maxlength 24
              :pattern "^[a-z0-9_-]+$"
              :required true}
             rules))
      (is (= "Username must be at least 3 characters."
             (:minlength messages)))
      (is (= "Username must be at most 24 characters."
             (:maxlength messages)))
      (is (= "Use lowercase letters, numbers, underscores, or hyphens."
             (:pattern messages)))
      (is (= "Username is required."
             (:required messages)))))

  (testing "numeric constraints become HTML5 numeric validation attrs"
    (let [{:keys [rules required messages type]}
          (vmalli/extract-constraints signup-schema :age)]
      (is (= :int type))
      (is (= true required))
      (is (= {:min 18
              :max 120
              :required true}
             rules))
      (is (= "You must be at least 18." (:min messages)))
      (is (= "Age must be 120 or less." (:max messages)))))

  (testing "optional fields do not receive required"
    (let [{:keys [rules required nilable?]}
          (vmalli/extract-constraints signup-schema :displayName)]
      (is (= false required))
      (is (= false nilable?))
      (is (= {:maxlength 80} rules))))

  (testing "maybe fields do not receive HTML required"
    (let [{:keys [rules required nilable?]}
          (vmalli/extract-constraints signup-schema :bio)]
      (is (= false required))
      (is (= true nilable?))
      (is (= {:maxlength 240} rules))))

  (testing "explicit browser pattern wins over JVM :re"
    (let [rules (:rules (vmalli/extract-constraints signup-schema :username))]
      (is (= "^[a-z0-9_-]+$" (:pattern rules)))))

  (testing "narrow :and support extracts browser attrs from the primary child"
    (let [{:keys [rules required messages type]}
          (vmalli/extract-constraints signup-schema :email)]
      (is (= :string type))
      (is (= true required))
      (is (= {:minlength 5
              :maxlength 120
              :pattern ".+@.+"
              :required true}
             rules))
      (is (= "Enter an email address." (:pattern messages)))))

  (testing "unsafe Java regex fallback is rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"cannot be safely emitted as an HTML pattern"
         (vmalli/extract-constraints unsafe-regex-schema :name)))))

(deftest validation-facade-field-plan-test
  (testing "gesso.core exposes field-plan"
    (let [{:keys [attrs script constraints]}
          (g/field-plan signup-schema :username "username-error")]
      (is (= {:minlength 3
              :maxlength 24
              :pattern "^[a-z0-9_-]+$"
              :required true}
             attrs))
      (is (string? script))
      (is (str/includes? script "on input or blur"))
      (is (str/includes? script "put msg into #username-error"))
      (is (= :string (:type constraints)))
      (is (= true (:required constraints)))))

  (testing "empty-field-plan is exposed through gesso.core"
    (is (= {:attrs {}
            :script nil
            :constraints nil}
           (g/empty-field-plan)))))

;; -----------------------------------------------------------------------------
;; Field rendering tests
;; -----------------------------------------------------------------------------

(deftest field-short-form-validation-test
  (testing "schema-backed field annotates a direct control"
    (let [node (g/field
                {:label-text "Username"
                 :for :username
                 :schema signup-schema
                 :control [:input {:type "text"
                                    :name "username"
                                    :aria-describedby "external-help"
                                    :_ "on focus add .focused to me"}]
                 :description "Lowercase letters, numbers, underscores, or hyphens."})
          root-attrs (h/element-attrs node)
          control (h/by-id "username" node)
          control-attrs (h/element-attrs control)
          description (h/by-id "username-description" node)
          error (h/by-id "username-error" node)
          error-attrs (h/element-attrs error)]
      (is (= :div (first node)))
      (is (= true (:data-field root-attrs)))
      (is (= "username" (:data-field-for root-attrs)))

      (is (= :input (first control)))
      (is (= "username" (:id control-attrs)))
      (is (= "username" (:name control-attrs)))
      (is (= true (:data-field-control control-attrs)))

      (is (= true (:required control-attrs)))
      (is (= 3 (:minlength control-attrs)))
      (is (= 24 (:maxlength control-attrs)))
      (is (= "^[a-z0-9_-]+$" (:pattern control-attrs)))

      (is (= "external-help username-description"
             (:aria-describedby control-attrs)))
      (is (= "username-error" (:aria-errormessage control-attrs)))
      (is (= "false" (:aria-invalid control-attrs)))

      (is (h/script-includes? control-attrs "on focus add .focused to me"))
      (is (h/script-includes? control-attrs "on input or blur"))
      (is (h/script-includes? control-attrs "put msg into #username-error"))
      (is (h/script-includes? control-attrs "on keydown"))
      (is (h/script-includes? control-attrs "dataset.serverError"))

      (is description)
      (is (= true (:data-field-description (h/element-attrs description))))

      (is error)
      (is (= true (:data-field-error error-attrs)))
      (is (= "alert" (:role error-attrs)))
      (is (= "polite" (:aria-live error-attrs)))
      (is (h/has-class? error-attrs "hidden")))))

(deftest field-schema-backed-error-target-test
  (testing "schema-backed optional field with no local attrs still renders an OOB error target"
    (let [node (g/field
                {:label-text "Notes"
                 :for :notes
                 :schema signup-schema
                 :control [:textarea {:name "notes"}]})
          control (h/by-id "notes" node)
          control-attrs (h/element-attrs control)
          error (h/by-id "notes-error" node)
          error-attrs (h/element-attrs error)]
      (is control)
      (is (= "notes" (:id control-attrs)))
      (is (= "notes-error" (:aria-errormessage control-attrs)))
      (is (= "false" (:aria-invalid control-attrs)))
      (is (h/script-includes? control-attrs "on keydown"))
      (is (not (h/script-includes? control-attrs "on input or blur")))
      (is error)
      (is (h/has-class? error-attrs "hidden")))))

(deftest field-active-error-test
  (testing "active errors mark root and control invalid and show the error container"
    (let [node (g/field
                {:label-text "Email"
                 :for :email
                 :schema signup-schema
                 :control [:input {:type "email"
                                    :name "email"}]
                 :description "Used for recovery."
                 :error "Email is already taken."})
          root-attrs (h/element-attrs node)
          control-attrs (h/element-attrs (h/by-id "email" node))
          error-attrs (h/element-attrs (h/by-id "email-error" node))]
      (is (= "true" (:data-invalid root-attrs)))
      (is (= "true" (:aria-invalid control-attrs)))
      (is (= "email-description email-error"
             (:aria-describedby control-attrs)))
      (is (= "email-error" (:aria-errormessage control-attrs)))
      (is (not (h/has-class? error-attrs "hidden"))))))

(deftest field-key-override-test
  (testing ":field-key lets schema lookup differ from the DOM id"
    (let [node (g/field
                {:label-text "User email"
                 :for "user-email"
                 :field-key :email
                 :schema signup-schema
                 :control [:input {:type "email"
                                    :name "email"}]})
          control-attrs (h/element-attrs (h/by-id "user-email" node))]
      (is (= "user-email" (:id control-attrs)))
      (is (= true (:required control-attrs)))
      (is (= ".+@.+" (:pattern control-attrs)))
      (is (= "user-email-error" (:aria-errormessage control-attrs))))))

(deftest field-long-form-test
  (testing "long form is structural and does not annotate children"
    (let [manual-input [:input {:id "manual"
                                :name "manual"
                                :class "input-theme"}]
          node (g/field
                {:for :manual
                 :orientation :horizontal
                 :valid? true}
                [:div {:class "custom-layout"}
                 manual-input])
          root-attrs (h/element-attrs node)
          control-attrs (h/element-attrs (h/by-id "manual" node))]
      (is (= true (:data-field root-attrs)))
      (is (= "manual" (:data-field-for root-attrs)))
      (is (= "horizontal" (:data-orientation root-attrs)))
      (is (= "true" (:data-valid root-attrs)))
      (is (= "manual" (:id control-attrs)))
      (is (nil? (:data-field-control control-attrs)))
      (is (nil? (:aria-errormessage control-attrs))))))

;; -----------------------------------------------------------------------------
;; Form attr and script tests
;; -----------------------------------------------------------------------------

(deftest form-submission-attrs-test
  (testing ":to is POST shorthand"
    (is (= {:hx-post "/save"
            :hx-swap "innerHTML"}
           (form-attr/submission-attrs {:to "/save"}))))

  (testing "explicit POST supports target and swap"
    (is (= {:hx-post "/save"
            :hx-target "#panel"
            :hx-swap "outerHTML"}
           (form-attr/submission-attrs
            {:post "/save"
             :target "#panel"
             :swap "outerHTML"}))))

  (testing "put patch and delete verbs are supported"
    (is (= {:hx-put "/resource"
            :hx-swap "innerHTML"}
           (form-attr/submission-attrs {:put "/resource"})))

    (is (= {:hx-patch "/resource"
            :hx-swap "innerHTML"}
           (form-attr/submission-attrs {:patch "/resource"})))

    (is (= {:hx-delete "/resource"
            :hx-swap "innerHTML"}
           (form-attr/submission-attrs {:delete "/resource"}))))

  (testing "no route produces no HTMX submission attrs"
    (is (= {} (form-attr/submission-attrs {}))))

  (testing "multiple route keys are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"only one submission route key"
         (form-attr/submission-attrs
          {:post "/a"
           :patch "/b"})))))

(deftest validation-sentinel-attrs-test
  (testing "no validate-url means no sentinel"
    (is (nil? (form-attr/validation-sentinel-attrs {}))))

  (testing "validate-url creates the default hidden sentinel attrs"
    (is (= {:data-form-validator true
            :hidden true
            :aria-hidden "true"
            :hx-post "/validate"
            :hx-trigger "validateField from:closest form delay:250ms"
            :hx-include "closest form"
            :hx-target "this"
            :hx-swap "none"
            :hx-sync "closest form:drop"}
           (form-attr/validation-sentinel-attrs
            {:validate-url "/validate"}))))

  (testing "sentinel attrs can be overridden"
    (let [attrs (form-attr/validation-sentinel-attrs
                 {:validate-url "/validate"
                  :validation-trigger "validateField delay:500ms"
                  :validation-include "#profile-form"
                  :validation-target "#validation-sink"
                  :validation-swap "innerHTML"
                  :validation-sync "closest form:replace"})]
      (is (= "validateField delay:500ms" (:hx-trigger attrs)))
      (is (= "#profile-form" (:hx-include attrs)))
      (is (= "#validation-sink" (:hx-target attrs)))
      (is (= "innerHTML" (:hx-swap attrs)))
      (is (= "closest form:replace" (:hx-sync attrs))))))

(deftest form-script-test
  (testing "submission guard includes the intended lifecycle and invalid-state checks"
    (let [script (form-scripts/submission-guard)]
      (is (str/includes? script "on htmx:validation:validate"))
      (is (str/includes? script "trigger blur on el"))
      (is (str/includes? script "aria-invalid='true'"))
      (is (str/includes? script "halt the event")))))

;; -----------------------------------------------------------------------------
;; Form rendering through gesso.core facade
;; -----------------------------------------------------------------------------

(deftest form-rendering-test
  (testing "form renders a real form with CSRF, submission attrs, validation sentinel, guard, and children"
    (let [ctx {:anti-forgery-token "TOKEN"}
          node (g/form ctx
                 {:post "/profile"
                  :target "#profile-form"
                  :swap "outerHTML"
                  :validate-url "/profile/validate"
                  :class "extra-form"
                  :attrs {:id "profile-form"
                          :data-demo "yes"}}
                 [:section {:id "nested-section"}
                  (g/field
                   {:label-text "Username"
                    :for :username
                    :schema signup-schema
                    :control [:input {:type "text"
                                       :name "username"}]})
                  [:div {:class "deeply-nested"}
                   [:input {:type "hidden"
                            :name "step"
                            :value "account"}]]]
                 [:button {:type "submit"} "Save"])
          form-attrs (h/element-attrs node)
          token-input (h/by-name "__anti-forgery-token" node)
          sentinel (h/by-attr :data-form-validator true node)
          username-control (h/by-id "username" node)
          hidden-step (h/by-name "step" node)]
      (is (= :form (first node)))
      (is (= "profile-form" (:id form-attrs)))
      (is (= "yes" (:data-demo form-attrs)))
      (is (= true (:data-form form-attrs)))
      (is (h/has-class? form-attrs "form-theme"))
      (is (h/has-class? form-attrs "w-full"))
      (is (h/has-class? form-attrs "extra-form"))

      (is (= "/profile" (:hx-post form-attrs)))
      (is (= "#profile-form" (:hx-target form-attrs)))
      (is (= "outerHTML" (:hx-swap form-attrs)))
      (is (h/script-includes? form-attrs "htmx:validation:validate"))

      (is token-input)
      (is (= "hidden" (:type (h/element-attrs token-input))))
      (is (= "TOKEN" (:value (h/element-attrs token-input))))

      (is sentinel)
      (is (= "/profile/validate" (:hx-post (h/element-attrs sentinel))))
      (is (= "closest form" (:hx-include (h/element-attrs sentinel))))
      (is (= "none" (:hx-swap (h/element-attrs sentinel))))

      (is username-control)
      (is (= "username" (:name (h/element-attrs username-control))))
      (is hidden-step)
      (is (= "account" (:value (h/element-attrs hidden-step)))))))

(deftest form-without-validation-test
  (testing "non-validating form does not attach sentinel or guard by default"
    (let [node (g/form {}
                 {:post "/plain"
                  :attrs {:id "plain-form"}}
                 [:input {:name "plain"}]
                 [:button {:type "submit"} "Submit"])
          form-attrs (h/element-attrs node)]
      (is (= "/plain" (:hx-post form-attrs)))
      (is (nil? (:_ form-attrs)))
      (is (nil? (h/by-attr :data-form-validator true node)))
      (is (nil? (h/by-name "__anti-forgery-token" node))))))

(deftest form-existing-script-test
  (testing "validation guard appends to existing form scripts"
    (let [node (g/form {}
                 {:post "/save"
                  :validate-url "/validate"
                  :attrs {:_ "on submit log me"}}
                 [:input {:name "x"}])
          form-attrs (h/element-attrs node)]
      (is (h/script-includes? form-attrs "on submit log me"))
      (is (h/script-includes? form-attrs "on htmx:validation:validate")))))

(deftest form-guard-override-test
  (testing "guard can be disabled even when validate-url is present"
    (let [node (g/form {}
                 {:post "/save"
                  :validate-url "/validate"
                  :guard? false}
                 [:input {:name "x"}])
          form-attrs (h/element-attrs node)]
      (is (nil? (:_ form-attrs)))
      (is (h/by-attr :data-form-validator true node)))))

;; -----------------------------------------------------------------------------
;; post-button tests
;; -----------------------------------------------------------------------------

(deftest post-button-rendering-test
  (testing "post-button renders an inline form with CSRF, route attrs, and one submit button"
    (let [ctx {:biff/anti-forgery-token "BIFF-TOKEN"}
          node (g/post-button ctx
                 {:delete "/sessions/current"
                  :target "#session-panel"
                  :swap "outerHTML"
                  :form-attrs {:id "signout-form"
                               :data-form-kind "action"}
                  :button-attrs {:class "btn-outline"
                                 :data-action "sign-out"}
                  :class "extra-button"
                  :attrs {:aria-label "Sign out"}}
                 "Sign out")
          form-attrs (h/element-attrs node)
          token-input (h/by-name "__anti-forgery-token" node)
          button (first (h/elements-by-tag :button node))
          button-attrs (h/element-attrs button)]
      (is (= :form (first node)))
      (is (= "signout-form" (:id form-attrs)))
      (is (= "action" (:data-form-kind form-attrs)))
      (is (= true (:data-form form-attrs)))
      (is (= true (:data-form-inline form-attrs)))
      (is (= "/sessions/current" (:hx-delete form-attrs)))
      (is (= "#session-panel" (:hx-target form-attrs)))
      (is (= "outerHTML" (:hx-swap form-attrs)))
      (is (h/has-class? form-attrs "inline-flex"))

      (is token-input)
      (is (= "BIFF-TOKEN" (:value (h/element-attrs token-input))))

      (is button)
      (is (= "submit" (:type button-attrs)))
      (is (= "Sign out" (:aria-label button-attrs)))
      (is (= "sign-out" (:data-action button-attrs)))
      (is (h/has-class? button-attrs "button-density"))
      (is (h/has-class? button-attrs "btn-outline"))
      (is (h/has-class? button-attrs "extra-button")))))

(deftest post-button-content-precedence-test
  (testing "explicit children beat :children and :label"
    (let [node (g/post-button {}
                 {:post "/do"
                  :children "From opts"
                  :label "From label"}
                 [:span {:id "explicit-child"} "Explicit"])
          button (first (h/elements-by-tag :button node))
          child (first (h/element-children button))]
      (is (= [:span {:id "explicit-child"} "Explicit"] child))))

  (testing ":children beats :label and preserves Hiccup child nodes"
    (let [node (g/post-button {}
                 {:post "/do"
                  :children [:span {:id "opts-child"} "From opts"]
                  :label "From label"})
          button (first (h/elements-by-tag :button node))
          child (first (h/element-children button))]
      (is (= [:span {:id "opts-child"} "From opts"] child))))

  (testing ":label is used when no children are provided"
    (let [node (g/post-button {}
                 {:post "/do"
                  :label "From label"})
          button (first (h/elements-by-tag :button node))]
      (is (= ["From label"] (vec (h/element-children button)))))))

(deftest post-button-without-token-test
  (testing "post-button omits anti-forgery input when no token is present"
    (let [node (g/post-button {}
                 {:post "/do"}
                 "Do it")]
      (is (nil? (h/by-name "__anti-forgery-token" node)))
      (is (h/no-nil-children? node)))))

;; -----------------------------------------------------------------------------
;; Server OOB validation tests
;; -----------------------------------------------------------------------------

(deftest render-oob-errors-through-facade-test
  (testing "render-oob-errors is exposed and targets field error containers"
    (let [bad-data {:username "a"
                    :email "not-an-email"
                    :age 12}
          explain-data (m/explain signup-schema bad-data)
          node (g/render-oob-errors explain-data)
          username-error (h/by-id "username-error" node)
          email-error (h/by-id "email-error" node)
          age-error (h/by-id "age-error" node)
          scripts (h/elements-by-tag :script node)]
      (is (= :<> (first node)))
      (is username-error)
      (is (= "innerHTML" (:hx-swap-oob (h/element-attrs username-error))))
      (is email-error)
      (is (= "innerHTML" (:hx-swap-oob (h/element-attrs email-error))))
      (is age-error)
      (is (= "innerHTML" (:hx-swap-oob (h/element-attrs age-error))))
      (is (some #(str/includes? (second %) "dataset.serverError='true'")
                scripts))
      (is (some #(str/includes? (second %) "aria-invalid")
                scripts)))))

(deftest validation-path-id-facade-test
  (testing "path id helpers are exposed through gesso.core"
    (is (= "email" (g/path->field-id [:email])))
    (is (= "email-error" (g/path->err-id [:email])))
    (is (= "user-email" (g/path->field-id [:user :email])))
    (is (= "user-email-error" (g/path->err-id [:user :email])))))

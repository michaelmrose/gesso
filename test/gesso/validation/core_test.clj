(ns gesso.validation.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.validation.core :as validation]
   [gesso.validation.htmx :as vhtmx]
   [gesso.validation.malli :as vmalli]
   [gesso.validation.scripts :as vscripts]
   [malli.core :as m]))

(def ^:private user-schema
  [:map
   [:email
    [:string {:min 5
              :max 100
              :gesso.html/pattern ".+@.+"
              :gesso.error/min "Email is too short."
              :gesso.error/maxlength "Email is too long."
              :gesso.error/pattern "Email format is invalid."
              :gesso.error/required "Email is required."}]]

   [:username
    [:string {:min 3
              :max 24
              :re #"^[a-z0-9_-]+$"
              :gesso.html/pattern "^[a-z0-9_-]+$"
              :gesso.error/pattern "Use lowercase letters, numbers, underscores, or hyphens."}]]

   [:age
    [:int {:min 18
           :max 120
           :gesso.error/min "Must be at least 18."
           :gesso.error/max "Must be no more than 120."}]]

   [:nickname {:optional true}
    [:string {:max 20}]]

   [:middle-name
    [:maybe [:string {:max 40}]]]])

(defn- hiccup-seq
  [x]
  (tree-seq coll? seq x))

(defn- attrs-with-id
  [node-id hiccup]
  (some (fn [x]
          (when (and (map? x)
                     (= node-id (:id x)))
            x))
        (hiccup-seq hiccup)))

(defn- script-node-js
  [node]
  (when (and (vector? node)
             (= :script (first node)))
    (get-in (second node) [:dangerouslySetInnerHTML :__html] "")))

(deftest extract-string-field-constraints-test
  (testing "string fields map min/max/pattern/required into HTML5 attrs"
    (let [{:keys [rules required messages type nilable?]}
          (vmalli/extract-constraints user-schema :email)]
      (is (= :string type))
      (is (= false nilable?))
      (is (= true required))
      (is (= {:minlength 5
              :maxlength 100
              :pattern ".+@.+"
              :required true}
             rules))
      (is (= "Email is too short." (:minlength messages)))
      (is (= "Email is too long." (:maxlength messages)))
      (is (= "Email format is invalid." (:pattern messages)))
      (is (= "Email is required." (:required messages))))))

(deftest extract-numeric-field-constraints-test
  (testing "numeric fields map min/max as numeric range attrs"
    (let [{:keys [rules required messages type nilable?]}
          (vmalli/extract-constraints user-schema :age)]
      (is (= :int type))
      (is (= false nilable?))
      (is (= true required))
      (is (= {:min 18
              :max 120
              :required true}
             rules))
      (is (= "Must be at least 18." (:min messages)))
      (is (= "Must be no more than 120." (:max messages))))))

(deftest optional-and-maybe-fields-test
  (testing "optional map entries do not receive required"
    (let [{:keys [rules required nilable?]}
          (vmalli/extract-constraints user-schema :nickname)]
      (is (= false required))
      (is (= false nilable?))
      (is (= {:maxlength 20} rules))))

  (testing "required map keys wrapped in maybe do not receive HTML required"
    (let [{:keys [rules required nilable?]}
          (vmalli/extract-constraints user-schema :middle-name)]
      (is (= false required))
      (is (= true nilable?))
      (is (= {:maxlength 40} rules)))))

(deftest browser-pattern-precedence-test
  (testing "explicit browser pattern wins over :re"
    (let [{:keys [rules messages]}
          (vmalli/extract-constraints user-schema :username)]
      (is (= "^[a-z0-9_-]+$" (:pattern rules)))
      (is (= "Use lowercase letters, numbers, underscores, or hyphens."
             (:pattern messages))))))

(deftest java-regex-fallback-test
  (testing "simple Java regex can be used as fallback pattern"
    (let [schema [:map
                  [:code
                   [:string {:re #"^[A-Z]{2}[0-9]{4}$"}]]]
          result (vmalli/extract-constraints schema :code)]
      (is (= "^[A-Z]{2}[0-9]{4}$"
             (get-in result [:rules :pattern])))))

  (testing "known unsafe Java regex constructs are rejected"
    (let [schema [:map
                  [:name
                   [:string {:re #"(?i)^[a-z]+$"}]]]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot be safely emitted as an HTML pattern"
           (vmalli/extract-constraints schema :name))))))

(deftest unknown-field-test
  (testing "unknown fields return an empty result"
    (is (= {:rules {}
            :required false
            :messages {}
            :type nil
            :nilable? false}
           (vmalli/extract-constraints user-schema :missing)))))

(deftest gatekeeper-script-test
  (testing "gatekeeper script contains native validity checks and error target"
    (let [script (vscripts/gatekeeper-script
                  {:required "Email is required."
                   :minlength "Email is too short."
                   :maxlength "Email is too long."
                   :pattern "Email is invalid."
                   :min "Too small."
                   :max "Too large."}
                  "email-error")]
      ;; The current gatekeeper intentionally uses separate event handlers:
      ;; input validates quietly unless already touched; blur marks touched and
      ;; reveals validation state.
      (is (str/includes? script "on input"))
      (is (str/includes? script "on blur"))
      (is (not (str/includes? script "on input or blur")))

      (is (str/includes? script "my.checkValidity()"))
      (is (str/includes? script "my.validity.valueMissing"))
      (is (str/includes? script "my.validity.tooShort"))
      (is (str/includes? script "my.validity.tooLong"))
      (is (str/includes? script "my.validity.typeMismatch"))
      (is (str/includes? script "my.validity.patternMismatch"))
      (is (str/includes? script "my.validity.rangeUnderflow"))
      (is (str/includes? script "my.validity.rangeOverflow"))

      (is (str/includes? script "else if my.dataset.touched == 'true'"))
      (is (str/includes? script "call me.setAttribute('data-touched', 'true')"))
      (is (str/includes? script "put msg into #email-error"))
      (is (str/includes? script "remove .hidden from #email-error"))
      (is (str/includes? script "call me.setAttribute('aria-invalid', 'true')"))
      (is (str/includes? script "call me.setAttribute('aria-invalid', 'false')"))
      (is (str/includes? script "trigger validateField on me"))))

  (testing "messages are escaped before interpolation"
    (let [script (vscripts/gatekeeper-script
                  {:required "Can't be blank"}
                  "field-error")]
      (is (str/includes? script "Can\\'t be blank")))))

(deftest field-plan-test
  (testing "nil schema returns the empty plan"
    (is (= {:attrs {}
            :script nil
            :constraints nil}
           (validation/field-plan nil :email "email-error"))))

  (testing "field-plan returns attrs, script, and raw constraints"
    (let [{:keys [attrs script constraints]}
          (validation/field-plan user-schema :email "email-error")]
      (is (= {:minlength 5
              :maxlength 100
              :pattern ".+@.+"
              :required true}
             attrs))
      (is (string? script))
      (is (str/includes? script "on input"))
      (is (str/includes? script "on blur"))
      (is (str/includes? script "put msg into #email-error"))
      (is (= :string (:type constraints)))
      (is (= true (:required constraints)))))

  (testing "field-plan accepts string field keys"
    (let [{:keys [attrs]} (validation/field-plan user-schema "email" "email-error")]
      (is (= 5 (:minlength attrs)))))

  (testing "field-plan does not generate a script when no err-id is supplied"
    (let [{:keys [attrs script]}
          (validation/field-plan user-schema :email nil)]
      (is (seq attrs))
      (is (nil? script))))

  (testing "field-plan does not generate a script for optional fields without attrs"
    (let [schema [:map
                  [:notes {:optional true} [:string]]]
          {:keys [attrs script constraints]}
          (validation/field-plan schema :notes "notes-error")]
      (is (= {} attrs))
      (is (nil? script))
      (is (= :string (:type constraints)))
      (is (= false (:required constraints))))))

(deftest htmx-id-conversion-test
  (testing "paths are converted to stable field and error ids"
    (is (= "email" (vhtmx/path->field-id [:email])))
    (is (= "email-error" (vhtmx/path->err-id [:email])))
    (is (= "user-email" (vhtmx/path->field-id [:user :email])))
    (is (= "user-email-error" (vhtmx/path->err-id [:user :email])))
    (is (= "email-error" (vhtmx/path->err-id :email)))))

(deftest render-oob-errors-test
  (testing "Malli explain-data renders HTMX OOB updates"
    (let [schema [:map
                  [:email [:string {:min 5}]]
                  [:age [:int {:min 18}]]]
          explain-data (m/explain schema {:email "x"
                                          :age 12})
          hiccup (vhtmx/render-oob-errors explain-data)]
      (is (= :<> (first hiccup)))
      (is (= "innerHTML" (:hx-swap-oob (attrs-with-id "email-error" hiccup))))
      (is (= "innerHTML" (:hx-swap-oob (attrs-with-id "age-error" hiccup))))
      (is (some (fn [node]
                  (when-let [js (script-node-js node)]
                    (str/includes? js "aria-invalid")))
                (hiccup-seq hiccup))))))

#_(deftest render-oob-errors-test
  (testing "Malli explain-data renders HTMX OOB updates"
    (let [schema [:map
                  [:email [:string {:min 5}]]
                  [:age [:int {:min 18}]]]
          explain-data (m/explain schema {:email "x"
                                          :age 12})
          hiccup (vhtmx/render-oob-errors explain-data)]
      (is (= :<> (first hiccup)))
      (is (= "innerHTML" (:hx-swap-oob (attrs-with-id "email-error" hiccup))))
      (is (= "innerHTML" (:hx-swap-oob (attrs-with-id "age-error" hiccup))))
      (is (some #(str/includes? (script-node-js %) "aria-invalid")
                (hiccup-seq hiccup))))))

(deftest render-oob-errors-empty-test
  (testing "nil explain-data returns nil"
    (is (nil? (vhtmx/render-oob-errors nil)))))

(deftest optional-entry-field-plan-test
  (testing "field-plan preserves optional map entry requiredness"
    (let [schema [:map
                  [:notes {:optional true} [:string]]]
          plan   (validation/field-plan schema :notes "notes-error")]
      (is (= {} (:attrs plan)))
      (is (nil? (:script plan)))
      (is (= false (get-in plan [:constraints :required])))
      (is (= :string (get-in plan [:constraints :type]))))))

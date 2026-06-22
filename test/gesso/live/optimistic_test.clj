(ns gesso.live.optimistic-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.optimistic :as optimistic]))

(def pending-card
  [:details
   {:data-request-card "request-1"
    :open true}
   [:summary "Claimed"]
   [:div "Confirming…"]])

;; -----------------------------------------------------------------------------
;; Descriptor construction
;; -----------------------------------------------------------------------------

(deftest new-template-name-test
  (testing "generated template names are browser-safe and unique"
    (let [a (optimistic/new-template-name)
          b (optimistic/new-template-name)]
      (is (str/starts-with? a optimistic/default-template-prefix))
      (is (str/starts-with? b optimistic/default-template-prefix))
      (is (not= a b)))))

(deftest target-sync-test
  (testing "target-sync preserves selector-like targets"
    (is (= "closest [data-request-card]:drop"
           (optimistic/target-sync
            "closest [data-request-card]"))))

  (testing "target-sync normalizes bare ids"
    (is (= "#request-card-1:drop"
           (optimistic/target-sync "request-card-1"))))

  (testing "target-sync accepts another strategy"
    (is (= "#request-card-1:abort"
           (optimistic/target-sync "request-card-1" :abort))))

  (testing "target-sync rejects missing targets and blank strategies"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":target must be a non-blank string"
         (optimistic/target-sync nil)))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"strategy"
         (optimistic/target-sync "request-card-1" "")))))

(deftest optimistic-descriptor-test
  (let [descriptor
        (optimistic/->optimistic
         {:template-name "request-1-claim"
          :target "closest [data-request-card]"
          :action :claim
          :pending-label "Claiming…"
          :content pending-card
          :attrs {:class "action-source"}
          :template-attrs {:class "optimistic-template"}})]
    (testing "descriptor normalizes the public config"
      (is (optimistic/optimistic? descriptor))
      (is (= optimistic/descriptor-type
             (:gesso.live.optimistic/type descriptor)))
      (is (= "request-1-claim"
             (:template-name descriptor)))
      (is (= "closest [data-request-card]"
             (:target descriptor)))
      (is (= "claim"
             (:action descriptor)))
      (is (= "Claiming…"
             (:pending-label descriptor)))
      (is (= pending-card
             (:content descriptor)))
      (is (= "closest [data-request-card]:drop"
             (:sync descriptor)))
      (is (= {:class "action-source"}
             (:attrs descriptor)))
      (is (= {:class "optimistic-template"}
             (:template-attrs descriptor))))

    (testing "ensure-optimistic preserves an existing descriptor"
      (is (identical? descriptor
                      (optimistic/ensure-optimistic descriptor))))))

(deftest optimistic-target-normalization-test
  (testing "bare target ids become CSS id selectors"
    (is (= "#request-card-1"
           (:target
            (optimistic/->optimistic
             {:template-name "request-1-claim"
              :target "request-card-1"
              :content pending-card})))))

  (testing "explicit sync overrides the target-scoped default"
    (is (= "closest form:drop"
           (:sync
            (optimistic/->optimistic
             {:template-name "request-1-claim"
              :target "request-card-1"
              :content pending-card
              :sync "closest form:drop"})))))

  (testing "explicit nil or false disables the suggested sync"
    (is (nil?
         (:sync
          (optimistic/->optimistic
           {:template-name "request-1-claim"
            :target "request-card-1"
            :content pending-card
            :sync nil}))))

    (is (nil?
         (:sync
          (optimistic/->optimistic
           {:template-name "request-1-claim"
            :target "request-card-1"
            :content pending-card
            :sync false}))))))

;; -----------------------------------------------------------------------------
;; Browser-facing pieces
;; -----------------------------------------------------------------------------

(deftest source-attrs-test
  (let [attrs
        (optimistic/source-attrs
         (optimistic/->optimistic
          {:template-name "request-1-claim"
           :target "closest [data-request-card]"
           :action :claim
           :pending-label "Claiming…"
           :content pending-card
           :attrs {:class "claim-button"
                   :data-app-action "claim"}}))]
    (testing "source attrs match the current gesso-live.js protocol"
      (is (= {:class "claim-button"
              :data-app-action "claim"
              :data-gesso-optimistic-template "request-1-claim"
              :data-gesso-optimistic-target "closest [data-request-card]"
              :data-gesso-optimistic-action "claim"
              :data-gesso-optimistic-label "Claiming…"}
             attrs)))))

(deftest source-attrs-protect-required-protocol-test
  (testing "caller attrs cannot replace required optimistic identity or target"
    (is (= {:data-gesso-optimistic-template "real-template"
            :data-gesso-optimistic-target "#real-target"
            :data-gesso-optimistic-action "claim"
            :data-gesso-optimistic-label "Claiming…"}
           (optimistic/source-attrs
            (optimistic/->optimistic
             {:template-name "real-template"
              :target "real-target"
              :action :claim
              :pending-label "Claiming…"
              :content pending-card
              :attrs
              {:data-gesso-optimistic-template "wrong-template"
               :data-gesso-optimistic-target "#wrong-target"
               :data-gesso-optimistic-action "wrong-action"
               :data-gesso-optimistic-label "Wrong label"}}))))))

(deftest optimistic-template-test
  (testing "template renders one hidden server-rendered optimistic root"
    (is (= [:template
            {:class "optimistic-template"
             :data-gesso-optimistic-template "request-1-claim"}
            pending-card]
           (optimistic/template
            (optimistic/->optimistic
             {:template-name "request-1-claim"
              :target "closest [data-request-card]"
              :content pending-card
              :template-attrs {:class "optimistic-template"}})))))

  (testing "caller template attrs cannot replace template identity"
    (is (= [:template
            {:data-gesso-optimistic-template "real-template"}
            pending-card]
           (optimistic/template
            (optimistic/->optimistic
             {:template-name "real-template"
              :target "closest [data-request-card]"
              :content pending-card
              :template-attrs
              {:data-gesso-optimistic-template "wrong-template"}}))))))

(deftest render-parts-test
  (let [parts
        (optimistic/render-parts
         {:template-name "request-1-claim"
          :target "closest [data-request-card]"
          :action :claim
          :pending-label "Claiming…"
          :content pending-card})]
    (testing "render-parts prepares the pieces needed by gesso.live.ui"
      (is (optimistic/optimistic? (:optimistic parts)))
      (is (= "closest [data-request-card]:drop"
             (:sync parts)))
      (is (= "request-1-claim"
             (get (:source-attrs parts)
                  :data-gesso-optimistic-template)))
      (is (= :template
             (first (:template parts)))))))

(deftest render-parts-shares-generated-identity-test
  (let [parts
        (optimistic/render-parts
         {:target "closest [data-request-card]"
          :content pending-card})
        source-name
        (get (:source-attrs parts)
             :data-gesso-optimistic-template)
        template-name
        (get-in parts [:template 1 :data-gesso-optimistic-template])]
    (testing "one preparation generates one identity shared by source and template"
      (is (str/starts-with?
           source-name
           optimistic/default-template-prefix))
      (is (= source-name template-name)))))

(deftest markup-helpers-require-one-prepared-descriptor-test
  (testing "separate raw calls cannot silently generate mismatched identities"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"require a prepared descriptor"
         (optimistic/source-attrs
          {:target "request-card-1"
           :content pending-card})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"require a prepared descriptor"
         (optimistic/template
          {:target "request-card-1"
           :content pending-card})))))

;; -----------------------------------------------------------------------------
;; Validation
;; -----------------------------------------------------------------------------

(deftest optimistic-validation-test
  (testing "target is required and must be a selector/id string"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":target must be a non-blank string"
         (optimistic/->optimistic
          {:content pending-card})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":target must be a non-blank string"
         (optimistic/->optimistic
          {:target "   "
           :content pending-card})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":target must be a non-blank string"
         (optimistic/->optimistic
          {:target :request-card-1
           :content pending-card}))))

  (testing "content is required"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires :content"
         (optimistic/->optimistic
          {:target "request-card-1"}))))

  (testing "content must have exactly one rooted Hiccup element"
    (doseq [invalid-content
            [[:<> [:div "one"] [:div "two"]]
             [[:div "one"] [:div "two"]]
             (list [:div "one"])
             "not hiccup"]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"one rooted Hiccup element"
           (optimistic/->optimistic
            {:target "request-card-1"
             :content invalid-content})))))

  (testing "blank explicit names and labels are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":template-name must not be blank"
         (optimistic/->optimistic
          {:template-name ""
           :target "request-card-1"
           :content pending-card})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":action must not be blank"
         (optimistic/->optimistic
          {:action "   "
           :target "request-card-1"
           :content pending-card})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":pending-label must be a non-blank string"
         (optimistic/->optimistic
          {:pending-label ""
           :target "request-card-1"
           :content pending-card}))))

  (testing "attrs must be maps"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":attrs must be a map"
         (optimistic/->optimistic
          {:target "request-card-1"
           :content pending-card
           :attrs [:not-a-map]})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":template-attrs must be a map"
         (optimistic/->optimistic
          {:target "request-card-1"
           :content pending-card
           :template-attrs [:not-a-map]}))))

  (testing "sync must be a non-blank string when enabled"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":sync must be"
         (optimistic/->optimistic
          {:target "request-card-1"
           :content pending-card
           :sync ""})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":sync must be"
         (optimistic/->optimistic
          {:target "request-card-1"
           :content pending-card
           :sync :drop})))))

(ns gesso.live.ui-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.continuity :as continuity]
   [gesso.live.htmx :as htmx]
   [gesso.live.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Fixtures / Hiccup helpers
;; -----------------------------------------------------------------------------

(def ctx
  {:anti-forgery-token
   "token-primary"})

(def fragment-spec
  {:id
   "request-list"

   :src
   "/app/requests/fragment"

   :stream-url
   "/app/requests/stream"})

(defn- children
  [node]
  (let [tail
        (rest
         node)]
    (if (map?
         (first
          tail))
      (rest
       tail)
      tail)))

(defn- child-by-tag
  [node tag]
  (some
   #(when
      (and
       (vector?
        %)
       (= tag
          (first
           %)))
      %)
   (children
    node)))

(defn- form-node
  [markup]
  (if (= :form
         (first
          markup))
    markup
    (child-by-tag
     markup
     :form)))

(defn- button-node
  [markup]
  (child-by-tag
   (form-node
    markup)
   :button))

(defn- template-node
  [markup]
  (child-by-tag
   markup
   :template))

(defn- hidden-anti-forgery
  [markup]
  (some
   #(when
      (and
       (vector?
        %)
       (= :input
          (first
           %))
       (= "__anti-forgery-token"
          (get-in
           %
           [1
            :name])))
      %)
   (children
    (form-node
     markup))))

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch Throwable error
      error)))

(defn- thrown-data
  [f]
  (some->
   (thrown
    f)
   ex-data))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(deftest defaults-test
  (is (= "/app/gesso/live/stream"
         ui/default-stream-base-url))

  (is (= "/gesso/gesso-live.js"
         ui/default-live-script-src))

  (is (= htmx/default-fragment-swap
         ui/default-fragment-swap))

  (is (= htmx/default-post-swap
         ui/default-post-swap))

  (is (= "closest [data-gesso-live-fragment]:drop"
         ui/default-post-sync))

  (is (= "closest [data-gesso-live-post]"
         ui/default-post-include)))

;; -----------------------------------------------------------------------------
;; Fragment descriptors
;; -----------------------------------------------------------------------------

(deftest fragment-descriptor-defaults-test
  (let [fragment
        (ui/->fragment
         fragment-spec)]
    (is (ui/fragment? fragment))
    (is (= :gesso.live.ui/fragment
           (:gesso.live.ui/type fragment)))
    (is (= "request-list" (:id fragment)))
    (is (= "/app/requests/fragment" (:src fragment)))
    (is (= "/app/requests/stream" (:stream-url fragment)))
    (is (= htmx/default-event (:event fragment)))
    (is (= ui/default-fragment-swap (:swap fragment)))
    (is (not (contains? fragment :trigger)))
    (is (not (contains? fragment :jitter-ms)))
    (is (not (contains? fragment :jitter-delay-ms)))
    (is (= {} (:attrs fragment)))
    (is (= {} (:root-attrs fragment)))
    (is (= {} (:target-attrs fragment)))))

(deftest fragment-predicate-is-marker-based-test
  (is (ui/fragment?
       {:gesso.live.ui/type
        :gesso.live.ui/fragment}))

  (is (false?
       (ui/fragment?
        {})))

  (is (false?
       (ui/fragment?
        nil)))

  (is (false?
       (ui/fragment?
        {:gesso.live.ui/type
         :other}))))

(deftest ensure-fragment-preserves-prepared-descriptor-identity-test
  (let [fragment
        (ui/->fragment
         fragment-spec)]

    (is (identical?
         fragment
         (ui/ensure-fragment
          fragment)))))

(deftest ensure-fragment-builds-raw-map-test
  (is (= (ui/->fragment
          fragment-spec)
         (ui/ensure-fragment
          fragment-spec))))

(deftest fragment-explicit-options-test
  (let [continuity-config
        {:preserve {:inputs true}}
        fragment
        (ui/->fragment
         (merge
          fragment-spec
          {:event "board-updated"
           :swap "innerHTML"
           :include "#board-state"
           :client-continuity continuity-config
           :attrs {:class "base"}
           :root-attrs {:data-root true}
           :target-attrs {:class "target"}}))]
    (is (= "board-updated" (:event fragment)))
    (is (= "innerHTML" (:swap fragment)))
    (is (= "#board-state" (:include fragment)))
    (is (= continuity-config (:client-continuity fragment)))
    (is (= {:class "base"} (:attrs fragment)))
    (is (= {:data-root true} (:root-attrs fragment)))
    (is (= {:class "target"} (:target-attrs fragment)))))

(deftest fragment-subscription-string-generates-stream-url-test
  (is (= "/app/gesso/live/stream?subscription=requests"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            "requests"})))))

(deftest fragment-subscription-keyword-generates-stream-url-test
  (is (= "/app/gesso/live/stream?subscription=requests"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            :requests})))))

(deftest fragment-subscription-symbol-generates-stream-url-test
  (is (= "/app/gesso/live/stream?subscription=requests"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            'requests})))))

(deftest fragment-subscription-map-token-precedence-test
  (let [fragment
        (ui/->fragment
         {:id
          "request-list"

          :src
          "/fragment"

          :subscription
          {:token
           "explicit-token"

           :subscription/token
           "compat-token"

           :id
           "id-token"

           :topic
           :topic-token}})]

    (is (= "explicit-token"
           (:subscription/token
            fragment)))

    (is (= "/app/gesso/live/stream?subscription=explicit-token"
           (:stream-url
            fragment)))))

(deftest fragment-subscription-map-compat-token-test
  (is (= "compat-token"
         (:subscription/token
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            {:subscription/token
             "compat-token"}})))))

(deftest fragment-subscription-map-id-fallback-test
  (is (= "request-7"
         (:subscription/token
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            {:id
             "request-7"}})))))

(deftest fragment-subscription-map-topic-fallback-test
  (is (= "store-board"
         (:subscription/token
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            {:topic
             :store-board}})))))

(deftest fragment-custom-stream-base-url-test
  (is (= "/custom/stream?subscription=requests"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :stream-base-url
            "/custom/stream"

            :subscription
            :requests})))))

(deftest fragment-custom-stream-base-url-with-query-test
  (is (= "/custom/stream?tenant=1&subscription=requests"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :stream-base-url
            "/custom/stream?tenant=1"

            :subscription
            :requests})))))

(deftest fragment-subscription-token-is-url-encoded-test
  (is (= "/app/gesso/live/stream?subscription=request+list%2F1"
         (:stream-url
          (ui/->fragment
           {:id
            "request-list"

            :src
            "/fragment"

            :subscription
            "request list/1"})))))

(deftest explicit-stream-url-wins-over-derived-url-test
  (let [fragment
        (ui/->fragment
         {:id
          "request-list"

          :src
          "/fragment"

          :stream-url
          "/explicit"

          :subscription
          :requests})]

    (is (= "/explicit"
           (:stream-url
            fragment)))

    (is (= "requests"
           (:subscription/token
            fragment)))))

(deftest fragment-requires-id-test
  (doseq [id
          [nil
           ""
           "   "]]

    (let [data
          (thrown-data
           #(ui/->fragment
             {:id
              id

              :src
              "/fragment"

              :stream-url
              "/stream"}))]

      (is (= id
             (:id
              data))))))

(deftest fragment-requires-src-test
  (doseq [src
          [nil
           ""
           "   "]]

    (let [data
          (thrown-data
           #(ui/->fragment
             {:id
              "fragment"

              :src
              src

              :stream-url
              "/stream"}))]

      (is (= src
             (:src
              data))))))

(deftest fragment-requires-stream-or-subscription-test
  (let [error
        (thrown
         #(ui/->fragment
           {:id
            "fragment"

            :src
            "/fragment"}))]

    (is (instance?
         clojure.lang.ExceptionInfo
         error))

    (is (= "gesso.live UI fragment requires :stream-url or :subscription."
           (ex-message
            error)))

    (is (= {:fragment
            {:id
             "fragment"

             :src
             "/fragment"}}
           (ex-data
            error)))))

(deftest fragment-legacy-shape-test
  (let [fragment
        (ui/->fragment
         {:fragment/id "legacy-fragment"
          :fragment/src "/legacy/fragment"
          :fragment/swap "innerHTML"
          :subscription/token "legacy-token"
          :stream/base-url "/legacy/stream"
          :event "legacy-event"
          :include "#legacy-state"
          :client-continuity {:preserve {:inputs true}}
          :attrs {:class "legacy-root"}
          :root-attrs {:data-root "legacy"}
          :inner-attrs {:class "legacy-target"}})]
    (is (= "legacy-fragment" (:id fragment)))
    (is (= "/legacy/fragment" (:src fragment)))
    (is (= "innerHTML" (:swap fragment)))
    (is (= "legacy-token" (:subscription/token fragment)))
    (is (= "/legacy/stream?subscription=legacy-token"
           (:stream-url fragment)))
    (is (= {:class "legacy-target"}
           (:target-attrs fragment)))))

(deftest fragment-legacy-map-is-canonicalized-not-merged-with-modern-keys-test
  (let [fragment
        (ui/->fragment
         {:fragment/id
          "legacy-id"

          :id
          "modern-id"

          :fragment/src
          "/legacy"

          :src
          "/modern"

          :subscription/token
          "legacy-token"

          :stream-url
          "/explicit"})]

    (is (= "legacy-id"
           (:id
            fragment)))

    (is (= "/legacy"
           (:src
            fragment)))

    (is (= "/explicit"
           (:stream-url
            fragment)))))

;; -----------------------------------------------------------------------------
;; Stable fragment root attrs
;; -----------------------------------------------------------------------------

(deftest fragment-root-attrs-default-test
  (let [attrs
        (ui/fragment-root-attrs
         fragment-spec)]
    (is (= "sse" (:hx-ext attrs)))
    (is (= "/app/requests/stream" (:sse-connect attrs)))
    (is (= "request-list" (:data-gesso-live-fragment attrs)))
    (is (= "/app/requests/fragment" (:hx-get attrs)))
    (is (= "#request-list" (:hx-target attrs)))
    (is (= ui/default-fragment-swap (:hx-swap attrs)))
    (is (= "gesso:live-refresh" (:hx-trigger attrs)))
    (is (not (contains? attrs :_)))))

(deftest fragment-root-owns-include-test
  (is (= "#board-state"
         (:hx-include
          (ui/fragment-root-attrs
           (assoc
            fragment-spec
            :include
            "#board-state"))))))

(deftest fragment-root-continuity-metadata-test
  (let [attrs
        (ui/fragment-root-attrs
         (assoc
          fragment-spec
          :client-continuity
          {:preserve
           {:inputs
            true

            :focus
            true}}))]

    (is (= "true"
           (get
            attrs
            continuity/continuity-attr)))

    (is (= "request-list"
           (get
            attrs
            continuity/continuity-fragment-attr)))

    (is (string?
         (get
          attrs
          continuity/continuity-config-attr)))

    (is (str/includes?
         (get
          attrs
          continuity/continuity-config-attr)
         "\"inputs\":true"))))

(deftest fragment-root-disabled-continuity-emits-no-continuity-attrs-test
  (let [attrs
        (ui/fragment-root-attrs
         (assoc
          fragment-spec
          :client-continuity
          false))]

    (doseq [key
            continuity/continuity-attrs]

      (is (not
           (contains?
            attrs
            key))))))

(deftest fragment-root-attrs-then-root-attrs-merge-order-test
  (let [attrs
        (ui/fragment-root-attrs
         (merge
          fragment-spec
          {:attrs {:class "attrs-class"
                   :hx-get "/attrs-get"
                   :data-shared "attrs"}
           :root-attrs {:class "root-class"
                        :data-shared "root"}}))]
    (testing "ordinary root attrs retain caller merge order"
      (is (= "root-class" (:class attrs)))
      (is (= "root" (:data-shared attrs))))
    (testing "managed request ownership wins over caller attrs"
      (is (= "/app/requests/fragment" (:hx-get attrs)))
      (is (= "gesso:live-refresh" (:hx-trigger attrs)))
      (is (= "#request-list" (:hx-target attrs)))
      (is (= ui/default-fragment-swap (:hx-swap attrs))))))

(deftest fragment-root-caller-cannot-override-managed-request-attrs-test
  (let [attrs
        (ui/fragment-root-attrs
         (merge
          fragment-spec
          {:root-attrs
           {:hx-get "/override"
            :hx-trigger "load"
            :hx-target "#other"
            :hx-swap "none"
            :sse-connect "/wrong-stream"}}))]
    (is (= "/app/requests/fragment" (:hx-get attrs)))
    (is (= "gesso:live-refresh" (:hx-trigger attrs)))
    (is (= "#request-list" (:hx-target attrs)))
    (is (= ui/default-fragment-swap (:hx-swap attrs)))
    (is (= "/app/requests/stream" (:sse-connect attrs)))))

(deftest fragment-root-composes-extra-hx-extension-test
  (let [attrs
        (ui/fragment-root-attrs
         (assoc
          fragment-spec
          :attrs
          {:hx-ext
           "path-deps"}))]

    (is (= "sse, path-deps"
           (:hx-ext
            attrs)))))

(deftest fragment-custom-event-is-owned-by-invalidation-listener-test
  (let [fragment
        (assoc fragment-spec :event "request-updated")
        root-attrs
        (ui/fragment-root-attrs fragment)
        listener-attrs
        (ui/fragment-invalidation-listener-attrs fragment)]
    (is (= "gesso:live-refresh" (:hx-trigger root-attrs)))
    (is (= "request-updated" (:sse-swap listener-attrs)))
    (is (= "request-list"
           (:data-gesso-live-invalidation listener-attrs)))
    (is (= "none" (:hx-swap listener-attrs)))))

(deftest fragment-direct-trigger-is-rejected-test
  (let [error
        (thrown
         #(ui/fragment-root-attrs
           (assoc fragment-spec
                  :trigger "load, custom-event from:body")))]
    (is (instance? clojure.lang.ExceptionInfo error))
    (is (= {:trigger "load, custom-event from:body"}
           (:unsupported-options (ex-data error))))))

(deftest fragment-direct-jitter-is-rejected-test
  (doseq [opts [{:jitter-ms 250}
                {:jitter-delay-ms 250}
                {:jitter-ms 250
                 :jitter-delay-ms 100}]]
    (let [error
          (thrown
           #(ui/fragment-root-attrs
             (merge fragment-spec opts)))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (= opts
             (:unsupported-options (ex-data error)))))))

;; -----------------------------------------------------------------------------
;; Replaceable target attrs
;; -----------------------------------------------------------------------------

(deftest fragment-target-attrs-default-test
  (is (= {:id
          "request-list"}
         (ui/fragment-target-attrs
          fragment-spec))))

(deftest fragment-target-attrs-custom-test
  (is (= {:id
          "request-list"

          :class
          "target"

          :aria-live
          "polite"

          :data-target
          true}
         (ui/fragment-target-attrs
          (assoc
           fragment-spec
           :target-attrs
           {:class
            "target"

            :aria-live
            "polite"

            :data-target
            true})))))

(deftest fragment-target-id-is-framework-owned-test
  (let [attrs
        (ui/fragment-target-attrs
         (assoc fragment-spec
                :target-attrs
                {:id "other-id"
                 :class "target"}))]
    (is (= "request-list" (:id attrs)))
    (is (= "target" (:class attrs)))))

(deftest fragment-target-attrs-clean-nil-values-test
  (let [attrs
        (ui/fragment-target-attrs
         (assoc
          fragment-spec
          :target-attrs
          {:class
           nil

           :data-false
           false

           :data-zero
           0}))]

    (is (not
         (contains?
          attrs
          :class)))

    (is (= false
           (:data-false
            attrs)))

    (is (= 0
           (:data-zero
            attrs)))))

(deftest fragment-target-does-not-inherit-root-behavior-test
  (let [attrs
        (ui/fragment-target-attrs
         fragment-spec)]

    (doseq [key
            [:hx-ext
             :sse-connect
             :hx-get
             :hx-trigger
             :hx-target
             :hx-swap
             :hx-include
             continuity/continuity-attr
             continuity/continuity-config-attr
             continuity/continuity-fragment-attr]]

      (is (not
           (contains?
            attrs
            key))))))

;; -----------------------------------------------------------------------------
;; Fragment panel
;; -----------------------------------------------------------------------------

(deftest fragment-panel-shape-test
  (let [panel
        (ui/fragment-panel
         fragment-spec)]
    (is (= :div (first panel)))
    (is (= "request-list"
           (get-in panel [1 :data-gesso-live-fragment])))
    (is (= :div (get-in panel [2 0])))
    (is (= "request-list"
           (get-in panel [2 1 :data-gesso-live-invalidation])))
    (is (= "live-update"
           (get-in panel [2 1 :sse-swap])))
    (is (= :div (get-in panel [3 0])))
    (is (= "request-list"
           (get-in panel [3 1 :id])))))

(deftest fragment-panel-stable-root-owns-behavior-test
  (let [panel
        (ui/fragment-panel
         fragment-spec)
        root-attrs (second panel)
        listener-attrs (get-in panel [2 1])
        target-attrs (get-in panel [3 1])]
    (doseq [key [:hx-ext
                 :sse-connect
                 :hx-get
                 :hx-trigger
                 :hx-target
                 :hx-swap]]
      (is (contains? root-attrs key)))

    (testing "the invalidation listener owns only advisory SSE observation"
      (is (= "live-update" (:sse-swap listener-attrs)))
      (is (= "none" (:hx-swap listener-attrs)))
      (is (not (contains? listener-attrs :sse-connect)))
      (is (not (contains? listener-attrs :hx-get)))
      (is (not (contains? listener-attrs :hx-trigger))))

    (testing "the replaceable target owns no request or SSE behavior"
      (doseq [key [:hx-ext
                   :sse-connect
                   :sse-swap
                   :hx-get
                   :hx-trigger
                   :hx-target
                   :hx-swap]]
        (is (not (contains? target-attrs key)))))))

(deftest fragment-panel-accepts-prepared-descriptor-test
  (let [fragment
        (ui/->fragment
         fragment-spec)]

    (is (= (ui/fragment-panel
            fragment-spec)
           (ui/fragment-panel
            fragment)))))

;; -----------------------------------------------------------------------------
;; Browser runtime script
;; -----------------------------------------------------------------------------

(deftest live-script-default-test
  (is (= [:script
          {:src
           "/gesso/gesso-live.js"

           :defer
           true}]
         (ui/live-script))))

(deftest live-script-nil-options-test
  (is (= (ui/live-script)
         (ui/live-script
          nil))))

(deftest live-script-custom-src-test
  (is (= "/assets/gesso-live.123.js"
         (get-in
          (ui/live-script
           {:src
            "/assets/gesso-live.123.js"})
          [1
           :src]))))

(deftest live-script-extra-attrs-merge-last-test
  (let [attrs
        (second
         (ui/live-script
          {:src
           "/runtime.js"

           :attrs
           {:defer
            false

            :nonce
            "nonce-1"

            :integrity
            "hash"}}))]

    (is (= "/runtime.js"
           (:src
            attrs)))

    (is (= false
           (:defer
            attrs)))

    (is (= "nonce-1"
           (:nonce
            attrs)))

    (is (= "hash"
           (:integrity
            attrs)))))

(deftest live-script-cleans-nil-attrs-test
  (let [attrs
        (second
         (ui/live-script
          {:attrs
           {:nonce
            nil

            :defer
            false}}))]

    (is (not
         (contains?
          attrs
          :nonce)))

    (is (= false
           (:defer
            attrs)))))

;; -----------------------------------------------------------------------------
;; Anti-forgery
;; -----------------------------------------------------------------------------

(deftest anti-forgery-token-precedence-test
  (is (= "primary"
         (ui/anti-forgery-token
          {:anti-forgery-token
           "primary"

           :biff/anti-forgery-token
           "biff"

           :session
           {:anti-forgery-token
            "session"}})))

  (is (= "biff"
         (ui/anti-forgery-token
          {:biff/anti-forgery-token
           "biff"

           :session
           {:anti-forgery-token
            "session"}})))

  (is (= "session"
         (ui/anti-forgery-token
          {:session
           {:anti-forgery-token
            "session"}}))))

(deftest anti-forgery-token-missing-test
  (is (nil?
       (ui/anti-forgery-token
        {})))

  (is (nil?
       (ui/anti-forgery-token
        nil))))

(deftest anti-forgery-token-preserves-false-test
  (is (= "fallback"
         (ui/anti-forgery-token
          {:anti-forgery-token
           false

           :biff/anti-forgery-token
           "fallback"}))))

(deftest anti-forgery-input-test
  (is (= [:input
          {:type
           "hidden"

           :name
           "__anti-forgery-token"

           :value
           "token-primary"}]
         (ui/anti-forgery-input
          ctx))))

(deftest anti-forgery-input-missing-test
  (is (nil?
       (ui/anti-forgery-input
        {}))))

;; -----------------------------------------------------------------------------
;; POST form attrs
;; -----------------------------------------------------------------------------

(deftest post-form-attrs-default-test
  (is (= {:method
          "post"

          :hx-post
          "/claim"

          :hx-swap
          "innerHTML"

          :hx-target
          "#request-list"

          :hx-sync
          ui/default-post-sync}
         (ui/post-form-attrs
          {:to
           "/claim"

           :target
           "request-list"}))))

(deftest post-form-attrs-fragment-target-test
  (let [fragment
        (ui/->fragment
         fragment-spec)]

    (is (= "#request-list"
           (:hx-target
            (ui/post-form-attrs
             {:to
              "/claim"

              :target
              fragment}))))))

(deftest post-form-attrs-normalizes-explicit-selector-target-test
  (is (= "closest [data-request-card]"
         (:hx-target
          (ui/post-form-attrs
           {:to
            "/claim"

            :target
            "closest [data-request-card]"})))))

(deftest post-form-attrs-default-omits-native-action-test
  (let [attrs
        (ui/post-form-attrs
         {:to
          "/claim"})]

    (is (not
         (contains?
          attrs
          :action)))

    (is (= "/claim"
           (:hx-post
            attrs)))))

(deftest post-form-attrs-native-action-test
  (let [attrs
        (ui/post-form-attrs
         {:to
          "/claim"

          :native-action?
          true})]

    (is (= "/claim"
           (:action
            attrs)))

    (is (= "/claim"
           (:hx-post
            attrs)))))

(deftest post-form-attrs-custom-swap-test
  (is (= "none"
         (:hx-swap
          (ui/post-form-attrs
           {:to
            "/claim"

            :swap
            "none"})))))

(deftest post-form-attrs-custom-sync-test
  (is (= "this:abort"
         (:hx-sync
          (ui/post-form-attrs
           {:to
            "/claim"

            :sync
            "this:abort"})))))

(deftest post-form-attrs-nil-sync-uses-default-and-false-disables-test
  (testing "explicit nil delegates without :sync, so the lower HTMX default applies"
    (is (= htmx/default-post-sync
           (:hx-sync
            (ui/post-form-attrs
             {:to
              "/claim"

              :sync
              nil})))))

  (testing "false explicitly disables hx-sync"
    (is (nil?
         (:hx-sync
          (ui/post-form-attrs
           {:to
            "/claim"

            :sync
            false}))))))

(deftest post-form-attrs-attrs-merge-last-test
  (let [attrs
        (ui/post-form-attrs
         {:to
          "/real"

          :target
          "request-list"

          :attrs
          {:hx-post
           "/override"

           :hx-target
           "#override"

           :class
           "form"}})]

    (is (= "/override"
           (:hx-post
            attrs)))

    (is (= "#override"
           (:hx-target
            attrs)))

    (is (= "form"
           (:class
            attrs)))))

(deftest post-form-attrs-requires-to-test
  (doseq [to
          [nil
           ""
           "   "]]

    (let [data
          (thrown-data
           #(ui/post-form-attrs
             {:to
              to}))]

      (is (= to
             (:to
              data))))))

;; -----------------------------------------------------------------------------
;; POST form markup
;; -----------------------------------------------------------------------------

(deftest post-form-markup-test
  (let [markup
        (ui/post-form
         ctx
         {:to
          "/claim"

          :target
          "request-list"}

         [:input
          {:name
           "note"}]

         [:button
          {:type
           "submit"}
          "Save"])]

    (is (= :form
           (first
            markup)))

    (is (= "post"
           (get-in
            markup
            [1
             :method])))

    (is (= "/claim"
           (get-in
            markup
            [1
             :hx-post])))

    (is (= [:input
            {:type
             "hidden"

             :name
             "__anti-forgery-token"

             :value
             "token-primary"}]
           (nth
            markup
            2)))

    (is (= [:input
            {:name
             "note"}]
           (nth
            markup
            3)))

    (is (= [:button
            {:type
             "submit"}
            "Save"]
           (nth
            markup
            4)))))

(deftest post-form-without-token-has-no-placeholder-child-test
  (is (= [:form
          {:method
           "post"

           :hx-post
           "/claim"

           :hx-swap
           "innerHTML"

           :hx-sync
           ui/default-post-sync}
          [:span
           "child"]]
         (ui/post-form
          {}
          {:to
           "/claim"}
          [:span
           "child"]))))

(deftest post-form-preserves-arbitrary-children-test
  (let [child-a
        [:input
         {:name
          "a"}]

        child-b
        "plain text"

        markup
        (ui/post-form
         {}
         {:to
          "/save"}
         child-a
         child-b)]

    (is (= [child-a
            child-b]
           (vec
            (drop
             2
             markup))))))

;; -----------------------------------------------------------------------------
;; Ordinary POST button
;; -----------------------------------------------------------------------------

(deftest post-button-default-markup-test
  (let [markup
        (ui/post-button
         ctx
         {:to
          "/claim"

          :target
          "request-list"

          :label
          "Claim"})

        form
        (form-node
         markup)

        button
        (button-node
         markup)]

    (is (= :form
           (first
            markup)))

    (is (= true
           (get-in
            form
            [1
             :data-gesso-live-post])))

    (is (= [:input
            {:type
             "hidden"

             :name
             "__anti-forgery-token"

             :value
             "token-primary"}]
           (hidden-anti-forgery
            markup)))

    (is (= :button
           (first
            button)))

    (is (= "button"
           (get-in
            button
            [1
             :type])))

    (is (= "/claim"
           (get-in
            button
            [1
             :hx-post])))

    (is (= "#request-list"
           (get-in
            button
            [1
             :hx-target])))

    (is (= ui/default-post-swap
           (get-in
            button
            [1
             :hx-swap])))

    (is (= ui/default-post-sync
           (get-in
            button
            [1
             :hx-sync])))

    (is (= ui/default-post-include
           (get-in
            button
            [1
             :hx-include])))

    (is (= "Claim"
           (nth
            button
            2)))

    (is (nil?
         (template-node
          markup)))))

(deftest post-button-without-token-test
  (let [markup
        (ui/post-button
         {}
         {:to
          "/claim"

          :label
          "Claim"})]

    (is (nil?
         (hidden-anti-forgery
          markup)))

    (is (= 1
           (count
            (filter
             vector?
             (children
              markup)))))))

(deftest post-button-default-has-no-native-action-test
  (let [attrs
        (second
         (form-node
          (ui/post-button
           ctx
           {:to
            "/claim"

            :label
            "Claim"})))]

    (is (not
         (contains?
          attrs
          :action)))

    (is (not
         (contains?
          attrs
          :method)))

    (is (not
         (contains?
          attrs
          :hx-post)))))

(deftest post-button-form-marker-is-framework-owned-test
  (let [markup
        (ui/post-button
         ctx
         {:to
          "/claim"

          :label
          "Claim"

          :form-attrs
          {:data-gesso-live-post
           false

           :class
           "wrapper"}})]

    (is (= true
           (get-in
            markup
            [1
             :data-gesso-live-post])))

    (is (= "wrapper"
           (get-in
            markup
            [1
             :class])))))

(deftest post-button-form-attrs-remain-on-wrapper-only-test
  (let [markup
        (ui/post-button
         ctx
         {:to
          "/claim"

          :label
          "Claim"

          :form-attrs
          {:class
           "wrapper"

           :data-form-only
           true}})

        button-attrs
        (second
         (button-node
          markup))]

    (is (= "wrapper"
           (get-in
            markup
            [1
             :class])))

    (is (= true
           (get-in
            markup
            [1
             :data-form-only])))

    (is (not
         (contains?
          button-attrs
          :data-form-only)))))

(deftest post-button-button-attrs-merge-last-test
  (let [button-attrs
        (second
         (button-node
          (ui/post-button
           ctx
           {:to
            "/real"

            :target
            "request-list"

            :label
            "Claim"

            :button-attrs
            {:class
             "primary"

             :hx-post
             "/override"

             :hx-target
             "#override"

             :hx-swap
             "none"

             :hx-sync
             "this:abort"

             :hx-include
             "#override-include"

             :type
             "submit"}})))]

    (is (= "primary"
           (:class
            button-attrs)))

    (is (= "/override"
           (:hx-post
            button-attrs)))

    (is (= "#override"
           (:hx-target
            button-attrs)))

    (is (= "none"
           (:hx-swap
            button-attrs)))

    (is (= "this:abort"
           (:hx-sync
            button-attrs)))

    (is (= "#override-include"
           (:hx-include
            button-attrs)))

    (is (= "submit"
           (:type
            button-attrs)))))

(deftest post-button-children-default-to-label-test
  (is (= ["Claim"]
         (vec
          (drop
           2
           (button-node
            (ui/post-button
             ctx
             {:to
              "/claim"

              :label
              "Claim"})))))))

(deftest post-button-sequential-children-replace-label-test
  (is (= [[:span
           "Icon"]

          [:span
           "Claim"]]
         (vec
          (drop
           2
           (button-node
            (ui/post-button
             ctx
             {:to
              "/claim"

              :label
              "Ignored"

              :children
              [[:span
                "Icon"]

               [:span
                "Claim"]]})))))))

(deftest post-button-single-nonsequential-child-test
  (is (= ["Only child"]
         (vec
          (drop
           2
           (button-node
            (ui/post-button
             ctx
             {:to
              "/claim"

              :children
              "Only child"})))))))

(deftest post-button-nil-label-is-preserved-as-child-test
  (is (= [nil]
         (vec
          (drop
           2
           (button-node
            (ui/post-button
             ctx
             {:to
              "/claim"})))))))

(deftest post-button-include-one-extra-selector-test
  (is (= "closest [data-gesso-live-post], #board-state"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :include
             "#board-state"}))
          [1
           :hx-include]))))

(deftest post-button-include-sequence-is-flattened-and-deduplicated-test
  (is (= "closest [data-gesso-live-post], #a, #b, #c"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :include
             ["#a"
              ["#b"
               "#a"]
              ["#c"]]}))
          [1
           :hx-include]))))

(deftest post-button-include-false-means-wrapper-only-test
  (is (= ui/default-post-include
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :include
             false}))
          [1
           :hx-include]))))

(deftest post-button-include-validation-test
  (doseq [include
          [""
           "   "
           :not-a-selector
           42
           {:selector
            "#x"}]]

    (is (instance?
         clojure.lang.ExceptionInfo
         (thrown
          #(ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :include
             include}))))))

(deftest post-button-custom-swap-test
  (is (= "none"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :swap
             "none"}))
          [1
           :hx-swap]))))

(deftest post-button-explicit-nil-swap-removes-swap-attr-test
  (is (nil?
       (get-in
        (button-node
         (ui/post-button
          ctx
          {:to
           "/claim"

           :label
           "Claim"

           :swap
           nil}))
        [1
         :hx-swap]))))

(deftest post-button-custom-sync-test
  (is (= "this:queue last"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :sync
             "this:queue last"}))
          [1
           :hx-sync]))))

(deftest post-button-nil-or-false-sync-disables-sync-test
  (doseq [sync
          [nil
           false]]

    (is (nil?
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :label
             "Claim"

             :sync
             sync}))
          [1
           :hx-sync])))))

(deftest post-button-target-fragment-id-test
  (is (= "#request-list"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :target
             "request-list"

             :label
             "Claim"}))
          [1
           :hx-target]))))

(deftest post-button-target-extended-selector-test
  (is (= "closest [data-request-card]"
         (get-in
          (button-node
           (ui/post-button
            ctx
            {:to
             "/claim"

             :target
             "closest [data-request-card]"

             :label
             "Claim"}))
          [1
           :hx-target]))))

(deftest post-button-missing-target-omits-hx-target-test
  (is (nil?
       (get-in
        (button-node
         (ui/post-button
          ctx
          {:to
           "/claim"

           :label
           "Claim"}))
        [1
         :hx-target]))))

(deftest post-button-requires-to-test
  (doseq [to
          [nil
           ""
           "   "]]

    (let [data
          (thrown-data
           #(ui/post-button
             ctx
             {:to
              to

              :label
              "Claim"}))]

      (is (= to
             (:to
              data))))))

(deftest post-button-two-arity-and-three-arity-equivalence-with-explicit-target-test
  (let [fragment
        (ui/->fragment
         fragment-spec)

        options
        {:to
         "/claim"

         :target
         "request-list"

         :swap
         "outerHTML"

         :label
         "Claim"}]

    (is (= (ui/post-button
            ctx
            options)
           (ui/post-button
            ctx
            fragment
            options)))))

(deftest post-button-three-arity-inherits-fragment-target-and-swap-test
  (let [fragment
        (ui/->fragment
         (assoc
          fragment-spec
          :swap
          "outerHTML"))

        button
        (button-node
         (ui/post-button
          ctx
          fragment
          {:to
           "/claim"

           :label
           "Claim"}))]

    (is (= "#request-list"
           (get-in
            button
            [1
             :hx-target])))

    (is (= "outerHTML"
           (get-in
            button
            [1
             :hx-swap])))))

(deftest post-button-three-arity-explicit-options-win-test
  (let [fragment
        (ui/->fragment
         (assoc
          fragment-spec
          :swap
          "outerHTML"))

        button
        (button-node
         (ui/post-button
          ctx
          fragment
          {:to
           "/claim"

           :target
           "other"

           :swap
           "none"

           :label
           "Claim"}))]

    (is (= "#other"
           (get-in
            button
            [1
             :hx-target])))

    (is (= "none"
           (get-in
            button
            [1
             :hx-swap])))))

;; -----------------------------------------------------------------------------
;; Protocol-v3 optimistic UI delegation — general integration contract
;;
;; Detailed validation and settlement-marker coverage belongs in
;; gesso.live.ui-optimistic-test. This suite keeps only the post-button
;; composition expectations that interact with the rest of gesso.live.ui.
;; -----------------------------------------------------------------------------

(def optimistic-action
  {:operation :request/claim
   :arguments {:request-id "request-1"}
   :observed-basis {:tx-id 42
                    :system-time "2026-08-25T01:00:00Z"}
   :scope [:request "request-1"]
   :target-id "request-card-request-1"
   :rollback-eligible? true
   :timeout-ms 5000})

(defn- decoded-optimistic-action
  [markup]
  (some-> (button-node markup)
          second
          (get ui/optimistic-action-attr)
          edn/read-string))

(deftest optimistic-post-button-binds-v3-action-to-clicked-button-test
  (let [markup
        (ui/post-button
         ctx
         {:to "/claim"
          :swap "none"
          :label "Claim"
          :optimistic optimistic-action})
        button-attrs
        (second (button-node markup))
        form-attrs
        (second (form-node markup))]
    (is (= optimistic-action
           (decoded-optimistic-action markup)))
    (is (contains? button-attrs
                   ui/optimistic-action-attr))
    (is (not (contains? form-attrs
                        ui/optimistic-action-attr)))
    (is (nil? (template-node markup)))))

(deftest optimistic-post-button-preserves-ordinary-request-mechanics-test
  (let [button-attrs
        (second
         (button-node
          (ui/post-button
           ctx
           {:to "/claim"
            :target "request-list"
            :swap "none"
            :include "#board-state"
            :sync "this:abort"
            :label "Claim"
            :optimistic optimistic-action})))]
    (is (= "/claim" (:hx-post button-attrs)))
    (is (= "#request-list" (:hx-target button-attrs)))
    (is (= "none" (:hx-swap button-attrs)))
    (is (= "closest [data-gesso-live-post], #board-state"
           (:hx-include button-attrs)))
    (is (= "this:abort" (:hx-sync button-attrs)))
    (is (= optimistic-action
           (-> button-attrs
               (get ui/optimistic-action-attr)
               edn/read-string)))))

(deftest optimistic-post-button-three-arity-keeps-fragment-target-authoritative-test
  (let [fragment
        (ui/->fragment
         (assoc fragment-spec
                :swap "outerHTML"))
        button-attrs
        (second
         (button-node
          (ui/post-button
           ctx
           fragment
           {:to "/claim"
            :label "Claim"
            :optimistic
            (dissoc optimistic-action :target-id)})))]
    (is (= "#request-list" (:hx-target button-attrs)))
    (is (= "outerHTML" (:hx-swap button-attrs)))
    (is (= (dissoc optimistic-action :target-id)
           (-> button-attrs
               (get ui/optimistic-action-attr)
               edn/read-string)))))

(deftest optimistic-post-button-nil-and-false-remain-ordinary-test
  (doseq [value [nil false]]
    (let [markup
          (ui/post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :optimistic value})
          button-attrs
          (second (button-node markup))]
      (is (not (contains? button-attrs
                          ui/optimistic-action-attr)))
      (is (nil? (template-node markup))))))

(deftest optimistic-post-button-protects-framework-action-annotation-test
  (let [button-attrs
        (second
         (button-node
          (ui/post-button
           ctx
           {:to "/claim"
            :label "Claim"
            :button-attrs
            {ui/optimistic-action-attr
             "{:operation :attacker/forged}"
             :data-app-owned "kept"}
            :optimistic optimistic-action})))]
    (is (= "kept" (:data-app-owned button-attrs)))
    (is (= optimistic-action
           (-> button-attrs
               (get ui/optimistic-action-attr)
               edn/read-string)))))

;; -----------------------------------------------------------------------------
;; Composition invariants
;; -----------------------------------------------------------------------------

(deftest stable-fragment-root-and-post-button-compose-without-shared-request-owner-test
  (let [fragment
        (ui/->fragment
         fragment-spec)

        panel
        (ui/fragment-panel
         fragment)

        button
        (ui/post-button
         ctx
         fragment
         {:to
          "/claim"

          :label
          "Claim"})]

    (testing "fragment root owns live refresh behavior"
      (is (= "/app/requests/fragment"
             (get-in
              panel
              [1
               :hx-get])))

      (is (= "#request-list"
             (get-in
              panel
              [1
               :hx-target]))))

    (testing "clicked button independently owns the mutation request"
      (is (= "/claim"
             (get-in
              (button-node
               button)
              [1
               :hx-post])))

      (is (= "#request-list"
             (get-in
              (button-node
               button)
              [1
               :hx-target]))))

    (testing "lightweight wrapper form owns neither live refresh nor mutation request"
      (let [attrs
            (second
             (form-node
              button))]

        (is (= true
               (:data-gesso-live-post
                attrs)))

        (is (nil?
             (:hx-get
              attrs)))

        (is (nil?
             (:hx-post
              attrs)))))))

(deftest fragment-panel-keeps-sse-behavior-off-replaceable-target-test
  (let [panel
        (ui/fragment-panel
         (assoc
          fragment-spec
          :client-continuity
          {:preserve
           {:inputs true}}))

        root-attrs
        (second
         panel)

        target-attrs
        (get-in
         panel
         [2
          1])]

    (is (= "sse"
           (:hx-ext
            root-attrs)))

    (is (= "/app/requests/stream"
           (:sse-connect
            root-attrs)))

    (is (= "true"
           (get
            root-attrs
            continuity/continuity-attr)))

    (doseq [key
            [:hx-ext
             :sse-connect
             :hx-trigger
             continuity/continuity-attr
             continuity/continuity-config-attr
             continuity/continuity-fragment-attr]]

      (is (not
           (contains?
            target-attrs
            key))))))

(deftest post-button-progressive-safety-contract-test
  (let [markup
        (ui/post-button
         ctx
         {:to
          "/fragment-only-mutation"

          :label
          "Do it"})

        wrapper-attrs
        (second
         markup)

        button-attrs
        (second
         (button-node
          markup))]

    (testing "wrapper cannot natively submit to the mutation route"
      (is (nil?
           (:action
            wrapper-attrs)))

      (is (nil?
           (:method
            wrapper-attrs)))

      (is (nil?
           (:hx-post
            wrapper-attrs))))

    (testing "only the explicit button owns HTMX mutation behavior"
      (is (= "button"
             (:type
              button-attrs)))

      (is (= "/fragment-only-mutation"
             (:hx-post
              button-attrs))))))

;; -----------------------------------------------------------------------------
;; Error surfaces remain simple and local
;; -----------------------------------------------------------------------------

(deftest fragment-error-data-is-input-focused-test
  (is (= {:id
          nil}
         (thrown-data
          #(ui/->fragment
            {:src
             "/fragment"

             :stream-url
             "/stream"}))))

  (is (= {:src
          nil}
         (thrown-data
          #(ui/->fragment
            {:id
             "fragment"

             :stream-url
             "/stream"})))))

(deftest post-form-error-data-is-input-focused-test
  (is (= {:to
          nil}
         (thrown-data
          #(ui/post-form-attrs
            {})))))

(deftest post-button-error-data-is-input-focused-test
  (is (= {:to
          nil}
         (thrown-data
          #(ui/post-button
            ctx
            {:label
             "Missing route"})))))

;; -----------------------------------------------------------------------------
;; Determinism
;; -----------------------------------------------------------------------------

(deftest fragment-construction-is-deterministic-test
  (is (= (ui/->fragment
          fragment-spec)
         (ui/->fragment
          fragment-spec))))

(deftest fragment-markup-is-deterministic-without-random-jitter-test
  (is (= (ui/fragment-panel
          fragment-spec)
         (ui/fragment-panel
          fragment-spec))))

(deftest live-script-is-deterministic-test
  (is (= (ui/live-script)
         (ui/live-script))))

(deftest post-form-is-deterministic-test
  (is (= (ui/post-form
          ctx
          {:to
           "/claim"

           :target
           "request-list"}
          [:span
           "child"])
         (ui/post-form
          ctx
          {:to
           "/claim"

           :target
           "request-list"}
          [:span
           "child"]))))

(deftest ordinary-post-button-is-deterministic-test
  (is (= (ui/post-button
          ctx
          {:to
           "/claim"

           :target
           "request-list"

           :label
           "Claim"})
         (ui/post-button
          ctx
          {:to
           "/claim"

           :target
           "request-list"

           :label
           "Claim"}))))

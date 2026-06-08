(ns gesso.live.htmx-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [gesso.live.htmx :as h]))

;; -----------------------------------------------------------------------------
;; Shared expected values
;; -----------------------------------------------------------------------------

(def default-fragment-trigger
  "load, pageshow from:window, focus from:window, visibilitychange from:document, online from:window, htmx:sseOpen from:body, gesso:live-connected from:body, sse:live-update")

(def default-live-connected-script
  "on 'htmx:sseOpen' send 'gesso:live-connected' to body")

;; -----------------------------------------------------------------------------
;; Small attr helpers
;; -----------------------------------------------------------------------------

(deftest clean-attrs-test
  (testing "clean-attrs removes nil values but preserves falsey/useful values"
    (is (= {:b false
            :c ""
            :d 0
            :e []}
           (h/clean-attrs
            {:a nil
             :b false
             :c ""
             :d 0
             :e []})))))

(deftest hx-ext-value-test
  (testing "hx-ext-value composes extension names"
    (is (= "sse"
           (h/hx-ext-value "sse")))

    (is (= "sse, path-deps"
           (h/hx-ext-value "sse" "path-deps")))

    (is (= "sse, path-deps, debug"
           (h/hx-ext-value "sse,path-deps" :debug))))

  (testing "hx-ext-value removes duplicates while preserving first occurrence"
    (is (= "sse, path-deps, debug"
           (h/hx-ext-value "sse" "path-deps" "sse" "debug" :path-deps))))

  (testing "hx-ext-value accepts sequential extension collections"
    (is (= "sse, path-deps, debug"
           (h/hx-ext-value ["sse" "path-deps"] [:debug]))))

  (testing "hx-ext-value ignores nil and blank extension values"
    (is (= "sse"
           (h/hx-ext-value nil "" "   " "sse")))

    (is (nil?
         (h/hx-ext-value nil "" "   ")))))

;; -----------------------------------------------------------------------------
;; Shared normalization
;; -----------------------------------------------------------------------------

(deftest token-header-name-test
  (testing "token-header-name returns the canonical consistency header"
    (is (= "x-gesso-live-consistency-token"
           (h/token-header-name)))))

(deftest event-name-test
  (testing "event-name normalizes app-facing event refs"
    (is (= "live-update" (h/event-name nil)))
    (is (= "live-update" (h/event-name "")))
    (is (= "live-update" (h/event-name "   ")))
    (is (= "live-update" (h/event-name :live-update)))
    (is (= "client-oob" (h/event-name 'client-oob)))
    (is (= "custom-event" (h/event-name "custom-event")))))

(deftest normalize-target-test
  (testing "normalize-target leaves nil alone"
    (is (nil? (h/normalize-target nil))))

  (testing "normalize-target leaves selector-like values unchanged"
    (is (= "this" (h/normalize-target "this")))
    (is (= "#fragment" (h/normalize-target "#fragment")))
    (is (= ".fragment" (h/normalize-target ".fragment")))
    (is (= "closest form" (h/normalize-target "closest form")))
    (is (= "find input" (h/normalize-target "find input")))
    (is (= "next .panel" (h/normalize-target "next .panel")))
    (is (= "previous .panel" (h/normalize-target "previous .panel")))
    (is (= "[data-demo] .target" (h/normalize-target "[data-demo] .target"))))

  (testing "normalize-target turns bare ids into CSS id selectors"
    (is (= "#fragment" (h/normalize-target "fragment")))
    (is (= "#123" (h/normalize-target 123)))))

(deftest sse-trigger-test
  (testing "sse-trigger prefixes normalized event names"
    (is (= "sse:live-update" (h/sse-trigger nil)))
    (is (= "sse:live-update" (h/sse-trigger :live-update)))
    (is (= "sse:client-oob" (h/sse-trigger "client-oob")))))

;; -----------------------------------------------------------------------------
;; Jitter helpers
;; -----------------------------------------------------------------------------

(deftest jitter-helper-test
  (testing "delay-modifier renders HTMX delay modifiers only for positive ints"
    (is (= " delay:250ms" (h/delay-modifier 250)))
    (is (nil? (h/delay-modifier 0)))
    (is (nil? (h/delay-modifier -1)))
    (is (nil? (h/delay-modifier nil)))
    (is (nil? (h/delay-modifier "250"))))

  (testing "trigger-with-delay attaches a delay modifier when present"
    (is (= "sse:live-update delay:250ms"
           (h/trigger-with-delay "sse:live-update" 250)))

    (is (= "sse:live-update"
           (h/trigger-with-delay "sse:live-update" 0))))

  (testing "effective-jitter-delay-ms prefers deterministic caller delay"
    (is (= 37
           (h/effective-jitter-delay-ms
            {:jitter-ms 500
             :jitter-delay-ms 37}))))

  (testing "effective-jitter-delay-ms preserves explicit zero delay"
    (is (= 0
           (h/effective-jitter-delay-ms
            {:jitter-ms 500
             :jitter-delay-ms 0}))))

  (testing "random-jitter-delay-ms returns nil for missing/non-positive values"
    (is (nil? (h/random-jitter-delay-ms nil)))
    (is (nil? (h/random-jitter-delay-ms 0)))
    (is (nil? (h/random-jitter-delay-ms -1))))

  (testing "random-jitter-delay-ms returns a value inside the configured range"
    (let [values (repeatedly 100 #(h/random-jitter-delay-ms 5))]
      (is (every? int? values))
      (is (every? #(<= 0 % 5) values)))))

;; -----------------------------------------------------------------------------
;; Fragment attrs
;; -----------------------------------------------------------------------------

(deftest fragment-root-attrs-test
  (testing "fragment-root-attrs builds the SSE connection wrapper attrs and emits a reconnect signal"
    (is (= {:hx-ext "sse"
            :sse-connect "/app/live/stream"
            :_ default-live-connected-script}
           (h/fragment-root-attrs
            {:stream-url "/app/live/stream"}))))

  (testing "fragment-root-attrs composes hx-ext instead of overwriting SSE"
    (is (= {:hx-ext "sse, path-deps"
            :sse-connect "/app/live/stream"
            :class "live-root"
            :_ default-live-connected-script}
           (h/fragment-root-attrs
            {:stream-url "/app/live/stream"
             :attrs {:hx-ext "path-deps"
                     :class "live-root"}}))))

  (testing "fragment-root-attrs rejects missing or blank stream URLs"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE stream URL is required"
         (h/fragment-root-attrs {})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE stream URL is required"
         (h/fragment-root-attrs {:stream-url ""})))))

(deftest fragment-trigger-test
  (testing "fragment-trigger builds load/recovery triggers plus the SSE trigger by default"
    (is (= default-fragment-trigger
           (h/fragment-trigger {})))

    (is (= "revealed, sse:store-update"
           (h/fragment-trigger
            {:trigger "revealed"
             :event :store-update}))))

  (testing "fragment-trigger applies delay only to the SSE trigger"
    (is (= (str default-fragment-trigger " delay:250ms")
           (h/fragment-trigger
            {:event :live-update
             :jitter-delay-ms 250})))))

(deftest fragment-target-attrs-test
  (testing "fragment-target-attrs builds the live fragment target attrs"
    (is (= {:id "store-queue"
            :hx-get "/app/store/queue"
            :hx-trigger default-fragment-trigger
            :hx-swap "outerHTML"}
           (h/fragment-target-attrs
            {:id "store-queue"
             :src "/app/store/queue"}))))

  (testing "fragment-target-attrs supports custom event, swap, trigger, jitter, and attrs"
    (is (= {:id "store-queue"
            :hx-get "/app/store/queue"
            :hx-trigger "revealed, sse:store-update delay:125ms"
            :hx-swap "innerHTML"
            :class "target"}
           (h/fragment-target-attrs
            {:id "store-queue"
             :src "/app/store/queue"
             :event :store-update
             :swap "innerHTML"
             :trigger "revealed"
             :jitter-delay-ms 125
             :attrs {:class "target"}}))))

  (testing "fragment-target-attrs rejects missing required values"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Fragment id is required"
         (h/fragment-target-attrs
          {:src "/fragment"})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Fragment source URL is required"
         (h/fragment-target-attrs
          {:id "fragment"})))))

;; -----------------------------------------------------------------------------
;; POST attrs
;; -----------------------------------------------------------------------------

(deftest post-form-attrs-test
  (testing "post-form-attrs builds HTMX POST attrs without native action by default"
    (is (= {:method "post"
            :hx-post "/save"
            :hx-swap "innerHTML"
            :hx-sync "closest form:drop"}
           (h/post-form-attrs
            {:to "/save"}))))

  (testing "post-form-attrs supports explicit native action fallback"
    (is (= {:method "post"
            :action "/save"
            :hx-post "/save"
            :hx-swap "innerHTML"
            :hx-sync "closest form:drop"}
           (h/post-form-attrs
            {:to "/save"
             :native-action? true}))))

  (testing "post-form-attrs supports target normalization, custom swap, and attrs"
    (is (= {:method "post"
            :hx-post "/save"
            :hx-swap "outerHTML"
            :hx-target "#profile"
            :hx-sync "closest form:drop"
            :class "inline-form"}
           (h/post-form-attrs
            {:to "/save"
             :target "profile"
             :swap "outerHTML"
             :attrs {:class "inline-form"}}))))

  (testing "post-form-attrs supports target normalization, custom swap, attrs, and explicit native fallback together"
    (is (= {:method "post"
            :action "/save"
            :hx-post "/save"
            :hx-swap "outerHTML"
            :hx-target "#profile"
            :hx-sync "closest form:drop"
            :class "inline-form"}
           (h/post-form-attrs
            {:to "/save"
             :target "profile"
             :swap "outerHTML"
             :native-action? true
             :attrs {:class "inline-form"}}))))

  (testing "post-form-attrs rejects missing or blank POST URLs"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"POST URL is required"
         (h/post-form-attrs {})))

    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"POST URL is required"
         (h/post-form-attrs {:to ""})))))

;; -----------------------------------------------------------------------------
;; SSE callback attrs
;; -----------------------------------------------------------------------------

(deftest sse-callback-root-attrs-test
  (testing "sse-callback-root-attrs builds parent SSE connection attrs"
    (is (= {:hx-ext "sse"
            :sse-connect "/app/live/stream"
            :aria-hidden "true"
            :id "callback-root"}
           (h/sse-callback-root-attrs
            {:id "callback-root"
             :stream-url "/app/live/stream"}))))

  (testing "sse-callback-root-attrs composes hx-ext"
    (is (= {:hx-ext "sse, path-deps"
            :sse-connect "/app/live/stream"
            :aria-hidden "true"}
           (h/sse-callback-root-attrs
            {:stream-url "/app/live/stream"
             :attrs {:hx-ext "path-deps"}}))))

  (testing "sse-callback-root-attrs rejects missing stream URL"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE stream URL is required"
         (h/sse-callback-root-attrs {})))))

(deftest sse-callback-trigger-attrs-test
  (testing "sse-callback-trigger-attrs builds GET callback attrs"
    (is (= {:hx-trigger "sse:live-update"
            :hx-swap "none"
            :hx-get "/fragment"
            :hx-target "#target"}
           (h/sse-callback-trigger-attrs
            {:event :live-update
             :get "/fragment"
             :target "target"}))))

  (testing "sse-callback-trigger-attrs builds POST callback attrs with jitter"
    (is (= {:hx-trigger "sse:client-oob delay:50ms"
            :hx-swap "none"
            :hx-post "/callback"
            :class "callback-trigger"}
           (h/sse-callback-trigger-attrs
            {:event :client-oob
             :post "/callback"
             :jitter-delay-ms 50
             :attrs {:class "callback-trigger"}}))))

  (testing "sse-callback-trigger-attrs rejects both GET and POST"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"may not include both"
         (h/sse-callback-trigger-attrs
          {:get "/a"
           :post "/b"}))))

  (testing "sse-callback-trigger-attrs requires one method"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires either :get or :post"
         (h/sse-callback-trigger-attrs
          {:event :live-update})))))

(deftest sse-callback-test
  (testing "sse-callback renders parent connection and child trigger"
    (is (= [:div
            {:hx-ext "sse"
             :sse-connect "/app/live/stream"
             :aria-hidden "true"
             :id "root"}
            [:div
             {:hx-trigger "sse:live-update"
              :hx-swap "none"
              :hx-get "/fragment"
              :class "trigger"}]]
           (h/sse-callback
            {:id "root"
             :stream-url "/app/live/stream"
             :event :live-update
             :get "/fragment"
             :trigger-attrs {:class "trigger"}})))))

;; -----------------------------------------------------------------------------
;; Direct SSE swap / OOB attrs
;; -----------------------------------------------------------------------------

(deftest sse-swap-attrs-test
  (testing "sse-swap-attrs builds direct SSE swap attrs"
    (is (= {:hx-ext "sse"
            :sse-connect "/app/live/oob"
            :sse-swap "client-oob"
            :hx-swap "none"
            :aria-hidden "true"
            :data-gesso-live-direct-sse true
            :data-gesso-live-direct-sse-experimental true
            :id "oob-listener"}
           (h/sse-swap-attrs
            {:id "oob-listener"
             :stream-url "/app/live/oob"
             :event :client-oob}))))

  (testing "sse-swap-attrs composes hx-ext and preserves caller attrs"
    (is (= {:hx-ext "sse, path-deps"
            :sse-connect "/app/live/oob"
            :sse-swap "client-oob"
            :hx-swap "none"
            :aria-hidden "true"
            :data-gesso-live-direct-sse true
            :data-gesso-live-direct-sse-experimental true
            :class "invisible"}
           (h/sse-swap-attrs
            {:stream-url "/app/live/oob"
             :event "client-oob"
             :attrs {:hx-ext "path-deps"
                     :class "invisible"}}))))

  (testing "sse-swap-attrs rejects missing stream URL"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"SSE stream URL is required"
         (h/sse-swap-attrs
          {:event :client-oob})))))

(deftest sse-swap-listener-test
  (testing "sse-swap-listener renders a div with direct SSE attrs"
    (is (= [:div
            {:hx-ext "sse"
             :sse-connect "/app/live/oob"
             :sse-swap "client-oob"
             :hx-swap "none"
             :aria-hidden "true"
             :data-gesso-live-direct-sse true
             :data-gesso-live-direct-sse-experimental true}]
           (h/sse-swap-listener
            {:stream-url "/app/live/oob"
             :event :client-oob})))))

(deftest sse-oob-listener-test
  (testing "sse-oob-listener marks the direct OOB experimental path"
    (is (= [:div
            {:hx-ext "sse"
             :sse-connect "/app/live/oob"
             :sse-swap "client-oob"
             :hx-swap "none"
             :aria-hidden "true"
             :data-gesso-live-direct-sse true
             :data-gesso-live-direct-sse-experimental true
             :data-gesso-live-oob-listener true
             :data-gesso-live-oob-experimental true}]
           (h/sse-oob-listener
            {:stream-url "/app/live/oob"
             :event :client-oob}))))

  (testing "sse-oob-listener preserves caller attrs while adding marker attrs"
    (is (= [:div
            {:hx-ext "sse, path-deps"
             :sse-connect "/app/live/oob"
             :sse-swap "client-oob"
             :hx-swap "none"
             :aria-hidden "true"
             :data-gesso-live-direct-sse true
             :data-gesso-live-direct-sse-experimental true
             :class "listener"
             :data-gesso-live-oob-listener true
             :data-gesso-live-oob-experimental true}]
           (h/sse-oob-listener
            {:stream-url "/app/live/oob"
             :event :client-oob
             :attrs {:hx-ext "path-deps"
                     :class "listener"}})))))

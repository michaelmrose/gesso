(ns gesso.live.continuity-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.continuity :as continuity]
   [gesso.live.htmx :as htmx]
   [gesso.live.ui :as ui]))

;; -----------------------------------------------------------------------------
;; Hiccup helpers
;; -----------------------------------------------------------------------------

(defn attrs
  [node]
  (when (and (vector? node)
             (map? (second node)))
    (second node)))

(defn children
  [node]
  (let [xs (rest node)]
    (if (map? (first xs))
      (rest xs)
      xs)))

;; -----------------------------------------------------------------------------
;; Continuity constructor tests
;; -----------------------------------------------------------------------------

(deftest anchor-scroll-test
  (testing "selector string shorthand"
    (is (= {:type "anchor-scroll"
            :selector "[data-row]"}
           (continuity/anchor-scroll "[data-row]"))))

  (testing "explicit selector options"
    (is (= {:type "anchor-scroll"
            :selector "[data-row]"
            :container-selector "[data-scroll-container]"
            :key-attr "data-id"}
           (continuity/anchor-scroll
            {:selector "[data-row]"
             :container-selector "[data-scroll-container]"
             :key-attr "data-id"}))))

  (testing "anchor-selector is also accepted"
    (is (= {:type "anchor-scroll"
            :anchor-selector "[data-card]"}
           (continuity/anchor-scroll
            {:anchor-selector "[data-card]"}))))

  (testing "selector or anchor-selector is required for explicit anchor-scroll"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"anchor-scroll requires :selector or :anchor-selector"
         (continuity/anchor-scroll {})))))

(deftest focus-test
  (testing "default focus box"
    (is (= {:type "focus"}
           (continuity/focus))))

  (testing "selector shorthand"
    (is (= {:type "focus"
            :selector "input, textarea"}
           (continuity/focus "input, textarea"))))

  (testing "explicit options"
    (is (= {:type "focus"
            :selector "[data-preserve-focus]"
            :key-attr "data-focus-key"}
           (continuity/focus
            {:selector "[data-preserve-focus]"
             :key-attr "data-focus-key"})))))

(deftest inputs-test
  (testing "default inputs box"
    (is (= {:type "inputs"}
           (continuity/inputs))))

  (testing "selector shorthand"
    (is (= {:type "inputs"
            :selector "[data-preserve-input]"}
           (continuity/inputs "[data-preserve-input]"))))

  (testing "explicit options"
    (is (= {:type "inputs"
            :selector "input[data-preserve]"
            :key-attr "name"}
           (continuity/inputs
            {:selector "input[data-preserve]"
             :key-attr "name"})))))

(deftest event-box-test
  (testing "event-backed box"
    (is (= {:type "event"
            :name "selected-row"}
           (continuity/event :selected-row))))

  (testing "event-backed box with options"
    (is (= {:type "event"
            :name "selected-row"
            :selector "[data-row]"}
           (continuity/event
            :selected-row
            {:selector "[data-row]"})))))

(deftest hyperscript-box-test
  (testing "hyperscript-backed box"
    (is (= {:type "hyperscript"
            :name "selected-row"}
           (continuity/hyperscript :selected-row))))

  (testing "hyperscript-backed box may carry capture/restore strings"
    (is (= {:type "hyperscript"
            :name "selected-row"
            :capture "capture hs"
            :restore "restore hs"}
           (continuity/hyperscript
            :selected-row
            {:capture "capture hs"
             :restore "restore hs"})))))

(deftest js-box-test
  (testing "js-backed box"
    (is (= {:type "js"
            :name "grid"
            :capture "myapp.grid.capture"
            :restore "myapp.grid.restore"}
           (continuity/js
            {:name "grid"
             :capture "myapp.grid.capture"
             :restore "myapp.grid.restore"}))))

  (testing "capture function is required"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires :capture"
         (continuity/js
          {:restore "myapp.grid.restore"}))))

  (testing "restore function is required"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"requires :restore"
         (continuity/js
          {:capture "myapp.grid.capture"})))))

(deftest generic-box-test
  (testing "keyword type normalizes to browser-runtime key"
    (is (= {:type "anchor-scroll"
            :selector "[data-row]"}
           (continuity/box
            :anchor-scroll
            {:selector "[data-row]"}))))

  (testing "symbol type normalizes to string"
    (is (= {:type "custom.widget"
            :foo "bar"}
           (continuity/box
            'custom.widget
            {:foo "bar"}))))

  (testing "box opts must be a map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"box opts must be a map"
         (continuity/box :focus [:not :a :map])))))

(deftest boxes-test
  (testing "boxes builds top-level client-continuity config"
    (is (= {:enabled true
            :boxes [{:type "anchor-scroll"
                     :selector "[data-row]"}
                    {:type "focus"}]}
           (continuity/boxes
            (continuity/anchor-scroll "[data-row]")
            (continuity/focus))))))

(deftest preserve-test
  (testing "preserve builds top-level config with built-in preserve map"
    (is (= {:enabled true
            :preserve {:scroll {:selector "[data-row]"}
                       :focus true
                       :inputs {:selector "[data-preserve-input]"}}}
           (continuity/preserve
            {:scroll {:selector "[data-row]"}
             :focus true
             :inputs {:selector "[data-preserve-input]"}}))))

  (testing "raw/basic scroll preservation is expressible without an anchor selector"
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true}}
           (continuity/preserve
            {:scroll true
             :focus true}))))

  (testing "preserve can include explicit boxes"
    (is (= {:enabled true
            :preserve {:focus true}
            :boxes [{:type "event"
                     :name "selected-row"}]}
           (continuity/preserve
            {:focus true
             :boxes [(continuity/event :selected-row)]}))))

  (testing "preserve opts must be a map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"preserve opts must be a map"
         (continuity/preserve [:not :a :map])))))

(deftest with-boxes-test
  (testing "with-boxes can start from nil"
    (is (= {:enabled true
            :boxes [{:type "focus"}]}
           (continuity/with-boxes
            nil
            (continuity/focus)))))

  (testing "with-boxes can start from true"
    (is (= {:enabled true
            :boxes [{:type "focus"}]}
           (continuity/with-boxes
            true
            (continuity/focus)))))

  (testing "with-boxes appends to existing boxes"
    (is (= {:enabled true
            :preserve {:focus true}
            :boxes [{:type "event"
                     :name "existing"}
                    {:type "inputs"}]}
           (continuity/with-boxes
            {:enabled true
             :preserve {:focus true}
             :boxes [(continuity/event :existing)]}
            (continuity/inputs)))))

  (testing "with-boxes rejects unsupported base shapes"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"with-boxes expects nil, true, or a map"
         (continuity/with-boxes
          [:not :a :map]
          (continuity/focus))))))

(deftest hx-preserve-test
  (testing "default attrs"
    (is (= {:hx-preserve true}
           (continuity/hx-preserve))))

  (testing "merge with supplied attrs"
    (is (= {:id "humanhelp-search"
            :name "q"
            :hx-preserve true}
           (continuity/hx-preserve
            {:id "humanhelp-search"
             :name "q"}))))

  (testing "hx-preserve-attrs constant"
    (is (= {:hx-preserve true}
           continuity/hx-preserve-attrs))))

;; -----------------------------------------------------------------------------
;; HTMX attr/config contract tests
;; -----------------------------------------------------------------------------

(deftest normalize-client-continuity-test
  (testing "true means useful conservative defaults, including basic scroll"
    ;; This is intentionally stricter than the earlier focus-only default.
    ;; A feature called client continuity should not require app code to opt into
    ;; basic no-jump scroll preservation.
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true}}
           (htmx/normalize-client-continuity true))))

  (testing "preserve sugar normalizes into the runtime preserve map"
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true
                       :inputs {:selector "[data-input]"}}}
           (htmx/normalize-client-continuity
            {:preserve-scroll true
             :preserve-focus true
             :preserve-inputs {:selector "[data-input]"}}))))

  (testing "selector scroll remains an anchor-scroll intent at the data boundary"
    (is (= {:enabled true
            :preserve {:scroll {:selector "[data-card]"}
                       :focus true}}
           (htmx/normalize-client-continuity
            (continuity/preserve
             {:scroll {:selector "[data-card]"}
              :focus true}))))))

(deftest client-continuity-attrs-test
  (testing "disabled continuity emits no attrs"
    (is (= {}
           (htmx/client-continuity-attrs
            {:fragment-id "request-list"
             :client-continuity false}))))

  (testing "raw scroll/focus config is encoded on the stable root"
    (let [attrs (htmx/client-continuity-attrs
                 {:fragment-id "request-list"
                  :client-continuity (continuity/preserve
                                      {:scroll true
                                       :focus true})})
          config (:data-gesso-live-continuity-config attrs)]
      (is (= "true" (:data-gesso-live-continuity attrs)))
      (is (= "request-list" (:data-gesso-live-continuity-fragment attrs)))
      (is (string? config))
      (is (str/includes? config "\"enabled\":true"))
      (is (str/includes? config "\"scroll\":true"))
      (is (str/includes? config "\"focus\":true"))))

  (testing "anchor selector config is encoded on the stable root"
    (let [attrs (htmx/client-continuity-attrs
                 {:fragment-id "request-list"
                  :client-continuity (continuity/preserve
                                      {:scroll {:selector "[data-card]"}
                                       :focus true})})
          config (:data-gesso-live-continuity-config attrs)]
      (is (= "true" (:data-gesso-live-continuity attrs)))
      (is (= "request-list" (:data-gesso-live-continuity-fragment attrs)))
      (is (str/includes? config "\"selector\":\"[data-card]\"")))))

(deftest fragment-panel-continuity-contract-test
  (let [panel (ui/fragment-panel
               {:id "request-list"
                :src "/app/fragments/requests"
                :stream-url "/app/streams/requests"
                :client-continuity (continuity/preserve
                                    {:scroll true
                                     :focus true})})
        root-attrs (attrs panel)
        target (first (children panel))
        target-attrs (attrs target)
        config (:data-gesso-live-continuity-config root-attrs)]
    (testing "stable root owns live behavior and continuity config"
      (is (= :div (first panel)))
      (is (= "sse" (:hx-ext root-attrs)))
      (is (= "/app/streams/requests" (:sse-connect root-attrs)))
      (is (= "/app/fragments/requests" (:hx-get root-attrs)))
      (is (= "#request-list" (:hx-target root-attrs)))
      (is (= "outerHTML" (:hx-swap root-attrs)))
      (is (= "true" (:data-gesso-live-continuity root-attrs)))
      (is (= "request-list" (:data-gesso-live-continuity-fragment root-attrs)))
      (is (str/includes? config "\"scroll\":true"))
      (is (str/includes? config "\"focus\":true")))

    (testing "replaceable target owns only the fragment id by default"
      (is (= :div (first target)))
      (is (= {:id "request-list"} target-attrs)))))

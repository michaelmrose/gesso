(ns gesso.live.continuity-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [gesso.live.continuity :as continuity]))

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

(deftest details-open-test
  (testing "selector shorthand"
    (is (= {:type "details-open"
            :selector "details[data-request]"}
           (continuity/details-open
            "details[data-request]"))))

  (testing "explicit options preserve browser-facing selector/key metadata"
    (is (= {:type "details-open"
            :selector "details[data-request]"
            :key-attr "data-request-id"}
           (continuity/details-open
            {:selector "details[data-request]"
             :key-attr "data-request-id"}))))

  (testing ":single? normalizes to the browser-facing :single key"
    (is (= {:type "details-open"
            :selector "details[data-request]"
            :single true}
           (continuity/details-open
            {:selector "details[data-request]"
             :single? true}))))

  (testing "existing :single is preserved"
    (is (= {:type "details-open"
            :selector "details[data-request]"
            :single false}
           (continuity/details-open
            {:selector "details[data-request]"
             :single false}))))

  (testing "selector is required"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"details-open requires :selector"
         (continuity/details-open {})))))

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
            {:selector "[data-row]"}))))

  (testing "event constructor owns both type and name"
    (doseq [[opts owned]
            [[{:type "js"} #{:type}]
             [{:name "different"} #{:name}]
             [{:type "js" :name "different"} #{:type :name}]]]
      (let [error (try
                    (continuity/event :selected-row opts)
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      e))]
        (is (some? error))
        (is (re-find #"cannot override constructor-owned keys"
                     (ex-message error)))
        (is (= owned
               (:owned-keys (ex-data error)))))))

  (testing "event opts must be a map"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"event opts must be a map"
         (continuity/event :selected-row [:not :a :map])))))

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
             :restore "restore hs"}))))

  (testing "hyperscript constructor owns both type and name"
    (doseq [[opts owned]
            [[{:type "event"} #{:type}]
             [{:name "different"} #{:name}]]]
      (let [error (try
                    (continuity/hyperscript :selected-row opts)
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      e))]
        (is (some? error))
        (is (re-find #"cannot override constructor-owned keys"
                     (ex-message error)))
        (is (= owned
               (:owned-keys (ex-data error))))))))

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

  (testing "unforeseen application box types remain open-ended"
    (is (= {:type "future-widget-that-gesso-does-not-know"
            :mode :special
            :nested {:application/data [1 2 3]}}
           (continuity/box
            "future-widget-that-gesso-does-not-know"
            {:mode :special
             :nested {:application/data [1 2 3]}}))))

  (testing "constructor-owned type cannot be replaced through generic options"
    (let [error (try
                  (continuity/box :focus {:type "js"})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    e))]
      (is (some? error))
      (is (re-find #"cannot override constructor-owned keys"
                   (ex-message error)))
      (is (= #{:type}
             (:owned-keys (ex-data error))))))

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

  (testing "with-boxes preserves the continuity policy represented by true"
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true}
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
;; Continuity wire/config contract tests
;; -----------------------------------------------------------------------------

(deftest normalize-client-continuity-test
  (testing "true means useful conservative defaults, including basic scroll"
    ;; This is intentionally stricter than the earlier focus-only default.
    ;; A feature called client continuity should not require app code to opt into
    ;; basic no-jump scroll preservation.
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true}}
           (continuity/normalize-client-continuity true))))

  (testing "preserve sugar normalizes into the runtime preserve map"
    (is (= {:enabled true
            :preserve {:scroll true
                       :focus true
                       :inputs {:selector "[data-input]"}}}
           (continuity/normalize-client-continuity
            {:preserve-scroll true
             :preserve-focus true
             :preserve-inputs {:selector "[data-input]"}}))))

  (testing "selector scroll remains an anchor-scroll intent at the data boundary"
    (is (= {:enabled true
            :preserve {:scroll {:selector "[data-card]"}
                       :focus true}}
           (continuity/normalize-client-continuity
            (continuity/preserve
             {:scroll {:selector "[data-card]"}
              :focus true}))))))

(deftest normalize-client-continuity-shapes-test
  (testing "disabled forms all normalize to nil"
    (is (nil? (continuity/normalize-client-continuity nil)))
    (is (nil? (continuity/normalize-client-continuity false)))
    (is (nil?
         (continuity/normalize-client-continuity
          {:enabled false
           :preserve {:focus true}}))))

  (testing "sequential shorthand is normalized as explicit boxes"
    (is (= {:enabled true
            :boxes [{:type "focus"}
                    {:type "widget"
                     :name "app/selection"}]}
           (continuity/normalize-client-continuity
            [:focus
             {:type :application/widget
              :name :app/selection}]))))

  (testing ":preserve true means the conservative focus-only preserve map"
    (is (= {:enabled true
            :preserve {:focus true}}
           (continuity/normalize-client-continuity
            {:preserve true}))))

  (testing "unknown map keys remain data for higher-level extensions"
    (is (= {:enabled true
            :extension {:mode :custom}}
           (continuity/normalize-client-continuity
            {:extension {:mode :custom}})))))

(deftest normalize-client-continuity-validation-test
  (testing "unsupported top-level shapes fail"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"must be nil, false, true, a map, or a sequential collection"
         (continuity/normalize-client-continuity 42))))

  (testing "invalid preserve shape fails"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":preserve must be"
         (continuity/normalize-client-continuity
          {:preserve :not-a-map}))))

  (testing "boxes must be sequential"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":boxes must be a sequential collection"
         (continuity/normalize-client-continuity
          {:boxes {:type :focus}}))))

  (testing "individual box entries must be supported data forms"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #":boxes entries must be maps, keywords, symbols, or strings"
         (continuity/normalize-client-continuity
          {:boxes [42]})))))

(deftest client-continuity-attrs-test
  (testing "disabled continuity emits no attrs"
    (is (= {}
           (continuity/client-continuity-attrs
            {:fragment-id "request-list"
             :client-continuity false}))))

  (testing "raw scroll/focus config is encoded on the stable root"
    (let [attrs (continuity/client-continuity-attrs
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
    (let [attrs (continuity/client-continuity-attrs
                 {:fragment-id "request-list"
                  :client-continuity (continuity/preserve
                                      {:scroll {:selector "[data-card]"}
                                       :focus true})})
          config (:data-gesso-live-continuity-config attrs)]
      (is (= "true" (:data-gesso-live-continuity attrs)))
      (is (= "request-list" (:data-gesso-live-continuity-fragment attrs)))
      (is (str/includes? config "\"selector\":\"[data-card]\"")))))

(deftest client-continuity-attrs-exact-wire-test
  (is (= {:data-gesso-live-continuity "true"
          :data-gesso-live-continuity-fragment "request-list"
          :data-gesso-live-continuity-config
          "{\"enabled\":true,\"preserve\":{\"focus\":true,\"scroll\":true}}"}
         (continuity/client-continuity-attrs
          {:fragment-id "request-list"
           :client-continuity true}))))

(deftest continuity-wire-attrs-test
  (testing "continuity owns the exact browser-runtime attribute vocabulary"
    (is (= :data-gesso-live-continuity
           continuity/continuity-attr))
    (is (= :data-gesso-live-continuity-config
           continuity/continuity-config-attr))
    (is (= :data-gesso-live-continuity-fragment
           continuity/continuity-fragment-attr))
    (is (= #{:data-gesso-live-continuity
             :data-gesso-live-continuity-config
             :data-gesso-live-continuity-fragment}
           continuity/continuity-attrs))))

(deftest client-continuity-json-test
  (testing "disabled continuity has no wire representation"
    (is (nil? (continuity/client-continuity-json nil)))
    (is (nil? (continuity/client-continuity-json false))))

  (testing "JSON encoding is deterministic regardless of input map insertion order"
    (let [a (array-map
             :preserve-focus true
             :preserve-scroll {:selector "[data-card]"}
             :custom {:z 3 :a 1})
          b (array-map
             :custom (array-map :a 1 :z 3)
             :preserve-scroll {:selector "[data-card]"}
             :preserve-focus true)]
      (is (= (continuity/client-continuity-json a)
             (continuity/client-continuity-json b)))))

  (testing "Clojure functions cannot leak into browser configuration"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"cannot contain Clojure functions"
         (continuity/client-continuity-json
          {:boxes [{:type :custom
                    :capture (fn [] :nope)}]})))))

(deftest client-continuity-json-edge-test
  (testing "JSON escaping preserves a deterministic valid wire string"
    (is (= "{\"enabled\":true,\"label\":\"quote=\\\" slash=\\\\ newline=\\n\"}"
           (continuity/client-continuity-json
            {:label "quote=\" slash=\\ newline=\n"}))))

  (testing "sets are encoded deterministically"
    (is (= "{\"enabled\":true,\"values\":[\"a\",\"b\",\"c\"]}"
           (continuity/client-continuity-json
            {:values #{:c :a :b}}))))

  (testing "non-finite numbers cannot enter browser configuration"
    (doseq [value [Double/NaN
                   Double/POSITIVE_INFINITY
                   Double/NEGATIVE_INFINITY]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot encode non-finite numbers"
           (continuity/client-continuity-json
            {:value value}))))))

(deftest client-continuity-attrs-validation-test
  (testing "enabled continuity requires a stable fragment id"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"fragment id must be a non-blank string"
         (continuity/client-continuity-attrs
          {:client-continuity true})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"fragment id must be a non-blank string"
         (continuity/client-continuity-attrs
          {:fragment-id "   "
           :client-continuity true}))))

  (testing "disabled continuity does not require a fragment id"
    (is (= {}
           (continuity/client-continuity-attrs
            {:client-continuity false})))))

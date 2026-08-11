(ns gesso.live.browser.optimistic-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.choreo :as runtime]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as browser]
   [gesso.live.optimistic.choreo :as optimistic
    :include-macros true]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(def source-key :gesso.live.browser.optimistic/source)
(def target-key :gesso.live.browser.optimistic/target)
(def target-id-key :gesso.live.browser.optimistic/target-id)
(def descriptor-key :gesso.live.browser.optimistic/descriptor)
(def root-key :gesso.live.browser.optimistic/root)
(def snapshot-key :gesso.live.browser.optimistic/snapshot)
(def projection-key :gesso.live.browser.optimistic/projection)
(def pending-source-state-key :gesso.live.browser.optimistic/pending-source-state)

(def scope-wire "e:[:request \"request-1\"]")
(def transition-wire "request/claim")
(def template-name "request-1-claim")
(def target-selector "closest [data-request-card]")

(defn- element
  ([tag] (element tag nil))
  ([tag attrs]
   (let [node (.createElement js/document tag)]
     (doseq [[k v] attrs :when (some? v)]
       (.setAttribute node (dom/attr-name k) (str v)))
     node)))

(defn- append! [parent child]
  (.appendChild parent child)
  child)

(defn- text! [node value]
  (set! (.-textContent node) value)
  node)

(defn- thrown [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data [f]
  (some-> (thrown f) ex-data))

(defn- reset-runtime! []
  (runtime/reset-runtime!)
  (reset! browser/target-locks {})
  (reset! browser/executions-by-source {})
  (reset! browser/outgoing-actions {})
  (reset! continuity/slots {})
  (continuity/register-built-in-boxes!)
  true)

(defn- sandbox []
  (let [root (element "div" {:data-gesso-test (str (random-uuid))})]
    (.appendChild (.-body js/document) root)
    root))

(defn- with-sandbox* [f]
  (let [old-htmx (.-htmx js/window)
        root (sandbox)]
    (reset-runtime!)
    (try
      (f root)
      (finally
        (set! (.-htmx js/window) old-htmx)
        (reset-runtime!)
        (.remove root)))))

(defn- source-attrs
  ([] (source-attrs nil))
  ([overrides]
   (merge
    {protocol/protocol-attr protocol/version
     protocol/transition-attr transition-wire
     protocol/template-attr template-name
     protocol/target-attr target-selector
     protocol/scope-attr scope-wire
     protocol/base-revision-attr "i:7"
     protocol/pending-label-attr "Claiming…"
     protocol/projection-mode-attr "provisional"}
    overrides)))

(defn- canonical-attrs
  ([revision] (canonical-attrs scope-wire revision))
  ([scope revision]
   {protocol/canonical-attr "true"
    protocol/protocol-attr protocol/version
    protocol/scope-attr scope
    protocol/revision-attr (protocol/revision->wire revision)}))

(defn- projection-template
  ([] (projection-template {}))
  ([{:keys [name scope tag id text]
     :or {name template-name
          scope scope-wire
          tag "details"
          id "request-1"
          text "Claiming…"}}]
   (let [template
         (element
          "template"
          {protocol/protocol-attr protocol/version
           protocol/transition-attr transition-wire
           protocol/template-attr name
           protocol/scope-attr scope
           protocol/projection-mode-attr "provisional"})
         projection
         (element
          tag
          (merge {:data-request-card "request-1"}
                 (when id {:id id})))]
     (append! projection (text! (element "summary") text))
     (.appendChild (.-content template) projection)
     template)))

(defn- command-dom!
  ([root] (command-dom! root {}))
  ([root {:keys [scope target-id template-name* target-selector*
                 base-revision-wire projection-tag projection-id
                 continuity?]
          :or {scope scope-wire
               target-id "request-1"
               template-name* template-name
               target-selector* target-selector
               base-revision-wire "i:7"
               projection-tag "details"
               projection-id "request-1"
               continuity? true}}]
   (let [continuity-root
         (if continuity?
           (append!
            root
            (element
             "section"
             {continuity/continuity-attr "true"
              continuity/continuity-fragment-attr-key target-id}))
           root)
         target
         (append!
          continuity-root
          (element
           "details"
           (merge {:id target-id
                   :data-request-card "request-1"}
                  (canonical-attrs scope 7))))
         _summary
         (append! target (text! (element "summary") "Unclaimed"))
         form
         (append! target (element "form" {:data-gesso-live-post "true"}))
         source
         (append!
          form
          (element
           "button"
           (source-attrs
            {protocol/template-attr template-name*
             protocol/target-attr target-selector*
             protocol/scope-attr scope
             protocol/base-revision-attr base-revision-wire
             :type "button"})))
         label
         (append!
          source
          (text!
           (element "span" {:data-gesso-button-label "true"})
           "Claim"))
         template
         (projection-template
          {:name template-name*
           :scope scope
           :tag projection-tag
           :id projection-id})]
     ;; Intended corrected server shape: request form and template are siblings.
     (append! target template)
     {:root root
      :continuity-root (when continuity? continuity-root)
      :target target
      :form form
      :source source
      :label label
      :template template})))

(defn- canonical-node
  ([revision text]
   (canonical-node scope-wire revision text "request-1"))
  ([scope revision text id]
   (let [node
         (element
          "details"
          (merge {:id id
                  :data-request-card id}
                 (canonical-attrs scope revision)))]
     (append! node (text! (element "summary") text))
     node)))

(defn- settlement-marker
  ([execution-id outcome revision]
   (settlement-marker execution-id scope-wire outcome revision nil))
  ([execution-id scope outcome revision reason]
   (element
    "template"
    {protocol/settlement-attr "true"
     protocol/protocol-attr protocol/version
     protocol/execution-attr execution-id
     protocol/scope-attr scope
     protocol/outcome-attr (protocol/settlement-outcome->wire outcome)
     protocol/command-applied-attr
     (protocol/command-applied->wire
      (protocol/command-applied-for-outcome? outcome))
     protocol/revision-attr
     (when (some? revision)
       (protocol/revision->wire revision))
     protocol/reason-attr reason})))

(defn- settlement-root
  ([execution-id outcome revision]
   (settlement-root execution-id scope-wire outcome revision))
  ([execution-id scope outcome revision]
   (let [fragment (.createDocumentFragment js/document)
         marker (settlement-marker execution-id scope outcome revision nil)
         canonical (canonical-node scope revision
                                   (str "Canonical " (name outcome))
                                   "request-1")]
     (.appendChild fragment marker)
     (.appendChild fragment canonical)
     {:root fragment
      :marker marker
      :canonical canonical})))

(defn- fragment-html [fragment]
  (let [container (element "div")]
    (.appendChild container (.cloneNode fragment true))
    (.-innerHTML container)))

(defn- install! []
  (browser/initialize!))

(defn- terminal [execution-id]
  (some #(when (= execution-id (:execution-id %)) %)
        (runtime/terminal-summaries)))

;; -----------------------------------------------------------------------------
;; Compiled product / identity
;; -----------------------------------------------------------------------------

(deftest browser-plan-test
  (is (= optimistic/protocol-name (:name browser/browser-plan)))
  (is (= optimistic/browser-role (:role browser/browser-plan)))
  (is (map? (:states browser/browser-plan)))
  (is (contains? (:states browser/browser-plan)
                 (:initial browser/browser-plan))))

(deftest runtime-constants-test
  (is (= "data-gesso-optimistic-active" browser/active-attr))
  (is (= "data-gesso-optimistic-pending" browser/pending-attr))
  (is (= "data-gesso-optimistic-locked" browser/locked-attr))
  (is (= "data-gesso-optimistic-source-pending" browser/pending-source-attr))
  (is (= 15000 browser/default-settlement-timeout-ms))
  (is (= :optimistic/settlement-timeout browser/settlement-timer-key)))

(deftest browser-machine-handler-set-test
  (is (= #{optimistic/browser-acquire-target-machine
           optimistic/browser-capture-continuity-machine
           optimistic/browser-capture-snapshot-machine
           optimistic/browser-install-projection-machine
           optimistic/browser-schedule-timeout-machine
           optimistic/browser-install-canonical-machine
           optimistic/browser-discard-snapshot-machine
           optimistic/browser-recover-snapshot-machine
           optimistic/browser-restore-continuity-machine
           optimistic/browser-cancel-timeout-machine
           optimistic/browser-clear-pending-machine
           optimistic/browser-release-target-machine}
         (set (keys browser/browser-machine-handlers)))))

;; -----------------------------------------------------------------------------
;; Source discovery / descriptor
;; -----------------------------------------------------------------------------

(deftest optimistic-source-test
  (let [source (element "button" (source-attrs))
        child (append! source (element "span"))]
    (is (identical? source (browser/optimistic-source source)))
    (is (identical? source (browser/optimistic-source child)))
    (is (nil? (browser/optimistic-source (element "button"))))
    (is (nil? (browser/optimistic-source nil)))))

(deftest source-descriptor-test
  (let [descriptor
        (browser/source-descriptor
         (element "button" (source-attrs)))]
    (is (= protocol/version (:protocol-version descriptor)))
    (is (= transition-wire (:transition descriptor)))
    (is (= template-name (:template-name descriptor)))
    (is (= target-selector (:target descriptor)))
    (is (= scope-wire (:scope descriptor)))
    (is (= 7 (:base-revision descriptor)))
    (is (= "Claiming…" (:pending-label descriptor)))
    (is (= :provisional (:projection-mode descriptor)))))

(deftest source-descriptor-defaults-projection-mode-test
  (let [source
        (element
         "button"
         (dissoc (source-attrs) protocol/projection-mode-attr))]
    (is (= :provisional
           (:projection-mode (browser/source-descriptor source))))))

(deftest source-descriptor-decodes-opaque-revision-test
  (let [source
        (element
         "button"
         (source-attrs {protocol/base-revision-attr "s:opaque"}))]
    (is (= "opaque"
           (:base-revision (browser/source-descriptor source))))))

(deftest source-descriptor-rejects-unsupported-protocol-test
  (let [source
        (element
         "button"
         (source-attrs {protocol/protocol-attr "999"}))
        data (thrown-data #(browser/source-descriptor source))]
    (is (= :gesso.live.optimistic/unsupported-protocol (:error/type data)))
    (is (= protocol/version (:expected data)))
    (is (= "999" (:actual data)))))

(deftest source-descriptor-requires-fields-test
  (doseq [[attr-key field]
          [[protocol/transition-attr :transition]
           [protocol/template-attr :template]
           [protocol/target-attr :target]
           [protocol/scope-attr :scope]]]
    (let [source (element "button" (dissoc (source-attrs) attr-key))
          data (thrown-data #(browser/source-descriptor source))]
      (is (= :gesso.live.optimistic/incomplete-descriptor (:error/type data)))
      (is (= field (:field data))))))

;; -----------------------------------------------------------------------------
;; Extended target resolution
;; -----------------------------------------------------------------------------

(deftest extended-selector-test
  (let [outer (element "details" {:data-card "request"})
        source (append! outer (element "div"))
        nested (append! source (element "span" {:data-found "yes"}))]
    (is (identical? source
                    (browser/resolve-extended-selector source "this")))
    (is (identical? outer
                    (browser/resolve-extended-selector
                     source "closest [data-card]")))
    (is (identical? nested
                    (browser/resolve-extended-selector
                     source "find [data-found]")))))

(deftest next-and-previous-selector-test
  (let [parent (element "div")
        previous (append! parent (element "div" {:data-target "previous"}))
        _skip-a (append! parent (element "span"))
        source (append! parent (element "button"))
        _skip-b (append! parent (element "span"))
        next (append! parent (element "div" {:data-target "next"}))]
    (is (identical? previous
                    (browser/resolve-extended-selector
                     source "previous [data-target]")))
    (is (identical? next
                    (browser/resolve-extended-selector
                     source "next [data-target]")))))

(deftest document-selector-test
  (with-sandbox*
   (fn [root]
     (let [target (append! root (element "div" {:id "global-target"}))]
       (is (identical?
            target
            (browser/resolve-extended-selector
             (element "button") "#global-target")))))))

;; -----------------------------------------------------------------------------
;; Template / prepare
;; -----------------------------------------------------------------------------

(deftest resolve-template-prefers-local-parent-test
  (with-sandbox*
   (fn [root]
     (let [parent (append! root (element "div"))
           source (append! parent (element "button"))
           local (append! parent (projection-template))
           _global (append! root (projection-template))]
       (is (identical?
            local
            (browser/resolve-template
             source nil {:template-name template-name})))))))

(deftest prepare-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [continuity-root target source template]}
           (command-dom! root)
           prepared (browser/prepare source nil)]
       (is (identical? source (:source prepared)))
       (is (identical? target (:target prepared)))
       (is (= "request-1" (:target-id prepared)))
       (is (identical? template (:template prepared)))
       (is (= "DETAILS" (.-tagName (:projection prepared))))
       (is (identical? continuity-root (:root prepared)))
       (is (= scope-wire (get-in prepared [:descriptor :scope])))))))

(deftest prepare-requires-target-and-template-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source template]}
           (command-dom! root {:target-selector* "#missing"})]
       (is (= :gesso.live.optimistic/no-target
              (:error/type
               (thrown-data #(browser/prepare source nil)))))
       ;; Restore a valid target selector, remove the template.
       (dom/set-attr! source protocol/target-attr target-selector)
       (.remove template)
       (is (= :gesso.live.optimistic/no-template
              (:error/type
               (thrown-data #(browser/prepare source nil)))))))))

(deftest prepare-enforces-root-tag-and-id-test
  (with-sandbox*
   (fn [root]
     (let [{source-a :source}
           (command-dom! root {:projection-tag "div"
                               :projection-id nil})]
       (is (= :gesso.live.browser.dom/root-tag-mismatch
              (:error/type
               (thrown-data #(browser/prepare source-a nil))))))
     (let [{source-b :source}
           (command-dom! root {:target-id "request-2"
                               :template-name* "template-2"
                               :projection-id "wrong-id"})]
       (is (= :gesso.live.browser.dom/target-id-mismatch
              (:error/type
               (thrown-data #(browser/prepare source-b nil)))))))))

;; -----------------------------------------------------------------------------
;; Critical regression: HTMX transport target is not semantic target authority
;; -----------------------------------------------------------------------------

(deftest transport-target-must-not-override-descriptor-target-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]}
           (command-dom! root)
           incidental
           (append! root (text! (element "details") "Incidental HTMX target"))]
       (install!)
       (let [result
             (browser/start!
              source
              {:execution-id "execution-1"
               ;; Models event.detail.target from HTMX. Structurally compatible,
               ;; but not the semantic optimistic object selected by descriptor.
               :target incidental})]
         (testing "descriptor target owns optimistic projection"
           (is (= "execution-1" (dom/attr target browser/active-attr)))
           (is (= "Claiming…"
                  (.-textContent (.-firstElementChild target)))))
         (testing "incidental transport target remains untouched"
           (is (= "Incidental HTMX target" (.-textContent incidental)))
           (is (nil? (dom/attr incidental browser/active-attr))))
         (is (machine/suspended? (:execution result))))))))

(deftest incompatible-transport-target-cannot-cancel-valid-semantic-start-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           incidental (append! root (element "button"))]
       (install!)
       (let [attempt
             (try
               {:result
                (browser/start!
                 source
                 {:execution-id "execution-1"
                  :target incidental})}
               (catch :default error
                 {:error error}))]
         (is (nil? (:error attempt))
             "HTMX transport target must not override a valid semantic descriptor target.")
         (when-let [result (:result attempt)]
           (is (machine/suspended? (:execution result)))
           (is (= "execution-1" (dom/attr target browser/active-attr)))))))))

;; -----------------------------------------------------------------------------
;; Scope locks
;; -----------------------------------------------------------------------------

(deftest target-lock-test
  (reset-runtime!)
  (let [target (element "details")]
    (is (browser/reserve-target! scope-wire "execution-1" target "request-1"))
    (is (= "execution-1"
           (:execution-id (browser/current-lock scope-wire))))
    (is (= scope-wire
           (:scope (browser/execution-lock "execution-1"))))
    (is (browser/scope-busy? scope-wire))
    (is (false?
         (browser/reserve-target! scope-wire "execution-2" target "request-1")))
    (browser/release-target! scope-wire "execution-2")
    (is (browser/scope-busy? scope-wire))
    (browser/release-target! scope-wire "execution-1")
    (is (false? (browser/scope-busy? scope-wire)))))

;; -----------------------------------------------------------------------------
;; Runtime installation
;; -----------------------------------------------------------------------------

(deftest initialize-test
  (reset-runtime!)
  (is (true? (browser/initialize!)))
  (is (= (set (keys browser/browser-machine-handlers))
         (runtime/registered-fx-machines)))
  (is (= (set (keys browser/browser-machine-handlers))
         (runtime/registered-fx-handlers)))
  (is (ifn? (runtime/current-send-handler)))
  (reset-runtime!))

(deftest transport-handoff-rejects-unexpected-send-test
  (reset-runtime!)
  (browser/install-transport-handoff!)
  (let [handler (runtime/current-send-handler)
        data
        (thrown-data
         #(handler
           {:kind :send
            :from :browser
            :to :wrong
            :event :wrong
            :payload {}}
           {:execution-id "execution-1"}))]
    (is (= :gesso.live.optimistic/unexpected-send (:error/type data)))
    (is (= "execution-1" (:execution-id data))))
  (reset-runtime!))

;; -----------------------------------------------------------------------------
;; Start lifecycle
;; -----------------------------------------------------------------------------

(deftest start-installs-projection-and-command-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [continuity-root target source label]}
           (command-dom! root)
           processed (atom [])
           started (atom nil)]
       (set! (.-htmx js/window)
             #js {:process (fn [node] (swap! processed conj node))})
       (.addEventListener
        continuity-root
        "gesso:optimistic:started"
        (fn [event] (reset! started (.-detail event))))
       (install!)
       (let [{:keys [execution-id execution command]}
             (browser/start!
              source
              {:execution-id "execution-1"
               :consistency-token "xtdb-token"})]
         (is (= "execution-1" execution-id))
         (is (machine/suspended? execution))
         (is (runtime/active? execution-id))
         (testing "projection is provisional and in-place"
           (is (dom/connected? target))
           (is (false? (dom/canonical? target)))
           (is (= scope-wire (dom/scope target)))
           (is (= "execution-1" (dom/attr target browser/active-attr)))
           (is (= "true" (dom/attr target browser/pending-attr)))
           (is (= "true" (dom/attr target browser/locked-attr)))
           (is (= "Claiming…"
                  (.-textContent (.-firstElementChild target)))))
         (testing "initiating source carries pending UI state"
           (is (= "execution-1" (dom/attr source protocol/execution-attr)))
           (is (= "true" (dom/attr source browser/pending-source-attr)))
           (is (= "true" (dom/attr source "aria-busy")))
           (is (= "true" (dom/attr source "aria-disabled")))
           (is (true? (.-disabled source)))
           (is (= "Claiming…" (.-textContent label))))
         (testing "command handoff is exact projected protocol data"
           (is (= command (browser/command-action execution-id)))
           (is (= (:payload command) (browser/command-payload execution-id)))
           (is (= {:execution-id "execution-1"
                   :transition transition-wire
                   :scope scope-wire
                   :base-revision 7
                   :consistency-token "xtdb-token"}
                  (:payload command))))
         (is (= [target] @processed))
         (is (browser/scope-busy? scope-wire))
         (is (some? (runtime/timer execution-id browser/settlement-timer-key)))
         (is (= execution-id (browser/execution-for-source source)))
         (is (= "execution-1" (aget @started "executionId")))
         (is (= transition-wire (aget @started "transition")))
         (is (= scope-wire (aget @started "scope"))))))))

(deftest start-without-base-revision-omits-command-field-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]}
           (command-dom! root {:base-revision-wire nil})]
       (dom/remove-attr! source protocol/base-revision-attr)
       (install!)
       (let [payload
             (:payload
              (:command
               (browser/start! source {:execution-id "execution-1"})))]
         (is (= #{:execution-id :transition :scope}
                (set (keys payload)))))))))

(deftest second-command-for-same-scope-is-rejected-test
  (with-sandbox*
   (fn [root]
     (let [{source-a :source}
           (command-dom! root {:target-id "request-1"})
           {source-b :source}
           (command-dom! root {:target-id "request-2"
                               :template-name* "request-2-claim"})]
       (install!)
       (browser/start! source-a {:execution-id "execution-1"})
       (let [data
             (thrown-data
              #(browser/start! source-b {:execution-id "execution-2"}))]
         (is (= :gesso.live.optimistic/target-busy (:error/type data)))
         (is (= "execution-1" (:owner data)))
         (is (false? (runtime/active? "execution-2"))))))))

(deftest request-header-test
  (is (= ["Gesso-Optimistic-Execution" "execution-1"]
         (browser/request-header "execution-1"))))

;; -----------------------------------------------------------------------------
;; Settlement marker / response parsing
;; -----------------------------------------------------------------------------

(deftest marker-to-settlement-test
  (let [settlement
        (browser/marker->settlement
         (settlement-marker "execution-1" scope-wire :confirmed 8 "ok"))]
    (is (= "execution-1" (get settlement optimistic/execution-id-key)))
    (is (= scope-wire (get settlement optimistic/scope-key)))
    (is (= :confirmed (get settlement optimistic/outcome-key)))
    (is (true? (get settlement optimistic/command-applied-key)))
    (is (= 8 (get settlement optimistic/revision-key)))
    (is (= "ok" (get settlement optimistic/reason-key)))))

(deftest marker-supports-all-semantic-outcomes-test
  (doseq [outcome protocol/settlement-outcomes]
    (let [settlement
          (browser/marker->settlement
           (settlement-marker "execution-1" outcome 8))]
      (is (= outcome (get settlement optimistic/outcome-key)))
      (is (= (protocol/command-applied-for-outcome? outcome)
             (get settlement optimistic/command-applied-key))))))

(deftest marker-requires-version-correlation-and-consistency-test
  (let [bad-version (settlement-marker "execution-1" :confirmed 8)]
    (dom/set-attr! bad-version protocol/protocol-attr "999")
    (is (= :gesso.live.optimistic/unsupported-settlement-protocol
           (:error/type
            (thrown-data #(browser/marker->settlement bad-version))))))
  (let [missing-execution (settlement-marker "execution-1" :confirmed 8)]
    (dom/remove-attr! missing-execution protocol/execution-attr)
    (is (= :gesso.live.optimistic/incomplete-settlement
           (:error/type
            (thrown-data #(browser/marker->settlement missing-execution))))))
  (let [inconsistent (settlement-marker "execution-1" :confirmed 8)]
    (dom/set-attr! inconsistent protocol/command-applied-attr "false")
    (is (thrown? cljs.core.ExceptionInfo
                 (browser/marker->settlement inconsistent)))))

(deftest settlement-from-root-test
  (let [{:keys [root canonical]}
        (settlement-root "execution-1" :confirmed 8)
        settlement (browser/settlement-from-root root "execution-1")]
    (is (= "execution-1" (get settlement optimistic/execution-id-key)))
    (is (= 8 (get settlement optimistic/revision-key)))
    (is (identical? canonical (get settlement optimistic/canonical-key)))))

(deftest settlement-root-correlation-and-authority-errors-test
  (let [{other-root :root}
        (settlement-root "other" :confirmed 8)]
    (is (nil? (browser/settlement-from-root other-root "execution-1"))))
  (let [root (.createDocumentFragment js/document)]
    (.appendChild root (settlement-marker "execution-1" :confirmed 8))
    (is (= :gesso.live.optimistic/missing-canonical
           (:error/type
            (thrown-data #(browser/settlement-from-root root "execution-1"))))))
  (let [{:keys [root]} (settlement-root "execution-1" :confirmed 8)]
    (.appendChild root (settlement-marker "execution-1" :confirmed 8))
    (is (= :gesso.live.optimistic/duplicate-settlement
           (:error/type
            (thrown-data #(browser/settlement-from-root root "execution-1")))))))

(deftest settlement-revision-must-match-canonical-test
  (let [{:keys [root canonical]}
        (settlement-root "execution-1" :confirmed 8)]
    (dom/set-attr! canonical protocol/revision-attr "i:9")
    (let [data
          (thrown-data #(browser/settlement-from-root root "execution-1"))]
      (is (= :gesso.live.optimistic/settlement-revision-mismatch
             (:error/type data)))
      (is (= "i:8" (:marker-revision data)))
      (is (= "i:9" (:canonical-revision data))))))

(deftest response-root-test
  (let [root
        (browser/response-root
         #js {:responseText "<div id='a'>A</div><div id='b'>B</div>"})]
    (is (= 2 (.-childElementCount root))))
  (is (nil? (browser/response-root #js {:responseText "  "}))))

;; -----------------------------------------------------------------------------
;; Request failure recovery
;; -----------------------------------------------------------------------------

(deftest request-failure-recovers-snapshot-through-continuity-barrier-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           callback (atom nil)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (with-redefs
        [continuity/restore!
         (fn [_root complete!]
           (reset! callback complete!)
           nil)]
        (let [result (browser/request-failed! "execution-1" :network)]
          (is (= :resumed (:status result)))
          (testing "old structural snapshot is restored before post-layout continuity"
            (is (= "Unclaimed"
                   (.-textContent (.-firstElementChild target))))
            (is (dom/canonical? target))
            (is (= 7 (dom/revision target))))
          (is (runtime/active? "execution-1"))
          (is (browser/scope-busy? scope-wire))
          (is (ifn? @callback))
          ;; Callback is intentionally invoked after the first resume commits.
          (@callback {:root nil :target target :slot nil})
          (is (false? (runtime/active? "execution-1")))
          (is (false? (browser/scope-busy? scope-wire)))
          (is (nil? (browser/command-action "execution-1")))
          (is (= {:outcome :request-failed}
                 (:result (terminal "execution-1"))))))))))

(deftest request-failure-never-overwrites-explicit-canonical-state-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           restore-count (atom 0)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [canonical (canonical-node 8 "New canonical")]
         (dom/copy-canonical-into! target (.cloneNode canonical true)))
       (with-redefs
        [continuity/restore! (fn [& _] (swap! restore-count inc))]
        (let [result (browser/request-failed! "execution-1" :network)]
          (is (= :completed (:status result)))
          (is (= {:outcome :superseded} (:result result)))
          (is (= "New canonical"
                 (.-textContent (.-firstElementChild target))))
          (is (= 0 @restore-count))))))))

(deftest missing-or-lost-recovery-authority-is-visible-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (.remove target)
       (is (= :gesso.live.optimistic/no-recovery-target
              (:error/type
               (thrown-data
                #(browser/request-failed! "execution-1" :network))))))))
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (dom/set-attr! target browser/active-attr "execution-2")
       (is (= :gesso.live.optimistic/recovery-authority-lost
              (:error/type
               (thrown-data
                #(browser/request-failed! "execution-1" :network)))))))))

;; -----------------------------------------------------------------------------
;; Settlement lifecycle
;; -----------------------------------------------------------------------------

(deftest confirmed-settlement-installs-canonical-before-continuity-completes-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           callback (atom nil)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (with-redefs
        [continuity/restore!
         (fn [_root complete!]
           (reset! callback complete!)
           nil)]
        (let [{settlement-root :root}
              (settlement-root "execution-1" :confirmed 8)
              settlement
              (browser/settlement-from-root settlement-root "execution-1")
              result (browser/settle! settlement)]
          (is (= :resumed (:status result)))
          (is (dom/canonical? target))
          (is (= 8 (dom/revision target)))
          (is (= "Canonical confirmed"
                 (.-textContent (.-firstElementChild target))))
          (is (nil? (get (runtime/execution-context "execution-1") snapshot-key)))
          (is (runtime/active? "execution-1"))
          (is (ifn? @callback))
          (@callback {:root nil :target target :slot nil})
          (is (false? (runtime/active? "execution-1")))
          (is (= {:outcome :confirmed}
                 (:result (terminal "execution-1"))))))))))

(deftest all-settlement-outcomes-return-semantic-outcome-test
  (doseq [outcome protocol/settlement-outcomes]
    (with-sandbox*
     (fn [root]
       (let [{:keys [source]} (command-dom! root)]
         (install!)
         (browser/start! source {:execution-id "execution-1"})
         (with-redefs [continuity/restore! (fn [_ _] nil)]
           (let [{settlement-root :root}
                 (settlement-root "execution-1" outcome 8)
                 result
                 (browser/settle!
                  (browser/settlement-from-root
                   settlement-root "execution-1"))]
             (is (= :resumed (:status result)))
             (let [finished
                   (runtime/resume-event!
                    "execution-1"
                    optimistic/continuity-restored-event
                    {:test true})]
               (is (= :completed (:status finished)))
               (is (= {:outcome outcome} (:result finished)))))))))))

(deftest already-installed-newer-canonical-wins-over-correlated-post-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           restore-count (atom 0)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [newer (canonical-node 10 "Live newer")]
         (dom/copy-canonical-into! target (.cloneNode newer true)))
       (with-redefs [continuity/restore! (fn [& _] (swap! restore-count inc))]
         (let [{settlement-root :root}
               (settlement-root "execution-1" :confirmed 8)
               result
               (browser/settle!
                (browser/settlement-from-root
                 settlement-root "execution-1"))]
           (is (= :completed (:status result)))
           (is (= {:outcome :superseded} (:result result)))
           (is (= 10 (dom/revision target)))
           (is (= "Live newer"
                  (.-textContent (.-firstElementChild target))))
           (is (= 0 @restore-count))))))))

(deftest equal-or-incomparable-installed-canonical-is-not-overwritten-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [existing (canonical-node 8 "Already installed")]
         (dom/copy-canonical-into! target (.cloneNode existing true)))
       (let [{settlement-root :root}
             (settlement-root "execution-1" :confirmed 8)
             result
             (browser/settle!
              (browser/settlement-from-root
               settlement-root "execution-1"))]
         (is (= :completed (:status result)))
         (is (= {:outcome :superseded} (:result result)))
         (is (= "Already installed"
                (.-textContent (.-firstElementChild target)))))))))

;; -----------------------------------------------------------------------------
;; Canonical supersession observation
;; -----------------------------------------------------------------------------

(deftest strictly-newer-canonical-supersedes-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           restore-count (atom 0)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [canonical (canonical-node 8 "Live won")]
         (dom/copy-canonical-into! target (.cloneNode canonical true))
         (with-redefs [continuity/restore! (fn [& _] (swap! restore-count inc))]
           (browser/canonical-installed! canonical)))
       (is (false? (runtime/active? "execution-1")))
       (is (= {:outcome :superseded}
              (:result (terminal "execution-1"))))
       (is (= 0 @restore-count))
       (is (false? (browser/scope-busy? scope-wire)))))))

(deftest canonical-supersession-requires-proof-test
  (doseq [[base-wire revision]
          [["i:7" 7]
           ["i:7" 6]
           ["s:base" "other"]]]
    (with-sandbox*
     (fn [root]
       (let [{:keys [source]}
             (command-dom! root {:base-revision-wire base-wire})]
         (install!)
         (browser/start! source {:execution-id "execution-1"})
         (browser/canonical-installed!
          (canonical-node scope-wire revision "Not provably newer" "request-1"))
         (is (runtime/active? "execution-1")))))))

(deftest canonical-other-scope-does-not-supersede-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (browser/canonical-installed!
        (canonical-node "other-scope" 100 "Other" "request-1"))
       (is (runtime/active? "execution-1"))))))

(deftest observe-canonical-tree-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           tree (element "div")
           canonical (append! tree (canonical-node 8 "Nested canonical"))]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (dom/copy-canonical-into! target (.cloneNode canonical true))
       (is (identical? tree (browser/observe-canonical-tree! tree)))
       (is (false? (runtime/active? "execution-1")))))))

;; -----------------------------------------------------------------------------
;; Source correlation / cleanup / abort
;; -----------------------------------------------------------------------------

(deftest source-correlation-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (is (= "execution-1" (browser/execution-for-source source)))
       (is (true? (browser/forget-source! source)))
       (is (nil? (browser/execution-for-source source)))))))

(deftest cleanup-source-only-after-terminal-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (browser/cleanup-source-if-terminal! source)
       (is (= "execution-1" (browser/execution-for-source source)))
       (let [canonical (canonical-node 8 "Canonical")]
         (dom/copy-canonical-into! target (.cloneNode canonical true))
         (browser/canonical-installed! canonical))
       (browser/cleanup-source-if-terminal! source)
       (is (nil? (browser/execution-for-source source)))))))

(deftest abort-cleans-process-owned-runtime-state-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (is (browser/scope-busy? scope-wire))
       (is (some? (browser/command-action "execution-1")))
       (is (= {:status :aborted
               :execution-id "execution-1"
               :reason :page-destroyed}
              (browser/abort! "execution-1" :page-destroyed)))
       (is (false? (browser/scope-busy? scope-wire)))
       (is (nil? (browser/command-action "execution-1")))
       (is (nil? (browser/execution-for-source source)))
       (is (false? (runtime/active? "execution-1")))))))

;; -----------------------------------------------------------------------------
;; Direct authority operations
;; -----------------------------------------------------------------------------

(deftest recover-snapshot-never-overwrites-canonical-test
  (with-sandbox*
   (fn [root]
     (let [target (append! root (canonical-node 8 "New canonical"))
           old (canonical-node 7 "Old")
           snapshot (dom/snapshot old)
           result
           (browser/recover-snapshot!
            {optimistic/execution-id-key "execution-1"
             target-key target
             target-id-key "request-1"
             snapshot-key snapshot})]
       (is (= :canonical-wins
              (get result optimistic/recovery-disposition-key)))
       (is (= "New canonical"
              (.-textContent (.-firstElementChild target))))))))

(deftest recover-snapshot-requires-owned-provisional-target-test
  (with-sandbox*
   (fn [root]
     (let [target (append! root (element "details" {:id "request-1"}))
           snapshot (dom/snapshot (canonical-node 7 "Old"))]
       (dom/set-attr! target browser/active-attr "other")
       (is (= :gesso.live.optimistic/recovery-authority-lost
              (:error/type
               (thrown-data
                #(browser/recover-snapshot!
                  {optimistic/execution-id-key "execution-1"
                   target-key target
                   target-id-key "request-1"
                   snapshot-key snapshot})))))))))

(deftest install-canonical-requires-explicit-authority-and-scope-test
  (let [target (element "details")]
    (is (= :gesso.live.optimistic/noncanonical-settlement
           (:error/type
            (thrown-data
             #(browser/install-canonical!
               {optimistic/execution-id-key "execution-1"
                optimistic/scope-key scope-wire
                target-key target
                optimistic/canonical-key (element "details")})))))
    (is (= :gesso.live.optimistic/canonical-scope-mismatch
           (:error/type
            (thrown-data
             #(browser/install-canonical!
               {optimistic/execution-id-key "execution-1"
                optimistic/scope-key scope-wire
                target-key target
                optimistic/canonical-key
                (canonical-node "other" 8 "Other" "request-1")})))))))

;; -----------------------------------------------------------------------------
;; Continuity barrier
;; -----------------------------------------------------------------------------

(deftest restore-continuity-emits-modeled-event-after-callback-test
  (let [root (element "div")
        callback (atom nil)
        calls (atom [])]
    (with-redefs
     [continuity/restore!
      (fn [_root complete!]
        (reset! callback complete!)
        nil)
      runtime/resume-event!
      (fn [execution-id event-id data]
        (swap! calls conj [execution-id event-id data])
        {:status :resumed})]
      (is (= {:optimistic/continuity-restore-scheduled? true}
             (browser/restore-continuity!
              {optimistic/execution-id-key "execution-1"
               optimistic/scope-key scope-wire
               root-key root})))
      (is (empty? @calls))
      (@callback {:root root :target nil :slot nil})
      (is (= "execution-1" (get-in @calls [0 0])))
      (is (= optimistic/continuity-restored-event
             (get-in @calls [0 1])))
      (is (= scope-wire (get-in @calls [0 2 :scope]))))))

;; -----------------------------------------------------------------------------
;; XHR integration
;; -----------------------------------------------------------------------------

(deftest settle-from-xhr-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)
           callback (atom nil)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [{settlement-fragment :root}
             (settlement-root "execution-1" :confirmed 8)
             xhr #js {:responseText (fragment-html settlement-fragment)}]
         (with-redefs
          [continuity/restore!
           (fn [_root complete!]
             (reset! callback complete!)
             nil)]
          (let [result (browser/settle-from-xhr! "execution-1" xhr)]
            (is (= :resumed (:status result)))
            (is (dom/canonical? target))
            (is (= 8 (dom/revision target)))
            (@callback {:root nil :target target :slot nil})
            (is (= {:outcome :confirmed}
                   (:result (terminal "execution-1")))))))))))

(deftest xhr-without-marker-is-not-semantic-success-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (is (nil?
            (browser/settle-from-xhr!
             "execution-1"
             #js {:responseText "<div>ordinary response</div>"})))
       (is (runtime/active? "execution-1"))))))

(deftest malformed-matching-xhr-is-visible-protocol-error-test
  (let [fragment (.createDocumentFragment js/document)]
    (.appendChild fragment
                  (settlement-marker "execution-1" :failed 8))
    (is (= :gesso.live.optimistic/missing-canonical
           (:error/type
            (thrown-data
             #(browser/settle-from-xhr!
               "execution-1"
               #js {:responseText (fragment-html fragment)})))))))

;; -----------------------------------------------------------------------------
;; Diagnostics and authority invariants
;; -----------------------------------------------------------------------------

(deftest diagnostics-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (let [diagnostics (browser/diagnostics)
             lock (get-in diagnostics [:active-scopes scope-wire])]
         (is (= {:execution-id "execution-1"
                 :target-id "request-1"}
                lock))
         (is (= 1 (:source-correlations diagnostics)))
         (is (= optimistic/protocol-name
                (get-in diagnostics [:browser-plan :name])))
         (is (not (contains? lock :target))))))))

(deftest projection-is-never-canonical-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source template]} (command-dom! root)
           projection (.-firstElementChild (.-content template))]
       ;; Even a malicious/mistaken template marker cannot grant authority.
       (dom/set-attr! projection protocol/canonical-attr "true")
       (install!)
       (browser/start! source {:execution-id "execution-1"})
       (is (false? (dom/canonical? target)))
       (is (= "execution-1" (dom/attr target browser/active-attr)))))))

(deftest command-payload-is-plain-protocol-data-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [source]} (command-dom! root)]
       (install!)
       (let [payload
             (:payload
              (:command
               (browser/start! source {:execution-id "execution-1"})))]
         (is (= transition-wire (:transition payload)))
         (is (= scope-wire (:scope payload)))
         (is (every? (fn [[_ value]] (not (dom/element? value))) payload)))))))

(deftest swap-none-command-still-projects-semantic-target-test
  (with-sandbox*
   (fn [root]
     (let [{:keys [target source]} (command-dom! root)]
       (dom/set-attr! source "hx-swap" "none")
       (dom/set-attr! source "hx-post" "/app/requests/request-1/claim")
       (install!)
       (let [result (browser/start! source {:execution-id "execution-1"})]
         (is (machine/suspended? (:execution result)))
         (is (= "execution-1" (dom/attr target browser/active-attr)))
         (is (= "Claiming…"
                (.-textContent (.-firstElementChild target)))))))))

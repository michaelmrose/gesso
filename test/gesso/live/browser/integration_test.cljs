(ns gesso.live.browser.integration-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.choreo.machine :as machine]
   [gesso.live.browser.choreo :as runtime]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.optimistic.choreo :as optimistic-choreo]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Integration fixture
;; -----------------------------------------------------------------------------

(def default-scope
  "e:[:request \"request-1\"]")

(def default-transition
  "request/claim")

(def default-target-id
  "request-1")

(def default-template-name
  "request-1-claim")

(defn- element
  ([tag]
   (element tag nil))
  ([tag attrs]
   (let [node
         (.createElement
          js/document
          tag)]
     (doseq [[attribute value]
             attrs
             :when (some? value)]
       (.setAttribute
        node
        (dom/attr-name attribute)
        (str value)))
     node)))

(defn- append!
  [parent child]
  (.appendChild
   parent
   child)
  child)

(defn- text!
  [node value]
  (set!
   (.-textContent node)
   value)
  node)

(defn- attr
  [node name]
  (dom/attr
   node
   name))

(defn- config-json
  [value]
  (.stringify
   js/JSON
   (clj->js value)))

(defn- sandbox
  []
  (let [root
        (element
         "div"
         {:data-gesso-browser-integration
          (str
           "fixture-"
           (random-uuid))})]
    (.appendChild
     (.-body js/document)
     root)
    root))

(defn- canonical-attrs
  [scope revision]
  {protocol/protocol-attr
   protocol/version

   protocol/canonical-attr
   "true"

   protocol/scope-attr
   scope

   protocol/revision-attr
   (protocol/revision->wire
    revision)})

(defn- projection-template
  [{:keys [scope
           template-name
           transition
           target-id
           projection-summary]
    :or {scope
         default-scope

         template-name
         default-template-name

         transition
         default-transition

         target-id
         default-target-id

         projection-summary
         "Claiming…"}}]
  (let [template
        (element
         "template"
         {protocol/protocol-attr
          protocol/version

          protocol/transition-attr
          transition

          protocol/template-attr
          template-name

          protocol/scope-attr
          scope

          protocol/projection-mode-attr
          "provisional"})

        projected
        (element
         "details"
         {:id
          target-id

          :data-request-card
          target-id})

        summary
        (append!
         projected
         (text!
          (element "summary")
          projection-summary))]

    (.appendChild
     (.-content template)
     projected)

    {:template
     template

     :projection
     projected

     :summary
     summary}))

(defn- request-fixture!
  ([sandbox-root]
   (request-fixture!
    sandbox-root
    nil))
  ([sandbox-root
    {:keys [scope
            transition
            target-id
            template-name
            base-revision
            consistency-token
            continuity-config
            canonical-summary
            projection-summary]
     :or {scope
          default-scope

          transition
          default-transition

          target-id
          default-target-id

          template-name
          default-template-name

          base-revision
          7

          consistency-token
          "visibility-token-7"

          continuity-config
          {:enabled true
           :boxes
           [{:type "details-open"}]
           :preserve
           {:inputs true}}

          canonical-summary
          "Unclaimed"

          projection-summary
          "Claiming…"}}]
   (let [continuity-root
         (append!
          sandbox-root
          (element
           "section"
           {continuity/continuity-attr
            "true"

            continuity/continuity-fragment-attr-key
            target-id

            continuity/continuity-config-attr-key
            (config-json
             continuity-config)}))

         target
         (append!
          continuity-root
          (element
           "details"
           (merge
            {:id
             target-id

             :data-request-card
             target-id}
            (canonical-attrs
             scope
             base-revision))))

         summary
         (append!
          target
          (text!
           (element "summary")
           canonical-summary))

         draft
         (append!
          target
          (element
           "input"
           {:id
            (str
             target-id
             "-draft")

            :type
            "text"

            :value
            "server-default"}))

         form
         (append!
          target
          (element
           "form"
           {:data-gesso-live-post
            "true"}))

         source
         (append!
          form
          (element
           "button"
           {protocol/protocol-attr
            protocol/version

            protocol/transition-attr
            transition

            protocol/template-attr
            template-name

            protocol/target-attr
            "closest [data-request-card]"

            protocol/scope-attr
            scope

            protocol/base-revision-attr
            (protocol/revision->wire
             base-revision)

            protocol/pending-label-attr
            "Claiming…"

            protocol/projection-mode-attr
            "provisional"

            :type
            "button"

            :hx-post
            (str
             "/requests/"
             target-id
             "/claim")

            :hx-swap
            "none"}))

         label
         (append!
          source
          (text!
           (element
            "span"
            {:data-gesso-button-label
             "true"})
           "Claim"))

         {:keys [template
                 projection]}
         (projection-template
          {:scope
           scope

           :template-name
           template-name

           :transition
           transition

           :target-id
           target-id

           :projection-summary
           projection-summary})]

     ;; Important server-side structural contract: request form and projection
     ;; template are siblings under the behavior-owning request card. A
     ;; projection is allowed to contain form markup of its own.
     (append!
      target
      template)

     {:sandbox
      sandbox-root

      :continuity-root
      continuity-root

      :target
      target

      :summary
      summary

      :draft
      draft

      :form
      form

      :source
      source

      :label
      label

      :template
      template

      :projection
      projection

      :scope
      scope

      :transition
      transition

      :target-id
      target-id

      :template-name
      template-name

      :base-revision
      base-revision

      :consistency-token
      consistency-token})))

(defn- settlement-html
  ([execution-id outcome revision]
   (settlement-html
    execution-id
    default-scope
    default-target-id
    outcome
    revision
    nil))
  ([execution-id
    scope
    target-id
    outcome
    revision
    reason]
   (let [container
         (element "div")

         marker
         (append!
          container
          (element
           "template"
           {protocol/settlement-attr
            "true"

            protocol/protocol-attr
            protocol/version

            protocol/execution-attr
            execution-id

            protocol/scope-attr
            scope

            protocol/outcome-attr
            (protocol/settlement-outcome->wire
             outcome)

            protocol/command-applied-attr
            (protocol/command-applied->wire
             (protocol/command-applied-for-outcome?
              outcome))

            protocol/revision-attr
            (protocol/revision->wire
             revision)

            protocol/reason-attr
            reason}))

         canonical
         (append!
          container
          (element
           "details"
           (merge
            {:id
             target-id

             :data-request-card
             target-id}
            (canonical-attrs
             scope
             revision))))

         _summary
         (append!
          canonical
          (text!
           (element "summary")
           (str
            "Canonical "
            (name outcome))))

         _draft
         (append!
          canonical
          (element
           "input"
           {:id
            (str
             target-id
             "-draft")

            :type
            "text"

            :value
            "server-new-default"}))]

     ;; Keep marker binding explicit: this function intentionally emits the
     ;; exact two-part settlement response expected by the optimistic protocol.
     (is (dom/template?
          marker))

     (.-innerHTML
      container))))

(defn- xhr
  ([status response-text]
   #js {:status
        status

        :responseText
        response-text})
  ([status]
   (xhr
    status
    "")))

(defn- event*
  ([type detail]
   (event*
    type
    detail
    nil))
  ([type detail target]
   (let [prevented?
         (atom false)

         event
         #js {}]

     (aset event
           "type"
           type)

     (aset event
           "detail"
           detail)

     (aset event
           "target"
           target)

     (aset event
           "preventDefault"
           (fn []
             (reset!
              prevented?
              true)))

     {:event
      event

      :prevented?
      prevented?})))

(defn- event
  ([type detail]
   (:event
    (event*
     type
     detail)))
  ([type detail target]
   (:event
    (event*
     type
     detail
     target))))

(defn- reset-runtime!
  []
  (runtime/reset-runtime!)

  (reset!
   optimistic/target-locks
   {})

  (reset!
   optimistic/executions-by-source
   {})

  (reset!
   optimistic/outgoing-actions
   {})

  (reset!
   core/pending-requests
   {})

  (reset!
   core/settled-response-sources
   #{})

  (reset!
   continuity/slots
   {})

  (continuity/register-built-in-boxes!)

  (optimistic/initialize!)

  true)

(defn- with-fixture*
  [f]
  (let [sandbox-root
        (sandbox)

        old-htmx
        (.-htmx
         js/window)]

    (reset-runtime!)

    (set!
     (.-htmx js/window)
     #js {:process
          (fn [_node]
            true)})

    (try
      (f sandbox-root)
      (finally
        (set!
         (.-htmx js/window)
         old-htmx)

        (reset-runtime!)

        (.remove
         sandbox-root)))))

(defn- layout-queue
  []
  (atom []))

(defn- queue-layout!
  [queue callback]
  (swap!
   queue
   conj
   callback)
  true)

(defn- flush-layout!
  [queue]
  (loop []
    (when-some [callback
                (first
                 @queue)]
      (swap!
       queue
       subvec
       1)

      (callback)

      ;; A restoration callback may itself schedule another layout callback.
      (recur)))
  true)

(defn- config-request!
  ([fixture]
   (config-request!
    fixture
    nil))
  ([{:keys [source
            consistency-token]}
    {:keys [headers]
     :or {headers
          #js {}}}]
   (when consistency-token
     (aset headers
           core/consistency-request-header
           consistency-token))

   (let [e
         (event
          "htmx:configRequest"
          #js {:elt
               source

               :headers
               headers})]

     (core/on-config-request!
      e)

     {:event
      e

      :headers
      headers

      :execution-id
      (aget
       headers
       core/optimistic-request-header)})))

(defn- before-request!
  ([fixture]
   (before-request!
    fixture
    nil))
  ([{:keys [source]}
    {:keys [target]}]
   (let [{:keys [event
                 prevented?]}
         (event*
          "htmx:beforeRequest"
          (cond->
              #js {:elt
                   source}

            target
            (doto
             (aset
              "target"
              target))))]

     (core/on-before-request!
      event)

     {:event
      event

      :prevented?
      prevented?})))

(defn- terminal
  [execution-id]
  (some
   #(when
      (= execution-id
         (:execution-id %))
      %)
   (runtime/terminal-summaries)))

(defn- active-command
  [execution-id]
  (optimistic/command-payload
   execution-id))

(defn- current-card
  [fixture]
  (.getElementById
   js/document
   (:target-id
    fixture)))

(defn- current-draft
  [fixture]
  (.getElementById
   js/document
   (str
    (:target-id fixture)
    "-draft")))


(defn- optimistic-source
  [scope transition template-name]
  (element
   "button"
   {protocol/protocol-attr
    protocol/version

    protocol/transition-attr
    transition

    protocol/template-attr
    template-name

    protocol/target-attr
    "closest [data-request-card]"

    protocol/scope-attr
    scope

    :type
    "button"}))

;; -----------------------------------------------------------------------------
;; Preflight -> start integration
;; -----------------------------------------------------------------------------

(deftest config-request-and-before-request-form-one-correlated-command-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id
                   headers]}
           (config-request!
            fixture)

           before
           (before-request!
            fixture)

           command
           (active-command
            execution-id)]

       (is (string?
            execution-id))

       (is (= execution-id
              (aget
               headers
               core/optimistic-request-header)))

       (is (false?
            @(:prevented?
              before)))

       (is (runtime/active?
            execution-id))

       (is (= execution-id
              (:execution-id
               command)))

       (is (= (:transition
               fixture)
              (:transition
               command)))

       (is (= (:scope
               fixture)
              (:scope
               command)))

       (is (= (:base-revision
               fixture)
              (:base-revision
               command)))

       (is (= (:consistency-token
               fixture)
              (:consistency-token
               command)))

       (is (= {}
              @core/pending-requests))))))

(deftest config-request-does-not-project-before-before-request-test
  (with-fixture*
   (fn [sandbox-root]
     (let [{:keys [target
                   source]
            :as fixture}
           (request-fixture!
            sandbox-root)]

       (config-request!
        fixture)

       (testing "request construction still sees the live source and canonical card"
         (is (dom/connected?
              source))

         (is (dom/canonical?
              target))

         (is (= "Unclaimed"
                (.-textContent
                 (.-firstElementChild
                  target))))

         (is (false?
              (optimistic/scope-busy?
               (:scope
                fixture)))))

       (before-request!
        fixture)

       (testing "projection begins only at the final pre-send boundary"
         (is (false?
              (dom/connected?
               source)))

         (is (false?
              (dom/canonical?
               target)))

         (is (= "Claiming…"
                (.-textContent
                 (.-firstElementChild
                  target))))

         (is (optimistic/scope-busy?
              (:scope
               fixture))))))))

;; -----------------------------------------------------------------------------
;; The current target-authority regression
;; -----------------------------------------------------------------------------

(deftest htmx-transport-target-cannot-override-semantic-optimistic-target-test
  (with-fixture*
   (fn [sandbox-root]
     (let [{:keys [target]
            :as fixture}
           (request-fixture!
            sandbox-root)

           incidental
           (append!
            sandbox-root
            (text!
             (element "details")
             "Incidental transport target"))

           {:keys [execution-id]}
           (config-request!
            fixture)

           attempted
           (try
             {:before
              (before-request!
               fixture
               {:target
                incidental})}
             (catch :default error
               {:error
                error}))]

       (is (nil?
            (:error
             attempted))
           "HTMX's transport target must not become optimistic target authority.")

       (when-not (:error
                  attempted)
         (testing "the descriptor-selected request card receives optimism"
           (is (= execution-id
                  (attr
                   target
                   optimistic/active-attr)))

           (is (= "Claiming…"
                  (.-textContent
                   (.-firstElementChild
                    target)))))

         (testing "the incidental transport target remains untouched"
           (is (= "Incidental transport target"
                  (.-textContent
                   incidental)))

           (is (nil?
                (attr
                 incidental
                 optimistic/active-attr))))

         (is (runtime/active?
              execution-id)))))))

(deftest incompatible-htmx-transport-target-cannot-cancel-valid-request-test
  (with-fixture*
   (fn [sandbox-root]
     (let [{:keys [target]
            :as fixture}
           (request-fixture!
            sandbox-root)

           incidental-button
           (append!
            sandbox-root
            (element "button"))

           {:keys [execution-id]}
           (config-request!
            fixture)

           attempted
           (try
             {:before
              (before-request!
               fixture
               {:target
                incidental-button})}
             (catch :default error
               {:error
                error}))]

       (is (nil?
            (:error
             attempted))
           "An unrelated HTMX button target must not trigger optimistic root-tag failure.")

       (when-not (:error
                  attempted)
         (is (= execution-id
                (attr
                 target
                 optimistic/active-attr)))

         (is (runtime/active?
              execution-id)))))))

;; -----------------------------------------------------------------------------
;; Successful authoritative settlement
;; -----------------------------------------------------------------------------

(deftest post-settlement-replaces-projection-with-canonical-and-completes-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        (is (runtime/active?
             execution-id))

        (let [response
              (xhr
               200
               (settlement-html
                execution-id
                :confirmed
                8))]

          (core/on-after-request!
           (event
            "htmx:afterRequest"
            #js {:elt
                 (:source
                  fixture)

                 :successful
                 true

                 :xhr
                 response})))

        (let [card
              (current-card
               fixture)]

          (testing "authoritative canonical DOM is installed immediately"
            (is (dom/canonical?
                 card))

            (is (= 8
                   (dom/revision
                    card)))

            (is (= "Canonical confirmed"
                   (.-textContent
                    (.-firstElementChild
                     card)))))

          (testing "the modeled continuity barrier still owns target authority"
            (is (runtime/active?
                 execution-id))

            (is (optimistic/scope-busy?
                 (:scope
                  fixture)))

            (is (pos?
                 (count
                  @layout))))

          (flush-layout!
           layout)

          (testing "completion follows the real continuity boundary"
            (is (false?
                 (runtime/active?
                  execution-id)))

            (is (false?
                 (optimistic/scope-busy?
                  (:scope
                   fixture))))

            (is (= {:outcome
                    :confirmed}
                   (:result
                    (terminal
                     execution-id))))

            (is (nil?
                 (optimistic/command-payload
                  execution-id))))))))))

(deftest reconciled-rejected-and-failed-settlements-preserve-semantic-outcome-test
  (doseq [outcome
          [:reconciled
           :rejected
           :failed]]

    (with-fixture*
     (fn [sandbox-root]
       (let [layout
             (layout-queue)

             fixture
             (request-fixture!
              sandbox-root)

             {:keys [execution-id]}
             (config-request!
              fixture)]

         (with-redefs
          [continuity/after-layout!
           #(queue-layout!
             layout
             %)]

          (before-request!
           fixture)

          (core/on-after-request!
           (event
            "htmx:afterRequest"
            #js {:elt
                 (:source
                  fixture)

                 :successful
                 true

                 :xhr
                 (xhr
                  200
                  (settlement-html
                   execution-id
                   outcome
                   8))}))

          (flush-layout!
           layout)

          (is (= {:outcome
                  outcome}
                 (:result
                  (terminal
                   execution-id)))
              (str
               "Expected semantic settlement outcome "
               outcome))))))))

;; -----------------------------------------------------------------------------
;; Browser-owned continuity across authoritative settlement
;; -----------------------------------------------------------------------------

(deftest user-input-and-details-state-survive-authoritative-settlement-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           {:keys [target
                   draft]
            :as fixture}
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (set!
        (.-open target)
        true)

       (set!
        (.-value draft)
        "typed locally")

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt
               (:source
                fixture)

               :successful
               true

               :xhr
               (xhr
                200
                (settlement-html
                 execution-id
                 :confirmed
                 8))}))

        (let [card
              (current-card
               fixture)

              new-draft
              (current-draft
               fixture)]

          (testing "server-owned structure is authoritative before local restoration"
            (is (= "Canonical confirmed"
                   (.-textContent
                    (.-firstElementChild
                     card))))

            (is (= "server-new-default"
                   (.-value
                    new-draft))))

          (flush-layout!
           layout)

          (testing "browser-owned interaction state is restored onto authoritative structure"
            (is (true?
                 (.-open
                  card)))

            (is (= "typed locally"
                   (.-value
                    new-draft))))

          (testing "restoration never rolls server content back"
            (is (= "Canonical confirmed"
                   (.-textContent
                    (.-firstElementChild
                     card)))))))))))

;; -----------------------------------------------------------------------------
;; Network/request failure recovery
;; -----------------------------------------------------------------------------

(deftest send-error-recovers-snapshot-and-completes-request-failed-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        (is (= "Claiming…"
               (.-textContent
                (.-firstElementChild
                 (current-card
                  fixture)))))

        (core/on-request-failed!
         (event
          "htmx:sendError"
          #js {:elt
               (:source
                fixture)}))

        (let [card
              (current-card
               fixture)]

          (testing "authorized snapshot is restored structurally first"
            (is (dom/canonical?
                 card))

            (is (= 7
                   (dom/revision
                    card)))

            (is (= "Unclaimed"
                   (.-textContent
                    (.-firstElementChild
                     card))))

            (is (runtime/active?
                 execution-id)))

          (flush-layout!
           layout)

          (testing "continuity completion releases protocol authority"
            (is (false?
                 (runtime/active?
                  execution-id)))

            (is (= {:outcome
                    :request-failed}
                   (:result
                    (terminal
                     execution-id))))

            (is (false?
                 (optimistic/scope-busy?
                  (:scope
                   fixture)))))))))))

(deftest timeout-event-path-recovers-through-same-snapshot-authority-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        ;; Deliver the same modeled timeout event the browser-owned timer would
        ;; emit, without waiting 15 seconds in the test suite.
        (runtime/resume-event!
         execution-id
         optimistic-choreo/timeout-event
         {optimistic-choreo/reason-key
          :settlement-timeout})

        (is (= "Unclaimed"
               (.-textContent
                (.-firstElementChild
                 (current-card
                  fixture)))))

        (flush-layout!
         layout)

        (is (= {:outcome
                :timeout}
               (:result
                (terminal
                 execution-id)))))))))

;; -----------------------------------------------------------------------------
;; HTTP success alone is never semantic success
;; -----------------------------------------------------------------------------

(deftest successful-http-without-settlement-stays-pending-for-modeled-timeout-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)

           errors
           (atom [])]

       (.addEventListener
        (.-documentElement js/document)
        "gesso:optimistic:error"
        (fn [e]
          (swap!
           errors
           conj
           (.-detail e))))

       (before-request!
        fixture)

       (core/on-after-request!
        (event
         "htmx:afterRequest"
         #js {:elt
              (:source
               fixture)

              :successful
              true

              :xhr
              (xhr
               204
               "")}))

       (is (runtime/active?
            execution-id))

       (is (optimistic/scope-busy?
            (:scope
             fixture)))

       (is (= "Claiming…"
              (.-textContent
               (.-firstElementChild
                (current-card
                 fixture)))))

       (is (= 1
              (count
               @errors)))

       (is (= "after-request"
              (aget
               (first
                @errors)
               "phase")))

       (is (= "missing-settlement"
              (aget
               (first
                @errors)
               "reason")))))))

;; -----------------------------------------------------------------------------
;; HTTP failure with authoritative settlement still follows settlement
;; -----------------------------------------------------------------------------

(deftest semantic-settlement-outranks-http-success-flag-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        ;; Transport status may be non-2xx while still carrying the explicit
        ;; semantic settlement protocol. Core must consume settlement first.
        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt
               (:source
                fixture)

               :successful
               false

               :xhr
               (xhr
                409
                (settlement-html
                 execution-id
                 :rejected
                 8))}))

        (flush-layout!
         layout)

        (is (= {:outcome
                :rejected}
               (:result
                (terminal
                 execution-id))))

        (is (= "Canonical rejected"
               (.-textContent
                (.-firstElementChild
                 (current-card
                  fixture))))))))))

;; -----------------------------------------------------------------------------
;; Live/SSE canonical supersession
;; -----------------------------------------------------------------------------

(deftest newer-live-canonical-supersedes-pending-post-without-snapshot-rollback-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (let [card
             (current-card
              fixture)]

         ;; Model the externally installed authoritative DOM that an HTMX/SSE
         ;; Live refetch would have produced.
         (dom/copy-canonical-into!
          card
          (let [canonical
                (element
                 "details"
                 (merge
                  {:id
                   (:target-id
                    fixture)

                   :data-request-card
                   (:target-id
                    fixture)}
                  (canonical-attrs
                   (:scope
                    fixture)
                   8)))]

            (append!
             canonical
             (text!
              (element "summary")
              "Live canonical"))

            canonical))

         (core/on-sse-message!
          (event
           "htmx:sseMessage"
           #js {:target
                card}))

         (is (= "Live canonical"
                (.-textContent
                 (.-firstElementChild
                  card))))

         (is (= 8
                (dom/revision
                 card)))

         (is (false?
              (runtime/active?
               execution-id)))

         (is (= {:outcome
                 :superseded}
                (:result
                 (terminal
                  execution-id))))

         (is (false?
              (optimistic/scope-busy?
               (:scope
                fixture)))))))))

(deftest equal-or-older-live-canonical-does-not-prove-supersession-test
  (doseq [revision
          [6
           7]]

    (with-fixture*
     (fn [sandbox-root]
       (let [fixture
             (request-fixture!
              sandbox-root)

             {:keys [execution-id]}
             (config-request!
              fixture)]

         (before-request!
          fixture)

         (let [card
               (current-card
                fixture)

               canonical
               (element
                "details"
                (merge
                 {:id
                  (:target-id
                   fixture)

                  :data-request-card
                  (:target-id
                   fixture)}
                 (canonical-attrs
                  (:scope
                   fixture)
                  revision)))]

           (append!
            canonical
            (text!
             (element "summary")
             (str
              "Canonical "
              revision)))

           ;; Observation is intentionally separate from installation. The
           ;; current target remains provisional so this test isolates revision
           ;; proof rather than installation policy.
           (core/on-sse-message!
            (event
             "htmx:sseMessage"
             #js {:target
                  canonical}))

           (is (runtime/active?
                execution-id)
               (str
                "Revision "
                revision
                " must not supersede base revision 7."))))))))

;; -----------------------------------------------------------------------------
;; beforeSwap semantic consumption
;; -----------------------------------------------------------------------------

(deftest settlement-consumed-at-before-swap-is-not-recaptured-as-ordinary-continuity-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)

           response
           (xhr
            200
            (settlement-html
             execution-id
             :confirmed
             8))]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        (let [slot-before
              (continuity/captured-slot
               (:continuity-root
                fixture))]

          (is (some?
               slot-before))

          (core/on-before-swap!
           (event
            "htmx:beforeSwap"
            #js {:elt
                 (:source
                  fixture)

                 :xhr
                 response}))

          (testing "semantic delivery preserves the optimistic execution's original slot"
            (is (identical?
                 slot-before
                 (continuity/captured-slot
                  (:continuity-root
                   fixture)))))

          (testing "the response is remembered as already semantically consumed"
            (is (= 1
                   (count
                    @core/settled-response-sources))))

          (flush-layout!
           layout)

          (is (= {:outcome
                  :confirmed}
                 (:result
                  (terminal
                   execution-id))))))))))

;; -----------------------------------------------------------------------------
;; OOB semantic settlement
;; -----------------------------------------------------------------------------

(deftest oob-before-swap-can-deliver-the-same-authoritative-settlement-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (before-request!
         fixture)

        (core/on-oob-before-swap!
         (event
          "htmx:oobBeforeSwap"
          #js {:elt
               (:source
                fixture)

               :xhr
               (xhr
                200
                (settlement-html
                 execution-id
                 :confirmed
                 8))}))

        (flush-layout!
         layout)

        (is (= {:outcome
                :confirmed}
               (:result
                (terminal
                 execution-id))))

        (is (dom/canonical?
             (current-card
              fixture))))))))

;; -----------------------------------------------------------------------------
;; Duplicate activation across one logical scope
;; -----------------------------------------------------------------------------

(deftest second-click-is-blocked-while-first-command-owns-scope-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (is (runtime/active?
            execution-id))

       ;; The original source is detached after projection, but an equivalent
       ;; newly rendered action for the same scope must also be blocked.
       (let [new-source
             (optimistic-source
              (:scope
               fixture)
              (:transition
               fixture)
              (:template-name
               fixture))

             {:keys [event
                     prevented?]}
             (event*
              "click"
              #js {}
              new-source)]

         (core/on-click-capture!
          event)

         (is (true?
              @prevented?)))))))

;; -----------------------------------------------------------------------------
;; Different logical scopes remain independent
;; -----------------------------------------------------------------------------

(deftest two-request-cards-can-run-concurrently-test
  (with-fixture*
   (fn [sandbox-root]
     (let [first
           (request-fixture!
            sandbox-root
            {:scope
             "scope-1"

             :target-id
             "request-1"

             :template-name
             "request-1-claim"

             :consistency-token
             "token-1"})

           second
           (request-fixture!
            sandbox-root
            {:scope
             "scope-2"

             :target-id
             "request-2"

             :template-name
             "request-2-claim"

             :consistency-token
             "token-2"})

           first-id
           (:execution-id
            (config-request!
             first))

           second-id
           (:execution-id
            (config-request!
             second))]

       (before-request!
        first)

       (before-request!
        second)

       (is (= #{first-id
                second-id}
              (runtime/active-execution-ids)))

       (is (optimistic/scope-busy?
            "scope-1"))

       (is (optimistic/scope-busy?
            "scope-2"))

       (is (= "Claiming…"
              (.-textContent
               (.-firstElementChild
                (current-card
                 first)))))

       (is (= "Claiming…"
              (.-textContent
               (.-firstElementChild
                (current-card
                 second)))))))))

;; -----------------------------------------------------------------------------
;; Same logical scope is single-flight even across different DOM roots
;; -----------------------------------------------------------------------------

(deftest same-scope-cannot-start-twice-through-two-rendered-actions-test
  (with-fixture*
   (fn [sandbox-root]
     (let [first
           (request-fixture!
            sandbox-root
            {:scope
             "shared-scope"

             :target-id
             "request-1"

             :template-name
             "request-1-claim"})

           second
           (request-fixture!
            sandbox-root
            {:scope
             "shared-scope"

             :target-id
             "request-2"

             :template-name
             "request-2-claim"})

           first-id
           (:execution-id
            (config-request!
             first))]

       (before-request!
        first)

       (is (optimistic/scope-busy?
            "shared-scope"))

       (is (= first-id
              (:execution-id
               (optimistic/current-lock
                "shared-scope"))))

       (let [second-id
             (:execution-id
              (config-request!
               second))

             attempted
             (try
               {:before
                (before-request!
                 second)}
               (catch :default error
                 {:error
                  error}))]

         (is (some?
              (:error
               attempted))
             (pr-str
              {:first-id first-id
               :second-id second-id
               :lock (optimistic/current-lock
                      "shared-scope")
               :attempted attempted}))

         (is (= :gesso.live.optimistic/target-busy
                (:error/type
                 (ex-data
                  (ex-cause
                   (:error
                    attempted))))))

         (is (runtime/active?
              first-id))

         (is (false?
              (runtime/active?
               second-id)))

         (is (= first-id
                (:execution-id
                 (optimistic/current-lock
                  "shared-scope")))))))))

;; -----------------------------------------------------------------------------
;; Correlation header and choreography command identity stay identical
;; -----------------------------------------------------------------------------

(deftest request-header-execution-id-is-the-choreography-execution-id-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [headers
                   execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (is (= execution-id
              (aget
               headers
               protocol/execution-header-name)))

       (is (= execution-id
              (:execution-id
               (active-command
                execution-id))))

       (is (= execution-id
              (optimistic/execution-for-source
               (:source
                fixture))))))))

;; -----------------------------------------------------------------------------
;; Consistency token is request context, not DOM state
;; -----------------------------------------------------------------------------

(deftest consistency-token-reaches-command-without-being-written-into-projection-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root
            {:consistency-token
             "token-secret-ish"})

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (is (= "token-secret-ish"
              (:consistency-token
               (active-command
                execution-id))))

       (is (nil?
            (attr
             (current-card
              fixture)
             core/consistency-request-header)))

       (is (nil?
            (.querySelector
             (current-card
              fixture)
             "[data-consistency-token]")))))))

;; -----------------------------------------------------------------------------
;; Source pending state is restored after canonical supersession
;; -----------------------------------------------------------------------------

(deftest detached-source-pending-state-is-cleaned-after-live-supersession-test
  (with-fixture*
   (fn [sandbox-root]
     (let [{:keys [source
                   label]
            :as fixture}
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (is (= "true"
              (attr
               source
               optimistic/pending-source-attr)))

       (is (= "Claiming…"
              (.-textContent
               label)))

       (let [card
             (current-card
              fixture)

             canonical
             (element
              "details"
              (merge
               {:id
                (:target-id
                 fixture)

                :data-request-card
                (:target-id
                 fixture)}
               (canonical-attrs
                (:scope
                 fixture)
                8)))]

         (append!
          canonical
          (text!
           (element "summary")
           "Canonical elsewhere"))

         (dom/copy-canonical-into!
          card
          canonical)

         (core/on-sse-message!
          (event
           "htmx:sseMessage"
           #js {:target
                card})))

       (is (false?
            (runtime/active?
             execution-id)))

       (is (nil?
            (attr
             source
             optimistic/pending-source-attr)))

       (is (false?
            (.-disabled source)))

       (testing "detached old source markup is not cosmetically rewritten"
         (is (false?
              (dom/connected?
               label))))))))

;; -----------------------------------------------------------------------------
;; Request failure after newer canonical state is installed never rolls back
;; -----------------------------------------------------------------------------

(deftest request-failure-after-live-canonical-installation-does-not-restore-stale-snapshot-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (let [card
             (current-card
              fixture)

             canonical
             (element
              "details"
              (merge
               {:id
                (:target-id
                 fixture)

                :data-request-card
                (:target-id
                 fixture)}
               (canonical-attrs
                (:scope
                 fixture)
                8)))]

         (append!
          canonical
          (text!
           (element "summary")
           "Authoritative already"))

         ;; Do not notify canonical observer yet: request-failure recovery itself
         ;; must notice explicit canonical authority and refuse rollback.
         (dom/copy-canonical-into!
          card
          canonical)

         (core/on-request-failed!
          (event
           "htmx:sendError"
           #js {:elt
                (:source
                 fixture)}))

         (is (= "Authoritative already"
                (.-textContent
                 (.-firstElementChild
                  card))))

         (is (= 8
                (dom/revision
                 card)))

         (is (= {:outcome
                 :superseded}
                (:result
                 (terminal
                  execution-id)))))))))

;; -----------------------------------------------------------------------------
;; A second beforeRequest without configRequest is blocked from the network
;; -----------------------------------------------------------------------------

(deftest preflight-is-single-use-in-real-runtime-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)]

       (config-request!
        fixture)

       (before-request!
        fixture)

       (let [{:keys [event
                     prevented?]}
             (event*
              "htmx:beforeRequest"
              #js {:elt
                   (:source
                    fixture)})

             error
             (try
               (core/on-before-request!
                event)
               nil
               (catch :default error
                 error))]

         (is (= :gesso.live.browser.core/missing-optimistic-preflight
                (:error/type
                 (ex-data
                  error))))

         (is (true?
              @prevented?)))))))

;; -----------------------------------------------------------------------------
;; Ordinary authoritative Live delivery outside optimism still preserves continuity
;; -----------------------------------------------------------------------------

(deftest nonoptimistic-sse-cycle-preserves-input-state-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           {:keys [continuity-root
                   target
                   draft]
            :as fixture}
           (request-fixture!
            sandbox-root)]

       (set!
        (.-value draft)
        "typed locally")

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (core/on-sse-before-message!
         (event
          "htmx:sseBeforeMessage"
          #js {:target
               target}))

        ;; Simulate ordinary HTMX SSE replacement of the target with a newer
        ;; canonical server render.
        (let [replacement
              (element
               "details"
               (merge
                {:id
                 (:target-id
                  fixture)

                 :data-request-card
                 (:target-id
                  fixture)}
                (canonical-attrs
                 (:scope
                  fixture)
                 8)))

              replacement-summary
              (append!
               replacement
               (text!
                (element "summary")
                "Live updated"))

              replacement-draft
              (append!
               replacement
               (element
                "input"
                {:id
                 (str
                  (:target-id
                   fixture)
                  "-draft")

                 :type
                 "text"

                 :value
                 "server replacement"}))]

          (.replaceWith
           target
           replacement)

          (core/on-sse-message!
           (event
            "htmx:sseMessage"
            #js {:target
                 replacement}))

          (flush-layout!
           layout)

          (is (= "Live updated"
                 (.-textContent
                  replacement-summary)))

          (is (= "typed locally"
                 (.-value
                  replacement-draft)))

          (is (nil?
               (continuity/captured-slot
                continuity-root)))))))))

;; -----------------------------------------------------------------------------
;; Browser diagnostics agree across adapter, process runtime, and policy runtime
;; -----------------------------------------------------------------------------

(deftest runtime-diagnostics-agree-during-one-pending-command-test
  (with-fixture*
   (fn [sandbox-root]
     (let [fixture
           (request-fixture!
            sandbox-root)

           {:keys [execution-id]}
           (config-request!
            fixture)]

       (before-request!
        fixture)

       (let [state
             (core/runtime-state)]

         (is (= core/runtime-version
                (:version
                 state)))

         (is (= protocol/version
                (:protocol-version
                 state)))

         (is (= 1
                (get-in
                 state
                 [:choreo
                  :active-count])))

         (is (= execution-id
                (get-in
                 state
                 [:optimistic
                  :active-scopes
                  (:scope
                   fixture)
                  :execution-id])))

         (is (= 1
                (get-in
                 state
                 [:optimistic
                  :source-correlations])))

         (is (= 1
                (count
                 (get-in
                  state
                  [:continuity
                   :slots])))))))))

;; -----------------------------------------------------------------------------
;; Final full-path smoke test
;; -----------------------------------------------------------------------------

(deftest complete-request-lifecycle-smoke-test
  (with-fixture*
   (fn [sandbox-root]
     (let [layout
           (layout-queue)

           {:keys [target
                   draft]
            :as fixture}
           (request-fixture!
            sandbox-root)

           _typed
           (set!
            (.-value draft)
            "draft survives")

           {:keys [execution-id
                   headers]}
           (config-request!
            fixture)]

       (with-redefs
        [continuity/after-layout!
         #(queue-layout!
           layout
           %)]

        (testing "preflight"
          (is (= execution-id
                 (aget
                  headers
                  protocol/execution-header-name)))

          (is (dom/canonical?
               target)))

        (testing "optimistic start"
          (before-request!
           fixture)

          (is (= "Claiming…"
                 (.-textContent
                  (.-firstElementChild
                   (current-card
                    fixture)))))

          (is (= execution-id
                 (:execution-id
                  (active-command
                   execution-id)))))

        (testing "authoritative response"
          (core/on-after-request!
           (event
            "htmx:afterRequest"
            #js {:elt
                 (:source
                  fixture)

                 :successful
                 true

                 :xhr
                 (xhr
                  200
                  (settlement-html
                   execution-id
                   :confirmed
                   8))}))

          (is (dom/canonical?
               (current-card
                fixture)))

          (is (= "Canonical confirmed"
                 (.-textContent
                  (.-firstElementChild
                   (current-card
                    fixture))))))

        (testing "continuity barrier"
          (flush-layout!
           layout)

          (is (= "draft survives"
                 (.-value
                  (current-draft
                   fixture)))))

        (testing "terminal cleanup"
          (is (false?
               (runtime/active?
                execution-id)))

          (is (= {:outcome
                  :confirmed}
                 (:result
                  (terminal
                   execution-id))))

          (is (false?
               (optimistic/scope-busy?
                (:scope
                 fixture))))

          (is (nil?
               (optimistic/command-payload
                execution-id)))))))))

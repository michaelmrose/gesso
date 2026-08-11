(ns gesso.live.browser.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.choreo :as choreo-runtime]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.dom :as dom]
   [gesso.live.browser.optimistic :as optimistic]
   [gesso.live.optimistic.protocol :as protocol]))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

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

(defn- optimistic-source
  ([]
   (optimistic-source
    nil))
  ([attrs]
   (element
    "button"
    (merge
     {protocol/protocol-attr
      protocol/version

      protocol/transition-attr
      "request/claim"

      protocol/template-attr
      "request-claim"

      protocol/target-attr
      "closest [data-request-card]"

      protocol/scope-attr
      "request-1"}
     attrs))))

(defn- event*
  ([type]
   (event*
    type
    #js {}
    nil))
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

     {:event event
      :prevented? prevented?})))

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

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- thrown-data
  [f]
  (some->
   (thrown f)
   ex-data))

(defn- reset-core-state!
  []
  (reset!
   core/pending-requests
   {})
  (reset!
   core/settled-response-sources
   #{})
  true)

(defn- with-core-state*
  [f]
  (let [old-gesso-live
        (aget js/window
              "gessoLive")]
    (reset-core-state!)
    (try
      (f)
      (finally
        (reset-core-state!)
        (aset js/window
              "gessoLive"
              old-gesso-live)))))

(defn- only-pending-request
  []
  (let [values
        (vals
         @core/pending-requests)]
    (is (= 1
           (count values)))
    (first values)))

(defn- headers
  [event]
  (some->
   (.-detail event)
   (aget "headers")))

(defn- header
  [event name]
  (some->
   (headers event)
   (aget name)))

(defn- js-headers
  [& keyvals]
  (let [headers
        #js {}]
    (doseq [[name value]
            (partition 2 keyvals)]
      (aset headers
            name
            value))
    headers))

;; -----------------------------------------------------------------------------
;; Runtime identity
;; -----------------------------------------------------------------------------

(deftest runtime-identity-test
  (is (= "2.0.0"
         core/runtime-version))

  (is (= protocol/execution-header-name
         core/optimistic-request-header))

  (is (= "X-Gesso-Live-Consistency-Token"
         core/consistency-request-header)))

;; -----------------------------------------------------------------------------
;; Generic event adaptation
;; -----------------------------------------------------------------------------

(deftest event-detail-test
  (let [detail
        #js {:value 42}

        e
        (event
         "test"
         detail)]

    (is (identical?
         detail
         (core/event-detail
          e)))))

(deftest detail-field-test
  (let [target
        (element "div")

        e
        (event
         "test"
         #js {:target
              target})]

    (is (identical?
         target
         (core/detail-field
          e
          "target")))

    (is (nil?
         (core/detail-field
          e
          "missing")))))

(deftest event-source-delegates-to-continuity-test
  (let [source
        (element "button")

        e
        (event
         "test"
         #js {})]

    (with-redefs
     [continuity/event-source
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        source)]

      (is (identical?
           source
           (core/event-source
            e))))))

(deftest event-target-precedence-test
  (let [detail-target
        (element "div")

        detail-elt
        (element "section")

        event-target
        (element "button")]

    (testing "HTMX detail target wins"
      (is (identical?
           detail-target
           (core/event-target
            (event
             "test"
             #js {:target detail-target
                  :elt detail-elt}
             event-target)))))

    (testing "detail elt is the next fallback"
      (is (identical?
           detail-elt
           (core/event-target
            (event
             "test"
             #js {:elt detail-elt}
             event-target)))))

    (testing "native event target is the final fallback"
      (is (identical?
           event-target
           (core/event-target
            (event
             "test"
             #js {}
             event-target)))))))

(deftest event-target-ignores-non-elements-test
  (let [event-target
        (element "button")]

    (is (identical?
         event-target
         (core/event-target
          (event
           "test"
           #js {:target "#selector"
                :elt "not-an-element"}
           event-target))))

    (is (nil?
         (core/event-target
          (event
           "test"
           #js {:target "#selector"
                :elt "not-an-element"}
           nil))))))

(deftest optimistic-source-from-event-prefers-request-source-test
  (let [source
        (optimistic-source)

        nested
        (append!
         source
         (element "span"))

        unrelated
        (element "div")

        e
        (event
         "test"
         #js {:elt nested
              :target unrelated}
         unrelated)]

    (is (identical?
         source
         (core/optimistic-source-from-event
          e)))))

(deftest optimistic-source-from-event-falls-back-across-event-elements-test
  (let [source
        (optimistic-source)

        nested
        (append!
         source
         (element "span"))

        e
        (event
         "test"
         #js {:target nested})]

    (with-redefs
     [continuity/event-source
      (fn [_event]
        nil)]

      (is (identical?
           source
           (core/optimistic-source-from-event
            e))))))

(deftest optimistic-source-from-event-may-be-absent-test
  (is (nil?
       (core/optimistic-source-from-event
        (event
         "test"
         #js {}
         (element "div"))))))

(deftest prevent-event-test
  (let [{:keys [event
                prevented?]}
        (event*
         "test")]

    (is (false?
         (core/prevent-event!
          event)))

    (is (true?
         @prevented?))))

(deftest prevent-event-without-prevent-default-is-harmless-test
  (let [e
        #js {:type
             "test"

             :detail
             #js {}}]

    (is (false?
         (core/prevent-event!
          e)))))

;; -----------------------------------------------------------------------------
;; Request status / failure classification
;; -----------------------------------------------------------------------------

(deftest request-successful-explicit-boolean-wins-test
  (doseq [[explicit status expected]
          [[true 500 true]
           [false 200 false]]]

    (is (= expected
           (core/request-successful?
            (event
             "htmx:afterRequest"
             #js {:successful explicit
                  :xhr
                  #js {:status status}}))))))

(deftest request-successful-falls-back-to-http-status-test
  (doseq [[status expected]
          [[199 false]
           [200 true]
           [204 true]
           [299 true]
           [300 true]
           [399 true]
           [400 false]
           [500 false]]]

    (is (= expected
           (boolean
            (core/request-successful?
             (event
              "htmx:afterRequest"
              #js {:xhr
                   #js {:status status}}))))
        (str
         "Unexpected success classification for status "
         status))))

(deftest request-successful-without-status-is-false-test
  (is (false?
       (boolean
        (core/request-successful?
         (event
          "htmx:afterRequest"
          #js {}))))))

(deftest failure-reason-test
  (doseq [[event-type expected-name]
          [["htmx:responseError"
            "response-error"]

           ["htmx:sendError"
            "send-error"]

           ["htmx:timeout"
            "timeout"]

           ["htmx:abort"
            "aborted"]

           ["anything-else"
            "request-failed"]]]

    (let [reason
          (core/failure-reason
           (event
            event-type
            #js {}))]

      (is (= "gesso.live.optimistic.request"
             (namespace
              reason)))

      (is (= expected-name
             (name
              reason))))))

(deftest xhr-from-event-test
  (let [xhr
        #js {:status
             200}

        e
        (event
         "test"
         #js {:xhr xhr})]

    (is (identical?
         xhr
         (core/xhr-from-event
          e)))))

;; -----------------------------------------------------------------------------
;; configRequest preflight
;; -----------------------------------------------------------------------------

(deftest config-request-allocates-execution-and-header-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           detail
           #js {:elt
                source}

           e
           (event
            "htmx:configRequest"
            detail)]

       (with-redefs
        [optimistic/execution-id
         (fn []
           "execution-1")]

        (is (true?
             (core/on-config-request!
              e)))

        (is (= "execution-1"
               (header
                e
                core/optimistic-request-header)))

        (is (= {:execution-id
                "execution-1"

                :consistency-token
                nil}
               (only-pending-request))))))))

(deftest config-request-preserves-existing-headers-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           existing
           #js {"Existing"
                "value"}

           e
           (event
            "htmx:configRequest"
            #js {:elt source
                 :headers existing})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")]

        (core/on-config-request!
         e)

        (is (identical?
             existing
             (headers e)))

        (is (= "value"
               (aget existing
                     "Existing")))

        (is (= "execution-1"
               (aget existing
                     core/optimistic-request-header))))))))

(deftest config-request-captures-consistency-token-case-insensitively-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           e
           (event
            "htmx:configRequest"
            #js {:elt
                 source

                 :headers
                 #js {"x-gesso-live-consistency-token"
                      "token-123"}})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")]

        (core/on-config-request!
         e)

        (is (= {:execution-id
                "execution-1"

                :consistency-token
                "token-123"}
               (only-pending-request))))))))

(deftest repeated-config-request-reuses-preflight-correlation-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           first-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source

                 :headers
                 (js-headers
                  core/consistency-request-header
                  "token-1")})

           second-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source

                 :headers
                 (js-headers
                  core/consistency-request-header
                  "token-2")})

           ids
           (atom
            ["execution-1"
             "execution-2"])]

       (with-redefs
        [optimistic/execution-id
         (fn []
           (let [id
                 (first @ids)]
             (swap!
              ids
              subvec
              1)
             id))]

        (core/on-config-request!
         first-event)

        (core/on-config-request!
         second-event)

        (is (= "execution-1"
               (header
                second-event
                core/optimistic-request-header)))

        (is (= {:execution-id
                "execution-1"

                :consistency-token
                "token-1"}
               (only-pending-request)))

        (testing "the second generated id was never needed"
          (is (= ["execution-2"]
                 @ids))))))))

(deftest config-request-nonoptimistic-source-is-no-op-test
  (with-core-state*
   (fn []
     (let [e
           (event
            "htmx:configRequest"
            #js {:elt
                 (element "button")})]

       (is (true?
            (core/on-config-request!
             e)))

       (is (= {}
              @core/pending-requests))

       (is (nil?
            (headers e)))))))

;; -----------------------------------------------------------------------------
;; beforeRequest start boundary
;; -----------------------------------------------------------------------------

(deftest before-request-starts-correlated-optimistic-execution-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           config-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source

                 :headers
                 (js-headers
                  core/consistency-request-header
                  "token-123")})

           before
           (event*
            "htmx:beforeRequest"
            #js {:elt
                 source})

           seen
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [actual-source opts]
           (reset!
            seen
            [actual-source
             opts])
           {:execution-id
            (:execution-id opts)})]

        (core/on-config-request!
         config-event)

        (is (true?
             (core/on-before-request!
              (:event before))))

        (is (identical?
             source
             (first
              @seen)))

        (is (= "execution-1"
               (get-in
                @seen
                [1
                 :execution-id])))

        (is (= "token-123"
               (get-in
                @seen
                [1
                 :consistency-token])))

        (is (false?
             @(:prevented? before)))

        (is (= {}
              @core/pending-requests)))))))

(deftest before-request-does-not-forward-htmx-transport-target-as-optimistic-authority-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           semantic-card
           (element
            "details"
            {:data-request-card
             "request-1"})

           _source-parent
           (append!
            semantic-card
            source)

           transport-target
           (element
            "div"
            {:id
             "htmx-transport-target"})

           config-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source})

           before-event
           (event
            "htmx:beforeRequest"
            #js {:elt
                 source

                 ;; HTMX's swap/transport target is incidental to the
                 ;; optimistic choreography's semantic target selector.
                 :target
                 transport-target})

           seen-opts
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [_source opts]
           (reset!
            seen-opts
            opts)
           {:execution-id
            "execution-1"})]

        (core/on-config-request!
         config-event)

        (core/on-before-request!
         before-event)

        (testing "core forwards correlation but not semantic target authority"
          (is (= "execution-1"
                 (:execution-id
                  @seen-opts)))

          ;; This assertion intentionally fails against the current
          ;; implementation. The optimistic adapter owns semantic target
          ;; resolution from data-gesso-optimistic-target.
          (is (not
               (contains?
                @seen-opts
                :target))))

        (testing "the descriptor still has all information required to resolve the card"
          (is (= "closest [data-request-card]"
                 (dom/attr
                  source
                  protocol/target-attr)))))))))

(deftest before-request-missing-preflight-prevents-network-send-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           root
           (append!
            (element "section")
            source)

           before
           (event*
            "htmx:beforeRequest"
            #js {:elt
                 source})

           emitted
           (atom nil)]

       (with-redefs
        [continuity/root
         (fn [actual-source]
           (is (identical?
                source
                actual-source))
           root)

         optimistic/emit!
         (fn [actual-root name detail]
           (reset!
            emitted
            [actual-root
             name
             detail])
           detail)]

        (let [error
              (thrown
               #(core/on-before-request!
                 (:event before)))]

          (is (= :gesso.live.browser.core/missing-optimistic-preflight
                 (:error/type
                  (ex-data error))))

          (is (true?
               @(:prevented? before)))

          (is (identical?
               root
               (first
                @emitted)))

          (is (= "error"
                 (second
                  @emitted)))

          (is (= "before-request"
                 (aget
                  (nth
                   @emitted
                   2)
                  "phase")))

          (is (= "missing-config-request"
                 (aget
                  (nth
                   @emitted
                   2)
                  "reason")))))))))

(deftest before-request-start-failure-prevents-send-and-cleans-preflight-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           config-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source})

           before
           (event*
            "htmx:beforeRequest"
            #js {:elt
                 source})

           cause
           (js/Error.
            "optimistic startup failed")

           emitted
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [& _]
           (throw
            cause))

         optimistic/emit!
         (fn [root name detail]
           (reset!
            emitted
            [root
             name
             detail])
           detail)]

        (core/on-config-request!
         config-event)

        (let [error
              (thrown
               #(core/on-before-request!
                 (:event before)))]

          (is (identical?
               cause
               error))

          (is (true?
               @(:prevented? before)))

          (is (= {}
                 @core/pending-requests))

          (is (= "error"
                 (second
                  @emitted)))

          (is (= "before-request"
                 (aget
                  (nth
                   @emitted
                   2)
                  "phase")))

          (is (= "execution-1"
                 (aget
                  (nth
                   @emitted
                   2)
                  "executionId")))))))))

(deftest before-request-nonoptimistic-source-is-no-op-test
  (with-core-state*
   (fn []
     (let [{:keys [event
                   prevented?]}
           (event*
            "htmx:beforeRequest"
            #js {:elt
                 (element "button")})]

       (is (true?
            (core/on-before-request!
             event)))

       (is (false?
            @prevented?))))))

;; -----------------------------------------------------------------------------
;; afterRequest settlement / failure translation
;; -----------------------------------------------------------------------------

(deftest after-request-delivers-semantic-settlement-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           xhr
           #js {:status
                200

                :responseText
                "<settlement/>"}

           cleanup
           (atom [])

           failures
           (atom [])

           emitted
           (atom [])]

       (with-redefs
        [optimistic/execution-for-source
         (fn [actual-source]
           (when
             (identical?
              source
              actual-source)
             "execution-1"))

         optimistic/active?
         (fn [execution-id]
           (= "execution-1"
              execution-id))

         optimistic/settle-from-xhr!
         (fn [execution-id actual-xhr]
           (is (= "execution-1"
                  execution-id))
           (is (identical?
                xhr
                actual-xhr))
           {:status
            :resumed})

         optimistic/cleanup-source-if-terminal!
         (fn [actual-source]
           (swap!
            cleanup
            conj
            actual-source)
           true)

         optimistic/request-failed!
         (fn [& args]
           (swap!
            failures
            conj
            args))

         optimistic/emit!
         (fn [& args]
           (swap!
            emitted
            conj
            args))]

        (is (true?
             (core/on-after-request!
              (event
               "htmx:afterRequest"
               #js {:elt source
                    :successful true
                    :xhr xhr}))))

        (is (= []
               @failures))

        (is (= []
               @emitted))

        (is (pos?
             (count
              @cleanup))))))))

(deftest after-request-unsuccessful-without-settlement-becomes-modeled-request-failure-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           seen
           (atom nil)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           nil)

         optimistic/request-failed!
         (fn [execution-id reason]
           (reset!
            seen
            [execution-id
             reason])
           :resumed)

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful false
               :xhr
               #js {:status
                    500}}))

        (is (= "execution-1"
               (first
                @seen)))

        (is (= "gesso.live.optimistic.request"
               (namespace
                (second
                 @seen))))

        (is (= "request-failed"
               (name
                (second
                 @seen)))))))))

(deftest after-request-success-without-settlement-is-protocol-diagnostic-not-network-failure-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           failures
           (atom [])

           emitted
           (atom nil)

           root
           (element "section")]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           nil)

         optimistic/request-failed!
         (fn [& args]
           (swap!
            failures
            conj
            args))

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/root
         (fn [_source]
           root)

         optimistic/emit!
         (fn [actual-root name detail]
           (reset!
            emitted
            [actual-root
             name
             detail])
           detail)]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful true
               :xhr
               #js {:status
                    200}}))

        (is (= []
               @failures))

        (is (identical?
             root
             (first
              @emitted)))

        (is (= "error"
               (second
                @emitted)))

        (let [detail
              (nth
               @emitted
               2)]

          (is (= "after-request"
                 (aget detail
                       "phase")))

          (is (= "missing-settlement"
                 (aget detail
                       "reason")))

          (is (= "execution-1"
                 (aget detail
                       "executionId")))))))))

(deftest after-request-inactive-execution-does-not-deliver-settlement-or-failure-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           settlement-count
           (atom 0)

           failure-count
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          false)

         optimistic/settle-from-xhr!
         (fn [& _]
           (swap!
            settlement-count
            inc))

         optimistic/request-failed!
         (fn [& _]
           (swap!
            failure-count
            inc))

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful false
               :xhr
               #js {:status
                    500}}))

        (is (= 0
               @settlement-count))

        (is (= 0
               @failure-count)))))))

(deftest after-request-cleans-preflight-even-when-execution-is-inactive-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           config-event
           (event
            "htmx:configRequest"
            #js {:elt source})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/execution-for-source
         (constantly
          nil)

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)]

        (core/on-config-request!
         config-event)

        (is (= 1
               (count
                @core/pending-requests)))

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful true
               :xhr
               #js {:status
                    200}}))

        (is (= {}
               @core/pending-requests)))))))

;; -----------------------------------------------------------------------------
;; Explicit HTMX failure events
;; -----------------------------------------------------------------------------

(deftest request-failed-event-delivers-modeled-failure-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           calls
           (atom [])]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/request-failed!
         (fn [execution-id reason]
           (swap!
            calls
            conj
            [execution-id
             reason])
           :resumed)

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)]

        (doseq [[event-type reason-name]
                [["htmx:responseError"
                  "response-error"]

                 ["htmx:sendError"
                  "send-error"]

                 ["htmx:timeout"
                  "timeout"]

                 ["htmx:abort"
                  "aborted"]]]

          (core/on-request-failed!
           (event
            event-type
            #js {:elt source}))

          (let [[execution-id reason]
                (last
                 @calls)]

            (is (= "execution-1"
                   execution-id))

            (is (= reason-name
                   (name
                    reason))))))))))

(deftest request-failed-event-for-inactive-execution-is-cleanup-only-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           failures
           (atom 0)

           cleanups
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          false)

         optimistic/request-failed!
         (fn [& _]
           (swap!
            failures
            inc))

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           (swap!
            cleanups
            inc)
           true)]

        (core/on-request-failed!
         (event
          "htmx:sendError"
          #js {:elt source}))

        (is (= 0
               @failures))

        (is (= 1
               @cleanups)))))))

;; -----------------------------------------------------------------------------
;; beforeSwap semantic settlement versus ordinary continuity
;; -----------------------------------------------------------------------------

(deftest before-swap-settlement-is-consumed-before-continuity-capture-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           captures
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           {:status
            :resumed})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/capture-from-event!
         (fn [_event]
           (swap!
            captures
            inc))]

        (core/on-before-swap!
         (event
          "htmx:beforeSwap"
          #js {:elt source
               :xhr
               #js {:status
                    200}}))

        (is (= 0
               @captures))

        (is (= 1
               (count
                @core/settled-response-sources))))))))

(deftest ordinary-before-swap-captures-continuity-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           seen
           (atom nil)

           e
           (event
            "htmx:beforeSwap"
            #js {:elt source
                 :xhr
                 #js {:status
                      200}})]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          nil)

         continuity/capture-from-event!
         (fn [actual-event]
           (reset!
            seen
            actual-event)
           true)]

        (core/on-before-swap!
         e)

        (is (identical?
             e
             @seen)))))))

(deftest settled-response-marker-prevents-recapture-on-later-before-swap-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           captures
           (atom 0)

           active?
           (atom true)

           e
           (event
            "htmx:beforeSwap"
            #js {:elt source
                 :xhr
                 #js {:status
                      200}})]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (fn [_execution-id]
           @active?)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           (reset!
            active?
            false)
           {:status
            :completed})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/capture-from-event!
         (fn [_event]
           (swap!
            captures
            inc)
           true)]

        (core/on-before-swap!
         e)

        (is (= 0
               @captures))

        (core/on-before-swap!
         e)

        (is (= 0
               @captures)))))))

;; -----------------------------------------------------------------------------
;; Ordinary swap observation / restoration ordering
;; -----------------------------------------------------------------------------

(deftest after-swap-observes-canonical-before-immediate-continuity-restore-test
  (let [calls
        (atom [])

        e
        (event
         "htmx:afterSwap"
         #js {})]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [(element "div")])

      optimistic/observe-canonical-tree!
      (fn [_element]
        (swap!
         calls
         conj
         :observe)
        true)

      continuity/restore-immediate-from-event!
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        (swap!
         calls
         conj
         :restore-immediate)
        true)]

      (is (true?
           (core/on-after-swap!
            e)))

      (is (= [:observe
              :restore-immediate]
             @calls)))))

(deftest after-settle-observes-canonical-before-delayed-continuity-restore-test
  (let [calls
        (atom [])

        e
        (event
         "htmx:afterSettle"
         #js {})]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [(element "div")])

      optimistic/observe-canonical-tree!
      (fn [_element]
        (swap!
         calls
         conj
         :observe)
        true)

      continuity/restore-after-layout-from-event!
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        (swap!
         calls
         conj
         :restore-after-layout)
        true)]

      (core/on-after-settle!
       e)

      (is (= [:observe
              :restore-after-layout]
             @calls)))))

(deftest canonical-observation-visits-all-event-elements-test
  (let [one
        (element "div")

        two
        (element "section")

        seen
        (atom [])]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [one
         two])

      optimistic/observe-canonical-tree!
      (fn [element]
        (swap!
         seen
         conj
         element)
        true)

      continuity/restore-immediate-from-event!
      (fn [_event]
        true)]

      (core/on-after-swap!
       (event
        "htmx:afterSwap"
        #js {}))

      (is (= [one
              two]
             @seen)))))

;; -----------------------------------------------------------------------------
;; OOB lifecycle
;; -----------------------------------------------------------------------------

(deftest oob-before-swap-uses-same-semantic-settlement-gate-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           captures
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           {:status
            :resumed})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/capture-from-event!
         (fn [_event]
           (swap!
            captures
            inc))]

        (core/on-oob-before-swap!
         (event
          "htmx:oobBeforeSwap"
          #js {:elt source
               :xhr
               #js {:status
                    200}}))

        (is (= 0
               @captures)))))))

(deftest ordinary-oob-before-swap-captures-test
  (with-core-state*
   (fn []
     (let [e
           (event
            "htmx:oobBeforeSwap"
            #js {})

           seen
           (atom nil)]

       (with-redefs
        [continuity/capture-from-event!
         (fn [actual-event]
           (reset!
            seen
            actual-event)
           true)]

        (core/on-oob-before-swap!
         e)

        (is (identical?
             e
             @seen)))))))

(deftest oob-after-swap-observes-then-performs-full-continuity-restore-test
  (let [calls
        (atom [])

        e
        (event
         "htmx:oobAfterSwap"
         #js {})]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [(element "div")])

      optimistic/observe-canonical-tree!
      (fn [_element]
        (swap!
         calls
         conj
         :observe)
        true)

      continuity/restore-from-event!
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        (swap!
         calls
         conj
         :restore)
        true)]

      (core/on-oob-after-swap!
       e)

      (is (= [:observe
              :restore]
             @calls)))))

;; -----------------------------------------------------------------------------
;; SSE lifecycle
;; -----------------------------------------------------------------------------

(deftest sse-before-message-captures-continuity-test
  (let [e
        (event
         "htmx:sseBeforeMessage"
         #js {})

        seen
        (atom nil)]

    (with-redefs
     [continuity/capture-from-event!
      (fn [actual-event]
        (reset!
         seen
         actual-event)
        true)]

      (is (true?
           (core/on-sse-before-message!
            e)))

      (is (identical?
           e
           @seen)))))

(deftest sse-message-observes-explicit-canonical-authority-then-restores-test
  (let [calls
        (atom [])

        e
        (event
         "htmx:sseMessage"
         #js {})]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [(element "div")])

      optimistic/observe-canonical-tree!
      (fn [_element]
        (swap!
         calls
         conj
         :observe)
        true)

      continuity/restore-from-event!
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        (swap!
         calls
         conj
         :restore)
        true)]

      (core/on-sse-message!
       e)

      (is (= [:observe
              :restore]
             @calls)))))

;; -----------------------------------------------------------------------------
;; Duplicate activation suppression
;; -----------------------------------------------------------------------------

(deftest busy-click-is-prevented-test
  (let [source
        (optimistic-source)

        nested
        (append!
         source
         (element "span"))

        {:keys [event
                prevented?]}
        (event*
         "click"
         #js {}
         nested)]

    (with-redefs
     [optimistic/source-descriptor
      (fn [_source]
        {:scope
         "request-1"})

      optimistic/scope-busy?
      (fn [scope]
        (= "request-1"
           scope))]

      (core/on-click-capture!
       event)

      (is (true?
           @prevented?)))))

(deftest idle-click-is-not-prevented-test
  (let [source
        (optimistic-source)

        {:keys [event
                prevented?]}
        (event*
         "click"
         #js {}
         source)]

    (with-redefs
     [optimistic/source-descriptor
      (fn [_source]
        {:scope
         "request-1"})

      optimistic/scope-busy?
      (constantly
       false)]

      (core/on-click-capture!
       event)

      (is (false?
           @prevented?)))))

(deftest malformed-source-descriptor-does-not-break-capture-phase-test
  (let [source
        (optimistic-source)

        {:keys [event
                prevented?]}
        (event*
         "click"
         #js {}
         source)]

    (with-redefs
     [optimistic/source-descriptor
      (fn [_source]
        (throw
         (js/Error.
          "bad descriptor")))]

      (is (true?
           (core/on-click-capture!
            event)))

      (is (false?
           @prevented?)))))

(deftest busy-submit-is-prevented-test
  (let [form
        (element
         "form"
         (merge
          {protocol/protocol-attr
           protocol/version

           protocol/transition-attr
           "request/claim"

           protocol/template-attr
           "request-claim"

           protocol/target-attr
           "closest [data-request-card]"

           protocol/scope-attr
           "request-1"}))

        {:keys [event
                prevented?]}
        (event*
         "submit"
         #js {}
         form)]

    (with-redefs
     [optimistic/source-descriptor
      (fn [_source]
        {:scope
         "request-1"})

      optimistic/scope-busy?
      (constantly
       true)]

      (core/on-submit-capture!
       event)

      (is (true?
           @prevented?)))))

(deftest unrelated-click-and-submit-are-no-ops-test
  (doseq [type
          ["click"
           "submit"]]

    (let [{:keys [event
                  prevented?]}
          (event*
           type
           #js {}
           (element "div"))]

      ((if (= type
              "click")
         core/on-click-capture!
         core/on-submit-capture!)
       event)

      (is (false?
           @prevented?)))))

;; -----------------------------------------------------------------------------
;; Cleanup
;; -----------------------------------------------------------------------------

(deftest before-cleanup-releases-continuity-resources-test
  (let [target
        (element "section")

        e
        (event
         "htmx:beforeCleanupElement"
         #js {:target
              target})

        seen
        (atom nil)]

    (with-redefs
     [continuity/cleanup-element!
      (fn [actual-target]
        (reset!
         seen
         actual-target)
        true)]

      (core/on-before-cleanup!
       e)

      (is (identical?
           target
           @seen)))))

(deftest before-cleanup-cleans-terminal-source-correlation-and-preflight-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           cleanup-count
           (atom 0)

           config-event
           (event
            "htmx:configRequest"
            #js {:elt
                 source})

           cleanup-event
           (event
            "htmx:beforeCleanupElement"
            #js {:elt
                 source

                 :target
                 source})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/cleanup-source-if-terminal!
         (fn [actual-source]
           (is (identical?
                source
                actual-source))
           (swap!
            cleanup-count
            inc)
           true)

         continuity/cleanup-element!
         (fn [_target]
           true)]

        (core/on-config-request!
         config-event)

        (is (= 1
               (count
                @core/pending-requests)))

        (core/on-before-cleanup!
         cleanup-event)

        (is (= 1
               @cleanup-count))

        (is (= {}
               @core/pending-requests)))))))

(deftest before-cleanup-does-not-semantically-abort-active-optimistic-process-test
  (let [target
        (element "details")

        abort-count
        (atom 0)]

    (with-redefs
     [continuity/cleanup-element!
      (fn [_target]
        true)

      optimistic/abort!
      (fn [& _]
        (swap!
         abort-count
         inc))]

      (core/on-before-cleanup!
       (event
        "htmx:beforeCleanupElement"
        #js {:target target}))

      (is (= 0
             @abort-count)))))

;; -----------------------------------------------------------------------------
;; Runtime state / diagnostics
;; -----------------------------------------------------------------------------

(deftest runtime-state-test
  (with-core-state*
   (fn []
     (reset!
      core/pending-requests
      {"one"
       {:execution-id
        "execution-1"}

       "two"
       {:execution-id
        "execution-2"}})

     (reset!
      core/settled-response-sources
      #{"one"})

     (with-redefs
      [choreo-runtime/diagnostics
       (fn []
         {:active-count
          2})

       continuity/slot-summaries
       (fn []
         [{:target-id
           "target-1"}])

       optimistic/diagnostics
       (fn []
         {:active-scopes
          {"scope"
           {:execution-id
            "execution-1"}}})]

       (is (= {:version
               core/runtime-version

               :protocol-version
               protocol/version

               :pending-request-count
               2

               :settled-response-count
               1

               :choreo
               {:active-count
                2}

               :continuity
               {:slots
                [{:target-id
                  "target-1"}]}

               :optimistic
               {:active-scopes
                {"scope"
                 {:execution-id
                  "execution-1"}}}}
              (core/runtime-state)))))))

;; -----------------------------------------------------------------------------
;; Public JS API
;; -----------------------------------------------------------------------------

(deftest install-public-api-test
  (with-core-state*
   (fn []
     (let [existing
           #js {:existing
                "preserved"}

           registered
           (atom nil)]

       (aset js/window
             "gessoLive"
             existing)

       (with-redefs
        [continuity/register-box!
         (fn [type implementation]
           (reset!
            registered
            [type
             implementation])
           true)

         continuity/parse-config
         (fn [_root]
           {:enabled true})

         continuity/boxes-from-config
         (fn [_config]
           [{:type
             "inputs"}])

         continuity/capture-from-event!
         (fn [_event]
           :captured)

         continuity/restore-from-event!
         (fn [_event]
           :restored)

         choreo-runtime/diagnostics
         (fn []
           {:active-count
            1})

         choreo-runtime/active-summaries
         (fn []
           [{:execution-id
             "execution-1"}])

         choreo-runtime/terminal-summaries
         (fn []
           [{:execution-id
             "done"}])

         optimistic/diagnostics
         (fn []
           {:active-scopes
            {}})

         optimistic/active?
         (fn [execution-id]
           (= "execution-1"
              execution-id))

         optimistic/scope-busy?
         (fn [scope]
           (= "scope-1"
              scope))

         optimistic/command-payload
         (fn [execution-id]
           {:execution-id
            execution-id})]

        (is (true?
             (core/install-public-api!)))

        (let [api
              (aget js/window
                    "gessoLive")

              continuity-api
              (aget api
                    "continuity")

              choreo-api
              (aget api
                    "choreo")

              optimistic-api
              (aget api
                    "optimistic")]

          (is (identical?
               existing
               api))

          (is (= "preserved"
                 (aget api
                       "existing")))

          (is (= core/runtime-version
                 (aget api
                       "version")))

          (is (= protocol/version
                 (aget api
                       "protocolVersion")))

          (is (= core/runtime-version
                 (aget continuity-api
                       "version")))

          (is (= core/runtime-version
                 (aget choreo-api
                       "version")))

          (is (= core/runtime-version
                 (aget optimistic-api
                       "version")))

          (is (fn?
               (aget api
                     "state")))

          (is (fn?
               (aget continuity-api
                     "registerBox")))

          (is (fn?
               (aget choreo-api
                     "state")))

          (is (fn?
               (aget choreo-api
                     "active")))

          (is (fn?
               (aget choreo-api
                     "terminal")))

          (is (fn?
               (aget optimistic-api
                     "state")))

          (is (fn?
               (aget optimistic-api
                     "active")))

          (is (fn?
               (aget optimistic-api
                     "scopeBusy")))

          (is (fn?
               (aget optimistic-api
                     "commandPayload")))

          ((aget continuity-api
                 "registerBox")
           "custom"
           #js {:capture
                (fn []
                  nil)})

          (is (= "custom"
                 (first
                  @registered)))

          (is (= true
                 ((aget optimistic-api
                        "active")
                  "execution-1")))

          (is (= true
                 ((aget optimistic-api
                        "scopeBusy")
                  "scope-1")))

          (is (= "execution-1"
                 (aget
                  ((aget optimistic-api
                         "commandPayload")
                   "execution-1")
                  "execution-id")))))))))

(deftest install-public-api-creates-object-when-absent-test
  (with-core-state*
   (fn []
     (aset js/window
           "gessoLive"
           nil)

     (core/install-public-api!)

     (let [api
           (aget js/window
                 "gessoLive")]

       (is (some?
            api))

       (is (= core/runtime-version
              (aget api
                    "version")))))))

;; -----------------------------------------------------------------------------
;; Listener adapter
;; -----------------------------------------------------------------------------

(deftest add-document-listener-test
  (let [calls
        (atom [])

        original
        (.-addEventListener
         js/document)]

    (set!
     (.-addEventListener js/document)
     (fn [& args]
       (swap!
        calls
        conj
        args)))

    (try
      (let [handler
            (fn [_event]
              nil)]

        (core/add-document-listener!
         "test:event"
         handler)

        (core/add-document-listener!
         "test:capture"
         handler
         true)

        (is (= 2
               (count
                @calls)))

        (is (= "test:event"
               (first
                (first
                 @calls))))

        (is (= 2
               (count
                (first
                 @calls))))

        (is (= "test:capture"
               (first
                (second
                 @calls))))

        (is (= true
               (nth
                (second
                 @calls)
                2))))

      (finally
        (set!
         (.-addEventListener js/document)
         original)))))

;; -----------------------------------------------------------------------------
;; init! listener installation
;; -----------------------------------------------------------------------------

(deftest init-installs-runtime-and-all-framework-listeners-once-test
  (let [old-initialized
        @core/initialized?

        listeners
        (atom [])

        calls
        (atom [])]

    (reset!
     core/initialized?
     false)

    (try
      (with-redefs
       [continuity/initialize!
        (fn []
          (swap!
           calls
           conj
           :continuity)
          true)

        optimistic/initialize!
        (fn []
          (swap!
           calls
           conj
           :optimistic)
          true)

        core/add-document-listener!
        (fn
          ([name handler]
           (swap!
            listeners
            conj
            [name
             handler
             false]))
          ([name handler capture?]
           (swap!
            listeners
            conj
            [name
             handler
             capture?])))

        core/install-public-api!
        (fn []
          (swap!
           calls
           conj
           :api)
          true)]

        (is (true?
             (core/init!)))

        (is (= [:continuity
                :optimistic
                :api]
               @calls))

        (is (= 17
               (count
                @listeners)))

        (is (= #{"click"
                 "submit"
                 "htmx:configRequest"
                 "htmx:beforeRequest"
                 "htmx:afterRequest"
                 "htmx:responseError"
                 "htmx:sendError"
                 "htmx:timeout"
                 "htmx:abort"
                 "htmx:beforeSwap"
                 "htmx:afterSwap"
                 "htmx:afterSettle"
                 "htmx:oobBeforeSwap"
                 "htmx:oobAfterSwap"
                 "htmx:sseBeforeMessage"
                 "htmx:sseMessage"
                 "htmx:beforeCleanupElement"}
               (set
                (map
                 first
                 @listeners))))

        (is (= #{"click"
                 "submit"}
               (set
                (for [[name _handler capture?]
                      @listeners
                      :when capture?]
                  name))))

        (let [listener-count
              (count
               @listeners)

              call-count
              (count
               @calls)]

          (is (true?
               (core/init!)))

          (is (= listener-count
                 (count
                  @listeners)))

          (is (= call-count
                 (count
                  @calls)))))

      (finally
        (reset!
         core/initialized?
         old-initialized)))))

;; -----------------------------------------------------------------------------
;; Handler return values remain HTMX-listener-friendly
;; -----------------------------------------------------------------------------

(deftest framework-handlers-return-true-test
  (with-core-state*
   (fn []
     (let [plain-event
           (event
            "plain"
            #js {})]

       (with-redefs
        [continuity/capture-from-event!
         (fn [_event]
           true)

         continuity/restore-immediate-from-event!
         (fn [_event]
           true)

         continuity/restore-after-layout-from-event!
         (fn [_event]
           true)

         continuity/restore-from-event!
         (fn [_event]
           true)

         continuity/event-elements
         (fn [_event]
           [])

         continuity/cleanup-element!
         (fn [_element]
           true)]

        (is (true?
             (core/on-config-request!
              plain-event)))

        (is (true?
             (core/on-before-request!
              plain-event)))

        (is (true?
             (core/on-after-request!
              plain-event)))

        (is (true?
             (core/on-request-failed!
              plain-event)))

        (is (true?
             (core/on-before-swap!
              plain-event)))

        (is (true?
             (core/on-after-swap!
              plain-event)))

        (is (true?
             (core/on-after-settle!
              plain-event)))

        (is (true?
             (core/on-oob-before-swap!
              plain-event)))

        (is (true?
             (core/on-oob-after-swap!
              plain-event)))

        (is (true?
             (core/on-sse-before-message!
              plain-event)))

        (is (true?
             (core/on-sse-message!
              plain-event)))

        (is (true?
             (core/on-click-capture!
              plain-event)))

        (is (true?
             (core/on-submit-capture!
              plain-event)))

        (is (true?
             (core/on-before-cleanup!
              plain-event))))))))

;; -----------------------------------------------------------------------------
;; Integration: configRequest -> beforeRequest correlation
;; -----------------------------------------------------------------------------

(deftest preflight-correlation-is-stable-across-htmx-request-boundary-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           config-detail
           #js {:elt
                source

                :headers
                (js-headers
                core/consistency-request-header
                "token-a")}

           config-event
           (event
            "htmx:configRequest"
            config-detail)

           before-event
           (event
            "htmx:beforeRequest"
            #js {:elt
                 source})

           started
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [_source opts]
           (reset!
            started
            opts)
           {:execution-id
            (:execution-id opts)})]

        (core/on-config-request!
         config-event)

        (is (= "execution-1"
               (aget
                (aget config-detail
                      "headers")
                core/optimistic-request-header)))

        (core/on-before-request!
         before-event)

        (is (= "execution-1"
               (:execution-id
                @started)))

        (is (= "token-a"
               (:consistency-token
                @started)))

        (is (= {}
               @core/pending-requests)))))))

;; -----------------------------------------------------------------------------
;; Integration: semantically consumed response does not become ordinary swap
;; -----------------------------------------------------------------------------

(deftest semantic-settlement-response-is-not-recaptured-as-ordinary-swap-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           active
           (atom true)

           capture-count
           (atom 0)

           before-swap
           (event
            "htmx:beforeSwap"
            #js {:elt
                 source

                 :xhr
                 #js {:status
                      200}})]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (fn [_execution-id]
           @active)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           (reset!
            active
            false)
           {:status
            :completed})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/capture-from-event!
         (fn [_event]
           (swap!
            capture-count
            inc)
           true)]

        (core/on-before-swap!
         before-swap)

        (is (= 0
               @capture-count))

        (is (= 1
               (count
                @core/settled-response-sources)))

        ;; A duplicate beforeSwap pass for the same HTMX lifecycle still must
        ;; not overwrite the optimistic execution's continuity capture.
        (core/on-before-swap!
         before-swap)

        (is (= 0
               @capture-count)))))))

;; -----------------------------------------------------------------------------
;; Integration: successful response without semantic settlement remains active
;; -----------------------------------------------------------------------------

(deftest missing-settlement-diagnostic-does-not-terminate-optimistic-process-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           request-failure-count
           (atom 0)

           diagnostics
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           nil)

         optimistic/request-failed!
         (fn [& _]
           (swap!
            request-failure-count
            inc))

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         optimistic/emit!
         (fn [_root _name _detail]
           (swap!
            diagnostics
            inc)
           true)]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful true
               :xhr
               #js {:status
                    200}}))

        (is (= 0
               @request-failure-count))

        (is (= 1
               @diagnostics)))))))

;; -----------------------------------------------------------------------------
;; Integration: no optimistic owner means ordinary continuity still works
;; -----------------------------------------------------------------------------

(deftest nonoptimistic-swap-still-participates-in-continuity-test
  (let [e
        (event
         "htmx:beforeSwap"
         #js {:target
              (element "div")})

        capture-count
        (atom 0)]

    (with-redefs
     [continuity/capture-from-event!
      (fn [actual-event]
        (is (identical?
             e
             actual-event))
        (swap!
         capture-count
         inc)
        true)]

      (core/on-before-swap!
       e)

      (is (= 1
             @capture-count)))))

;; -----------------------------------------------------------------------------
;; Regression: click suppression happens only after a scope is actually busy
;; -----------------------------------------------------------------------------

(deftest first-click-is-not-prevented-just-because-source-is-optimistic-test
  (let [source
        (optimistic-source)

        first
        (event*
         "click"
         #js {}
         source)

        busy?
        (atom false)]

    (with-redefs
     [optimistic/source-descriptor
      (fn [_source]
        {:scope
         "request-1"})

      optimistic/scope-busy?
      (fn [_scope]
        @busy?)]

      (core/on-click-capture!
       (:event first))

      (is (false?
           @(:prevented? first)))

      (reset!
       busy?
       true)

      (let [second
            (event*
             "click"
             #js {}
             source)]

        (core/on-click-capture!
         (:event second))

        (is (true?
             @(:prevented? second)))))))

;; -----------------------------------------------------------------------------
;; Regression: configRequest itself never mutates optimistic DOM
;; -----------------------------------------------------------------------------

(deftest config-request-only-correlates-and-never-starts-optimism-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           start-count
           (atom 0)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [& _]
           (swap!
            start-count
            inc))]

        (core/on-config-request!
         (event
          "htmx:configRequest"
          #js {:elt source}))

        (is (= 0
               @start-count))

        (is (= 1
               (count
                @core/pending-requests))))))))

;; -----------------------------------------------------------------------------
;; Regression: beforeRequest starts exactly once for one preflight
;; -----------------------------------------------------------------------------

(deftest one-preflight-is-consumed-by-one-before-request-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           start-count
           (atom 0)

           config
           (event
            "htmx:configRequest"
            #js {:elt source})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [_source _opts]
           (swap!
            start-count
            inc)
           {:execution-id
            "execution-1"})

         optimistic/emit!
         (fn [& _]
           true)]

        (core/on-config-request!
         config)

        (core/on-before-request!
         (event
          "htmx:beforeRequest"
          #js {:elt source}))

        (is (= 1
               @start-count))

        (is (= {}
               @core/pending-requests))

        (let [{:keys [event
                      prevented?]}
              (event*
               "htmx:beforeRequest"
               #js {:elt source})]

          (is (thrown?
               cljs.core.ExceptionInfo
               (core/on-before-request!
                event)))

          (is (true?
               @prevented?))

          (is (= 1
                 @start-count))))))))

;; -----------------------------------------------------------------------------
;; Regression: continuity and optimistic semantic settlement have one owner each
;; -----------------------------------------------------------------------------

(deftest settled-response-skips-continuity-capture-but-still-allows-post-swap-restoration-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           captures
           (atom 0)

           immediate-restores
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           {:status
            :resumed})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         continuity/capture-from-event!
         (fn [_event]
           (swap!
            captures
            inc)
           true)

         continuity/event-elements
         (fn [_event]
           [])

         continuity/restore-immediate-from-event!
         (fn [_event]
           (swap!
            immediate-restores
            inc)
           true)]

        (core/on-before-swap!
         (event
          "htmx:beforeSwap"
          #js {:elt source
               :xhr
               #js {:status
                    200}}))

        (core/on-after-swap!
         (event
          "htmx:afterSwap"
          #js {:elt source}))

        (is (= 0
               @captures))

        (is (= 1
               @immediate-restores)))))))

;; -----------------------------------------------------------------------------
;; Public API state is a snapshot, not a live mutable object
;; -----------------------------------------------------------------------------

(deftest public-state-function-returns-fresh-js-values-test
  (with-core-state*
   (fn []
     (core/install-public-api!)

     (let [state-fn
           (aget
            (aget js/window
                  "gessoLive")
            "state")

           first-state
           (state-fn)

           second-state
           (state-fn)]

       (is (not
            (identical?
             first-state
             second-state)))

       (is (= core/runtime-version
              (aget first-state
                    "version")))))))

;; -----------------------------------------------------------------------------
;; No browser framework event creates server truth
;; -----------------------------------------------------------------------------

(deftest core-only-observes-canonical-dom-through-optimistic-authority-adapter-test
  (let [candidate
        (element "details")

        seen
        (atom nil)]

    (with-redefs
     [continuity/event-elements
      (fn [_event]
        [candidate])

      optimistic/observe-canonical-tree!
      (fn [node]
        (reset!
         seen
         node)
        node)

      continuity/restore-from-event!
      (fn [_event]
        true)]

      (core/on-sse-message!
       (event
        "htmx:sseMessage"
        #js {:target
             candidate}))

      (is (identical?
           candidate
           @seen))

      (testing "core itself never marks the node canonical"
        (is (false?
             (dom/canonical?
              candidate)))))))

;; -----------------------------------------------------------------------------
;; Request header correlation is plain HTTP data
;; -----------------------------------------------------------------------------

(deftest config-request-header-is-string-wire-data-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           e
           (event
            "htmx:configRequest"
            #js {:elt
                 source})]

       (with-redefs
        [optimistic/execution-id
         (fn []
           :execution/id)]

        (core/on-config-request!
         e)

        (is (= "execution/id"
               (header
                e
                core/optimistic-request-header)))

        (is (= :execution/id
               (:execution-id
                (only-pending-request)))))))))

;; -----------------------------------------------------------------------------
;; Existing differently-cased optimistic header is overwritten canonically
;; -----------------------------------------------------------------------------

(deftest config-request-writes-canonical-header-name-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           existing
           #js {"gesso-optimistic-execution"
                "old"}

           e
           (event
            "htmx:configRequest"
            #js {:elt
                 source

                 :headers
                 existing})]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "new")]

        (core/on-config-request!
         e)

        (is (= "new"
               (aget existing
                     core/optimistic-request-header))))))))

;; -----------------------------------------------------------------------------
;; Cleanup target selection uses HTMX target semantics, not optimistic semantics
;; -----------------------------------------------------------------------------

(deftest before-cleanup-uses-framework-event-target-for-continuity-cleanup-test
  (let [source
        (optimistic-source)

        framework-target
        (element "section")

        seen
        (atom nil)]

    (with-redefs
     [continuity/cleanup-element!
      (fn [target]
        (reset!
         seen
         target)
        true)

      optimistic/cleanup-source-if-terminal!
      (fn [_source]
        true)]

      (core/on-before-cleanup!
       (event
        "htmx:beforeCleanupElement"
        #js {:elt source
             :target framework-target}))

      (is (identical?
           framework-target
           @seen)))))

;; -----------------------------------------------------------------------------
;; Contrast: transport target is appropriate for cleanup but not optimistic start
;; -----------------------------------------------------------------------------

(deftest transport-target-has-framework-purpose-without-becoming-choreography-authority-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           framework-target
           (element "section")

           start-opts
           (atom nil)

           cleanup-target
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [_source opts]
           (reset!
            start-opts
            opts)
           {:execution-id
            "execution-1"})

         continuity/cleanup-element!
         (fn [target]
           (reset!
            cleanup-target
            target)
           true)

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)]

        (core/on-config-request!
         (event
          "htmx:configRequest"
          #js {:elt source}))

        (core/on-before-request!
         (event
          "htmx:beforeRequest"
          #js {:elt source
               :target framework-target}))

        (core/on-before-cleanup!
         (event
          "htmx:beforeCleanupElement"
          #js {:elt source
               :target framework-target}))

        (testing "framework target is valid for framework cleanup"
          (is (identical?
               framework-target
               @cleanup-target)))

        (testing "but it is not semantic target authority for optimism"
          ;; Expected to fail against the current implementation.
          (is (not
               (contains?
                @start-opts
                :target)))))))))

;; -----------------------------------------------------------------------------
;; Correlated start failure cannot leak pending preflight state
;; -----------------------------------------------------------------------------

(deftest repeated-failed-starts-do-not-accumulate-pending-request-state-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           counter
           (atom 0)]

       (with-redefs
        [optimistic/execution-id
         (fn []
           (str
            "execution-"
            (swap!
             counter
             inc)))

         optimistic/start!
         (fn [& _]
           (throw
            (js/Error.
             "nope")))

         optimistic/emit!
         (fn [& _]
           true)]

        (dotimes [_ 3]
          (core/on-config-request!
           (event
            "htmx:configRequest"
            #js {:elt source}))

          (let [{:keys [event]}
                (event*
                 "htmx:beforeRequest"
                 #js {:elt source})]

            (is (some?
                 (thrown
                  #(core/on-before-request!
                    event)))))

          (is (= {}
                 @core/pending-requests))))))))

;; -----------------------------------------------------------------------------
;; Non-optimistic request failures remain outside optimistic protocol
;; -----------------------------------------------------------------------------

(deftest nonoptimistic-request-failure-does-not-call-optimistic-failure-test
  (let [calls
        (atom 0)]

    (with-redefs
     [optimistic/request-failed!
      (fn [& _]
        (swap!
         calls
         inc))]

      (core/on-request-failed!
       (event
        "htmx:sendError"
        #js {:elt
             (element "button")}))

      (is (= 0
             @calls)))))

;; -----------------------------------------------------------------------------
;; Browser core is choreography-policy integration, not choreography execution
;; -----------------------------------------------------------------------------

(deftest core-never-directly-resumes-portable-machine-test
  (let [calls
        (atom 0)]

    (with-redefs
     [choreo-runtime/resume!
      (fn [& _]
        (swap!
         calls
         inc))]

      (core/on-sse-message!
       (event
        "htmx:sseMessage"
        #js {}))

      (core/on-after-swap!
       (event
        "htmx:afterSwap"
        #js {}))

      (is (= 0
             @calls)))))

;; -----------------------------------------------------------------------------
;; Browser core does not infer command success from HTTP success
;; -----------------------------------------------------------------------------

(deftest http-2xx-without-settlement-never-becomes-semantic-success-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           settlements
           (atom 0)

           failures
           (atom 0)

           diagnostics
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           (swap!
            settlements
            inc)
           nil)

         optimistic/request-failed!
         (fn [& _]
           (swap!
            failures
            inc))

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         optimistic/emit!
         (fn [& _]
           (swap!
            diagnostics
            inc)
           true)]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :xhr
               #js {:status
                    204}}))

        (is (= 1
               @settlements))

        (is (= 0
               @failures))

        (is (= 1
               @diagnostics)))))))

;; -----------------------------------------------------------------------------
;; Browser core does not infer command failure from settlement outcome itself
;; -----------------------------------------------------------------------------

(deftest semantic-failed-settlement-is-still-a-successful-protocol-delivery-test
  (with-core-state*
   (fn []
     (let [source
           (optimistic-source)

           failures
           (atom 0)]

       (with-redefs
        [optimistic/execution-for-source
         (constantly
          "execution-1")

         optimistic/active?
         (constantly
          true)

         optimistic/settle-from-xhr!
         (fn [_execution-id _xhr]
           {:status
            :resumed

            :result
            {:outcome
             :failed}})

         optimistic/cleanup-source-if-terminal!
         (fn [_source]
           true)

         optimistic/request-failed!
         (fn [& _]
           (swap!
            failures
            inc))]

        (core/on-after-request!
         (event
          "htmx:afterRequest"
          #js {:elt source
               :successful true
               :xhr
               #js {:status
                    200}}))

        (is (= 0
               @failures)))))))

;; -----------------------------------------------------------------------------
;; End-to-end adapter regression: source -> preflight -> start opts
;; -----------------------------------------------------------------------------

(deftest request-adapter-preserves-protocol-boundary-test
  (with-core-state*
   (fn []
     (let [card
           (element
            "details"
            {:data-request-card
             "request-1"})

           source
           (append!
            card
            (optimistic-source))

           transport-target
           (element
            "div"
            {:id "transport"})

           seen
           (atom nil)]

       (with-redefs
        [optimistic/execution-id
         (constantly
          "execution-1")

         optimistic/start!
         (fn [actual-source opts]
           (reset!
            seen
            {:source
             actual-source

             :opts
             opts})
           {:execution-id
            "execution-1"})]

        (let [config
              (event
               "htmx:configRequest"
               #js {:elt source
                    :headers
                    (js-headers
                    core/consistency-request-header
                    "token-1")})

              before
              (event
               "htmx:beforeRequest"
               #js {:elt source
                    :target transport-target})]

          (core/on-config-request!
           config)

          (core/on-before-request!
           before)

          (is (identical?
               source
               (:source
                @seen)))

          (is (= "execution-1"
                 (get-in
                  @seen
                  [:opts
                   :execution-id])))

          (is (= "token-1"
                 (get-in
                  @seen
                  [:opts
                   :consistency-token])))

          ;; The request adapter may correlate transport and protocol, but the
          ;; choreography descriptor remains the sole semantic target source.
          ;; Expected to fail against current browser.core.
          (is (not
               (contains?
                (:opts
                 @seen)
                :target)))))))))

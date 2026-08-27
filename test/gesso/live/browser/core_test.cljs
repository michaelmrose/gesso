(ns gesso.live.browser.core-test
  "Boundary tests for the thin HTMX/browser normalization layer.

   These tests deliberately treat browser.core as a trusted physical boundary,
   not as a second semantic state machine. They therefore focus on:

   - identifying managed fragment roots without appropriating unrelated HTMX
   - converting documented HTMX lifecycle callbacks to adapter events
   - exact physical request/XHR correlation across reused logical roots
   - synchronous re-entry when HTMX trigger immediately begins a request
   - fail-closed request/swap behavior at the FFI boundary
   - authoritative metadata remaining plain carrier data until adapter judgment
   - adapter-approved progression requirements returning through HTMX request headers
   - explicit invalidation and fragment retirement
   - listener ownership and teardown
   - diagnostics excluding raw DOM/XHR/HTMX objects

   The pure adapter has its own example/property corpus. These tests assert only
   the additional guarantees introduced by the physical HTMX normalization
   boundary."
  (:require
   [cljs.test :refer-macros [async deftest is testing]]
   [gesso.live.browser.adapter :as adapter]
   [gesso.live.browser.continuity :as continuity]
   [gesso.live.browser.core :as core]
   [gesso.live.browser.shell :as shell]
   [gesso.live.progression :as progression]
   [gesso.live.progression.http :as progression.http]))

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- thrown
  [f]
  (try
    (f)
    nil
    (catch :default error
      error)))

(defn- error-kind
  [f]
  (some-> (thrown f)
          ex-data
          :error/kind))

(defn- deep-identical?
  [root needle]
  (cond
    (identical? root needle)
    true

    (map? root)
    (boolean
     (some true?
           (concat
            (map #(deep-identical? % needle) (keys root))
            (map #(deep-identical? % needle) (vals root)))))

    (or (vector? root)
        (list? root)
        (seq? root)
        (set? root))
    (boolean
     (some #(deep-identical? % needle) root))

    :else
    false))

(defn- id-generator
  [& ids]
  (let [remaining (atom (vec ids))]
    (fn []
      (let [id (first @remaining)]
        (swap! remaining #(if (seq %) (subvec % 1) %))
        id))))

;; =============================================================================
;; Fake DOM / HTMX host objects
;; =============================================================================

(defn- make-element
  [{:keys [fragment-id closest-root nested attrs]}]
  (let [element (js-obj)
        nested (vec (or nested []))
        attrs (cond-> (or attrs {})
                fragment-id
                (assoc core/fragment-attribute fragment-id))]
    (aset element "nodeType" 1)
    ;; browser.dom/continuity legitimately inspect children on a real Element.
    ;; This fake host supplies the corresponding empty collection.
    (aset element "children" #js [])
    (aset element
          "getAttribute"
          (fn [name]
            (get attrs name)))
    (aset element
          "closest"
          (fn [selector]
            (when (= selector core/fragment-selector)
              (or (when fragment-id element)
                  closest-root))))
    (aset element
          "querySelectorAll"
          (fn [selector]
            (if (= selector core/fragment-selector)
              (to-array nested)
              #js [])))
    element))

(defn- make-root
  ([fragment-id]
   (make-root fragment-id []))
  ([fragment-id nested]
   (make-element {:fragment-id fragment-id
                  :nested nested})))

(defn- make-managed-root
  ([fragment-id]
   (make-managed-root fragment-id []))
  ([fragment-id nested]
   (make-element {:fragment-id fragment-id
                  :nested nested
                  :attrs {"hx-trigger" core/refresh-event-name}})))

(defn- make-legacy-sse-root
  [fragment-id event-name]
  (make-element {:fragment-id fragment-id
                 :attrs {"hx-trigger" (str "sse:" event-name)}}))

(defn- make-invalidation-listener
  [root fragment-id]
  (make-element {:closest-root root
                 :attrs {core/invalidation-listener-attribute fragment-id}}))

(defn- make-child
  [root]
  (make-element {:closest-root root}))

(defn- make-document
  [roots]
  (let [roots* (atom (vec roots))
        added (atom [])
        removed (atom [])
        document (js-obj)]
    (aset document
          "querySelectorAll"
          (fn [selector]
            (if (= selector core/fragment-selector)
              (to-array @roots*)
              #js [])))
    (aset document
          "addEventListener"
          (fn [name handler capture?]
            (swap! added conj [name handler capture?])))
    (aset document
          "removeEventListener"
          (fn [name handler capture?]
            (swap! removed conj [name handler capture?])))
    {:document document
     :roots roots*
     :added added
     :removed removed}))

(defn- make-htmx
  ([]
   (make-htmx nil))
  ([on-trigger]
   (let [calls (atom [])
         htmx (js-obj)]
     (aset htmx
           "trigger"
           (fn [root name detail]
             (swap! calls conj {:root root
                                :name name
                                :detail detail})
             (when on-trigger
               (on-trigger root name detail))
             true))
     {:htmx htmx
      :calls calls})))

(defn- make-xhr
  ([status]
   (make-xhr status nil))
  ([status on-abort]
   (let [aborted? (atom false)
         xhr (js-obj)]
     (aset xhr "status" status)
     (aset xhr
           "abort"
           (fn []
             (reset! aborted? true)
             (when on-abort
               (on-abort))
             true))
     {:xhr xhr
      :aborted? aborted?})))

(defn- make-event
  ([type]
   (make-event type nil))
  ([type {:keys [elt target xhr successful should-swap extra-detail]}]
   (let [prevented? (atom false)
         detail (js-obj)
         event (js-obj)]
     (when elt
       (aset detail "elt" elt))
     (when target
       (aset detail "target" target))
     (when xhr
       (aset detail "xhr" xhr))
     (when (some? successful)
       (aset detail "successful" successful))
     (when (some? should-swap)
       (aset detail "shouldSwap" should-swap))
     (doseq [[k v] extra-detail]
       (aset detail (name k) v))
     (aset event "type" type)
     (aset event "detail" detail)
     (when target
       (aset event "target" target))
     (aset event
           "preventDefault"
           (fn []
             (reset! prevented? true)))
     {:event event
      :detail detail
      :prevented? prevented?})))

(defn- runtime-fixture
  ([fragment-id]
   (runtime-fixture fragment-id nil))
  ([fragment-id options]
   (let [root (make-root fragment-id)
         document-fixture (make-document [root])
         htmx-fixture (make-htmx)
         runtime
         (core/create
          (merge
           {:document (:document document-fixture)
            :htmx (:htmx htmx-fixture)
            :request-id-fn (id-generator "request-1" "request-2" "request-3")}
           options))]
     {:root root
      :document-fixture document-fixture
      :htmx-fixture htmx-fixture
      :runtime runtime})))

(defn- request-generation
  [runtime root]
  (:request-generation
   (core/pending-refresh runtime root)))

(defn- begin-refresh!
  ([runtime fragment-id]
   (core/notify-fragment! runtime fragment-id))
  ([runtime fragment-id requirement]
   (core/notify-fragment! runtime fragment-id requirement)))

(defn- bind-request!
  [runtime root xhr]
  (let [{:keys [event]}
        (make-event
         "htmx:beforeRequest"
         {:elt root
          :target root
          :xhr xhr})]
    (core/on-before-request! runtime event)))

(defn- before-swap!
  [runtime root xhr]
  (let [fixture
        (make-event
         "htmx:beforeSwap"
         {:elt root
          :target root
          :xhr xhr
          :should-swap true})]
    (core/on-before-swap! runtime (:event fixture))
    fixture))

(defn- after-swap!
  [runtime root xhr]
  (core/on-after-swap!
   runtime
   (:event
    (make-event
     "htmx:afterSwap"
     {:elt root
      :target root
      :xhr xhr}))))

(defn- after-request!
  [runtime root xhr successful]
  (core/on-after-request!
   runtime
   (:event
    (make-event
     "htmx:afterRequest"
     {:elt root
      :target root
      :xhr xhr
      :successful successful}))))

;; =============================================================================
;; Construction / option ownership
;; =============================================================================

(deftest create-builds-isolated-core-and-shell-test
  (let [{runtime-a :runtime} (runtime-fixture "a")
        {runtime-b :runtime} (runtime-fixture "b")]
    (is (core/core? runtime-a))
    (is (core/core? runtime-b))
    (is (shell/shell? (core/shell-runtime runtime-a)))
    (is (continuity/runtime? (core/continuity-runtime runtime-a)))
    (is (adapter/state? (core/state runtime-a)))
    (is (not (identical? (core/shell-runtime runtime-a)
                         (core/shell-runtime runtime-b))))
    (is (not (identical? (core/continuity-runtime runtime-a)
                         (core/continuity-runtime runtime-b))))
    (is (false? (:started? (core/diagnostics runtime-a))))))

(deftest framework-owned-physical-handlers-cannot-be-overridden-test
  (let [root (make-root "f")
        document (:document (make-document [root]))
        htmx (:htmx (make-htmx))]
    (doseq [kind core/protected-handler-kinds]
      (is (= :protected-handler-override
             (error-kind
              #(core/create
                {:document document
                 :htmx htmx
                 :handlers {kind (fn [_] nil)}})))))))

(deftest generic-handlers-cannot-replace-continuity-ownership-test
  (let [root (make-root "f")
        document (:document (make-document [root]))
        htmx (:htmx (make-htmx))
        continuity-kinds
        #{:continuity/capture
          :continuity/restore
          :continuity/release}
        error
        (thrown
         #(core/create
           {:document document
            :htmx htmx
            :handlers
            (into {}
                  (map (fn [kind]
                         [kind (fn [_] :application-handler)]))
                  continuity-kinds)}))]
    (is (= :protected-handler-override
           (some-> error ex-data :error/kind)))
    (is (= continuity-kinds
           (some-> error ex-data :effect-kinds)))
    (is (every? core/protected-handler-kinds continuity-kinds))))

(deftest dedicated-continuity-handler-seams-remain-extensible-test
  (let [capture-resource (js-obj)
        captures (atom [])
        restores (atom [])
        releases (atom [])
        {:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:continuity-capture!
          (fn [context]
            (swap! captures conj context)
            capture-resource)
          :continuity-restore!
          (fn [context]
            (swap! restores conj context)
            true)
          :continuity-release!
          (fn [context]
            (swap! releases conj context)
            true)})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (let [fixture (before-swap! runtime root xhr)]
      (is (false? @(:prevented? fixture)))
      (is (= 1 (count @captures)))
      (is (= 1 (:continuity
                (shell/resource-counts (core/shell-runtime runtime))))))
    (after-swap! runtime root xhr)
    (is (= 1 (count @restores)))
    (is (identical? capture-resource
                    (:resource (first @restores))))
    (is (empty? @releases))
    (is (= 0 (:continuity
              (shell/resource-counts (core/shell-runtime runtime)))))

    ;; The dedicated release seam is also application-extensible, but release
    ;; remains adapter-authorized. Retiring a captured fragment requests exactly
    ;; one physical release of the opaque application resource.
    (let [release-resource (js-obj)
          release-captures (atom [])
          release-restores (atom [])
          release-calls (atom [])
          {release-runtime :runtime release-root :root}
          (runtime-fixture
           "fragment-2"
           {:continuity-capture!
            (fn [context]
              (swap! release-captures conj context)
              release-resource)
            :continuity-restore!
            (fn [context]
              (swap! release-restores conj context)
              true)
            :continuity-release!
            (fn [context]
              (swap! release-calls conj context)
              true)})
          {release-xhr :xhr} (make-xhr 200)]
      (begin-refresh! release-runtime "fragment-2")
      (bind-request! release-runtime release-root release-xhr)
      (let [fixture (before-swap! release-runtime release-root release-xhr)]
        (is (false? @(:prevented? fixture))))
      (core/on-before-cleanup!
       release-runtime
       (:event
        (make-event
         "htmx:beforeCleanupElement"
         {:elt release-root
          :target release-root})))
      (is (= 1 (count @release-captures)))
      (is (empty? @release-restores))
      (is (= 1 (count @release-calls)))
      (is (identical? release-resource
                      (:resource (first @release-calls))))
      (is (= 0 (:continuity
                (shell/resource-counts
                 (core/shell-runtime release-runtime))))))))

(deftest nested-shell-handler-map-is-rejected-test
  (let [{:keys [document]} (make-document [])
        {:keys [htmx]} (make-htmx)]
    (is (= :nested-shell-handlers
           (error-kind
            #(core/create
              {:document document
               :htmx htmx
               :shell-options
               {:handlers {:diagnostic/ignored (fn [_] nil)}}}))))))

(deftest continuity-runtime-is-per-core-and-continuity-options-are-forwarded-test
  (let [capture-a (fn [_runtime _root _target _box] :captured-a)
        restore-a (fn [_runtime _root _target _box _state] :restored-a)
        {runtime-a :runtime}
        (runtime-fixture
         "a"
         {:continuity-options
          {:boxes
           {:custom-a
            {:capture capture-a
             :restore restore-a}}}})
        {runtime-b :runtime}
        (runtime-fixture "b")
        continuity-a (core/continuity-runtime runtime-a)
        continuity-b (core/continuity-runtime runtime-b)]
    (is (continuity/runtime? continuity-a))
    (is (continuity/runtime? continuity-b))
    (is (not (identical? continuity-a continuity-b)))
    (is (contains? (set (continuity/registered-box-types continuity-a))
                   "custom-a"))
    (is (not (contains? (set (continuity/registered-box-types continuity-b))
                        "custom-a")))

    ;; Registration through the accessor is physical and per-runtime only.
    (continuity/register-box!
     continuity-a
     :late-a
     {:capture capture-a
      :restore restore-a})
    (is (contains? (set (continuity/registered-box-types continuity-a))
                   "late-a"))
    (is (not (contains? (set (continuity/registered-box-types continuity-b))
                        "late-a")))))

(deftest invalid-continuity-options-fail-runtime-construction-test
  (let [{:keys [document]} (make-document [])
        {:keys [htmx]} (make-htmx)]
    (is (= :unknown-options
           (error-kind
            #(core/create
              {:document document
               :htmx htmx
               :continuity-options {:not-a-continuity-option true}}))))))

;; =============================================================================
;; Root / physical event normalization
;; =============================================================================

(deftest fragment-root-resolution-prefers-request-source-over-swap-target-test
  (let [root-a (make-root "a")
        root-b (make-root "b")
        child-a (make-child root-a)
        {:keys [event]}
        (make-event
         "htmx:beforeSwap"
         {:elt child-a
          :target root-b})]
    (is (identical? root-a
                    (core/fragment-root-from-event event)))
    (is (= "a" (core/fragment-id-from-root root-a)))
    (is (nil? (core/fragment-id-from-root child-a)))))

(deftest unrelated-htmx-event-has-no-managed-root-test
  (let [ordinary (make-element {})
        {:keys [event]}
        (make-event "htmx:beforeRequest"
                    {:elt ordinary
                     :target ordinary})]
    (is (nil? (core/fragment-root-from-event event)))))

(deftest request-success-normalization-is-physical-only-test
  (let [{ok :xhr} (make-xhr 204)
        {redirect :xhr} (make-xhr 302)
        {bad :xhr} (make-xhr 500)]
    (is (true?
         (core/request-successful?
          (:event (make-event "htmx:afterRequest" {:xhr ok})))))
    (is (true?
         (core/request-successful?
          (:event (make-event "htmx:afterRequest" {:xhr redirect})))))
    (is (false?
         (core/request-successful?
          (:event (make-event "htmx:afterRequest" {:xhr bad})))))
    (is (false?
         (core/request-successful?
          (:event
           (make-event
            "htmx:afterRequest"
            {:xhr ok
             :successful false})))))))

(deftest failure-reasons-are-transport-classification-only-test
  (is (= :http/response-error
         (core/failure-reason (:event (make-event "htmx:responseError")))))
  (is (= :http/send-error
         (core/failure-reason (:event (make-event "htmx:sendError")))))
  (is (= :http/timeout
         (core/failure-reason (:event (make-event "htmx:timeout")))))
  (is (= :http/aborted
         (core/failure-reason (:event (make-event "htmx:abort"))))))

;; =============================================================================
;; Explicit invalidation -> HTMX-owned refresh
;; =============================================================================

(deftest notify-fragment-triggers-htmx-and-establishes-pending-generation-test
  (let [{:keys [runtime root htmx-fixture]}
        (runtime-fixture "fragment-1")
        requirement
        (progression/requirement
         {:tx-id 7
          :system-time "2026-08-26T07:00:00Z"})]
    (begin-refresh! runtime "fragment-1" requirement)
    (let [pending (core/pending-refresh runtime root)
          call (first @(:calls htmx-fixture))]
      (is (= "fragment-1" (:fragment-id pending)))
      (is (pos-int? (:request-generation pending)))
      (is (= #{requirement}
             (:requirements pending)))
      (is (identical? root (:root call)))
      (is (= core/refresh-event-name (:name call)))
      (is (= "fragment-1"
             (aget (:detail call) "fragmentId")))
      (is (= (:request-generation pending)
             (aget (:detail call) "requestGeneration"))))))

(deftest missing-fragment-root-retires-semantic-fragment-instead-of-stranding-inflight-test
  (let [document-fixture (make-document [])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})]
    (let [result (core/notify-fragment! runtime "missing")]
      (is (= :dispatched (:status result)))
      (is (nil? (get-in (core/state runtime)
                        [:fragments "missing"])))
      (is (empty? @(:calls htmx-fixture))))))

(deftest explicit-dom-invalidation-without-requirement-remains-advisory-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        {:keys [event]}
        (make-event
         core/invalidated-event-name
         {:extra-detail
          {:fragmentId "fragment-1"}})]
    (is (true? (core/on-invalidated! runtime event)))
    (is (= #{}
           (:requirements (core/pending-refresh runtime root))))))

(deftest supplied-refresh-requirement-must-be-canonical-progression-test
  (doseq [invalid-requirement
          [nil
           :basis-a
           {:tx 7}
           {}]]
    (let [{:keys [runtime htmx-fixture]}
          (runtime-fixture "fragment-1")
          before (core/state runtime)
          error (thrown
                 #(core/notify-fragment!
                   runtime
                   "fragment-1"
                   invalid-requirement))
          data (ex-data error)]
      (testing (str "invalid supplied requirement fails before adapter admission: "
                    (pr-str invalid-requirement))
        (is (= :invalid-progression-requirement
               (:error/kind data)))
        (is (= invalid-requirement
               (:requirement data)))
        (is (= before (core/state runtime))
            "Rejected progression data must not mutate AdapterState.")
        (is (empty? @(:calls htmx-fixture))
            "Rejected progression data must not trigger a physical HTMX refresh.")))))

;; =============================================================================
;; Adapter-approved refresh progression -> HTMX request header
;; =============================================================================

(deftest config-request-carries-pending-progression-requirement-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        basis-a {:tx-id 41
                 :system-time "2026-08-25T20:00:41Z"}
        basis-b {:tx-id 42
                 :system-time "2026-08-25T20:00:42Z"}
        requirement
        (progression/compose
         (progression/requirement basis-a)
         (progression/requirement basis-b))
        headers (js-obj)
        fixture
        (make-event
         "htmx:configRequest"
         {:elt root
          :target root
          :extra-detail {:headers headers}})]
    (begin-refresh! runtime "fragment-1" requirement)
    (let [pending-before (core/pending-refresh runtime root)]
      (is (true? (core/on-config-request! runtime (:event fixture))))
      (is (= requirement
             (progression.http/decode-request-progression
              (aget headers progression.http/request-header-name))))
      (is (= pending-before
             (core/pending-refresh runtime root))
          "configRequest must carry but not consume the pending generation."))))

(deftest config-request-without-progression-does-not-invent-header-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        headers (js-obj)
        fixture
        (make-event
         "htmx:configRequest"
         {:elt root
          :target root
          :extra-detail {:headers headers}})]
    (begin-refresh! runtime "fragment-1")
    (is (true? (core/on-config-request! runtime (:event fixture))))
    (is (nil? (aget headers progression.http/request-header-name)))
    (is (some? (core/pending-refresh runtime root)))))

(deftest config-request-progression-fails-closed-when-headers-are-unavailable-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        requirement
        (progression/requirement
         {:tx-id 42
          :system-time "2026-08-25T20:00:42Z"})
        fixture
        (make-event
         "htmx:configRequest"
         {:elt root
          :target root})]
    (begin-refresh! runtime "fragment-1" requirement)
    (is (= :missing-config-request-headers
           (error-kind
            #(core/on-config-request! runtime (:event fixture)))))
    (is (true? @(:prevented? fixture)))
    (is (some? (core/pending-refresh runtime root))
        "A failed physical configuration must not consume semantic ownership.")))

;; =============================================================================
;; Managed SSE wakeup -> adapter invalidation -> HTMX-owned refresh
;; =============================================================================

(deftest managed-fragment-root-recognition-is-explicit-test
  (let [managed (make-managed-root "managed")
        spaced (make-element {:fragment-id "spaced"
                              :attrs {"hx-trigger"
                                      (str "  " core/refresh-event-name "  ")}})
        legacy (make-legacy-sse-root "legacy" "live-update")
        unrelated (make-root "plain")]
    (is (true? (core/managed-fragment-root? managed)))
    (is (true? (core/managed-fragment-root? spaced)))
    (is (false? (core/managed-fragment-root? legacy)))
    (is (false? (core/managed-fragment-root? unrelated)))))

(deftest managed-sse-message-is-cancelled-and-normalized-through-adapter-test
  (let [root (make-managed-root "fragment-1")
        listener (make-invalidation-listener root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)
          :request-id-fn (id-generator "request-1")})
        fixture
        (make-event
         core/sse-before-message-event-name
         {:elt listener
          :target listener})]
    (is (true? (core/on-sse-before-message! runtime (:event fixture))))
    (is (true? @(:prevented? fixture)))
    (let [pending (core/pending-refresh runtime root)
          calls @(:calls htmx-fixture)]
      (is (= "fragment-1" (:fragment-id pending)))
      (is (= #{} (:requirements pending)))
      (is (= 1 (count calls)))
      (is (identical? root (:root (first calls))))
      (is (= core/refresh-event-name (:name (first calls)))))))

(deftest managed-sse-progression-wire-is-decoded-before-adapter-invalidation-test
  (let [basis {:tx-id 42
               :system-time "2026-08-25T20:00:42Z"}
        requirement (progression/requirement basis)
        wire (progression/requirement->wire requirement)
        root (make-managed-root "fragment-1")
        listener (make-invalidation-listener root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})
        fixture
        (make-event
         core/sse-before-message-event-name
         {:elt listener
          :target listener
          :extra-detail
          {:data
           (pr-str
            {:progression wire
             :invalidation {:progression wire}})}})]
    (is (true? (core/on-sse-before-message! runtime (:event fixture))))
    (is (true? @(:prevented? fixture)))
    (is (= #{requirement}
           (:requirements (core/pending-refresh runtime root))))
    (is (= 1 (count @(:calls htmx-fixture))))
    (is (= requirement
           (first (:requirements (core/pending-refresh runtime root)))))
    (is (not= wire
              (first (:requirements (core/pending-refresh runtime root))))
        "Wire representation must not leak past browser Core into AdapterState.")))

(deftest managed-sse-progression-copies-must-both-be-present-and-agree-test
  (let [basis-a {:basis :a}
        basis-b {:basis :b}
        wire-a (progression/requirement->wire
                (progression/requirement basis-a))
        wire-b (progression/requirement->wire
                (progression/requirement basis-b))]
    (doseq [[label payload]
            [["top-level only"
              {:progression wire-a
               :invalidation {}}]
             ["nested only"
              {:invalidation {:progression wire-a}}]
             ["disagreeing copies"
              {:progression wire-a
               :invalidation {:progression wire-b}}]]]
      (testing label
        (let [root (make-managed-root "fragment-1")
              listener (make-invalidation-listener root "fragment-1")
              document-fixture (make-document [root])
              htmx-fixture (make-htmx)
              runtime
              (core/create
               {:document (:document document-fixture)
                :htmx (:htmx htmx-fixture)})
              fixture
              (make-event
               core/sse-before-message-event-name
               {:elt listener
                :target listener
                :extra-detail {:data (pr-str payload)}})]
          (is (= :inconsistent-sse-progression
                 (error-kind
                  #(core/on-sse-before-message! runtime (:event fixture)))))
          (is (true? @(:prevented? fixture)))
          (is (empty? @(:calls htmx-fixture)))
          (is (nil? (core/pending-refresh runtime root))))))))

(deftest malformed-or-unsupported-managed-sse-progression-fails-before-refresh-test
  (let [valid-wire
        (progression/requirement->wire
         (progression/requirement {:basis :valid}))
        unsupported-wire
        (assoc valid-wire
               :gesso.live.progression/version
               (inc progression/progression-version))]
    (doseq [[label payload]
            [["unsupported version"
              {:progression unsupported-wire
               :invalidation {:progression unsupported-wire}}]
             ["wrong wire type"
              {:progression
               (assoc valid-wire
                      :gesso.live.progression/type :forged/type)
               :invalidation
               {:progression
                (assoc valid-wire
                       :gesso.live.progression/type :forged/type)}}]]]
      (testing label
        (let [root (make-managed-root "fragment-1")
              listener (make-invalidation-listener root "fragment-1")
              document-fixture (make-document [root])
              htmx-fixture (make-htmx)
              runtime
              (core/create
               {:document (:document document-fixture)
                :htmx (:htmx htmx-fixture)})
              fixture
              (make-event
               core/sse-before-message-event-name
               {:elt listener
                :target listener
                :extra-detail {:data (pr-str payload)}})]
          (is (= :invalid-sse-progression
                 (error-kind
                  #(core/on-sse-before-message! runtime (:event fixture)))))
          (is (true? @(:prevented? fixture)))
          (is (empty? @(:calls htmx-fixture)))
          (is (nil? (core/pending-refresh runtime root))))))))

(deftest malformed-managed-sse-payload-fails-before-refresh-test
  (doseq [[label data]
          [["unreadable EDN" "{:progression"]
           ["non-map EDN" "[:not :a :live-event]"]]]
    (testing label
      (let [root (make-managed-root "fragment-1")
            listener (make-invalidation-listener root "fragment-1")
            document-fixture (make-document [root])
            htmx-fixture (make-htmx)
            runtime
            (core/create
             {:document (:document document-fixture)
              :htmx (:htmx htmx-fixture)})
            fixture
            (make-event
             core/sse-before-message-event-name
             {:elt listener
              :target listener
              :extra-detail {:data data}})]
        (is (= :invalid-sse-payload
               (error-kind
                #(core/on-sse-before-message! runtime (:event fixture)))))
        (is (true? @(:prevented? fixture)))
        (is (empty? @(:calls htmx-fixture)))
        (is (nil? (core/pending-refresh runtime root)))))))

(deftest repeated-managed-sse-wakeups-coalesce-before-request-start-test
  (let [root (make-managed-root "fragment-1")
        listener (make-invalidation-listener root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})]
    (dotimes [_ 3]
      (core/on-sse-before-message!
       runtime
       (:event
        (make-event
         core/sse-before-message-event-name
         {:elt listener
          :target listener}))))
    ;; No physical request has begun yet. Adapter owns the pending generation, so
    ;; duplicate wakeups cannot issue duplicate HTMX triggers around it.
    (is (= 1 (count @(:calls htmx-fixture))))
    (is (some? (core/pending-refresh runtime root)))))

(deftest mismatched-managed-sse-listener-fails-closed-before-refresh-test
  (let [root (make-managed-root "fragment-1")
        listener (make-invalidation-listener root "fragment-2")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})
        fixture
        (make-event
         core/sse-before-message-event-name
         {:elt listener
          :target listener})]
    (is (= :invalid-sse-invalidation-listener
           (error-kind #(core/on-sse-before-message! runtime (:event fixture)))))
    ;; The direct SSE mutation path is cancelled before identity validation. Even
    ;; malformed managed markup therefore cannot fall back to swapping payload.
    (is (true? @(:prevented? fixture)))
    (is (empty? @(:calls htmx-fixture)))
    (is (nil? (core/pending-refresh runtime root)))))

(deftest unmarked-legacy-sse-message-remains-outside-managed-path-test
  (let [root (make-legacy-sse-root "fragment-1" "live-update")
        child (make-child root)
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})
        fixture
        (make-event
         core/sse-before-message-event-name
         {:elt child
          :target child})]
    (is (true? (core/on-sse-before-message! runtime (:event fixture))))
    (is (false? @(:prevented? fixture)))
    (is (empty? @(:calls htmx-fixture)))
    (is (nil? (core/pending-refresh runtime root)))))

(deftest sse-open-refreshes-managed-root-and-ignores-legacy-root-test
  (let [managed-root (make-managed-root "managed")
        legacy-root (make-legacy-sse-root "legacy" "live-update")
        document-fixture (make-document [managed-root legacy-root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})]
    (is (true?
         (core/on-sse-open!
          runtime
          (:event
           (make-event
            core/sse-open-event-name
            {:elt managed-root
             :target managed-root})))))
    (is (= 1 (count @(:calls htmx-fixture))))
    (is (some? (core/pending-refresh runtime managed-root)))

    ;; Reopening before the pending request starts is another advisory wakeup,
    ;; not permission to create a second physical request.
    (core/on-sse-open!
     runtime
     (:event
      (make-event
       core/sse-open-event-name
       {:elt managed-root
        :target managed-root})))
    (is (= 1 (count @(:calls htmx-fixture))))

    (core/on-sse-open!
     runtime
     (:event
      (make-event
       core/sse-open-event-name
       {:elt legacy-root
        :target legacy-root})))
    (is (= 1 (count @(:calls htmx-fixture))))
    (is (nil? (core/pending-refresh runtime legacy-root)))))

;; =============================================================================
;; Managed / unmanaged request binding
;; =============================================================================

(deftest unmanaged-before-request-is-ignored-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        {xhr :xhr} (make-xhr 200)
        fixture
        (make-event
         "htmx:beforeRequest"
         {:elt root
          :target root
          :xhr xhr})]
    (is (true? (core/on-before-request! runtime (:event fixture))))
    (is (nil? (core/active-request runtime root)))
    (is (false? @(:prevented? fixture)))))

(deftest managed-before-request-consumes-pending-and-binds-exact-physical-request-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (let [generation (request-generation runtime root)]
      (bind-request! runtime root xhr)
      (is (nil? (core/pending-refresh runtime root)))
      (is (= {:fragment-id "fragment-1"
              :request-generation generation
              :request-id "request-1"
              :requirements #{}}
             (core/active-request runtime root)))
      (is (= "request-1"
             (get-in (core/state runtime)
                     [:fragments "fragment-1" :inflight :request-id]))))))

(deftest htmx-trigger-may-synchronously-enter-before-request-test
  (let [root (make-root "fragment-1")
        document-fixture (make-document [root])
        runtime* (atom nil)
        xhr-fixture (make-xhr 200)
        htmx-fixture
        (make-htmx
         (fn [trigger-root name _detail]
           (when (= core/refresh-event-name name)
             (core/on-before-request!
              @runtime*
              (:event
               (make-event
                "htmx:beforeRequest"
                {:elt trigger-root
                 :target trigger-root
                 :xhr (:xhr xhr-fixture)}))))))
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)
          :request-id-fn (id-generator "sync-request")})]
    (reset! runtime* runtime)
    (core/notify-fragment! runtime "fragment-1")
    (is (nil? (core/pending-refresh runtime root)))
    (is (= "sync-request"
           (:request-id (core/active-request runtime root))))
    (is (= "sync-request"
           (get-in (core/state runtime)
                   [:fragments "fragment-1" :inflight :request-id])))))

;; =============================================================================
;; Fail-closed FFI behavior
;; =============================================================================

(deftest request-id-generation-failure-must-prevent-the-unowned-htmx-request-test
  (let [root (make-root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)
          :request-id-fn
          (fn []
            (throw (js/Error. "id generator failed")))})
        {xhr :xhr} (make-xhr 200)
        fixture
        (make-event
         "htmx:beforeRequest"
         {:elt root
          :target root
          :xhr xhr})]
    (begin-refresh! runtime "fragment-1")
    (let [generation (request-generation runtime root)]
      (is (some? (thrown #(core/on-before-request! runtime (:event fixture)))))
      (is (true? @(:prevented? fixture))
          "A failure before semantic request binding must still fail the physical request closed.")
      (is (= generation
             (:request-generation (core/pending-refresh runtime root)))
          "A pre-binding host failure must not silently consume the only pending correlation.")
      (is (nil? (core/active-request runtime root))))))

(deftest malformed-generated-request-id-must-also-fail-closed-test
  (let [root (make-root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)
          :request-id-fn (constantly "   ")})
        {xhr :xhr} (make-xhr 200)
        fixture
        (make-event
         "htmx:beforeRequest"
         {:elt root
          :target root
          :xhr xhr})]
    (begin-refresh! runtime "fragment-1")
    (is (some? (thrown #(core/on-before-request! runtime (:event fixture)))))
    (is (true? @(:prevented? fixture)))
    (is (some? (core/pending-refresh runtime root)))
    (is (nil? (core/active-request runtime root)))))

(deftest authoritative-parser-failure-must-cancel-swap-test
  (let [{:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:authoritative-from-event
          (fn [_]
            (throw (js/Error. "bad response metadata")))})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (let [fixture
          (make-event
           "htmx:beforeSwap"
           {:elt root
            :target root
            :xhr xhr
            :should-swap true})]
      (is (some? (thrown #(core/on-before-swap! runtime (:event fixture)))))
      (is (true? @(:prevented? fixture))
          "A parser/normalization failure at beforeSwap must physically suppress the swap.")
      (is (false? (aget (:detail fixture) "shouldSwap"))))))

(deftest malformed-authoritative-parser-result-must-cancel-swap-test
  (let [{:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:authoritative-from-event (constantly "not-plain-data")})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (let [fixture
          (make-event
           "htmx:beforeSwap"
           {:elt root
            :target root
            :xhr xhr
            :should-swap true})]
      (is (= :invalid-authoritative-parser-result
             (error-kind
              #(core/on-before-swap! runtime (:event fixture)))))
      (is (true? @(:prevented? fixture)))
      (is (false? (aget (:detail fixture) "shouldSwap"))))))

;; =============================================================================
;; Swap / authoritative carrier behavior
;; =============================================================================

(deftest ordinary-managed-swap-captures-and-restores-continuity-without-owning-semantics-test
  (let [captures (atom [])
        restores (atom [])
        releases (atom [])
        {:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:continuity-capture!
          (fn [context]
            (swap! captures conj context)
            #js {:snapshot "opaque"})
          :continuity-restore!
          (fn [context]
            (swap! restores conj context)
            true)
          :continuity-release!
          (fn [context]
            (swap! releases conj context)
            true)})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (let [fixture (before-swap! runtime root xhr)]
      (is (false? @(:prevented? fixture)))
      (is (= 1 (count @captures)))
      (is (= 1 (:continuity (shell/resource-counts (core/shell-runtime runtime))))))
    (after-swap! runtime root xhr)
    (is (= 1 (count @restores)))
    (is (= 0 (:continuity (shell/resource-counts (core/shell-runtime runtime)))))
    (is (empty? @releases))))

(deftest default-core-continuity-is-shell-owned-and-completes-through-adapter-test
  (async done
    (let [{:keys [runtime root]}
          (runtime-fixture "fragment-1")
          {xhr :xhr} (make-xhr 200)]
      (begin-refresh! runtime "fragment-1")
      (bind-request! runtime root xhr)
      (let [fixture (before-swap! runtime root xhr)
            physical-resources
            (vals (:continuity
                   (shell/resources (core/shell-runtime runtime))))
            resource (first physical-resources)]
        (is (false? @(:prevented? fixture)))
        (is (= 1 (count physical-resources)))
        (is (continuity/resource? resource))
        ;; The fake root has no continuity metadata. The real default runtime
        ;; still returns an opaque disabled resource rather than a sentinel or
        ;; semantic value.
        (is (false? (:enabled? resource))))

      (after-swap! runtime root xhr)

      ;; continuity/restore! returns a Promise even for disabled resources.
      ;; The shell alone translates its completion into the exact adapter slot
      ;; completion and releases physical ownership.
      (js/setTimeout
       (fn []
         (try
           (is (= 0
                  (:continuity
                   (shell/resource-counts (core/shell-runtime runtime)))))
           (is (empty? (:continuity (core/state runtime))))
           (done)
           (catch :default error
             (is false (str "async assertion failed: " (.-message error)))
             (done))))
       0))))

(deftest authoritative-candidate-is-installed-only-after-adapter-approved-swap-and-after-swap-test
  (let [candidate* (atom {:scope :request/id-1
                          :basis :basis-1})
        installed (atom [])
        {:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:authoritative-from-event (fn [_] @candidate*)
          :handlers
          {:authoritative/installed
           (fn [{:keys [effect]}]
             (swap! installed conj effect))}})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (before-swap! runtime root xhr)
    (is (nil? (adapter/authoritative-frontier
               (core/state runtime)
               :request/id-1)))
    (after-swap! runtime root xhr)
    (is (= {:basis :basis-1}
           (adapter/authoritative-frontier
            (core/state runtime)
            :request/id-1)))
    (is (= [{:scope :request/id-1
             :basis :basis-1
             :fragment-id "fragment-1"
             :request-generation
             (:request-generation (core/active-request runtime root))}]
           @installed))))

(deftest nonmonotone-authoritative-candidate-is-physically-prevented-test
  (let [candidate* (atom {:scope :request/id-1
                          :basis :basis-1})
        {:keys [runtime root]}
        (runtime-fixture
         "fragment-1"
         {:authoritative-from-event (fn [_] @candidate*)})
        {xhr-a :xhr} (make-xhr 200)
        {xhr-b :xhr} (make-xhr 200)]
    ;; Establish basis-1.
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr-a)
    (before-swap! runtime root xhr-a)
    (after-swap! runtime root xhr-a)
    (after-request! runtime root xhr-a true)

    ;; A distinct opaque basis without an exact progression witness cannot win.
    (reset! candidate* {:scope :request/id-1
                        :basis :basis-2})
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr-b)
    (let [fixture (before-swap! runtime root xhr-b)]
      (is (true? @(:prevented? fixture)))
      (is (false? (aget (:detail fixture) "shouldSwap")))
      (is (= {:basis :basis-1}
             (adapter/authoritative-frontier
              (core/state runtime)
              :request/id-1))))))

;; =============================================================================
;; Exact XHR correlation / re-entry
;; =============================================================================

(deftest queued-refresh-started-during-after-request-is-not-erased-by-old-cleanup-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        requirement-a
        (progression/requirement
         {:tx-id 201
          :system-time "2026-08-26T08:10:01Z"})
        requirement-b
        (progression/requirement
         {:tx-id 202
          :system-time "2026-08-26T08:10:02Z"})
        {xhr-a :xhr} (make-xhr 200)
        {xhr-b :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1" requirement-a)
    (bind-request! runtime root xhr-a)

    ;; While A is active, invalidate again. Adapter queues instead of starting B.
    (begin-refresh! runtime "fragment-1" requirement-b)
    (is (nil? (core/pending-refresh runtime root)))

    ;; Completing A synchronously emits the next :fragment/refresh. Core must not
    ;; let cleanup for A erase B's new pending generation.
    (after-request! runtime root xhr-a true)
    (let [pending-b (core/pending-refresh runtime root)]
      (is (some? pending-b))
      (is (= #{requirement-b} (:requirements pending-b))))

    (bind-request! runtime root xhr-b)
    (is (= "request-2"
           (:request-id (core/active-request runtime root))))))

(deftest queued-progression-requirements-become-next-config-request-header-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        basis-a {:tx-id 101
                 :system-time "2026-08-26T08:00:01Z"}
        basis-b {:tx-id 102
                 :system-time "2026-08-26T08:00:02Z"}
        basis-c {:tx-id 103
                 :system-time "2026-08-26T08:00:03Z"}
        requirement-a (progression/requirement basis-a)
        requirement-b (progression/requirement basis-b)
        requirement-c (progression/requirement basis-c)
        {xhr-a :xhr} (make-xhr 200)
        headers-a (js-obj)
        headers-b (js-obj)]
    ;; Generation A owns only requirement A, and configRequest carries exactly A.
    (begin-refresh! runtime "fragment-1" requirement-a)
    (is (true?
         (core/on-config-request!
          runtime
          (:event
           (make-event
            "htmx:configRequest"
            {:elt root
             :target root
             :extra-detail {:headers headers-a}})))))
    (is (= requirement-a
           (progression.http/decode-request-progression
            (aget headers-a progression.http/request-header-name))))
    (bind-request! runtime root xhr-a)

    ;; B and C arrive while A is active. They must remain queued, and must not
    ;; leak backward into A's already-configured physical request.
    (begin-refresh! runtime "fragment-1" requirement-b)
    (begin-refresh! runtime "fragment-1" requirement-c)
    (is (nil? (core/pending-refresh runtime root)))
    (is (= #{requirement-b requirement-c}
           (get-in (core/state runtime)
                   [:fragments "fragment-1" :queued-requirements])))
    (is (= requirement-a
           (progression.http/decode-request-progression
            (aget headers-a progression.http/request-header-name))))

    ;; Completing A promotes the complete queued set into one new generation.
    ;; The next configRequest must therefore carry exactly B+C, preserving
    ;; incomparability rather than weakening the requirement or reusing A.
    (after-request! runtime root xhr-a true)
    (let [pending-b (core/pending-refresh runtime root)
          expected (progression/compose requirement-b requirement-c)]
      (is (= #{requirement-b requirement-c}
             (:requirements pending-b)))
      (is (true?
           (core/on-config-request!
            runtime
            (:event
             (make-event
              "htmx:configRequest"
              {:elt root
               :target root
               :extra-detail {:headers headers-b}})))))
      (is (= expected
             (progression.http/decode-request-progression
              (aget headers-b progression.http/request-header-name))))
      (is (= #{basis-b basis-c}
             (:bases
              (progression.http/decode-request-progression
               (aget headers-b progression.http/request-header-name)))))
      (is (= pending-b (core/pending-refresh runtime root))
          "Configuring the next physical request must not consume generation B."))))

(deftest late-xhr-callback-from-request-a-cannot-fail-newer-request-b-on-same-root-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        queued-requirement
        (progression/requirement
         {:tx-id 301
          :system-time "2026-08-26T08:20:01Z"})
        {xhr-a :xhr} (make-xhr 200)
        {xhr-b :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr-a)
    (begin-refresh! runtime "fragment-1" queued-requirement)
    (after-request! runtime root xhr-a true)
    (bind-request! runtime root xhr-b)

    (let [active-b (core/active-request runtime root)
          late-a
          (:event
           (make-event
            "htmx:sendError"
            {:elt root
             :target root
             :xhr xhr-a}))]
      (is (true? (core/on-request-failed! runtime late-a)))
      (is (= active-b (core/active-request runtime root)))
      (is (= (:request-id active-b)
             (get-in (core/state runtime)
                     [:fragments "fragment-1" :inflight :request-id]))))))

(deftest request-failure-removes-only-the-exact-current-physical-correlation-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        {xhr :xhr} (make-xhr 500)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (is (some? (core/active-request runtime root)))
    (core/on-request-failed!
     runtime
     (:event
      (make-event
       "htmx:responseError"
       {:elt root
        :target root
        :xhr xhr})))
    (is (nil? (core/active-request runtime root)))
    (is (nil? (get-in (core/state runtime)
                      [:fragments "fragment-1" :inflight])))

    ;; Duplicate afterRequest for the same XHR is now unrelated/stale physically.
    (after-request! runtime root xhr false)
    (is (nil? (core/active-request runtime root)))))

;; =============================================================================
;; DOM cleanup / semantic retirement
;; =============================================================================

(deftest before-cleanup-retires-root-and-nested-managed-fragments-test
  (let [child-root (make-root "child")
        parent-root (make-root "parent" [child-root])
        document-fixture (make-document [parent-root child-root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)
          :request-id-fn (id-generator "p" "c")})]
    (begin-refresh! runtime "parent")
    (begin-refresh! runtime "child")
    (is (some? (get-in (core/state runtime) [:fragments "parent"])))
    (is (some? (get-in (core/state runtime) [:fragments "child"])))

    (core/on-before-cleanup!
     runtime
     (:event
      (make-event
       "htmx:beforeCleanupElement"
       {:elt parent-root
        :target parent-root})))

    (is (nil? (get-in (core/state runtime) [:fragments "parent"])))
    (is (nil? (get-in (core/state runtime) [:fragments "child"])))
    (is (nil? (core/pending-refresh runtime parent-root)))
    (is (nil? (core/pending-refresh runtime child-root)))))

(deftest cleanup-of-active-fragment-aborts-exact-xhr-and-retires-fragment-test
  (let [{:keys [runtime root]}
        (runtime-fixture "fragment-1")
        {xhr :xhr aborted? :aborted?} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (core/on-before-cleanup!
     runtime
     (:event
      (make-event
       "htmx:beforeCleanupElement"
       {:elt root
        :target root})))
    (is (true? @aborted?))
    (is (nil? (core/active-request runtime root)))
    (is (nil? (get-in (core/state runtime)
                      [:fragments "fragment-1"])))))

;; =============================================================================
;; Listener ownership / diagnostics
;; =============================================================================

(deftest start-installs-each-documented-listener-once-and-stop-removes-it-test
  (let [root (make-root "fragment-1")
        document-fixture (make-document [root])
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})]
    (is (identical? runtime (core/start! runtime)))
    (is (identical? runtime (core/start! runtime)))
    (is (= (count core/listener-specs)
           (count @(:added document-fixture))))
    (is (= (set (map first core/listener-specs))
           (set (map first @(:added document-fixture)))))
    (is (true? (:started? (core/diagnostics runtime))))

    (is (= :stopped (core/stop! runtime)))
    (is (= (count core/listener-specs)
           (count @(:removed document-fixture))))
    (is (= (set @(:added document-fixture))
           (set @(:removed document-fixture))))
    (is (false? (:started? (core/diagnostics runtime))))))

(deftest diagnostics-never-expose-dom-htmx-or-xhr-host-resources-test
  (let [capture-fn (fn [_runtime _root _target _box] :captured)
        restore-fn (fn [_runtime _root _target _box _state] :restored)
        {:keys [runtime root document-fixture htmx-fixture]}
        (runtime-fixture
         "fragment-1"
         {:continuity-options
          {:boxes
           {:diagnostic-only-name
            {:capture capture-fn
             :restore restore-fn}}}})
        {xhr :xhr} (make-xhr 200)]
    (begin-refresh! runtime "fragment-1")
    (bind-request! runtime root xhr)
    (let [diagnostics (core/diagnostics runtime)]
      (is (false? (deep-identical? diagnostics root)))
      (is (false? (deep-identical? diagnostics xhr)))
      (is (false? (deep-identical? diagnostics (:document document-fixture))))
      (is (false? (deep-identical? diagnostics (:htmx htmx-fixture))))
      (is (false? (deep-identical? diagnostics capture-fn)))
      (is (false? (deep-identical? diagnostics restore-fn)))
      (is (= (continuity/diagnostics (core/continuity-runtime runtime))
             (:continuity diagnostics)))
      (is (contains? (set (get-in diagnostics
                                  [:continuity :registered-box-types]))
                     "diagnostic-only-name"))
      (is (adapter/state? (core/state runtime))))))

;; =============================================================================
;; Shared document-event observation seam
;; =============================================================================

(defn- installed-listener
  [document-fixture event-name]
  (some (fn [[name handler capture? :as registration]]
          (when (= event-name name)
            {:handler handler
             :capture? capture?
             :registration registration}))
        @(:added document-fixture)))

(deftest event-observer-seam-uses-only-core-owned-document-listeners-test
  (let [document-fixture (make-document [])
        htmx-fixture (make-htmx)
        runtime (core/create {:document (:document document-fixture)
                              :htmx (:htmx htmx-fixture)})
        seen (atom [])]
    (is (= :optimistic/config
           (core/register-event-observer!
            runtime
            "htmx:configRequest"
            :optimistic/config
            #(swap! seen conj %))))
    (is (= {"htmx:configRequest" #{:optimistic/config}}
           (core/event-observers runtime)))
    (is (empty? @(:added document-fixture))
        "Registering a physical observer must not acquire a document listener.")

    (core/start! runtime)
    (is (= (count core/listener-specs)
           (count @(:added document-fixture))))
    (let [{:keys [handler capture?]}
          (installed-listener document-fixture "htmx:configRequest")
          event (js-obj)]
      (is (fn? handler))
      (is (false? capture?))
      (is (true? (handler event)))
      (is (= 1 (count @seen)))
      (is (identical? event (first @seen))))

    (is (= :optimistic/config
           (core/unregister-event-observer!
            runtime
            "htmx:configRequest"
            :optimistic/config)))
    (is (empty? (core/event-observers runtime)))
    (is (= (count core/listener-specs)
           (count @(:added document-fixture)))
        "Removing an observer must not alter Core's document-listener ownership.")
    (core/stop! runtime)))

(deftest observer-replacement-is-exactly-keyed-by-event-and-owner-test
  (let [document-fixture (make-document [])
        htmx-fixture (make-htmx)
        runtime (core/create {:document (:document document-fixture)
                              :htmx (:htmx htmx-fixture)})
        seen (atom [])
        old-handler #(swap! seen conj [:old %])
        new-handler #(swap! seen conj [:new %])]
    (core/register-event-observer!
     runtime "htmx:beforeSend" :optimistic/send old-handler)
    (core/register-event-observer!
     runtime "htmx:beforeSend" :optimistic/send new-handler)
    (core/register-event-observer!
     runtime "htmx:configRequest" :optimistic/send old-handler)
    (is (= {"htmx:beforeSend" #{:optimistic/send}
            "htmx:configRequest" #{:optimistic/send}}
           (core/event-observers runtime)))

    (core/start! runtime)
    (let [event (js-obj)]
      ((:handler (installed-listener document-fixture "htmx:beforeSend")) event)
      (is (= 1 (count @seen)))
      (is (= :new (ffirst @seen)))
      (is (identical? event (second (first @seen)))))

    (core/unregister-event-observer!
     runtime "htmx:beforeSend" :optimistic/send)
    (is (= {"htmx:configRequest" #{:optimistic/send}}
           (core/event-observers runtime))
        "Removing one event/owner pair must not remove the same owner from another event.")
    (core/stop! runtime)))

(deftest observer-delivery-snapshots-registration-for-the-current-browser-event-test
  (let [document-fixture (make-document [])
        htmx-fixture (make-htmx)
        runtime (core/create {:document (:document document-fixture)
                              :htmx (:htmx htmx-fixture)})
        calls (atom {:a 0 :b 0 :c 0})
        c-handler (fn [_] (swap! calls update :c inc))]
    (core/register-event-observer!
     runtime
     "htmx:beforeSend"
     :a
     (fn [_]
       (swap! calls update :a inc)
       (core/unregister-event-observer! runtime "htmx:beforeSend" :b)
       (core/register-event-observer! runtime "htmx:beforeSend" :c c-handler)))
    (core/register-event-observer!
     runtime "htmx:beforeSend" :b
     (fn [_] (swap! calls update :b inc)))

    (core/start! runtime)
    (let [handler (:handler (installed-listener document-fixture "htmx:beforeSend"))]
      (handler (js-obj))
      (is (= {:a 1 :b 1 :c 0} @calls)
          "Observer mutation during delivery must affect only later browser events.")
      (is (= {"htmx:beforeSend" #{:a :c}}
             (core/event-observers runtime)))

      (handler (js-obj))
      (is (= {:a 2 :b 1 :c 1} @calls)))
    (core/stop! runtime)))

(deftest observers-run-before-core-request-normalization-test
  (let [{:keys [runtime root document-fixture]} (runtime-fixture "fragment-1")
        {xhr :xhr} (make-xhr 200)
        observed (atom nil)]
    (begin-refresh! runtime "fragment-1")
    (let [generation (request-generation runtime root)]
      (core/register-event-observer!
       runtime
       "htmx:beforeRequest"
       :optimistic/correlation
       (fn [event]
         (reset! observed
                 {:same-event? (identical? event event)
                  :pending (core/pending-refresh runtime root)
                  :active (core/active-request runtime root)})))
      (core/start! runtime)
      (let [fixture (make-event "htmx:beforeRequest"
                                {:elt root
                                 :target root
                                 :xhr xhr})]
        (is (true?
             ((:handler (installed-listener document-fixture "htmx:beforeRequest"))
              (:event fixture))))
        (is (= generation
               (get-in @observed [:pending :request-generation])))
        (is (nil? (:active @observed))
            "The observer must run before Core consumes the pending refresh.")
        (is (nil? (core/pending-refresh runtime root)))
        (is (= generation
               (:request-generation (core/active-request runtime root))))))
    (core/stop! runtime)))

(deftest observer-exception-fails-before-core-built-in-normalization-test
  (let [{:keys [runtime root document-fixture]} (runtime-fixture "fragment-1")
        {xhr :xhr} (make-xhr 200)
        fixture (make-event "htmx:beforeRequest"
                            {:elt root
                             :target root
                             :xhr xhr})]
    (begin-refresh! runtime "fragment-1")
    (let [generation (request-generation runtime root)]
      (core/register-event-observer!
       runtime
       "htmx:beforeRequest"
       :failing/integration
       (fn [event]
         (.preventDefault event)
         (throw (ex-info "observer failed" {:owner :failing/integration}))))
      (core/start! runtime)
      (is (some?
           (thrown
            #((:handler (installed-listener document-fixture "htmx:beforeRequest"))
              (:event fixture)))))
      (is (true? @(:prevented? fixture)))
      (is (= generation
             (:request-generation (core/pending-refresh runtime root)))
          "Core built-in request normalization must not run after an observer failure.")
      (is (nil? (core/active-request runtime root))))
    (core/stop! runtime)))

(deftest event-observer-diagnostics-expose-identities-not-callbacks-test
  (let [{:keys [runtime]} (runtime-fixture "fragment-1")
        handler-a (fn [_] :a)
        handler-b (fn [_] :b)]
    (core/register-event-observer!
     runtime "htmx:configRequest" :optimistic/config handler-a)
    (core/register-event-observer!
     runtime "htmx:beforeSend" :optimistic/send handler-b)
    (let [diagnostics (core/diagnostics runtime)]
      (is (= {"htmx:configRequest" #{:optimistic/config}
              "htmx:beforeSend" #{:optimistic/send}}
             (:event-observers diagnostics)))
      (is (false? (deep-identical? diagnostics handler-a)))
      (is (false? (deep-identical? diagnostics handler-b))))))

(deftest event-observer-registration-validates-the-owned-listener-contract-test
  (let [{:keys [runtime]} (runtime-fixture "fragment-1")]
    (is (= :unsupported-observed-event
           (error-kind
            #(core/register-event-observer! runtime "click" :x identity))))
    (is (= :invalid-observer-id
           (error-kind
            #(core/register-event-observer!
              runtime "htmx:configRequest" "x" identity))))
    (is (= :invalid-callable
           (error-kind
            #(core/register-event-observer!
              runtime "htmx:configRequest" :x :not-a-function))))))

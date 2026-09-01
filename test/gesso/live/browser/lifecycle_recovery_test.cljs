(ns gesso.live.browser.lifecycle-recovery-test
  "Browser lifecycle recovery contract for adapter-managed Live fragments.

   Managed fragments deliberately do not put pageshow/online/visibilitychange
   directly in hx-trigger. Those physical lifecycle signals must instead enter
   browser.core, become advisory fragment invalidations, and pass through the
   same AdapterState generation/single-flight rules as SSE wakeups.

   This namespace is intentionally small and host-fakeable. Real Chromium tests
   can separately prove browser event delivery, while these tests pin the
   semantic ownership boundary and coalescing behavior."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.core :as core]))

;; =============================================================================
;; Fake DOM / HTMX host
;; =============================================================================

(defn- make-element
  [fragment-id trigger]
  (let [element (js-obj)
        attrs {core/fragment-attribute fragment-id
               "hx-trigger" trigger}]
    (aset element "nodeType" 1)
    (aset element "children" (array))
    (aset element
          "getAttribute"
          (fn [name]
            (get attrs name)))
    (aset element
          "closest"
          (fn [selector]
            (when (= selector core/fragment-selector)
              element)))
    (aset element
          "querySelectorAll"
          (fn [_selector]
            (array)))
    element))

(defn- make-managed-root
  [fragment-id]
  (make-element fragment-id core/refresh-event-name))

(defn- make-legacy-root
  [fragment-id]
  (make-element fragment-id "sse:live-update"))

(defn- make-document
  [roots]
  (let [roots* (atom (vec roots))
        added (atom [])
        removed (atom [])
        document (js-obj)]
    (aset document "visibilityState" "hidden")
    (aset document
          "querySelectorAll"
          (fn [selector]
            (if (= selector core/fragment-selector)
              (to-array @roots*)
              (array))))
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
  []
  (let [calls (atom [])
        htmx (js-obj)]
    (aset htmx
          "trigger"
          (fn [root name detail]
            (swap! calls conj
                   {:root root
                    :name name
                    :detail detail})
            true))
    {:htmx htmx
     :calls calls}))

(defn- make-event
  ([type]
   (make-event type nil))
  ([type properties]
   (let [event (js-obj)]
     (aset event "type" type)
     (doseq [[k v] properties]
       (aset event (name k) v))
     event)))

(defn- lifecycle-fixture
  [roots]
  (let [document-fixture (make-document roots)
        htmx-fixture (make-htmx)
        runtime
        (core/create
         {:document (:document document-fixture)
          :htmx (:htmx htmx-fixture)})]
    {:runtime runtime
     :document-fixture document-fixture
     :htmx-fixture htmx-fixture}))

(defn- installed-entry
  [document-fixture event-name]
  (some
   (fn [[name handler capture? :as entry]]
     (when (= event-name name)
       entry))
   @(:added document-fixture)))

(defn- installed-handler
  [document-fixture event-name]
  (second (installed-entry document-fixture event-name)))

(defn- refresh-calls
  [htmx-fixture]
  (filterv
   #(= core/refresh-event-name (:name %))
   @(:calls htmx-fixture)))

(defn- pending-requirements
  [runtime root]
  (:requirements
   (core/pending-refresh runtime root)))

;; =============================================================================
;; Lifecycle ownership
;; =============================================================================

(deftest lifecycle-repair-events-belong-to-core-owned-listener-set-test
  (let [events (set (map first core/listener-specs))]
    (testing "online recovery is owned by browser.core"
      (is (contains? events "online")))

    (testing "bfcache/pageshow recovery is owned by browser.core"
      (is (contains? events "pageshow")))

    (testing "foreground visibility recovery is owned by browser.core"
      (is (contains? events "visibilitychange")))))

(deftest start-stop-own-lifecycle-listeners-exactly-test
  (let [{:keys [runtime document-fixture]}
        (lifecycle-fixture [])]
    (core/start! runtime)
    (let [entries
          (keep
           #(installed-entry document-fixture %)
           ["online" "pageshow" "visibilitychange"])]
      (is (= 3 (count entries))
          "Core must physically install all three lifecycle-recovery listeners.")

      (core/stop! runtime)

      (doseq [entry entries]
        (is (some #(= entry %)
                  @(:removed document-fixture))
            "Core stop must remove the exact handler/capture tuple it installed.")))))

;; =============================================================================
;; Advisory convergence repair
;; =============================================================================

(deftest online-recovery-invalidates-all-managed-fragments-test
  (let [managed-a (make-managed-root "managed-a")
        managed-b (make-managed-root "managed-b")
        legacy (make-legacy-root "legacy")
        {:keys [runtime document-fixture htmx-fixture]}
        (lifecycle-fixture [managed-a managed-b legacy])]
    (core/start! runtime)
    (try
      (let [handler (installed-handler document-fixture "online")]
        (is (fn? handler)
            "The online recovery listener must be installed by Core.")
        (when handler
          (handler (make-event "online"))))

      (let [calls (refresh-calls htmx-fixture)]
        (is (= 2 (count calls))
            "Online recovery must advisory-refresh each managed root exactly once.")
        (is (= #{managed-a managed-b}
               (set (map :root calls))))
        (is (= #{} (pending-requirements runtime managed-a)))
        (is (= #{} (pending-requirements runtime managed-b)))
        (is (nil? (core/pending-refresh runtime legacy))
            "Legacy direct-SSE roots remain outside adapter-managed lifecycle repair."))
      (finally
        (core/stop! runtime)))))

(deftest persisted-pageshow-repairs-but-ordinary-pageshow-does-not-test
  (let [managed (make-managed-root "managed")
        {:keys [runtime document-fixture htmx-fixture]}
        (lifecycle-fixture [managed])]
    (core/start! runtime)
    (try
      (let [handler (installed-handler document-fixture "pageshow")]
        (is (fn? handler)
            "The pageshow recovery listener must be installed by Core.")

        (when handler
          (handler
           (make-event "pageshow" {:persisted false})))
        (is (empty? (refresh-calls htmx-fixture))
            "Ordinary pageshow must not duplicate normal initial-load/SSE-open recovery.")

        (when handler
          (handler
           (make-event "pageshow" {:persisted true})))
        (is (= 1 (count (refresh-calls htmx-fixture)))
            "Persisted pageshow represents bfcache restoration and must repair authority.")
        (is (= #{}
               (pending-requirements runtime managed))
            "Lifecycle repair is advisory and must not invent progression authority."))
      (finally
        (core/stop! runtime)))))

(deftest visibility-recovery-runs-only-when-document-becomes-visible-test
  (let [managed (make-managed-root "managed")
        {:keys [runtime document-fixture htmx-fixture]}
        (lifecycle-fixture [managed])
        document (:document document-fixture)]
    (core/start! runtime)
    (try
      (let [handler (installed-handler document-fixture "visibilitychange")]
        (is (fn? handler)
            "The visibilitychange recovery listener must be installed by Core.")

        (aset document "visibilityState" "hidden")
        (when handler
          (handler (make-event "visibilitychange")))
        (is (empty? (refresh-calls htmx-fixture))
            "Hiding/backgrounding is not itself permission to refresh.")

        (aset document "visibilityState" "visible")
        (when handler
          (handler (make-event "visibilitychange")))
        (is (= 1 (count (refresh-calls htmx-fixture)))
            "Foreground/resume must create one advisory repair opportunity.")
        (is (= #{}
               (pending-requirements runtime managed))))
      (finally
        (core/stop! runtime)))))

(deftest lifecycle-repair-signals-coalesce-through-adapter-test
  (let [managed (make-managed-root "managed")
        {:keys [runtime document-fixture htmx-fixture]}
        (lifecycle-fixture [managed])
        document (:document document-fixture)]
    (core/start! runtime)
    (try
      (let [online-handler
            (installed-handler document-fixture "online")
            pageshow-handler
            (installed-handler document-fixture "pageshow")
            visibility-handler
            (installed-handler document-fixture "visibilitychange")]
        (is (every? fn?
                    [online-handler pageshow-handler visibility-handler])
            "All lifecycle recovery handlers must be physically installed.")

        ;; Fire three distinct repair boundaries before HTMX has begun the
        ;; adapter-issued physical request. They are three advisory facts about
        ;; possible staleness, not permission for three parallel GETs.
        (when online-handler
          (online-handler (make-event "online")))

        (when pageshow-handler
          (pageshow-handler
           (make-event "pageshow" {:persisted true})))

        (aset document "visibilityState" "visible")
        (when visibility-handler
          (visibility-handler (make-event "visibilitychange"))))

      (is (= 1 (count (refresh-calls htmx-fixture)))
          "Repeated lifecycle repair signals must collapse behind one adapter generation.")
      (is (= #{}
             (pending-requirements runtime managed))
          "Coalesced lifecycle repair remains advisory and authority-free.")
      (finally
        (core/stop! runtime)))))

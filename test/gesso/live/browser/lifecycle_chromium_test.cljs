(ns gesso.live.browser.lifecycle-chromium-test
  "Native-browser lifecycle recovery regressions.

   The portable lifecycle-recovery namespace uses fake Window and Document
   event targets so the semantic contract remains deterministic under Node.
   This namespace adds the complementary browser-host check: when executed by
   the ordinary real-Chromium cljs.test corpus, actual Window/Document event
   dispatch must reach the production browser Core listeners.

   The namespace is intentionally auto-discoverable as a normal *_test.cljs
   file. Under Node it performs a small explicit skip assertion; under Chromium
   it exercises the real DOM and browser event targets."
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [gesso.live.browser.core :as core]))

(defn- browser-host?
  []
  (and (exists? js/window)
       (exists? js/document)))

(defn- make-htmx
  []
  (let [calls (atom [])
        htmx (js-obj)]
    (aset htmx
          "trigger"
          (fn [root event-name detail]
            (swap! calls conj
                   {:root root
                    :event-name event-name
                    :detail detail})
            true))
    {:htmx htmx
     :calls calls}))

(defn- install-managed-root!
  [fragment-id]
  (let [root (.createElement js/document "div")]
    (.setAttribute root core/fragment-attribute fragment-id)
    (.setAttribute root "hx-trigger" core/refresh-event-name)
    (.setAttribute root "data-gesso-lifecycle-chromium-test" "true")
    (.appendChild (.-body js/document) root)
    root))

(defn- remove-root!
  [root]
  (when root
    (if-let [remove-fn (.-remove root)]
      (.call remove-fn root)
      (when-let [parent (.-parentNode root)]
        (.removeChild parent root))))
  nil)

(defn- make-event
  [event-name]
  (js/Event. event-name))

(defn- make-pageshow-event
  [persisted?]
  ;; PageTransitionEvent is the native pageshow type, but Event expando
  ;; properties are sufficient for dispatch and make the test independent of
  ;; constructor availability across Chromium revisions.
  (let [event (make-event "pageshow")]
    (aset event "persisted" (boolean persisted?))
    event))

(defn- refresh-calls
  [calls]
  (filterv
   #(= core/refresh-event-name (:event-name %))
   @calls))

(defn- with-browser-runtime
  [fragment-id f]
  (if-not (browser-host?)
    (is true
        "Native lifecycle dispatch is exercised by the Chromium host; Node keeps this namespace auto-discoverable.")
    (let [{:keys [htmx calls]} (make-htmx)
          root (install-managed-root! fragment-id)
          runtime (core/create
                   {:document js/document
                    :htmx htmx})]
      (try
        (core/start! runtime)
        (f {:runtime runtime
            :root root
            :calls calls})
        (finally
          (try
            (core/stop! runtime)
            (catch :default _
              nil))
          (remove-root! root))))))

(deftest real-window-online-dispatch-enters-managed-adapter-test
  (with-browser-runtime
    "lifecycle-online"
    (fn [{:keys [runtime root calls]}]
      (testing "the real Document supplies the real Window ownership seam"
        (is (identical? js/window (.-defaultView js/document))))

      (testing "dispatching online on the real Window produces one advisory refresh"
        (.dispatchEvent js/window (make-event "online"))

        (let [refreshes (refresh-calls calls)]
          (is (= 1 (count refreshes)))
          (is (identical? root (:root (first refreshes))))
          (is (= #{}
                 (:requirements
                  (core/pending-refresh runtime root)))
              "Window online recovery must remain advisory and authority-free."))))))

(deftest real-window-pageshow-distinguishes-bfcache-restoration-test
  (with-browser-runtime
    "lifecycle-pageshow"
    (fn [{:keys [runtime root calls]}]
      (testing "ordinary pageshow does not duplicate normal startup recovery"
        (.dispatchEvent
         js/window
         (make-pageshow-event false))
        (is (empty? (refresh-calls calls))))

      (testing "persisted pageshow on the real Window repairs bfcache restoration"
        (.dispatchEvent
         js/window
         (make-pageshow-event true))

        (let [refreshes (refresh-calls calls)]
          (is (= 1 (count refreshes)))
          (is (identical? root (:root (first refreshes))))
          (is (= #{}
                 (:requirements
                  (core/pending-refresh runtime root)))
              "bfcache recovery must not manufacture a progression requirement."))))))

(deftest real-document-visible-visibilitychange-enters-managed-adapter-test
  (with-browser-runtime
    "lifecycle-visible"
    (fn [{:keys [runtime root calls]}]
      (testing "the ordinary Chromium test page is a foreground document"
        (is (= "visible" (.-visibilityState js/document))
            "The Chromium harness disables renderer backgrounding; this test requires a genuinely visible Document."))

      (testing "dispatching visibilitychange on the real Document repairs foreground state"
        (.dispatchEvent
         js/document
         (make-event "visibilitychange"))

        (let [refreshes (refresh-calls calls)]
          (is (= 1 (count refreshes)))
          (is (identical? root (:root (first refreshes))))
          (is (= #{}
                 (:requirements
                  (core/pending-refresh runtime root)))))))))

(deftest real-lifecycle-events-coalesce-through-one-adapter-generation-test
  (with-browser-runtime
    "lifecycle-coalesce"
    (fn [{:keys [runtime root calls]}]
      (is (= "visible" (.-visibilityState js/document))
          "The real Chromium document must be visible for the visibility recovery signal.")

      ;; Three different browser lifecycle boundaries arrive before HTMX begins
      ;; the adapter-issued request. They are three reasons to distrust cached
      ;; presentation state, not permission for three parallel requests.
      (.dispatchEvent js/window (make-event "online"))
      (.dispatchEvent js/window (make-pageshow-event true))
      (.dispatchEvent js/document (make-event "visibilitychange"))

      (let [refreshes (refresh-calls calls)
            pending (core/pending-refresh runtime root)]
        (is (= 1 (count refreshes))
            "Native lifecycle events must collapse behind one current adapter generation.")
        (is (identical? root (:root (first refreshes))))
        (is (map? pending))
        (is (= #{} (:requirements pending))
            "Coalesced lifecycle repair remains advisory.")))))

(deftest stop-removes-real-window-and-document-lifecycle-listeners-test
  (if-not (browser-host?)
    (is true
        "Native listener removal is exercised by Chromium; Node keeps this namespace auto-discoverable.")
    (let [{:keys [htmx calls]} (make-htmx)
          root (install-managed-root! "lifecycle-stop")
          runtime (core/create
                   {:document js/document
                    :htmx htmx})]
      (try
        (core/start! runtime)
        (core/stop! runtime)

        (.dispatchEvent js/window (make-event "online"))
        (.dispatchEvent js/window (make-pageshow-event true))
        (.dispatchEvent js/document (make-event "visibilitychange"))

        (is (empty? (refresh-calls calls))
            "After stop!, actual Window/Document lifecycle dispatch must no longer reach Core.")
        (finally
          (remove-root! root))))))

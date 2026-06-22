(ns gesso.live.ui
  "Hiccup convenience helpers for gesso.live.

   This namespace sits above gesso.live.htmx and below gesso.live.core.

   It owns user-facing markup helpers such as:

   - ->fragment
   - fragment-panel
   - live-script
   - post-form
   - post-button
   - optimistic-post-button
   - anti-forgery-token
   - anti-forgery-input

   It intentionally does not depend on gesso.live.core. Core can safely require
   this namespace and re-export its public helpers."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx]
   [gesso.live.optimistic :as optimistic]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-stream-base-url
  "/app/gesso/live/stream")

(def default-live-script-src
  "Default public path for the Gesso Live browser runtime.

   Downstream apps should include this script when they use browser-side
   Gesso Live behavior such as client-continuity capture/restore."
  "/gesso/gesso-live.js")

(def default-fragment-swap
  htmx/default-fragment-swap)

(def default-post-swap
  htmx/default-post-swap)

(def default-post-sync
  "closest [data-gesso-live-fragment]:drop")

(def default-post-include
  "Selector that includes the lightweight post-button wrapper form.

   The wrapper owns the anti-forgery input and any app-supplied hidden inputs.
   Additional :include selectors are appended rather than replacing this value."
  "closest [data-gesso-live-post]")

;; -----------------------------------------------------------------------------
;; Small helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- blank-string?
  [x]
  (and (string? x)
       (str/blank? x)))

(defn- present?
  [x]
  (not (or (nil? x)
           (blank-string? x))))

(defn- require-present!
  [k value]
  (when-not (present? value)
    (throw
     (ex (str "gesso.live UI requires " k ".")
         {k value})))
  value)

(defn- normalize-name
  [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (nil? x)     nil
    :else        (str x)))

(defn- subscription-token
  [subscription]
  (cond
    (nil? subscription)
    nil

    (string? subscription)
    subscription

    (keyword? subscription)
    (name subscription)

    (symbol? subscription)
    (name subscription)

    (map? subscription)
    (or (:token subscription)
        (:subscription/token subscription)
        (:id subscription)
        (some-> (:topic subscription)
                normalize-name))

    :else
    (str subscription)))

(defn- encode-query-value
  [x]
  (java.net.URLEncoder/encode (str x) "UTF-8"))

(defn- stream-url-from-token
  [base-url token]
  (str base-url
       (if (str/includes? base-url "?") "&" "?")
       "subscription="
       (encode-query-value token)))

(defn- legacy-fragment-map?
  [m]
  (or (contains? m :fragment/id)
      (contains? m :fragment/src)
      (contains? m :fragment/swap)
      (contains? m :subscription/token)))

(defn- canonical-fragment-map
  [m]
  (if (legacy-fragment-map? m)
    {:id (:fragment/id m)
     :src (:fragment/src m)
     :swap (:fragment/swap m)
     :subscription (:subscription/token m)
     :stream-url (:stream-url m)
     :stream-base-url (:stream/base-url m)
     :event (:event m)
     :trigger (:trigger m)
     :include (:include m)
     :client-continuity (:client-continuity m)
     :jitter-ms (:jitter-ms m)
     :jitter-delay-ms (:jitter-delay-ms m)
     :attrs (:attrs m)
     :root-attrs (:root-attrs m)
     :target-attrs (or (:target-attrs m)
                       (:inner-attrs m))}
    m))

(defn- compact-map
  [m]
  (into {}
        (remove (comp nil? val))
        m))

(defn- include-selectors
  [include]
  (cond
    (or (nil? include)
        (false? include))
    []

    (string? include)
    (let [include' (str/trim include)]
      (when (str/blank? include')
        (throw
         (ex "gesso.live UI :include selectors must not be blank."
             {:include include})))
      [include'])

    (sequential? include)
    (mapcat include-selectors include)

    :else
    (throw
     (ex "gesso.live UI :include must be nil, false, a selector string, or a sequential collection of selector strings."
         {:include include}))))

(defn- post-include-value
  [include]
  (->> (concat [default-post-include]
               (include-selectors include))
       distinct
       (str/join ", ")))

(defn- button-children
  [{:keys [label children]}]
  (cond
    (nil? children) [label]
    (sequential? children) children
    :else [children]))

;; -----------------------------------------------------------------------------
;; Fragment descriptor
;; -----------------------------------------------------------------------------

(defn ->fragment
  "Create a live fragment descriptor.

   Preferred shape:

     (live/->fragment
      {:id 'simple-shared-counter-fragment'
       :src '/app/demo/simple-shared-counter/fragment'
       :subscription {:topic :demo-counter
                      :id 'global-shared-counter'}
       :stream-url '/app/gesso/live/stream?subscription=shared-counter'
       :swap :innerHTML})

   Legacy config maps are also accepted for migration:

     {:subscription/token 'shared-counter'
      :fragment/id 'simple-shared-counter-fragment'
      :fragment/src '/app/demo/simple-shared-counter/fragment'
      :fragment/swap 'innerHTML'}

   Required:
     :id
     :src
     either :stream-url or :subscription / :subscription-token

   Optional:
     :stream-base-url
     :event
     :swap
     :trigger
     :include
     :client-continuity
     :jitter-ms
     :jitter-delay-ms
     :attrs / :root-attrs
     :target-attrs / :inner-attrs

   :client-continuity is app-facing, Clojure/data-first configuration for
   preserving browser interaction context across fragment refreshes. Examples
   include scroll anchoring, focus/caret restoration, preserved DOM islands, and
   custom capture/restore boxes. This namespace stores the config on the
   fragment descriptor and delegates browser-facing attribute construction to
   gesso.live.htmx.

   Markup model:
     fragment-panel renders a stable outer live wrapper and a replaceable inner
     target. The outer wrapper owns SSE, hx-get, hx-trigger, hx-target, hx-swap,
     hx-include, and client-continuity attrs. The inner target owns only the
     replaceable DOM id plus target attrs."
  [fragment]
  (let [fragment'   (canonical-fragment-map fragment)
        id'         (require-present! :id (:id fragment'))
        src'        (require-present! :src (:src fragment'))
        token       (or (:subscription/token fragment')
                        (subscription-token (:subscription fragment')))
        base-url    (or (:stream-base-url fragment')
                        default-stream-base-url)
        stream-url' (or (:stream-url fragment')
                        (when token
                          (stream-url-from-token base-url token)))]
    (when-not (present? stream-url')
      (throw
       (ex "gesso.live UI fragment requires :stream-url or :subscription."
           {:fragment fragment})))
    (merge
     {:gesso.live.ui/type :gesso.live.ui/fragment
      :id id'
      :src src'
      :stream-url stream-url'
      :subscription (:subscription fragment')
      :subscription/token token
      :event (or (:event fragment') htmx/default-event)
      :swap (or (:swap fragment') default-fragment-swap)
      :trigger (or (:trigger fragment') htmx/default-fragment-trigger)
      :attrs {}
      :root-attrs {}
      :target-attrs {}}
     (compact-map
      {:include (:include fragment')
       :client-continuity (:client-continuity fragment')
       :jitter-ms (:jitter-ms fragment')
       :jitter-delay-ms (:jitter-delay-ms fragment')
       :attrs (:attrs fragment')
       :root-attrs (:root-attrs fragment')
       :target-attrs (:target-attrs fragment')}))))

(defn fragment?
  [x]
  (= :gesso.live.ui/fragment
     (:gesso.live.ui/type x)))

(defn ensure-fragment
  [fragment]
  (if (fragment? fragment)
    fragment
    (->fragment fragment)))

;; -----------------------------------------------------------------------------
;; Fragment markup
;; -----------------------------------------------------------------------------

(defn fragment-root-attrs
  "Build attrs for the stable outer live fragment wrapper.

   The outer wrapper owns both the SSE connection and the HTMX refresh request.
   This keeps hx-get/hx-trigger stable even when the replaceable inner target is
   swapped with outerHTML.

   Client-continuity attrs also belong on this stable outer wrapper, since it is
   the element that survives the inner target replacement."
  [fragment]
  (let [{:keys [stream-url
                src
                event
                swap
                trigger
                include
                client-continuity
                jitter-ms
                jitter-delay-ms
                id
                attrs
                root-attrs]} (ensure-fragment fragment)]
    (htmx/merge-attrs
     (htmx/fragment-root-attrs
      {:stream-url stream-url})
     {:data-gesso-live-fragment id
      :hx-get src
      :hx-trigger (htmx/fragment-trigger
                   {:event event
                    :trigger trigger
                    :jitter-ms jitter-ms
                    :jitter-delay-ms jitter-delay-ms})
      :hx-target (htmx/normalize-target id)
      :hx-swap swap}
     (when include
       {:hx-include include})
     (when client-continuity
       (htmx/client-continuity-attrs
        {:fragment-id id
         :client-continuity client-continuity}))
     attrs
     root-attrs)))

(defn fragment-target-attrs
  "Build attrs for the replaceable inner fragment target.

   The target intentionally does not own hx-get, hx-trigger, or
   client-continuity attrs. Those attrs live on the stable outer wrapper rendered
   by fragment-panel."
  [fragment]
  (let [{:keys [id target-attrs]} (ensure-fragment fragment)]
    (htmx/clean-attrs
     (merge
      {:id id}
      target-attrs))))

(defn fragment-panel
  "Render a standard live fragment panel.

   The outer element is stable and owns:
     - hx-ext='sse'
     - sse-connect
     - hx-get
     - hx-trigger
     - hx-target
     - hx-swap
     - optional hx-include
     - optional client-continuity attrs

   The inner element owns only the replaceable fragment id and optional
   target-attrs.

   This prevents the common outerHTML failure mode where a swapped fragment
   response replaces the element that used to own hx-get/hx-trigger, causing
   later SSE events to arrive without triggering a follow-up fetch.

   It also gives client-continuity code a stable root from which it can capture
   state before the inner target is replaced and restore state after HTMX
   settles."
  [fragment]
  (let [fragment' (ensure-fragment fragment)]
    [:div (fragment-root-attrs fragment')
     [:div (fragment-target-attrs fragment')]]))

;; -----------------------------------------------------------------------------
;; Browser runtime script helper
;; -----------------------------------------------------------------------------

(defn live-script
  "Render the Gesso Live browser runtime script tag.

   The runtime is framework-owned browser code for live-fragment behavior such as
   client-continuity capture/restore. It is intentionally separate from
   gesso-theme.js and from downstream app-owned main.js files.

   Options:
     :src
       Override script URL. Defaults to /gesso/gesso-live.js.

     :attrs
       Extra attrs merged last. Use this for cache-busting, nonce, integrity,
       crossorigin, or to override :defer."
  ([] (live-script nil))
  ([{:keys [src attrs]}]
   [:script
    (htmx/clean-attrs
     (merge
      {:src (or src default-live-script-src)
       :defer true}
      attrs))]))

;; -----------------------------------------------------------------------------
;; Anti-forgery helpers
;; -----------------------------------------------------------------------------

(defn anti-forgery-token
  "Extract a Biff/Ring-style anti-forgery token from ctx."
  [ctx]
  (or (:anti-forgery-token ctx)
      (:biff/anti-forgery-token ctx)
      (get-in ctx [:session :anti-forgery-token])))

(defn anti-forgery-input
  "Return a hidden anti-forgery input when ctx contains a token."
  [ctx]
  (when-let [token (anti-forgery-token ctx)]
    [:input {:type "hidden"
             :name "__anti-forgery-token"
             :value token}]))

;; -----------------------------------------------------------------------------
;; POST form helper
;; -----------------------------------------------------------------------------

(defn post-form-attrs
  "Build attrs for a POST form that refreshes a live fragment target.

   Required:
     :to

   Optional:
     :target
     :swap
     :attrs
     :native-action?
     :sync

   If :target is a fragment descriptor, its :id is used.

   This delegates to gesso.live.htmx/post-form-attrs. That helper intentionally
   omits native :action by default, so missed HTMX submits do not navigate to
   fragment-only mutation routes."
  [{:keys [to target swap attrs native-action? sync]
    :or {swap default-post-swap
         sync default-post-sync}}]
  (let [to'     (require-present! :to to)
        target' (if (fragment? target)
                  (:id target)
                  target)
        request (cond-> {:to to'
                         :target target'
                         :swap swap
                         :native-action? native-action?
                         :attrs attrs}
                  (some? sync) (assoc :sync sync))]
    (htmx/post-form-attrs request)))

(defn post-form
  "Render a POST form with anti-forgery input.

   This is useful when you explicitly want form semantics.

   For ordinary live buttons, prefer post-button. post-button uses type=button
   with hx-post directly on the button so missed HTMX events cannot fall back to
   native form submission."
  [ctx opts & children]
  (into
   [:form (post-form-attrs opts)]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    children)))

;; -----------------------------------------------------------------------------
;; POST button helpers
;; -----------------------------------------------------------------------------

(defn- post-button-args
  [fragment-or-opts maybe-opts]
  (if maybe-opts
    (let [fragment (ensure-fragment fragment-or-opts)]
      [(assoc (or maybe-opts {})
              :target (or (:target maybe-opts)
                          (:id fragment))
              :swap (or (:swap maybe-opts)
                        (:swap fragment)
                        default-post-swap))
       fragment])
    [(or fragment-or-opts {}) nil]))

(def ^:private optimistic-protocol-attrs
  [optimistic/template-attr
   optimistic/action-attr
   optimistic/target-attr
   optimistic/label-attr])

(defn- split-optimistic-source-attrs
  [source-attrs]
  {:request-attrs (apply dissoc source-attrs optimistic-protocol-attrs)
   :protocol-attrs (select-keys source-attrs optimistic-protocol-attrs)})

(defn- post-button-attrs
  [{:keys [to
           target
           include
           button-attrs
           request-attrs
           protocol-attrs]
    :as opts}]
  (let [swap (if (contains? opts :swap)
               (:swap opts)
               default-post-swap)
        sync (if (contains? opts :sync)
               (:sync opts)
               default-post-sync)]
    (htmx/merge-attrs
     request-attrs
     {:type "button"
      :hx-post (require-present! :to to)
      :hx-swap swap
      :hx-include (post-include-value include)}
     (when sync
       {:hx-sync sync})
     (when-let [target' (htmx/normalize-target target)]
       {:hx-target target'})
     button-attrs
     protocol-attrs)))

(defn- post-button-form-attrs
  [form-attrs]
  (htmx/merge-attrs
   form-attrs
   {:data-gesso-live-post true}))

(defn- render-post-button
  [ctx opts siblings]
  (into
   [:form (post-button-form-attrs (:form-attrs opts))]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    [(into
      [:button (post-button-attrs opts)]
      (button-children opts))]
    siblings)))

(defn post-button
  "Render a tiny HTMX POST button.

   Supported call shapes:

     (post-button ctx
       {:to '/increment'
        :target 'counter-fragment'
        :label '+'})

     (post-button ctx fragment
       {:to '/increment'
        :label '+'})

   Unlike post-form, this intentionally does not render a submit button. It
   renders a type=button with hx-post directly on the button. If HTMX misses a
   click under mobile tap storms, the native browser fallback is therefore a
   no-op rather than navigation or native POST.

   A lightweight wrapping form is still used so anti-forgery input can be
   included by hx-include.

   Options:
     :to
       POST target.

     :label
       Button label when :children is absent.

     :children
       Button children. A single non-sequential value is accepted.

     :target
       HTMX target. Defaults to fragment id in the 3-arity form.

     :swap
       HTMX swap. Defaults to fragment swap in the 3-arity form, otherwise
       'innerHTML'.

     :sync
       HTMX request synchronization. Defaults to synchronizing against the
       nearest live fragment panel and dropping overlapping requests:
       'closest [data-gesso-live-fragment]:drop'.

     :include
       One additional hx-include selector, or a sequential collection of
       selectors. These are appended to the required lightweight wrapper-form
       selector rather than replacing it.

     :form-attrs
       Extra attrs merged into the lightweight wrapper form.

     :button-attrs
       Extra attrs merged into button attrs."
  ([ctx opts]
   (post-button ctx opts nil))
  ([ctx fragment-or-opts maybe-opts]
   (let [[opts _fragment] (post-button-args fragment-or-opts maybe-opts)]
     (render-post-button ctx opts []))))

(defn optimistic-post-button
  "Render a type=button HTMX POST control with a matched optimistic template.

   This mirrors post-button's two call shapes and ordinary options, adding one
   required option:

     :optimistic
       A prepared gesso.live.optimistic descriptor or a raw options map accepted
       by optimistic/render-parts. The optimistic source attrs are placed on the
       actual button, and the associated <template> is rendered beside it inside
       the lightweight wrapper form.

   Unless :sync is explicitly supplied on the button options, the optimistic
   descriptor's target-scoped sync value is used. Explicit nil or false disables
   hx-sync. This keeps one request card's mutation from unnecessarily blocking
   unrelated cards in the surrounding live fragment.

   :include has the same additive behavior as post-button: the anti-forgery form
   remains included, and app selectors such as a stable board-state form are
   appended."
  ([ctx opts]
   (optimistic-post-button ctx opts nil))
  ([ctx fragment-or-opts maybe-opts]
   (let [[opts _fragment] (post-button-args fragment-or-opts maybe-opts)
         optimistic-config (require-present! :optimistic (:optimistic opts))
         {:keys [source-attrs template sync]}
         (optimistic/render-parts optimistic-config)
         {:keys [request-attrs protocol-attrs]}
         (split-optimistic-source-attrs source-attrs)
         effective-sync (if (contains? opts :sync)
                          (:sync opts)
                          sync)
         opts' (-> opts
                   (dissoc :optimistic)
                   (assoc :sync effective-sync
                          :request-attrs request-attrs
                          :protocol-attrs protocol-attrs))]
     (render-post-button ctx opts' [template]))))

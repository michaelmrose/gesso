(ns gesso.live.ui
  "Hiccup convenience helpers for gesso.live.

   This namespace sits above gesso.live.htmx and below gesso.live.core.

   It owns user-facing markup helpers such as:

   - ->fragment
   - fragment-panel
   - post-form
   - post-button
   - anti-forgery-token
   - anti-forgery-input

   It intentionally does not depend on gesso.live.core. Core can safely require
   this namespace and re-export its public helpers."
  (:require
   [clojure.string :as str]
   [gesso.live.htmx :as htmx]))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

(def default-stream-base-url
  "/app/gesso/live/stream")

(def default-fragment-swap
  htmx/default-fragment-swap)

(def default-post-swap
  htmx/default-post-swap)

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

;; -----------------------------------------------------------------------------
;; Fragment descriptor
;; -----------------------------------------------------------------------------

(defn ->fragment
  "Create a live fragment descriptor.

   Preferred shape:

     (live/->fragment
      {:id \"simple-shared-counter-fragment\"
       :src \"/app/demo/simple-shared-counter/fragment\"
       :subscription {:topic :demo-counter
                      :id \"global-shared-counter\"}
       :stream-url \"/app/gesso/live/stream?subscription=shared-counter\"
       :swap :innerHTML})

   Legacy config maps are also accepted for migration:

     {:subscription/token \"shared-counter\"
      :fragment/id \"simple-shared-counter-fragment\"
      :fragment/src \"/app/demo/simple-shared-counter/fragment\"
      :fragment/swap \"innerHTML\"}

   Required:
     :id
     :src
     either :stream-url or :subscription / :subscription/token

   Optional:
     :stream-base-url
     :event
     :swap
     :trigger
     :attrs / :root-attrs
     :target-attrs / :inner-attrs"
  [fragment]
  (let [fragment' (canonical-fragment-map fragment)
        id'       (require-present! :id (:id fragment'))
        src'      (require-present! :src (:src fragment'))
        token     (or (:subscription/token fragment')
                      (subscription-token (:subscription fragment')))
        base-url  (or (:stream-base-url fragment')
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
      {:attrs (:attrs fragment')
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
  "Build attrs for the outer live fragment SSE root."
  [fragment]
  (let [{:keys [stream-url attrs root-attrs]} (ensure-fragment fragment)]
    (merge
     (htmx/fragment-root-attrs {:stream-url stream-url})
     attrs
     root-attrs)))

(defn fragment-target-attrs
  "Build attrs for the inner HTMX refresh target."
  [fragment]
  (let [{:keys [id src event swap trigger target-attrs]} (ensure-fragment fragment)]
    (merge
     (htmx/fragment-target-attrs
      {:id id
       :src src
       :event event
       :swap swap
       :trigger trigger})
     target-attrs)))

(defn fragment-panel
  "Render a standard live fragment panel.

   The outer element owns the SSE connection. The inner element owns the HTMX
   refresh target that reloads on page load and on matching SSE events."
  [fragment]
  (let [fragment' (ensure-fragment fragment)]
    [:div (fragment-root-attrs fragment')
     [:div (fragment-target-attrs fragment')]]))

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
;; POST helpers
;; -----------------------------------------------------------------------------

(defn post-form-attrs
  "Build attrs for a POST form that refreshes a live fragment target.

   Required:
     :to

   Optional:
     :target
     :swap
     :attrs

   If :target is a fragment descriptor, its :id is used."
  [{:keys [to target swap attrs]
    :or {swap default-post-swap}}]
  (let [to' (require-present! :to to)
        target' (if (fragment? target)
                  (:id target)
                  target)]
    (htmx/post-form-attrs
     {:to to'
      :target target'
      :swap swap
      :attrs attrs})))

(defn post-form
  "Render a POST form with anti-forgery input.

   Usage:

     (post-form ctx
       {:to \"/increment\"
        :target \"counter-fragment\"}
       [:button {:type \"submit\"} \"+\"])"
  [ctx opts & children]
  (into
   [:form (post-form-attrs opts)]
   (concat
    (keep identity [(anti-forgery-input ctx)])
    children)))

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

(defn post-button
  "Render a tiny HTMX POST form containing one submit button.

   Supported call shapes:

     (post-button ctx
       {:to \"/increment\"
        :target \"counter-fragment\"
        :label \"+\"})

     (post-button ctx fragment
       {:to \"/increment\"
        :label \"+\"})

   Options:
     :to
       POST target.

     :label
       Button label when :children is absent.

     :children
       Button children.

     :target
       HTMX target. Defaults to fragment id in the 3-arity form.

     :swap
       HTMX swap. Defaults to fragment swap in the 3-arity form, otherwise
       \"innerHTML\".

     :form-attrs
       Extra attrs merged into form attrs.

     :button-attrs
       Extra attrs merged into button attrs."
  ([ctx opts]
   (post-button ctx opts nil))
  ([ctx fragment-or-opts maybe-opts]
   (let [[opts _fragment] (post-button-args fragment-or-opts maybe-opts)
         {:keys [to label children target swap form-attrs button-attrs]
          :or {swap default-post-swap}} opts
         button-children (or children [label])]
     (post-form
      ctx
      {:to to
       :target target
       :swap swap
       :attrs form-attrs}
      (into
       [:button (merge {:type "submit"} button-attrs)]
       button-children)))))

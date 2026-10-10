(ns gesso.live.application-preflight.biff
  "Internal Biff 2 application-handler lifecycle implementation.

   The public application-preflight namespace owns the authoritative
   ApplicationAssembly predicate and per-render validation. This namespace
   accepts those two functions (preferably as Vars to preserve
   their existing dynamic resolution semantics) rather than depending on the public
   namespace (which would introduce a require cycle).

   This is lifecycle plumbing, not a second source of application authority:
   a caller must supply the actual current-assembly? and
   require-rendered-surface! implementations from application-preflight.
   The public namespace alone exposes the canonical application module and
   startup function to applications."
  (:require
   [com.biffweb.core :as biff]
   [gesso.http :as http]))

(def canonical-html-response-surface
  :gesso.live.application-preflight/html-response)

(def biff-ring-handler-key
  :biff.ring/handler)

(def application-handler-module-id
  :gesso.live.application-preflight/use-application-handler)

(def application-assembly-system-key
  :gesso.live.application-preflight/application-assembly)

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.application-preflight/error
     :error/kind kind}
    data)))

(defn- actual-function?
  [value]
  (or
   (fn? value)
   (and
    (var? value)
    (fn? @value))))

(defn- invoke-actual-function
  [f & args]
  (apply (if (var? f) @f f) args))

(defn wrap-application-handler
  "Internal implementation of the canonical Gesso HTML/Ring wrapper.

   current-assembly? must be the current physical ApplicationAssembly
   recognizer. require-rendered-surface! must check each actual Hiccup body
   before gesso.http serializes it. The caller-facing two-argument wrapper
   remains owned by gesso.live.application-preflight.

   The returned handler rechecks the assembly per HTML response. This
   boundary does not cover arbitrary pre-serialized Ring HTML, later handler
   replacement, or server modules that ignore :biff.ring/handler."
  [current-assembly? require-rendered-surface! application-assembly handler]
  (when-not (current-assembly? application-assembly)
    (throw
     (preflight-error
      :invalid-application-handler-assembly
      "wrap-application-handler requires a current ApplicationAssembly."
      {:application-assembly application-assembly})))
  (when-not (actual-function? handler)
    (throw
     (preflight-error
      :invalid-application-handler
      "wrap-application-handler requires an actual function or a Var currently containing one."
      {:handler handler})))
  (fn [request]
    (http/with-html-response-preflight
     (fn [rendered]
       (require-rendered-surface!
        application-assembly
        canonical-html-response-surface
        rendered))
     (fn []
       (invoke-actual-function handler request)))))

(defn install-application-handler
  "Install the HTML response guard in one native Biff system map.

   The injected callbacks are fixed when the canonical module is constructed;
   they cannot be selected through the system map or browser metadata."
  [current-assembly? require-rendered-surface! system]
  (when-not (map? system)
    (throw
     (preflight-error
      :invalid-application-handler-system
      "Gesso application handler module requires a Biff system map."
      {:system system})))
  (let [application-assembly (get system application-assembly-system-key)]
    (when-not (current-assembly? application-assembly)
      (throw
       (preflight-error
        :invalid-application-handler-assembly
        "Gesso application handler module requires a current ApplicationAssembly supplied by canonical startup."
        {:application-assembly application-assembly})))
    (when-not (contains? system biff-ring-handler-key)
      (throw
       (preflight-error
        :missing-biff-ring-handler
        "Gesso application handler module requires :biff.ring/handler to exist after Biff module initialization and before lifecycle start."
        {:system-keys (set (keys system))})))
    (let [handler (get system biff-ring-handler-key)]
      (when-not (actual-function? handler)
        (throw
         (preflight-error
          :invalid-biff-ring-handler
          "Gesso application handler module requires :biff.ring/handler to be an actual function or a Var currently containing one."
          {:handler handler})))
      (-> system
          (assoc
           biff-ring-handler-key
           (wrap-application-handler
            current-assembly?
            require-rendered-surface!
            application-assembly
            handler))
          (dissoc application-assembly-system-key)))))

(defn make-application-handler-module
  "Create the canonical native Biff 2 module for the supplied *framework*
   assembly and render-check functions. This must be called exactly once by
   the public application-preflight namespace; the returned module object is
   the one compared by identity at canonical startup. Pass the public validator
   Vars (not their dereferenced functions) to preserve with-redefs and runtime
   rebinding semantics of the original inline implementation.

   Biff executes all module :biff.core/init entries before lifecycle start,
   so the :biff.ring/handler is installed before this module starts."
  [current-assembly? require-rendered-surface!]
  {:biff.core/id application-handler-module-id
   :biff.core/start
   (fn [system]
     (install-application-handler
      current-assembly?
      require-rendered-surface!
      system))})

(defn- canonical-application-handler-module?
  [canonical-module module]
  (and (= application-handler-module-id (:biff.core/id module))
       (identical?
        (:biff.core/start canonical-module)
        (:biff.core/start module))))

(defn- require-canonical-application-handler-module!
  [canonical-module modules-var]
  (when-not (var? modules-var)
    (throw
     (preflight-error
      :invalid-biff-application-modules-var
      "start-biff-application! requires the Biff modules collection as a Var."
      {:modules-var modules-var})))
  (let [modules @modules-var
        candidates
        (filterv
         #(= application-handler-module-id (:biff.core/id %))
         (if (sequential? modules) modules []))]
    (when-not (= 1 (count candidates))
      (throw
       (preflight-error
        :missing-or-duplicate-application-handler-module
        "Canonical Gesso/Biff startup requires exactly one application-handler-module in the modules Var."
        {:module-id application-handler-module-id
         :matching-module-count (count candidates)})))
    (when-not (canonical-application-handler-module?
               canonical-module
               (first candidates))
      (throw
       (preflight-error
        :invalid-application-handler-module
        "The module using Gesso's application-handler module ID is not the canonical Gesso module."
        {:module-id application-handler-module-id})))))

(defn- require-canonical-start-order!
  [start-order]
  (when-not (sequential? start-order)
    (throw
     (preflight-error
      :invalid-biff-application-start-order
      "start-biff-application! requires a sequential collection of qualified Biff module IDs."
      {:start-order start-order})))
  (when-not (= application-handler-module-id (first start-order))
    (throw
     (preflight-error
      :application-handler-module-not-first
      "Canonical Gesso/Biff startup requires the application-handler module to be first in the Biff lifecycle start order."
      {:required-first application-handler-module-id
       :start-order start-order}))))

(defn start-biff-application!
  "Internal implementation of canonical Gesso/Biff 2 startup.

   current-assembly? is the application-preflight recognizer. canonical-module
   is the exact module object exposed by the public namespace. Biff initializes
   modules and executes the start order after this boundary has checked the
   current assembly, module identity, and start order.

   The initial system cannot override the selected ApplicationAssembly. Direct
   biff.core/start, later handler replacement, and pre-serialized Ring responses
   are outside this guarantee."
  ([current-assembly? canonical-module application-assembly modules-var start-order]
   (start-biff-application!
    current-assembly?
    canonical-module
    application-assembly
    {}
    modules-var
    start-order))
  ([current-assembly? canonical-module application-assembly initial-system modules-var start-order]
   (when-not (current-assembly? application-assembly)
     (throw
      (preflight-error
       :invalid-biff-application-start-assembly
       "start-biff-application! requires a current ApplicationAssembly."
       {:application-assembly application-assembly})))
   (when-not (map? initial-system)
     (throw
      (preflight-error
       :invalid-biff-application-initial-system
       "start-biff-application! requires initial-system to be a map."
       {:initial-system initial-system})))
   (require-canonical-application-handler-module!
    canonical-module
    modules-var)
   (require-canonical-start-order! start-order)
   (biff/start
    (assoc
     initial-system
     application-assembly-system-key
     application-assembly)
    modules-var
    start-order)))

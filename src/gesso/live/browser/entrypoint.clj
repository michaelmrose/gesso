(ns gesso.live.browser.entrypoint
  "JVM compile-time bridge from verified Choreo plans to one closed browser
   assembly manifest embedded in ClojureScript.

   Downstream applications should not maintain application-specific macros that
   independently repeat Gesso's plan-registry, browser-role, feature, transport,
   or bootstrap checks. This namespace resolves a deliberately small set of
   compile-time declarations, runs the canonical Gesso preflights, and emits the
   resulting BrowserAssemblyManifest as portable ClojureScript data.

   Compile-time declarations may be literal values or fully-qualified symbols
   naming JVM vars. Arbitrary forms are intentionally not evaluated. This keeps
   the build boundary deterministic and prevents an application macro from
   silently reconstructing or mutating verified ExecutablePlans while embedding
   them.

   A typical application declaration is therefore only data plus references to
   its production choreography namespace:

     (entrypoint/embed-browser-assembly
      {:name :example/request-browser
       :plans example.request.choreo/browser-plans
       :required-plan-keys example.request.choreo/operation-keys
       :browser-role example.request.choreo/browser-role
       :optimistic? true
       :optimistic-htmx? true})

   Macro expansion fails before ClojureScript emission when the referenced vars
   cannot be resolved, the declared values have the wrong shape, Choreo plan
   coverage/role/digest preflight fails, or browser runtime assembly preflight
   fails.

   This namespace only owns build-time declaration resolution and canonical
   preflight composition. The browser-side consumer of the emitted manifest is
   a separate runtime boundary; application-specific optimistic rendering,
   authorization, DOM policy, and model semantics remain outside this compiler
   bridge."
  (:require
   [clojure.set :as set]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.browser.preflight :as browser-preflight]))

;; -----------------------------------------------------------------------------
;; Identity / declaration vocabulary
;; -----------------------------------------------------------------------------

(def entrypoint-version
  1)

(def declaration-type
  :gesso.live.browser.entrypoint/declaration)

(def ^:private declaration-keys
  #{:name
    :plans
    :required-plan-keys
    :browser-role
    :expected-digests
    :optimistic?
    :optimistic-htmx?
    :optimistic-command-transport})

(def ^:private reference-capable-keys
  #{:plans
    :required-plan-keys
    :browser-role
    :expected-digests})

;; -----------------------------------------------------------------------------
;; Errors / compile-time reference resolution
;; -----------------------------------------------------------------------------

(defn- entrypoint-error
  ([kind message data]
   (entrypoint-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.browser.entrypoint/error
      :error/kind kind}
     data)
    cause)))

(defn- fully-qualified-symbol?
  [value]
  (and
   (symbol? value)
   (some? (namespace value))))

(defn- resolve-var-reference!
  [declaration-key reference]
  (when-not (fully-qualified-symbol? reference)
    (throw
     (entrypoint-error
      :unqualified-compile-time-reference
      "Gesso browser entrypoint compile-time references must be fully-qualified symbols."
      {:declaration-key declaration-key
       :reference reference})))
  (let [resolved
        (try
          (requiring-resolve reference)
          (catch Throwable error
            (throw
             (entrypoint-error
              :compile-time-reference-resolution-failed
              "Gesso browser entrypoint could not resolve a compile-time declaration var."
              {:declaration-key declaration-key
               :reference reference}
              error))))]
    (when-not (var? resolved)
      (throw
       (entrypoint-error
        :compile-time-reference-not-var
        "Gesso browser entrypoint compile-time reference did not resolve to a Var."
        {:declaration-key declaration-key
         :reference reference
         :resolved resolved})))
    (try
      (var-get resolved)
      (catch Throwable error
        (throw
         (entrypoint-error
          :compile-time-reference-read-failed
          "Gesso browser entrypoint could not read a compile-time declaration Var."
          {:declaration-key declaration-key
           :reference reference}
          error))))))

(defn- resolve-declaration-value!
  [declaration-key value]
  (if (symbol? value)
    (if (contains? reference-capable-keys declaration-key)
      (resolve-var-reference! declaration-key value)
      (throw
       (entrypoint-error
        :symbol-not-allowed
        "Gesso browser entrypoint only permits Var references for designated compile-time declaration fields."
        {:declaration-key declaration-key
         :value value
         :reference-capable-keys reference-capable-keys})))
    value))

(defn- validate-declaration-shape!
  [declaration]
  (when-not (map? declaration)
    (throw
     (entrypoint-error
      :invalid-declaration
      "Gesso browser entrypoint declaration must be a literal map."
      {:declaration declaration})))

  (let [unknown
        (set/difference
         (set (keys declaration))
         declaration-keys)]
    (when (seq unknown)
      (throw
       (entrypoint-error
        :unknown-declaration-keys
        "Gesso browser entrypoint declaration contains unsupported keys."
        {:unknown-keys unknown
         :allowed-keys declaration-keys}))))

  (doseq [required-key [:plans :browser-role]]
    (when-not (contains? declaration required-key)
      (throw
       (entrypoint-error
        :missing-declaration-key
        "Gesso browser entrypoint declaration is missing a required field."
        {:missing-key required-key
         :required-keys #{:plans :browser-role}}))))

  declaration)

(defn resolve-declaration!
  "Resolve the compile-time reference-capable fields of one literal browser
   declaration.

   Public primarily for build tooling and dedicated tests. Application CLJS
   should normally use embed-browser-assembly instead of calling this function.

   Only :plans, :required-plan-keys, :browser-role, and :expected-digests may be
   fully-qualified Var symbols. Other fields must be literal declaration data."
  [declaration]
  (let [declaration
        (validate-declaration-shape! declaration)]
    (reduce-kv
     (fn [resolved declaration-key value]
       (assoc
        resolved
        declaration-key
        (resolve-declaration-value!
         declaration-key
         value)))
     {:gesso.live.browser.entrypoint/type declaration-type
      :gesso.live.browser.entrypoint/version entrypoint-version}
     declaration)))

;; -----------------------------------------------------------------------------
;; Canonical preflight composition
;; -----------------------------------------------------------------------------

(defn- declaration-payload
  [resolved]
  (dissoc
   resolved
   :gesso.live.browser.entrypoint/type
   :gesso.live.browser.entrypoint/version))

(defn compile-browser-assembly!
  "Compile one resolved or reference-bearing browser declaration into the
   canonical BrowserAssemblyManifest.

   This function composes exactly two existing proof/preflight boundaries:

   1. gesso.choreo.preflight/require-plan-registry!
   2. gesso.live.browser.preflight/require-browser-assembly!

   Browser registries are always non-empty, single-role registries constrained
   to the declared physical browser role. Required logical plan coverage and
   expected executable digests are enforced when supplied.

   The returned value is portable data and can be embedded directly into CLJS."
  [declaration]
  (let [{:keys
         [name
          plans
          required-plan-keys
          browser-role
          expected-digests
          optimistic?
          optimistic-htmx?
          optimistic-command-transport]}
        (-> declaration
            resolve-declaration!
            declaration-payload)

        plan-registry
        (choreo-preflight/require-plan-registry!
         (cond->
          {:name name
           :plans plans
           :single-role? true
           :expected-role browser-role
           :allow-empty? false}
           (some? required-plan-keys)
           (assoc :required-keys required-plan-keys)

           (some? expected-digests)
           (assoc :expected-digests expected-digests)))

        assembly
        (browser-preflight/require-browser-assembly!
         (cond->
          {:name name
           :plan-registry plan-registry
           :browser-role browser-role}
           (some? required-plan-keys)
           (assoc :required-plan-keys required-plan-keys)

           (some? optimistic?)
           (assoc :optimistic? optimistic?)

           (some? optimistic-htmx?)
           (assoc :optimistic-htmx? optimistic-htmx?)

           (some? optimistic-command-transport)
           (assoc
            :optimistic-command-transport
            optimistic-command-transport)))]
    (when-not (browser-preflight/assembly-manifest? assembly)
      (throw
       (entrypoint-error
        :invalid-compiled-browser-assembly
        "Gesso browser entrypoint compiler produced an invalid BrowserAssemblyManifest."
        {:assembly assembly})))
    assembly))

(defmacro embed-browser-assembly
  "Compile and embed one verified browser assembly as portable ClojureScript
   data.

   The declaration must be a literal map. Reference-capable fields may contain
   fully-qualified JVM Var symbols; they are resolved by the ClojureScript
   compiler process before either Choreo or browser assembly preflight runs.

   No arbitrary declaration form is eval'd. The macro expansion is a quoted,
   closed BrowserAssemblyManifest, so downstream CLJS cannot accidentally
   reconstruct a different plan registry while initializing the browser."
  [declaration]
  (when-not (map? declaration)
    (throw
     (entrypoint-error
      :nonliteral-macro-declaration
      "embed-browser-assembly requires a literal declaration map."
      {:declaration declaration})))
  (list
   'quote
   (compile-browser-assembly! declaration)))

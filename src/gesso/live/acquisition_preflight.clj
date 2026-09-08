(ns gesso.live.acquisition-preflight
  "JVM-side preflight for one managed Gesso Live authoritative-acquisition path.

   The current Gesso Live runtime already owns the mechanism needed to recover
   authoritative server-rendered state:

     managed fragment EventSource opens / advisory invalidation arrives
       -> browser adapter admits one fragment refresh
       -> HTMX GETs the fragment route
       -> gesso.live.core/render-fragment-response binds the canonical
          progression requirement before authorization/query/render
       -> the compiled Live fragment query runs inside that progression-bound
          request context and its renderer produces the replacement projection

   gesso.live.model owns the semantic fragment, scope, query, and render facts.
   gesso.live.fragment / gesso.live.ui own the standard managed browser shape.
   gesso.live.core owns the progression-safe fragment response boundary, and
   gesso.live.transport.sse owns the standard model-backed stream boundary.

   What was missing was an inspectable relation saying that a particular
   compiled fragment has physical route realizations for those existing Gesso
   boundaries. This namespace supplies that relation without creating another
   refresh mechanism, client state store, or optimistic-only abstraction.

   Applications supply only physical facts that Gesso cannot derive from the
   choreography or Live model: the concrete fragment and stream route
   coordinates, plus (for non-canonical diagnostic fixtures) which boundary a
   route claims to realize. The normal helpers fragment-route and stream-route
   select the canonical Gesso boundaries automatically.

   Important honesty boundary:

   A route capability is a trusted application-owned declaration about physical
   routing. This checker does not inspect arbitrary Ring/Biff/Reitit handler
   bodies and therefore does not prove that a path string really dispatches to
   the declared function. It verifies that a managed acquisition is closed
   *given* those route declarations, and it keeps the trusted boundary identity
   explicit in the resulting realization.

   The fragment-local relation is also lifted into an invalidation-acquisition
   assembly. That assembly derives required fragment acquisitions from the
   compiled Live graph itself:

     change topic -> invalidated scope -> fragment projecting that scope

   Applications therefore do not maintain a second required-fragment registry.
   Realizations for fragments outside the current invalidation graph are allowed
   as additional physical evidence but are reported as unrequired; the checker
   does not pretend that merely declaring a fragment makes it managed or
   invalidation-driven."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.live.model :as model]))

;; =============================================================================
;; Identity / supported current realization
;; =============================================================================

(def preflight-version 1)

(def report-type
  :gesso.live.acquisition-preflight/report)

(def route-capability-type
  :gesso.live.acquisition-preflight/route-capability)

(def acquisition-realization-type
  :gesso.live.acquisition-preflight/authoritative-acquisition-realization)

(def acquisition-obligation-type
  :gesso.live.acquisition-preflight/authoritative-acquisition-obligation)

(def acquisition-assembly-report-type
  :gesso.live.acquisition-preflight/acquisition-assembly-report)

(def acquisition-assembly-type
  :gesso.live.acquisition-preflight/authoritative-acquisition-assembly)

(def fragment-route-kind
  :fragment)

(def stream-route-kind
  :stream)

(def progression-safe-fragment-boundary
  "Stable identity for the standard app-facing fragment reread boundary.

   gesso.live.core/render-fragment-response decodes/binds the browser's
   canonical progression requirement before authorization and fragment query."
  :gesso.live.core/render-fragment-response)

(def managed-stream-boundary
  "Stable identity for the standard model-backed Gesso Live SSE boundary."
  :gesso.live.transport.sse/start-fragment-stream!)

(def managed-acquisition-profile
  "Current standard managed Live acquisition semantics.

   The EventSource open/reopen and advisory invalidations enter the browser
   adapter; only an admitted adapter :fragment/refresh effect causes the HTMX
   fragment GET."
  :gesso.live/managed-stream-reread)

(def ^:private supported-route-kinds
  #{fragment-route-kind
    stream-route-kind})

(def ^:private report-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private analysis-keys
  #{:name
    :live-app
    :fragment
    :known-fragments
    :scope
    :fragment-route
    :stream-route
    :acquisition-profile})

(def ^:private route-capability-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :kind
    :fragment
    :method
    :path
    :boundary})

(def ^:private route-capability-option-keys
  #{:kind
    :fragment
    :method
    :path
    :boundary})

(def ^:private canonical-route-option-keys
  #{:fragment
    :path})

(def ^:private realization-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :name
    :live-app
    :fragment
    :scope
    :fragment-route
    :stream-route
    :acquisition})

(def ^:private acquisition-keys
  #{:profile
    :initial
    :invalidation
    :refresh-owner
    :reread
    :render})

(def ^:private option-keys
  #{:name
    :live-app
    :fragment
    :fragment-route
    :stream-route})

(def ^:private obligation-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :fragment
    :scope
    :scope-topic
    :scope-id-key
    :change-topics
    :acquisition-profile})

(def ^:private assembly-option-keys
  #{:name
    :live-app
    :realizations})

(def ^:private assembly-report-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private assembly-analysis-keys
  #{:name
    :live-app
    :known-fragments
    :obligations
    :required-fragments
    :supplied-fragments
    :realizations})

(def ^:private assembly-keys
  #{:gesso.live.acquisition-preflight/type
    :gesso.live.acquisition-preflight/version
    :name
    :live-app
    :obligations
    :required-fragments
    :realizations
    :acquisition-profile})

;; =============================================================================
;; Errors / primitive validation
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.acquisition-preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (preflight-error
      :invalid-shape
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- nonblank-string?
  [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- closed-map?
  [expected value]
  (and
   (map? value)
   (= expected
      (set (keys value)))))

(defn- compiled-live-app?
  "Recognize the normalized/validated product emitted by model/compile-live-app.

   We intentionally validate the semantic model again rather than trusting only
   :compiled?. live-rule expand functions are compiler-owned closures and are
   therefore checked structurally here by topic coverage rather than by function
   identity."
  [value]
  (and
   (map? value)
   (true? (:compiled? value))
   (map? (:scopes value))
   (map? (:graph value))
   (map? (:fragments value))
   (vector? (:live-rules value))
   (empty?
    (model/validate-normalized-live-app value))
   (= (set (keys (:graph value)))
      (set (map :when-topic (:live-rules value))))
   (every?
    (fn [rule]
      (and
       (map? rule)
       (keyword? (:when-topic rule))
       (fn? (:expand rule))))
    (:live-rules value))))

(defn- known-fragments
  [live-app]
  (if (compiled-live-app? live-app)
    (set (keys (:fragments live-app)))
    #{}))

(defn- validate-options!
  [options]
  (let [options' (require-map! "Authoritative acquisition preflight options" options)
        keys' (set (keys options'))
        unknown (set/difference keys' option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option
        "Authoritative acquisition preflight contains unknown options."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (when (and (contains? options' :name)
               (some? (:name options'))
               (not (keyword? (:name options'))))
      (throw
       (preflight-error
        :invalid-name
        "Authoritative acquisition preflight :name must be nil or a keyword."
        {:name (:name options')})))
    (when (and (contains? options' :fragment)
               (some? (:fragment options'))
               (not (keyword? (:fragment options'))))
      (throw
       (preflight-error
        :invalid-fragment
        "Authoritative acquisition preflight :fragment must be a keyword."
        {:fragment (:fragment options')})))
    options'))

;; =============================================================================
;; Trusted physical route capability declarations
;; =============================================================================

(defn route-capability
  "Construct one closed trusted physical route declaration.

   Required options:

     :kind      :fragment or :stream
     :fragment  semantic compiled Live fragment this route claims to realize
     :method    physical HTTP/Ring method keyword
     :path      non-blank opaque physical route/path coordinate
     :boundary  stable identity of the Gesso/app handler boundary this route
                claims to realize

   This constructor deliberately accepts non-canonical methods/boundaries so
   preflight can represent and diagnose an existing malformed application.
   Normal applications should use fragment-route and stream-route instead."
  [options]
  (let [options' (require-map! "Authoritative acquisition route capability" options)
        keys' (set (keys options'))
        missing (set/difference route-capability-option-keys keys')
        unknown (set/difference keys' route-capability-option-keys)]
    (when (seq missing)
      (throw
       (preflight-error
        :missing-route-capability-key
        "Authoritative acquisition route capability is missing required keys."
        {:missing-keys missing
         :required-keys route-capability-option-keys})))
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-route-capability-key
        "Authoritative acquisition route capability contains unknown keys."
        {:unknown-keys unknown
         :allowed-keys route-capability-option-keys})))
    (let [{:keys [kind fragment method path boundary]} options']
      (when-not (contains? supported-route-kinds kind)
        (throw
         (preflight-error
          :invalid-route-kind
          "Authoritative acquisition route capability has an unsupported :kind."
          {:kind kind
           :supported-kinds supported-route-kinds})))
      (when-not (keyword? fragment)
        (throw
         (preflight-error
          :invalid-route-fragment
          "Authoritative acquisition route capability :fragment must be a keyword."
          {:kind kind
           :fragment fragment})))
      (when-not (keyword? method)
        (throw
         (preflight-error
          :invalid-route-method
          "Authoritative acquisition route capability :method must be a keyword."
          {:kind kind
           :method method})))
      (when-not (nonblank-string? path)
        (throw
         (preflight-error
          :invalid-route-path
          "Authoritative acquisition route capability :path must be a non-blank string."
          {:kind kind
           :path path})))
      (when-not (keyword? boundary)
        (throw
         (preflight-error
          :invalid-route-boundary
          "Authoritative acquisition route capability :boundary must be a keyword."
          {:kind kind
           :boundary boundary})))
      {:gesso.live.acquisition-preflight/type route-capability-type
       :gesso.live.acquisition-preflight/version preflight-version
       :kind kind
       :fragment fragment
       :method method
       :path path
       :boundary boundary})))

(defn route-capability?
  "True for one closed current acquisition route capability declaration."
  [value]
  (and
   (closed-map? route-capability-keys value)
   (= route-capability-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (contains? supported-route-kinds (:kind value))
   (keyword? (:fragment value))
   (keyword? (:method value))
   (nonblank-string? (:path value))
   (keyword? (:boundary value))))

(defn- canonical-route-options!
  [label options]
  (let [options' (require-map! label options)
        keys' (set (keys options'))
        missing (set/difference canonical-route-option-keys keys')
        unknown (set/difference keys' canonical-route-option-keys)]
    (when (seq missing)
      (throw
       (preflight-error
        :missing-route-key
        (str label " is missing required keys.")
        {:missing-keys missing
         :required-keys canonical-route-option-keys})))
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-route-key
        (str label " contains unknown keys.")
        {:unknown-keys unknown
         :allowed-keys canonical-route-option-keys})))
    options'))

(defn fragment-route
  "Declare the normal trusted physical GET route for a managed fragment reread.

   The app supplies semantic :fragment identity plus physical :path. Method and
   boundary identity are derived:

     GET -> gesso.live.core/render-fragment-response

   Calling this function is a trusted application declaration that the physical
   route really uses that boundary; preflight does not inspect arbitrary route
   handler bodies."
  [options]
  (let [{:keys [fragment path]}
        (canonical-route-options!
         "Managed Live fragment route"
         options)]
    (route-capability
     {:kind fragment-route-kind
      :fragment fragment
      :method :get
      :path path
      :boundary progression-safe-fragment-boundary})))

(defn stream-route
  "Declare the normal trusted physical GET route for a managed fragment stream.

   The app supplies semantic :fragment identity plus physical :path. Method and
   boundary identity are derived:

     GET -> gesso.live.transport.sse/start-fragment-stream!

   Calling this function is a trusted application declaration that the physical
   route really uses that boundary."
  [options]
  (let [{:keys [fragment path]}
        (canonical-route-options!
         "Managed Live stream route"
         options)]
    (route-capability
     {:kind stream-route-kind
      :fragment fragment
      :method :get
      :path path
      :boundary managed-stream-boundary})))

;; =============================================================================
;; Per-fragment acquisition relation
;; =============================================================================

(defn- route-errors
  [fragment route-label expected-kind expected-boundary route]
  (cond
    (nil? route)
    [(issue
      (if (= expected-kind fragment-route-kind)
        :missing-fragment-route
        :missing-stream-route)
      (if (= expected-kind fragment-route-kind)
        "Managed Live fragment has no trusted physical fragment GET route."
        "Managed Live fragment has no trusted physical stream route.")
      {:fragment fragment
       :route route-label
       :expected-kind expected-kind
       :expected-method :get
       :expected-boundary expected-boundary})]

    (not (route-capability? route))
    [(issue
      :invalid-route-capability
      "Managed Live authoritative acquisition received an invalid or tampered route capability."
      {:fragment fragment
       :route route-label
       :expected-kind expected-kind
       :route-capability route})]

    :else
    (cond-> []
      (not= expected-kind (:kind route))
      (conj
       (issue
        :route-kind-mismatch
        "Managed Live acquisition route kind does not match its required role."
        {:fragment fragment
         :route route-label
         :expected expected-kind
         :actual (:kind route)}))

      (not= fragment (:fragment route))
      (conj
       (issue
        :route-fragment-mismatch
        "Managed Live acquisition route is bound to a different semantic fragment."
        {:fragment fragment
         :route route-label
         :expected fragment
         :actual (:fragment route)
         :path (:path route)}))

      (not= :get (:method route))
      (conj
       (issue
        (if (= expected-kind fragment-route-kind)
          :fragment-route-not-get
          :stream-route-not-get)
        "Managed Live authoritative acquisition requires a GET route."
        {:fragment fragment
         :route route-label
         :expected :get
         :actual (:method route)
         :path (:path route)}))

      (not= expected-boundary (:boundary route))
      (conj
       (issue
        (if (= expected-kind fragment-route-kind)
          :fragment-reread-not-progression-safe
          :stream-route-not-managed-live)
        (if (= expected-kind fragment-route-kind)
          "Managed fragment reread does not declare the standard progression-safe Gesso Live response boundary."
          "Managed fragment stream does not declare the standard model-backed Gesso Live stream boundary.")
        {:fragment fragment
         :route route-label
         :expected expected-boundary
         :actual (:boundary route)
         :path (:path route)})))))

(defn- acquisition-summary
  []
  {:profile managed-acquisition-profile
   :initial :stream-open-reread
   :invalidation :advisory-sse
   :refresh-owner :browser-adapter
   :reread :progression-bound
   :render :compiled-live-fragment})

(defn check-acquisition
  "Return a structured report for one compiled Live fragment's managed
   authoritative-acquisition path.

   Required options:

     :live-app
       A current compiled gesso.live.model application.

     :fragment
       Semantic fragment keyword in that compiled model.

     :fragment-route
       Trusted route capability for the standard progression-safe fragment GET.
       Use fragment-route for the normal formulation.

     :stream-route
       Trusted route capability for the standard model-backed SSE stream. Use
       stream-route for the normal formulation.

   Optional :name identifies this application/projection slice diagnostically.

   The successful relation establishes the existence of the current standard
   managed progression-bound acquisition path *relative to the trusted physical
   route declarations*. It does not claim arbitrary query correctness, authorization,
   eventual network delivery, or automatic adoption of every newly advanced
   authoritative projection."
  [options]
  (let [{:keys [name live-app fragment fragment-route stream-route]}
        (validate-options! options)

        live-app-valid?
        (compiled-live-app? live-app)

        known
        (known-fragments live-app)

        fragment-known?
        (and live-app-valid?
             (keyword? fragment)
             (contains? known fragment))

        scope
        (when fragment-known?
          (model/fragment-scope-name live-app fragment))

        base-errors
        (cond-> []
          (nil? live-app)
          (conj
           (issue
            :missing-live-app
            "Authoritative acquisition preflight requires a compiled Gesso Live application."
            {:live-app live-app}))

          (and (some? live-app)
               (not live-app-valid?))
          (conj
           (issue
            :invalid-live-app
            "Authoritative acquisition preflight requires current normalized/validated compiled Live metadata."
            {:live-app live-app}))

          (nil? fragment)
          (conj
           (issue
            :missing-fragment
            "Authoritative acquisition preflight requires a semantic Live fragment."
            {:known-fragments known}))

          (and live-app-valid?
               (keyword? fragment)
               (not fragment-known?))
          (conj
           (issue
            :unknown-fragment
            "Authoritative acquisition preflight references a fragment absent from the compiled Live application."
            {:fragment fragment
             :known-fragments known})))

        both-routes-missing?
        (and fragment-known?
             (nil? fragment-route)
             (nil? stream-route))

        acquisition-errors
        (if fragment-known?
          (vec
           (concat
            (when both-routes-missing?
              [(issue
                :missing-authoritative-acquisition
                "Managed Live fragment has no declared authoritative acquisition path."
                {:fragment fragment
                 :scope scope
                 :required #{:fragment-route :stream-route}})])
            (route-errors
             fragment
             :fragment-route
             fragment-route-kind
             progression-safe-fragment-boundary
             fragment-route)
            (route-errors
             fragment
             :stream-route
             stream-route-kind
             managed-stream-boundary
             stream-route)))
          [])

        errors
        (vec (concat base-errors acquisition-errors))]
    {:gesso.live.acquisition-preflight/type report-type
     :gesso.live.acquisition-preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings []
     :analysis
     {:name name
      :live-app live-app
      :fragment fragment
      :known-fragments known
      :scope scope
      :fragment-route fragment-route
      :stream-route stream-route
      :acquisition-profile
      (when fragment-known?
        (acquisition-summary))}}))

(defn report?
  "True only when value is exactly the current report derivable from the
   embedded preflight inputs.

   Recognition intentionally re-runs check-acquisition. A caller cannot forge a
   positive report by changing :valid?/:errors or by relabeling the analyzed
   fragment/routes while retaining a plausible report shape."
  [value]
  (and
   (closed-map? report-keys value)
   (= report-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (closed-map? analysis-keys (:analysis value))
   (set? (get-in value [:analysis :known-fragments]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (let [{:keys [name live-app fragment fragment-route stream-route]}
           (:analysis value)]
       (= value
          (check-acquisition
           {:name name
            :live-app live-app
            :fragment fragment
            :fragment-route fragment-route
            :stream-route stream-route})))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized successful authoritative-acquisition report."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

;; =============================================================================
;; Closed successful product
;; =============================================================================

(defn- realization-errors
  [value]
  (:errors
   (check-acquisition
    {:name (:name value)
     :live-app (:live-app value)
     :fragment (:fragment value)
     :fragment-route (:fragment-route value)
     :stream-route (:stream-route value)})))

(defn acquisition-realization?
  "True when value is a closed current AuthoritativeAcquisitionRealization.

   Recognition revalidates the embedded Live model and trusted route
   capabilities through check-acquisition, then requires the derived scope and
   acquisition summary to agree exactly. Neither may be independently relabeled
   after preflight."
  [value]
  (and
   (closed-map? realization-keys value)
   (= acquisition-realization-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (compiled-live-app? (:live-app value))
   (keyword? (:fragment value))
   (keyword? (:scope value))
   (route-capability? (:fragment-route value))
   (route-capability? (:stream-route value))
   (empty? (realization-errors value))
   (= (:scope value)
      (model/fragment-scope-name
       (:live-app value)
       (:fragment value)))
   (= acquisition-keys
      (set (keys (:acquisition value))))
   (= (acquisition-summary)
      (:acquisition value))))

(defn require-acquisition-realization!
  "Run fragment-local authoritative-acquisition preflight and return one closed
   realization.

   Later whole-application closure can carry this object rather than rediscover
   which fragment/route/managed-Live relation was checked."
  [options]
  (let [{:keys [name live-app fragment fragment-route stream-route]
         :as options'}
        (validate-options! options)

        report
        (check-acquisition options')]
    (when-not (valid? report)
      (throw
       (preflight-error
        :authoritative-acquisition-preflight-failed
        "Gesso Live authoritative-acquisition preflight failed."
        {:preflight report})))
    (let [realization
          {:gesso.live.acquisition-preflight/type acquisition-realization-type
           :gesso.live.acquisition-preflight/version preflight-version
           :name name
           :live-app live-app
           :fragment fragment
           :scope (model/fragment-scope-name live-app fragment)
           :fragment-route fragment-route
           :stream-route stream-route
           :acquisition (acquisition-summary)}]
      (when-not (acquisition-realization? realization)
        (throw
         (preflight-error
          :invalid-emitted-acquisition-realization
          "Authoritative acquisition preflight produced an internally inconsistent realization."
          {:realization realization
           :preflight report})))
      realization)))

(defn- invalidation-change-topics-by-scope
  [live-app]
  (reduce-kv
   (fn [acc change-topic targets]
     (reduce
      (fn [acc' target]
        (update acc' (:scope target) (fnil conj #{}) change-topic))
      acc
      targets))
   {}
   (:graph live-app)))

(defn acquisition-obligations
  "Derive invalidation-driven authoritative-acquisition obligations from a
   compiled Live application.

   No application-owned required-fragment registry is accepted. The obligation
   set follows only relations already present in compiled Live metadata:

     graph change topic -> invalidated scope
     fragment            -> projected scope

   A fragment whose scope is never targeted by the compiled invalidation graph
   does not become a required managed acquisition merely because the fragment is
   declared. Such a fragment may still have a valid realization supplied to an
   assembly, but it is additional evidence rather than a graph-derived
   obligation.

   Each returned value is closed plain data and records the change topics that
   can invalidate the fragment's scope."
  [live-app]
  (when-not (compiled-live-app? live-app)
    (throw
     (preflight-error
      :invalid-live-app
      "Acquisition obligation derivation requires a current compiled Gesso Live application."
      {:live-app live-app})))
  (let [change-topics-by-scope
        (invalidation-change-topics-by-scope live-app)]
    (into
     (sorted-map)
     (keep
      (fn [[fragment-name fragment]]
        (let [scope-name (:scope fragment)
              change-topics (get change-topics-by-scope scope-name)]
          (when (seq change-topics)
            (let [scope-desc (model/scope-descriptor live-app scope-name)]
              [fragment-name
               {:gesso.live.acquisition-preflight/type acquisition-obligation-type
                :gesso.live.acquisition-preflight/version preflight-version
                :fragment fragment-name
                :scope scope-name
                :scope-topic (:topic scope-desc)
                :scope-id-key (:id-key scope-desc)
                :change-topics change-topics
                :acquisition-profile managed-acquisition-profile}])))))
     (:fragments live-app))))

(defn acquisition-obligation?
  "True for one closed current obligation derived from some compiled Live app.

   This predicate validates the value's local shape. Exact correspondence to a
   particular Live app is checked by acquisition assembly recognition."
  [value]
  (and
   (closed-map? obligation-keys value)
   (= acquisition-obligation-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (keyword? (:fragment value))
   (keyword? (:scope value))
   (keyword? (:scope-topic value))
   (keyword? (:scope-id-key value))
   (set? (:change-topics value))
   (seq (:change-topics value))
   (every? keyword? (:change-topics value))
   (= managed-acquisition-profile
      (:acquisition-profile value))))

(defn- validate-assembly-options!
  [options]
  (let [options' (require-map! "Authoritative acquisition assembly options" options)
        keys' (set (keys options'))
        unknown (set/difference keys' assembly-option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-assembly-option
        "Authoritative acquisition assembly contains unknown options."
        {:unknown-keys unknown
         :allowed-keys assembly-option-keys})))
    (when (and (contains? options' :name)
               (some? (:name options'))
               (not (keyword? (:name options'))))
      (throw
       (preflight-error
        :invalid-name
        "Authoritative acquisition assembly :name must be nil or a keyword."
        {:name (:name options')})))
    (when (and (contains? options' :realizations)
               (some? (:realizations options'))
               (not (map? (:realizations options'))))
      (throw
       (preflight-error
        :invalid-acquisition-realizations
        "Authoritative acquisition assembly :realizations must be a fragment-keyed map."
        {:realizations (:realizations options')})))
    options'))

(defn- realization-entry-errors
  [live-app known-fragments fragment realization]
  (cond-> []
    (not (keyword? fragment))
    (conj
     (issue
      :invalid-acquisition-fragment-key
      "Acquisition realization registry key must be a semantic fragment keyword."
      {:fragment fragment}))

    (and (keyword? fragment)
         (not (contains? known-fragments fragment)))
    (conj
     (issue
      :unknown-acquisition-fragment
      "Acquisition realization is supplied for a fragment absent from the compiled Live application."
      {:fragment fragment
       :known-fragments known-fragments}))

    (not (acquisition-realization? realization))
    (conj
     (issue
      :invalid-acquisition-realization
      "Acquisition assembly received an invalid or tampered fragment realization."
      {:fragment fragment
       :realization realization}))

    (and (acquisition-realization? realization)
         (not= fragment (:fragment realization)))
    (conj
     (issue
      :acquisition-fragment-key-mismatch
      "Acquisition realization registry key does not match the realization's semantic fragment."
      {:fragment fragment
       :expected fragment
       :actual (:fragment realization)}))

    (and (acquisition-realization? realization)
         (not= live-app (:live-app realization)))
    (conj
     (issue
      :acquisition-live-app-mismatch
      "Acquisition realization belongs to a different compiled Live application."
      {:fragment fragment
       :expected-live-app live-app
       :actual-live-app (:live-app realization)}))))

(defn check-acquisition-assembly
  "Check invalidation-driven acquisition closure for one compiled Live app.

   Required acquisition fragments are derived from the compiled invalidation
   graph; callers supply only fragment-keyed AuthoritativeAcquisitionRealization
   values already established by require-acquisition-realization!.

   A missing graph-derived realization is fatal. A valid same-application
   realization for a fragment outside the current invalidation graph is retained
   but reported as :unrequired-acquisition-realization. This keeps the checker
   honest about what the graph actually requires while allowing other explicit
   acquisition uses such as initial-only projections.

   This relation does not yet connect a particular optimistic settlement to a
   particular Live change topic. That later whole-application edge can consume
   this closed Live acquisition assembly instead of reconstructing its topology."
  [options]
  (let [{:keys [name live-app realizations]}
        (validate-assembly-options! options)

        live-app-valid?
        (compiled-live-app? live-app)

        realizations'
        (or realizations {})

        known
        (known-fragments live-app)

        obligations
        (if live-app-valid?
          (acquisition-obligations live-app)
          (sorted-map))

        required-fragments
        (set (keys obligations))

        supplied-fragments
        (if (map? realizations')
          (set (keys realizations'))
          #{})

        missing-fragments
        (set/difference required-fragments supplied-fragments)

        base-errors
        (cond-> []
          (nil? live-app)
          (conj
           (issue
            :missing-live-app
            "Authoritative acquisition assembly requires a compiled Gesso Live application."
            {:live-app live-app}))

          (and (some? live-app)
               (not live-app-valid?))
          (conj
           (issue
            :invalid-live-app
            "Authoritative acquisition assembly requires current normalized/validated compiled Live metadata."
            {:live-app live-app})))

        missing-errors
        (mapv
         (fn [fragment]
           (issue
            :missing-acquisition-realization
            "Compiled Live invalidation topology requires an authoritative acquisition realization that was not supplied."
            {:fragment fragment
             :obligation (get obligations fragment)
             :required-fragments required-fragments
             :supplied-fragments supplied-fragments}))
         (sort missing-fragments))

        realization-errors
        (if (and live-app-valid? (map? realizations'))
          (vec
           (mapcat
            (fn [[fragment realization]]
              (realization-entry-errors live-app known fragment realization))
            realizations'))
          [])

        unrequired
        (set/difference supplied-fragments required-fragments)

        warnings
        (if live-app-valid?
          (mapv
           (fn [fragment]
             (issue
              :unrequired-acquisition-realization
              "A valid acquisition realization is supplied for a fragment not required by the current compiled invalidation graph."
              {:fragment fragment
               :required-fragments required-fragments}))
           (sort
            (filter
             (fn [fragment]
               (let [realization (get realizations' fragment)]
                 (and
                  (keyword? fragment)
                  (contains? known fragment)
                  (acquisition-realization? realization)
                  (= fragment (:fragment realization))
                  (= live-app (:live-app realization)))))
             unrequired)))
          [])

        errors
        (vec (concat base-errors missing-errors realization-errors))]
    {:gesso.live.acquisition-preflight/type acquisition-assembly-report-type
     :gesso.live.acquisition-preflight/version preflight-version
     :valid? (empty? errors)
     :errors errors
     :warnings warnings
     :analysis
     {:name name
      :live-app live-app
      :known-fragments known
      :obligations obligations
      :required-fragments required-fragments
      :supplied-fragments supplied-fragments
      :realizations realizations'}}))

(defn acquisition-assembly-report?
  "True only when value is exactly the current acquisition-assembly report
   derivable from its embedded inputs."
  [value]
  (and
   (closed-map? assembly-report-keys value)
   (= acquisition-assembly-report-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (closed-map? assembly-analysis-keys (:analysis value))
   (set? (get-in value [:analysis :known-fragments]))
   (map? (get-in value [:analysis :obligations]))
   (every? acquisition-obligation?
           (vals (get-in value [:analysis :obligations])))
   (set? (get-in value [:analysis :required-fragments]))
   (set? (get-in value [:analysis :supplied-fragments]))
   (map? (get-in value [:analysis :realizations]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (let [{:keys [name live-app realizations]}
           (:analysis value)]
       (= value
          (check-acquisition-assembly
           {:name name
            :live-app live-app
            :realizations realizations})))
     (catch Exception _
       false))))

(defn acquisition-assembly-valid?
  "True only for a recognized successful acquisition-assembly report."
  [report]
  (and
   (acquisition-assembly-report? report)
   (true? (:valid? report))))

(defn- assembly-recheck
  [value]
  (check-acquisition-assembly
   {:name (:name value)
    :live-app (:live-app value)
    :realizations (:realizations value)}))

(defn acquisition-assembly?
  "True for one closed invalidation-driven authoritative-acquisition assembly.

   Recognition re-derives obligations from the embedded compiled Live graph and
   revalidates every supplied fragment realization. The obligation set therefore
   cannot be independently edited, relabeled, or copied from a different Live
   application."
  [value]
  (and
   (closed-map? assembly-keys value)
   (= acquisition-assembly-type
      (:gesso.live.acquisition-preflight/type value))
   (= preflight-version
      (:gesso.live.acquisition-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (compiled-live-app? (:live-app value))
   (map? (:obligations value))
   (every? acquisition-obligation? (vals (:obligations value)))
   (set? (:required-fragments value))
   (map? (:realizations value))
   (= managed-acquisition-profile (:acquisition-profile value))
   (try
     (let [report (assembly-recheck value)]
       (and
        (acquisition-assembly-valid? report)
        (= (:obligations value)
           (get-in report [:analysis :obligations]))
        (= (:required-fragments value)
           (get-in report [:analysis :required-fragments]))))
     (catch Exception _
       false))))

(defn require-acquisition-assembly!
  "Require all acquisition realizations implied by the compiled Live
   invalidation graph and return one closed assembly.

   The caller does not enumerate required fragments. It supplies whatever
   fragment realizations physically exist; Gesso derives the required subset and
   rejects omissions before browser interaction."
  [options]
  (let [{:keys [name live-app realizations]
         :as options'}
        (validate-assembly-options! options)

        report
        (check-acquisition-assembly options')]
    (when-not (acquisition-assembly-valid? report)
      (throw
       (preflight-error
        :authoritative-acquisition-assembly-failed
        "Gesso Live authoritative-acquisition assembly failed."
        {:preflight report})))
    (let [assembly
          {:gesso.live.acquisition-preflight/type acquisition-assembly-type
           :gesso.live.acquisition-preflight/version preflight-version
           :name name
           :live-app live-app
           :obligations (get-in report [:analysis :obligations])
           :required-fragments (get-in report [:analysis :required-fragments])
           :realizations (or realizations {})
           :acquisition-profile managed-acquisition-profile}]
      (when-not (acquisition-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-acquisition-assembly
          "Authoritative acquisition preflight produced an internally inconsistent assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn explain
  "Return a compact stable summary of an acquisition report, realization,
   assembly report, or closed acquisition assembly."
  [value]
  (cond
    (acquisition-assembly? value)
    {:type acquisition-assembly-type
     :version preflight-version
     :name (:name value)
     :required-fragments (:required-fragments value)
     :obligations (:obligations value)
     :realizations
     (into
      (sorted-map)
      (map
       (fn [[fragment realization]]
         [fragment
          (select-keys realization
                       [:fragment :scope :fragment-route :stream-route])]))
      (:realizations value))
     :acquisition-profile (:acquisition-profile value)
     :guarantee :invalidation-acquisition-preflight-closed-relative-to-trusted-routes}

    (acquisition-assembly-report? value)
    {:type acquisition-assembly-report-type
     :version preflight-version
     :valid? (:valid? value)
     :errors (:errors value)
     :warnings (:warnings value)
     :analysis
     (select-keys
      (:analysis value)
      [:name
       :known-fragments
       :obligations
       :required-fragments
       :supplied-fragments])}

    (acquisition-realization? value)
    {:type acquisition-realization-type
     :version preflight-version
     :name (:name value)
     :fragment (:fragment value)
     :scope (:scope value)
     :fragment-route
     (select-keys (:fragment-route value)
                  [:fragment :method :path :boundary])
     :stream-route
     (select-keys (:stream-route value)
                  [:fragment :method :path :boundary])
     :acquisition (:acquisition value)
     :guarantee :preflight-closed-relative-to-trusted-routes}

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :errors (:errors value)
     :warnings (:warnings value)
     :analysis (:analysis value)}

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an authoritative-acquisition report, realization, assembly report, or acquisition assembly."
      {:value value}))))

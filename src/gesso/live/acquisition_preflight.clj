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

   This first acquisition relation is intentionally fragment-local. A later
   whole-application assembly can derive which fragment acquisitions are
   required by exposed operations, settlements, and invalidation topology rather
   than asking applications to maintain a duplicate required-fragment registry."
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

(defn explain
  "Return a compact stable summary of an acquisition report or realization."
  [value]
  (cond
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
      "Expected an authoritative-acquisition preflight report or realization."
      {:value value}))))

(ns gesso.live.application-preflight
  "JVM-side whole-application preflight for one assembled Gesso Live application.

   ApplicationAssembly composes the already-closed browser, operation, route,
   trusted execution, settlement/progression, Live publication, invalidation,
   and authoritative-acquisition relations with the exact generated browser
   artifact currently on disk. Optional rendered-surface snapshots add concrete
   canonical :choreo/op affordance -> plan -> HTTP route correspondence.

   Dynamic Clojure rendering is handled at the real serialization boundary rather
   than by pretending every possible Hiccup value is statically enumerable.
   check-rendered-surface validates one actual render; wrap-application-handler
   installs that validation around the canonical Gesso HTML response path;
   application-handler-module lifts the wrapper into Biff's native module lifecycle;
   and start-biff-application! requires that stable-ID module to be first in the
   canonical Gesso/Biff start order.

   The resulting guarantee is verified assembly plus pre-browser enforcement for
   canonical Gesso HTML responses when that startup path is used. ApplicationAssembly
   itself cannot prove deployment used the canonical startup, that later arbitrary
   Clojure did not replace :biff.ring/handler, that a server did not ignore the
   standard Biff handler key, or that code did not hand-build pre-serialized HTML
   Ring responses. Those are explicit escape/deployment boundaries, not hidden
   holes or fabricated static proof obligations.

   Recognition remains physical and fail-closed: current artifact bytes/receipt,
   nested assemblies, rendered metadata, operation/plan identity, route method/path,
   anonymous physical request collisions with assembled semantic routes, canonical
   node request-to-affordance correspondence, and authoritative
   acquisition are revalidated from their source facts. A semantic command route may
   never silently degrade into an unmarked physical request merely because an
   application failed to construct its canonical :choreo/op binding. Browser
   metadata never grants authority, and trusted application/model declarations remain
   named assumptions rather than being promoted to v4.5 machine-checked proof."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.application-preflight.operation-summary :as operation-summary]
   [gesso.live.application-preflight.assurance :as assurance]
   [gesso.live.application-preflight.semantic-audit :as semantic-audit]
   [gesso.live.optimistic.resolution-assurance :as resolution-assurance]
   [gesso.model.formal-observation :as formal-observation]
   [gesso.model.tx-refinement-evidence :as tx-refinement-evidence]
   [gesso.live.application-preflight.affordance :as affordance]
   [gesso.live.application-preflight.biff :as biff-lifecycle]
   [gesso.live.application-preflight.rendered-surface :as rendered-surface]))

;; =============================================================================
;; Identity / closed vocabulary
;; =============================================================================

(def preflight-version 3)

(def report-type
  :gesso.live.application-preflight/report)

(def application-assembly-type
  :gesso.live.application-preflight/application-assembly)

(def backbone-guarantee
  assurance/backbone-guarantee)

(def rendered-affordance-guarantee
  assurance/rendered-affordance-guarantee)

(def rendered-surface-guarantee
  assurance/rendered-surface-guarantee)

(def rendered-surface-report-type
  :gesso.live.application-preflight/rendered-surface-report)

(def canonical-html-response-surface
  biff-lifecycle/canonical-html-response-surface)

(def biff-ring-handler-key
  "Canonical Biff 2 system key consumed by Ring server modules."
  biff-lifecycle/biff-ring-handler-key)

(def application-handler-module-id
  "Stable Biff 2 lifecycle ID for Gesso's canonical application-handler module."
  biff-lifecycle/application-handler-module-id)

(def application-assembly-system-key
  "Temporary canonical-startup system key used to carry the current
   ApplicationAssembly into application-handler-module.

   start-biff-application! owns this value: caller-supplied initial-system data
   cannot select or replace the assembly used by the module. The module removes
   the key before returning so later application modules do not depend on a
   second long-lived Gesso assembly registry in the Biff system map."
  biff-lifecycle/application-assembly-system-key)

(def ^:private option-keys
  #{:name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path
    :rendered-surfaces})

(def ^:private report-keys
  #{:gesso.live.application-preflight/type
    :gesso.live.application-preflight/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private rendered-surface-report-keys
  #{:gesso.live.application-preflight/type
    :gesso.live.application-preflight/version
    :valid?
    :errors
    :analysis})

(def ^:private rendered-surface-analysis-keys
  #{:application-assembly
    :surface
    :rendered
    :affordances
    :guarantee})

(def ^:private analysis-keys
  #{:name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path
    :operations
    :artifact-receipt
    :rendered-surfaces
    :affordances
    :affordance-closure
    :open-obligations
    :trusted-assumptions})

(def ^:private assembly-keys
  #{:gesso.live.application-preflight/type
    :gesso.live.application-preflight/version
    :name
    :operation-acquisition-assembly
    :browser-artifact-path
    :browser-receipt-path})

(def ^:private assembly-with-rendered-surfaces-keys
  (conj assembly-keys :rendered-surfaces))

;; Assurance vocabulary (guarantees, caveats, and escape hatches) is owned by
;; application-preflight.assurance. The application coordinator only validates
;; the source products and uses the resulting report to supply those facts.

;; =============================================================================
;; Errors / option validation
;; =============================================================================

(defn- preflight-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.application-preflight/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- closed-map?
  [expected value]
  (and
   (map? value)
   (= expected (set (keys value)))))

(defn- nonblank-string?
  [value]
  (and
   (string? value)
   (not (str/blank? value))))

(defn- rendered-surfaces?
  [value]
  (and
   (map? value)
   (not (empty? value))
   (every?
    (fn [[surface-name rendered]]
      (and
       (keyword? surface-name)
       (some? rendered)))
    value)))

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

(defn- validate-options!
  [options]
  (let [options' (require-map! "Application preflight options" options)
        unknown (set/difference (set (keys options')) option-keys)]
    (when (seq unknown)
      (throw
       (preflight-error
        :unknown-option-keys
        "Application preflight options contain unknown keys."
        {:unknown-keys unknown
         :allowed-keys option-keys})))
    (doseq [required [:operation-acquisition-assembly :browser-artifact-path]]
      (when-not (contains? options' required)
        (throw
         (preflight-error
          :missing-option
          "Application preflight is missing a required option."
          {:required required
           :provided-keys (set (keys options'))}))))
    (when (and (contains? options' :name)
               (some? (:name options'))
               (not (keyword? (:name options'))))
      (throw
       (preflight-error
        :invalid-name
        "Application preflight :name must be nil or a keyword."
        {:name (:name options')})))
    (when-not (nonblank-string? (:browser-artifact-path options'))
      (throw
       (preflight-error
        :invalid-browser-artifact-path
        "Application preflight :browser-artifact-path must be a non-blank string."
        {:browser-artifact-path (:browser-artifact-path options')})))
    (when (and (contains? options' :browser-receipt-path)
               (some? (:browser-receipt-path options'))
               (not (nonblank-string? (:browser-receipt-path options'))))
      (throw
       (preflight-error
        :invalid-browser-receipt-path
        "Application preflight :browser-receipt-path must be nil or a non-blank string."
        {:browser-receipt-path (:browser-receipt-path options')})))
    (when (and (contains? options' :rendered-surfaces)
               (not (rendered-surfaces? (:rendered-surfaces options'))))
      (throw
       (preflight-error
        :invalid-rendered-surfaces
        "Application preflight :rendered-surfaces must be a non-empty map of keyword surface names to non-nil rendered values."
        {:rendered-surfaces (:rendered-surfaces options')})))
    options'))

;; =============================================================================
;; Operation summary facade and application assembly currentness
;; =============================================================================

(declare application-assembly?
         application-assembly-shape?
         current-application-report
         check-application-assembly)

;; All nested operation, route, execution, browser, and acquisition derivations
;; live in application-preflight.operation-summary. The facade retains the
;; established public API; this namespace owns application-level authority.

(defn operation-summary
  "Return one completely derived operation-keyed explanation for a current
   OperationAcquisitionAssembly.

   This is deliberately a view over nested closed products, not another stored
   registry.  It exposes enough of the current application backbone for tooling
   and LLM-assisted repair without forcing a consumer to reconstruct the chain
   manually from several namespaces.

   v623 derives one current operation-acquisition report and reuses its analysis
   instead of recursively re-recognizing the same nested assemblies through
   several public accessors."
  [operation-acquisition-assembly]
  (operation-summary/operation-summary operation-acquisition-assembly))

(defn open-obligations
  "Return the currently known obligations that ApplicationAssembly alone cannot
   discharge.

   These are now deployment/coverage boundaries rather than missing operation,
   route, settlement, Live, or acquisition plumbing. Canonical Gesso/Biff startup
   provides dynamic pre-browser enforcement, but the assembly value itself does
   not prove that deployment adopted that startup path or avoided explicit raw
   response/handler-replacement escape hatches."
  [application-or-operation-acquisition]
  (cond
    (application-assembly-shape? application-or-operation-acquisition)
    (if-let [report
             (current-application-report
              application-or-operation-acquisition)]
      (get-in report [:analysis :open-obligations])
      (throw
       (preflight-error
        :invalid-application-input
        "Open-obligation inspection requires a current ApplicationAssembly or OperationAcquisitionAssembly."
        {:value application-or-operation-acquisition})))

    (and (map? application-or-operation-acquisition)
         (= application-assembly-type
            (:gesso.live.application-preflight/type
             application-or-operation-acquisition)))
    (throw
     (preflight-error
      :invalid-application-input
      "Open-obligation inspection requires a current ApplicationAssembly or OperationAcquisitionAssembly."
      {:value application-or-operation-acquisition}))

    (operation-summary/current-operation-acquisition-report
     application-or-operation-acquisition)
    (assurance/open-obligations-for-surfaces nil)

    :else
    (throw
     (preflight-error
      :invalid-application-input
      "Open-obligation inspection requires a current ApplicationAssembly or OperationAcquisitionAssembly."
      {:value application-or-operation-acquisition}))))

(defn- verify-browser-artifact
  [manifest artifact-path receipt-path]
  (try
    {:receipt
     (browser-build/verify-generated-artifact!
      manifest
      artifact-path
      (cond-> {}
        receipt-path
        (assoc :receipt-path receipt-path)))}
    (catch clojure.lang.ExceptionInfo error
      {:error
       (issue
        :browser-artifact-verification-failed
        "Application browser artifact does not currently correspond to the nested BrowserAssemblyManifest."
        {:browser-artifact-path artifact-path
         :browser-receipt-path receipt-path
         :cause-type (:error/type (ex-data error))
         :cause-kind (:error/kind (ex-data error))
         :cause-data (dissoc (ex-data error) :error/type :error/kind)})})
    (catch Throwable error
      {:error
       (issue
        :browser-artifact-verification-failed
        "Application browser artifact verification failed at the physical build boundary."
        {:browser-artifact-path artifact-path
         :browser-receipt-path receipt-path
         :exception-class (str (class error))
         :exception-message (.getMessage error)})})))

;; =============================================================================
;; Application-backbone report
;; =============================================================================

(defn check-application-assembly
  "Check the currently modeled whole-application runtime backbone and, when
   supplied, the canonical Choreo affordances discoverable from named rendered
   Hiccup surfaces.

   Required inputs:

     :operation-acquisition-assembly
       The closed semantic/runtime chain from browser operation through trusted
       execution, settlement publication, Live invalidation, and authoritative
       reacquisition.

     :browser-artifact-path
       The exact generated JavaScript artifact to verify physically against the
       BrowserAssemblyManifest nested in the supplied assembly.

   Optional:

     :browser-receipt-path
       Selects a non-default artifact receipt path.

     :rendered-surfaces
       Non-empty map of keyword surface-name -> rendered server-side Hiccup/value.
       Canonical :choreo/op affordances are derived from Gesso-owned metadata on
       the ordinary rendered nodes and checked against the assembled operation,
       browser-plan, HTTP method, and trusted route template. Every rendered
       ordinary hx-* request, native form POST, and submitter override is compared
       with the assembled semantic route set; an anonymous request may not target
       a semantic operation route after application
       code dropped :choreo/op identity. The supplied surface set is itself still
       an application snapshot and does not independently prove that it enumerates
       every possible render surface.

   A valid report means every modeled runtime-backbone edge is closed, the browser
   artifact is current, and every canonical affordance in any supplied snapshot
   resolves to that backbone. Arbitrary render-state completeness is handled by
   canonical dynamic pre-browser enforcement, not by claiming a static snapshot is
   exhaustive. Deployment adoption of that checked boundary remains explicit."
  [options]
  (let [{:keys [name
                operation-acquisition-assembly
                browser-artifact-path
                browser-receipt-path
                rendered-surfaces]}
        (validate-options! options)

        operation-acquisition-report
        (operation-summary/current-operation-acquisition-report
         operation-acquisition-assembly)

        operation-acquisition-valid?
        (some? operation-acquisition-report)

        manifest
        (when operation-acquisition-valid?
          (operation-summary/browser-assembly operation-acquisition-assembly))

        artifact-verification
        (when manifest
          (verify-browser-artifact
           manifest
           browser-artifact-path
           browser-receipt-path))

        artifact-error
        (:error artifact-verification)

        base-operation-summary
        (if operation-acquisition-valid?
          (operation-summary/operation-summary-from-report
           operation-acquisition-assembly
           operation-acquisition-report)
          (sorted-map))

        surface-analysis
        (rendered-surface/analyze-rendered-surfaces
         base-operation-summary
         rendered-surfaces)

        surface-scan
        (:scan surface-analysis)

        semantic-route-errors
        (:semantic-route-errors surface-analysis)

        affordances
        (:affordances surface-analysis)

        operation-summary'
        (if (some? rendered-surfaces)
          (affordance/enrich-operation-summary-with-affordances
           base-operation-summary
           affordances)
          base-operation-summary)

        errors
        (cond-> []
          (not operation-acquisition-valid?)
          (conj
           (issue
            :invalid-operation-acquisition-assembly
            "Application preflight requires a current untampered OperationAcquisitionAssembly."
            {:operation-acquisition-assembly operation-acquisition-assembly}))

          artifact-error
          (conj artifact-error)

          (seq (:errors surface-analysis))
          (into (:errors surface-analysis)))

        unhandled-topics
        (if operation-acquisition-valid?
          (get-in operation-acquisition-report
                  [:analysis
                   :unhandled-published-change-topics-by-operation])
          {})

        ;; Assurance is a view of the already-checked semantic/runtime inputs.
        ;; It must never establish application authority or widen the checker scope.
        warnings
        (assurance/warnings rendered-surfaces unhandled-topics)

        obligations
        (assurance/open-obligations-for-surfaces rendered-surfaces)

        affordance-closure
        (assurance/affordance-closure
         rendered-surfaces
         surface-scan
         semantic-route-errors
         surface-analysis
         affordances)]
    {:gesso.live.application-preflight/type report-type
     :gesso.live.application-preflight/version preflight-version
     :valid? (empty? errors)
     :errors (vec errors)
     :warnings (vec warnings)
     :analysis
     {:name name
      :operation-acquisition-assembly operation-acquisition-assembly
      :browser-artifact-path browser-artifact-path
      :browser-receipt-path browser-receipt-path
      :operations operation-summary'
      :artifact-receipt (:receipt artifact-verification)
      :rendered-surfaces rendered-surfaces
      :affordances (vec affordances)
      :affordance-closure affordance-closure
      :open-obligations obligations
      :trusted-assumptions assurance/trusted-assumptions}}))

(defn report?
  "True only when value is exactly the current application report derivable from
   its embedded inputs and current physical browser artifact.

   Recognition intentionally re-hashes/re-verifies the browser artifact. A
   formerly valid report becomes unrecognized when artifact bytes or receipt
   metadata change."
  [value]
  (and
   (closed-map? report-keys value)
   (= report-type
      (:gesso.live.application-preflight/type value))
   (= preflight-version
      (:gesso.live.application-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (vector? (:warnings value))
   (closed-map? analysis-keys (:analysis value))
   (map? (get-in value [:analysis :operations]))
   (vector? (get-in value [:analysis :open-obligations]))
   (set? (get-in value [:analysis :trusted-assumptions]))
   (= (:valid? value)
      (empty? (:errors value)))
   (try
     (let [{:keys [name
                   operation-acquisition-assembly
                   browser-artifact-path
                   browser-receipt-path
                   rendered-surfaces]}
           (:analysis value)]
       (= value
          (check-application-assembly
           (cond->
            {:name name
             :operation-acquisition-assembly operation-acquisition-assembly
             :browser-artifact-path browser-artifact-path
             :browser-receipt-path browser-receipt-path}
             (some? rendered-surfaces)
             (assoc :rendered-surfaces rendered-surfaces)))))
     (catch Exception _
       false))))

(defn valid?
  "True only for a recognized report whose modeled runtime-backbone edges are
   closed. Open obligations outside the currently modeled backbone remain
   visible in :analysis and do not silently disappear."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

;; =============================================================================
;; Dynamic pre-browser rendered-surface boundary
;; =============================================================================

(defn- validate-rendered-surface-input!
  [surface rendered]
  (when-not (keyword? surface)
    (throw
     (preflight-error
      :invalid-rendered-surface-name
      "Rendered surface name must be a keyword."
      {:surface surface})))
  (when (nil? rendered)
    (throw
     (preflight-error
      :invalid-rendered-surface
      "Rendered surface must be non-nil."
      {:surface surface
       :rendered rendered})))
  true)

(defn check-rendered-surface
  "Validate one actual named rendered Hiccup/value against a current
   ApplicationAssembly before browser delivery.

   This is the dynamic counterpart to :rendered-surfaces snapshot preflight.
   Arbitrary Clojure handlers may render different values for different users,
   data, and control-flow branches, so current route/page declarations cannot
   statically enumerate the complete value space. This function closes exactly
   one actual surface occurrence against the current semantic operation, browser
   plan, HTTP method, trusted route realization, and current physical browser
   artifact.

   Success means this rendered value is safe to hand to a response renderer
   relative to the current ApplicationAssembly: canonical semantic affordances
   resolve exactly and no anonymous inspected physical request collides with an assembled
   semantic operation route. It does not prove that every application handler uses
   this boundary."
  [application-assembly surface rendered]
  (validate-rendered-surface-input! surface rendered)
  (let [application-report
        (current-application-report application-assembly)

        application-valid?
        (some? application-report)

        base-operation-summary
        (if application-valid?
          (get-in application-report [:analysis :operations])
          (sorted-map))

        surface-analysis
        (rendered-surface/analyze-rendered-surfaces
         base-operation-summary
         {surface rendered})

        errors
        (cond-> []
          (not application-valid?)
          (conj
           (issue
            :invalid-application-assembly
            "Rendered-surface validation requires a current ApplicationAssembly."
            {:application-assembly application-assembly
             :surface surface}))

          (seq (:errors surface-analysis))
          (into (:errors surface-analysis)))

        affordances
        (:affordances surface-analysis)]
    {:gesso.live.application-preflight/type rendered-surface-report-type
     :gesso.live.application-preflight/version preflight-version
     :valid? (empty? errors)
     :errors (vec errors)
     :analysis
     {:application-assembly application-assembly
      :surface surface
      :rendered rendered
      :affordances affordances
      :guarantee (when (empty? errors) rendered-surface-guarantee)}}))

(defn rendered-surface-report?
  "True only when value is exactly the current rendered-surface report derivable
   from its embedded ApplicationAssembly, surface name, and rendered value.

   Recognition re-verifies the current physical browser artifact through the
   embedded ApplicationAssembly and rescans the rendered Hiccup."
  [value]
  (and
   (closed-map? rendered-surface-report-keys value)
   (= rendered-surface-report-type
      (:gesso.live.application-preflight/type value))
   (= preflight-version
      (:gesso.live.application-preflight/version value))
   (boolean? (:valid? value))
   (vector? (:errors value))
   (closed-map? rendered-surface-analysis-keys (:analysis value))
   (= (:valid? value) (empty? (:errors value)))
   (try
     (let [{:keys [application-assembly surface rendered]} (:analysis value)]
       (= value
          (check-rendered-surface application-assembly surface rendered)))
     (catch Exception _
       false))))

(defn rendered-surface-valid?
  "True only for a recognized successful rendered-surface report."
  [report]
  (and
   (rendered-surface-report? report)
   (true? (:valid? report))))

(defn require-rendered-surface!
  "Require one actual rendered surface to close against the current
   ApplicationAssembly and return its recognized report.

   This function performs no HTML serialization and grants no authority."
  [application-assembly surface rendered]
  (let [report (check-rendered-surface application-assembly surface rendered)]
    (when-not (true? (:valid? report))
      (throw
       (preflight-error
        :rendered-surface-preflight-failed
        "Rendered Choreo surface does not close against the current application backbone."
        {:surface surface
         :preflight report})))
    report))

(defn checked-rendered-response!
  "Validate one actual rendered surface *before* invoking response-fn.

   response-fn must be a callable node -> Ring-response renderer such as
   gesso.core/html-response. On validation failure response-fn is never called,
   so malformed or route-incoherent canonical affordances cannot reach browser
   serialization through this boundary.

   This is explicit runtime enforcement, not a claim that all application
   handlers have been statically proven to use the boundary."
  [application-assembly surface response-fn rendered]
  (when-not (ifn? response-fn)
    (throw
     (preflight-error
      :invalid-response-renderer
      "checked-rendered-response! requires a callable response renderer."
      {:surface surface
       :response-fn response-fn})))
  (require-rendered-surface! application-assembly surface rendered)
  (response-fn rendered))

;; =============================================================================
;; Canonical application-handler HTML boundary
;; =============================================================================

(defn wrap-application-handler
  "Wrap one ordinary one-argument application/Ring handler so every nested
   canonical Gesso HTML response is checked against application-assembly before
   Rum serialization.

   The wrapper installs the generic gesso.http pre-serialization guard once for
   the dynamic extent of each handler invocation. Existing descendants may keep
   calling gesso.core/html-response or gesso.http/html-response; they do not need
   a special response function or an explicit checked-rendered-response! call.

   Each actual Hiccup body is validated with require-rendered-surface! using the
   stable diagnostic surface canonical-html-response-surface. Semantic operation,
   browser-plan, method, concrete URL, trusted route, and current artifact
   correspondence are still derived from the rendered value and current
   ApplicationAssembly. The surface keyword is diagnostic only and grants no
   authority.

   This closes the producer -> checked canonical Gesso HTML boundary *relative
   to this wrapped handler invocation*. It does not claim that arbitrary manual
   Ring responses containing pre-rendered HTML pass through gesso.http/html-response,
   nor does ApplicationAssembly by itself prove that every application entrypoint
   installed this wrapper.

   application-assembly must be current when the wrapper is created. It is also
   rechecked for every actual HTML response by require-rendered-surface!, so an
   artifact/receipt that becomes stale after wrapper construction still fails
   before serialization."
  [application-assembly handler]
  (biff-lifecycle/wrap-application-handler
   #'application-assembly?
   #'require-rendered-surface!
   application-assembly
   handler))

(def application-handler-module
  "Native Biff 2 lifecycle module that installs Gesso's canonical HTML
   application-preflight wrapper.

   Include this exact module in the application's modules Var and place
   application-handler-module-id first in the Biff start order. Biff runs all
   :biff.core/init entries before lifecycle start, so :biff.ring/handler must be
   supplied by module initialization or initial-system before this module starts.
   A server module may then start later and consume the already wrapped handler.

   The current ApplicationAssembly is supplied by start-biff-application! through
   application-assembly-system-key. The module rechecks it at installation time,
   wraps only :biff.ring/handler, and removes the temporary assembly key before
   returning. The installed handler still rechecks the assembly for every
   canonical HTML response, preserving fail-closed artifact/render currentness."
  (biff-lifecycle/make-application-handler-module
   #'application-assembly?
   #'require-rendered-surface!))

(defn start-biff-application!
  "Start a released Biff 2 application through Gesso's canonical application-
   preflight lifecycle boundary.

   This mirrors biff.core/start's two arities with a current
   ApplicationAssembly prepended:

     (start-biff-application!
      application-assembly
      modules-var
      start-order)

     (start-biff-application!
      application-assembly
      initial-system
      modules-var
      start-order)

   modules-var must contain application-handler-module exactly once, and
   application-handler-module-id must be the first entry in start-order. This is
   the released Biff 2 lifecycle contract: modules carry stable :biff.core/id and
   :biff.core/start values, while start-order contains qualified module IDs.

   Gesso places application-assembly into temporary initial startup state and
   delegates to biff.core/start without mutating modules-var. Biff first combines
   all module :biff.core/init values with initial-system. Gesso's first lifecycle
   module then requires an actual :biff.ring/handler, installs the checked wrapper,
   removes the temporary assembly key, and returns the system for later modules.
   Consequently a canonical server module cannot start before preflight.

   Direct calls to biff.core/start, a different start order, a later module that
   deliberately replaces :biff.ring/handler, a server that ignores that handler,
   or manually pre-serialized HTML remain explicit boundaries outside this
   canonical path."
  ([application-assembly modules-var start-order]
   (biff-lifecycle/start-biff-application!
    #'application-assembly?
    application-handler-module
    application-assembly
    modules-var
    start-order))
  ([application-assembly initial-system modules-var start-order]
   (biff-lifecycle/start-biff-application!
    #'application-assembly?
    application-handler-module
    application-assembly
    initial-system
    modules-var
    start-order)))

;; =============================================================================
;; Closed current physical application product
;; =============================================================================

(defn- application-assembly-shape?
  [value]
  (and
   (or
    (closed-map? assembly-keys value)
    (closed-map? assembly-with-rendered-surfaces-keys value))
   (= application-assembly-type
      (:gesso.live.application-preflight/type value))
   (= preflight-version
      (:gesso.live.application-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))
   (operation-summary/operation-acquisition-assembly-shape?
    (:operation-acquisition-assembly value))
   (nonblank-string? (:browser-artifact-path value))
   (or (nil? (:browser-receipt-path value))
       (nonblank-string? (:browser-receipt-path value)))
   (or
    (not (contains? value :rendered-surfaces))
    (rendered-surfaces? (:rendered-surfaces value)))))

(defn- current-application-report
  [value]
  (when (application-assembly-shape? value)
    (try
      (let [report
            (check-application-assembly
             (cond->
              {:name (:name value)
               :operation-acquisition-assembly
               (:operation-acquisition-assembly value)
               :browser-artifact-path (:browser-artifact-path value)
               :browser-receipt-path (:browser-receipt-path value)}
               (contains? value :rendered-surfaces)
               (assoc :rendered-surfaces (:rendered-surfaces value))))]
        (when (true? (:valid? report))
          report))
      (catch Exception _
        nil))))

(defn application-assembly?
  "True for one current ApplicationAssembly whose nested semantic/runtime
   backbone remains closed and whose physical browser artifact still verifies.

   This predicate performs one fresh whole-application derivation by design.
   v623 no longer validates the same freshly generated report again through the
   public report recognizer."
  [value]
  (boolean (current-application-report value)))

(defn require-application-assembly!
  "Require the modeled application runtime backbone and return one current physical
   ApplicationAssembly.

   Optional rendered surfaces provide static sample evidence and close every
   discovered canonical affordance to the route/runtime backbone. Dynamic render
   completeness is enforced on the canonical Gesso/Biff response path rather than
   fabricated as a static enumeration proof. The assembly itself still does not
   prove deployment adoption of that canonical startup/response boundary."
  [options]
  (let [{:keys [name
                operation-acquisition-assembly
                browser-artifact-path
                browser-receipt-path
                rendered-surfaces]
         :as options'}
        (validate-options! options)

        report
        (check-application-assembly options')]
    (when-not (true? (:valid? report))
      (throw
       (preflight-error
        :application-backbone-preflight-failed
        "Gesso whole-application runtime-backbone preflight failed."
        {:preflight report})))
    (let [assembly
          (cond->
           {:gesso.live.application-preflight/type application-assembly-type
            :gesso.live.application-preflight/version preflight-version
            :name name
            :operation-acquisition-assembly operation-acquisition-assembly
            :browser-artifact-path browser-artifact-path
            :browser-receipt-path browser-receipt-path}
            (contains? options' :rendered-surfaces)
            (assoc :rendered-surfaces rendered-surfaces))]
      (when-not (application-assembly? assembly)
        (throw
         (preflight-error
          :invalid-emitted-application-assembly
          "Application preflight produced an internally inconsistent or no-longer-current assembly."
          {:assembly assembly
           :preflight report})))
      assembly)))

(defn explain-operation
  "Explain one route-exposed operation through the assembled application
   backbone without requiring callers to traverse nested preflight products.

   Currentness and the full derived operation summary come from the same fresh
   application report, avoiding a second recursive validation pass."
  [application-assembly operation]
  (if-let [report (current-application-report application-assembly)]
    (assurance/explain-operation-from-report report operation)
    (throw
     (preflight-error
      :invalid-application-assembly
      "Operation explanation requires a current ApplicationAssembly."
      {:application-assembly application-assembly}))))

(defn explain
  "Return one compact current application explanation.

   The explanation separates five things that should not be conflated:

     :guarantee                  verified assembly closure;
     :canonical-html-enforcement enforcement available on canonical Gesso/Biff startup;
     :open-obligations           deployment/coverage facts not carried by the assembly;
     :trusted-assumptions        named facts outside the verified assembly core;
     :explicit-escape-hatches    deliberate ways arbitrary Clojure can bypass the canonical path.

   Successful explanations additionally expose a source-derived semantic audit.
   The audit identifies operation-local trusted model boundaries and explicitly
   unknown business-rule coverage; it does not imply certification of opaque
   Clojure code. Recognized unsuccessful reports retain their existing failure
   explanation without claiming a successful semantic audit.

   One fresh application report is reused throughout the explanation."
  [value]
  (cond
    (application-assembly-shape? value)
    (if-let [report (current-application-report value)]
      (assoc (assurance/explain-application-from-report value report)
             :semantic-audit
             (semantic-audit/audit-from-current-report report))
      (throw
       (preflight-error
        :unrecognized-value
        "Expected an application preflight report or current ApplicationAssembly."
        {:value value})))

    (report? value)
    (cond-> (assurance/explain-report-from-report value)
      (:valid? value)
      (assoc :semantic-audit
             (semantic-audit/audit-from-current-report value)))

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an application preflight report or current ApplicationAssembly."
      {:value value}))))


;; =============================================================================
;; Opt-in aggregate XTDB candidate-evidence consumer (NOT admission)
;; =============================================================================

(def ^:private expected-tx-refinement-premise-count 9)

(def ^:private tx-refinement-consumer-nonclaims
  #{:actual-executor-to-transaction-plan-binding
    :operation-branch-or-prestate-completeness
    :application-policy-or-external-writer-closure
    :evidence-artifact-authentication
    :independent-proof-verification
    :any-xtdb-refinement-premise-discharged
    :physical-execution-refinement
    :application-certification-or-admission})

(defn- reject-refinement-candidates!
  [reason operation]
  ;; Do not include a supplied prestate, plan, descriptor, digest, claimed
  ;; artifact, or exception cause. Those may contain confidential application
  ;; data even if the upstream reviewer normally redacts its own reports.
  (throw
   (preflight-error
    :refinement-candidate-review-failed
    "The selected XTDB refinement candidate package is not ready for independent review."
    {:reason reason
     :operation operation})))

(defn- aggregate-review-ready?
  [review]
  (let [aggregate (:aggregate-candidate-package review)
        requirements (:requirements review)
        proposals (:proposals review)]
    ;; Require the actual v1039 aggregate decision and its independent
    ;; non-certification statements. Per-premise completeness, or caller
    ;; supplied booleans that merely resemble it, are not a substitute.
    (and (= :gesso.model.tx/instance-refinement-evidence-review-v1
            (:obligation review))
         (= :independent-review-required (:status review))
         (true? (:instance-subject-derived? review))
         (true? (:plan-specific-subject-bound? review))
         (= :checked (:local-argument-status review))
         (= :complete-unverified-candidate-package (:status aggregate))
         (= :declared-application-instance (:binding-scope aggregate))
         (true? (:all-proposals-reviewable? aggregate))
         (true? (:all-premise-candidate-packages-complete? aggregate))
         (true? (:ready-for-independent-review? aggregate))
         (vector? requirements)
         (= expected-tx-refinement-premise-count (count requirements))
         (= expected-tx-refinement-premise-count
            (count (set (map :id requirements))))
         (every? #(and (= :undischarged (:status %))
                       (true? (:review-package-complete? %)))
                 requirements)
         (vector? proposals)
         (seq proposals)
         (every? #(and (= :review-required (:status %))
                       (false? (:authenticated? %))
                       (false? (:proof-checked? %))
                       (false? (:discharges-premise? %)))
                 proposals)
         (= #{} (:external-premises-discharged review))
         (every? false?
                 [(:verified? aggregate)
                  (:premises-discharged? aggregate)
                  (:application-admitted? aggregate)
                  (:certifies-physical-execution? aggregate)
                  (:evidence-authenticated? review)
                  (:any-proof-checked? review)
                  (:backend-contract-admitted? review)
                  (:general-refinement-proved? review)
                  (:certifies-physical-execution? review)
                  (:instance-declarations-authenticated? review)
                  (:application-dependencies-proved-complete? review)
                  (:instance-evidence-verified? review)]))))

(defn require-refinement-candidate-packages!
  "Check candidate evidence for explicitly selected registered operations.

   Requires a CURRENT ApplicationAssembly, NOT a caller-supplied preflight
   report. `cases` must be a nonempty map from a registered operation keyword
   to exactly {:abstract-state :plan :environment :proposals}. For each
   supplied selection this consumer derives a fresh instance-bound review and
   requires the v1039 AGGREGATE complete/unverified decision; individual
   :review-package-complete? flags never confer readiness. An incomplete,
   rejected, malformed, or cross-instance batch fails closed.

   This is an OPT-IN consumer for assembling candidate proof packages, not a
   startup requirement or application-admission gate. It checks ONE caller-
   selected state/plan per named operation, and does NOT establish that the
   supplied plan is the actual registered executor, that every branch/prestate
   was considered, or that supplied environment/source declarations are true.
   Its returned summaries deliberately contain no proofs, subjects, hashes,
   application states, executors, or source declarations. All nine external
   XTDB premises remain undischarged; NO application guarantee is upgraded."
  [application-assembly cases]
  (if-let [report (current-application-report application-assembly)]
    (let [registered (set (keys (get-in report [:analysis :operations])))]
      (when-not (and (map? cases)
                     (seq cases)
                     (<= (count cases) (count registered))
                     (every? #(and (keyword? %) (contains? registered %))
                             (keys cases)))
        (reject-refinement-candidates! :invalid-selected-operations nil))
      (let [summaries
            (into (sorted-map)
                  (for [[operation candidate] cases]
                    (do
                      (when-not (and (map? candidate)
                                     (= #{:abstract-state :plan :environment
                                          :proposals}
                                        (set (keys candidate)))
                                     (map? (:environment candidate))
                                     (= operation
                                        (get-in candidate
                                                [:environment :application
                                                 :operation])))
                        (reject-refinement-candidates!
                         :invalid-or-mismatched-instance-declaration operation))
                      (let [review
                            (try
                              (tx-refinement-evidence/review-instance-evidence
                               (:abstract-state candidate)
                               (:plan candidate)
                               (:environment candidate)
                               (:proposals candidate))
                              (catch Exception _
                                (reject-refinement-candidates!
                                 :invalid-instance-or-evidence-input operation)))]
                        (when-not (aggregate-review-ready? review)
                          (reject-refinement-candidates!
                           :aggregate-package-not-ready operation))
                        [operation
                         {:status :complete-unverified-candidate-package
                          :ready-for-independent-review? true
                          :instance-subject-derived? true
                          :evidence-verified? false
                          :premises-discharged? false
                          :application-admitted? false}]))))]
        {:status :selected-candidates-ready-for-independent-review
         :scope :explicitly-selected-declared-transaction-instances
         :reviewed-operation-count (count summaries)
         :registered-operation-count (count registered)
         :selected-operations summaries
         :evidence-authenticated? false
         :premises-discharged? false
         :application-admitted? false
         :application-guarantee-upgraded? false
         :not-established tx-refinement-consumer-nonclaims}))
    (reject-refinement-candidates! :invalid-or-stale-application-assembly nil)))


;; A physical execution witness is deliberately NOT included in the ordinary
;; application explanation. Diagnostic executions are optional, process-local,
;; and cannot establish the application's behavior in general. The caller must
;; opt in with opaque handles issued by resolution-assurance/run-and-check!.
(def ^:private execution-observation-executor-root
  [:analysis :operation-acquisition-assembly
   :execution-assembly :server :operations])

(def ^:private executor-bound-refinement-nonclaims
  ;; An observed, exactly registered executor attempt provides a stronger
  ;; provenance link than caller-selected transaction plans. It still says
  ;; nothing universal about other branches, states or the truth of supplied
  ;; application/backend source and policy descriptors.
  (-> tx-refinement-consumer-nonclaims
      (disj :actual-executor-to-transaction-plan-binding)
      (conj :unobserved-executor-branches-and-prestates
            :complete-executor-model-preparation-correspondence
            :process-independent-observation-provenance)))

(defn require-executor-bound-refinement-candidates!
  "Review candidate evidence for ONE OBSERVED physical attempt per selected
   operation, binding its plan and prestate to the exact CURRENT registered
   executor rather than trusting a caller-selected plan.

   `cases` is {registered-operation
               {:evidence-handle opaque-process-local-handle
                :environment declared-backend-gesso-and-application
                :proposals candidate-proposals}}. The handle must originate in
   resolution-assurance/run-and-check!, which captures the same executor's
   submitted transaction and physical initial state. This function performs
   the exact-executor process-local registry check and the source-derived
   prepared-event correspondence check BEFORE deriving a transaction instance
   subject and requiring the nine-premise aggregate candidate-package review.

   The caller cannot supply :plan or :abstract-state on this path. The selected
   environment and candidate artifacts remain unauthenticated declarations.
   No arbitrary function invocation occurs here; this consumes an ALREADY
   COMPLETED, definitive controlled diagnostic. Missing, forged, stale,
   mismatched, replay-only and indeterminate handles fail closed.

   This is an opt-in ONE-ATTEMPT diagnostic path, not production admission,
   universal executor verification, complete domain-guard coverage, evidence
   authentication, or discharge of any XTDB premise. Returned data is redacted
   and contains no plan, prestate, handle, source digest or command binding."
  [application-assembly cases]
  (if-let [report (current-application-report application-assembly)]
    (let [registered (set (keys (get-in report [:analysis :operations])))]
      (when-not (and (map? cases)
                     (seq cases)
                     (<= (count cases) (count registered))
                     (every? #(and (keyword? %) (contains? registered %))
                             (keys cases)))
        (reject-refinement-candidates! :invalid-selected-operations nil))
      (let [derived
            (into (sorted-map)
                  (for [[operation selection] cases]
                    (do
                      (when-not (and (map? selection)
                                     (= #{:evidence-handle :environment :proposals}
                                        (set (keys selection)))
                                     (some? (:evidence-handle selection))
                                     (map? (:environment selection))
                                     (= operation
                                        (get-in selection
                                                [:environment :application
                                                 :operation])))
                        (reject-refinement-candidates!
                         :invalid-or-mismatched-executor-selection operation))
                      (let [executor (get-in report
                                             (conj execution-observation-executor-root
                                                   operation :execute!))
                            result (resolution-assurance/inspect-evidence-for-executor
                                    (:evidence-handle selection) executor)
                            observation (:observation result)]
                        (when-not (and (= :bounded-execution-correspondence
                                          (:status result))
                                       (= operation (get-in result [:binding :operation]))
                                       (map? observation)
                                       (= :consistent
                                          (get-in observation [:evidence :status]))
                                       (= operation
                                          (get-in observation [:binding :operation])))
                          (reject-refinement-candidates!
                           :unrecognized-executor-observation operation))
                        ;; A successful process-local identity lookup by itself
                        ;; is not enough: require that the captured physical
                        ;; attempt refines its source-derived reference step.
                        (let [checked
                              (try
                                (formal-observation/prepared-event-case observation)
                                (catch Exception _
                                  (reject-refinement-candidates!
                                   :noncorresponding-executor-observation operation)))]
                          (when-not (= :single-executor-attempt-diagnostic
                                       (:observation-origin checked))
                            (reject-refinement-candidates!
                             :noncorresponding-executor-observation operation)))
                        (let [plan (get-in observation
                                           [:submitted-incorporation :plan])
                              prestate (:initial-state observation)]
                          (when-not (and (map? plan) (map? prestate))
                            (reject-refinement-candidates!
                             :incomplete-executor-observation operation))
                          [operation
                           {:abstract-state prestate
                            :plan plan
                            :environment (:environment selection)
                            :proposals (:proposals selection)}])))))]
        ;; This established consumer independently rechecks assembly
        ;; currentness and the entire v1039 aggregate decision. A restart,
        ;; hot-reload or replaced physical artifact between the two reviews
        ;; therefore fails closed rather than minting stale review readiness.
        (let [review (require-refinement-candidate-packages!
                      application-assembly derived)]
          (assoc review
                 :status :observed-executor-candidates-ready-for-independent-review
                 :scope :exact-registered-executor-controlled-attempt
                 :observation-count (count derived)
                 :evidence-origin :process-local-executor-bound-observation
                 :not-established executor-bound-refinement-nonclaims))))
    (reject-refinement-candidates! :invalid-or-stale-application-assembly nil)))


;; This is the MINIMUM assurance contract for the opt-in finite-model checker,
;; independently enforced by the application-facing consumer. A returned set's
;; type alone is not evidence that essential premises or limitations survived
;; transport. Keep this vocabulary in correspondence with the formally admitted
;; checker scope; additions by that checker may make claims WEAKER, not stronger.
;; Do not require Recife to be on the ordinary application-preflight classpath.
(def ^:private required-finite-model-assumptions
  #{:complete-selected-finite-prestate-and-plans
    :serial-atomic-reference-semantics
    :faithful-reversible-tlc-token-encoding
    :correct-recife-tlc-execution})

(def ^:private required-finite-model-nonclaims
  #{:complete-application-operation-enumeration
    :generic-inductive-theorem
    :actual-xtdb-implementation-refinement
    :authorization-or-application-policy
    :server-browser-composition
    :independent-application-certificate})

(defn- invalid-execution-observation!
  [kind details]
  (throw
   (preflight-error
    kind
    "Execution observations require a current assembly and exact executor-bound diagnostic handles."
    details)))

(defn- require-execution-observation-input!
  [report observations]
  (let [operations (set (keys (get-in report [:analysis :operations])))]
    (when-not (and (map? observations)
                   (every? (fn [[operation handles]]
                             (and (contains? operations operation)
                                  (vector? handles)
                                  (seq handles)
                                  (every? some? handles)))
                           observations))
      (invalid-execution-observation!
       :invalid-execution-observation-request
       {:expected :map-of-registered-operation-to-nonempty-handle-vector
        :registered-operations operations
        :supplied-operations (when (map? observations)
                               (set (keys observations)))}))
    observations))

;; TLC failures are local diagnostics, not public assurance evidence. Preserve
;; only the closed, framework-owned explanation vocabulary in the *outer*
;; application error. Raw traces and process-local inspection handles stay in
;; the underlying JVM exception, which callers must never serialize or log as
;; an HTTP error response. Application errors deliberately do not depend on
;; the Recife namespace or load a checker during ordinary preflight.
(def ^:private finite-checker-failure-classes
  #{:invariant-counterexample
    :model-counterexample
    :backend-error
    :backend-exception
    :checker-interrupted
    :checker-timeout
    :malformed-checker-result
    :inconclusive-checker-result
    :unrecognized-checker-result})

(def ^:private finite-checker-obligation-ids
  #{:action-accounting
    :observation-chain
    :rejected-is-atomic
    :committed-write-set
    :outcome-check-consistency})

;; These values are framework-owned diagnostic provenance, not names or
;; evidence supplied by the application. Admit only known constants from the
;; current finite checker. Unknown/future backends must remain unclassified
;; until an explicit contract update, never acquire a plausible source label
;; by echoing arbitrary backend metadata.
(def ^:private finite-checker-source-boundaries
  #{:prepared-transaction-finite-model :recife-tlc-execution})
(def ^:private finite-checker-subject-kind
  :gesso.model.formal-transition/prepared-serial-v1)
(def ^:private finite-checker-backend
  :recife-0.22.0/tlc)

(defn- safe-finite-transition-prefix [prefix]
  ;; The TLC backend exposes only bounded state/plan indices and enum statuses
  ;; in its safe summary. Never forward arbitrary backend-provided maps, even
  ;; if a checker exception claims to contain a :diagnostic/summary.
  (when (vector? prefix)
    (mapv
     (fn [state]
       (let [index (when (map? state) (:state-index state))
             observations (when (map? state) (:observations state))]
         (cond-> {}
           (and (integer? index) (<= 0 index 1000000))
           (assoc :state-index index)
           (vector? observations)
           (assoc :observations
                  (mapv (fn [observation]
                          (let [plan (when (map? observation)
                                       (:plan-index observation))
                                status (when (map? observation)
                                         (:status observation))]
                            (cond-> {}
                              (and (integer? plan) (<= 0 plan 5))
                              (assoc :plan-index plan)
                              (contains? #{:committed :rejected} status)
                              (assoc :status status))))
                        (take 6 observations))))))
     (take 32 prefix))))

(defn- safe-finite-checker-failure [exception]
  (let [data (when (instance? clojure.lang.ExceptionInfo exception)
               (ex-data exception))
        recognized? (= :gesso.model.formal-check/invalid-formal-check
                       (:error/type data))
        summary (when (and recognized? (map? (:diagnostic/summary data)))
                  (:diagnostic/summary data))
        class (:failure/class data)
        ;; When a checker reports two conflicting classifications, never
        ;; attach a credible-sounding invariant label to the application
        ;; error. An absent summary class is allowed for older emitters.
        consistent-class?
        (or (not (contains? summary :failure/class))
            (= class (:failure/class summary)))
        recognized-class?
        (and recognized? consistent-class?
             (contains? finite-checker-failure-classes class))
        obligation (:failed-obligation summary)
        states (:distinct-states summary)
        prefix (safe-finite-transition-prefix (:transition-prefix summary))
        boundary (:source-boundary summary)
        subject-kind (:subject-kind summary)
        checker (:checker summary)
        plan-count (:selected-plan-count summary)]
    (cond-> {:failure/class
             (if recognized-class? class :unclassified-checker-failure)}
      (and recognized-class? (contains? finite-checker-obligation-ids obligation))
      (assoc :failed-obligation obligation)
      (and recognized-class? (integer? states) (<= 1 states 1000000000))
      (assoc :distinct-states states)
      (and recognized-class? (seq prefix))
      (assoc :transition-prefix prefix)
      ;; Additional source attribution is safe only for exact framework
      ;; vocabulary. These fields describe the diagnostic's stated scope;
      ;; neither they nor the outer exception establish an application proof.
      (and recognized-class? (contains? finite-checker-source-boundaries boundary))
      (assoc :source-boundary boundary)
      (and recognized-class? (= finite-checker-subject-kind subject-kind))
      (assoc :subject-kind subject-kind)
      (and recognized-class? (= finite-checker-backend checker))
      (assoc :checker checker)
      (and recognized-class? (integer? plan-count) (<= 1 plan-count 6))
      (assoc :selected-plan-count plan-count))))
(defn- reject-finite-model-check!
  [operation index exception]
  (let [details
        {:operation operation
         :observation-index index
         :formal/failure (if exception
                           (safe-finite-checker-failure exception)
                           {:failure/class :invalid-checker-result})}
        error (preflight-error
               :finite-model-check-failed
               "Selected finite-model checking did not establish required obligations."
               details)]
    ;; Only the outer ex-data is safe application-facing metadata. The cause
    ;; preserves the backend's local inspection handle for explicit, trusted
    ;; JVM debugging, and may contain confidential exception-chain material.
    (throw (if exception
             (ex-info (ex-message error) (ex-data error) exception)
             error))))

(defn- checked-operation-observations
  [report operation handles model-check?]
  (let [executor (get-in report
                         (conj execution-observation-executor-root
                               operation :execute!))
        observations
        (mapv
         (fn [index handle]
           ;; Only opaque handles from the v815 process-local registry can pass,
           ;; and only for this exact currently registered executor. Never
           ;; accept a copied report, matching operation keyword or metadata.
           (let [result (resolution-assurance/inspect-evidence-for-executor
                         handle executor)]
             (when-not (and (= :bounded-execution-correspondence
                               (:status result))
                            (= operation (get-in result [:binding :operation]))
                            (= :consistent
                               (get-in result [:observation :evidence :status]))
                            (contains? #{:committed :rejected}
                                       (get-in result [:observation :physical :status])))
               (invalid-execution-observation!
                :unrecognized-execution-observation
                {:operation operation :observation-index index}))
             ;; The exact-executor registry authenticates THIS evidence handle,
             ;; not an arbitrary caller-supplied map. Derive a finite formal
             ;; subject only from the completed observation inside that handle.
             ;; The bridge checks the admitted plan, every inspected target and
             ;; the source-derived transition for the observed physical step.
             ;; No Recife run, theorem or application-wide proof is implied.
             (let [case (try
                          (formal-observation/prepared-event-case
                           (:observation result))
                          (catch clojure.lang.ExceptionInfo e
                            (invalid-execution-observation!
                             :invalid-execution-reference-observation
                             {:operation operation :observation-index index
                              :cause-kind (:error/kind (ex-data e))})))
                   replay (formal-observation/observed-path-report case)]
               (when-not (and (= :single-executor-attempt-diagnostic
                                 (:observation-origin case))
                              (= :corresponds (:status replay))
                              (= [0] (:checked-order replay)))
                 (invalid-execution-observation!
                  :noncorresponding-execution-reference
                  {:operation operation :observation-index index}))
               ;; Recife and its classpath are loaded ONLY for the explicit
               ;; formal diagnostic API. Ordinary application admission and
               ;; explanations do not load TLC or run a model checker.
               (let [finite-check
                     (when model-check?
                       (let [result
                             (try
                               ((requiring-resolve
                                 'gesso.model.formal-check/check-prepared!)
                                (:subject case))
                               (catch Exception error
                                 (reject-finite-model-check!
                                  operation index error)))]
                         (when-not
                          (and (map? result)
                               (= :checked-finite-model (:status result))
                               (= :bounded-reference-model-check-not-certificate
                                  (:evidence-class result))
                               (= (:kind (:subject case)) (:subject-kind result))
                               (= :recife-0.22.0/tlc (:checker result))
                               (pos-int? (:distinct-states result))
                               (= #{:action-accounting
                                    :observation-chain
                                    :rejected-is-atomic
                                    :committed-write-set
                                    :outcome-check-consistency}
                                  (:obligations result))
                               (set? (:assumed result))
                               (set/subset? required-finite-model-assumptions
                                            (:assumed result))
                               (set? (:not-established result))
                               (set/subset? required-finite-model-nonclaims
                                            (:not-established result)))
                           (reject-finite-model-check!
                            operation index nil))
                         ;; Only checker-supplied finite scope and invariant IDs;
                         ;; no actual documents, commands or model states.
                         (select-keys result
                                      [:status :evidence-class :subject-kind
                                       :checker :distinct-states :obligations
                                       :assumed :not-established])))]
                 ;; Do not leak opaque handles, principal, arguments, raw
                 ;; snapshots or receipts through the application explanation.
                 (cond->
                  {:index index
                   :status :bounded-correspondence-observed
                   :physical-status (get-in result [:observation :physical :status])
                   :finite-reference-status
                   (get-in result [:observation :evidence :status])}
                   model-check? (assoc :finite-model-safety-check finite-check))))))
         (range (count handles))
         handles)]
    {:status :bounded-process-local-execution-observations
     :evidence-class :exact-registered-executor-diagnostic-handle
     :observation-count (count observations)
     :observations observations
     :source-derived-reference-check
     {:status :selected-attempts-correspond
      :checked-attempt-count (count observations)
      :subject (:subject formal-observation/event-observation-bridge-scope)
      :assurance-class :finite-reference-replay-not-formal-proof
      :model-checked? false}
     :scope resolution-assurance/assurance-scope
     :assurance-class :bounded-observation-not-proof
     :universal-guarantees :not-established}))

(defn- explain-with-execution-evidence*
  "Explain a CURRENT ApplicationAssembly together with separately collected
   bounded diagnostic execution observations.

   `observations` is {registered-operation [opaque-handle ...]} using handles
   issued by resolution-assurance/run-and-check! in this JVM. The evidence is
   inspected against the EXACT executor obtained from the freshly rechecked
   assembly. Invalid, stale, forged, cloned, cross-operation or cross-adapter
   handles FAIL CLOSED. This API deliberately refuses bare preflight reports:
   report-shaped data is not a current assembly or an executor-identity proof.

   An explicit empty map is permitted and means NO executions were observed.
   No observation is implied for other operations. These observations are
   diagnostic facts about FINITE executions, never whole-application proof,
   authorization proof, projection correctness or universal command replay.
   Each supplied handle is also checked against the actual source-derived
   finite transaction step. The ordinary public entry point performs only
   deterministic reference replay; the separate formal opt-in entry point
   additionally runs Recife/TLC on selected finite subjects. Neither proves
   universal correspondence. No business callbacks or transactions execute.

   The original explain function remains unchanged and does not acquire a
   process-local diagnostic dependency. Returned explanations contain only
   redacted observational summaries, not handles, command bindings, receipts,
   transaction plans, principals, or actual database document contents."
  [assembly observations model-check?]
  (if-let [report (current-application-report assembly)]
    (let [observations (require-execution-observation-input!
                        report observations)
          ;; Build from the SAME freshly checked report used to inspect each
          ;; operation's exact registered executor. Preserve every existing
          ;; assurance class and open business-policy obligation unchanged.
          audit (semantic-audit/audit-from-current-report report)
          checked (into {}
                        (map (fn [[operation handles]]
                               [operation (checked-operation-observations
                                           report operation handles model-check?)]))
                        observations)
          audit' (-> audit
                     (assoc :execution-observation-summary
                            (cond->
                             {:status :diagnostic-execution-observations-only
                              :observed-operation-count (count checked)
                              :observation-count
                              (reduce + 0 (map :observation-count (vals checked)))
                              :source :current-assembly-exact-executor-registry
                              :not-established
                              #{:all-executions-observed
                                :complete-business-policy-guards
                                :authorization-correctness
                                :truthful-application-projections
                                :universal-refinement-or-certification}}
                              model-check?
                              (assoc :finite-model-safety-check
                                     {:status :selected-finite-subjects-checked
                                      :checked-subject-count
                                      (reduce + 0 (map :observation-count (vals checked)))
                                      :assurance-class
                                      :bounded-reference-model-check-not-certificate
                                      :subject-origin
                                      :exact-executor-observed-attempt
                                      :universal-guarantees :not-established})))
                     (update :operation-cards
                             (fn [cards]
                               (reduce-kv
                                (fn [cards operation evidence]
                                  (assoc-in cards
                                            [operation :runtime-execution-observations]
                                            evidence))
                                cards checked))))]
      (assoc (assurance/explain-application-from-report assembly report)
             :semantic-audit audit'))
    (invalid-execution-observation!
     :invalid-or-stale-observation-assembly
     {:reason :requires-current-application-assembly})))


(defn explain-with-execution-evidence
  "Explain source-derived correspondence for exact-executor diagnostic handles.

   This established API is intentionally NOT a model-checking entry point.
   It does not load Recife and preserves the non-certifying original report."
  [assembly observations]
  (explain-with-execution-evidence* assembly observations false))

(defn explain-with-formal-execution-evidence
  "Opt in to Recife/TLC checks for each authenticated executor observation.

   Accepts the same CURRENT ApplicationAssembly and opaque, exact-executor
   diagnostic handles as explain-with-execution-evidence. Each checked finite
   subject is derived from the actual admitted plan and observed prestate,
   AFTER executor identity, physical completeness and reference correspondence
   have been verified. TLC explores all enabled orders of each supplied
   subject and discharges the five generic prepared-transaction safety
   obligations. The result cannot verify other prestates, other operation
   inputs, absent traces, policy coverage, truthful projections, or actual
   XTDB/browser refinement. These checks are optional, expensive, JVM-only,
   and must run under an enclosing timeout; they do not run at normal startup.

   Invalid evidence, malformed model-checker outputs, checker failures or
   missing Recife support fail closed. Formal-check failures carry an
   application-safe :formal/failure summary (a recognized failure class,
   named obligation, bounded state/plan/status prefix and strictly enumerated
   checker/subject/source attribution when available).
   The underlying exception cause is LOCAL, may contain confidential TLC
   data or an inspection handle, and must not be serialized to clients.
   Neither success nor failure issues application certification. An empty
   observation map reports zero checks and proves nothing."
  [assembly observations]
  (explain-with-execution-evidence* assembly observations true))

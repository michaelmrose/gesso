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

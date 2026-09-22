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
   application-handler-component lifts the wrapper into Biff's :biff.ring/handler
   lifecycle; and start-biff-application! makes that component mandatory on the
   canonical Gesso/Biff startup path.

   The resulting guarantee is verified assembly plus pre-browser enforcement for
   canonical Gesso HTML responses when that startup path is used. ApplicationAssembly
   itself cannot prove deployment used the canonical startup, that later arbitrary
   Clojure did not replace :biff.ring/handler, that a server did not ignore the
   standard Biff handler key, or that code did not hand-build pre-serialized HTML
   Ring responses. Those are explicit escape/deployment boundaries, not hidden
   holes or fabricated static proof obligations.

   Recognition remains physical and fail-closed: current artifact bytes/receipt,
   nested assemblies, rendered metadata, operation/plan identity, route method/path,
   anonymous HTMX POST collisions with assembled semantic routes, and authoritative
   acquisition are revalidated from their source facts. A semantic command route may
   never silently degrade into an ordinary unmarked HTMX POST merely because an
   application failed to construct its canonical :choreo/op binding. Browser
   metadata never grants authority, and trusted application/model declarations remain
   named assumptions rather than being promoted to v4.5 machine-checked proof."
  (:require
   [clojure.set :as set]
   [com.biffweb.core :as biff]
   [clojure.string :as str]
   [gesso.http :as http]
   [gesso.live.browser.build :as browser-build]
   [gesso.live.operation-acquisition-preflight :as operation-acquisition]
   [gesso.live.ui :as ui]))

;; =============================================================================
;; Identity / closed vocabulary
;; =============================================================================

(def preflight-version 3)

(def report-type
  :gesso.live.application-preflight/report)

(def application-assembly-type
  :gesso.live.application-preflight/application-assembly)

(def backbone-guarantee
  :application-runtime-backbone-preflight-closed)

(def rendered-affordance-guarantee
  :supplied-rendered-affordances-preflight-closed)

(def rendered-surface-guarantee
  :rendered-surface-pre-browser-preflight-closed)

(def rendered-surface-report-type
  :gesso.live.application-preflight/rendered-surface-report)

(def canonical-html-response-surface
  :gesso.live.application-preflight/html-response)

(def biff-ring-handler-key
  "Canonical Biff 2 system key consumed by Ring server components."
  :biff.ring/handler)

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

(def ^:private operation-acquisition-assembly-keys
  #{:gesso.live.operation-acquisition-preflight/type
    :gesso.live.operation-acquisition-preflight/version
    :name
    :execution-assembly
    :acquisition-assembly})

(def ^:private affordance-open-obligation
  {:kind :static-affordance-closure-not-yet-modeled
   :edge :rendered-affordance->semantic-operation
   :status :open
   :message
   "No rendered-surface snapshot was supplied to this ApplicationAssembly, so it contains no static affordance sample. Canonical Gesso/Biff startup can still validate every actual canonical HTML render before serialization. What ApplicationAssembly alone cannot establish is deployment adoption of that checked startup/response boundary or absence of deliberate raw-response escape hatches."})

(def ^:private rendered-surface-completeness-open-obligation
  {:kind :rendered-surface-enumeration-completeness-not-yet-modeled
   :edge :application->rendered-surfaces
   :status :open
   :message
   "All canonical affordances in the supplied render snapshot are closed to the assembled application backbone. Arbitrary Clojure render state is not statically enumerable, so snapshot completeness is not treated as proof. Canonical Gesso/Biff startup validates each actual canonical HTML response dynamically; ApplicationAssembly alone still cannot prove that deployment used that boundary or avoided deliberate raw-response/handler-replacement escape hatches."})

(def ^:private canonical-html-enforcement
  {:status :available-on-canonical-gesso-biff-startup
   :startup-boundary :gesso.live.application-preflight/start-biff-application!
   :biff-handler-key biff-ring-handler-key
   :handler-boundary :gesso.live.application-preflight/wrap-application-handler
   :response-boundary :gesso.http/html-response
   :per-render-guarantee rendered-surface-guarantee
   :installation-proof :not-carried-by-application-assembly})

(def ^:private explicit-escape-hatches
  [{:kind :direct-biff-start-bypass
    :boundary :application-startup
    :description
    "Calling Biff startup directly instead of start-biff-application! can omit the canonical Gesso application-preflight component."}
   {:kind :later-handler-replacement
    :boundary :biff-component-order
    :description
    "Arbitrary later Clojure can deliberately replace :biff.ring/handler after Gesso installed the checked handler."}
   {:kind :server-ignores-biff-ring-handler
    :boundary :server-integration
    :description
    "A server that does not consume the canonical :biff.ring/handler system key is outside this integration guarantee."}
   {:kind :pre-serialized-html-ring-response
    :boundary :html-serialization
    :description
    "Hand-built Ring responses containing already-serialized HTML bypass gesso.http/html-response and therefore bypass canonical Hiccup affordance validation."}])

(def ^:private trusted-assumptions
  #{:application-publication-declarations-match-model-publication
    :trusted-command-route-declarations-match-installed-handlers
    :trusted-acquisition-route-declarations-match-installed-handlers
    :browser-artifact-emitted-through-gesso-owned-application-build
    :clojurescript-compiler-realizes-generated-entrypoint
    :authenticated-principal-binding-is-correct
    :public-model-operation-contracts-hold
    :xtdb-transaction-and-consistency-contracts-hold})

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
;; Nested assembly navigation / derivation
;; =============================================================================

(defn- execution-assembly
  [operation-acquisition-assembly]
  (:execution-assembly operation-acquisition-assembly))

(defn- route-assembly
  [operation-acquisition-assembly]
  (:route-assembly
   (execution-assembly operation-acquisition-assembly)))

(defn- operation-assembly
  [operation-acquisition-assembly]
  (:operation-assembly
   (route-assembly operation-acquisition-assembly)))

(defn- browser-assembly
  [operation-acquisition-assembly]
  (:browser-assembly
   (operation-assembly operation-acquisition-assembly)))

(defn- acquisition-assembly
  [operation-acquisition-assembly]
  (:acquisition-assembly operation-acquisition-assembly))

(defn- route-realizations-by-operation
  [route-assembly']
  (reduce-kv
   (fn [acc route-id realization]
     (update
      acc
      (:operation realization)
      (fnil conj [])
      {:route-id route-id
       :method (:method realization)
       :path (:path realization)
       :required-transport (:required-transport realization)}))
   (sorted-map)
   (:routes route-assembly')))

(declare application-assembly?
         application-assembly-shape?
         current-application-report
         check-application-assembly)

(defn- operation-acquisition-assembly-shape?
  [value]
  (and
   (closed-map? operation-acquisition-assembly-keys value)
   (= operation-acquisition/operation-acquisition-assembly-type
      (:gesso.live.operation-acquisition-preflight/type value))
   (= operation-acquisition/preflight-version
      (:gesso.live.operation-acquisition-preflight/version value))
   (or (nil? (:name value))
       (keyword? (:name value)))))

(defn- current-operation-acquisition-report
  "Return one freshly derived successful operation-acquisition report when the
   supplied closed-product shape and both embedded upstream assemblies remain
   current.  This internal helper deliberately uses the fresh report returned by
   check-operation-acquisition directly instead of asking report?/valid? to
   re-derive the same graph a second time."
  [value]
  (when (operation-acquisition-assembly-shape? value)
    (try
      (let [report
            (operation-acquisition/check-operation-acquisition
             {:name (:name value)
              :execution-assembly (:execution-assembly value)
              :acquisition-assembly (:acquisition-assembly value)})]
        (when (true? (:valid? report))
          report))
      (catch Exception _
        nil))))

(defn- operation-summary-from-report
  [operation-acquisition-assembly operation-acquisition-report]
  (let [execution' (execution-assembly operation-acquisition-assembly)
        route' (route-assembly operation-acquisition-assembly)
        operation' (operation-assembly operation-acquisition-assembly)
        acquisition' (acquisition-assembly operation-acquisition-assembly)
        routes-by-operation (route-realizations-by-operation route')
        execution-capabilities (:execution-capabilities execution')
        settlement-contracts (:settlement-contracts execution')
        publication
        (get-in operation-acquisition-report
                [:analysis :published-change-topics-by-operation])
        affected-scopes
        (get-in operation-acquisition-report
                [:analysis :affected-scopes-by-operation])
        affected-fragments
        (get-in operation-acquisition-report
                [:analysis :affected-fragments-by-operation])]
    (into
     (sorted-map)
     (map
      (fn [operation]
        (let [realization (get-in operation' [:operations operation])
              route-requirement
              (get-in operation' [:route-requirements operation])]
          [operation
           {:choreography-name (:choreography-name realization)
            :browser-role (:browser-role realization)
            :authority-role (:authority-role realization)
            :browser-plan-key (:plan-key realization)
            :browser-plan-digest (:browser-plan-digest realization)
            :authority-plan-digest (:authority-plan-digest realization)
            :trusted-operation-name
            (get-in execution' [:server :operations operation :name])
            :command-transport (:transport route-requirement)
            :routes (vec (sort-by (juxt :path :route-id)
                                  (get routes-by-operation operation [])))
            :execution-capabilities (get execution-capabilities operation)
            :settlement-contract (get settlement-contracts operation)
            :published-change-topics (get publication operation)
            :affected-scopes (get affected-scopes operation #{})
            :affected-fragments (get affected-fragments operation #{})
            :authoritative-acquisitions
            (into
             (sorted-map)
             (keep
              (fn [fragment]
                (when-let [acquisition-realization
                           (get-in acquisition' [:realizations fragment])]
                  [fragment
                   {:scope (:scope acquisition-realization)
                    :fragment-route
                    (select-keys (:fragment-route acquisition-realization)
                                 [:method :path :boundary])
                    :stream-route
                    (select-keys (:stream-route acquisition-realization)
                                 [:method :path :boundary])}]))
              (sort-by pr-str (get affected-fragments operation #{}))))}]))
      (sort-by pr-str (keys (:operations operation')))))))

(defn- path-only
  [path]
  (first (str/split path #"[?#]" 2)))

(defn- route-template-segments
  [path]
  (str/split (path-only path) #"/" -1))

(defn- route-placeholder-segment?
  [segment]
  (and
   (str/starts-with? segment ":")
   (> (count segment) 1)))

(defn- route-template-matches?
  "Match one concrete rendered affordance path against the small physical route
   template vocabulary already used by current Biff/Reitit applications.

   A full `:name` path segment is treated as one non-empty concrete segment;
   every other segment is literal. Query/fragment suffixes on the rendered URL
   are ignored for route selection. Gesso does not infer optional segments,
   wildcards, regexes, or application-specific coercion here."
  [template concrete]
  (let [template-segments (route-template-segments template)
        concrete-segments (route-template-segments concrete)]
    (and
     (= (count template-segments) (count concrete-segments))
     (every?
      true?
      (map
       (fn [template-segment concrete-segment]
         (if (route-placeholder-segment? template-segment)
           (not (str/blank? concrete-segment))
           (= template-segment concrete-segment)))
       template-segments
       concrete-segments)))))

(defn- rendered-post-coordinates
  "Enumerate every concrete hx-post coordinate in one rendered Hiccup/value.

   This deliberately scans more broadly than ui/rendered-choreo-affordances.
   Ordinary HTMX remains legal, but application preflight needs visibility of
   its physical POST coordinates so an already-assembled semantic command route
   cannot silently be invoked after application code drops :choreo/op metadata.

   :choreo-operation-declared? records only the framework-owned metadata installed
   by canonical gesso.live.ui/post-button :choreo/op rendering. It is not browser
   authority and is consumed before HTML serialization."
  [rendered]
  (letfn [(walk [value render-path]
            (lazy-seq
             (concat
              (when (and (vector? value)
                         (map? (second value))
                         (contains? (second value) :hx-post))
                (let [path (:hx-post (second value))]
                  (when (and (string? path)
                             (not (str/blank? path)))
                    [{:method :post
                      :path path
                      :render-path render-path
                      :choreo-operation-declared?
                      (contains?
                       (meta value)
                       ui/choreo-affordance-metadata-key)}])))
              (when (sequential? value)
                (mapcat
                 (fn [[index child]]
                   (walk child (conj render-path index)))
                 (map-indexed vector value))))))]
    (vec (walk rendered []))))

(defn- scan-rendered-surfaces
  [rendered-surfaces]
  (if (nil? rendered-surfaces)
    {:affordances []
     :post-coordinates []
     :errors []}
    (reduce
     (fn [{:keys [affordances post-coordinates errors]} surface-name]
       (let [rendered
             (get rendered-surfaces surface-name)

             surface-post-coordinates
             (into []
                   (map #(assoc % :surface surface-name))
                   (rendered-post-coordinates rendered))]
         (try
           {:affordances
            (into affordances
                  (map #(assoc % :surface surface-name))
                  (ui/rendered-choreo-affordances rendered))
            :post-coordinates
            (into post-coordinates surface-post-coordinates)
            :errors errors}
           (catch clojure.lang.ExceptionInfo error
             {:affordances affordances
              :post-coordinates
              (into post-coordinates surface-post-coordinates)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-scan-failed
                "Application preflight could not enumerate canonical Choreo affordances from a supplied rendered surface."
                {:surface surface-name
                 :cause-type (:error/type (ex-data error))
                 :cause-kind (:error/kind (ex-data error))
                 :cause-data (dissoc (ex-data error) :error/type :error/kind)}))})
           (catch Throwable error
             {:affordances affordances
              :post-coordinates
              (into post-coordinates surface-post-coordinates)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-scan-failed
                "Application preflight failed while enumerating a supplied rendered surface."
                {:surface surface-name
                 :exception-class (str (class error))
                 :exception-message (.getMessage error)}))}))))
     {:affordances []
      :post-coordinates []
      :errors []}
     (sort-by pr-str (keys rendered-surfaces)))))

(defn- semantic-route-matches
  [operation-summary' {:keys [method path]}]
  (vec
   (for [[operation operation-entry] operation-summary'
         route (:routes operation-entry)
         :when (and (= method (:method route))
                    (route-template-matches? (:path route) path))]
     {:operation operation
      :route-id (:route-id route)
      :method (:method route)
      :route-template (:path route)
      :required-transport (:required-transport route)})))

(defn- semantic-route-identity-errors
  "Reject anonymous physical POSTs that collide with assembled semantic routes.

   Ordinary HTMX is still allowed for routes outside the semantic operation
   slice. Once a route is assembled as a realization of a semantic operation,
   however, every rendered invocation of that route must retain canonical
   :choreo/op identity. This prevents application code from silently degrading a
   semantic operation to an ordinary HTMX request when a per-render binding is
   unavailable."
  [operation-summary' post-coordinates]
  (vec
   (keep
    (fn [{:keys [surface render-path method path
                 choreo-operation-declared?]
          :as coordinate}]
      (when-not choreo-operation-declared?
        (let [matches
              (semantic-route-matches
               operation-summary'
               coordinate)]
          (when (seq matches)
            (issue
             :rendered-semantic-route-without-choreo-operation
             "Rendered HTMX POST targets an assembled semantic operation route but carries no canonical :choreo/op declaration. Semantic operation identity may not silently degrade to ordinary HTMX."
             {:surface surface
              :render-path render-path
              :method method
              :path path
              :candidate-operations
              (set (map :operation matches))
              :matching-routes matches})))))
    post-coordinates)))

(defn- enrich-render-scan-errors-with-semantic-routes
  "Attach physical semantic-route context to rendered-affordance scanner errors.

   Malformed or forged framework metadata already fails closed in
   ui/rendered-choreo-affordances, but the scanner-local cause alone can obscure
   an important application fact: the malformed node may also target a route
   already assembled as a semantic operation.  Preserve the scanner's original
   error kind/cause while attaching every matching semantic route on that same
   rendered surface.

   This is diagnostic enrichment only.  It does not reinterpret malformed
   metadata as an anonymous affordance and therefore does not duplicate or
   weaken semantic-route-identity-errors."
  [operation-summary' post-coordinates scan-errors]
  (mapv
   (fn [scan-error]
     (let [surface (:surface scan-error)
           collisions
           (->> post-coordinates
                (filter #(= surface (:surface %)))
                (mapcat
                 (fn [{:keys [method path render-path] :as coordinate}]
                   (map
                    (fn [match]
                      (merge
                       {:surface surface
                        :render-path render-path
                        :rendered-method method
                        :rendered-path path}
                       match))
                    (semantic-route-matches operation-summary' coordinate))))
                (sort-by (juxt :render-path :rendered-path :operation :route-id))
                vec)]
       (cond-> scan-error
         (seq collisions)
         (assoc :matching-semantic-routes collisions))))
   scan-errors))

(defn- resolve-rendered-affordances
  [operation-summary' affordances]
  (reduce
   (fn [{:keys [affordances errors]} affordance]
     (let [operation (:operation affordance)
           operation-entry (get operation-summary' operation)]
       (cond
         (nil? operation-entry)
         {:affordances (conj affordances affordance)
          :errors
          (conj
           errors
           (issue
            :rendered-affordance-unknown-operation
            "Rendered Choreo affordance references an operation outside the assembled route-exposed application slice."
            {:surface (:surface affordance)
             :render-path (:render-path affordance)
             :operation operation
             :available-operations (set (keys operation-summary'))}))}

         (not= (:plan-key affordance)
               (:browser-plan-key operation-entry))
         {:affordances (conj affordances affordance)
          :errors
          (conj
           errors
           (issue
            :rendered-affordance-plan-mismatch
            "Rendered Choreo affordance plan key does not match the assembled browser plan for its semantic operation."
            {:surface (:surface affordance)
             :render-path (:render-path affordance)
             :operation operation
             :affordance-plan-key (:plan-key affordance)
             :assembled-plan-key (:browser-plan-key operation-entry)}))}

         :else
         (let [declared-routes (:routes operation-entry)
               method-routes
               (filterv #(= (:method affordance) (:method %)) declared-routes)
               matching-routes
               (filterv
                #(route-template-matches? (:path %) (:path affordance))
                method-routes)]
           (cond
             (empty? method-routes)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-method-mismatch
                "Rendered Choreo affordance HTTP method is not realized by a trusted route for its semantic operation."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :declared-routes declared-routes}))}

             (empty? matching-routes)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :rendered-affordance-path-mismatch
                "Rendered Choreo affordance URL does not match a trusted route template for its semantic operation."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :path (:path affordance)
                 :route-templates (mapv :path method-routes)}))}

             (> (count matching-routes) 1)
             {:affordances (conj affordances affordance)
              :errors
              (conj
               errors
               (issue
                :ambiguous-rendered-affordance-route
                "Rendered Choreo affordance matches more than one trusted route realization."
                {:surface (:surface affordance)
                 :render-path (:render-path affordance)
                 :operation operation
                 :method (:method affordance)
                 :path (:path affordance)
                 :matching-routes matching-routes}))}

             :else
             (let [route (first matching-routes)]
               {:affordances
                (conj
                 affordances
                 (assoc affordance
                        :route-id (:route-id route)
                        :route-template (:path route)
                        :required-transport (:required-transport route)))
                :errors errors}))))))
   {:affordances []
    :errors []}
   affordances))

(defn- affordances-by-operation
  [affordances]
  (reduce
   (fn [acc affordance]
     (update acc (:operation affordance) (fnil conj []) affordance))
   (sorted-map)
   affordances))

(defn- enrich-operation-summary-with-affordances
  [operation-summary' affordances]
  (let [by-operation (affordances-by-operation affordances)]
    (into
     (sorted-map)
     (map
      (fn [[operation summary]]
        [operation
         (assoc summary
                :rendered-affordances
                (vec (get by-operation operation [])))])
      operation-summary'))))

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
  (if-let [report
           (current-operation-acquisition-report
            operation-acquisition-assembly)]
    (operation-summary-from-report
     operation-acquisition-assembly
     report)
    (throw
     (preflight-error
      :invalid-operation-acquisition-assembly
      "Application operation summary requires a current OperationAcquisitionAssembly."
      {:operation-acquisition-assembly operation-acquisition-assembly}))))

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

    (current-operation-acquisition-report
     application-or-operation-acquisition)
    [affordance-open-obligation]

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
       ordinary hx-post is also compared with the assembled semantic route set; an
       anonymous POST may not target a semantic operation route after application
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
        (current-operation-acquisition-report
         operation-acquisition-assembly)

        operation-acquisition-valid?
        (some? operation-acquisition-report)

        manifest
        (when operation-acquisition-valid?
          (browser-assembly operation-acquisition-assembly))

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
          (operation-summary-from-report
           operation-acquisition-assembly
           operation-acquisition-report)
          (sorted-map))

        surface-scan
        (scan-rendered-surfaces rendered-surfaces)

        semantic-route-errors
        (semantic-route-identity-errors
         base-operation-summary
         (:post-coordinates surface-scan))

        surface-scan-errors
        (enrich-render-scan-errors-with-semantic-routes
         base-operation-summary
         (:post-coordinates surface-scan)
         (:errors surface-scan))

        affordance-resolution
        (resolve-rendered-affordances
         base-operation-summary
         (:affordances surface-scan))

        affordances
        (:affordances affordance-resolution)

        operation-summary'
        (if (some? rendered-surfaces)
          (enrich-operation-summary-with-affordances
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

          (seq surface-scan-errors)
          (into surface-scan-errors)

          (seq semantic-route-errors)
          (into semantic-route-errors)

          (seq (:errors affordance-resolution))
          (into (:errors affordance-resolution)))

        unhandled-topics
        (if operation-acquisition-valid?
          (get-in operation-acquisition-report
                  [:analysis
                   :unhandled-published-change-topics-by-operation])
          {})

        supplied-surfaces?
        (some? rendered-surfaces)

        warnings
        (cond->
         [(if supplied-surfaces?
            (issue
             :rendered-surface-enumeration-completeness-not-yet-modeled
             "Every canonical Choreo affordance in the supplied rendered surfaces is checked. Static snapshot completeness is intentionally not treated as proof; canonical Gesso/Biff startup can validate each actual render dynamically, while deployment adoption remains outside ApplicationAssembly."
             {:edge :application->rendered-surfaces
              :surface-names (set (keys rendered-surfaces))})
            (issue
             :static-affordance-closure-not-yet-modeled
             "No rendered-surface snapshot was supplied. Canonical Gesso/Biff startup can still validate each actual canonical HTML render dynamically; ApplicationAssembly alone does not prove that deployment used that checked boundary."
             {:edge :rendered-affordance->semantic-operation}))]
          (seq unhandled-topics)
          (conj
           (issue
            :published-change-topics-not-consumed-by-live-app
            "Some trusted semantic publication topics are not consumed by this compiled Live application."
            {:topics-by-operation unhandled-topics})))

        obligations
        [(if supplied-surfaces?
           rendered-surface-completeness-open-obligation
           affordance-open-obligation)]

        affordance-closure
        (if supplied-surfaces?
          {:status
           (if (or (seq (:errors surface-scan))
                   (seq semantic-route-errors)
                   (seq (:errors affordance-resolution)))
             :failed
             :closed-relative-to-supplied-rendered-surfaces)
           :guarantee
           (when (and (empty? (:errors surface-scan))
                      (empty? semantic-route-errors)
                      (empty? (:errors affordance-resolution)))
             rendered-affordance-guarantee)
           :surface-count (count rendered-surfaces)
           :affordance-count (count affordances)}
          {:status :not-modeled
           :guarantee nil
           :surface-count 0
           :affordance-count 0})]
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
      :trusted-assumptions trusted-assumptions}}))

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
   resolve exactly and no anonymous ordinary hx-post collides with an assembled
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

        surface-scan
        (scan-rendered-surfaces {surface rendered})

        semantic-route-errors
        (semantic-route-identity-errors
         base-operation-summary
         (:post-coordinates surface-scan))

        surface-scan-errors
        (enrich-render-scan-errors-with-semantic-routes
         base-operation-summary
         (:post-coordinates surface-scan)
         (:errors surface-scan))

        affordance-resolution
        (resolve-rendered-affordances
         base-operation-summary
         (:affordances surface-scan))

        errors
        (cond-> []
          (not application-valid?)
          (conj
           (issue
            :invalid-application-assembly
            "Rendered-surface validation requires a current ApplicationAssembly."
            {:application-assembly application-assembly
             :surface surface}))

          (seq surface-scan-errors)
          (into surface-scan-errors)

          (seq semantic-route-errors)
          (into semantic-route-errors)

          (seq (:errors affordance-resolution))
          (into (:errors affordance-resolution)))

        affordances
        (vec (:affordances affordance-resolution))]
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
  (when-not (application-assembly? application-assembly)
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

(defn application-handler-component
  "Return a Biff 2-style system component that installs application preflight
   on the canonical :biff.ring/handler.

   The returned component is an ordinary one-argument system-map transformer,
   so an application can place it directly in its Biff component vector before
   the HTTP server component:

     [(application-handler-component application-assembly)
      use-server]

   At component execution time Gesso requires a system map containing an actual
   function (or Var currently containing one) at :biff.ring/handler and replaces
   only that value with wrap-application-handler. All other system entries are
   preserved exactly. No Gesso-specific system marker is added, avoiding a second
   handler registry and avoiding Biff schema/registry coupling.

   application-assembly is checked both when this component is constructed and
   again when the wrapped handler is installed. The installed handler rechecks
   the assembly for every canonical HTML response, so later artifact/receipt
   staleness still fails before browser serialization.

   This establishes the canonical Biff handler integration when the returned
   component is present in the application's component sequence. It does not
   claim that a server which ignores :biff.ring/handler, a startup path omitting
   the component, or a manually constructed pre-serialized HTML Ring response is
   covered by this boundary."
  [application-assembly]
  (when-not (application-assembly? application-assembly)
    (throw
     (preflight-error
      :invalid-application-handler-component-assembly
      "application-handler-component requires a current ApplicationAssembly."
      {:application-assembly application-assembly})))
  (fn [system]
    (when-not (map? system)
      (throw
       (preflight-error
        :invalid-application-handler-system
        "Gesso application handler component requires a Biff-style system map."
        {:system system})))
    (when-not (contains? system biff-ring-handler-key)
      (throw
       (preflight-error
        :missing-biff-ring-handler
        "Gesso application handler component requires :biff.ring/handler in the system map."
        {:system-keys (set (keys system))})))
    (let [handler (get system biff-ring-handler-key)]
      (when-not (actual-function? handler)
        (throw
         (preflight-error
          :invalid-biff-ring-handler
          "Gesso application handler component requires :biff.ring/handler to be an actual function or a Var currently containing one."
          {:handler handler})))
      (assoc
       system
       biff-ring-handler-key
       (wrap-application-handler
        application-assembly
        handler)))))

(defn start-biff-application!
  "Start a Biff 2 application through the canonical Gesso application-preflight
   boundary.

   This has the same two arities as biff.core/start, with a current
   ApplicationAssembly prepended:

     (start-biff-application!
      application-assembly
      modules-var
      components)

     (start-biff-application!
      application-assembly
      initial-system
      modules-var
      components)

   Gesso automatically prepends application-handler-component to the supplied
   ordinary Biff component sequence before delegating to biff.core/start. Because
   Biff merges module initialization and initial-system before reducing components,
   the canonical :biff.ring/handler must already exist at that point. A normal
   server component therefore cannot accidentally run before Gesso installs the
   checked wrapper simply because the application author forgot or misplaced the
   preflight component.

   The supplied component sequence is otherwise preserved exactly and in order.
   This function does not claim to constrain direct calls to biff.core/start, later
   components that deliberately replace :biff.ring/handler, servers that ignore
   that system key, or manually constructed pre-serialized HTML Ring responses.
   Those remain explicit escape hatches outside the canonical Gesso/Biff path."
  ([application-assembly modules-var components]
   (start-biff-application!
    application-assembly
    {}
    modules-var
    components))
  ([application-assembly initial-system modules-var components]
   (when-not (application-assembly? application-assembly)
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
   (when-not (sequential? components)
     (throw
      (preflight-error
       :invalid-biff-application-components
       "start-biff-application! requires a sequential collection of Biff components."
       {:components components})))
   (biff/start
    initial-system
    modules-var
    (into
     [(application-handler-component application-assembly)]
     components))))

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
   (operation-acquisition-assembly-shape?
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
    (let [summary (get-in report [:analysis :operations])]
      (when-not (contains? summary operation)
        (throw
         (preflight-error
          :unknown-operation
          "Application operation explanation references an operation outside the assembled route-exposed application slice."
          {:operation operation
           :available-operations (set (keys summary))})))
      (assoc
       (get summary operation)
       :operation operation
       :guarantee backbone-guarantee
       :affordance-guarantee
       (get-in report [:analysis :affordance-closure :guarantee])
       :canonical-html-enforcement canonical-html-enforcement))
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

   One fresh application report is reused throughout the explanation."
  [value]
  (cond
    (application-assembly-shape? value)
    (if-let [report (current-application-report value)]
      {:type application-assembly-type
       :version preflight-version
       :name (:name value)
       :guarantee backbone-guarantee
       :operations (get-in report [:analysis :operations])
       :browser-artifact
       {:path (:browser-artifact-path value)
        :receipt-path
        (or (:browser-receipt-path value)
            (browser-build/receipt-path (:browser-artifact-path value)))
        :receipt (get-in report [:analysis :artifact-receipt])}
       :live-app-name
       (get-in value
               [:operation-acquisition-assembly
                :acquisition-assembly
                :live-app
                :name])
       :rendered-surface-names
       (set (keys (or (get-in report [:analysis :rendered-surfaces]) {})))
       :affordances (get-in report [:analysis :affordances])
       :affordance-closure (get-in report [:analysis :affordance-closure])
       :open-obligations (get-in report [:analysis :open-obligations])
       :trusted-assumptions (get-in report [:analysis :trusted-assumptions])
       :canonical-html-enforcement canonical-html-enforcement
       :explicit-escape-hatches explicit-escape-hatches
       :warnings (:warnings report)}
      (throw
       (preflight-error
        :unrecognized-value
        "Expected an application preflight report or current ApplicationAssembly."
        {:value value})))

    (report? value)
    {:type report-type
     :version preflight-version
     :valid? (:valid? value)
     :errors (:errors value)
     :warnings (:warnings value)
     :canonical-html-enforcement canonical-html-enforcement
     :explicit-escape-hatches explicit-escape-hatches
     :analysis
     (select-keys
      (:analysis value)
      [:name
       :browser-artifact-path
       :browser-receipt-path
       :operations
       :rendered-surfaces
       :affordances
       :affordance-closure
       :open-obligations
       :trusted-assumptions])}

    :else
    (throw
     (preflight-error
      :unrecognized-value
      "Expected an application preflight report or current ApplicationAssembly."
      {:value value}))))

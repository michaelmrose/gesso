(ns gesso.live.application-preflight.assurance
  "Pure explanation and assurance vocabulary for whole-application preflight.

   This internal module describes what the application checker established,
   what still depends on trusted external facts, and which canonical Biff/HTML
   boundaries must be used for the reported guarantee to apply. All exported
   rendering functions require the *current report* that the application
   coordinator has already checked; they neither recognize ApplicationAssembly
   objects nor certify artifact bytes, routes, or browser behavior.

   No additional application registry, authority, or proof claim is created."
  (:require
   [gesso.live.application-preflight.biff :as biff-lifecycle]
   [gesso.live.browser.build :as browser-build]))

;; Keep the public-facing vocabulary source-derived and identical to that used
;; by the application coordinator. These are descriptions, not certificates.

(def backbone-guarantee
  :application-runtime-backbone-preflight-closed)

(def rendered-affordance-guarantee
  :supplied-rendered-affordances-preflight-closed)

(def rendered-surface-guarantee
  :rendered-surface-pre-browser-preflight-closed)

(def application-assembly-type
  :gesso.live.application-preflight/application-assembly)

(def report-type
  :gesso.live.application-preflight/report)

(def preflight-version 3)

(def affordance-open-obligation
  {:kind :static-affordance-closure-not-yet-modeled
   :edge :rendered-affordance->semantic-operation
   :status :open
   :message
   "No rendered-surface snapshot was supplied to this ApplicationAssembly, so it contains no static affordance sample. Canonical Gesso/Biff startup can still validate every actual canonical HTML render before serialization. What ApplicationAssembly alone cannot establish is deployment adoption of that checked startup/response boundary or absence of deliberate raw-response escape hatches."})

(def rendered-surface-completeness-open-obligation
  {:kind :rendered-surface-enumeration-completeness-not-yet-modeled
   :edge :application->rendered-surfaces
   :status :open
   :message
   "All canonical affordances in the supplied render snapshot are closed to the assembled application backbone. Arbitrary Clojure render state is not statically enumerable, so snapshot completeness is not treated as proof. Canonical Gesso/Biff startup validates each actual canonical HTML response dynamically; ApplicationAssembly alone still cannot prove that deployment used that boundary or avoided deliberate raw-response/handler-replacement escape hatches."})

(def canonical-html-enforcement
  {:status :available-on-canonical-gesso-biff-startup
   :startup-boundary :gesso.live.application-preflight/start-biff-application!
   :biff-module-id biff-lifecycle/application-handler-module-id
   :biff-handler-key biff-lifecycle/biff-ring-handler-key
   :handler-boundary :gesso.live.application-preflight/wrap-application-handler
   :response-boundary :gesso.http/html-response
   :per-render-guarantee rendered-surface-guarantee
   :installation-proof :not-carried-by-application-assembly})

(def explicit-escape-hatches
  [{:kind :direct-biff-start-bypass
    :boundary :application-startup
    :description
    "Calling Biff startup directly instead of start-biff-application! can omit or misorder the canonical Gesso application-preflight module."}
   {:kind :later-handler-replacement
    :boundary :biff-module-start-order
    :description
    "A later Biff module or arbitrary Clojure can deliberately replace :biff.ring/handler after Gesso installed the checked handler."}
   {:kind :server-ignores-biff-ring-handler
    :boundary :server-integration
    :description
    "A server that does not consume the canonical :biff.ring/handler system key is outside this integration guarantee."}
   {:kind :pre-serialized-html-ring-response
    :boundary :html-serialization
    :description
    "Hand-built Ring responses containing already-serialized HTML bypass gesso.http/html-response and therefore bypass canonical Hiccup affordance validation."}])

(def trusted-assumptions
  #{:application-publication-declarations-match-model-publication
    :trusted-command-route-declarations-match-installed-handlers
    :trusted-acquisition-route-declarations-match-installed-handlers
    :browser-artifact-emitted-through-gesso-owned-application-build
    :clojurescript-compiler-realizes-generated-entrypoint
    :authenticated-principal-binding-is-correct
    :public-model-operation-contracts-hold
    :xtdb-transaction-and-consistency-contracts-hold})

(defn- preflight-error
  [kind message data]
  (ex-info message
           (merge {:error/type :gesso.live.application-preflight/error
                   :error/kind kind}
                  data)))

(defn- issue
  [kind message data]
  (merge {:kind kind :message message} data))

(defn open-obligations-for-surfaces
  "Return the appropriate admitted-coverage caveat for a static surface sample.
   A missing sample differs from a supplied sample; neither establishes that
   deployed code adopted the canonical pre-render boundary."
  [rendered-surfaces]
  [(if (some? rendered-surfaces)
     rendered-surface-completeness-open-obligation
     affordance-open-obligation)])

(defn warnings
  "Build the existing application-level warning vector from source-derived
   surface coverage and unhandled publication topics."
  [rendered-surfaces unhandled-topics]
  (let [supplied-surfaces? (some? rendered-surfaces)]
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
        {:topics-by-operation unhandled-topics})))))

(defn affordance-closure
  "Describe correspondence achieved relative to optional rendered snapshots.
   Error vectors originate with the shared rendered-surface analyzer; this
   function does not re-run the scanner or establish snapshot completeness."
  [rendered-surfaces surface-scan semantic-route-errors surface-analysis affordances]
  (if (some? rendered-surfaces)
    {:status
     (if (or (seq (:errors surface-scan))
             (seq semantic-route-errors)
             (seq (:resolution-errors surface-analysis)))
       :failed
       :closed-relative-to-supplied-rendered-surfaces)
     :guarantee
     (when (and (empty? (:errors surface-scan))
                (empty? semantic-route-errors)
                (empty? (:resolution-errors surface-analysis)))
       rendered-affordance-guarantee)
     :surface-count (count rendered-surfaces)
     :affordance-count (count affordances)}
    {:status :not-modeled
     :guarantee nil
     :surface-count 0
     :affordance-count 0}))

(defn explain-operation-from-report
  "Explain a single operation from a *current* application report provided by
   the caller. The caller must establish currentness; this does not do so."
  [report operation]
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
     :canonical-html-enforcement canonical-html-enforcement)))

(defn explain-application-from-report
  "Render an application explanation from an assembly and its freshly checked
   successful report. The coordinator owns freshness and application authority."
  [application-assembly report]
  {:type application-assembly-type
   :version preflight-version
   :name (:name application-assembly)
   :guarantee backbone-guarantee
   :operations (get-in report [:analysis :operations])
   :browser-artifact
   {:path (:browser-artifact-path application-assembly)
    :receipt-path
    (or (:browser-receipt-path application-assembly)
        (browser-build/receipt-path (:browser-artifact-path application-assembly)))
    :receipt (get-in report [:analysis :artifact-receipt])}
   :live-app-name
   (get-in application-assembly
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
   :warnings (:warnings report)})

(defn explain-report-from-report
  "Render a recognized application report's public explanation without
   exposing the nested source-product reference carried for revalidation."
  [report]
  {:type report-type
   :version preflight-version
   :valid? (:valid? report)
   :errors (:errors report)
   :warnings (:warnings report)
   :canonical-html-enforcement canonical-html-enforcement
   :explicit-escape-hatches explicit-escape-hatches
   :analysis
   (select-keys
    (:analysis report)
    [:name
     :browser-artifact-path
     :browser-receipt-path
     :operations
     :rendered-surfaces
     :affordances
     :affordance-closure
     :open-obligations
     :trusted-assumptions])})

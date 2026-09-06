(ns gesso.live.browser.artifact
  "Closed correspondence data for generated Gesso Live browser artifacts.

   Browser assembly preflight proves that one current BrowserAssemblyManifest is
   internally coherent. A generated JavaScript artifact introduces another
   boundary: the file on disk may have been produced from an older manifest than
   the application currently expects.

   This namespace makes that boundary explicit without pretending to inspect or
   execute JavaScript. A build derives one BrowserArtifactStamp from the exact
   BrowserAssemblyManifest it embeds. Later build/start tooling derives the
   current stamp from the application's current manifest and requires exact
   correspondence with the stamp carried by the generated artifact or its
   sibling metadata.

   The stamp deliberately contains the exact executable-plan digest registry as
   well as the browser assembly facts that can change runtime meaning: physical
   role, runtime-visible plan set, feature closure, optimistic command transport,
   and bootstrap entrypoint. A stale plan, removed operation, changed role,
   changed transport, or changed bootstrap contract therefore becomes explicit
   artifact drift rather than a browser-time surprise.

   This namespace is pure. It does not decide how a build serializes the stamp or
   where generated artifact metadata lives; build/bootstrap tooling owns that
   physical packaging policy."
  (:require
   [clojure.set :as set]
   [gesso.choreo.preflight :as choreo-preflight]
   [gesso.live.browser.preflight :as browser-preflight]))

;; -----------------------------------------------------------------------------
;; Identity / closed vocabulary
;; -----------------------------------------------------------------------------

(def artifact-version
  1)

(def stamp-type
  :gesso.live.browser.artifact/stamp)

(def correspondence-report-type
  :gesso.live.browser.artifact/correspondence-report)

(def ^:private stamp-keys
  #{:gesso.live.browser.artifact/type
    :gesso.live.browser.artifact/version
    :assembly-manifest-version
    :plan-registry-version
    :assembly-name
    :plan-registry-name
    :plan-digests
    :browser-role
    :required-plan-keys
    :features
    :optimistic-command-transport
    :bootstrap-entrypoint})

(def ^:private report-keys
  #{:gesso.live.browser.artifact/type
    :gesso.live.browser.artifact/version
    :valid?
    :errors
    :warnings
    :analysis})

(def ^:private analysis-keys
  #{:current-stamp
    :generated-stamp})

(def ^:private supported-features
  #{:optimistic
    :optimistic-htmx})

(def ^:private supported-command-transports
  browser-preflight/command-transports)

;; -----------------------------------------------------------------------------
;; Errors / shape helpers
;; -----------------------------------------------------------------------------

(defn- artifact-error
  [kind message data]
  (ex-info
   message
   (merge
    {:error/type :gesso.live.browser.artifact/error
     :error/kind kind}
    data)))

(defn- issue
  [kind message data]
  (merge
   {:kind kind
    :message message}
   data))

(defn- keyword-set?
  [value]
  (and
   (set? value)
   (every? keyword? value)))

(defn- sha-256-hex?
  [value]
  (and
   (string? value)
   (boolean
    (re-matches #"[0-9a-f]{64}" value))))

(defn- digest-map?
  [value]
  (and
   (map? value)
   (every?
    (fn [[plan-key digest]]
      (and
       (keyword? plan-key)
       (sha-256-hex? digest)))
    value)))

(defn- closed-map?
  [expected-keys value]
  (and
   (map? value)
   (= expected-keys
      (set (keys value)))))

(defn- feature-transport-coherent?
  [features transport]
  (let [optimistic?
        (contains? features :optimistic)

        optimistic-htmx?
        (contains? features :optimistic-htmx)]
    (and
     (or (not optimistic-htmx?)
         optimistic?)
     (= optimistic?
        (not= :none transport))
     (= optimistic-htmx?
        (= :htmx transport)))))

;; -----------------------------------------------------------------------------
;; Stamp construction / recognition
;; -----------------------------------------------------------------------------

(defn stamp?
  "True when value is one closed current BrowserArtifactStamp.

   A stamp is intentionally only correspondence metadata. Its plan digests are
   trusted only after comparing it with a current, independently preflighted
   BrowserAssemblyManifest through check-correspondence or
   require-correspondence!."
  [value]
  (and
   (closed-map? stamp-keys value)
   (= stamp-type
      (:gesso.live.browser.artifact/type value))
   (= artifact-version
      (:gesso.live.browser.artifact/version value))
   (= browser-preflight/preflight-version
      (:assembly-manifest-version value))
   (= choreo-preflight/preflight-version
      (:plan-registry-version value))
   (or (nil? (:assembly-name value))
       (keyword? (:assembly-name value)))
   (or (nil? (:plan-registry-name value))
       (keyword? (:plan-registry-name value)))
   (digest-map?
    (:plan-digests value))
   (keyword?
    (:browser-role value))
   (keyword-set?
    (:required-plan-keys value))
   (= (set (keys (:plan-digests value)))
      (:required-plan-keys value))
   (set?
    (:features value))
   (set/subset?
    (:features value)
    supported-features)
   (contains?
    supported-command-transports
    (:optimistic-command-transport value))
   (feature-transport-coherent?
    (:features value)
    (:optimistic-command-transport value))
   (= browser-preflight/canonical-bootstrap-entrypoint
      (:bootstrap-entrypoint value))))

(defn stamp
  "Derive one closed BrowserArtifactStamp from an exact current
   BrowserAssemblyManifest.

   This is the value a browser build should package with the JavaScript artifact
   it produced. The input manifest is revalidated here rather than accepted by
   shape so stale/tampered plan digests cannot be blessed into a stamp."
  [manifest]
  (when-not
   (browser-preflight/assembly-manifest? manifest)
    (throw
     (artifact-error
      :invalid-browser-assembly-manifest
      "Browser artifact stamping requires a current untampered BrowserAssemblyManifest."
      {:manifest manifest})))

  (let [plan-registry
        (:plan-registry manifest)

        value
        {:gesso.live.browser.artifact/type
         stamp-type

         :gesso.live.browser.artifact/version
         artifact-version

         :assembly-manifest-version
         (:gesso.live.browser/version manifest)

         :plan-registry-version
         (:gesso.choreo/version plan-registry)

         :assembly-name
         (:name manifest)

         :plan-registry-name
         (:name plan-registry)

         :plan-digests
         (:digests plan-registry)

         :browser-role
         (:browser-role manifest)

         :required-plan-keys
         (:required-plan-keys manifest)

         :features
         (:features manifest)

         :optimistic-command-transport
         (:optimistic-command-transport manifest)

         :bootstrap-entrypoint
         (get-in manifest
                 [:bootstrap :entrypoint])}]
    (when-not
     (stamp? value)
      (throw
       (artifact-error
        :invalid-generated-artifact-stamp
        "Gesso derived an invalid BrowserArtifactStamp from a valid BrowserAssemblyManifest."
        {:manifest manifest
         :stamp value})))
    value))

;; -----------------------------------------------------------------------------
;; Correspondence analysis
;; -----------------------------------------------------------------------------

(defn- field-mismatch
  [field current generated]
  (when-not (= current generated)
    (issue
     :browser-artifact-field-mismatch
     "Generated browser artifact metadata does not match the current browser assembly."
     {:field field
      :current current
      :generated generated})))

(defn- digest-errors
  [current generated]
  (let [current-keys
        (set (keys current))

        generated-keys
        (set (keys generated))

        missing-generated
        (set/difference
         current-keys
         generated-keys)

        stale-generated
        (set/difference
         generated-keys
         current-keys)

        shared
        (set/intersection
         current-keys
         generated-keys)

        mismatches
        (->> shared
             (keep
              (fn [plan-key]
                (let [current-digest
                      (get current plan-key)

                      generated-digest
                      (get generated plan-key)]
                  (when-not
                   (= current-digest generated-digest)
                    {:plan-key plan-key
                     :current-digest current-digest
                     :generated-digest generated-digest}))))
             (sort-by
              (comp pr-str :plan-key))
             vec)]
    (cond-> []
      (seq missing-generated)
      (conj
       (issue
        :generated-artifact-missing-plan-digests
        "Generated browser artifact is missing digests for current executable plans."
        {:missing-plan-keys missing-generated}))

      (seq stale-generated)
      (conj
       (issue
        :generated-artifact-stale-plan-digests
        "Generated browser artifact carries digests for executable plans that are no longer current."
        {:stale-plan-keys stale-generated}))

      (seq mismatches)
      (conj
       (issue
        :generated-artifact-plan-digest-mismatch
        "Generated browser artifact was built from different executable plan content."
        {:mismatches mismatches})))))

(defn check-correspondence
  "Compare one generated BrowserArtifactStamp with the application's current
   BrowserAssemblyManifest and return a closed report.

   The current manifest must independently pass browser assembly preflight.
   Malformed generated metadata is reported as artifact corruption/drift rather
   than accepted as an alternate stamp format.

   Exact agreement is required for plan digests and every browser assembly fact
   represented by the stamp. This is deliberately stronger than checking only a
   filename or timestamp: semantically stale generated JavaScript must fail even
   when the physical artifact exists and is non-empty."
  [current-manifest generated-stamp]
  (when-not
   (browser-preflight/assembly-manifest? current-manifest)
    (throw
     (artifact-error
      :invalid-current-browser-assembly-manifest
      "Browser artifact correspondence requires a current untampered BrowserAssemblyManifest."
      {:manifest current-manifest})))

  (let [current-stamp
        (stamp current-manifest)

        generated-valid?
        (stamp? generated-stamp)

        errors
        (if-not generated-valid?
          [(issue
            :invalid-generated-artifact-stamp
            "Generated browser artifact does not carry a recognized current BrowserArtifactStamp."
            {:generated-stamp generated-stamp})]

          (vec
           (concat
            (digest-errors
             (:plan-digests current-stamp)
             (:plan-digests generated-stamp))

            (keep
             identity
             [(field-mismatch
               :assembly-manifest-version
               (:assembly-manifest-version current-stamp)
               (:assembly-manifest-version generated-stamp))

              (field-mismatch
               :plan-registry-version
               (:plan-registry-version current-stamp)
               (:plan-registry-version generated-stamp))

              (field-mismatch
               :assembly-name
               (:assembly-name current-stamp)
               (:assembly-name generated-stamp))

              (field-mismatch
               :plan-registry-name
               (:plan-registry-name current-stamp)
               (:plan-registry-name generated-stamp))

              (field-mismatch
               :browser-role
               (:browser-role current-stamp)
               (:browser-role generated-stamp))

              (field-mismatch
               :required-plan-keys
               (:required-plan-keys current-stamp)
               (:required-plan-keys generated-stamp))

              (field-mismatch
               :features
               (:features current-stamp)
               (:features generated-stamp))

              (field-mismatch
               :optimistic-command-transport
               (:optimistic-command-transport current-stamp)
               (:optimistic-command-transport generated-stamp))

              (field-mismatch
               :bootstrap-entrypoint
               (:bootstrap-entrypoint current-stamp)
               (:bootstrap-entrypoint generated-stamp))]))))]
    {:gesso.live.browser.artifact/type
     correspondence-report-type

     :gesso.live.browser.artifact/version
     artifact-version

     :valid?
     (empty? errors)

     :errors
     errors

     :warnings
     []

     :analysis
     {:current-stamp current-stamp
      :generated-stamp generated-stamp}}))

(defn report?
  "True when value has the closed shape of a current artifact-correspondence
   report and :valid? agrees with its error collection."
  [value]
  (and
   (closed-map? report-keys value)
   (= correspondence-report-type
      (:gesso.live.browser.artifact/type value))
   (= artifact-version
      (:gesso.live.browser.artifact/version value))
   (boolean?
    (:valid? value))
   (vector?
    (:errors value))
   (vector?
    (:warnings value))
   (closed-map?
    analysis-keys
    (:analysis value))
   (stamp?
    (get-in value
            [:analysis :current-stamp]))
   (or (not (:valid? value))
       (stamp?
        (get-in value
                [:analysis :generated-stamp])))
   (= (:valid? value)
      (empty? (:errors value)))))

(defn valid?
  "True only for a recognized successful browser artifact correspondence report."
  [report]
  (and
   (report? report)
   (true? (:valid? report))))

(defn require-correspondence!
  "Require a generated browser artifact stamp to correspond exactly to the
   application's current BrowserAssemblyManifest.

   Returns the generated stamp on success so callers can thread the already
   checked metadata into later build/start steps. Throws ExceptionInfo carrying
   the complete correspondence report on drift."
  [current-manifest generated-stamp]
  (let [report
        (check-correspondence
         current-manifest
         generated-stamp)]
    (when-not
     (valid? report)
      (throw
       (artifact-error
        :browser-artifact-correspondence-failed
        "Generated Gesso browser artifact does not correspond to the current browser assembly."
        {:correspondence report})))
    generated-stamp))

(defn explain
  "Return a compact stable summary of a BrowserArtifactStamp or correspondence
   report."
  [value]
  (cond
    (stamp? value)
    {:type stamp-type
     :version artifact-version
     :assembly-name (:assembly-name value)
     :plan-count (count (:plan-digests value))
     :plan-keys (set (keys (:plan-digests value)))
     :browser-role (:browser-role value)
     :features (:features value)
     :optimistic-command-transport
     (:optimistic-command-transport value)
     :bootstrap-entrypoint
     (:bootstrap-entrypoint value)}

    (report? value)
    {:type correspondence-report-type
     :version artifact-version
     :valid? (:valid? value)
     :error-count (count (:errors value))
     :warning-count (count (:warnings value))
     :assembly-name
     (get-in value
             [:analysis :current-stamp :assembly-name])}

    :else
    (throw
     (artifact-error
      :unsupported-explain-value
      "Expected a BrowserArtifactStamp or browser artifact correspondence report."
      {:value value}))))

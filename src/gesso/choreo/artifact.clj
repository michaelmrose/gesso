(ns gesso.choreo.artifact
  "JVM compiler artifacts for Gesso Choreo.

   This namespace emits two sibling products from one choreography:

   - ExecutablePlan values, one per role, containing only portable runtime data;
   - one DiagnosticProofSidecar containing proof/diagnostic information bound to
     those exact executable values by content digest.

   The sidecar is deliberately non-executable. Runtime code must not need it,
   and the sidecar never embeds an ExecutablePlan. Semantic/source identities
   may therefore remain useful for diagnostics without leaking back into the
   production runtime artifact."
  (:require
   [gesso.choreo.project :as project]
   [gesso.choreo.proof :as proof]
   [gesso.choreo.verify :as verify])
  (:import
   (java.nio.charset StandardCharsets)
   (java.security MessageDigest)))

;; -----------------------------------------------------------------------------
;; Artifact identity
;; -----------------------------------------------------------------------------

(def artifact-version
  1)

(def artifact-set-type
  :gesso.choreo/artifact-set)

(def diagnostic-sidecar-type
  :gesso.choreo/diagnostic-proof-sidecar)

;; -----------------------------------------------------------------------------
;; Errors
;; -----------------------------------------------------------------------------

(defn- artifact-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.choreo.artifact/error
      :error/kind kind}
     data))))

;; -----------------------------------------------------------------------------
;; Canonical executable content and digests
;; -----------------------------------------------------------------------------

(declare canonical-value)

(defn- canonical-sort-key
  [value]
  (pr-str
   (canonical-value value)))

(defn- canonical-map
  [value]
  [:map
   (mapv
    (fn [[k v]]
      [(canonical-value k)
       (canonical-value v)])
    (sort-by
     (comp canonical-sort-key key)
     value))])

(defn- canonical-set
  [value]
  [:set
   (mapv canonical-value
         (sort-by canonical-sort-key
                  value))])

(defn- canonical-value
  "Return a deterministic, type-preserving data representation suitable for
   hashing. Map/set iteration order cannot affect the result."
  [value]
  (cond
    (map? value)
    (canonical-map value)

    (set? value)
    (canonical-set value)

    (vector? value)
    [:vector
     (mapv canonical-value value)]

    (list? value)
    [:list
     (mapv canonical-value value)]

    (seq? value)
    [:seq
     (mapv canonical-value value)]

    :else
    [:scalar value]))

(defn- sha-256-hex
  [text]
  (let [digest
        (.digest
         (MessageDigest/getInstance "SHA-256")
         (.getBytes ^String text
                    StandardCharsets/UTF_8))]
    (apply str
           (map #(format "%02x"
                         (bit-and 0xff %))
                digest))))

(defn executable-digest
  "Return the SHA-256 content identity of one exact ExecutablePlan.

   The digest is computed from canonicalized runtime artifact data only. Source
   map insertion order and compiler metadata that does not survive projection
   therefore cannot perturb it."
  [plan]
  (when-not (project/executable-plan? plan)
    (artifact-error
     :invalid-executable-plan
     "Executable digest requires a Gesso Choreo ExecutablePlan."
     {:value plan}))

  (sha-256-hex
   (pr-str
    (canonical-value plan))))

;; -----------------------------------------------------------------------------
;; Diagnostic locations
;; -----------------------------------------------------------------------------

(defn- base-location
  [obligation runtime-locator]
  (cond->
   {:role
    (:role obligation)

    :runtime-locator
    runtime-locator

    :semantic-state
    (:state obligation)

    :proof-obligation-id
    (:id obligation)}

    (contains? obligation :endpoint)
    (assoc :endpoint
           (:endpoint obligation))))

(defn- obligation-locations
  [obligation]
  (let [endpoint
        (:endpoint obligation)]
    (cond
      (= :receiver endpoint)
      (mapv
       (fn [{:keys [runtime-locator alternative-index]}]
         (cond->
          (base-location
           obligation
           runtime-locator)

          (some? alternative-index)
          (assoc :alternative-index
                 alternative-index)))
       (get-in obligation
               [:actual :matching-alternatives]))

      (nat-int? (:runtime-locator obligation))
      [(base-location
        obligation
        (:runtime-locator obligation))]

      :else
      [])))

(defn- diagnostic-locations
  [proof-result]
  (->> (:obligations proof-result)
       (mapcat obligation-locations)
       (sort-by
        (juxt
         (comp pr-str :semantic-state)
         (comp pr-str :role)
         (comp pr-str :endpoint)
         :runtime-locator
         #(or (:alternative-index %) -1)))
       vec))

;; -----------------------------------------------------------------------------
;; Sidecar and artifact predicates
;; -----------------------------------------------------------------------------

(defn diagnostic-sidecar?
  "True when value has the closed top-level identity/shape of the current
   DiagnosticProofSidecar format. Digest agreement with a particular set of
   plans is checked separately by sidecar-matches?."
  [value]
  (and
   (map? value)
   (= diagnostic-sidecar-type
      (:gesso.choreo/type value))
   (= artifact-version
      (:gesso.choreo/version value))
   (= project/executable-plan-version
      (:executable-plan-version value))
   (= proof/proof-version
      (:proof-version value))
   (or (nil? (:choreography-name value))
       (keyword? (:choreography-name value)))
   (map? (:executable-digests value))
   (every?
    (fn [[role digest]]
      (and (keyword? role)
           (string? digest)
           (boolean
            (re-matches #"[0-9a-f]{64}"
                        digest))))
    (:executable-digests value))
   (vector? (:locations value))
   (proof/result? (:proof value))))

(declare sidecar-matches?)

(defn artifact-set?
  "True when value is a current ArtifactSet whose executable plans are valid
   and whose sibling DiagnosticProofSidecar is bound to those exact plans."
  [value]
  (and
   (map? value)
   (= artifact-set-type
      (:gesso.choreo/type value))
   (= artifact-version
      (:gesso.choreo/version value))
   (map? (:executable-plans value))
   (every?
    (fn [[role plan]]
      (and (keyword? role)
           (project/executable-plan? plan)
           (= role (:role plan))))
    (:executable-plans value))
   (sidecar-matches?
    (:executable-plans value)
    (:diagnostic-proof-sidecar value))))

;; -----------------------------------------------------------------------------
;; Sidecar binding
;; -----------------------------------------------------------------------------

(defn- plan-digests
  [plans]
  (into
   (sorted-map)
   (map
    (fn [[role plan]]
      [role
       (executable-digest plan)]))
   plans))

(defn- first-digest-mismatch
  [plans sidecar]
  (let [actual
        (plan-digests plans)

        claimed
        (:executable-digests sidecar)

        roles
        (sort-by pr-str
                 (into #{}
                       (concat
                        (keys actual)
                        (keys claimed))))]
    (some
     (fn [role]
       (let [actual-digest
             (get actual role ::missing)

             claimed-digest
             (get claimed role ::missing)]
         (when-not (= actual-digest
                      claimed-digest)
           {:role role
            :actual-digest
            (when-not (= ::missing actual-digest)
              actual-digest)
            :sidecar-digest
            (when-not (= ::missing claimed-digest)
              claimed-digest)})))
     roles)))

(defn sidecar-matches?
  "True exactly when sidecar is a valid DiagnosticProofSidecar whose role set
   and executable digests match plans."
  [plans sidecar]
  (and
   (map? plans)
   (every?
    (fn [[role plan]]
      (and (keyword? role)
           (project/executable-plan? plan)
           (= role (:role plan))))
    plans)
   (diagnostic-sidecar? sidecar)
   (nil?
    (first-digest-mismatch
     plans
     sidecar))))

(defn require-sidecar-match!
  "Return sidecar when it is valid and bound to exactly plans; otherwise throw
   a deterministic diagnostic error. This function never alters plans."
  [plans sidecar]
  (when-not (diagnostic-sidecar? sidecar)
    (artifact-error
     :invalid-diagnostic-sidecar
     "Expected a Gesso Choreo DiagnosticProofSidecar."
     {:sidecar sidecar}))

  (if-let [{:keys [role actual-digest sidecar-digest]}
           (first-digest-mismatch
            plans
            sidecar)]
    (artifact-error
     :sidecar-digest-mismatch
     "Diagnostic proof sidecar does not match the executable artifact."
     {:role role
      :actual-digest actual-digest
      :sidecar-digest sidecar-digest})
    sidecar))

;; -----------------------------------------------------------------------------
;; Emission
;; -----------------------------------------------------------------------------

(defn emit-artifacts
  "Compile one choreography into sibling executable and diagnostic products.

   Projection and the current finite structural proof are run from the same
   successful verification artifact. The proof result is diagnostic/compiler
   data only and is not inserted into any ExecutablePlan.

   Emission refuses to produce an ArtifactSet when the current structural proof
   fails. This makes the checked compiler invariant a build-time gate without
   making the sidecar a runtime dependency."
  [choreography-or-verified]
  (let [verified
        (verify/ensure-verified
         choreography-or-verified)

        choreography
        (:choreography verified)

        plans
        (project/project-all
         verified)

        proof-result
        (proof/check-projection-boundaries
         verified)]

    (when-not (proof/valid? proof-result)
      (artifact-error
       :proof-check-failed
       "Cannot emit Choreo artifacts because projection-boundary checking failed."
       {:proof proof-result}))

    (let [sidecar
          {:gesso.choreo/type
           diagnostic-sidecar-type

           :gesso.choreo/version
           artifact-version

           :executable-plan-version
           project/executable-plan-version

           :proof-version
           proof/proof-version

           :choreography-name
           (:name choreography)

           :executable-digests
           (plan-digests plans)

           :locations
           (diagnostic-locations
            proof-result)

           :proof
           proof-result}]

      {:gesso.choreo/type
       artifact-set-type

       :gesso.choreo/version
       artifact-version

       :executable-plans
       plans

       :diagnostic-proof-sidecar
       sidecar})))

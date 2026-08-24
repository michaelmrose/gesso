(ns gesso.choreo.artifact
  "JVM compiler artifacts for Gesso Choreo.

   This namespace emits two sibling products from one choreography:

   - ExecutablePlan values, one per role, containing only portable runtime data;
   - one DiagnosticProofSidecar containing the current projection structural
     certificate and diagnostic information bound to those exact executable
     values by content digest.

   The sidecar is deliberately non-executable. Runtime code must not need it,
   and the sidecar never embeds an ExecutablePlan. Semantic/source identities
   may therefore remain useful for diagnostics without leaking back into the
   production runtime artifact.

   This namespace also owns the JVM-only exact-artifact binding for concrete
   correspondence evidence. A correspondence witness may be attached to an
   ArtifactSet only after re-projecting the same verified choreography and
   proving that those projected ExecutablePlan values match the ArtifactSet
   exactly. This prevents structural proof evidence for choreography A from
   being accidentally composed with behavioral evidence for choreography B."
  (:require
   [gesso.choreo.correspondence :as correspondence]
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
  2)

(def artifact-set-type
  :gesso.choreo/artifact-set)

(def diagnostic-sidecar-type
  :gesso.choreo/diagnostic-proof-sidecar)

(def ^:private diagnostic-sidecar-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :executable-plan-version
    :proof-version
    :projection-structural-certificate-version
    :choreography-name
    :executable-digests
    :locations
    :proof})

(def ^:private artifact-set-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :executable-plans
    :diagnostic-proof-sidecar})

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
   (= diagnostic-sidecar-keys
      (set (keys value)))
   (= diagnostic-sidecar-type
      (:gesso.choreo/type value))
   (= artifact-version
      (:gesso.choreo/version value))
   (= project/executable-plan-version
      (:executable-plan-version value))
   (= proof/proof-version
      (:proof-version value))
   (= proof/projection-structural-certificate-version
      (:projection-structural-certificate-version value))
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
   (proof/projection-structural-certificate?
    (:proof value))
   ;; A current sidecar may not relabel a recognized historical certificate as
   ;; current. Historical certificate recognition is useful to proof tooling,
   ;; but an ArtifactSet sidecar certifies the exact current compiler proof
   ;; contract emitted alongside its ExecutablePlans. Bind the declared
   ;; certificate version to the nested certificate itself before accepting it.
   (= (:projection-structural-certificate-version value)
      (get-in value [:proof :gesso.choreo/version]))
   (proof/structural-certificate-valid?
    (:proof value))))

(defn- executable-plans?
  "True when plans is a role-keyed map of current ExecutablePlan values and
   every map key agrees with the plan's own role identity."
  [plans]
  (and
   (map? plans)
   (every?
    (fn [[role plan]]
      (and (keyword? role)
           (project/executable-plan? plan)
           (= role (:role plan))))
    plans)))

(defn- require-executable-plans!
  [plans]
  (when-not (executable-plans? plans)
    (artifact-error
     :invalid-executable-plans
     "Expected a role-keyed map of matching Gesso Choreo ExecutablePlan values."
     {:plans plans}))
  plans)

(declare sidecar-matches?)

(defn artifact-set?
  "True when value is a current ArtifactSet whose executable plans are valid
   and whose sibling DiagnosticProofSidecar is bound to those exact plans."
  [value]
  (and
   (map? value)
   (= artifact-set-keys
      (set (keys value)))
   (= artifact-set-type
      (:gesso.choreo/type value))
   (= artifact-version
      (:gesso.choreo/version value))
   (executable-plans?
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
   (executable-plans? plans)
   (diagnostic-sidecar? sidecar)
   (nil?
    (first-digest-mismatch
     plans
     sidecar))))

(defn require-sidecar-match!
  "Return sidecar when it is valid and bound to exactly plans; otherwise throw
   a deterministic diagnostic error. This function never alters plans."
  [plans sidecar]
  (require-executable-plans! plans)

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

   Projection and the current finite structural proof certificate are produced
   from the same successful verification artifact. The certificate is
   diagnostic/compiler data only and is not inserted into any ExecutablePlan.

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

        proof-certificate
        (proof/check-projection-structure
         verified)]

    (when-not (proof/structural-certificate-valid?
               proof-certificate)
      (artifact-error
       :proof-check-failed
       "Cannot emit Choreo artifacts because projection structural checking failed."
       {:proof proof-certificate}))

    (let [sidecar
          {:gesso.choreo/type
           diagnostic-sidecar-type

           :gesso.choreo/version
           artifact-version

           :executable-plan-version
           project/executable-plan-version

           :proof-version
           proof/proof-version

           :projection-structural-certificate-version
           proof/projection-structural-certificate-version

           :choreography-name
           (:name choreography)

           :executable-digests
           (plan-digests plans)

           :locations
           (diagnostic-locations
            (:boundary-proof proof-certificate))

           :proof
           proof-certificate}]

      {:gesso.choreo/type
       artifact-set-type

       :gesso.choreo/version
       artifact-version

       :executable-plans
       plans

       :diagnostic-proof-sidecar
       sidecar})))

;; -----------------------------------------------------------------------------
;; Exact-artifact-bound concrete correspondence evidence
;; -----------------------------------------------------------------------------

(def artifact-correspondence-evidence-version
  1)

(def artifact-correspondence-evidence-type
  :gesso.choreo.artifact/concrete-correspondence-evidence)

(def artifact-correspondence-evidence-property
  :exact-artifact-bound-concrete-correspondence-v1)

(def artifact-correspondence-evidence-classification
  :exact-artifact-bound-concrete-witness-evidence)

(def artifact-correspondence-evidence-nonclaims
  #{:all-projected-executions
    :trace-refinement
    :projection-refinement})

(def ^:private artifact-correspondence-evidence-keys
  #{:gesso.choreo/type
    :gesso.choreo/version
    :property
    :classification
    :valid?
    :mode
    :artifact-version
    :correspondence-version
    :executable-digests
    :structural-certificate
    :correspondence
    :nonclaims})

(defn artifact-correspondence-evidence?
  "True when value has the closed shape of exact-artifact-bound concrete
   correspondence evidence.

   This predicate checks the internal evidence contract. Use evidence-matches?
   when the question is whether the evidence belongs to a particular ArtifactSet."
  [value]
  (and
   (map? value)
   (= artifact-correspondence-evidence-keys
      (set (keys value)))
   (= artifact-correspondence-evidence-type
      (:gesso.choreo/type value))
   (= artifact-correspondence-evidence-version
      (:gesso.choreo/version value))
   (= artifact-correspondence-evidence-property
      (:property value))
   (= artifact-correspondence-evidence-classification
      (:classification value))
   (boolean? (:valid? value))
   (contains? #{:lockstep :weak}
              (:mode value))
   (= artifact-version
      (:artifact-version value))
   (= correspondence/correspondence-version
      (:correspondence-version value))
   (map? (:executable-digests value))
   (every?
    (fn [[role digest]]
      (and (keyword? role)
           (string? digest)
           (boolean
            (re-matches #"[0-9a-f]{64}"
                        digest))))
    (:executable-digests value))
   (proof/projection-structural-certificate?
    (:structural-certificate value))
   (proof/structural-certificate-valid?
    (:structural-certificate value))
   (correspondence/result?
    (:correspondence value))
   (= (case (:mode value)
        :lockstep correspondence/correspondence-property
        :weak correspondence/weak-correspondence-property)
      (get-in value [:correspondence :property]))
   (= artifact-correspondence-evidence-nonclaims
      (:nonclaims value))
   (= (:valid? value)
      (correspondence/valid?
       (:correspondence value)))))


(defn artifact-correspondence-valid?
  "True only for recognized exact-artifact-bound evidence whose concrete
   correspondence result is valid. Binding to a particular ArtifactSet is a
   separate question checked by evidence-matches?."
  [evidence]
  (and
   (artifact-correspondence-evidence? evidence)
   (true? (:valid? evidence))))

(defn evidence-matches?
  "True exactly when recognized correspondence evidence is bound to
   artifact-set. A failed concrete witness can still be correctly artifact-bound;
   use artifact-correspondence-valid? when semantic success is also required.

   Matching requires the exact executable digest map and the exact structural
   certificate carried by the ArtifactSet sidecar. A matching digest map alone
   cannot launder a stale, foreign, or fabricated proof certificate."
  [artifact-set evidence]
  (and
   (artifact-set? artifact-set)
   (artifact-correspondence-evidence? evidence)
   (= (get-in artifact-set
              [:diagnostic-proof-sidecar
               :executable-digests])
      (:executable-digests evidence))
   (= (get-in artifact-set
              [:diagnostic-proof-sidecar
               :proof])
      (:structural-certificate evidence))))

(defn require-evidence-match!
  "Return evidence when it is valid and bound to exactly artifact-set;
   otherwise throw a deterministic diagnostic error."
  [artifact-set evidence]
  (when-not (artifact-set? artifact-set)
    (artifact-error
     :invalid-artifact-set
     "Expected a current Gesso Choreo ArtifactSet."
     {:artifact-set artifact-set}))

  (when-not (artifact-correspondence-evidence? evidence)
    (artifact-error
     :invalid-artifact-correspondence-evidence
     "Expected Gesso Choreo exact-artifact-bound correspondence evidence."
     {:evidence evidence}))

  (when-not (= (get-in artifact-set
                       [:diagnostic-proof-sidecar
                        :executable-digests])
               (:executable-digests evidence))
    (artifact-error
     :evidence-digest-mismatch
     "Concrete correspondence evidence does not match the executable artifact digests."
     {:artifact-digests
      (get-in artifact-set
              [:diagnostic-proof-sidecar
               :executable-digests])
      :evidence-digests
      (:executable-digests evidence)}))

  (when-not (= (get-in artifact-set
                       [:diagnostic-proof-sidecar
                        :proof])
               (:structural-certificate evidence))
    (artifact-error
     :evidence-proof-mismatch
     "Concrete correspondence evidence does not match the ArtifactSet structural certificate."
     {:artifact-proof
      (get-in artifact-set
              [:diagnostic-proof-sidecar
               :proof])
      :evidence-proof
      (:structural-certificate evidence)}))

  evidence)

(defn- require-artifact-choreography-match!
  [artifact-set choreography-or-verified]
  (when-not (artifact-set? artifact-set)
    (artifact-error
     :invalid-artifact-set
     "Expected a current Gesso Choreo ArtifactSet."
     {:artifact-set artifact-set}))

  (let [verified
        (verify/ensure-verified
         choreography-or-verified)

        projected
        (project/project-all verified)

        structural-certificate
        (proof/check-projection-structure verified)

        sidecar
        (:diagnostic-proof-sidecar artifact-set)]

    (when-let [{:keys [role actual-digest sidecar-digest]}
               (first-digest-mismatch
                projected
                sidecar)]
      (artifact-error
       :artifact-choreography-mismatch
       "The supplied choreography does not project to the exact ExecutablePlan artifacts certified by this ArtifactSet."
       {:role role
        :projected-digest actual-digest
        :artifact-digest sidecar-digest}))

    ;; Exact executable identity alone is not enough to bind the global program:
    ;; projection intentionally erases some global-only facts, most notably the
    ;; authored terminal outcome. Recompute the current structural certificate
    ;; from the supplied verified choreography and require exact agreement with
    ;; the sidecar as a second, independent binding condition.
    (when-not (= (:proof sidecar)
                 structural-certificate)
      (artifact-error
       :artifact-proof-choreography-mismatch
       "The supplied choreography does not reproduce the structural proof certificate bound to this ArtifactSet."
       {:artifact-proof
        (:proof sidecar)
        :choreography-proof
        structural-certificate}))

    verified))

(defn- artifact-correspondence-evidence
  [artifact-set mode result]
  (let [sidecar
        (:diagnostic-proof-sidecar artifact-set)]
    {:gesso.choreo/type
     artifact-correspondence-evidence-type

     :gesso.choreo/version
     artifact-correspondence-evidence-version

     :property
     artifact-correspondence-evidence-property

     :classification
     artifact-correspondence-evidence-classification

     :valid?
     (correspondence/valid? result)

     :mode
     mode

     :artifact-version
     artifact-version

     :correspondence-version
     correspondence/correspondence-version

     :executable-digests
     (:executable-digests sidecar)

     :structural-certificate
     (:proof sidecar)

     :correspondence
     result

     :nonclaims
     artifact-correspondence-evidence-nonclaims}))

(defn check-artifact-witness
  "Check one concrete lockstep witness and bind the resulting correspondence
   evidence to exactly artifact-set.

   The supplied choreography is verified and independently re-projected first.
   Its exact ExecutablePlan digests AND recomputed structural certificate must
   match the ArtifactSet sidecar before correspondence is executed. The second
   condition matters because projection intentionally erases global-only facts
   such as authored terminal outcomes. Together they prevent evidence for another choreography,
   even one with a coincidentally similar name or role set, from being composed
   with this structural certificate.

   The result remains witness-level evidence only. It does not quantify over all
   projected executions and therefore explicitly does not claim trace or
   projection refinement."
  ([artifact-set choreography-or-verified witness]
   (check-artifact-witness
    artifact-set
    choreography-or-verified
    witness
    nil))
  ([artifact-set choreography-or-verified witness options]
   (let [verified
         (require-artifact-choreography-match!
          artifact-set
          choreography-or-verified)

         result
         (correspondence/check-witness
          verified
          witness
          options)

         evidence
         (artifact-correspondence-evidence
          artifact-set
          :lockstep
          result)]
     (require-evidence-match!
      artifact-set
      evidence))))

(defn check-artifact-weak-witness
  "Check one concrete weak/commuting witness and bind the resulting
   correspondence evidence to exactly artifact-set.

   Exact choreography->ExecutablePlan agreement is required before witness
   replay for the same reason as check-artifact-witness. The result is still one
   concrete schedule, not the quantified projection/refinement theorem."
  ([artifact-set choreography-or-verified witness]
   (check-artifact-weak-witness
    artifact-set
    choreography-or-verified
    witness
    nil))
  ([artifact-set choreography-or-verified witness options]
   (let [verified
         (require-artifact-choreography-match!
          artifact-set
          choreography-or-verified)

         result
         (correspondence/check-weak-witness
          verified
          witness
          options)

         evidence
         (artifact-correspondence-evidence
          artifact-set
          :weak
          result)]
     (require-evidence-match!
      artifact-set
      evidence))))


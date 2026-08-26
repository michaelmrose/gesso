(ns gesso.live.progression
  "Portable authoritative-basis and refresh-progression vocabulary for Gesso Live.

   This namespace is the semantic carrier between trusted authority/consistency
   adapters and the rest of Live.  It deliberately knows nothing about XTDB
   TransactionKey, HTMX, SSE, DOM state, application revisions, or browser
   resources.

   A basis is opaque.  Generic Live code may test exact equality, but it may not
   infer that one distinct basis is newer/stronger from arrival order, numbers,
   timestamps, strings, or storage-specific fields.

   A refresh requirement is therefore an *orderless set* of opaque bases.  Safe
   composition is set union: exact duplicates collapse, while distinct bases are
   retained unless an authority-specific layer supplies explicit progression
   evidence.  Coalescing can consequently never weaken a requirement merely by
   keeping the last invalidation it observed.

   Explicit progression evidence has the closed shape:

     {:from B1
      :to B2
      :relation :advances}

   This namespace validates only that shape and exact endpoints.  It does not
   establish that the witness is truthful.  Trusted XTDB/authority code owns that
   premise.  Browser-supplied witnesses therefore remain untrusted data until a
   trusted boundary establishes them.

   The public wire representation is versioned portable EDN data.  Its :bases
   vector is an encoding convenience only; vector position has no semantic
   ordering meaning."
  (:require
   [gesso.choreo.type :as choreo.type]))

;; =============================================================================
;; Public identity / vocabulary
;; =============================================================================

(def progression-version 1)

(def requirement-type
  :gesso.live.progression/requirement)

(def requirement-wire-type
  :gesso.live.progression/requirement-wire)

(def advance-relation
  :advances)

(def requirement-keys
  #{:gesso.live.progression/type
    :gesso.live.progression/version
    :bases})

(def requirement-wire-keys
  #{:gesso.live.progression/type
    :gesso.live.progression/version
    :bases})

(def advance-keys
  #{:from :to :relation})

;; =============================================================================
;; Errors / validation helpers
;; =============================================================================

(defn- progression-error
  [kind message data]
  (throw
   (ex-info
    message
    (merge
     {:error/type :gesso.live.progression/error
      :error/kind kind}
     data))))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (progression-error
     :invalid-map
     (str label " must be a map.")
     {:label label
      :value value}))
  value)

(defn- unknown-keys
  [allowed value]
  (seq (remove allowed (keys value))))

(defn- require-closed-map!
  [label allowed value]
  (require-map! label value)
  (when-let [unknown (unknown-keys allowed value)]
    (progression-error
     :unknown-fields
     (str label " contains unsupported fields.")
     {:label label
      :unknown-fields (set unknown)
      :allowed-fields allowed
      :value value}))
  value)

;; =============================================================================
;; Opaque authoritative bases
;; =============================================================================

(defn basis?
  "True when value has the portable Choreo AuthoritativeBasis shape.

   Shape validation says only that a basis is present.  It says nothing about
   authority, ordering, compatibility, or whether a browser may be trusted to
   assert it."
  [value]
  (choreo.type/valid? ::choreo.type/basis value))

(defn require-basis!
  "Return one opaque basis or throw when absent/invalid."
  ([value]
   (require-basis! :basis value))
  ([label value]
   (when-not (basis? value)
     (progression-error
      :invalid-basis
      "Authoritative basis must be a present portable basis value."
      {:label label
       :basis value}))
   value))

;; =============================================================================
;; Explicit advancement evidence
;; =============================================================================

(defn advance
  "Construct closed progression evidence that to advances from.

   Construction validates shape only.  The caller/trusted consistency adapter
   remains responsible for the truth of the progression claim."
  [from to]
  {:from (require-basis! :from from)
   :to (require-basis! :to to)
   :relation advance-relation})

(defn advance?
  "True for one closed, structurally valid :advances witness."
  [value]
  (and
   (map? value)
   (= advance-keys (set (keys value)))
   (= advance-relation (:relation value))
   (basis? (:from value))
   (basis? (:to value))))

(defn require-advance!
  "Return a structurally valid advancement witness or throw."
  [value]
  (when-not (advance? value)
    (progression-error
     :invalid-advance
     "Progression witness must be a closed {:from :to :relation :advances} map."
     {:advance value}))
  value)

(defn advances?
  "True when witness exactly states that next-basis advances current-basis.

   Exact endpoint matching is semantic here; truth of the supplied witness is a
   trusted-boundary premise outside this namespace."
  [current-basis next-basis witness]
  (and
   (advance? witness)
   (= current-basis (:from witness))
   (= next-basis (:to witness))))

(defn install-allowed?
  "Generic monotone-install rule for opaque authoritative bases.

   The first installed basis is allowed.  Reinstalling the exact same basis is
   idempotent.  Moving between distinct bases requires an exact explicit
   :advances witness.  No other comparison is attempted."
  [current-basis next-basis witness]
  (require-basis! :next-basis next-basis)
  (cond
    (nil? current-basis)
    true

    (= current-basis next-basis)
    true

    :else
    (do
      (require-basis! :current-basis current-basis)
      (advances? current-basis next-basis witness))))

;; =============================================================================
;; Orderless refresh requirements
;; =============================================================================

(defn- valid-basis-set?
  [value]
  (and
   (set? value)
   (seq value)
   (every? basis? value)))

(defn requirement?
  "True for one normalized non-empty refresh progression requirement."
  [value]
  (and
   (map? value)
   (= requirement-keys (set (keys value)))
   (= requirement-type
      (:gesso.live.progression/type value))
   (= progression-version
      (:gesso.live.progression/version value))
   (valid-basis-set? (:bases value))))

(defn require-requirement!
  "Return normalized requirement or throw."
  [value]
  (when-not (requirement? value)
    (progression-error
     :invalid-requirement
     "Expected a normalized non-empty Gesso Live progression requirement."
     {:requirement value}))
  value)

(defn requirement
  "Construct a singleton refresh requirement for one opaque authoritative basis."
  [basis]
  {:gesso.live.progression/type requirement-type
   :gesso.live.progression/version progression-version
   :bases #{(require-basis! basis)}})

(defn requirement-from-bases
  "Construct one orderless requirement from a non-empty collection of bases.

   Exact duplicate bases collapse.  Distinct bases remain distinct; collection
   order cannot weaken or strengthen the resulting requirement."
  [bases]
  ;; A basis itself may legitimately be a map. Treating maps as collections
  ;; here would silently reinterpret one opaque basis as a collection of map
  ;; entries, each of which is also a non-nil value and would therefore pass
  ;; shape validation. Require an unambiguous collection representation.
  (when-not (or (sequential? bases)
                (set? bases))
    (progression-error
     :invalid-bases
     "Progression requirement bases must be a non-empty sequential collection or set."
     {:bases bases}))
  (let [bases' (into #{} (map require-basis!) bases)]
    (when-not (seq bases')
      (progression-error
       :empty-requirement
       "Progression requirement must contain at least one authoritative basis."
       {:bases bases}))
    {:gesso.live.progression/type requirement-type
     :gesso.live.progression/version progression-version
     :bases bases'}))

(defn normalize-requirement
  "Normalize an optional requirement.

   nil means that no authoritative refresh requirement was supplied.  Non-nil
   input must already be the explicit requirement shape; arbitrary maps are not
   guessed to be either bases or requirements."
  [value]
  (when (some? value)
    (require-requirement! value)))

(defn required-bases
  "Return the orderless set of bases represented by requirement, or #{} for nil."
  [requirement-value]
  (if (nil? requirement-value)
    #{}
    (:bases (require-requirement! requirement-value))))

(defn compose
  "Safely compose zero or more optional progression requirements.

   Composition is commutative/idempotent set union.  It may collapse exact
   equality only.  In particular, it never treats the last-arriving basis as
   stronger and never drops a distinct basis without explicit authority-specific
   evidence.

   Returns nil when every argument is nil."
  [& requirements]
  (let [bases
        (reduce
         (fn [acc requirement-value]
           (into acc (required-bases requirement-value)))
         #{}
         requirements)]
    (when (seq bases)
      (requirement-from-bases bases))))

(defn covers?
  "True when candidate conservatively contains every basis in required.

   This checks requirement containment only; it does not infer that a distinct
   basis semantically satisfies another basis."
  [candidate required]
  (let [candidate-bases (required-bases candidate)
        required-bases' (required-bases required)]
    (every? candidate-bases required-bases')))

(defn satisfied-by?
  "True when installed-basis satisfies every atomic required basis using only
   exact equality or explicitly supplied direct advancement witnesses.

   advances is a collection of structurally valid witnesses.  Their truth must
   already have been established by a trusted authority-specific boundary.
   Transitive closure is deliberately not invented here: if one installed basis
   is claimed to satisfy an older requirement, provide the direct evidence that
   the trusted adapter is willing to stand behind."
  ([installed-basis requirement-value]
   (satisfied-by? installed-basis requirement-value []))
  ([installed-basis requirement-value advances]
   (require-basis! :installed-basis installed-basis)
   (let [required (required-bases requirement-value)
         advances' (mapv require-advance! advances)]
     (every?
      (fn [required-basis]
        (or
         (= required-basis installed-basis)
         (some
          #(advances? required-basis installed-basis %)
          advances')))
      required))))

;; =============================================================================
;; Portable EDN wire form
;; =============================================================================

(defn- encoding-sort-key
  [value]
  ;; This ordering exists solely to make ordinary EDN output stable enough for
  ;; inspection.  It is not exposed as a basis comparison and carries no
  ;; authority/progression meaning.
  (pr-str value))

(defn requirement->wire
  "Encode one normalized requirement as closed versioned portable data.

   The :bases vector ordering is encoding-only.  Consumers must recover the
   orderless requirement with wire->requirement before composing/interpreting it."
  [requirement-value]
  (let [requirement' (require-requirement! requirement-value)]
    {:gesso.live.progression/type requirement-wire-type
     :gesso.live.progression/version progression-version
     :bases (->> (:bases requirement')
                 (sort-by encoding-sort-key)
                 vec)}))

(defn wire->requirement
  "Decode/validate the versioned portable wire form into an orderless requirement."
  [wire]
  (require-closed-map!
   "Progression requirement wire"
   requirement-wire-keys
   wire)
  (when-not (= requirement-wire-type
               (:gesso.live.progression/type wire))
    (progression-error
     :wrong-wire-type
     "Progression wire has the wrong semantic type."
     {:expected requirement-wire-type
      :actual (:gesso.live.progression/type wire)}))
  (when-not (= progression-version
               (:gesso.live.progression/version wire))
    (progression-error
     :unsupported-wire-version
     "Unsupported Gesso Live progression wire version."
     {:expected progression-version
      :actual (:gesso.live.progression/version wire)}))
  (let [bases (:bases wire)]
    (when-not (sequential? bases)
      (progression-error
       :invalid-wire-bases
       "Progression wire :bases must be a non-empty sequential collection."
       {:bases bases}))
    (requirement-from-bases bases)))

;; =============================================================================
;; Diagnostics
;; =============================================================================

(defn explain
  "Return a compact portable diagnostic view with no implied basis ordering."
  [requirement-value]
  (when-some [requirement' (normalize-requirement requirement-value)]
    {:gesso.live.progression/type requirement-type
     :gesso.live.progression/version progression-version
     :basis-count (count (:bases requirement'))
     :bases (:bases requirement')}))

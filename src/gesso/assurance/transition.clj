(ns gesso.assurance.transition
  "Source-only constructor for CLOSED, typed guarded replacement (v1239).

   A downstream application declares ordinary Clojure DATA:

     {:subject {:kind :operation :id :example/claim
                :interface {:operation :example/claim}}
      :state-types {:request/status {:type :enum :name :request/status
                                     :values #{:open :claimed}}
                    :request/owner {:type :keyword :optional? true}
                    :request/created-at {:type :integer}}
      :input-types {:input/actor {:type :keyword}}
      :guard {:readers #{:request/status}
              :expression [:= [:read :request/status] [:literal :open]]}
      :reads #{:request/status :input/actor}
      :writes #{:request/status :request/owner}
      :replacements {:request/status [:literal :claimed]
                     :request/owner [:read :input/actor]}
      :frame #{:request/created-at}}

   :writes MUST equal all replacement keys, :frame MUST equal the exact
   complement of :writes in the declared state projection, and :reads MUST
   equal the exact guard/assignment read footprint. Only the original
   v1233 guarded transition checker validates the syntax and semantics of
   guards, types, and replacement expressions. This constructor does not
   introduce a second interpretation, arbitrary callbacks, deletes, dynamic
   writes, implicit effects, or unchecked extra keys. All nonwritten fields
   preserve both presence and value in the abstract relation.

   build-guarded-replacement! returns COPYABLE, UNADMITTED DATA containing
   original v1233 transition source for subsequent proof derivations. The
   projection is NOT an assertion that other application/database fields are
   unchanged; field coverage is restricted to the named state-types. A
   constructor success is NOT a proof, runtime guard, authorization, complete
   physical effect footprint, XTDB commit, or implementation correspondence.
   All of those remain external obligations."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.transition :as abstract-transition]))

(def transition-version 1)
(def construction-type :gesso.assurance.transition/unadmitted-guarded-replacement)

(def ^:private required-keys
  #{:subject :state-types :input-types :guard :reads :writes
    :replacements :frame})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso guarded replacement construction."
                  (merge {:error/type ::invalid-construction
                          :reason reason
                          :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x)
             (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value expected at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= actual expected)
      (reject! :wrong-keys at
               {:missing (set/difference expected actual)
                :unexpected (set/difference actual expected)})))
  value)

(defn- qualified-field? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- field-set! [value at]
  (when-not (plain-set? value)
    (reject! :expected-plain-field-set at {}))
  (metadata-free! value at)
  (doseq [field value]
    (when-not (qualified-field? field)
      (reject! :invalid-field at {:field field})))
  value)

(defn- replacement-keys! [replacements]
  (when-not (plain-map? replacements)
    (reject! :expected-replacement-map [:replacements] {}))
  (metadata-free! replacements [:replacements])
  (doseq [field (keys replacements)]
    (when-not (qualified-field? field)
      (reject! :invalid-field [:replacements field] {:field field})))
  (set (keys replacements)))

(defn- checked-original-source!
  [construction]
  (exact-map! construction required-keys [])
  (let [{:keys [subject state-types input-types guard reads writes
                replacements frame]} construction]
    (when-not (plain-map? state-types)
      (reject! :expected-state-type-map [:state-types] {}))
    (metadata-free! state-types [:state-types])
    (field-set! reads [:reads])
    (field-set! writes [:writes])
    (field-set! frame [:frame])
    (let [state-fields (set (keys state-types))
          replacement-fields (replacement-keys! replacements)]
      (when-not (set/subset? writes state-fields)
        (reject! :writes-outside-state [:writes]
                 {:unexpected (set/difference writes state-fields)}))
      (when-not (= writes replacement-fields)
        (reject! :write-footprint-mismatch [:writes]
                 {:missing (set/difference replacement-fields writes)
                  :unexpected (set/difference writes replacement-fields)}))
      (let [required-frame (set/difference state-fields writes)]
        (when-not (= frame required-frame)
          (reject! :frame-footprint-mismatch [:frame]
                   {:missing (set/difference required-frame frame)
                    :unexpected (set/difference frame required-frame)}))))
    ;; This is the ORIGINAL v1233 declaration, not a new semantic language.
    ;; v1233 checks exact reads, original guard, typed assignments, state and
    ;; input domain, and rejects unknown instructions or executable payloads.
    {:subject subject
     :state-types state-types
     :input-types input-types
     :guard guard
     :reads reads
     :writes writes
     :assignments replacements}))

(defn build-guarded-replacement!
  "Check one explicitly framed, typed guarded replacement and return its
   original v1233 abstract-transition declaration. No report provided by the
   caller is accepted as evidence. Callers must pass the SOURCE from this
   report to the independent v1235 derivation checker and separately satisfy
   runtime/implementation obligations."
  [construction]
  (let [source (checked-original-source! construction)
        description (abstract-transition/describe-transition! source)
        semantics (:semantics description)]
    ;; Deliberately defensive: the older interpreter remains the semantic
    ;; authority. Never silently take a weaker or incomplete frame from it.
    (when-not (and (= (:reads source) (:reads semantics))
                   (= (:writes source) (:writes semantics))
                   (= (:frame construction) (:frame-fields semantics)))
      (reject! :abstract-semantics-disagree [] {}))
    {:type construction-type
     :version transition-version
     :transition source
     :transition-fingerprint (:fingerprint description)
     :subject-fingerprint (:subject-fingerprint description)
     :reads (:reads semantics)
     :writes (:writes semantics)
     :frame (:frame-fields semantics)
     :state-projection (set (keys (:state-types semantics)))
     :checking :closed-source-and-typed-abstract-relation
     :construction-scope :declared-abstract-state-projection-only
     :open-obligations
     #{:initial-state-invariant
       :abstract-invariant-preservation
       :production-transition-correspondence
       :current-runtime-authorization
       :atomic-transaction-enforcement
       :physical-effect-containment}
     :proof-status :not-proved
     :certificate :not-issued
     :admission :not-admitted
     :runtime-enforcement :not-established
     :implementation-correspondence :not-established
     :authority :none}))

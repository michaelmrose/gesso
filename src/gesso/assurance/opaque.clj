(ns gesso.assurance.opaque
  "Explicit, UNADMITTED contracts for opaque computations in Gesso R4 (v1227).

   An opaque computation may call arbitrary Clojure code or an external service.
   This namespace NEVER executes that computation. It describes the boundary and
   checks caller-supplied input/output binding MAPS with the SAME typed v1219-
   v1225 rule language used elsewhere in Gesso assurance.

   Contract declaration (no executable functions or cached reports allowed):

   {:id :example/compute
    :implementation :opaque-clojure   ; or :external-service
    :implementation-boundary :example/calculate
    :input {:rule {:readers #{:input/id}
                   :expression [:> [:read :input/id] [:literal 0]]}
            :reader-types {:input/id {:type :integer}}}
    :output {:rule {:readers #{:result/status :result/score}
                    :expression
                    [:and
                     [:in [:read :result/status] [:literal #{:new :active}]]
                     [:> [:read :result/score] [:literal 7]]]}
             :reader-types
             {:result/status {:type :enum :name :example/status
                              :values #{:new :active}}
              :result/score {:type :integer}}}
    :effects {:mode :unknown :external-effects #{}}
    :termination {:mode :unknown :timeout-ms nil}}

   Effects modes are :unknown, :assumed-pure, and :declared-external. All
   effects descriptions are APPLICATION ASSERTIONS, not confinement evidence.
   Declared external effects do not establish completeness; hidden effects and
   authoritative mutations remain possible unless separately constrained.

   Termination modes are :unknown, :assumed-terminates, :requested-timeout.
   A requested timeout is a policy/intention, never a guarantee that a JVM
   computation, blocking I/O, or external service is preemptible.

   Output validation establishes only that the specific supplied binding map
   satisfies the typed rule at the time of this CHECK. It does NOT establish
   source provenance, result correctness, effect containment, proof, a commit
   guard, a deployed handler's correspondence, or authoritative admission.
   A caller must enforce this check at the proper transaction boundary; no
   general runtime enforcement is installed by this namespace."
  (:require [clojure.set :as set]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.rule-normalize :as rule-normalize]
            [gesso.assurance.subject :as subject]))

(def contract-version 1)
(def description-type :gesso.assurance.opaque/unadmitted-contract)
(def observation-type :gesso.assurance.opaque/interpreted-observation)
(def implementation-kinds #{:opaque-clojure :external-service})
(def effect-modes #{:unknown :assumed-pure :declared-external})
(def termination-modes #{:unknown :assumed-terminates :requested-timeout})

(def ^:private contract-keys
  #{:id :implementation :implementation-boundary :input :output
    :effects :termination})
(def ^:private interface-keys #{:rule :reader-types})
(def ^:private effect-keys #{:mode :external-effects})
(def ^:private termination-keys #{:mode :timeout-ms})
(def ^:private max-effects 64)
(def ^:private max-timeout-ms 86400000)

(defn- reject! [reason at data]
  (throw (ex-info "Invalid Gesso opaque computation contract."
                  (merge {:error/type ::invalid-opaque-contract
                          :reason reason :at at} data))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-set? [x]
  (instance? clojure.lang.PersistentHashSet x))

(defn- qualified-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- metadata-free! [x at]
  (when (and (instance? clojure.lang.IObj x) (some? (meta x)))
    (reject! :metadata-not-allowed at {})))

(defn- exact-map! [value expected at]
  (when-not (plain-map? value)
    (reject! :expected-plain-map at {}))
  (metadata-free! value at)
  (let [actual (set (keys value))]
    (when-not (= actual expected)
      (reject! :wrong-keys at {:missing (set/difference expected actual)
                               :unexpected (set/difference actual expected)})))
  value)

(defn- interface! [interface at]
  (exact-map! interface interface-keys at)
  ;; Never trust reports from previous phases. This rechecks original source
  ;; syntax, nominal types and interpreter/normalization semantics each time.
  (let [{:keys [rule reader-types]} interface]
    (rule-normalize/normalize-rule! rule reader-types)))

(defn- effects! [value]
  (exact-map! value effect-keys [:effects])
  (let [{:keys [mode external-effects]} value]
    (when-not (contains? effect-modes mode)
      (reject! :unknown-effect-mode [:effects :mode] {:mode mode}))
    (when-not (and (plain-set? external-effects)
                   (<= (count external-effects) max-effects))
      (reject! :invalid-external-effects [:effects :external-effects] {}))
    (metadata-free! external-effects [:effects :external-effects])
    (when-not (every? qualified-keyword? external-effects)
      (reject! :invalid-effect-id [:effects :external-effects] {}))
    (when (not= (= mode :declared-external) (boolean (seq external-effects)))
      ;; In :declared-external mode at least one disclosed effect is necessary;
      ;; in other modes the effect set must be empty (including unknown mode).
      (reject! :effect-mode-footprint-mismatch [:effects]
               {:mode mode}))
    value))

(defn- termination! [value]
  (exact-map! value termination-keys [:termination])
  (let [{:keys [mode timeout-ms]} value]
    (when-not (contains? termination-modes mode)
      (reject! :unknown-termination-mode [:termination :mode] {:mode mode}))
    (if (= mode :requested-timeout)
      (when-not (and (integer? timeout-ms)
                     (pos? timeout-ms)
                     (<= timeout-ms max-timeout-ms))
        (reject! :invalid-requested-timeout [:termination :timeout-ms]
                 {:maximum max-timeout-ms}))
      (when-not (nil? timeout-ms)
        (reject! :timeout-without-request [:termination :timeout-ms] {})))
    value))

(defn- validate-contract! [contract]
  (exact-map! contract contract-keys [])
  (when-not (qualified-keyword? (:id contract))
    (reject! :invalid-computation-id [:id] {}))
  (when-not (contains? implementation-kinds (:implementation contract))
    (reject! :unknown-implementation [:implementation] {}))
  (when-not (qualified-keyword? (:implementation-boundary contract))
    (reject! :invalid-implementation-boundary [:implementation-boundary] {}))
  (let [input (interface! (:input contract) [:input])
        output (interface! (:output contract) [:output])]
    (effects! (:effects contract))
    (termination! (:termination contract))
    {:input input :output output}))

(defn- obligations [contract]
  ;; These are NOT v1213 discharged premise objects. These explicit symbolic
  ;; obligations can later be turned into subject-bound v1213 assumptions by
  ;; a proof admission layer; merely listing them does not discharge them.
  (cond-> #{:opaque/source-provenance
            :opaque/business-correctness
            :opaque/effects-and-isolation
            :opaque/termination-and-cancellation
            :opaque/authoritative-enforcement
            :opaque/runtime-correspondence}
    (= :declared-external (get-in contract [:effects :mode]))
    (conj :opaque/external-effect-completeness)
    (= :requested-timeout (get-in contract [:termination :mode]))
    (conj :opaque/timeout-enforcement)))

(defn describe-contract
  "Validate a pure, closed opaque contract and compute its stable structural
   subject identity. The name of the callback/service is a reference only;
   no callback object, executable payload, asserted proof, or cached rule
   report is accepted. All open obligations remain explicit."
  [contract]
  (let [{:keys [input output]} (validate-contract! contract)
        identity (subject/describe-subject
                  {:kind :operation
                   :id (:id contract)
                   :interface
                   {:contract-version contract-version
                    :implementation (:implementation contract)
                    :implementation-boundary (:implementation-boundary contract)
                    :input (:semantics input)
                    :output (:semantics output)
                    :effects (:effects contract)
                    :termination (:termination contract)}})]
    {:type description-type
     :version contract-version
     :fingerprint (:fingerprint identity)
     :input-rule-fingerprint (:fingerprint input)
     :output-rule-fingerprint (:fingerprint output)
     :effects (:effects contract)
     :termination (:termination contract)
     :open-obligations (obligations contract)
     :effect-assurance :not-established
     :termination-assurance :not-established
     :checking :syntax-and-types
     :evaluation :not-performed
     :proof-status :not-proved
     :admission :not-admitted
     :runtime-enforcement :not-established
     :authority :none}))

(defn- validate-boundary! [contract boundary bindings]
  (let [{:keys [input output]} (validate-contract! contract)
        interface (case boundary :input (:input contract) :output (:output contract))
        normalized (case boundary :input input :output output)
        observed (rule-eval/evaluate-rule!
                  (:rule interface) (:reader-types interface) bindings)]
    (when-not (true? (:value observed))
      (reject! (case boundary
                 :input :input-contract-rejected
                 :output :output-contract-rejected)
               [boundary] {:computation (:id contract)}))
    ;; Descriptions are computed afresh. Their identities bind the check to
    ;; the declared source, NOT to deployed code or the supplied value's origin.
    (let [description (describe-contract contract)]
      {:type observation-type
       :version contract-version
       :boundary boundary
       :computation (:id contract)
       :contract-fingerprint (:fingerprint description)
       :rule-fingerprint (:fingerprint normalized)
       :observation :predicate-satisfied-for-supplied-bindings
       :effect-assurance :not-established
       :termination-assurance :not-established
       :open-obligations (:open-obligations description)
       :proof-status :not-proved
       :admission :not-admitted
       :runtime-enforcement :not-established
       :authority :none})))

(defn validate-input!
  "Check an externally SUPPLIED, exact input binding map. A successful return
   establishes only this interpreted observation. It does not run the opaque
   computation or confer execution/commit authority. Invalid bindings throw."
  [contract bindings]
  (validate-boundary! contract :input bindings))

(defn validate-result!
  "Check an externally SUPPLIED, exact result binding map against declared
   types and predicate. Invalid results throw typed exceptions. The caller
   remains responsible for enforcing this validation before an authoritative
   commit and for isolating external and opaque effects. No guarantee about
   whether the returned value is business-correct follows from this check."
  [contract bindings]
  (validate-boundary! contract :output bindings))

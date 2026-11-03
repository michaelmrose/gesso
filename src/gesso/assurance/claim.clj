(ns gesso.assurance.claim
  "Closed, descriptive vocabulary for R4 assurance claims (v1209).

   A claim description records WHAT is being claimed, over WHICH semantic
   subject and scope, and WHICH CLASS of evidence is alleged to support it.
   Descriptions are RAW DATA, not admitted evidence or certificates. In
   particular, supplying :kernel-checked, a proof reference, or a copied
   description never proves a proposition or grants application authority.

   Later layers own normalized subject identity, named assumptions, admission,
   dependency closure, proof checking, and implementation correspondence.
   Nothing in this namespace discharges those obligations."
  (:require [clojure.set :as set]))

(def description-type :gesso.assurance.claim/unadmitted-description)
(def description-version 1)

;; These are distinct evidence classes, not a numerical ranking.  A finite
;; exploration, assembly check, runtime boundary, test, or assertion never
;; becomes a proof merely because several of them are combined.
(def evidence-class-descriptions
  {:kernel-checked
   "A derivation is claimed to have been checked by a specified proof kernel; the checker result must be independently admitted."
   :exact-finite-checked
   "All states of an explicitly finite, closed semantic subject are claimed to have been checked; no unbounded theorem follows."
   :bounded-model-checked
   "A bounded state/configuration space is claimed to have been checked; behavior outside its bounds remains open."
   :assembly-verified
   "A specific assembled application's declared structural closure is claimed to have been checked; deployed runtime adoption is separate."
   :runtime-enforced
   "A concrete trusted execution boundary is claimed to enforce the premise; this is not an abstract proof of that boundary."
   :trusted
   "A fact is supplied as an external or implementation assumption; it is not mechanically discharged."
   :application-asserted
   "A proposition is supplied by the application author without derivation; stated intent is not established correctness."
   :tested
   "Observed executions are claimed to satisfy the proposition in named scenarios; unobserved executions are not covered."
   :unknown
   "No adequate supporting evidence has been established for this proposition."})

(def evidence-classes (set (keys evidence-class-descriptions)))

;; A *proof conditional on assumptions* is still a kernel-checked proof, not a
;; competing evidence class. Conditionality belongs to the proposition's named
;; premises; it is never promoted away by classification alone.
(def scope-kinds
  #{:schema :domain :relationship :rule :operation :choreography
    :projection :executable-plan :application :implementation
    :runtime :environment :protocol})

(def ^:private required-description-keys
  #{:subject :proposition :scope :evidence :assumptions
    :open-obligations :dependencies :implementation-boundary})
(def ^:private required-scope-keys #{:kind :id})
(def ^:private required-evidence-keys #{:class :reference})

(defn evidence-class?
  "Recognize only R4 evidence-class vocabulary. This does NOT validate evidence."
  [value]
  (contains? evidence-classes value))

(defn- qualified-keyword?
  [value]
  (and (keyword? value) (some? (namespace value))))

(defn- claim-error!
  [reason data]
  (throw (ex-info "Invalid unadmitted Gesso assurance claim description."
                  (merge {:error/type ::invalid-claim-description
                          :reason reason}
                         data))))

(defn- exact-keys!
  [value required where]
  (when-not (map? value)
    (claim-error! :expected-map {:at where :value value}))
  (let [actual (set (keys value))]
    (when-not (= actual required)
      (claim-error! :wrong-keys
                    {:at where
                     :missing (set/difference required actual)
                     :unexpected (set/difference actual required)})))
  value)

(defn- named-set!
  [value where]
  (when-not (and (set? value) (every? qualified-keyword? value))
    (claim-error! :expected-named-set {:at where :value value})))

(defn- scope!
  [scope]
  (exact-keys! scope required-scope-keys :scope)
  (when-not (contains? scope-kinds (:kind scope))
    (claim-error! :unknown-scope-kind {:value (:kind scope)}))
  (when-not (qualified-keyword? (:id scope))
    (claim-error! :invalid-scope-id {:value (:id scope)})))

(defn- evidence!
  [evidence]
  (exact-keys! evidence required-evidence-keys :evidence)
  (when-not (evidence-class? (:class evidence))
    (claim-error! :unknown-evidence-class {:value (:class evidence)}))
  ;; A reference here is a *claim about where evidence could be found*, not
  ;; verified provenance. v1215+ will admit references after real checking.
  (when-not (or (nil? (:reference evidence))
                (qualified-keyword? (:reference evidence)))
    (claim-error! :invalid-evidence-reference
                  {:value (:reference evidence)}))
  (when (and (= :unknown (:class evidence))
             (some? (:reference evidence)))
    (claim-error! :unknown-with-evidence-reference
                  {:value (:reference evidence)})))

(defn describe-claim
  "Validate and describe a RAW assurance assertion, without certifying it.

   Required input is a closed map:
   {:subject :example/claim
    :proposition [:example/preserves-ownership]
    :scope {:kind :operation :id :example/claim}
    :evidence {:class :tested :reference :test/claim-scenario}
    :assumptions #{}
    :open-obligations #{}
    :dependencies #{}
    :implementation-boundary nil}

   :proposition is deliberately only a symbolic placeholder at this stage;
   semantic proposition/type checking is not implemented until later revisions.
   :assumptions, :open-obligations and :dependencies are explicit rather than
   silently defaulted. An opaque Clojure function cannot supply such a proof.

   The returned :type is intentionally 'unadmitted-description'. Never use
   this function or its returned map as an authorization, proof, or certificate
   recognition boundary."
  [claim]
  (exact-keys! claim required-description-keys :claim)
  (when-not (qualified-keyword? (:subject claim))
    (claim-error! :invalid-subject {:value (:subject claim)}))
  (when-not (and (vector? (:proposition claim))
                 (seq (:proposition claim))
                 (qualified-keyword? (first (:proposition claim))))
    (claim-error! :invalid-proposition-placeholder
                  {:value (:proposition claim)}))
  (scope! (:scope claim))
  (evidence! (:evidence claim))
  (doseq [field [:assumptions :open-obligations :dependencies]]
    (named-set! (get claim field) field))
  (when-not (or (nil? (:implementation-boundary claim))
                (qualified-keyword? (:implementation-boundary claim)))
    (claim-error! :invalid-implementation-boundary
                  {:value (:implementation-boundary claim)}))
  {:type description-type
   :version description-version
   :claim claim
   :conditional? (boolean (seq (:assumptions claim)))
   :open? (boolean (seq (:open-obligations claim)))
   :checking :not-performed
   :admission :not-admitted})

(defn describe-evidence-class
  "Explain an evidence class. Explanations never authenticate its use."
  [class]
  (if-let [description (get evidence-class-descriptions class)]
    {:class class :description description}
    (claim-error! :unknown-evidence-class {:value class})))

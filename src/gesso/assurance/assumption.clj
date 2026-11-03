(ns gesso.assurance.assumption
  "Named, subject-bound, *unadmitted* assumptions for Gesso R4 (v1213).

   An assumption is a scoped proposition with an exact subject interface,
   parameters, semantic context, dependencies and an implementation boundary.
   Distinct propositions with the same friendly name MUST NOT merge. An opaque
   Clojure callback is described, never evaluated or proved by this namespace.

   This module intentionally cannot discharge an assumption. A matching EDN
   assertion is NOT evidence. The eventual admitted checker (v1215+) must
   authenticate a successful, appropriately scoped derivation and preserve its
   own assumptions/dependencies before emitting any discharged judgment.

   Runtime/current-principal, external API, and environment assumptions cannot
   be made true by an old or copied certificate. Applicability to an actual
   invocation must be re-established at the appropriate implementation boundary."
  (:require [clojure.set :as set]
            [gesso.assurance.claim :as claim]
            [gesso.assurance.subject :as subject]))

(def description-type :gesso.assurance.assumption/unadmitted-description)
(def description-version 1)

(def assumption-origins
  "Sources of opaque or not-yet-proved obligations; these are not evidence."
  #{:opaque-clojure :external-service :application-policy
    :environment :runtime :implementation-correspondence
    :distributed-protocol :other})

(def discharge-states
  "A vocabulary for subsequent admitted judgments. v1213 only EMITS :open.
   Trusting a premise is NOT discharging it; the trust must propagate instead."
  #{:open :discharged})

(def ^:private required-keys
  #{:id :subject :scope :proposition :parameters :semantic-context
    :dependencies :origin :implementation-boundary})
(def ^:private required-scope-keys #{:kind :id})

(defn- qualified-keyword? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- assumption-error!
  [reason data]
  (throw (ex-info "Invalid unadmitted Gesso assurance assumption."
                  (merge {:error/type ::invalid-assumption
                          :reason reason}
                         data))))

(defn- exact-keys!
  [value required at]
  (when-not (and (map? value) (not (record? value)))
    (assumption-error! :expected-map {:at at}))
  (let [actual (set (keys value))]
    (when-not (= required actual)
      (assumption-error!
       :wrong-keys
       {:at at
        :missing (set/difference required actual)
        :unexpected (set/difference actual required)})))
  value)

(defn- validate-scope!
  [scope]
  (exact-keys! scope required-scope-keys :scope)
  (when-not (contains? claim/scope-kinds (:kind scope))
    (assumption-error! :unknown-scope-kind {:at :scope :kind (:kind scope)}))
  (when-not (qualified-keyword? (:id scope))
    (assumption-error! :invalid-scope-id {:at :scope :value (:id scope)}))
  scope)

(defn- validate-proposition!
  [proposition]
  ;; Formula typing and evaluation are deliberately postponed to v1219+.
  ;; A symbolic vector with a qualified leading operator is only a declaration.
  (when-not (and (vector? proposition)
                 (seq proposition)
                 (qualified-keyword? (first proposition)))
    (assumption-error! :invalid-proposition {:at :proposition}))
  proposition)

(defn- validate-subject!
  [value]
  ;; The v1211 encoder validates pure/finite data, refusing callbacks, Java
  ;; objects, normalization collisions and caller-supplied fingerprint flags.
  (when-not (and (map? value) (not (record? value)))
    (assumption-error! :invalid-subject {:at :subject}))
  (subject/describe-subject value))

(defn- validate-assumption!
  [value]
  (exact-keys! value required-keys :assumption)
  (when-not (qualified-keyword? (:id value))
    (assumption-error! :invalid-id {:at :id :value (:id value)}))
  (let [identity (validate-subject! (:subject value))]
    (validate-scope! (:scope value))
    (validate-proposition! (:proposition value))
    (doseq [field [:parameters :semantic-context]]
      (when-not (and (map? (get value field))
                     (not (record? (get value field))))
        (assumption-error! :expected-map {:at field})))
    (when-not (and (set? (:dependencies value))
                   (every? qualified-keyword? (:dependencies value)))
      (assumption-error! :invalid-dependencies {:at :dependencies}))
    (when-not (contains? assumption-origins (:origin value))
      (assumption-error! :unknown-origin {:at :origin :value (:origin value)}))
    (when-not (or (nil? (:implementation-boundary value))
                  (qualified-keyword? (:implementation-boundary value)))
      (assumption-error! :invalid-implementation-boundary
                         {:at :implementation-boundary}))
    identity))

(defn- premise-subject
  "Build a strictly structural identity over ALL relevant premise information.
   This does not assert that two syntactically equal propositions are true."
  [value subject-fingerprint]
  {:kind :rule
   :id (:id value)
   :interface
   {:subject (:subject value)
    :subject-fingerprint subject-fingerprint
    :scope (:scope value)
    :proposition (:proposition value)
    :parameters (:parameters value)
    :semantic-context (:semantic-context value)
    :dependencies (:dependencies value)
    :origin (:origin value)
    :implementation-boundary (:implementation-boundary value)}})

(defn describe-assumption
  "Validate a named opaque/conditional premise and give it a structural ID.

   Input must be EXACTLY:
   {:id :claim/current-membership
    :subject {:kind :operation :id :claim/claim
              :interface {:reads #{:membership/status}}}
    :scope {:kind :operation :id :claim/claim}
    :proposition [:claim/current-membership-authoritative]
    :parameters {}
    :semantic-context {}
    :dependencies #{}
    :origin :runtime
    :implementation-boundary :xtdb/commit-time-read}

   A changed subject, scoped proposition, parameter, context, dependency,
   origin or boundary changes the premise identity. Both identities remain
   UNADMITTED. Input cannot contain :discharge, :discharged?, :valid? or
   :certificate. In particular :discharge-state is ALWAYS :open.

   No theorem, trust grant, current-principal decision, or runtime capability
   follows from this description."
  [value]
  (let [subject-identity (validate-assumption! value)
        premise-identity (subject/describe-subject
                          (premise-subject value (:fingerprint subject-identity)))]
    {:type description-type
     :version description-version
     :assumption value
     :subject-fingerprint (:fingerprint subject-identity)
     :premise-fingerprint (:fingerprint premise-identity)
     :discharge-state :open
     :checking :not-performed
     :admission :not-admitted}))

(defn match-premise!
  "Check only STRUCTURAL identity of required and offered premises.

   Rejects mismatched name, subject, scope, proposition, context, parameters,
   dependencies, origin or boundary. This is a PREREQUISITE to future proof
   discharge, not a discharge! A copied declaration can match itself without
   containing any accepted evidence.

   Both arguments must be raw assumption declarations, not cached descriptions
   or purported accepted verdicts. Later proof checking must authenticate the
   offered derivation and perform any separately specified substitution rules."
  [required offered]
  (let [a (describe-assumption required)
        b (describe-assumption offered)]
    (when-not (= (:scope required) (:scope offered))
      (assumption-error! :scope-mismatch
                         {:required (:scope required)
                          :offered (:scope offered)}))
    (when-not (= (:subject-fingerprint a) (:subject-fingerprint b))
      (assumption-error! :subject-mismatch
                         {:required (:subject-fingerprint a)
                          :offered (:subject-fingerprint b)}))
    (when-not (= (:premise-fingerprint a) (:premise-fingerprint b))
      (assumption-error! :premise-mismatch
                         {:required (:premise-fingerprint a)
                          :offered (:premise-fingerprint b)}))
    {:match :structural-only
     :required-premise (:premise-fingerprint a)
     :offered-premise (:premise-fingerprint b)
     :discharge-state :open
     :checking :not-performed
     :admission :not-admitted}))

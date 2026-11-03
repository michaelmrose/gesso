(ns gesso.assurance.admission
  "In-process, checker-backed *structural* admission handles (v1215).

   The only checks available in this revision are (1) canonical declarative
   subject identity and (2) exact structural matching of named assumptions.
   Neither establishes a business proposition, discharges an assumption,
   verifies executable code, or grants a production authorization capability.

   Admission is a two-part property: a concrete object created by this module
   AND an identity-keyed, private live registry entry written *only after* the
   corresponding check succeeded. A user-made deftype instance, copied map,
   deserialized description, fingerprint, or alleged :valid? verdict does not
   become admitted. There is no public accept-checker-result / register-checker
   API: caller-supplied success functions would be unsound before the proof
   kernel exists. Later kernel/certificate admission must be added explicitly
   and must never promote these structural handles into proof authority.

   Boundary: this guards against forged data and accidental misuse through the
   documented API in one trusted JVM. Clojure reflection, mutated Vars or
   arbitrary code executing with equivalent JVM privileges are NOT adversaries
   this API can exclude. The handles are neither durable nor transferable."
  (:require [gesso.assurance.assumption :as assumption]
            [gesso.assurance.subject :as subject])
  (:import (java.util WeakHashMap)))

(def admission-version 1)
(def admission-type :gesso.assurance.admission/structural-handle)
(def admitted-kinds
  "Checks which can currently create a live handle. Neither is a proof."
  #{:subject-identity :premise-structural-match})

;; A deftype has Object identity equality, unlike a defrecord or EDN map.
;; A caller can construct the class but CANNOT insert it into the private
;; registry via the public API. Weak keys avoid indefinite retention of handles.
(deftype AdmissionHandle []
  Object
  (toString [_] "#<gesso.assurance/admission-handle>"))

(def ^:private ^WeakHashMap live-handles (WeakHashMap.))

(defn- admission-error!
  [reason data]
  (throw (ex-info "Invalid Gesso assurance admission handle."
                  (merge {:error/type ::invalid-admission :reason reason}
                         data))))

(defn- registered-entry
  [handle]
  (when (instance? AdmissionHandle handle)
    (locking live-handles
      (.get live-handles handle))))

(defn admitted?
  "True only for a live checker-issued handle in THIS runtime.

   Note that this returns true for STRUCTURAL checks only. It does NOT mean
   that a theorem, implementation, dependency, or assumption is verified."
  [handle]
  (boolean (registered-entry handle)))

(defn- issue-structural-handle!
  "Internal issuer. Call only AFTER a built-in checker has returned without
   error; never pass caller-supplied verdicts into this function."
  [entry]
  (when-not (and (map? entry)
                 (contains? admitted-kinds (:kind entry))
                 (string? (:subject-fingerprint entry)))
    (admission-error! :internal-invalid-entry {}))
  (let [handle (AdmissionHandle.)]
    (locking live-handles
      (.put live-handles handle entry))
    handle))

(defn admit-subject!
  "Check a RAW declarative subject with the v1211 canonical validator and
   return a live handle attesting to that exact structural interface identity.

   A caller may describe any well-formed subject. This does not prove that it
   is an installed operation, correctly implemented, or semantically valid.
   The later assembly verifier must bind that subject to deployed source."
  [raw-subject]
  (let [identity (subject/describe-subject raw-subject)]
    (issue-structural-handle!
     {:kind :subject-identity
      :subject-fingerprint (:fingerprint identity)
      :subject-kind (:kind raw-subject)
      :subject-id (:id raw-subject)})))

(defn admit-premise-match!
  "Check exact structural match of two RAW assumption declarations and issue
   an identity-bound handle. Matching a premise is NOT discharging it.

   The v1213 assumption checker recomputes subject and premise fingerprints,
   and explicitly rejects wrong scope, subject, proposition or context."
  [required offered]
  (let [result (assumption/match-premise! required offered)]
    (when-not (and (= :structural-only (:match result))
                   (= :not-admitted (:admission result))
                   (= :open (:discharge-state result)))
      (admission-error! :internal-unsafe-premise-check {}))
    (let [identity (subject/describe-subject (:subject required))]
      (issue-structural-handle!
       {:kind :premise-structural-match
        :subject-fingerprint (:fingerprint identity)
        :subject-kind (get-in required [:subject :kind])
        :subject-id (get-in required [:subject :id])
        :premise-fingerprint (:required-premise result)}))))

(defn describe-handle
  "Return a COPYABLE, NON-AUTHORITATIVE summary of a live handle.
   Deserializing this map cannot reproduce the handle or its registry entry."
  [handle]
  (let [entry (registered-entry handle)]
    (when-not entry
      (admission-error! :unrecognized-handle {}))
    (assoc entry
           :type admission-type
           :version admission-version
           :checking :structural-only
           :proof-status :not-proved
           :discharge-state :open
           :implementation-correspondence :not-established
           :authority :none)))

(defn require-admitted-subject!
  "Authenticate a live SUBJECT-IDENTITY handle for an expected raw subject.
   Returns the handle itself, never a transferable verdict or certificate.

   The expected subject must come from the consumer's OWN trusted assembly
   input; using a subject chosen by an untrusted claimant proves nothing about
   the installed application. This function intentionally rejects handles for
   other check kinds even if the subject happens to have the same fingerprint."
  [handle expected-subject]
  (let [entry (registered-entry handle)]
    (when-not entry
      (admission-error! :unrecognized-handle {}))
    (when-not (= :subject-identity (:kind entry))
      (admission-error! :wrong-check-kind
                        {:expected :subject-identity :actual (:kind entry)}))
    (let [identity (subject/describe-subject expected-subject)]
      (when-not (and (= (:fingerprint identity)
                        (:subject-fingerprint entry))
                     (= (:kind expected-subject) (:subject-kind entry))
                     (= (:id expected-subject) (:subject-id entry)))
        (admission-error! :wrong-subject
                          {:expected (:fingerprint identity)
                           :actual (:subject-fingerprint entry)})))
    handle))

(defn require-admitted-premise-match!
  "Authenticate a live premise-match handle against the exact required AND
   offered declarations. Independently repeat the structural check, so a
   subject-identity handle or a match for a different premise cannot substitute.
   Successful return says ONLY that both declarations match structurally."
  [handle required offered]
  (let [entry (registered-entry handle)]
    (when-not entry
      (admission-error! :unrecognized-handle {}))
    (when-not (= :premise-structural-match (:kind entry))
      (admission-error! :wrong-check-kind
                        {:expected :premise-structural-match
                         :actual (:kind entry)}))
    (let [result (assumption/match-premise! required offered)
          identity (subject/describe-subject (:subject required))]
      (when-not (and (= (:premise-fingerprint entry)
                        (:required-premise result))
                     (= (:premise-fingerprint entry)
                        (:offered-premise result))
                     (= (:subject-fingerprint entry)
                        (:fingerprint identity))
                     (= (:subject-id entry)
                        (get-in required [:subject :id]))
                     (= (:subject-kind entry)
                        (get-in required [:subject :kind])))
        (admission-error! :wrong-premise
                          {:expected (:premise-fingerprint entry)
                           :actual (:required-premise result)})))
    handle))

(ns gesso.assurance.subject
  "Canonical *structural* identity for declarative R4 semantic subjects (v1211).

   A subject names one semantic interface, not an executing Clojure function:
     {:kind :operation
      :id :example/claim
      :interface {:inputs {:actor :identity/user}
                  :guard [:and [:equals [:field :owner] nil]]
                  :effects [[:set :owner [:input :actor]]]}}

   Accepted interfaces are pure, bounded EDN-like values. The versioned encoder
   distinguishes data types and preserves vector order and meaningful string
   whitespace; maps and sets are order-independent. EDN reader whitespace and
   source metadata are deliberately not identity-bearing. Integer literals are
   interpreted as mathematical integers (1 and 1N have identical identity),
   not Java/Clojure arithmetic execution promises. This is a STRUCTURAL digest,
   not a claim of denotational equivalence or a proof of implementation fidelity.

   Identities are descriptive and UNADMITTED. No caller-supplied fingerprint,
   proof flag, or certificate enters this API. Later admission/checking layers
   must bind checked evidence to exact semantic and implementation subjects."
  (:require [clojure.set :as set]
            [gesso.assurance.claim :as claim])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def identity-type :gesso.assurance.subject/unadmitted-identity)
(def identity-version 1)
(def canonical-format :gesso.assurance.subject/canonical-v1)

(def ^:private required-keys #{:kind :id :interface})
(def ^:private max-depth 64)
(def ^:private max-nodes 100000)
(def ^:private max-canonical-chars 2000000)

(defn- subject-error!
  [reason data]
  (throw (ex-info "Invalid unadmitted Gesso semantic subject."
                  (merge {:error/type ::invalid-semantic-subject
                          :reason reason}
                         data))))

(defn- qualified-keyword?
  [x]
  (and (keyword? x) (some? (namespace x))))

(defn- hex-unit
  [c]
  (format "%04x" (int c)))

(defn- text-token
  "A reversible, length-delimited UTF-16 encoding, including lone surrogates.
   The output uses ASCII only, so digest encoding cannot replace characters."
  [^String value]
  (str "s" (.length value) ":" (apply str (map hex-unit value)) ";"))

(defn- encode-value
  "Encode a narrowly defined EDN value; never invoke application callbacks.
   An atom budget and depth bound prevent unbounded accidental structures."
  [value path depth budget]
  (when (> depth max-depth)
    (subject-error! :depth-limit {:at path :maximum max-depth}))
  (when (> (swap! budget inc) max-nodes)
    (subject-error! :size-limit {:at path :maximum max-nodes}))
  (cond
    (nil? value) "n;"
    (true? value) "b1;"
    (false? value) "b0;"
    (integer? value) (str "i" (bigint value) ";")
    (string? value) (str "S" (text-token value))
    (keyword? value)
    (str "K" (if-let [ns (namespace value)] (text-token ns) "n;")
         (text-token (name value)))
    (vector? value)
    (str "V" (count value) "["
         (apply str (map-indexed
                     (fn [index item]
                       (encode-value item (conj path index) (inc depth) budget))
                     value))
         "]")
    (set? value)
    (let [items (mapv #(encode-value % (conj path :set-element) (inc depth) budget)
                      value)
          ordered (sort items)]
      (when-not (= (count items) (count (distinct ordered)))
        (subject-error! :normalized-collision {:at path :collection :set}))
      (str "T" (count items) "[" (apply str ordered) "]"))
    (and (map? value) (not (record? value)))
    (let [items (mapv (fn [[k v]]
                        [(encode-value k (conj path :map-key) (inc depth) budget)
                         (encode-value v (conj path k) (inc depth) budget)])
                      value)
          ordered (sort-by first items)
          keys* (map first ordered)]
      (when-not (= (count items) (count (distinct keys*)))
        (subject-error! :normalized-collision {:at path :collection :map}))
      (str "M" (count items) "["
           (apply str (map (fn [[k v]] (str k v)) ordered)) "]"))
    :else
    (subject-error! :unsupported-interface-value
                    {:at path :value-type (some-> value class str)})))

(defn- canonical-string
  [subject]
  (let [budget (atom 0)
        result (str "GESSO-ASSURANCE-SUBJECT-IDENTITY-V1:"
                    (encode-value {:format canonical-format :subject subject}
                                  [] 0 budget))]
    (when (> (count result) max-canonical-chars)
      (subject-error! :canonical-size-limit
                      {:size (count result) :maximum max-canonical-chars}))
    result))

(defn- sha256-hex
  [^String canonical]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes canonical StandardCharsets/US_ASCII))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn describe-subject
  "Build an UNADMITTED semantic-subject identity.

   Required input is EXACTLY {:kind <known scope kind>, :id <qualified keyword>,
                              :interface <nonempty pure data map>}.
   Canonicalization ignores map/set order, metadata, and EDN formatting, but
   never drops interface keys, trims strings, reorders vectors, or executes code.
   Any semantic-interface modification that changes the encoded structure
   changes the digest except for the cryptographic possibility of SHA-256
   collisions; the digest is not itself a certificate or admission capability.

   The interface's nested expressions are DATA, not yet type-checked formulas;
   formula typing and semantic admission belong to later R4 stages."
  [subject]
  (when-not (and (map? subject) (not (record? subject)))
    (subject-error! :expected-subject-map {:at :subject}))
  (let [actual (set (keys subject))]
    (when-not (= required-keys actual)
      (subject-error! :wrong-keys
                      {:at :subject
                       :missing (set/difference required-keys actual)
                       :unexpected (set/difference actual required-keys)})))
  (when-not (contains? claim/scope-kinds (:kind subject))
    (subject-error! :unknown-subject-kind {:kind (:kind subject)}))
  (when-not (qualified-keyword? (:id subject))
    (subject-error! :invalid-subject-id {:id (:id subject)}))
  (when-not (and (map? (:interface subject))
                 (not (record? (:interface subject)))
                 (seq (:interface subject)))
    (subject-error! :invalid-interface {:at :interface}))
  ;; Don't accept a cached :fingerprint or supplied :valid? as input, even if it
  ;; matches an independently computed digest. It is *always* recomputed here.
  (let [canonical (canonical-string subject)
        fingerprint (str "sha256:" (sha256-hex canonical))]
    {:type identity-type
     :version identity-version
     :subject {:kind (:kind subject) :id (:id subject)}
     :fingerprint fingerprint
     :checking :not-performed
     :admission :not-admitted}))

(defn same-interface?
  "Compare fully validated declarative subjects by their versioned identities.
   Equality is structural, not a proof of behavioral equivalence or execution."
  [left right]
  (= (:fingerprint (describe-subject left))
     (:fingerprint (describe-subject right))))

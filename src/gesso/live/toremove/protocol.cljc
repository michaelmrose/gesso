(ns gesso.live.protocol
  "Shared, platform-neutral wire vocabulary for Gesso Live.

   This namespace owns names and pure normalization used by both JVM Clojure
   and ClojureScript.

   It deliberately does not own:
   - choreography or execution semantics
   - DOM behavior
   - HTMX behavior
   - server rendering
   - application policy

   Browser-visible values should be produced and interpreted through this
   namespace instead of duplicating protocol literals across CLJ and CLJS."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Protocol identity
;; -----------------------------------------------------------------------------

(def optimistic-protocol-version
  "Optimistic protocol version.

   Version 2 introduces explicit canonical marking and typed revision wire
   encoding so numeric revisions cannot be confused with opaque string
   revisions."
  "2")

;; -----------------------------------------------------------------------------
;; Shared request headers
;; -----------------------------------------------------------------------------

(def consistency-token-header-name
  "Canonical lower-case Ring request header for propagated consistency tokens."
  "x-gesso-live-consistency-token")

(def optimistic-execution-header-name
  "Canonical lower-case Ring request header used to correlate an HTMX command
   with one browser-side optimistic execution."
  "gesso-optimistic-execution")

;; -----------------------------------------------------------------------------
;; Client continuity attributes
;; -----------------------------------------------------------------------------

(def continuity-attr
  :data-gesso-live-continuity)

(def continuity-config-attr
  :data-gesso-live-continuity-config)

(def continuity-fragment-attr
  :data-gesso-live-continuity-fragment)

;; -----------------------------------------------------------------------------
;; Optimistic protocol attributes
;; -----------------------------------------------------------------------------

(def optimistic-protocol-attr
  :data-gesso-optimistic-protocol)

(def optimistic-transition-attr
  :data-gesso-optimistic-transition)

(def optimistic-template-attr
  :data-gesso-optimistic-template)

(def optimistic-target-attr
  :data-gesso-optimistic-target)

(def optimistic-scope-attr
  :data-gesso-optimistic-scope)

(def optimistic-base-revision-attr
  :data-gesso-optimistic-base-revision)

(def optimistic-revision-attr
  :data-gesso-optimistic-revision)

(def optimistic-pending-label-attr
  :data-gesso-optimistic-label)

(def optimistic-projection-mode-attr
  :data-gesso-optimistic-mode)

(def optimistic-settlement-attr
  :data-gesso-optimistic-settlement)

(def optimistic-execution-attr
  :data-gesso-optimistic-execution)

(def optimistic-outcome-attr
  :data-gesso-optimistic-outcome)

(def optimistic-command-applied-attr
  :data-gesso-optimistic-command-applied)

(def optimistic-reason-attr
  :data-gesso-optimistic-reason)

(def optimistic-canonical-attr
  "Marks authoritative server-rendered content for optimistic reconciliation.

   Scope membership alone is intentionally not enough to imply canonical
   authority because action sources, projection templates, and settlement
   markers may carry the same scope."
  :data-gesso-optimistic-canonical)

(def reserved-optimistic-attrs
  "Framework-owned optimistic attributes that application attrs may not
   override."
  [optimistic-protocol-attr
   optimistic-transition-attr
   optimistic-template-attr
   optimistic-target-attr
   optimistic-scope-attr
   optimistic-base-revision-attr
   optimistic-revision-attr
   optimistic-pending-label-attr
   optimistic-projection-mode-attr
   optimistic-settlement-attr
   optimistic-execution-attr
   optimistic-outcome-attr
   optimistic-command-applied-attr
   optimistic-reason-attr
   optimistic-canonical-attr])

;; -----------------------------------------------------------------------------
;; Shared semantic vocabulary
;; -----------------------------------------------------------------------------

(def projection-modes
  #{:pending :provisional :full})

(def settlement-outcomes
  #{:confirmed :reconciled :rejected :failed})

(def max-safe-integer-revision
  "Largest integer revision that round-trips exactly through JavaScript."
  9007199254740991)

;; -----------------------------------------------------------------------------
;; Small validation helpers
;; -----------------------------------------------------------------------------

(defn- ex
  [message data]
  (ex-info message data))

(defn- non-blank-string?
  [x]
  (and (string? x)
       (not (str/blank? x))))

(defn qualified-name
  "Return a browser-safe textual name while preserving keyword namespaces."
  [x]
  (cond
    (keyword? x)
    (if-some [namespace' (namespace x)]
      (str namespace' "/" (name x))
      (name x))

    (symbol? x)
    (str x)

    (nil? x)
    nil

    :else
    (str x)))

(defn normalize-name
  "Normalize a required semantic name to a non-blank string."
  [k value]
  (let [value' (qualified-name value)]
    (when-not (non-blank-string? value')
      (throw
       (ex "Gesso Live protocol name must not be blank."
           {:key k
            :value value})))
    value'))

(defn normalize-optional-name
  "Normalize an optional semantic name."
  [k value]
  (when (some? value)
    (normalize-name k value)))

;; -----------------------------------------------------------------------------
;; Scope wire identity
;; -----------------------------------------------------------------------------

(defn wire-scope
  "Encode a semantic scope as an opaque browser identity.

   Strings and Clojure data are tagged separately so a string that happens to
   look like printed Clojure data cannot collide with the data value itself.

   The browser treats this value as opaque and compares it only for equality."
  [scope]
  (when (nil? scope)
    (throw
     (ex "Gesso Live protocol scope is required."
         {:scope scope})))
  (let [wire (if (string? scope)
               (str "s:" scope)
               (str "e:" (pr-str scope)))]
    (when (str/blank? (subs wire 2))
      (throw
       (ex "Gesso Live protocol scope must not be blank."
           {:scope scope})))
    wire))

;; -----------------------------------------------------------------------------
;; Revision wire format and comparison
;; -----------------------------------------------------------------------------

(defn normalize-revision
  "Normalize an optional semantic revision.

   Supported revisions are:
   - non-negative integers, which have total numeric ordering
   - non-blank strings, which are opaque identities and support equality only"
  [k revision]
  (when (some? revision)
    (cond
      (and (integer? revision)
           (not (neg? revision))
           (<= revision max-safe-integer-revision))
      revision

      (non-blank-string? revision)
      revision

      :else
      (throw
       (ex "Gesso Live revision must be a JavaScript-safe non-negative integer or non-blank string."
           {:key k
            :revision revision
            :max-safe-integer max-safe-integer-revision})))))

(defn revision->wire
  "Encode a revision without losing whether it was numeric or opaque.

   Integer 42 becomes \"i:42\".
   Opaque string \"42\" becomes \"s:42\".

   Keeping those distinct is required for correct browser-side comparison."
  [revision]
  (when-some [revision' (normalize-revision :revision revision)]
    (if (integer? revision')
      (str "i:" revision')
      (str "s:" revision'))))

(defn wire->revision
  "Decode one revision produced by revision->wire.

   Returns nil for nil. Throws for malformed or unknown encodings."
  [wire]
  (when (some? wire)
    (when-not (non-blank-string? wire)
      (throw
       (ex "Gesso Live wire revision must be a non-blank string."
           {:wire wire})))
    (cond
      (str/starts-with? wire "i:")
      (let [digits (subs wire 2)]
        (when-not (re-matches #"[0-9]+" digits)
          (throw
           (ex "Malformed numeric Gesso Live wire revision."
               {:wire wire})))
        #?(:clj
           (Long/parseLong digits)
           :cljs
           (let [n (js/Number digits)]
             (when-not (js/Number.isSafeInteger n)
               (throw
                (ex "Numeric Gesso Live wire revision exceeds JavaScript safe integer range."
                    {:wire wire})))
             n)))

      (str/starts-with? wire "s:")
      (let [value (subs wire 2)]
        (when (str/blank? value)
          (throw
           (ex "Opaque Gesso Live wire revision must not be blank."
               {:wire wire})))
        value)

      :else
      (throw
       (ex "Unknown Gesso Live wire revision encoding."
           {:wire wire})))))

(defn compare-revisions
  "Compare two normalized or decoded revisions.

   Returns:
   - :same         values are equal
   - :newer        left is numerically newer than right
   - :older        left is numerically older than right
   - :incomparable ordering is not defined

   Opaque string revisions intentionally have equality semantics only. Gesso
   must never invent ordering for them."
  [left right]
  (let [left'  (normalize-revision :left-revision left)
        right' (normalize-revision :right-revision right)]
    (cond
      (= left' right')
      :same

      (and (integer? left')
           (integer? right'))
      (if (> left' right') :newer :older)

      :else
      :incomparable)))

(defn compare-wire-revisions
  "Compare two encoded revisions using compare-revisions."
  [left-wire right-wire]
  (compare-revisions
   (wire->revision left-wire)
   (wire->revision right-wire)))

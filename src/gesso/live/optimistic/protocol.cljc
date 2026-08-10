(ns gesso.live.optimistic.protocol
  "Platform-neutral wire vocabulary for Gesso Live optimistic commands.

   This namespace is the single owner of browser/server optimistic protocol
   names and pure normalization. It is shared by JVM Clojure and ClojureScript.

   It owns:
   - optimistic protocol/version identity
   - optimistic request/settlement attribute names
   - optimistic execution request header name
   - command and settlement message contracts
   - semantic settlement outcomes
   - projection-mode normalization
   - opaque scope wire identity
   - typed revision encoding/comparison
   - strict settlement outcome and command-applied wire parsing

   It deliberately does not own:
   - choreography control flow
   - DOM behavior or continuity
   - HTMX request construction
   - server rendering
   - consistency-token transport headers
   - application/domain transition policy."
  (:require
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Protocol identity
;; -----------------------------------------------------------------------------

(def version
  "Optimistic browser/server wire protocol version.

   Version 2 uses explicit canonical marking and typed revision encoding. The
   choreography/runtime rewrite does not by itself change this wire format, so
   the version remains 2."
  "2")

(def command-event
  :optimistic/command)

(def settlement-event
  :optimistic/settlement)

;; -----------------------------------------------------------------------------
;; Request header
;; -----------------------------------------------------------------------------

(def execution-header-name
  "Canonical lower-case Ring request header carrying the browser-generated
   optimistic execution id."
  "gesso-optimistic-execution")

;; -----------------------------------------------------------------------------
;; Optimistic markup attributes
;; -----------------------------------------------------------------------------

(def protocol-attr
  :data-gesso-optimistic-protocol)

(def transition-attr
  :data-gesso-optimistic-transition)

(def template-attr
  :data-gesso-optimistic-template)

(def target-attr
  :data-gesso-optimistic-target)

(def scope-attr
  :data-gesso-optimistic-scope)

(def base-revision-attr
  :data-gesso-optimistic-base-revision)

(def revision-attr
  :data-gesso-optimistic-revision)

(def pending-label-attr
  :data-gesso-optimistic-label)

(def projection-mode-attr
  :data-gesso-optimistic-mode)

(def settlement-attr
  :data-gesso-optimistic-settlement)

(def execution-attr
  :data-gesso-optimistic-execution)

(def outcome-attr
  :data-gesso-optimistic-outcome)

(def command-applied-attr
  :data-gesso-optimistic-command-applied)

(def reason-attr
  :data-gesso-optimistic-reason)

(def canonical-attr
  "Marks authoritative server-rendered content.

   Scope membership alone never implies canonical authority: optimistic source
   elements, projection templates, and settlement markers may carry the same
   scope."
  :data-gesso-optimistic-canonical)

(def reserved-attrs
  "Framework-owned optimistic attrs that application attrs may not override."
  [protocol-attr
   transition-attr
   template-attr
   target-attr
   scope-attr
   base-revision-attr
   revision-attr
   pending-label-attr
   projection-mode-attr
   settlement-attr
   execution-attr
   outcome-attr
   command-applied-attr
   reason-attr
   canonical-attr])

;; -----------------------------------------------------------------------------
;; Shared semantic vocabulary
;; -----------------------------------------------------------------------------

(def projection-modes
  #{:pending :provisional :full})

(def settlement-outcomes
  #{:confirmed :reconciled :rejected :failed})

(def applied-settlement-outcomes
  #{:confirmed :reconciled})

(def max-safe-integer-revision
  "Largest integer revision that round-trips exactly through JavaScript."
  9007199254740991)

;; -----------------------------------------------------------------------------
;; Choreography payload keys and message contracts
;; -----------------------------------------------------------------------------

(def execution-id-key :execution-id)
(def transition-key :transition)
(def scope-key :scope)
(def base-revision-key :base-revision)
(def consistency-token-key :consistency-token)
(def outcome-key :outcome)
(def command-applied-key :command-applied?)
(def revision-key :revision)
(def canonical-key :canonical)
(def reason-key :reason)

(def command-required-keys
  #{execution-id-key
    transition-key
    scope-key})

(def command-optional-keys
  #{base-revision-key
    consistency-token-key})

(def command-correlation-keys
  #{execution-id-key
    scope-key})

(def settlement-required-keys
  #{execution-id-key
    scope-key
    outcome-key
    command-applied-key
    canonical-key})

(def settlement-optional-keys
  #{revision-key
    reason-key
    consistency-token-key})

(def settlement-correlation-keys
  #{execution-id-key
    scope-key})

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
       (ex "Gesso Live optimistic protocol name must not be blank."
           {:key k
            :value value})))
    value'))

(defn normalize-optional-name
  "Normalize an optional semantic name."
  [k value]
  (when (some? value)
    (normalize-name k value)))

;; -----------------------------------------------------------------------------
;; Projection mode
;; -----------------------------------------------------------------------------

(defn normalize-projection-mode
  "Normalize a projection mode keyword.

   nil means :provisional."
  [mode]
  (let [mode' (or mode :provisional)]
    (when-not (contains? projection-modes mode')
      (throw
       (ex "Invalid Gesso Live optimistic projection mode."
           {:projection-mode mode
            :allowed projection-modes})))
    mode'))

(defn projection-mode->wire
  [mode]
  (name (normalize-projection-mode mode)))

(defn wire->projection-mode
  [wire]
  (when-not (non-blank-string? wire)
    (throw
     (ex "Gesso Live optimistic projection mode wire value must be non-blank."
         {:wire wire})))
  (normalize-projection-mode (keyword wire)))

;; -----------------------------------------------------------------------------
;; Settlement semantics
;; -----------------------------------------------------------------------------

(defn normalize-settlement-outcome
  "Validate and return one semantic settlement outcome keyword."
  [outcome]
  (when-not (contains? settlement-outcomes outcome)
    (throw
     (ex "Invalid Gesso Live optimistic settlement outcome."
         {:outcome outcome
          :allowed settlement-outcomes})))
  outcome)

(defn command-applied-for-outcome?
  "True exactly when the semantic settlement means the command was applied."
  [outcome]
  (contains? applied-settlement-outcomes
             (normalize-settlement-outcome outcome)))

(defn settlement-outcome->wire
  [outcome]
  (name (normalize-settlement-outcome outcome)))

(defn wire->settlement-outcome
  "Strictly decode one settlement outcome attribute value."
  [wire]
  (when-not (non-blank-string? wire)
    (throw
     (ex "Gesso Live settlement outcome wire value is required."
         {:wire wire})))
  (normalize-settlement-outcome (keyword wire)))

(defn command-applied->wire
  "Encode the required settlement command-applied flag."
  [applied?]
  (cond
    (true? applied?) "true"
    (false? applied?) "false"
    :else
    (throw
     (ex "Gesso Live command-applied value must be boolean."
         {:command-applied? applied?}))))

(defn wire->command-applied
  "Strictly decode the required settlement command-applied attribute.

   Missing, blank, or non-boolean values throw. This prevents malformed
   settlement markers from silently becoming command-applied? false."
  [wire]
  (case wire
    "true" true
    "false" false
    (throw
     (ex "Malformed Gesso Live command-applied wire value."
         {:wire wire
          :allowed #{"true" "false"}}))))

(defn settlement-consistent?
  "True when outcome and command-applied? agree with protocol semantics."
  [outcome command-applied?]
  (= (command-applied-for-outcome? outcome)
     command-applied?))

(defn assert-settlement-consistent!
  "Return true when settlement outcome and command-applied? agree, otherwise
   throw."
  [outcome command-applied?]
  (let [outcome' (normalize-settlement-outcome outcome)]
    (when-not (or (true? command-applied?)
                  (false? command-applied?))
      (throw
       (ex "Gesso Live settlement command-applied? must be boolean."
           {:outcome outcome'
            :command-applied? command-applied?})))
    (when-not (settlement-consistent? outcome' command-applied?)
      (throw
       (ex "Gesso Live settlement outcome disagrees with command-applied?."
           {:outcome outcome'
            :command-applied? command-applied?
            :expected-command-applied?
            (command-applied-for-outcome? outcome')})))
    true))

;; -----------------------------------------------------------------------------
;; Scope wire identity
;; -----------------------------------------------------------------------------

(defn wire-scope
  "Encode a semantic scope as an opaque browser identity.

   Strings and Clojure data are tagged separately so a string that happens to
   resemble printed EDN cannot collide with the data value itself. The browser
   treats the result as opaque and compares it only for equality."
  [scope]
  (when (nil? scope)
    (throw
     (ex "Gesso Live optimistic scope is required."
         {:scope scope})))
  (let [wire (if (string? scope)
               (str "s:" scope)
               (str "e:" (pr-str scope)))]
    (when (str/blank? (subs wire 2))
      (throw
       (ex "Gesso Live optimistic scope must not be blank."
           {:scope scope})))
    wire))

;; -----------------------------------------------------------------------------
;; Revision wire format and comparison
;; -----------------------------------------------------------------------------

(defn normalize-revision
  "Normalize an optional semantic revision.

   Supported revisions:
   - JavaScript-safe non-negative integers, with total numeric ordering
   - non-blank strings, treated as opaque identities with equality only."
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

   Integer 42 becomes \"i:42\"; opaque string \"42\" becomes \"s:42\"."
  [revision]
  (when-some [revision' (normalize-revision :revision revision)]
    (if (integer? revision')
      (str "i:" revision')
      (str "s:" revision'))))

(defn wire->revision
  "Decode one revision produced by revision->wire.

   nil stays nil. Malformed or unknown encodings throw."
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
           (let [n (Long/parseLong digits)]
             (normalize-revision :wire-revision n))
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

   Returns :same, :newer, :older, or :incomparable. Opaque strings intentionally
   have equality semantics only; Gesso never invents ordering for them."
  [left right]
  (let [left' (normalize-revision :left-revision left)
        right' (normalize-revision :right-revision right)]
    (cond
      (= left' right')
      :same

      (and (integer? left')
           (integer? right'))
      (if (pos? (compare left' right'))
        :newer
        :older)

      :else
      :incomparable)))

(defn compare-wire-revisions
  "Compare two encoded revisions using compare-revisions."
  [left-wire right-wire]
  (compare-revisions (wire->revision left-wire)
                     (wire->revision right-wire)))

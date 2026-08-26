(ns gesso.live.progression.http
  "HTTP transport boundary for Gesso Live authoritative refresh progression.

   gesso.live.progression owns the portable semantic requirement and its closed
   versioned EDN wire *data*. This namespace owns only the HTTP representation
   used to carry that requirement from an adapter-authorized browser fragment
   refresh back to the trusted server request context.

   The browser-supplied requirement is never authority. It is a conservative
   minimum-read request: trusted server/model/XTDB code must still authenticate
   the request, authorize the read, validate authority-specific basis shape and
   compatibility, and decide whether/how the requested basis can be satisfied.

   Transport rules:

   - one canonical request header carries the requirement;
   - absence of the header means no browser progression requirement;
   - a present header must decode successfully or the request fails closed;
   - the header payload is URI-component encoded UTF-8 EDN text so arbitrary
     portable EDN characters cannot become raw HTTP header control characters;
   - decoding delegates semantic validation to
     gesso.live.progression/wire->requirement;
   - binding into ctx conservatively composes with any pre-existing canonical
     :gesso.live/progression requirement rather than replacing it.

   This namespace deliberately knows nothing about HTMX lifecycle events, DOM
   nodes, SSE, XTDB ordering, fragment generations, or application revisions."
  (:require
   [clojure.string :as str]
   [gesso.live.progression :as progression]
   #?(:clj [clojure.edn :as edn]
      :cljs [cljs.reader :as edn])))

;; =============================================================================
;; Public transport vocabulary
;; =============================================================================

(def request-header-name
  "Canonical HTTP request header carrying one encoded Live progression
   requirement. Ring normalizes request header names to lower-case strings."
  "gesso-live-progression")

(def canonical-context-key
  "Canonical request/read-context key consumed by gesso.live.core and the XTDB
   consistency adapter."
  :gesso.live/progression)

;; =============================================================================
;; Errors / validation helpers
;; =============================================================================

(defn- transport-error
  ([kind message data]
   (transport-error kind message data nil))
  ([kind message data cause]
   (ex-info
    message
    (merge
     {:error/type :gesso.live.progression.http/error
      :error/kind kind}
     data)
    cause)))

(defn- require-map!
  [label value]
  (when-not (map? value)
    (throw
     (transport-error
      :invalid-map
      (str label " must be a map.")
      {:label label
       :value value})))
  value)

(defn- require-nonblank-string!
  [label value]
  (when-not (and (string? value)
                 (not (str/blank? value)))
    (throw
     (transport-error
      :invalid-header-value
      (str label " must be a non-blank string.")
      {:label label
       :value value})))
  value)

;; =============================================================================
;; Header-safe text codec
;; =============================================================================

(defn- uri-encode
  [value]
  #?(:clj
     (-> (java.net.URLEncoder/encode
          (str value)
          (.name java.nio.charset.StandardCharsets/UTF_8))
         ;; java.net.URLEncoder is HTML-form flavored and encodes spaces as +.
         ;; Browsers use encodeURIComponent here, whose URI-component form is
         ;; %20. Normalize the JVM representation so either runtime can decode
         ;; values emitted by the other.
         (str/replace "+" "%20"))
     :cljs
     (js/encodeURIComponent (str value))))

(defn- uri-decode
  [value]
  #?(:clj
     (java.net.URLDecoder/decode
      (str value)
      (.name java.nio.charset.StandardCharsets/UTF_8))
     :cljs
     (js/decodeURIComponent (str value))))

(defn encode-request-progression
  "Encode one normalized progression requirement for the canonical HTTP header.

   nil means no requirement and returns nil. Non-nil values must already satisfy
   the portable progression requirement contract. The textual representation is
   transport-only; semantic ordering remains absent."
  [requirement-value]
  (when-some [requirement'
              (progression/normalize-requirement requirement-value)]
    (-> requirement'
        progression/requirement->wire
        pr-str
        uri-encode)))

(defn decode-request-progression
  "Decode one present canonical header value into a normalized requirement.

   nil means the header was absent and returns nil. A present blank, malformed,
   unreadable, or semantically invalid value fails closed."
  [header-value]
  (when (some? header-value)
    (let [encoded (require-nonblank-string!
                   "Gesso Live progression request header"
                   header-value)
          text
          (try
            (uri-decode encoded)
            (catch #?(:clj Throwable :cljs :default) error
              (throw
               (transport-error
                :invalid-header-encoding
                "Gesso Live progression request header is not valid URI-component encoding."
                {:header-name request-header-name
                 :header-value encoded}
                error))))
          wire
          (try
            (edn/read-string text)
            (catch #?(:clj Throwable :cljs :default) error
              (throw
               (transport-error
                :invalid-header-edn
                "Gesso Live progression request header does not contain readable EDN."
                {:header-name request-header-name
                 :decoded-text text}
                error))))]
      (try
        (progression/wire->requirement wire)
        (catch #?(:clj Throwable :cljs :default) error
          (throw
           (transport-error
            :invalid-progression-wire
            "Gesso Live progression request header contains invalid progression wire data."
            {:header-name request-header-name
             :wire wire}
            error)))))))

;; =============================================================================
;; Ring/Biff-ish request extraction
;; =============================================================================

(defn request-map
  "Return the Ring request map from a raw request or Biff-style ctx.

   Gesso commonly receives request fields directly on ctx, while some call paths
   nest the original Ring request under :request."
  [ctx-or-request]
  (require-map! "Gesso Live progression request/context" ctx-or-request)
  (let [request (or (:request ctx-or-request)
                    ctx-or-request)]
    (require-map! "Gesso Live progression Ring request" request)))

(defn- normalized-header-key
  [key]
  (cond
    (string? key)
    (str/lower-case key)

    (keyword? key)
    (str/lower-case (name key))

    :else
    nil))

(defn request-header-value
  "Return the unique canonical progression header value from request/ctx.

   Header names are case-insensitive. Multiple spellings carrying the same value
   are tolerated; conflicting duplicate values fail closed. A present value is
   not decoded by this function."
  [ctx-or-request]
  (let [request (request-map ctx-or-request)
        headers (:headers request)]
    (when (some? headers)
      (require-map! "Ring request :headers" headers))
    (let [matches
          (->> headers
               (keep
                (fn [[key value]]
                  (when (= request-header-name
                           (normalized-header-key key))
                    value)))
               vec)]
      (cond
        (empty? matches)
        nil

        (apply = matches)
        (first matches)

        :else
        (throw
         (transport-error
          :conflicting-header-values
          "Gesso Live progression request contains conflicting duplicate header values."
          {:header-name request-header-name
           :values matches}))))))

(defn progression-from-request
  "Extract and decode the optional browser progression requirement from request/ctx.

   This is the one server-side transport decode operation. Callers should bind
   the result into canonical request context at the fragment/request boundary and
   pass that context onward rather than repeatedly decoding HTTP metadata."
  [ctx-or-request]
  (decode-request-progression
   (request-header-value ctx-or-request)))

(defn bind-request-progression
  "Bind the optional HTTP progression requirement into canonical request ctx.

   If the request carries no progression header, ctx is returned unchanged.
   When a header is present, its decoded requirement is conservatively composed
   with any pre-existing canonical :gesso.live/progression requirement. This
   prevents untrusted request metadata from weakening a server-established read
   requirement.

   This function does not establish authority or XTDB ordering. It only installs
   the normalized minimum-read requirement for downstream trusted consistency
   code to interpret."
  [ctx]
  (require-map! "Gesso Live progression ctx" ctx)
  (if-some [request-requirement (progression-from-request ctx)]
    (let [existing
          (when (contains? ctx canonical-context-key)
            (progression/normalize-requirement
             (get ctx canonical-context-key)))
          combined
          (progression/compose existing request-requirement)]
      (assoc ctx canonical-context-key combined))
    ctx))

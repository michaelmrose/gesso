(ns gesso.assurance.reference-document-iid
  "Independent selected XTDB 2.2.0-beta1 document identity derivation."
  (:require [gesso.assurance.reference-arrow-framing :refer [require!]])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util HexFormat)))

(defn integer-iid
  "Derive the retained 128-bit IID (lowercase hex) for a signed int64 ID.
   The selected encoding is UTF-8 of i followed by signed decimal text,
   hashed with SHA-256 and truncated to its first 16 bytes."
  [id]
  (require! (and (integer? id) (<= Long/MIN_VALUE id Long/MAX_VALUE))
            :unsupported-reference-integer-id)
  (let [input (.getBytes (str "i" (Long/toString (long id))) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") input)]
    (.formatHex (HexFormat/of) digest 0 16)))

(defn string-iid
  "Derive the selected string IID from UTF-8 of s followed by the exact
   string. No normalization or replacement of malformed UTF-16. This
   diagnostic domain is bounded and does not imply reference admission."
  [id]
  (require! (and (string? id) (<= (count id) 4096)
                 (.canEncode (.newEncoder StandardCharsets/UTF_8) ^CharSequence id))
            :unsupported-reference-string-id)
  (let [input (.getBytes (str "s" id) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") input)]
    (.formatHex (HexFormat/of) digest 0 16)))

(defn keyword-iid
  "Derive the selected keyword IID from UTF-8 of k followed by the printed
   keyword, including its colon. Restrict namespace and local name to the
   selected retained keyword grammar. No provider helper or normalization."
  [id]
  (require! (and (keyword? id)
                 (re-matches #"[a-zA-Z][a-zA-Z0-9_-]*" (name id))
                 (or (nil? (namespace id))
                     (re-matches #"[a-zA-Z][a-zA-Z0-9_.-]*" (namespace id)))
                 (<= (count (str id)) 4096))
            :unsupported-reference-keyword-id)
  (let [input (.getBytes (str "k" id) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") input)]
    (.formatHex (HexFormat/of) digest 0 16)))

(defn uuid-iid
  "Derive the selected UUID IID directly from its 128 bits in network byte
   order, without hashing, type prefixes or provider helpers."
  [id]
  (require! (uuid? id) :unsupported-reference-uuid-id)
  (let [^java.util.UUID id id
        b (doto (java.nio.ByteBuffer/allocate 16)
            (.order java.nio.ByteOrder/BIG_ENDIAN)
            (.putLong (.getMostSignificantBits id))
            (.putLong (.getLeastSignificantBits id)))]
    (.formatHex (HexFormat/of) (.array b))))

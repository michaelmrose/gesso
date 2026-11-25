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

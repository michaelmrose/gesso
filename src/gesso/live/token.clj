(ns gesso.live.token
  "Small plumbing namespace for the live system's opaque consistency token.

   This namespace owns only:
   - where the token lives in request-shaped maps
   - how to extract it
   - how to associate and dissociate it

   It does not own:
   - token generation
   - token interpretation
   - backend-specific token semantics"
  (:require
   [gesso.live.htmx :as htmx]))

(defn header-name
  "Return the canonical request header used for the propagated consistency token."
  []
  (htmx/token-header-name))

(defn param-key
  "Return the canonical fallback request param key for the consistency token."
  []
  :consistency-token)

(defn- request-headers
  "Return request headers or an empty map."
  [request]
  (or (:headers request) {}))

(defn- request-params
  "Return request params or an empty map."
  [request]
  (or (:params request) {}))

(defn extract-request-token
  "Extract the propagated consistency token from a Ring/Biff-like request or ctx.

   Lookup order:
   1. canonical request header
   2. canonical fallback param"
  [ctx]
  (let [headers (request-headers ctx)
        params  (request-params ctx)
        hname   (header-name)
        pkey    (param-key)
        header-token (get headers hname)
        param-token  (get params pkey)]
    (or header-token param-token)))

(defn request-token-present?
  "Return true when the request carries a propagated consistency token."
  [ctx]
  (boolean (extract-request-token ctx)))

(defn assoc-request-token
  "Associate the propagated consistency token onto a request-shaped map.

   Uses the canonical request header.
   If token is nil, returns the request unchanged."
  [request token]
  (if (nil? token)
    request
    (let [headers (assoc (request-headers request)
                         (header-name)
                         token)]
      (assoc request :headers headers))))

(defn dissoc-request-token
  "Remove the propagated consistency token from a request-shaped map.

   Removes both:
   - the canonical request header
   - the canonical fallback param"
  [request]
  (let [headers' (not-empty (dissoc (request-headers request)
                                    (header-name)))
        params'  (not-empty (dissoc (request-params request)
                                    (param-key)))
        request' (dissoc request :headers :params)]
    (cond-> request'
      headers' (assoc :headers headers')
      params'  (assoc :params params'))))

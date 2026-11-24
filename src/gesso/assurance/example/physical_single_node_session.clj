(ns gesso.assurance.example.physical-single-node-session
  "v2338: Scoped server-issued credentials for the reservation proving fixture.
   This is NOT a login system, public cookie deployment or production session
   store. The trusted test host provisions principals; network clients must
   present an opaque server-issued cookie, and POSTs additionally require a
   session-bound synchronizer token. No identity is accepted from a header,
   request parameter or caller-populated Ring session. Revocation is observed
   on subsequent acquisitions (including the existing SSE event recheck),
   not an atomic fence against already admitted transactions or frames."
  (:require [gesso.assurance.example.physical-single-node-principal :as principal])
  (:import (java.security SecureRandom MessageDigest)
           (java.util HexFormat)
           (java.nio.charset StandardCharsets)))

(def ^:private cookie-name "gesso-proving-session")
(def ^:private csrf-header "x-gesso-proving-csrf")
(def ^:private maximum-sessions 64)

(defn- random-token [^SecureRandom random]
  (let [bytes (byte-array 32)]
    (.nextBytes random bytes)
    (.formatHex (HexFormat/of) bytes)))

(defn- cookie-id [request]
  ;; This isolated loopback fixture accepts exactly one canonical cookie.
  ;; Reject duplicates, malformed values and other cookies instead of
  ;; ambiguously picking a credential. No URL decoding or :session fallback.
  (let [header (get-in request [:headers "cookie"])]
    (when (string? header)
      (second (re-matches #"gesso-proving-session=([0-9a-f]{64})" header)))))

(defn- valid-csrf? [expected supplied]
  (and (string? supplied)
       (re-matches #"[0-9a-f]{64}" supplied)
       (MessageDigest/isEqual
        (.getBytes ^String expected StandardCharsets/US_ASCII)
        (.getBytes ^String supplied StandardCharsets/US_ASCII))))

(defn- reject! [reason]
  (throw (ex-info "Invalid proving session authority." {:reason reason})))

(defn with-session-policy!
  "Supply a checked grant policy WITHOUT :acquire-principal!. During f, the
   trusted host may issue credentials for declared principals and revoke them.
   :source can be passed to the existing granted Live fixture. :issue! returns
   :session-id, :cookie (Cookie request header value), and :csrf-token (value
   for X-Gesso-Proving-Csrf). This intentionally provisions credentials out of
   band; it does not serve login or Set-Cookie endpoints. Scope exit invalidates
   all credentials. Invalid/missing credentials or CSRF proof resolve anonymous
   (401 at the selected handler), with no mutation delegation."
  [policy f]
  (when-not (and (map? policy) (not (contains? policy :acquire-principal!))
                 (fn? f))
    (reject! :invalid-session-policy-input))
  (let [random (SecureRandom.)
        state (atom {:open? true :sessions {}})
        acquire! (fn [request]
                   (let [id (cookie-id request)
                         session (get-in @state [:sessions id])]
                     (if (and session
                              (or (not= :post (:request-method request))
                                  (valid-csrf? (:csrf-token session)
                                               (get-in request [:headers csrf-header]))))
                       {:status :authenticated :principal-id (:principal-id session)}
                       {:status :anonymous})))
        source (principal/checked-test-policy!
                (assoc policy :acquire-principal! acquire!))
        known (into (:read-principals source) (keys (:grants source)))
        issue! (fn [who]
                 (when-not (contains? known who)
                   (reject! :unknown-session-principal))
                 (let [id (random-token random)
                       csrf (random-token random)
                       entry {:principal-id who :csrf-token csrf}]
                   (swap! state
                          (fn [{:keys [open? sessions] :as s}]
                            (when-not open? (reject! :session-authority-closed))
                            (when (or (>= (count sessions) maximum-sessions)
                                      (contains? sessions id))
                              (reject! :session-capacity-or-collision))
                            (assoc-in s [:sessions id] entry)))
                   {:session-id id :cookie (str cookie-name "=" id)
                    :csrf-token csrf}))
        revoke! (fn [id] (swap! state update :sessions dissoc id) nil)]
    (try
      (f {:source source :issue! issue! :revoke! revoke!})
      (finally
        (reset! state {:open? false :sessions {}})))))

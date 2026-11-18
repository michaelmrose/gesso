(ns gesso.assurance.proof.checked-biff-request-principal
  "v1788: Source-relative ONE-operation principal/owner correspondence before
   the v1781 checked HTTP -> typed/canonical execution trial.

   A trusted host acquisition callback sees the ACTUAL selected Ring request,
   returns a closed anonymous/authenticated result, and the policy must grant
   the selected operation to the principal and bind it to the exact typed
   :input/owner. Denials never invoke the original certified handler.

   An optional request-intent callback can deny even an otherwise granted
   request, using the SAME Ring request and resolved principal/operation.
   Before issuing a grant, require the SAME checked original Claim catalog
   used by the typed trial to declare its input/owner -> document/owner
   relationship. This is source-relative ownership CORRESPONDENCE, not
   independent proof of principal authentication or business authorization.
   Both callbacks and grants are APPLICATION SUPPLIED. This module does not
   authenticate cookies, credentials or Ring adapters, audit callback purity,
   certify CSRF protection, or prove the policy is in the original catalog.
   No admission, certificate, transaction or retry capability is issued."
  (:require [gesso.assurance.proof.operation-set :as catalog]))

(def report-type
  :gesso.assurance.proof.checked-biff-request-principal/local-request-gate)
(def report-version 1)

(def ^:private original-catalog-check catalog/check-operation-set!)

(defn- closed-map? [m ks]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))
       (= ks (set (keys m)))))

(defn- reject! [reason]
  (throw (ex-info "Checked source-relative request principal gate rejected."
                  {:checked-biff-request-principal/rejection true
                   :reason reason})))

(defn- check-declared-owner-source! [catalog-source operation]
  ;; A source-derived write of :input/owner into the real owner field is
  ;; needed before a principal-to-typed-owner match says anything about the
  ;; submitted operation. This is deliberately one Claim shape, not a
  ;; generic authorization policy or a checker's proof of policy soundness.
  (when-not (and (closed-map? catalog-source #{:catalog :physical-field-map})
                 (map? (:catalog catalog-source))
                 (map? (:physical-field-map catalog-source))
                 (identical? original-catalog-check
                             (var-get #'catalog/check-operation-set!)))
    (reject! :principal-owner-not-bound-to-original-catalog))
  (let [checked (try (original-catalog-check (:catalog catalog-source))
                     (catch Exception _
                       (reject! :principal-owner-not-bound-to-original-catalog)))
        declarations (filterv #(= operation (:id %))
                              (get-in catalog-source [:catalog :operations]))
        construction (:construction (first declarations))]
    (when-not (and (= catalog/report-type (:type checked))
                   (= catalog/operation-set-version (:version checked))
                   (= #{operation} (:declared-operation-ids checked))
                   (= 1 (count declarations))
                   (= operation (get-in construction [:subject :id]))
                   (= {:type :string}
                      (get-in construction [:input-types :input/owner]))
                   (contains? (:reads construction) :input/owner)
                   (contains? (:writes construction) :doc/owner)
                   (= [:read :input/owner]
                      (get-in construction [:replacements :doc/owner]))
                   (= :owner
                      (get-in catalog-source [:physical-field-map :doc/owner]))
                   (= :none (:authority checked))
                   (= :not-issued (:certificate checked)))
      (reject! :principal-owner-not-bound-to-original-catalog))
    true))

(defn issue-request-principal-gate!
  "Issue one constrained principal grant for one typed operation/owner pair.
   Exactly one subject is supported here to avoid suggesting that the static
   trial can represent multiple distinct command owners. The trusted callback
   itself is NOT independently verified. Return a private invoke! gate, not a
   principal capability or a proof certificate. An optional
   :verify-request-intent! callback must return an exact boolean; false
   denies with 403, and an exception or malformed answer fails closed.
   The original catalog is mandatory; caller-supplied operation and owner
   labels by themselves cannot establish the typed Claim write."
  [policy operation inputs catalog-source]
  (check-declared-owner-source! catalog-source operation)
  (when-not (and (or (closed-map? policy #{:policy-id :principal-owners :grants
                                          :acquire-principal!})
                     (closed-map? policy #{:policy-id :principal-owners :grants
                                          :acquire-principal! :verify-request-intent!}))
                 (or (not (contains? policy :verify-request-intent!))
                     (fn? (:verify-request-intent! policy)))
                 (qualified-keyword? (:policy-id policy))
                 (fn? (:acquire-principal! policy))
                 (closed-map? inputs #{:input/owner})
                 (string? (:input/owner inputs))
                 (not-empty (:input/owner inputs))
                 (qualified-keyword? operation)
                 (map? (:principal-owners policy))
                 (map? (:grants policy))
                 (= 1 (count (:principal-owners policy)))
                 (= (set (keys (:principal-owners policy)))
                    (set (keys (:grants policy))))
                 (every? qualified-keyword?
                         (keys (:principal-owners policy)))
                 (= #{(:input/owner inputs)}
                    (set (vals (:principal-owners policy))))
                 (= #{#{operation}} (set (vals (:grants policy))))
                 (every? #(and (set? %) (nil? (meta %)))
                         (vals (:grants policy))))
    (reject! :invalid-or-unbound-principal-owner-grant))
  (let [[authorized-id] (keys (:principal-owners policy))
        original-acquire (:acquire-principal! policy)
        original-verify-intent (:verify-request-intent! policy)
        original-owners (:principal-owners policy)
        original-grants (:grants policy)
        policy-id (:policy-id policy)
        recheck! (fn []
                   ;; The original, admitted catalog is immutable Clojure
                   ;; data; the selected source is checked once at issuance.
                   ;; Reject replacement of the original checker at runtime
                   ;; without re-running expensive admission in the HTTP path.
                   (when-not (and (identical? original-catalog-check
                                              (var-get #'catalog/check-operation-set!))
                                  (identical? original-acquire
                                              (:acquire-principal! policy))
                                  (identical? original-verify-intent
                                              (:verify-request-intent! policy))
                                  (= original-owners (:principal-owners policy))
                                  (= original-grants (:grants policy))
                                  (= policy-id (:policy-id policy)))
                     (reject! :principal-policy-changed))
                   true)
        check! (fn [request]
                 (recheck!)
                 (when-not (and (map? request)
                                (= operation :request/claim)
                                (= :post (:request-method request))
                                (= "/operations/request/claim" (:uri request)))
                   (reject! :incorrect-request-principal-route))
                 ;; Exactly one call on the original Ring request. No request
                 ;; header, cookie or client-provided principal is believed
                 ;; unless the host callback chooses to validate it.
                 (let [acquired (original-acquire request)]
                   (cond
                     (= {:status :anonymous} acquired)
                     {:type report-type :version report-version
                      :status :anonymous :http-status 401}

                     (and (closed-map? acquired #{:status :principal-id})
                          (= :authenticated (:status acquired))
                          (qualified-keyword? (:principal-id acquired)))
                     (if (and (= authorized-id (:principal-id acquired))
                              (= (:input/owner inputs)
                                 (get original-owners (:principal-id acquired)))
                              (contains? (get original-grants
                                              (:principal-id acquired) #{})
                                         operation))
                       ;; Grant/owner matching cannot be manufactured by the
                       ;; intent callback. It can only DENY a previously
                       ;; granted request, using that exact Ring request.
                       (let [intent-ok? (if original-verify-intent
                                          (original-verify-intent
                                           request (:principal-id acquired)
                                           operation)
                                          true)]
                         (cond
                           (true? intent-ok?)
                           {:type report-type :version report-version
                            :status :grant-and-typed-owner-matched :http-status 200}

                           (false? intent-ok?)
                           {:type report-type :version report-version
                            :status :request-intent-denied :http-status 403}

                           :else (reject! :invalid-request-intent-result)))
                       {:type report-type :version report-version
                        :status :operation-not-granted :http-status 403})

                     :else
                     (reject! :invalid-principal-acquisition-result))))]
    {:type report-type :version report-version
     :check-request! check! :recheck! recheck!}))

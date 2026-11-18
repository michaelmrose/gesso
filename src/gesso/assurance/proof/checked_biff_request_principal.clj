(ns gesso.assurance.proof.checked-biff-request-principal
  "v1786: Source-relative ONE-operation principal/owner correspondence before
   the v1781 checked HTTP -> typed/canonical execution trial.

   A trusted host acquisition callback sees the ACTUAL selected Ring request,
   returns a closed anonymous/authenticated result, and the policy must grant
   the selected operation to the principal and bind it to the exact typed
   :input/owner. Denials never invoke the original certified handler.

   The callback and grants are APPLICATION SUPPLIED. This module does not
   authenticate cookies, credentials or Ring adapters, audit callback purity,
   establish CSRF protection, or prove the policy is in the original catalog.
   No admission, certificate, transaction or retry capability is issued.")

(def report-type
  :gesso.assurance.proof.checked-biff-request-principal/local-request-gate)
(def report-version 1)

(defn- closed-map? [m ks]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))
       (= ks (set (keys m)))))

(defn- reject! [reason]
  (throw (ex-info "Checked source-relative request principal gate rejected."
                  {:checked-biff-request-principal/rejection true
                   :reason reason})))

(defn issue-request-principal-gate!
  "Issue one constrained principal grant for one typed operation/owner pair.
   Exactly one subject is supported here to avoid suggesting that the static
   trial can represent multiple distinct command owners. The trusted callback
   itself is NOT independently verified. Return a private invoke! gate, not a
   principal capability or a proof certificate."
  [policy operation inputs]
  (when-not (and (closed-map? policy #{:policy-id :principal-owners :grants
                                      :acquire-principal!})
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
        original-owners (:principal-owners policy)
        original-grants (:grants policy)
        policy-id (:policy-id policy)
        recheck! (fn []
                   (when-not (and (identical? original-acquire
                                              (:acquire-principal! policy))
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
                       {:type report-type :version report-version
                        :status :grant-and-typed-owner-matched :http-status 200}
                       {:type report-type :version report-version
                        :status :operation-not-granted :http-status 403})

                     :else
                     (reject! :invalid-principal-acquisition-result))))]
    {:type report-type :version report-version
     :check-request! check! :recheck! recheck!}))

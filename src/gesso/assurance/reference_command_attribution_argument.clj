(ns gesso.assurance.reference-command-attribution-argument
  "Selected conditional attribution support, not an authentication checker."
  (:require [clojure.java.io :as io]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-command-authority :as authority]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.http-route :as route])
  (:import (java.security MessageDigest) (java.util HexFormat)))

(def profile :conditional-selected-command-attribution-v1)
(def ^:private reviewed-sources
  {"gesso/live/optimistic/server.clj" "7e9c7f00bb29e3b89d11bdad16d31fd760af84029181d2eb5a07d2ce391b740c"
   "gesso/live/outbox/http_operation.clj" "70b1da6abd4da2c0a1c9296218535c16a9baa182783b7f0e7178c374a9bb3b49"
   "gesso/live/outbox/http_route.clj" "11bd506ff7a689719cbe96f43dd319f92d88f6ba4071eb497be3ce3d091ead79"
   "gesso/live/outbox/model_publication.clj" "dc7555035723c7e866b1f6760cdbf42addf7f36e6cd40d1031f1a4411cebdef5"
   "gesso/assurance/reference_mapped_submission.clj" "b39a750fd56d6ed02327ade107cf179cadd99c6b1d3fa6e33921da79a6672ccd"
   "gesso/assurance/reference_mapped_observed_recovery.clj" "69b76d6c9657b927ead4629a31d174097fe361aea227bbd20250de6078502ed9"
   "gesso/assurance/reference_command_authority.clj" "a092914c7a339a4b2fe424071c2d51e33858d8e336f7dc728c622a01eee964f5"})
(def ^:private boundaries
  (into {} (map (juxt identity deref))
        [#'authority/describe! #'assumption/describe-assumption
         #'route/authority-binding? #'route/authority-acceptance?
         #'identity/principal? #'identity/command-id? #'identity/execution-id?]))

(defn- require! [condition reason]
  (when-not condition
    (throw (ex-info "Unqualified selected command attribution" {:reason reason}))))

(defn- boundaries! []
  (require! (every? (fn [[v f]] (identical? f @v)) boundaries)
            :command-attribution-boundary-replaced)
  (doseq [[path digest] reviewed-sources]
    (let [resource (io/resource path)]
      (require! (and resource (= digest (with-open [in (io/input-stream resource)]
                                           (.formatHex (HexFormat/of)
                                                       (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in))))))
                :unreviewed-command-attribution-source))))

(defn- encode-origin [origin]
  (reduce (fn [m k]
            (update-in m [k :gesso.choreo.identity/value]
                       #(if (uuid? %) [:uuid (str %)] [:structural %])))
          origin [:principal :command-id :execution-id]))

(defn derive!
  "Structural support for an already owner-qualified original cut. Recheck
   the exact descriptions and selected origins. No report supplied here is
   an issued handle; provenance comes from the calling owner-locked join.
   Source review and structural correspondence do not prove the callback
   semantics, loaded code, or association of an aborted call to a log row."
  [command adopted]
  (boundaries!)
  (let [scope (:scope command)
        origins (:selected-command-origins command)
        ids (:selected-tx-ids scope)
        commitments (:selected-commitments scope)
        binding (:installation-binding adopted)
        declaration (:deployment-acceptance adopted)
        original (:open-premise-fingerprints command)
        [_ aborted recovered] origins
        relation (if (= (:execution-id aborted) (:execution-id recovered))
                   :same-observed-execution :distinct-observed-executions)]
    (require! (and (route/authority-binding? binding) (route/authority-acceptance? declaration)
                   (vector? ids) (= 3 (count ids)) (every? #(and (integer? %) (not (neg? %))) ids)
                   (apply < ids) (= [:committed :aborted :committed] commitments)
                   (vector? origins) (= 3 (count origins))
                   (every? #(and (identity/principal? (:principal %))
                                 (identity/command-id? (:command-id %))
                                 (identity/execution-id? (:execution-id %))
                                 (= binding (get-in % [:http-route :authority-binding]))) origins)
                   (= (mapv encode-origin origins) (:selected-command-origins scope))
                   (= relation (:abort-recovery-execution command) (:abort-recovery-execution scope))
                   (every? #(= % (assumption/describe-assumption (:assumption %))) (:obligations command)))
              :command-attribution-origin-mismatch)
    (let [description (authority/describe! command (:policy binding))
          expected (assoc description :status :conditional-configured-command-authority
                          :subject-provenance :matched-original-issued-cut
                          :configured-authority :matched-current-local-installation
                          :installation-binding binding :deployment-adoption :configured-local-host
                          :premise-acceptance :explicit-declaration-only
                          :trusted-assumptions (:open-premise-fingerprints description)
                          :deployment-acceptance declaration)
          by-id (into {} (map (fn [p] [(get-in p [:assumption :id]) (:premise-fingerprint p)]))
                      (:obligations description))
          gate (by-id :reference-command-authority/request-gate)
          session (by-id :reference-command-authority/principal-session)]
      (require! (= adopted expected) :command-attribution-adoption-mismatch)
      (boundaries!)
      {:profile profile :status :conditional-selected-command-attribution-support
       :scope scope :installation-binding binding
       :structural-evidence :matched-selected-origin-subject
       :reviewed-sources reviewed-sources
       :selected-associations
       (mapv (fn [origin commitment]
               {:origin origin :commitment commitment
                :association (if (= :aborted commitment)
                               :observed-order-and-retained-payload-not-receipt
                               :matched-returned-transaction-key)})
             (:selected-command-origins scope) commitments)
       :abort-recovery-execution relation
       :conditional-conclusion
       {:request-gate {:proposition :selected-requests-satisfy-declared-gate-policy
                       :conditional-on #{gate}
                       :remaining-conditions #{:faithful-reviewed-command-and-loaded-code
                                               :faithful-issued-command-observation-and-retained-payload-attribution}}
        :principal-resolution {:proposition :selected-resolved-principals-authenticated-by-declared-policy
                               :conditional-on #{session}
                               :remaining-conditions #{:faithful-reviewed-command-and-loaded-code
                                                       :faithful-issued-command-observation-and-retained-payload-attribution}}
        :command-attribution {:proposition :selected-command-bindings-attributed-to-resolved-principals
                              :conditional-on #{gate session}
                              :remaining-conditions #{:faithful-reviewed-command-and-loaded-code
                                                      :faithful-issued-command-observation-and-retained-payload-attribution
                                                      :faithful-aborted-call-ordering-association}}}
       :trusted-assumptions #{gate session}
       :remaining-implementation-conditions
       #{:faithful-reviewed-command-and-loaded-code
         :faithful-issued-command-observation-and-retained-payload-attribution
         :faithful-aborted-call-ordering-association}
       :original-open-premise-fingerprints original
       :support-semantics :conditional-not-entailment-checker
       :command-authentication :not-established :aborted-receipt-identity :not-established
       :universal-refinement :not-established :proof-discharge :not-performed
       :authority :none :certificate :not-issued})))

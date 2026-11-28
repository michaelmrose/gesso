(ns gesso.assurance.reference-writer-classification
  "Closed declared alternatives and reviewed mapped-policy branch inventory.
   Source coverage is not physical writer or temporal coverage."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-source-transition-argument :as commit]
            [gesso.assurance.reference-read-only-replay-argument :as replay]
            [gesso.assurance.reference-transition-argument :as transition])
  (:import (java.security MessageDigest) (java.util HexFormat)))

(def profile :conditional-declared-writer-classification-v1)
(def ^:private check-catalog projection/check-catalog!)
(def ^:private derive-commit commit/derive!)
(def ^:private derive-replay replay/derive!)
(def ^:private derive-transition transition/derive!)
(def ^:private reviewed-sources
  {"gesso/live/outbox/writer_policy.clj" "2007cb7232625c6119b658a979455313cc8fab25d49b25d0ac8e489664c7ac74"
   "gesso/live/outbox/delivery.clj" "04e2190e1acf265d1e67a4fd943ef2180fc87e283fa6725d46401e6a363cd58f"
   "gesso/live/core.clj" "d009b5bfa16ae6c0b3d4ab1795fc219be3738892104b8e8bafba1fa144865f27"
   "gesso/live/source.clj" "4afe52d94d00042cca32f2611d9fcc0f2b044a84cc0c9fbd256c941c887176dc"
   "gesso/live/dispatch.clj" "c9630e375f45a047f291fcd7927de5f472d3848553aadbe997197107bd25c2c2"
   "gesso/live/outbox/maintenance_policy.clj" "59e7463ada951a30944d189913f29399fd3217e6676c93b1ac3eb4db8490109b"
   "gesso/live/outbox/initialization.clj" "36aac083e410e05b806137bf4540559f1ef3d77d4284dc6f387604aff3d81de7"
   "gesso/live/outbox/host.clj" "6029bb3c062a76d40a6ada1f77881ceda7367481f7fd2a6263253dfa1993dc6d"
   "gesso/live/outbox/lifecycle.clj" "05d489b78f09b3e5bc7cdc130753b1d54e82ccd1653595b18568d9b1a14a6b7c"
   "gesso/live/outbox/worker_capture.clj" "7ac234b08e4bf64a78366b1b633036ab7484cb81468a4ed30ef62a0847de3c58"
   "gesso/live/outbox/biff.clj" "0a53c933f9c0f8c5cff6e60825cce396d8e49e31a552af5a20a7391d76020e1a"
   "gesso/live/consistency/xtdb.clj" "2f191e2cdba15d7ce2ce07da02bc35671b44babb713bd85fce01c6d396156697"})

(defn- reject! [reason]
  (throw (ex-info "Unqualified declared writer classification" {:reason reason})))

(defn- source-digest [path]
  (when-let [r (io/resource path)]
    (with-open [in (io/input-stream r)]
      (.formatHex (HexFormat/of)
                  (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in))))))

(defn- boundaries! []
  (when-not (and (identical? check-catalog projection/check-catalog!)
                 (identical? derive-commit commit/derive!)
                 (identical? derive-replay replay/derive!)
                 (identical? derive-transition transition/derive!))
    (reject! :classification-boundary-replaced))
  (when-not (every? (fn [[path digest]] (= digest (source-digest path))) reviewed-sources)
    (reject! :unreviewed-writer-policy-source)))

(defn- mapped-tables [coordinates]
  (into #{:gesso_commands}
        (keep (fn [[kind table]] (when (#{:document :nullable-document} kind) table)))
        (vals coordinates)))

(defn- policy-branches! [protected]
  ;; This inventory is a review of the pinned policy, not a fresh host survey.
  ;; Initialization is a root obligation, not an induction stutter step.
  (let [manifest #{:gesso_publication_manifests}
        worker #{:gesso_live_outbox}
        reserved (set/union manifest worker #{:gesso_initializations :gesso_transition_evidence})]
    (when (seq (set/intersection protected reserved))
      (reject! :protected-infrastructure-overlap))
    {:mapped-operation {:policy-kind :mapped-operation :effect :conditional-abstract-transition
                        :requires :exact-framed-handoff}
     :initializer {:policy-kind :initializer :effect :initial-root
                   :requires :guarded-seed-and-pinned-initial-readback}
     :manifest {:policy-kind :manifest :effect :infrastructure-establishment
                :write-tables manifest :requires :exact-source-manifest-establishment}
     :lease-acquisition {:policy-kind :worker :effect :conditional-mapped-stutter
                         :write-tables worker :activation :current-activated-generation :installed-origin :matched-scheduled-maintenance :installed-lineage :current-managed-lifecycle :requires :pending-row-cas-and-clock}
     :lease-reclaim {:policy-kind :worker :effect :conditional-mapped-stutter
                     :write-tables worker :activation :current-activated-generation :installed-origin :matched-scheduled-maintenance :installed-lineage :current-managed-lifecycle :requires :expired-lease-cas-and-clock}
     :dispatch-acknowledgment {:policy-kind :worker :effect :conditional-mapped-stutter
                               :write-tables worker :activation :current-activated-generation :installed-origin :matched-scheduled-maintenance :installed-lineage :current-managed-lifecycle :requires :current-lease-cas-and-receipt}}))

(defn- classify! [checked writes reads generic]
  (let [mappings (:operation-mappings checked)
        w (:alternatives writes) r (:alternatives reads)
        ids (set (keys mappings)) wi (set (keys w)) ri (set (keys r))
        expected-w (into #{} (keep (fn [[id m]] (when (seq (:write-coordinates m)) id))) mappings)]
    (when-not (and (= :matched (:status checked))
                   (= :conditional-catalog-commit-composition (:status writes))
                   (= :conditional-catalog-replay-derivation (:status reads))
                   (= :conditional-equational-derivation (:status generic))
                   (= :not-performed (:proof-discharge checked) (:proof-discharge writes)
                      (:proof-discharge reads) (:proof-discharge generic))
                   (= (:catalog-id checked) (:catalog-id writes) (:catalog-id reads))
                   (= (:catalog-fingerprint checked) (:catalog-fingerprint writes) (:catalog-fingerprint reads))
                   (seq wi) (seq ri) (= ids (set/union wi ri))
                   (empty? (set/intersection wi ri))
                   (= wi expected-w (set (vals (:case-alternatives checked))))
                   (= ri (:outside-fragment writes)))
      (reject! :incomplete-declared-alternative-partition))
    (doseq [[id a] (merge w r)]
      (let [m (mappings id) identity (:identity a)]
        (when-not (and (= id (:alternative-id identity))
                       (= (:catalog-id checked) (:catalog-id identity))
                       (= (:catalog-fingerprint checked) (:catalog-fingerprint identity))
                       (= (:mapping-fingerprint m) (:mapping-fingerprint identity))
                       (if (wi id)
                         (and (= :conditional-committed-transition-derivation (:status a))
                              (= id (get (:case-alternatives checked) (:case-key a))))
                         (and (= :conditional-read-only-transition-derivation (:status a))
                              (= #{} (:writes a)) (= {} (:write-coordinates m))
                              (= (:field-coordinates checked) (:frame-coordinates m))
                              (let [origin (w (get-in a [:origin :alternative-id]))]
                                (and origin (= (:identity origin) (:origin a))
                                     (= (:case-key origin) (:case-key a)))))))
          (reject! :classification-alternative-identity-mismatch))))
    (let [protected (mapped-tables (:field-coordinates checked))]
      {:declared-alternative-coverage :complete
       :journal-writing-alternatives wi :read-only-alternatives ri
       :alternative-identities (into {} (map (fn [[id a]] [id (:identity a)])) (merge w r))
       :protected-tables protected :reviewed-policy-branches (policy-branches! protected)
       :physical-step-classes {:committed-mapped :conditional-abstract-transition
                               :aborted-attempt :conditional-stutter
                               :committed-worker :conditional-mapped-stutter
                               :initialization :initial-root
                               :manifest :infrastructure-establishment}
       :nontransaction-observations #{:read-only-replay :locally-refused-submission}
       :remaining-conditions (set/union (:remaining-conditions checked)
                                        (:remaining-conditions writes) (:remaining-conditions reads)
                                        (disj (:remaining-general-conditions generic)
                                              :source-derived-put-image-satisfies-original-abstract-relation)
                                        #{:faithful-reviewed-policy-and-loaded-code
                                          :all-physical-writers-mediated-by-exact-installed-policy
                                          :exact-admitted-envelope-and-alternative-attribution
                                          :qualified-initialization-and-manifest-root
                                          :complete-committed-aborted-and-temporal-boundary-classification
                                          :maintenance-exact-footprint-and-provider-effect-correspondence
                                          :complete-admitted-writer-and-temporal-boundary-coverage})})))

(defn derive!
  "Re-derive the declared partition and pin the reviewed policy inventory.
   No report is an admission credential; raw writers and temporal effects
   are still open even if every declared alternative is classified."
  [source declarations fingerprint cases]
  (let [base {:profile profile :scope :original-declared-catalog-and-reviewed-policy-only
              :writer-coverage :not-established :universal-refinement :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [args [source declarations fingerprint cases]
            result (classify! (apply check-catalog args) (apply derive-commit args)
                              (apply derive-replay args) (derive-transition))]
        (boundaries!)
        (merge base result {:status :conditional-declared-writer-classification
                            :catalog-fingerprint fingerprint :reviewed-sources reviewed-sources}))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-classification))))))

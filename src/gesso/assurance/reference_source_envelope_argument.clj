(ns gesso.assurance.reference-source-envelope-argument
  "Conditional composition of original write relations and exact envelope handoff."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [gesso.assurance.reference-source-transition-argument :as source-transition]
            [gesso.assurance.reference-mapped-evidence :as evidence]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.assurance.reference-mapped-input :as input]
            [gesso.assurance.reference-mapped-publication :as declared-publication]
            [gesso.assurance.reference-mapped-residual :as residual]
            [gesso.assurance.proof.mapped-prestate-lowering :as lowering]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.live.outbox.model-publication :as publication])
  (:import (java.security MessageDigest) (java.util HexFormat)))

(def profile :conditional-source-envelope-argument-v1)
(def ^:private original-transition source-transition/derive!)
(def ^:private original-check evidence/check-executor-envelope)
(def ^:private original-record evidence/derive-record!)
(def ^:private original-writes writes/derive-transition!)
(def ^:private original-puts writes/derive-puts!)
(def ^:private original-frequencies writes/put-row-frequencies!)
(def ^:private original-input input/compare-selected!)
(def ^:private original-publication declared-publication/derive-publication!)
(def ^:private original-publication-check declared-publication/check-publication!)
(def ^:private original-residual residual/derive-assertions!)
(def ^:private original-residual-check residual/check-assertions!)
(def ^:private original-prefix lowering/check-prefix!)
(def ^:private original-bound? envelope/bound-transition-envelope?)
(def ^:private original-claim publication/claim-framed-submission!)
(def ^:private binding-fields [:envelope-profile :envelope-fingerprint])

(def ^:private reviewed-sources
  {"gesso/live/outbox/model_publication.clj" "dc7555035723c7e866b1f6760cdbf42addf7f36e6cd40d1031f1a4411cebdef5"
   "gesso/assurance/xtdb_envelope.clj" "71f672cb580dcc420e0817cefd47f83a2746280b2f51e281d5de59ef560ad2f0"
   "gesso/assurance/proof/mapped_prestate_lowering.clj" "70ded1919b2373a0226e36429c8f028b77dffc1148c8d5487d58a750f8fd315b"
   "gesso/assurance/reference_mapped_evidence.clj" "9ed58db54fe1ac980cd68c03774f04465e3aa0a2f519fa748489ef1fd4da54df"
   "gesso/assurance/reference_source_transition_argument.clj" "20298a6c056b189592fd1d58782b65343d5ba9a5ea452f0c9442ec075d90fe54"})

(def ^:private base
  {:profile profile :authority :none :certificate :not-issued
   :provider-refinement :not-established :universal-refinement :not-established
   :writer-coverage :not-established :proof-discharge :not-performed})

(defn- reject! [reason]
  (throw (ex-info "Unqualified source-envelope argument" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-transition source-transition/derive!)
                 (identical? original-check evidence/check-executor-envelope)
                 (identical? original-record evidence/derive-record!)
                 (identical? original-writes writes/derive-transition!)
                 (identical? original-puts writes/derive-puts!)
                 (identical? original-frequencies writes/put-row-frequencies!)
                 (identical? original-input input/compare-selected!)
                 (identical? original-publication declared-publication/derive-publication!)
                 (identical? original-publication-check declared-publication/check-publication!)
                 (identical? original-residual residual/derive-assertions!)
                 (identical? original-residual-check residual/check-assertions!)
                 (identical? original-prefix lowering/check-prefix!)
                 (identical? original-bound? envelope/bound-transition-envelope?)
                 (identical? original-claim publication/claim-framed-submission!)
                 (= binding-fields envelope/binding-keys))
    (reject! :source-envelope-boundary-replaced))
  (doseq [[path digest] reviewed-sources]
    (let [resource (io/resource path)]
      (when-not (and resource (= digest (with-open [in (io/input-stream resource)]
                                          (.formatHex (HexFormat/of)
                                                      (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in))))))
        (reject! :unreviewed-source-envelope-artifact)))))

(defn derive!
  "Specialize the envelope premises to each original journal-writing relation.
   This is a conditional schema, not an assertion that every admitted step
   instantiates it. Source hashes and Var identity are not runtime attestation."
  [source declarations fingerprint cases]
  (try
    (boundaries!)
    (let [report (original-transition source declarations fingerprint cases)]
      (when-not (and (= :conditional-catalog-commit-composition (:status report))
                     (= fingerprint (:catalog-fingerprint report))
                     (seq (:alternatives report)))
        (reject! :unqualified-source-transition))
      (boundaries!)
      (assoc base :status :conditional-source-envelope-schema
             :catalog-fingerprint fingerprint :catalog-id (:catalog-id report)
             :alternatives (into {} (map (fn [[id a]]
                                          [id (select-keys a [:identity :case-key :source-equation-fact :goal :equalities])]))
                                 (:alternatives report))
             :outside-fragment (:outside-fragment report)
             :reviewed-sources reviewed-sources
             :required-envelope-equations
             {:unbound [:prepend-evidence-absence [:append-witness :admitted-ordered-ops :source-witness]]
              :unbound-from-bound [:strip-final-witness-fields :bound-ordered-ops binding-fields]
              :consumed :bound-ordered-ops}
             :remaining-conditions
             (set/union (:remaining-conditions report)
                        #{:faithful-source-relative-guard-and-write-lowering
                          :faithful-declaration-and-input-selector-provenance
                          :faithful-reviewed-source-and-loaded-envelope-code
                          :exact-admitted-envelope-and-alternative-attribution
                          :faithful-command-and-envelope-attribution
                          :faithful-one-use-consumption-and-installed-policy-mediation
                          :disjoint-evidence-extension-and-physical-identifiers})))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-source-envelope)))))

(def ^:private original-schema derive!)

(defn check-instance!
  "Independently qualify the source-relative envelope of a supplied instance.
   The admitted and consumed vectors are supplied diagnostic values, not
   issued handles or observations of one-use execution. Preserve actual order
   across the witness extension even when the source checker permits batching
   or residual assertion permutations. No provider commit is implied."
  [source declarations fingerprint cases id
   {:keys [docs journal spec context source-id prefix-count admitted witness bound consumed] :as instance}]
  (try
    (when-not (identical? original-schema derive!)
      (reject! :source-envelope-boundary-replaced))
    (let [schema (original-schema source declarations fingerprint cases)
          alternative (get-in schema [:alternatives id])
          original (:construction (first (filter #(= id (:id %)) (get-in source [:inductive-source :catalog :operations]))))
          targets (:target-map source)
          binding (:binding spec)]
      (when-not (and (= :conditional-source-envelope-schema (:status schema)) alternative
                     (= #{:docs :journal :spec :context :source-id :prefix-count :admitted :witness :bound :consumed}
                        (set (keys instance)))
                     (= fingerprint (:catalog-fingerprint context))
                     (= id (:alternative spec)) (= cases (:outcome-cases spec))
                     (map? (:document-effects source)) (contains? (:document-effects source) id)
                     (= (get-in source [:document-effects id]) (:effects spec))
                     (= (:case-key alternative) [(identity/raw-value (:principal binding)) (:operation binding)
                                                 (:arguments binding) (:outcome spec)])
                     (vector? admitted) (integer? prefix-count) (<= 1 prefix-count (count admitted) 16384)
                     (map? witness) (not-any? #(contains? witness %) binding-fields)
                     (vector? bound) (vector? consumed) (= bound consumed)
                     (= (+ 2 (count admitted)) (count bound))
                     (vector? (peek bound)) (= 3 (count (peek bound)))
                     (map? (peek (peek bound))))
        (reject! :source-envelope-instance-mismatch))
      (let [unbound (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence WHERE _id = ?)" (:xt/id witness)]]
                          (conj admitted [:put-docs :gesso_transition_evidence witness]))
            prefix (subvec admitted 0 prefix-count)
            guard (original-prefix original targets docs journal prefix)]
        (when-not (and (= unbound (update-in bound [(dec (count bound)) 2] #(apply dissoc % binding-fields)))
                       (original-bound? bound))
          (reject! :source-envelope-extension-mismatch))
        (when-not (and (= :checked-exact-prefix (:status guard))
                       (= (get-in alternative [:identity :mapping-fingerprint]) (:mapping-fingerprint guard))
                       (= :matched (:status (original-check original targets docs journal {:ops bound} prefix-count spec context
                                                          source-id (:publication-derivations source) (:residual-assertion-derivations source)))))
          (reject! :source-envelope-lowering-mismatch))
        (when-not (= :matched (:status (original-input source original targets docs journal {:ops bound} spec context)))
          (reject! :source-envelope-input-mismatch))
        (boundaries!)
        (when-not (identical? original-schema derive!)
          (reject! :source-envelope-boundary-replaced))
        (assoc base :status :conditional-source-envelope-instance
               :identity (:identity alternative) :case-key (:case-key alternative)
               :source-equation-fact (:source-equation-fact alternative)
               :guard-correspondence (:formatter-correspondence guard)
               :input-correspondence :matched-source-relative-selectors
               :induction-fingerprint-provenance :not-established
               :ordered-envelope :matched-source-relative-witness-extension
               :consumption-correspondence :matched-supplied-vector-only
               :remaining-conditions (set/union (:remaining-conditions schema) (:open-obligations guard)))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-source-envelope-instance)))))

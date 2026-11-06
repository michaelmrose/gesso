(ns gesso.assurance.proof.operation-interface
  "Source-bound, CONDITIONAL semantic operation interfaces (v1442).

   This is a projection of the exact, checked v1440 DECLARED application
   dependency graph onto a set of typed operation contracts. Each interface
   carries its original guard, state/input types, read/write/frame footprint,
   source and transition identity, individual conditional preservation steps,
   and all original open proof/physical assumptions. The common catalog and
   invariant results remain visible; no checked step discharges its original
   premise.

   Only a live v1440 structural closure together with fresh ORIGINAL source
   and a fresh, exact graph may produce this copyable report. Neither the
   report nor its fingerprint constitutes a proof token, runtime admission,
   an authenticated installed-writer catalog, or a certificate. The trusted
   JVM boundary and v1440 local-handle threat model are unchanged."
  (:require [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def interface-version 1)
(def interface-type
  :gesso.assurance.proof.operation-interface/conditional-semantic-interfaces)
(def ^:private identity-id
  :gesso.assurance.proof/conditional-operation-interfaces-v1)

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso conditional semantic operation interfaces."
                  (merge {:error/type ::invalid-operation-interface
                          :reason reason} details))))

(defn- raw-fingerprint [id original]
  (:fingerprint
   (subject/describe-subject
    {:kind :rule :id id :interface {:original-source original}})))

(defn- role-nodes [graph role]
  (filterv #(= role (get-in % [:subject :interface :role]))
           (:nodes graph)))

(defn- exact-by-id! [nodes role key expected]
  (let [ids (mapv #(get-in % [:subject :interface key]) nodes)
        actual (set ids)]
    (when-not (and (= expected actual)
                   (= (count ids) (count actual))
                   (= (count expected) (count ids)))
      (reject! :dependency-role-coverage-mismatch
               {:role role :expected expected :actual actual}))
    (into {} (map (fn [n]
                    [(get-in n [:subject :interface key])
                     (get-in n [:subject :interface])]))
          nodes)))

(defn- invariant-result! [invariant-id invariant-evidence operation-id]
  (let [{:keys [checked-step]} (get invariant-evidence invariant-id)
        step (get-in checked-step [:preservation-by-operation operation-id])]
    (when-not (and (= invariant-id (:invariant-id checked-step))
                   (= #{:obligation-id :status} (set (keys step)))
                   (#{:checked-conditional-inference :open} (:status step))
                   (#{:checked-conditional-inference :open}
                    (get-in checked-step [:initial :status]))
                   (#{:checked-conditional-induction :open}
                    (:induction-status checked-step))
                   (= (= :checked-conditional-induction
                         (:induction-status checked-step))
                      (and (= :checked-conditional-inference
                              (get-in checked-step [:initial :status]))
                           (every? #(= :checked-conditional-inference
                                       (:status %))
                                   (vals (:preservation-by-operation
                                          checked-step))))))
      (reject! :invalid-source-derived-invariant-step
               {:operation-id operation-id :invariant-id invariant-id}))
    {:initial-status (get-in checked-step [:initial :status])
     :preservation-status (:status step)
     :induction-status (:induction-status checked-step)
     :open-reason (:open-reason checked-step)}))

(defn- original-operation-interface!
  [catalog-id catalog-fingerprint invariant-ids original-op
   operation-evidence invariant-evidence open-meta operation-id]
  (let [source (:construction original-op)
        built (construction/build-guarded-replacement! source)
        description (abstract/describe-transition! (:transition built))
        semantics (:semantics description)
        evidence (get operation-evidence operation-id)
        obligations (:original-open-obligation-ids evidence)
        conditional (into (sorted-map-by #(compare (str %1) (str %2)))
                          (map (fn [id]
                                 [id (invariant-result! id invariant-evidence
                                                        operation-id)]))
                          (sort-by str invariant-ids))]
    (when-not (and (= operation-id (:operation-id original-op)
                            (:operation-id evidence)
                            (get-in source [:subject :id]))
                   (= (:reads built) (:reads semantics) (:reads source))
                   (= (:writes built) (:writes semantics) (:writes source))
                   (= (:frame built) (:frame-fields semantics) (:frame source))
                   (set? obligations)
                   (= catalog-fingerprint
                      (:source-catalog-fingerprint evidence))
                   (= invariant-ids (set (keys conditional))))
      (reject! :operation-evidence-correspondence-mismatch
               {:operation-id operation-id}))
    {:operation-id operation-id
     :catalog-id catalog-id
     :catalog-fingerprint catalog-fingerprint
     :source-fingerprint (raw-fingerprint operation-id source)
     :subject-fingerprint (:subject-fingerprint description)
     :transition-fingerprint (:fingerprint description)
     :state-types (:state-types semantics)
     :input-types (:input-types semantics)
     :guard (:guard semantics)
     :reads (:reads semantics)
     :writes (:writes semantics)
     :frame (:frame-fields semantics)
     :assignments (:assignments semantics)
     :conditional-invariant-results conditional
     :original-open-obligation-ids obligations
     :open-meta-obligations open-meta
     :contract-scope :declared-abstract-guarded-transition-only
     :guarantee-status :conditional-inference-or-open
     :physical-writer-coverage :not-established
     :implementation-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none}))

(defn derive-operation-interfaces!
  "Derive ALL operation interfaces from fresh ORIGINAL catalog, current graph,
   and its authentic v1440 source-closure handle. Returned data are untrusted
   summaries: their status and source digests are not independent credentials."
  [catalog current-graph closure]
  (app/require-current-application-dependencies!
   closure catalog current-graph)
  (let [summary (app/describe-application-dependencies closure)
        operation-ids (:operation-ids summary)
        invariant-ids (:invariant-ids summary)
        original-ops (exact-by-id!
                      (role-nodes current-graph :declared-abstract-operation)
                      :declared-abstract-operation :operation-id operation-ids)
        evidence (exact-by-id!
                  (role-nodes current-graph :conditional-operation-evidence)
                  :conditional-operation-evidence :operation-id operation-ids)
        invariants (exact-by-id!
                    (role-nodes current-graph :conditional-invariant-evidence)
                    :conditional-invariant-evidence :invariant-id invariant-ids)
        originals (:original-open-obligation-ids-by-operation summary)
        meta-obligations (:open-meta-obligations summary)
        fingerprint (:catalog-fingerprint summary)]
    (when-not (and (= operation-ids (set (keys originals)))
                   (set? meta-obligations)
                   (contains? meta-obligations :implementation-correspondence)
                   (contains? meta-obligations :physical-write-path-coverage)
                   (every? (fn [id]
                             (= (get originals id)
                                (get-in evidence
                                        [id :original-open-obligation-ids])))
                           operation-ids))
      (reject! :original-premise-coverage-mismatch {}))
    (let [interfaces
          (into (sorted-map-by #(compare (str %1) (str %2)))
                (map (fn [id]
                       [id (original-operation-interface!
                            (:catalog-id summary) fingerprint invariant-ids
                            (get original-ops id) evidence invariants
                            meta-obligations id)]))
                (sort-by str operation-ids))
          identity (subject/describe-subject
                    {:kind :rule :id identity-id
                     :interface {:version interface-version
                                 :catalog-id (:catalog-id summary)
                                 :catalog-fingerprint fingerprint
                                 :induction-fingerprint
                                 (:induction-fingerprint summary)
                                 :interfaces interfaces
                                 :open-meta-obligations meta-obligations
                                 :open-assumptions (:open-assumptions summary)}})]
      {:type interface-type
       :version interface-version
       :fingerprint (:fingerprint identity)
       :catalog-id (:catalog-id summary)
       :catalog-fingerprint fingerprint
       :induction-fingerprint (:induction-fingerprint summary)
       :operation-ids operation-ids
       :invariant-ids invariant-ids
       :interfaces interfaces
       :original-open-obligation-ids-by-operation originals
       :open-meta-obligations meta-obligations
       :open-assumptions (:open-assumptions summary)
       :scope :closed-declared-abstract-machine-only
       :proof-status :source-bound-conditional-operation-interfaces
       :assumption-discharge :not-performed
       :physical-writer-coverage :not-established
       :implementation-correspondence :not-established
       :certificate :not-issued
       :admission :not-admitted
       :authority :none})))

(defn require-current-operation-interfaces!
  "Reject copied/altered/stale interface reports by requiring fresh exact
   source-closure validation and comparing with recomputed interfaces. Never
   converts a report into an admission handle, certificate, or proof."
  [prior current-catalog current-graph closure]
  (let [now (derive-operation-interfaces!
             current-catalog current-graph closure)]
    (when-not (and (map? prior) (not (record? prior))
                   (nil? (meta prior)) (= prior now))
      (reject! :stale-or-forged-operation-interfaces
               {:expected (:fingerprint now)
                :actual (:fingerprint prior)}))
    now))

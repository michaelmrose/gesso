(ns gesso.assurance.proof.application-assurance-report
  "v1448: one NONAUTHORITATIVE summary of source-derived conditional assurance.

   Combine the live v1440 declared-source closure, v1443 operation-interface
   composition, v1444 provenance and v1446 observed-writer accounting. The
   public path RECOMPUTES these views from their respective original sources;
   it never accepts a caller-supplied proof, composition, manifest or writer
   success map as authority.

   Agreement of fingerprints and operation IDs is structural evidence, not
   proof of hidden-writer absence, installed handler equivalence, XTDB
   execution refinement, independent kernel soundness or browser behavior.
   A matching serialized report grants no admission, certificate or authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.application-dependency :as dependency]
            [gesso.assurance.proof.application-writer-coverage :as writer]
            [gesso.assurance.proof.operation-interface-composition :as composition]
            [gesso.assurance.proof.operation-interface-provenance :as provenance]
            [gesso.assurance.subject :as subject]))

(def report-version 1)
(def report-type
  :gesso.assurance.proof.application-assurance-report/conditional-source-report)
(def ^:private report-id
  :gesso.assurance.proof/conditional-application-assurance-report-v1)

(def ^:private permanent-open-obligations
  #{:independent-proof-kernel-soundness
    :complete-installed-application-adoption
    :actual-physical-xtdb-writer-completeness
    :universal-abstract-physical-transition-refinement
    :browser-htmx-sse-implementation-correspondence
    :certification-and-admission-not-established})

(defn- reject! [reason details]
  (throw (ex-info "Invalid Gesso conditional application assurance report."
                  (merge {:error/type ::invalid-application-assurance-report
                          :reason reason} details))))

(defn- ordinary-map? [x]
  (and (map? x) (not (record? x)) (nil? (meta x))))

(defn- exact-nonauthority! [report fields]
  (when-not (and (ordinary-map? report)
                 (every? (fn [[field expected]]
                           (= expected (get report field ::absent)))
                         fields))
    (reject! :untrusted-or-authoritative-upstream-evidence
             {:upstream-type (:type report)})))

(defn- checked-summary! [declared composed manifest coverage]
  ;; This pure join accepts already-computed snapshots only as DATA. The
  ;; public entry point below is responsible for their fresh acquisition.
  (exact-nonauthority!
   declared {:type dependency/closure-type
             :version dependency/closure-version
             :assumption-discharge :not-performed
             :installed-writer-coverage :not-established
             :implementation-correspondence :not-established
             :certificate :not-issued :admission :not-admitted :authority :none})
  (exact-nonauthority!
   composed {:type composition/composition-type
             :version composition/composition-version
             :assumption-discharge :not-performed
             :physical-writer-coverage :not-established
             :implementation-correspondence :not-established
             :concurrency-refinement :not-established
             :certificate :not-issued :admission :not-admitted :authority :none})
  (exact-nonauthority!
   manifest {:type provenance/provenance-type
             :version provenance/provenance-version
             :assumption-discharge :not-performed
             :installed-writer-coverage :not-established
             :implementation-correspondence :not-established
             :certificate :not-issued :admission :not-admitted :authority :none})
  (exact-nonauthority!
   coverage {:type writer/coverage-type
             :version writer/coverage-version
             :assumption-discharge :not-performed
             :physical-writer-coverage :not-established
             :abstract-to-physical-correspondence :not-established
             :deployment-correspondence :not-established
             :universal-kernel-soundness :not-established
             :certificate :not-issued :admission :not-admitted
             :transaction-authority :none :authority :none})
  (let [lineage (:lineage manifest)
        ids (:operation-ids declared)
        invs (:invariant-ids declared)
        contracts (:operation-contracts composed)
        invariant-results (:invariant-results composed)
        original (:original-open-obligation-ids-by-operation declared)
        meta-obligations (:open-meta-obligations declared)
        assumptions (:open-assumptions declared)
        source-id (:catalog-id declared)
        source-fingerprint (:catalog-fingerprint declared)
        induction-fingerprint (:induction-fingerprint declared)
        ledger-fingerprint (:ledger-fingerprint declared)
        operation-provenance (:operations lineage)
        invariant-provenance (:invariants lineage)]
    (when-not
     (and (set? ids) (seq ids) (set? invs)
          (set? meta-obligations)
          (set? (:open-obligations coverage))
          (= ids (set (keys contracts))
             (set (keys original))
             (:operation-ids composed)
             (set (keys operation-provenance))
             (:declared-operation-ids coverage))
          (= invs (:invariant-ids composed)
             (set (keys invariant-results))
             (set (keys invariant-provenance)))
          (= source-id (:catalog-id composed) (:catalog-id lineage)
             (:catalog-id coverage))
          (= source-fingerprint (:catalog-fingerprint composed)
             (:catalog-fingerprint lineage)
             (:catalog-fingerprint coverage))
          (= induction-fingerprint (:induction-fingerprint composed)
             (:induction-fingerprint lineage)
             (:induction-fingerprint coverage))
          (= ledger-fingerprint (:induction-ledger-fingerprint lineage)
             (:application-dependency-ledger-fingerprint coverage))
          (= (:fingerprint composed) (:composition-fingerprint lineage))
          (= (:interface-fingerprint composed)
             (:operation-interface-fingerprint lineage))
          (= original (:original-open-obligation-ids-by-operation composed)
             (:original-open-obligation-ids-by-operation lineage)
             (:original-open-obligation-ids-by-operation coverage))
          (= meta-obligations (:open-meta-obligations composed)
             (:open-meta-obligations lineage))
          (= assumptions (:open-assumptions composed)
             (:open-assumptions lineage)
             (:open-assumptions coverage))
          (every? (fn [[id contract]]
                    (and (= id (:operation-id contract))
                         (= :not-established (:physical-writer-coverage contract))
                         (= :not-established (:implementation-correspondence contract))
                         (= :not-issued (:certificate contract))
                         (= :not-admitted (:admission contract))
                         (= :none (:authority contract))
                         (= (:source-fingerprint contract)
                            (get-in operation-provenance [id :source-fingerprint]))
                         (= (:transition-fingerprint contract)
                            (get-in operation-provenance [id :transition-fingerprint]))))
                  contracts)
          (every? (fn [[id premise-ids]]
                    (and (set? premise-ids)
                         (= premise-ids
                            (get-in operation-provenance
                                    [id :original-open-obligation-ids]))
                         (= premise-ids
                            (get-in contracts [id :original-open-obligation-ids]))))
                  original)
          (every? (fn [[id result]]
                    (and (#{:checked-conditional-induction :open}
                          (:induction-status result))
                         (= (:induction-status result)
                            (get-in invariant-provenance [id :induction-status]))
                         (= ids (set (keys (:preservation-by-operation result))))))
                  invariant-results)
          (= (:node-count declared) (get-in lineage [:graph :node-count]))
          (= (:edge-count declared) (get-in lineage [:graph :edge-count]))
          (set? (:observed-unclassified-entry-point-ids coverage))
          (= (:exact-observed-operation-id-join? coverage)
             (and (= ids (:independent-operation-ids coverage))
                  (= ids (:issued-operation-ids coverage)))))
      (reject! :cross-source-or-premise-coverage-mismatch {}))
    (let [op-premises (reduce set/union #{} (vals original))
          invariant-open (set (for [[id result] invariant-results
                                    :when (= :open (:induction-status result))]
                                id))
          writer-accounting
          (select-keys coverage
                       [:status :declared-vs-independent :declared-vs-issued
                        :independent-vs-issued :independent-operation-ids
                        :issued-operation-ids :operation-rows
                        :observed-entry-point-ids
                        :observed-unclassified-entry-point-ids
                        :exact-observed-operation-id-join?])
          coverage-open? (or (not (:exact-observed-operation-id-join? coverage))
                             (seq (:observed-unclassified-entry-point-ids coverage))
                             (not= :conditional-observed-operation-id-join
                                   (:status coverage)))
          open (set/union permanent-open-obligations meta-obligations
                          op-premises (:open-obligations coverage)
                          (when (seq invariant-open)
                            #{:declared-abstract-invariant-induction-open})
                          (when coverage-open?
                            #{:observed-writer-coverage-open}))
          identity-input
          {:version report-version
           :catalog-id source-id
           :catalog-fingerprint source-fingerprint
           :induction-fingerprint induction-fingerprint
           :ledger-fingerprint ledger-fingerprint
           :composition-fingerprint (:fingerprint composed)
           :provenance-fingerprint (:fingerprint manifest)
           :writer-observation-source (:writer-application-catalog-fingerprint
                                       coverage)
           :operation-contracts contracts
           :invariant-results invariant-results
           :original-open-obligations original
           :open-assumptions assumptions
           :open-obligations open
           :observed-writer-accounting writer-accounting}
          fingerprint (:fingerprint
                       (subject/describe-subject
                        {:kind :rule :id report-id :interface identity-input}))]
      {:type report-type
       :version report-version
       :fingerprint fingerprint
       :catalog-id source-id
       :catalog-fingerprint source-fingerprint
       :induction-fingerprint induction-fingerprint
       :ledger-fingerprint ledger-fingerprint
       :composition-fingerprint (:fingerprint composed)
       :provenance-fingerprint (:fingerprint manifest)
       :writer-observation-source (:writer-application-catalog-fingerprint
                                   coverage)
       :operation-ids ids
       :invariant-ids invs
       :operation-contracts contracts
       :invariant-results invariant-results
       :original-open-obligation-ids-by-operation original
       :open-meta-obligations meta-obligations
       :open-assumptions assumptions
       :open-obligations open
       :open-invariant-ids invariant-open
       :observed-writer-accounting writer-accounting
       :status (cond
                 coverage-open? :open-observed-writer-coverage
                 (seq invariant-open) :open-abstract-invariant-induction
                 :else :conditional-declared-application-assurance)
       :checking :source-derived-composition-provenance-and-observed-writer-join
       :scope :declared-abstract-machine-and-supplied-host-observation
       :assumption-discharge :not-performed
       :physical-writer-coverage :not-established
       :abstract-to-physical-correspondence :not-established
       :browser-correspondence :not-established
       :universal-proof-kernel-soundness :not-established
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))

(defn describe-application-assurance!
  "Rederive the entire conditional assurance report from original catalog,
   live application source graph/closure and current issued/host writer source.
   No input derived report can substitute for its source or closure handle."
  [catalog current-graph closure writer-source]
  (let [declared (dependency/describe-application-dependencies closure)
        composed (composition/compose-operation-interfaces!
                  catalog current-graph closure)
        manifest (provenance/describe-composition-provenance!
                  composed catalog current-graph closure)
        observed (writer/describe-application-writer-coverage!
                  catalog current-graph closure writer-source)]
    (checked-summary! declared composed manifest observed)))

(defn require-current-application-assurance!
  "Require the same ORIGINAL inputs, live closure and a fresh host snapshot.
   An equal copy is only descriptive evidence. It never grants certification."
  [prior catalog current-graph closure writer-source]
  (let [now (describe-application-assurance!
             catalog current-graph closure writer-source)]
    (when-not (and (ordinary-map? prior) (= prior now))
      (reject! :stale-or-substituted-application-assurance-report
               {:expected (:fingerprint now) :actual (:fingerprint prior)}))
    now))

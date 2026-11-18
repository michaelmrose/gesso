(ns gesso.assurance.proof.typed-certified-transition-alignment
  "v1763: Opt-in exact ORIGINAL transition-source correspondence between a
   checked typed operation catalog and the certified-plan executor reached by
   the currently adopted Biff route. A qualified source match is stricter
   than shared operation IDs or matching caller-supplied fingerprints.

   Recheck the declared v1239 guarded construction, its canonical v1233
   transition, both independently generated transition fingerprints and the
   CURRENT v1251 issued plan executor binding. Reuse the v1762 one-shot
   in-process Biff/XTDB-adapter observation without changing the writer.

   This is a declared SOURCE correspondence, not a formal proof of arbitrary
   executable behavior, authenticity of the declared business policy,
   complete deployed writers, deployed HTTP adoption, or actual XTDB
   causality. v1764 separately labels the bounded composition of exact
   declared transition source and one matched adapter transaction footprint;
   that label is conditional on the v1480 actual prepared-ops/options and
   definitive execute-key checks. It is NOT transaction-time CAS proof.
   Neither a source fingerprint nor this observer is a certificate."
  (:require [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.typed-host-physical-observation :as observed]
            [gesso.assurance.transition :as construction]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.typed-certified-transition-alignment/declared-source-binding)
(def report-version 1)

(def ^:private original-catalog-check operation-set/check-operation-set!)
(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-plan-require plan/require-current-plan-operation!)
(def ^:private original-plan-describe plan/describe-plan-operation)
(def ^:private original-construct construction/build-guarded-replacement!)
(def ^:private original-observer-issuer
  observed/issue-typed-host-physical-observer!)

(defn- reject! [reason]
  (throw (ex-info "Unsupported typed/certified transition-source alignment."
                  {:typed-certified-transition-alignment/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-catalog-check
                             (var-get #'operation-set/check-operation-set!))
                 (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-plan-require
                             (var-get #'plan/require-current-plan-operation!))
                 (identical? original-plan-describe
                             (var-get #'plan/describe-plan-operation))
                 (identical? original-construct
                             (var-get #'construction/build-guarded-replacement!))
                 (identical? original-observer-issuer
                             (var-get #'observed/issue-typed-host-physical-observer!)))
    (reject! :substituted-original-source-checker)))

(defn- current-declaration! [catalog contract-source operation]
  (pinned!)
  (let [catalog-report (original-catalog-check catalog)
        contract-report (original-contract-check contract-source)
        entry (first (filter #(= operation (:id %)) (:operations catalog)))
        binding (get-in contract-source [:certified-operations operation])
        executor (:executor binding)
        source (:current-source binding)]
    (when-not (and (= operation-set/report-type (:type catalog-report))
                   (= operation-set/operation-set-version (:version catalog-report))
                   (= :closed-declared-abstract-operation-set-only
                      (:scope catalog-report))
                   (= :checked (:exact-declared-operation-coverage catalog-report))
                   (= certified/report-type (:type contract-report))
                   (= certified/adapter-version (:version contract-report))
                   (contains? (:declared-operation-ids catalog-report) operation)
                   (= (:declared-operation-ids catalog-report)
                      (set (keys (:certified-operations contract-report))))
                   (empty? (:uncertified-operations contract-report))
                   entry binding (fn? executor) (map? source))
      (reject! :unmatched-declared-or-issued-operation))
    ;; The v1251 registered callable, not a copyable report, identifies the
    ;; original certified plan. Recheck its original proof, construction,
    ;; callback identities and plan source before comparing to the catalog.
    (original-plan-require executor source)
    (let [original (:construction source)
          declared (:construction entry)
          built (original-construct original)
          plan-description (original-plan-describe executor)
          catalog-description (first (filter #(= operation (:operation-id %))
                                             (:operation-results catalog-report)))
          certified-description (get-in contract-report
                                        [:certified-operations operation])]
      (when-not (and (= declared original)
                     (= operation (get-in declared [:subject :id]))
                     (= (:transition built)
                        (get-in source [:certified-source :proof-source :transition]))
                     (= (:transition-fingerprint built)
                        (:transition-fingerprint catalog-description)
                        (:transition-fingerprint plan-description)
                        (:transition-fingerprint certified-description))
                     (= (:subject-fingerprint built)
                        (:subject-fingerprint catalog-description))
                     (= operation (:operation plan-description))
                     (= operation (:operation certified-description)))
        (reject! :typed-and-certified-original-transition-diverged))
      (pinned!)
      {:operation-id operation
       :catalog-fingerprint (:fingerprint catalog-report)
       :transition-fingerprint (:transition-fingerprint built)
       :subject-fingerprint (:subject-fingerprint built)})))

(defn issue-source-aligned-host-physical-observer!
  "Issue a one-use v1762 local physical observer ONLY when the original
   declared catalog's selected v1239 construction equals the original
   v1251-issued certified-plan construction and their independently checked
   v1233 transition/source identities agree. The wrapper rechecks this
   source relationship before and after the one synchronous observation.

   Successful source equality does not prove the catalog's business-policy
   authenticity or that any observed adapter call used the separately
   issued typed complete-CAS gateway. No execution authority is issued."
  [host-handle contract-source system catalog operation]
  (let [initial (current-declaration! catalog contract-source operation)
        diagnostic (original-observer-issuer host-handle contract-source system
                                             catalog operation)
        consumed (AtomicBoolean. false)
        qualified (atom :not-observed)
        footprint (atom :not-established)
        recheck! (fn []
                   (when-not (= initial
                                (current-declaration! catalog contract-source
                                                      operation))
                     (reject! :declared-source-or-certified-executor-changed))
                   true)
        decorate (fn [r]
                   (assoc r
                          :type report-type
                          :version report-version
                          :original-transition-source
                          :exact-typed-catalog-to-issued-certified-plan-match
                          :transition-fingerprint
                          (:transition-fingerprint initial)
                          :typed-to-certified-executor-transition-equivalence
                          :not-established
                          :declared-source-to-observed-adapter-footprint
                          @footprint
                          :original-business-policy-authenticity
                          :not-established
                          :typed-gateway-used-by-handler :not-established
                          :certificate :not-issued :admission :not-admitted
                          :authority :none))
        summary! (fn []
                   (recheck!)
                   (-> ((:summary! diagnostic))
                       decorate
                       (assoc :observation-status
                              (if (.get consumed)
                                (if (= :not-observed @qualified)
                                  :attempt-consumed-without-qualified-result
                                  @qualified)
                                :not-attempted))))
        observe! (fn [request]
                   (when-not (.compareAndSet consumed false true)
                     (reject! :aligned-observation-already-attempted))
                   (recheck!)
                   (let [result ((:observe-once! diagnostic) request)]
                     (recheck!)
                     (when-not (and (= observed/report-type (:type result))
                                    (= observed/report-version (:version result))
                                    (= operation (:operation-id result))
                                    (= :not-established
                                       (:typed-to-certified-executor-transition-equivalence
                                        result))
                                    (= :not-issued (:certificate result))
                                    (= :none (:authority result)))
                       (reject! :unexpected-underlying-physical-observation))
                     ;; The original declared transition construction is
                     ;; source-equal to the checked issued executor. Only a
                     ;; qualified v1480 single-call observation may compose
                     ;; that source fact with its *prepared transaction
                     ;; footprint*, NOT with authenticated XTDB persistence.
                     (let [positive? (= :bounded-same-invocation-adapter-correlation
                                        (:status result))
                           footprint-qualified?
                           (and positive?
                                (true? (:resolved-executor-attempt-observed? result))
                                (true? (:one-adapter-call-observed? result))
                                (true? (:prepared-operations-agree? result))
                                (true? (:prepared-options-agree? result))
                                (true? (:definitive-execute-key-observed? result)))]
                       (when (and positive? (not footprint-qualified?))
                         (reject! :positive-adapter-correlation-lacks-required-footprint))
                       (reset! footprint
                               (if footprint-qualified?
                                 :bounded-source-matched-adapter-plan-footprint
                                 :not-established))
                       (reset! qualified (:status result))
                       (decorate result))))]
    (recheck!)
    {:type report-type :version report-version
     :observe-once! observe! :summary! summary!}))

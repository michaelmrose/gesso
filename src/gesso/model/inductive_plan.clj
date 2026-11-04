(ns gesso.model.inductive-plan
  "v1290: optional source-bound induction gate when ISSUING a v1251
   authoritative plan executor. This is deliberately not a new executor.

   Supply the ORIGINAL v1286 induction evidence package and the ORIGINAL
   v1251 certified-plan registration source. A plan may be issued through
   this adapter only when its complete guarded replacement construction is
   exactly the one named by the source-checked induction catalog. Compare
   the actual typed transition fingerprints as well as the operation IDs:
   merely using the same operation keyword is not sufficient.

   The existing v1251 builder still owns physical candidate checking, the
   wrapped resolved-operation executor, and the pre-submit boundary. This
   module does not intercept XTDB writes, promise the issued executor is
   installed at the live route, or prove transition refinement. An issued
   function is never a permission to bypass application authorization.
   The optional source recheck is evidence about a trusted supplied snapshot,
   not ongoing runtime enforcement or a certification of all physical writers.

   Within the trusted JVM only, the exact returned function is recorded in a
   weak identity registry. Copying a report or presenting a different issued
   plan executor is not a valid substitute."
  (:require [clojure.set :as set]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.transition :as transition]
            [gesso.model.certified-plan :as plan])
  (:import (java.util WeakHashMap)))

(def adapter-version 1)
(def report-type :gesso.model.inductive-plan/source-bound-plan-description)
(def ^:private source-keys #{:inductive-source :certified-plan-source})
(def ^:private inductive-keys
  #{:catalog :certificate-handle :dependency-handle :current-graph})
(def ^:private plan-keys
  #{:certified-source :construction :field-map :command-operation
    :current-document! :transition-inputs! :target!})
(def ^:private additional-open
  #{:runtime-inductive-gate-adoption
    :prestate-to-commit-atomic-guard-enforcement
    :physical-write-path-completeness
    :abstract-to-xtdb-transition-correspondence
    :authorization-and-callback-purity
    :deployed-handler-and-router-adoption
    :induction-checker-independent-soundness})
(def ^:private ^WeakHashMap issued (WeakHashMap.))

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso induction-bound authoritative plan."
                  (merge {:error/type ::invalid-inductive-plan
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [m allowed at]
  (when-not (and (plain-map? m) (nil? (meta m)))
    (reject! :expected-unannotated-source-map at {}))
  (let [actual (set (keys m))]
    (when-not (= allowed actual)
      (reject! :wrong-source-keys at
               {:missing (set/difference allowed actual)
                :unexpected (set/difference actual allowed)})))
  m)

;; Retain original source-derived checkers: redefining an outward-facing Var
;; cannot supply a made-up success report in place of the original verifier.
(def ^:private original-report inductive/build-inductive-report!)
(def ^:private original-operation-set operation-set/check-operation-set!)
(def ^:private original-plan-builder plan/build-certified-plan-operation!)
(def ^:private original-plan-freshness plan/require-current-plan-operation!)

(defn- checked-report! [inductive-source]
  (exact-map! inductive-source inductive-keys [:inductive-source])
  (when-not (every? some? (map inductive-source
                               [:catalog :certificate-handle
                                :dependency-handle :current-graph]))
    (reject! :complete-current-induction-evidence-required
             [:inductive-source] {}))
  (let [report (original-report inductive-source)
        catalog-report (original-operation-set (:catalog inductive-source))]
    (when-not (and (= inductive/report-type (:type report))
                   (= inductive/report-version (:version report))
                   (= :source-bound-conditional-abstract-safety
                      (:verdict report))
                   (= :original-sources-and-both-live-handles-rechecked
                      (:checking report))
                   (true? (:all-declared-invariants-conditionally-inductive?
                           report))
                   (= :checked-exact-source-relative
                      (:source-dependency-closure report))
                   (= :in-process-conditional-abstract-certificate
                      (get-in report [:classification :certificate :status]))
                   (= (:catalog-id report) (:catalog-id catalog-report))
                   (= (get-in report [:identity :catalog-fingerprint])
                      (:fingerprint catalog-report))
                   (= (:declared-operation-ids report)
                      (:declared-operation-ids catalog-report))
                   (= (:declared-invariant-ids report)
                      (:invariant-ids catalog-report))
                   (= :none (:authority report))
                   (= :none (:transaction-authority report)))
      (reject! :incomplete-or-mismatched-induction-evidence
               [:inductive-source] {}))
    {:induction report :catalog catalog-report}))

(defn- checked-plan-source! [source]
  (exact-map! source source-keys [])
  (let [{:keys [inductive-source certified-plan-source]} source
        _ (exact-map! certified-plan-source plan-keys
                      [:certified-plan-source])
        {:keys [induction catalog]} (checked-report! inductive-source)
        operation (get-in certified-plan-source [:construction :subject :id])
        entries (:operations (:catalog inductive-source))
        matching (filter #(= operation (:id %)) entries)
        row (first (filter #(= operation (:operation-id %))
                           (:operation-results catalog)))]
    (when-not (and (keyword? operation) (some? (namespace operation))
                   (= 1 (count matching))
                   (contains? (:declared-operation-ids catalog) operation)
                   (some? row))
      (reject! :operation-not-in-inductive-catalog
               [:certified-plan-source :construction :subject] {}))
    (when-not (= (:construction (first matching))
                 (:construction certified-plan-source))
      (reject! :plan-abstract-construction-mismatch
               [:certified-plan-source :construction]
               {:operation operation}))
    ;; v1251's eventual builder separately checks proof source, guards,
    ;; callback registration, field mapping, and the real pre-submit gate.
    (let [built (transition/build-guarded-replacement!
                 (:construction (first matching)))
          registered-operation (get-in certified-plan-source
                                       [:certified-source :operation])
          proof-transition (get-in certified-plan-source
                                   [:certified-source :proof-source :transition])]
      (when-not (and (= registered-operation operation)
                     (= (:transition built) proof-transition)
                     (= (:transition-fingerprint built)
                        (:transition-fingerprint row))
                     (= (:subject-fingerprint built)
                        (:subject-fingerprint row)))
        (reject! :registered-plan-transition-mismatch
                 [:certified-plan-source :certified-source]
                 {:operation operation}))
      {:operation operation
       :transition-fingerprint (:transition-fingerprint built)
       :subject-fingerprint (:subject-fingerprint built)
       :catalog-fingerprint (:fingerprint catalog)
       :induction-fingerprint (get-in induction
                                      [:identity :induction-fingerprint])
       :certificate-fingerprint
       (get-in induction [:identity :certificate-fingerprint])
       :dependency-fingerprint
       (get-in induction [:identity :dependency-fingerprint])
       :catalog-operation-ids (:declared-operation-ids catalog)
       :invariant-ids (:declared-invariant-ids induction)
       :original-open-obligations
       (get-in induction [:classification :original-obligations :by-operation])
       :open-premises
       (get-in induction [:classification :certificate :open-premises])
       :open-obligations
       (set/union additional-open
                  (get-in induction [:classification :open :assumptions]))})))

(defn- description [checked plan-description]
  (when-not (and (= plan/description-type (:type plan-description))
                 (= plan/plan-version (:version plan-description))
                 (= (:operation checked) (:operation plan-description))
                 (= (:transition-fingerprint checked)
                    (:transition-fingerprint plan-description))
                 (= :trusted-observation-only
                    (:prestate-assurance plan-description))
                 (= :not-established (:atomicity-assurance plan-description))
                 (= :not-proved (:invariant-preservation plan-description))
                 (= :not-established (:physical-correspondence
                                     plan-description))
                 (= :none (:authority plan-description)))
    (reject! :issued-plan-description-mismatch [:executor] {}))
  {:type report-type
   :version adapter-version
   :checking :original-induction-and-exact-plan-transition-rechecked
   :operation (:operation checked)
   :catalog-operation-ids (:catalog-operation-ids checked)
   :invariant-ids (:invariant-ids checked)
   :catalog-fingerprint (:catalog-fingerprint checked)
   :induction-fingerprint (:induction-fingerprint checked)
   :certificate-fingerprint (:certificate-fingerprint checked)
   :dependency-fingerprint (:dependency-fingerprint checked)
   :transition-fingerprint (:transition-fingerprint checked)
   :subject-fingerprint (:subject-fingerprint checked)
   :field-map (:field-map plan-description)
   :command-operation (:command-operation plan-description)
   :plan-gate :original-v1251-pre-submit-checker
   :source-join :exact-guarded-transition-construction
   :scope :one-registered-plan-of-declared-abstract-machine
   :original-open-obligations (:original-open-obligations checked)
   :open-premises (set/union (:open-premises checked)
                             (:open-premises plan-description))
   :open-obligations (set/union (:open-obligations checked)
                                (:open-obligations plan-description))
   :inductive-invariant :conditionally-checked-in-declared-abstract-machine
   :prestate-assurance :trusted-observation-only
   :runtime-induction-gating :not-established
   :actual-router-adoption :not-established
   :atomicity-assurance :not-established
   :physical-correspondence :not-established
   :physical-write-path-completeness :not-established
   :transaction-authority :none
   :authority :none})

(defn build-inductive-plan-operation!
  "Optionally issue a v1251 authoritative executor only after the entire
   induction evidence and THIS operation's exact original construction match.
   The returned callable is the genuine v1251 plan executor, NOT a new wrapper
   or a proof-backed transaction authorization."
  [source]
  (let [checked (checked-plan-source! source)
        executor (original-plan-builder (:certified-plan-source source))
        info (description checked (plan/describe-plan-operation executor))]
    (locking issued (.put issued executor {:source source :description info}))
    executor))

(defn inductive-plan-operation? [executor]
  (boolean (and (fn? executor)
                (locking issued (.get issued executor)))))

(defn- registered! [executor]
  (or (when (fn? executor) (locking issued (.get issued executor)))
      (reject! :unregistered-inductive-plan [:executor] {})))

(defn describe-inductive-plan [executor]
  (let [{:keys [description]} (registered! executor)]
    ;; Copyable, display-only information; not a freshness witness.
    description))

(defn require-current-inductive-plan!
  "Recheck the ORIGINAL induction source and v1251 plan source against the
   same issued executor. The supplied current source must be obtained from
   the CURRENT trusted assembly. Merely replaying a cached source does not
   prove the deployed router or current database state."
  [executor current-source]
  (let [{original :source prior :description} (registered! executor)]
    (exact-map! current-source source-keys [:current-source])
    (let [old (:inductive-source original)
          now (:inductive-source current-source)]
      (exact-map! now inductive-keys [:current-source :inductive-source])
      (when-not (and (= (:catalog old) (:catalog now))
                     (= (:current-graph old) (:current-graph now))
                     (identical? (:certificate-handle old)
                                 (:certificate-handle now))
                     (identical? (:dependency-handle old)
                                 (:dependency-handle now)))
        (reject! :changed-induction-registration
                 [:current-source :inductive-source] {})))
    (let [checked (checked-plan-source! current-source)]
      (original-plan-freshness executor
                               (:certified-plan-source current-source))
      (let [fresh (description checked
                               (plan/describe-plan-operation executor))]
        (when-not (= prior fresh)
          (reject! :stale-inductive-plan [:current-source] {}))
        fresh))))

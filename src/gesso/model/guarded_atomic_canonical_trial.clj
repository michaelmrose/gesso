(ns gesso.model.guarded-atomic-canonical-trial
  "v1772: Opt-in, single-invocation execution EXPERIMENT at the existing
   resolved-operation canonical submission site. Before the original Gesso
   model.tx/transact! executes, reconstruct the actual domain+ledger source
   from the incorporated command and require v1771 guarded preparation.
   Observe the original canonical transaction site in the SAME invocation.

   This never installs a writer or changes Live/XTDB transaction behavior.
   It does not prove that application callbacks were safe, that no other
   writer ran, that XTDB acknowledged durably, or that the newer typed
   one-document gateway is used. An uncertain result grants NO retry permit."
  (:require [gesso.model.canonical-transaction-site :as site]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.guarded-atomic-incorporation-preparation :as guarded])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.model.guarded-atomic-canonical-trial/one-canonical-physical-experiment)
(def report-version 1)

(def ^:dynamic ^:private *trial* nil)

(def ^:private original-site-observer site/observe-canonical-transaction-site!)
(def ^:private original-incorporation resolution/incorporation-plan)
(def ^:private original-guarded-issuer guarded/issue-guarded-atomic-preparer!)

(defn- reject! [reason]
  (throw (ex-info "Guarded canonical execution experiment was not qualified."
                  {:guarded-atomic-canonical-trial/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-site-observer
                             (var-get #'site/observe-canonical-transaction-site!))
                 (identical? original-incorporation
                             (var-get #'resolution/incorporation-plan))
                 (identical? original-guarded-issuer
                             (var-get #'guarded/issue-guarded-atomic-preparer!)))
    (reject! :substituted-canonical-trial-boundary)))

(defn- reconstructed-source! [binding incorporated]
  (let [record (:record incorporated)
        commands (get-in incorporated [:plan :commands])]
    (when-not (and (map? incorporated) (map? record)
                   (vector? commands) (<= 2 (count commands))
                   (= binding (:gesso.command/binding record)))
      (reject! :no-canonical-incorporation-source))
    (let [source {:binding binding
                  :outcome (:gesso.command/outcome record)
                  :now (:gesso.command/created-at record)
                  :plan (update (:plan incorporated) :commands pop)}]
      (when-not (= incorporated
                   (try (original-incorporation source)
                        (catch Exception _
                          (reject! :canonical-source-reconstruction-failed))))
        (reject! :canonical-source-reconstruction-diverged))
      source)))

(defn before-canonical-submit!
  "Called ONLY by the canonical resolved-operation transaction site before
   its existing writer, and inert outside opt-in trial. Fail-closed if the
   source-derived original guarded preparation cannot be established.
   A trusted constructor can already have run; this checks the submission
   boundary, not all preceding effects or all concurrent writers."
  [binding incorporated]
  (when-let [{:keys [thread operation attempts prepared?]} *trial*]
    (let [number (swap! attempts inc)]
      (when-not (and (= 1 number)
                     (identical? thread (Thread/currentThread))
                     (= operation (:operation binding)))
        (reject! :wrong-thread-operation-or-repeated-submit))
      (pinned!)
      (let [source (reconstructed-source! binding incorporated)
            issued (original-guarded-issuer source)
            report ((:prepare-once! issued))]
        (when-not (and (= guarded/report-type (:type report))
                       (= guarded/report-version (:version report))
                       (= :guarded-atomic-prepared-not-submitted (:status report))
                       (= :disabled (:physical-submission report))
                       (= :not-issued (:certificate report))
                       (= :none (:authority report)))
          (reject! :unqualified-canonical-guarded-preparation))
        (reset! prepared? true)))))

(defn after-canonical-submit!
  "Best-effort original-site outcome recording. Caller MUST insulate its
   authoritative transaction result from diagnostic callback failure."
  [binding physical-status]
  (when-let [{:keys [thread operation attempts prepared? completed]} *trial*]
    (when (and (= 1 @attempts) @prepared?
               (identical? thread (Thread/currentThread))
               (= operation (:operation binding))
               (contains? #{:committed :rejected :indeterminate} physical-status))
      (reset! completed physical-status))))

(defn- report [operation status prepared?]
  {:type report-type
   :version report-version
   :operation-id operation
   :scope :one-original-resolved-operation-submission
   :status status
   :canonical-guarded-atomic-preparation
   (if prepared? :checked-before-model-transaction :not-established)
   :domain-and-ledger (if prepared? :one-guarded-incorporated-plan
                         :not-established)
   :canonical-writer (if (contains? #{:bounded-guarded-atomic-canonical-acknowledgment
                                     :guarded-canonical-submission-rejected} status)
                       :original-model-tx-transact :not-established)
   :physical-durability-and-receipt :not-established
   :original-business-policy-authenticity :not-established
   :complete-deployed-writer-coverage :not-established
   :typed-complete-cas-gateway-adoption :not-established
   :actual-live-delivery :not-established
   :deployed-http-handler-adoption :not-established
   :retry-authority :none
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})

(defn issue-guarded-canonical-trial!
  "One-use observer around a trusted ORIGINAL resolved-operation invocation.
   Never submits a second transaction or accepts a caller-supplied physical
   report. Requires the v1771 guarded full-plan preparer at the actual
   canonical pre-submit site and the v1768 same-invocation transaction-site
   report. Returns a REDACTED report only; it cannot authenticate external
   XTDB or safely authorize retry. The invoke! callback may have side effects
   before canonical submission, so failure does not imply zero side effects."
  [operation]
  (when-not (qualified-keyword? operation)
    (reject! :invalid-selected-operation))
  (pinned!)
  (let [consumed (AtomicBoolean. false)
        status (atom :not-attempted)
        verified (atom false)
        summary! (fn []
                   (pinned!)
                   (report operation @status @verified))
        observe! (fn [invoke!]
                   (when-not (.compareAndSet consumed false true)
                     (reject! :one-shot-canonical-trial-already-attempted))
                   (reset! status :attempt-consumed-without-qualified-result)
                   (pinned!)
                   (when-not (fn? invoke!)
                     (reject! :invalid-trial-invocation))
                   (when *trial*
                     (reject! :nested-canonical-trial))
                   (let [attempts (atom 0)
                         prepared? (atom false)
                         completed (atom :not-established)
                         trial {:thread (Thread/currentThread)
                                :operation operation
                                :attempts attempts :prepared? prepared?
                                :completed completed}
                         observed (binding [*trial* trial]
                                    (original-site-observer operation invoke!))
                         site-report (:report observed)
                         qualified? (and (= 1 @attempts)
                                         @prepared?
                                         (= :committed @completed)
                                         (= :returned (get-in observed [:execution :outcome]))
                                         (= :one-canonical-model-tx-acknowledged
                                            (:status site-report))
                                         (= :committed-ack-observed
                                            (:model-tx-acknowledgment site-report))
                                         (= :not-issued (:certificate site-report)))
                         result (cond
                                  qualified? :bounded-guarded-atomic-canonical-acknowledgment
                                  (zero? @attempts) :no-canonical-submission-observed
                                  (and (= 1 @attempts) @prepared?
                                       (= :rejected @completed)
                                       (= :canonical-model-tx-rejected
                                          (:status site-report)))
                                  :guarded-canonical-submission-rejected
                                  :else :not-qualified-or-uncertain)]
                     (pinned!)
                     (reset! status result)
                     (reset! verified @prepared?)
                     (report operation result @prepared?)))]
    {:type report-type :version report-version
     :observe-once! observe! :summary! summary!}))

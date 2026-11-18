(ns gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial
  "v1774: Compose v1773 source-relative typed FULL atomic preparation with
   v1772's actual canonical model transaction site in one synchronous trial.

   At the ORIGINAL resolved-operation pre-submit boundary, reconstruct its
   incorporated domain+command-ledger source, require typed abstract domain
   agreement and full guarded physical plan preparation, and only then allow
   the unchanged canonical writer to submit. The v1772 trial independently
   observes the single authoritative model tx site and its acknowledgment.

   This is cooperative process-local instrumentation. The declaration,
   inputs, mapping and read premises are supplied by the caller, not proven
   authentic. The typed one-document gateway is NOT the physical writer.
   Successful acknowledgment is not an authenticated durable XTDB receipt,
   universal semantic refinement, complete writer coverage, or Live delivery."
  (:require [gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation
             :as full]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.guarded-atomic-canonical-trial :as canonical]
            [gesso.model.typed-atomic-pre-submit-hook :as hook])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-full-atomic-canonical-trial/typed-canonical-trial)
(def report-version 1)

(def ^:dynamic ^:private *typed-trial* nil)

(def ^:private original-incorporation resolution/incorporation-plan)
(def ^:private original-typed-issuer full/issue-full-atomic-typed-preparer!)
(def ^:private original-canonical-issuer canonical/issue-guarded-canonical-trial!)
(def ^:private original-hook hook/before-canonical-submit!)

(defn- reject! [reason]
  (throw (ex-info "Typed full-atomic canonical trial is not established."
                  {:typed-full-atomic-canonical-trial/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-incorporation
                             (var-get #'resolution/incorporation-plan))
                 (identical? original-typed-issuer
                             (var-get #'full/issue-full-atomic-typed-preparer!))
                 (identical? original-canonical-issuer
                             (var-get #'canonical/issue-guarded-canonical-trial!))
                 (identical? original-hook
                             (var-get #'hook/before-canonical-submit!)))
    (reject! :substituted-typed-or-canonical-trial-boundary)))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- original-source! [binding incorporated]
  (let [record (:record incorporated)
        commands (get-in incorporated [:plan :commands])]
    (when-not (and (plain-map? incorporated)
                   (plain-map? record)
                   (vector? commands)
                   (= 2 (count commands))
                   (= binding (:gesso.command/binding record)))
      (reject! :unmatched-original-atomic-incorporation))
    (let [source {:binding binding
                  :outcome (:gesso.command/outcome record)
                  :now (:gesso.command/created-at record)
                  :plan (update (:plan incorporated) :commands pop)}]
      (when-not (= incorporated
                   (try (original-incorporation source)
                        (catch Exception _
                          (reject! :original-atomic-source-reconstruction-failed))))
        (reject! :original-atomic-source-diverged))
      source)))

(defn before-canonical-submit!
  "Called directly at the original resolved-operation transaction boundary,
   after v1772's full-plan preflight and BEFORE model.tx/transact!. Inert
   without an opt-in same-thread observer. Failure is fail-closed only for
   that canonical submission; trusted earlier callbacks may have effects."
  [binding incorporated]
  (when-let [{:keys [thread operation attempts prepared? declaration inputs
                    field-map premises]} *typed-trial*]
    (let [attempt (swap! attempts inc)]
      (when-not (and (= 1 attempt)
                     (identical? thread (Thread/currentThread))
                     (= operation (:operation binding)))
        (reject! :wrong-operation-thread-or-repeated-typed-submit))
      (pinned!)
      (let [source (original-source! binding incorporated)
            issued (original-typed-issuer source declaration inputs
                                         field-map premises)
            prepared ((:prepare-once! issued))]
        (when-not (and (= full/report-type (:type prepared))
                       (= full/report-version (:version prepared))
                       (= operation (:operation-id prepared))
                       (= :typed-full-atomic-prepared-not-submitted
                          (:status prepared))
                       (= :one-domain-update-plus-durable-ledger-create
                          (:atomic-commands prepared))
                       (= :full-plan-assert-and-write-order-checked
                          (:guarded-xtdb-operations prepared))
                       (= :disabled (:physical-submission prepared))
                       (= :not-established (:business-policy-authenticity
                                            prepared))
                       (= :not-issued (:certificate prepared))
                       (= :none (:authority prepared)))
          (reject! :unqualified-typed-full-atomic-preparation))
        (pinned!)
        (reset! prepared? true)))))

(defn- report [operation status typed?]
  {:type report-type
   :version report-version
   :operation-id operation
   :status status
   :scope :one-original-canonical-typed-full-atomic-invocation
   :typed-preparation-at-canonical-submit
   (if typed? :checked-before-model-transaction :not-established)
   :typed-domain-semantics
   (if typed? :original-and-independent-source-relative :not-established)
   :domain-and-command-ledger
   (if typed? :one-guarded-incorporated-plan :not-established)
   :canonical-writer
   (if (contains? #{:bounded-typed-full-atomic-canonical-acknowledgment
                    :typed-full-atomic-canonical-submission-rejected} status)
     :original-model-tx-transact :not-established)
   :typed-complete-cas-gateway-adoption :not-established
   :authenticated-xtdb-receipt-and-durability :not-established
   :business-policy-authenticity :not-established
   :deployed-http-adoption :not-established
   :complete-deployed-writer-coverage :not-established
   :actual-live-delivery :not-established
   :retry-authority :none
   :certificate :not-issued :admission :not-admitted
   :transaction-authority :none :authority :none})

(defn issue-typed-full-atomic-canonical-trial!
  "Issue a ONE-use trial against the exact original canonical model writer.
   The caller supplies abstract declarations/inputs, not an already-prepared
   transaction or a physical receipt. Inside the real submission path,
   recompute original command incorporation and apply v1773's typed+guarded
   full-plan checker. v1772 independently checks that exactly one actual
   model transaction was attempted during this invocation. The result is
   redacted; missing, failed, uncertain or duplicate attempts cannot be
   promoted to success and can never authorize a retry."
  [operation declaration inputs field-map premises]
  (when-not (and (qualified-keyword? operation)
                 (every? plain-map?
                         [declaration inputs field-map premises]))
    (reject! :invalid-source-relative-typed-trial))
  (pinned!)
  (let [canonical-issued (original-canonical-issuer operation)
        consumed (AtomicBoolean. false)
        status (atom :not-attempted)
        verified? (atom false)
        recheck! (fn []
                   (pinned!)
                   (let [s ((:summary! canonical-issued))]
                     (when-not (and (= canonical/report-type (:type s))
                                    (= canonical/report-version (:version s))
                                    (= operation (:operation-id s))
                                    (= :not-issued (:certificate s))
                                    (= :none (:authority s)))
                       (reject! :invalid-canonical-observation-boundary)))
                   true)
        summary! (fn [] (recheck!) (report operation @status @verified?))
        observe! (fn [invoke!]
                   (when-not (.compareAndSet consumed false true)
                     (reject! :typed-canonical-trial-already-attempted))
                   (reset! status :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (fn? invoke!)
                     (reject! :invalid-canonical-invocation))
                   (when *typed-trial*
                     (reject! :nested-typed-full-atomic-canonical-trial))
                   (let [attempts (atom 0)
                         prepared? (atom false)
                         context {:thread (Thread/currentThread)
                                  :operation operation :attempts attempts
                                  :prepared? prepared? :declaration declaration
                                  :inputs inputs :field-map field-map
                                  :premises premises}
                         result (binding [*typed-trial* context
                                           hook/*before-canonical-submit*
                                           before-canonical-submit!]
                                  ((:observe-once! canonical-issued) invoke!))
                         canonical-status (:status result)
                         canonical-valid? (and (= canonical/report-type (:type result))
                                               (= canonical/report-version
                                                  (:version result))
                                               (= operation (:operation-id result))
                                               (= :not-issued (:certificate result))
                                               (= :none (:authority result)))
                         status* (cond
                                   (not canonical-valid?)
                                   :unqualified-canonical-trial-report

                                   (and (= 1 @attempts) @prepared?
                                        (= :bounded-guarded-atomic-canonical-acknowledgment
                                           canonical-status))
                                   :bounded-typed-full-atomic-canonical-acknowledgment

                                   (and (= 1 @attempts) @prepared?
                                        (= :guarded-canonical-submission-rejected
                                           canonical-status))
                                   :typed-full-atomic-canonical-submission-rejected

                                   (zero? @attempts)
                                   :no-canonical-submission-observed

                                   (and (= 1 @attempts) (not @prepared?))
                                   :typed-preparation-not-established-before-submit

                                   :else :not-qualified-or-uncertain)]
                     (recheck!)
                     (reset! status status*)
                     (reset! verified? @prepared?)
                     (report operation status* @prepared?)))]
    (recheck!)
    {:type report-type :version report-version
     :observe-once! observe! :summary! summary!}))

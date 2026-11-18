(ns gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation
  "v1773: Join the ORIGINAL typed guarded-transition interpreter to the
   FULL Gesso atomic incorporated transaction, never to a domain-only write.
   The one typed UPDATE is embedded alongside the durable command ledger,
   transaction-time dependency guards, explicit assertions, Live changes,
   publication policy and transaction options. A single-document physical
   gateway is NOT silently promoted to a full atomic writer.

   Preparation is source-relative, redacted, single-use and submission-
   DISABLED. Original business policy authenticity, complete writer coverage,
   deployed HTTP and authenticated XTDB execution remain unproved."
  (:require [gesso.assurance.proof.xtdb-gateway-typed-transition-seam :as typed]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.guarded-atomic-incorporation-preparation :as guarded])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-full-atomic-typed-preparation/source-bound)
(def report-version 1)

(def ^:private original-typed-check typed/check-source-relative-typed-domain!)
(def ^:private original-incorporation resolution/incorporation-plan)
(def ^:private original-guarded-issuer guarded/issue-guarded-atomic-preparer!)
(def ^:private original-command-operation command/operation)

(defn- reject! [reason]
  (throw (ex-info "Full atomic typed preparation could not be established."
                  {:full-atomic-typed-preparation/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-typed-check
                             (var-get #'typed/check-source-relative-typed-domain!))
                 (identical? original-incorporation
                             (var-get #'resolution/incorporation-plan))
                 (identical? original-guarded-issuer
                             (var-get #'guarded/issue-guarded-atomic-preparer!))
                 (identical? original-command-operation
                             (var-get #'command/operation)))
    (reject! :substituted-original-source-or-preparer)))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- check! [source declaration inputs field-map premises]
  (pinned!)
  (let [binding (:binding source)
        operation (:operation binding)
        incorporated (try (original-incorporation source)
                          (catch Exception _
                            (reject! :invalid-canonical-incorporation)))
        commands (get-in incorporated [:plan :commands])
        domain (first commands)
        ledger (second commands)]
    (when-not (and (qualified-keyword? operation)
                   (= operation (get-in declaration [:subject :id]))
                   (= 2 (count commands))
                   (not= :create (original-command-operation domain))
                   (= :create (original-command-operation ledger))
                   (= resolution/command-entity-type (:model/entity-type ledger))
                   (not= (:model/entity-type domain) (:model/entity-type ledger)))
      (reject! :unsupported-domain-plus-ledger-command-shape))
    ;; The original domain command alone is projected for ABSTRACT semantic
    ;; checking. It is NEVER used as the physical transaction and confers no
    ;; submit capability. The complete incorporated plan stays intact below.
    (let [typed-report
          (try (original-typed-check
                operation {:plan {:commands [domain] :emit false}}
                declaration inputs field-map premises)
               (catch Exception _ (reject! :typed-domain-relation-not-established)))
          guarded-issued
          (try (original-guarded-issuer source)
               (catch Exception _ (reject! :atomic-guarded-plan-not-established)))
          guarded-report
          (try ((:prepare-once! guarded-issued))
               (catch Exception _ (reject! :atomic-guarded-plan-not-established)))]
      (when-not (and (= typed/source-check-type (:type typed-report))
                     (= typed/source-check-version (:version typed-report))
                     (= operation (:operation-id typed-report))
                     (= :original-and-independent-agree
                        (:abstract-interpreters typed-report))
                     (= :source-relative-values-screened
                        (:original-read-premises typed-report))
                     (= :not-established (:source-policy-authenticity typed-report))
                     (= :disabled (:physical-submission typed-report))
                     (= guarded/report-type (:type guarded-report))
                     (= guarded/report-version (:version guarded-report))
                     (= :guarded-atomic-prepared-not-submitted
                        (:status guarded-report))
                     (= :included-in-same-atomic-plan
                        (:incorporated-ledger guarded-report))
                     (= :checked-by-independent-isolated-preparation
                        (:formatted-xtdb-footprint guarded-report))
                     (= :disabled (:physical-submission guarded-report))
                     (= :none (:authority typed-report)
                        (:authority guarded-report)))
        (reject! :unqualified-typed-or-full-atomic-preparation))
      (pinned!)
      {:operation operation
       :typed-report typed-report
       :guarded-report guarded-report})))

(defn- report [operation status]
  {:type report-type :version report-version :operation-id operation
   :status status
   :typed-domain-transition :original-and-independent-source-relative
   :original-read-premises :source-relative-values-screened
   :atomic-commands :one-domain-update-plus-durable-ledger-create
   :guarded-xtdb-operations :full-plan-assert-and-write-order-checked
   :live-inputs :preserved-not-published
   :physical-submission :disabled
   :typed-one-document-gateway-adoption :not-established
   :business-policy-authenticity :not-established
   :current-xtdb-prestate :not-established
   :xtdb-commit-or-receipt :not-established
   :live-delivery :not-established
   :complete-deployed-writer-coverage :not-established
   :certificate :not-issued :admission :not-admitted
   :transaction-authority :none :authority :none})

(defn issue-full-atomic-typed-preparer!
  "Issue a one-attempt, no-submit, fully incorporated source-relative typed
   preparation. Original source {:binding :outcome :now :plan} is recomposed
   with the original command-resolution code, never supplied as a forged
   already-incorporated plan. Every invocation checks original + independent
   abstract transition semantics, read invariants, full ledger and ordered
   XTDB assertions/writes through v1771. No plan, tx-op or writer escapes."
  [source declaration inputs field-map premises]
  (when-not (and (plain-map? source)
                 (= #{:binding :outcome :now :plan} (set (keys source)))
                 (plain-map? (:plan source))
                 (plain-map? declaration) (plain-map? inputs)
                 (plain-map? field-map) (plain-map? premises))
    (reject! :invalid-full-atomic-typed-source))
  (let [initial (check! source declaration inputs field-map premises)
        operation (:operation initial)
        consumed (AtomicBoolean. false)
        recheck! (fn []
                   (when-not (= initial
                                (check! source declaration inputs field-map premises))
                     (reject! :original-full-atomic-typed-source-changed))
                   true)
        summary! (fn []
                   (recheck!)
                   (report operation (if (.get consumed)
                                       :preparation-permit-consumed
                                       :issued-unsubmitted)))
        prepare! (fn []
                   (when-not (.compareAndSet consumed false true)
                     (reject! :preparation-already-attempted))
                   (recheck!)
                   (report operation :typed-full-atomic-prepared-not-submitted))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :prepare-once! prepare!}))

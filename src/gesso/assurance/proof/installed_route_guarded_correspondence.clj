(ns gesso.assurance.proof.installed-route-guarded-correspondence
  "v1486: Strictly classified, read-only joining of an authenticated LOCAL
   v1480 route/executor/adapter observation and an OPTIONAL v1484 prestate
   acquisition diagnostic. Neither component is replayed or submitted.

   A v1480 identity-registered handle authenticates the original in-process
   controlled Ring invocation and matching XTDB-adapter call observation,
   conditional on its observer. The v1484 map has NO issued origin handle:
   even a structurally coherent guarded plan is caller-supplied, and its
   assertions are NOT known to be the operations executed by v1480.

   This report intentionally REFUSES to promote the separate observations
   into physical guard correspondence, authentic XTDB receipt, installed
   HTTP-server provenance, complete writer coverage or certified refinement.
   In particular it never returns a command, transaction, retry, settlement
   or application-admission authority. Do not log the input: its plans,
   request, snapshot token and documents may contain sensitive information."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.installed-route-physical-correlation :as physical]
            [gesso.assurance.proof.xtdb-guarded-prestate-acquisition :as guarded]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as capability]
            [gesso.model.tx :as tx]))

(def report-type
  :gesso.assurance.proof.installed-route-guarded-correspondence/classified)
(def report-version 1)

(def ^:private original-physical-recheck
  physical/require-current-installed-physical!)
(def ^:private original-guarded-summary guarded/redacted-summary)
(def ^:private original-capability-report capability/capability-report!)
(def ^:private original-prepare tx/prepare)

(def ^:private open-obligations
  #{:guarded-plan-not-bound-to-observed-issued-executor
    :prestate-acquisition-origin-not-authenticated
    :same-xtdb-node-for-read-and-write-not-authenticated
    :dml-transaction-internal-prestate-not-authenticated
    :complete-guard-and-authorization-dependencies
    :unmodeled-valid-time-and-nested-values
    :actual-deployed-http-handler-not-authenticated
    :actual-xtdb-api-and-receipt-not-authenticated
    :complete-physical-writer-coverage
    :universal-physical-refinement})

(defn- reject! [reason]
  ;; Do not include a private observation, request, document, token or SQL.
  (throw (ex-info "Invalid installed-route/guarded-prestate correlation."
                  {:installed-route-guarded-correspondence/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- pinned-boundaries! []
  (when-not (and (identical? original-physical-recheck
                             (var-get #'physical/require-current-installed-physical!))
                 (identical? original-guarded-summary
                             (var-get #'guarded/redacted-summary))
                 (identical? original-capability-report
                             (var-get #'capability/capability-report!))
                 (identical? original-prepare (var-get #'tx/prepare)))
    (reject! :substituted-correlation-boundary)))

(defn- checked-guarded-diagnostic! [diagnostic]
  ;; This checks coherence of a *caller-provided map*, not its origin, and does
  ;; not validate that an executor actually used any of its plan statements.
  (when-not (plain-map? diagnostic)
    (reject! :invalid-guarded-diagnostic))
  (let [summary (try (original-guarded-summary diagnostic)
                     (catch Throwable _
                       (reject! :invalid-guarded-diagnostic)))
        original (:original-plan diagnostic)
        guarded-plan (:guarded-plan diagnostic)
        operations (:guarded-tx-ops diagnostic)
        inserted (:inserted-assertion-count diagnostic)
        targets (:observed-rows diagnostic)
        original-assertions (vec (:assertions original))
        new-assertions (:assertions guarded-plan)
        prepared (try (original-prepare nil guarded-plan)
                      (catch Throwable _ nil))]
    (when-not (and (= guarded/report-type (:type diagnostic))
                   (= guarded/report-version (:version diagnostic))
                   (plain-map? original)
                   (plain-map? guarded-plan)
                   (vector? operations)
                   (vector? targets)
                   (= (:observed-target-count diagnostic) (count targets))
                   (= (:observed-present-count diagnostic)
                      (count (filter :document targets)))
                   (pos-int? inserted)
                   (vector? new-assertions)
                   (<= inserted (count new-assertions))
                   ;; Typed :assertions become SQL ASSERT operations in tx/prepare.
                   (every? (fn [x]
                             (and (vector? x) (string? (first x))
                                  (boolean (re-find #"(?i)^ASSERT\b" (first x)))))
                           (take inserted (:tx-ops prepared)))
                   (= original-assertions
                      (subvec new-assertions inserted))
                   (= (dissoc original :assertions)
                      (dissoc guarded-plan :assertions))
                   (= guarded-plan (:plan prepared))
                   (= operations (:tx-ops prepared))
                   (= :conditional-unexecuted-plan-diagnostic
                      (:proof-status diagnostic))
                   (= false (:actual-execution-observed? diagnostic))
                   (= :not-established (:guarded-ops-execution-binding diagnostic))
                   (= :not-authenticated (:transaction-internal-prestate diagnostic))
                   (= :not-issued (:certificate diagnostic))
                   (= :not-admitted (:admission diagnostic))
                   (= :none (:authority diagnostic))
                   (= :none (:transaction-authority diagnostic))
                   (= :none (:retry-authority diagnostic))
                   (= :none (:settlement-authority diagnostic))
                   (= (boolean (seq original-assertions))
                      (:unknown-assertion-dependencies? diagnostic))
                   (= (select-keys diagnostic (keys summary)) summary))
      (reject! :incoherent-guarded-plan))
    {:status :caller-supplied-coherent-plan-unbound-to-execution
     :observed-target-count (:observed-target-count diagnostic)
     :inserted-assertion-count inserted
     :unknown-assertion-dependencies?
     (:unknown-assertion-dependencies? diagnostic)
     :acquisition-origin :not-authenticated
     :exact-plan-executed? :not-established
     :same-physical-node-as-observed-write? :not-established}))

(defn classify-installed-route-guarded!
  "Compose existing observations without invoking any handler, query or DML.

   Required input: {:physical-handle <v1480 opaque issued handle>
                    :physical-options <EXACT original v1480 options>
                    :physical-report <EXACT redacted v1480 report>}
   Optional :guarded-prestate is the PRIVATE complete v1484 acquisition map.

   Only the v1480 origin is checked with a live in-process witness. The v1484
   report is a structurally rechecked unbound diagnostic; no cross-component
   execution identity exists yet. No amount of copying reports changes that.
   A forged/stale physical handle or incoherent guard map fails closed. The
   returned report is redacted and never authorizes any action."
  [{:keys [physical-handle physical-options physical-report guarded-prestate]
    :as input}]
  (pinned-boundaries!)
  (when-not (and (plain-map? input)
                 (contains? #{#{:physical-handle :physical-options :physical-report}
                              #{:physical-handle :physical-options :physical-report
                                :guarded-prestate}}
                            (set (keys input)))
                 (plain-map? physical-options)
                 (plain-map? physical-report))
    (reject! :invalid-input-shape))
  (let [verified (try (original-physical-recheck physical-handle
                                                  physical-options
                                                  physical-report)
                      (catch Throwable _
                        (reject! :unverified-route-to-adapter-observation)))]
    (when-not (and (= physical/report-type (:type verified))
                   (= physical/report-version (:version verified))
                   (= :bounded-installed-route-to-adapter-agreement
                      (:status verified))
                   (true? (:actual-issued-executor-attempt-observed? verified))
                   (true? (:one-adapter-call-observed? verified))
                   (true? (:prepared-operations-agree? verified))
                   (true? (:prepared-options-agree? verified))
                   (true? (:definitive-execute-key-observed? verified))
                   (= 1 (:handler-invocation-count verified))
                   (= :bounded-conditional-process-local-observation-only
                      (:proof-status verified))
                   (= :not-issued (:certificate verified))
                   (= :not-admitted (:admission verified))
                   (= :none (:authority verified)))
      (reject! :unsupported-route-to-adapter-classification))
    (let [guarded-status
          (if (contains? input :guarded-prestate)
            (checked-guarded-diagnostic! guarded-prestate)
            {:status :no-guarded-prestate-diagnostic
             :acquisition-origin :not-observed
             :exact-plan-executed? :not-established
             :same-physical-node-as-observed-write? :not-established})
          backend (try (original-capability-report)
                       (catch Throwable _
                         (reject! :unavailable-xtdb-capability-report)))]
      (when-not (and (= capability/report-type (:type backend))
                     (= capability/report-version (:version backend))
                     (= :pinned-api-and-gesso-option-surface
                        (:compatibility backend)))
        (reject! :unsupported-xtdb-capability))
      {:type report-type
       :version report-version
       :status :bounded-installed-route-to-adapter-observation
       :scope :one-process-local-observation-with-optional-unbound-prestate
       :route-id (:route-id verified)
       :operation-id (:operation-id verified)
       :installed-route :rechecked-process-local-biff-handler
       :issued-executor :observed-in-same-controlled-request
       :physical-submission :one-matching-xtdb-adapter-call-site-observed
       :adapter-result :definitive-transaction-key-observed-but-unattested
       :guarded-prestate guarded-status
       :guarded-plan-to-physical-execution :not-established
       :transaction-internal-prestate :not-authenticated
       :actual-production-deployment :not-established
       :complete-physical-writer-coverage :not-established
       :universal-execution-refinement :not-established
       :open-obligations (set/union open-obligations (:open-obligations verified))
       :proof-status :bounded-conditional-process-local-observation-only
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :retry-authority :none
       :settlement-authority :none
       :authority :none})))

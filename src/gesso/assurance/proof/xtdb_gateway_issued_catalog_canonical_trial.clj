(ns gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial
  "v1776: Bind the v1775 checked-catalog physical trial to the EXACT issued
   v1251 certified-plan executor and the CURRENT assembled application's
   certified route/operation table. An observed physical transaction comes
   from calling the registered executor, not an arbitrary invocation thunk.

   Only local, source-relative identities are established. The caller still
   provides the contract source, application assembly, typed catalog,
   physical field projection, request context and command invocation. This
   does NOT independently attest the deployed Biff handler or prove that
   arbitrary registered callbacks implement business policy correctly."
  (:require [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial
             :as canonical]
            [gesso.assurance.transition :as constructor]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/issued-executor)
(def report-version 1)

(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-operation-summary application/operation-summary)
(def ^:private original-catalog-check catalog/check-operation-set!)
(def ^:private original-construct constructor/build-guarded-replacement!)
(def ^:private original-plan-require plan/require-current-plan-operation!)
(def ^:private original-plan-describe plan/describe-plan-operation)
(def ^:private original-canonical-issuer
  canonical/issue-catalog-bound-canonical-trial!)

(defn- reject! [reason]
  (throw (ex-info "Issued catalog/canonical executor binding is not established."
                  {:issued-catalog-canonical-trial/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary))
                 (identical? original-catalog-check
                             (var-get #'catalog/check-operation-set!))
                 (identical? original-construct
                             (var-get #'constructor/build-guarded-replacement!))
                 (identical? original-plan-require
                             (var-get #'plan/require-current-plan-operation!))
                 (identical? original-plan-describe
                             (var-get #'plan/describe-plan-operation))
                 (identical? original-canonical-issuer
                             (var-get #'canonical/issue-catalog-bound-canonical-trial!)))
    (reject! :substituted-original-assembly-or-issuer)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- current-binding! [contract-source catalog-source operation]
  (pinned!)
  (when-not (and (plain-map? contract-source)
                 (plain-map? catalog-source)
                 (= #{:catalog :physical-field-map} (set (keys catalog-source)))
                 (plain-map? (:catalog catalog-source))
                 (plain-map? (:physical-field-map catalog-source))
                 (qualified-keyword? operation))
    (reject! :invalid-current-assembled-catalog-source))
  (let [checked (try (original-catalog-check (:catalog catalog-source))
                     (catch Exception _ (reject! :invalid-original-catalog)))
        contracts (try (original-contract-check contract-source)
                       (catch Exception _
                         (reject! :invalid-current-certified-operation-table)))
        summary (try (original-operation-summary
                      (get-in contract-source
                              [:application-assembly :operation-acquisition-assembly]))
                     (catch Exception _ (reject! :invalid-current-operation-summary)))
        ids (:declared-operation-ids checked)
        operation-row (get summary operation)
        catalog-row (first (filter #(= operation (:id %))
                                   (get-in catalog-source [:catalog :operations])))
        bound (get-in contract-source [:certified-operations operation])
        executor (:executor bound)
        source (:current-source bound)
        description (when (and executor source)
                      (try (original-plan-require executor source)
                           (original-plan-describe executor)
                           (catch Exception _
                             (reject! :current-issued-plan-executor-not-established))))
        transition (when catalog-row
                     (try (original-construct (:construction catalog-row))
                          (catch Exception _
                            (reject! :invalid-original-catalog-construction))))
        routes (get operation-row :routes)
        route-ids (set (map :route-id routes))
        certified-row (get-in contracts [:certified-operations operation])]
    (when-not (and (= catalog/report-type (:type checked))
                   (= :checked (:exact-declared-operation-coverage checked))
                   (= certified/report-type (:type contracts))
                   (= certified/adapter-version (:version contracts))
                   (= ids (set (keys summary))
                      (set (keys (:certified-operations contract-source)))
                      (set (keys (:certified-operations contracts))))
                   (empty? (:uncertified-operations contracts))
                   (= :none (:transaction-authority contracts))
                   (= :none (:certificate contracts))
                   (contains? ids operation)
                   (= (:construction catalog-row) (:construction source))
                   (= (:physical-field-map catalog-source) (:field-map source))
                   (= operation (:operation description)
                      (:operation certified-row))
                   (= (get-in source [:certified-source :proof-source :transition])
                      (:transition transition))
                   (= (:transition-fingerprint transition)
                      (:transition-fingerprint description)
                      (:transition-fingerprint certified-row))
                   (= (:command-operation source)
                      (get-in (:construction source) [:subject :interface :operation]))
                   (vector? routes) (seq routes)
                   (= (count routes) (count route-ids))
                   (= route-ids (:route-ids certified-row))
                   (= route-ids (:matched-certified-route-ids contracts))
                   (= route-ids (:route-ids contracts)))
      (reject! :catalog-executor-or-assembled-operation-diverged))
    (pinned!)
    {:operation-id operation
     :catalog-fingerprint (:fingerprint checked)
     :transition-fingerprint (:transition-fingerprint transition)
     :proof-fingerprint (:proof-fingerprint certified-row)
     :route-ids route-ids
     ;; The issued callable is captured privately for invocation, never
     ;; included in a report or user-provided to the physical trial.
     :executor executor}))

(defn- report [operation status invoked?]
  {:type report-type :version report-version :operation-id operation
   :status status
   :catalog-to-checked-assembly :exact-closed-operation-coverage
   :catalog-to-issued-executor :exact-original-construction-and-field-map
   :certified-routes :current-issued-handlers-in-assembly
   :issued-executor-return (if invoked?
                             :one-original-plan-executor-returned
                             :not-established)
   :canonical-physical-execution
   (if (= status :bounded-issued-executor-catalog-atomic-acknowledgment)
     :one-source-relative-model-transaction-acknowledged
     :not-established)
   :deployed-biff-handler-adoption :not-established
   :original-business-policy-authenticity :not-established
   :current-principal-authorization :not-established
   :deployed-writer-completeness :not-established
   :physical-xtdb-receipt-authenticity :not-established
   :typed-complete-cas-writer-adoption :not-established
   :actual-live-delivery :not-established
   :retry-authority :none :certificate :not-issued
   :admission :not-admitted :transaction-authority :none :authority :none})

(defn issue-issued-catalog-canonical-trial!
  "Issue a one-use trial bound to the current certified assembly's EXACT
   original plan executor. Only [ctx invocation] are accepted at observation;
   the caller cannot substitute an invocation callback for the executor.
   The v1775 catalog-derived typed pre-submit validation remains in the same
   call. Recheck original plan, catalog and routes before and after execution.

   Never infer actual deployed Ring/Biff handler adoption or independent
   authenticity of callback effects from this local result."
  [contract-source catalog-source operation inputs]
  (when-not (plain-map? inputs)
    (reject! :invalid-typed-inputs))
  (let [initial (current-binding! contract-source catalog-source operation)
        original-executor (:executor initial)
        underlying (original-canonical-issuer catalog-source operation inputs)
        permit (AtomicBoolean. false)
        status (atom :not-attempted)
        returned? (atom false)
        recheck! (fn []
                   (when-not (= initial
                                (current-binding! contract-source catalog-source operation))
                     (reject! :current-issued-catalog-binding-changed))
                   (let [s ((:summary! underlying))]
                     (when-not (and (= canonical/report-type (:type s))
                                    (= canonical/report-version (:version s))
                                    (= operation (:operation-id s))
                                    (= :not-issued (:certificate s))
                                    (= :none (:authority s)))
                       (reject! :unqualified-original-catalog-trial)))
                   true)
        summary! (fn [] (recheck!) (report operation @status @returned?))
        observe! (fn [ctx invocation]
                   (when-not (.compareAndSet permit false true)
                     (reject! :issued-executor-trial-already-attempted))
                   (reset! status :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (and (map? ctx) (plain-map? invocation)
                                  (= #{:binding :mode} (set (keys invocation)))
                                  (= operation (get-in invocation [:binding :operation])))
                     (reject! :invalid-bound-executor-invocation))
                   (let [called? (atom false)
                         result ((:observe-once! underlying)
                                 (fn []
                                   (let [outcome (original-executor ctx invocation)]
                                     (reset! called? true)
                                     outcome)))
                         accepted? (and @called?
                                        (= canonical/report-type (:type result))
                                        (= canonical/report-version (:version result))
                                        (= operation (:operation-id result))
                                        (= :not-issued (:certificate result))
                                        (= :none (:authority result)))
                         qualified? (and accepted?
                                         (= :bounded-catalog-bound-atomic-acknowledgment
                                            (:status result)))]
                     (recheck!)
                     (when-not accepted?
                       (reject! :issued-executor-or-canonical-result-not-established))
                     (reset! returned? true)
                     (let [outcome (cond
                                     qualified?
                                     :bounded-issued-executor-catalog-atomic-acknowledgment
                                     (= :catalog-bound-canonical-rejected (:status result))
                                     :issued-executor-canonical-submission-rejected
                                     (= :catalog-bound-typed-check-not-established
                                        (:status result))
                                     :issued-executor-typed-check-rejected
                                     (= :no-canonical-submission-observed
                                        (:status result))
                                     :no-canonical-submission-observed
                                     :else :not-qualified-or-uncertain)]
                       (reset! status outcome)
                       (report operation outcome @returned?))))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :observe-once! observe!}))

(ns gesso.assurance.proof.checked-biff-http-application-configuration
  "v1843: Application-owned entry point for one selected CLOSED Biff HTTP
   startup. One declared contract source and selected adapter are enough:
   issue the ORIGINAL host, consumption probe and guarded adapter internally,
   derive the canonical module Var/order, and execute v1783's pre-init sealed
   startup. Callers cannot accidentally supply a different module inventory,
   an unissued proof record, or an earlier start order through this API.

   This is a consciously selected application STARTUP CONFIGURATION, not a
   replacement for Biff's unrestricted start API. This code does not control
   independent Biff instances, raw socket listeners, a remote deployment,
   authentic request principals or physical transaction writers. It issues
   no proof certificate, admission, retry or transaction authority.

   v1793: If a typed trial selects a catalog, its ENTIRE declared certified
   route set must match the current original application assembly before
   the sealed Biff startup touches an HTTP listener. The coverage checker
   does not establish installed/deployed handler identity or prove that the
   selected catalog includes every business rule.

   v1794: AFTER the actual sealed adapter startup, require the exact issued
   FINAL consumption probe wrapper to remain in the returned Biff system.
   Recheck the ORIGINAL host's captured pre-probe system and its full certified
   route inventory beneath the wrapper. Only one real loopback challenge is
   observed; other routes are not claimed to be individually exercised.

   v1798: Require that the original issued probe has confirmed EACH route
   selected in the source-checked catalog through the original checked router
   on the actual returned adapter BEFORE declaring startup successful.
   Parameterized routes have one observed witness, not universal coverage.
   v1799 qualifies route inventory and dispatch from ONE original issued
   probe and the SAME returned Biff system, not a separate success summary.

   v1800: Recheck the SAME managed in-process Biff system and original
   guarded adapter/probe lifecycle before reporting current instance status.
   Managed stops and uncertain cleanup cannot be requalified by a saved
   startup report. Out-of-band raw adapter stops or external deployment
   liveness are outside this local observation.

   v1801: Inspect and managed stop share one private lifecycle monitor,
   preventing a successful in-process inspection from overlapping the
   transition to stopping or the physical stop call. Each report is still
   strictly point-in-time; it is NOT a lease on future liveness.

   v1802: The current-instance check also compares the ORIGINAL live
   checked-host certified routes to the EXACT selected catalog route IDs
   accepted at this startup. No one-shot request dispatch challenge is
   repeated after business execution; no new policy or liveness authority
   is inferred from this scoped inventory correspondence.

   v1803: Inspection also requires a fresh read-only loopback challenge
   reaching the same issued final handler on the selected local adapter.
   This is a one-time observation, not a guarantee of future availability.

   v1804: Return one opaque original-issued STARTED instance generation only
   after successful startup. A new :inspect-generation! callback requires
   that exact generation AND the exact returned Biff system, and rechecks
   the current handler/host/HTTP observations. Old or unrelated generation
   handles cannot be exchanged between independent starts. The generation
   is a local equality witness, not a lease, auth credential, certificate
   or proof that a remote deployment remains available.

   v1805: The original probe captures the successful startup's loopback
   listener endpoint. Neither :inspect-running! nor :inspect-generation!
   may silently move a fresh liveness challenge to another local port
   when the caller-owned URL locator changes. Same-port replacement is
   not independently identifiable from a loopback response.

   v1806: The selected trusted adapter may supply :listener-instance!
   returning its actual in-process listener object. Configured startup
   captures that object after the real HTTP proof, and later inspections
   recheck exact reference identity before and after fresh HTTP liveness.
   Same-port replacement is detected conditional on truthful host callback
   instrumentation; this does not authenticate an external deployment.

   v1813: Publish the selected route set, exact returned Biff system, and
   :verified-configured-start atomically with respect to original managed
   stop/inspection. Previously the state could become verified while the
   returned system was still nil, letting a racing stop miss physical cleanup.
   The monitor does not control out-of-band adapter stops or future liveness.

   v1826: Re-check current state after the post-challenge listener identity
   callback. The monitor is reentrant: source-trusted host callbacks can
   synchronously call the managed stop closure on the SAME thread. Do not
   return a current-instance report after that stop invalidates the issued
   generation. This is still only process-local, callback-conditional evidence.

   v1828: Reject recursively invoked current-instance inspections from
   source-owned callbacks. JVM locks are reentrant; a callback in a local
   liveness challenge must not recursively perform additional inspections
   or enter an unbounded recursion. Release the in-progress guard on every
   failure without blocking later independent observations.

   v1829: A source callback can change a pinned original Var root after the
   first source-integrity check and before inspection reports success. Recheck
   all original boundary Var roots after the final trusted listener callback.
   This is point-in-time source integrity, not a guarantee against future
   mutation or hostile concurrent code.

   v1830: Re-pin ORIGINAL proof roots after the last startup-side host
   callback and before atomically publishing :verified-configured-start.
   If source-owned code mutates a proof Var during the final listener
   capture, fail startup and invoke existing physical cleanup rather than
   returning a verified application with a substituted boundary.

   v1831: The final trusted listener capture itself may close the physical
   listener AFTER the earlier startup HTTP consumption challenge. When that
   optional callback is present, require one additional original issued,
   read-only fresh HTTP response after capture before publishing verified
   startup. This is a final bounded local observation, not a liveness lease.

   v1832: After the final post-capture read-only HTTP challenge, recheck
   the exact original source-reported physical listener object. A second
   server on the SAME port may answer the original issued Ring wrapper while
   the captured listener has been stopped; successful HTTP alone must not
   allow that replacement to qualify as the original configured startup.

   v1833: A drifted original proof Var must not prevent managed shutdown from
   attempting the ORIGINAL captured physical Biff stop. Preserve the uncertain
   cleanup outcome rather than claiming clean stop when the source-integrity
   check failed. A proof-source substitution must not strand a live listener.

   v1834: Also re-pin the original proof roots after physical shutdown.
   Selected Biff stop callbacks are host code and may mutate original proof
   roots DURING cleanup. Even if the pre-stop pin passed and the physical
   listener closed, the managed result cannot claim verified clean shutdown
   when original source integrity is no longer established at completion.

   v1842: Retain the original selected contract's per-certified-operation
   route/premise/obligation evidence through startup and current-instance
   reports. Shared assumptions stay catalog-scoped. No operation-specific
   evidence remains a current-instance assertion after managed stop.

   v1843: Publish original selected operation/route inventory AND the
   separately identified uncertified application operations alongside the
   unresolved source-relative evidence. Never imply catalog selection is
   a complete business-policy inventory or current deployment certificate."
  (:require [clojure.set :as set]
            [com.biffweb.core :as biff]
            [gesso.assurance.proof.checked-authorization-assembly :as coverage]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.assurance.proof.checked-biff-http-module-assembly :as sealed]
            [gesso.live.application-preflight :as app])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.checked-biff-http-application-configuration/selected-start)
(def report-version 1)
(def running-report-type
  :gesso.assurance.proof.checked-biff-http-application-configuration/current-instance)

;; Deliberately NOT a UUID/string or data-shaped status report. An issued
;; generation is a process-local exact-identity witness and is never emitted
;; inside a redacted assurance summary.
(deftype StartedInstanceGeneration []
  Object
  (toString [_] "#<gesso.assurance/started-instance-generation>"))

;; The ONLY Biff module Var used by this configured entry point. Its dynamic
;; binding is scoped around BOTH the v1783 preflight and Biff startup.
(def ^:dynamic ^:private *configured-modules* [])

(def ^:private original-host-issuer host/issue-checked-biff-host-module!)
(def ^:private original-probe-issuer probe/issue-http-consumption-probe-module!)
(def ^:private original-guarded-issuer guarded/issue-checked-http-adapter-start!)
(def ^:private original-sealed-start sealed/start-closed-checked-http-application!)
(def ^:private original-biff-stop biff/stop)
(def ^:private original-route-coverage coverage/check-declared-route-coverage!)
(def ^:private original-probe-dispatch probe/require-current-final-catalog-dispatch!)
(def ^:private original-probe-inventory probe/require-current-final-probe-route-inventory!)
(def ^:private original-probe-live probe/require-fresh-local-http-liveness!)

(defn- reject! [reason]
  (throw (ex-info "Configured closed Biff HTTP startup not established."
                  {:checked-biff-http-application-configuration/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-issuer
                             (var-get #'host/issue-checked-biff-host-module!))
                 (identical? original-probe-issuer
                             (var-get #'probe/issue-http-consumption-probe-module!))
                 (identical? original-guarded-issuer
                             (var-get #'guarded/issue-checked-http-adapter-start!))
                 (identical? original-sealed-start
                             (var-get #'sealed/start-closed-checked-http-application!))
                 (identical? original-biff-stop
                             (var-get #'biff/stop))
                 (identical? original-route-coverage
                             (var-get #'coverage/check-declared-route-coverage!))
                 (identical? original-probe-dispatch
                             (var-get #'probe/require-current-final-catalog-dispatch!))
                 (identical? original-probe-inventory
                             (var-get #'probe/require-current-final-probe-route-inventory!))
                 (identical? original-probe-live
                             (var-get #'probe/require-fresh-local-http-liveness!)))
    (reject! :substituted-original-configured-start-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- host-listener-instance!
  "Read one source-trusted actual listener object. A missing, data-shaped
   or throwing host callback cannot establish object identity."
  [listener-instance! system]
  (let [instance (try (listener-instance! system)
                      (catch Throwable _
                        (reject! :configured-physical-listener-not-current)))]
    (when-not (and (some? instance)
                   (not (or (map? instance) (coll? instance)
                            (string? instance) (number? instance)
                            (keyword? instance) (symbol? instance)
                            (boolean? instance) (char? instance)
                            (fn? instance))))
      (reject! :configured-physical-listener-not-current))
    instance))

(defn- require-original-host-listener!
  "The original trusted callback must still return the exact object captured
   after startup. Does not independently authenticate the callback."
  [listener-instance! original-listener system]
  (when listener-instance!
    (when-not (and (some? original-listener)
                   (identical? original-listener
                               (host-listener-instance! listener-instance!
                                                        system)))
      (reject! :configured-physical-listener-not-current)))
  true)

(defn- checked-source! [config]
  (pinned!)
  (when-not (and (plain-map? config)
                 (contains? #{#{:contract-source :adapter-module :base-url!}
                              #{:contract-source :adapter-module :base-url!
                                :typed-trial}
                              #{:contract-source :adapter-module :base-url!
                                :listener-instance!}
                              #{:contract-source :adapter-module :base-url!
                                :listener-instance! :typed-trial}}
                            (set (keys config)))
                 (plain-map? (:contract-source config))
                 (fn? (:base-url! config))
                 (or (not (contains? config :listener-instance!))
                     (fn? (:listener-instance! config)))
                 (or (not (contains? config :typed-trial))
                     (nil? (:typed-trial config))
                     (and (plain-map? (:typed-trial config))
                          (contains? #{#{:catalog-source :operation :inputs}
                                       #{:catalog-source :operation :inputs
                                         :principal-policy :authorization-source}}
                                     (set (keys (:typed-trial config))))
                          (plain-map? (get-in config [:typed-trial :catalog-source]))
                          (plain-map? (get-in config [:typed-trial :inputs]))
                          (qualified-keyword? (get-in config [:typed-trial :operation])))))
    (reject! :invalid-application-owned-http-configuration))
  config)

(defn- valid-selected-operation-evidence? [checked]
  ;; Preserve ONLY exact rows emitted by the original catalog checker. Verify
  ;; they account for the same certified operations and entire selected route
  ;; inventory; a partial or malformed operation attribution must fail BEFORE
  ;; the physical HTTP listener starts. Never infer discharged obligations.
  (let [rows (:certified-operation-open-evidence checked)]
    (and (map? rows)
         (= (set (keys rows)) (:catalog-operations checked))
         (every? (fn [[operation row]]
                   (and (qualified-keyword? operation)
                        (map? row)
                        (= #{:route-ids :open-premises :open-obligations}
                           (set (keys row)))
                        (set? (:route-ids row))
                        (seq (:route-ids row))
                        (every? qualified-keyword? (:route-ids row))
                        (set? (:open-premises row))
                        (set? (:open-obligations row))))
                 rows)
         (= (:matched-certified-route-ids checked)
            (into #{} (mapcat :route-ids) (vals rows))))))

(defn- valid-selected-catalog-inventory? [checked]
  ;; Selection coverage must account explicitly for the original catalog and
  ;; the application operations it leaves uncertified. A source-relative
  ;; catalog cannot quietly become an all-application policy certificate.
  (let [selected (:catalog-operations checked)
        uncertified (:uncertified-application-operations checked)
        routes (:matched-certified-route-ids checked)]
    (and (set? selected)
         (seq selected)
         (every? qualified-keyword? selected)
         (set? uncertified)
         (every? qualified-keyword? uncertified)
         (empty? (set/intersection selected uncertified))
         (set? routes)
         (seq routes)
         (every? qualified-keyword? routes)
         (= :selected-not-all-possible-application-policy
            (:catalog-scope checked))
         (= :not-established (:authorization-policy-provenance checked)))))

(defn- require-selected-route-coverage! [contract-source typed-trial]
  ;; There is deliberately no caller-supplied checked report to trust. Only
  ;; selected typed execution requires this check. Run it BEFORE Biff init,
  ;; since an invalid selected catalog must never start an HTTP listener.
  (when typed-trial
    (pinned!)
    (let [checked (try
                    (original-route-coverage
                     {:contract-source contract-source
                      :catalog-source (:catalog-source typed-trial)})
                    (catch Exception _
                      (reject! :selected-catalog-route-coverage-not-checked)))]
      (when-not (and (= coverage/coverage-report-type (:type checked))
                     (= coverage/report-version (:version checked))
                     (= :all-declared-catalog-operations-and-issued-routes
                        (:checking checked))
                     (contains? (:catalog-operations checked)
                                (:operation typed-trial))
                     (= :source-relative-checked
                        (:current-assembly-closure checked))
                     (set? (:matched-certified-route-ids checked))
                     (seq (:matched-certified-route-ids checked))
                     (= :not-established (:installed-handler-adoption checked))
                     ;; Source-relative qualification must not silently erase
                     ;; the original certified contract's unresolved proof.
                     (set? (:open-assumptions checked))
                     (set? (:open-premises checked))
                     (set? (:open-obligations checked))
                     (set/subset? (:open-assumptions checked)
                                  (:open-obligations checked))
                     (valid-selected-operation-evidence? checked)
                     (valid-selected-catalog-inventory? checked)
                     (= :none (:transaction-authority checked)
                        (:authority checked))
                     (= :not-issued (:certificate checked))
                     (= :not-admitted (:admission checked)))
        (reject! :selected-catalog-route-coverage-not-checked))
      checked)))

(defn- require-matching-selected-host-routes!
  "Check a genuine host-adoption report against the previously checked
   selected catalog. Only the caller's original issued host observation can
   establish the report itself; this helper cannot authenticate reports."
  [selected host-report]
  (when-not (and (= coverage/coverage-report-type (:type selected))
                 (= coverage/report-version (:version selected))
                 (= :source-relative-checked
                    (:current-assembly-closure selected))
                 (set? (:matched-certified-route-ids selected))
                 (seq (:matched-certified-route-ids selected))
                 (= probe/final-catalog-dispatch-report-type (:type host-report))
                 (= probe/report-version (:version host-report))
                 (= probe/module-id (:module-id host-report))
                 (= host/module-id (:host-module-id host-report))
                 (= :one-original-probe-one-returned-system-and-host
                    (:scope host-report))
                 (= :exact-issued-wrapper
                    (:final-probe-handler-slot host-report))
                 (= :one-returned-in-process-probe-handler-slot
                    (:installed-handler-adoption host-report))
                 (= :checked-and-selected-dispatch-witnessed
                    (:normal-http-route-inventory host-report))
                 (set? (:checked-certified-route-ids host-report))
                 (set? (:checked-route-ids host-report))
                 (= (:matched-certified-route-ids selected)
                    (:checked-certified-route-ids host-report))
                 (set/subset? (:checked-certified-route-ids host-report)
                              (:checked-route-ids host-report))
                 (= :not-established (:external-deployment host-report))
                 (= :not-issued (:certificate host-report))
                 (= :not-admitted (:admission host-report))
                 (= :none (:authority host-report)
                    (:transaction-authority host-report)))
    (reject! :selected-catalog-route-inventory-not-adopted))
  true)

(defn- require-selected-catalog-dispatch!
  "Qualify one ORIGINAL probe inspection already bound to the real final
   Biff system, not a separate callback-supplied success summary."
  [selected observed]
  (let [expected (:matched-certified-route-ids selected)]
    (when-not (and (set? expected)
                   (seq expected)
                   (= probe/final-catalog-dispatch-report-type (:type observed))
                   (= probe/report-version (:version observed))
                   (= :one-original-probe-one-returned-system-and-host
                      (:scope observed))
                   (= :all-selected-certified-routes-observed-through-original-router
                      (:catalog-read-only-route-dispatch observed))
                   (= expected (:catalog-expected-certified-route-ids observed)
                      (:catalog-observed-certified-route-ids observed))
                   (= (count expected) (:selected-route-challenge-observations observed))
                   (= :not-invoked-by-read-only-probe
                      (:selected-route-business-handler-execution observed))
                   (set? (:catalog-parameterized-witness-route-ids observed))
                   (set/subset? (:catalog-parameterized-witness-route-ids observed)
                                expected)
                   (= :not-established (:parameterized-route-generalization observed))
                   (zero? (:typed-trial-attempts observed))
                   (= :none (:authority observed)
                      (:transaction-authority observed)
                      (:retry-authority observed))
                   (= :not-issued (:certificate observed))
                   (= :not-admitted (:admission observed)))
      (reject! :selected-catalog-http-dispatch-not-observed))
    true))

(defn- require-running-selected-route-continuity!
  "Require the CURRENT original host route inventory to equal the saved
   startup-selected certified catalog route set. This predicate accepts
   inventory evidence ONLY from the original issued probe inspection done
   by its caller, never as an independently issued certificate.

   An application WITHOUT a selected typed catalog must not acquire
   selected-catalog correspondence by implication."
  [typed-selected? startup-route-ids inventory]
  (when-not
   (if typed-selected?
     (and (set? startup-route-ids)
          (seq startup-route-ids)
          (every? qualified-keyword? startup-route-ids)
          (set? (:checked-certified-route-ids inventory))
          (set? (:checked-route-ids inventory))
          (= startup-route-ids (:checked-certified-route-ids inventory))
          (set/subset? startup-route-ids (:checked-route-ids inventory)))
     (nil? startup-route-ids))
    (reject! :configured-instance-selected-routes-not-current))
  true)

(defn- fresh-original-http-report?
  "Validate the original issued read-only loopback challenge. Used for
   both current inspection and final post-capture startup verification;
   the report alone never certifies future liveness or external deployment."
  [fresh]
  (and (= probe/fresh-local-http-liveness-report-type (:type fresh))
       (= probe/report-version (:version fresh))
       (= :responded-to-current-inspection-challenge (:status fresh))
       (= :one-fresh-local-loopback-response-on-original-issued-wrapper
          (:scope fresh))
       (= :exact-issued-wrapper (:original-final-handler-slot fresh))
       (= :same-verified-loopback-port
          (:original-start-listener-endpoint fresh))
       (= :not-forwarded-to-business-router (:read-only-request fresh))
       (= :observed-at-inspection-only (:selected-local-listener fresh))
       (= :not-established
          (:external-listener-liveness fresh)
          (:future-liveness fresh))
       (= :not-issued (:certificate fresh))
       (= :not-admitted (:admission fresh))
       (= :none (:authority fresh)
          (:transaction-authority fresh)
          (:retry-authority fresh))))

(defn- selected-open-evidence [selected]
  ;; These are unresolved obligations of the ORIGINAL checked catalog source.
  ;; They are carried as STARTUP evidence, never reclassified as proof of
  ;; current authorization, installed adoption or physical XTDB authority.
  (if selected
    {:selected-catalog-open-assumptions (:open-assumptions selected)
     :selected-catalog-open-premises (:open-premises selected)
     :selected-catalog-open-obligations (:open-obligations selected)
     :selected-catalog-operation-open-evidence
     (:certified-operation-open-evidence selected)
     :selected-catalog-operation-ids (:catalog-operations selected)
     :selected-catalog-route-ids (:matched-certified-route-ids selected)
     :selected-catalog-uncertified-operation-ids
     (:uncertified-application-operations selected)
     :selected-catalog-proof-evidence-scope :original-startup-source-only}
    {:selected-catalog-open-assumptions :not-established
     :selected-catalog-open-premises :not-established
     :selected-catalog-open-obligations :not-established
     :selected-catalog-operation-open-evidence :not-established
     :selected-catalog-operation-ids :not-established
     :selected-catalog-route-ids :not-established
     :selected-catalog-uncertified-operation-ids :not-established
     :selected-catalog-proof-evidence-scope :not-established}))

(defn- report [state typed-selected? selected]
  (merge
   {:type report-type :version report-version
   :scope :one-application-owned-closed-biff-http-start
   :status state
   :startup-configuration :one-contract-and-selected-adapter
   :module-inventory :internally-derived-original-four-modules
   :module-order :internally-derived-canonical
   :real-http-consumption
   (if (= state :verified-configured-start)
     :selected-adapter-challenge-passed-during-start
     :not-established)
   :typed-catalog-execution :optional-separately-observed
   :declared-route-coverage-at-start
   (if (and typed-selected? (= state :verified-configured-start))
     :selected-certified-catalog-routes-source-checked-before-start
     :not-established)
   :returned-host-selected-route-inventory
   (if (and typed-selected? (= state :verified-configured-start))
     :exact-selected-certified-route-set-observed-beneath-final-probe-wrapper
     :not-established)
   :selected-catalog-read-only-http-dispatch
   (if (and typed-selected? (= state :verified-configured-start))
     :all-selected-certified-routes-observed-before-start-return
     :not-established)
   :unprobed-routes-over-real-http :not-established
   :parameterized-route-generalization :not-established
   :managed-instance-liveness :requires-current-instance-inspection
   :selected-catalog-business-policy-completeness :not-established
   :external-deployment :not-established
   :independent-biff-starts :not-established
   :uncontrolled-socket-listeners :not-established
   :authenticated-principal :not-established
   :complete-physical-writer-coverage :not-established
   :xtdb-physical-receipt :not-established
   :actual-live-delivery :not-established
   :retry-authority :none :transaction-authority :none
   :certificate :not-issued :admission :not-admitted :authority :none}
   (selected-open-evidence
    (when (= state :verified-configured-start) selected))))

(defn issue-configured-checked-http-application!
  "Issue a one-use, application-owned startup from exactly a checked
   contract, selected raw adapter module, and local adapter URL locator.
   Optional :typed-trial chooses a catalog-derived one-use execution observer.

   The result does NOT expose a modules Var or customizable start-order.
   (:start-once! issued) takes only an initial Biff system; startup modules
   are derived from the captured contract and adapter, checked by v1783,
   and run through the original canonical Gesso/Biff lifecycle. The optional
   (:stop! issued) stops the selected Biff system at most once. A failed start
   consumes issuance; it cannot promise rollback of earlier callback effects."
  [source]
  (let [{:keys [contract-source adapter-module base-url! typed-trial
               listener-instance!]}
        (checked-source! source)
        ;; Issuance itself validates original router, original proof
        ;; construction and the selected adapter before any server startup.
        issued-host (original-host-issuer contract-source)
        issued-probe (original-probe-issuer (:handle issued-host)
                                            contract-source typed-trial)
        issued-guarded (original-guarded-issuer issued-probe
                                               adapter-module base-url!)
        modules [app/application-handler-module (:module issued-host)
                 (:module issued-probe) (:module issued-guarded)]
        order [app/application-handler-module-id host/module-id
               probe/module-id guarded/module-id]
        consumed (AtomicBoolean. false)
        state (atom :issued)
        started (atom nil)
        lifecycle-monitor (Object.)
        inspection-in-progress? (atom false)
        instance-generation (StartedInstanceGeneration.)
        selected-startup-routes (atom nil)
        selected-startup-evidence (atom nil)
        original-listener-instance (atom nil)
        summary! (fn [] (pinned!)
                   (report @state (some? typed-trial)
                           @selected-startup-evidence))
        inspect-running! (fn [candidate-system]
                           (locking lifecycle-monitor
                           ;; The monitor serializes OTHER threads, but not
                           ;; recursive calls made by host callbacks on this
                           ;; same thread. Never let one inspection spawn a
                           ;; second fresh challenge inside the first.
                           (when-not (compare-and-set! inspection-in-progress?
                                                       false true)
                             (reject! :configured-instance-inspection-reentrant))
                           (try
                           ;; Historical startup success cannot be reused
                           ;; as present-tense local instance evidence.
                           (pinned!)
                           (when-not (and (= :verified-configured-start @state)
                                          (map? candidate-system)
                                          (identical? candidate-system @started))
                             (reject! :configured-instance-not-current))
                           (let [guard-status (try ((:summary! issued-guarded))
                                                   (catch Throwable _ nil))
                                 inventory (try
                                             (original-probe-inventory
                                              issued-probe contract-source
                                              candidate-system)
                                             (catch Throwable _ nil))]
                             (when-not (and (= :verified-selected-adapter-start
                                                (:status guard-status))
                                            (= :not-attempted
                                               (:selected-adapter-cleanup guard-status))
                                            (= probe/final-route-inventory-report-type
                                               (:type inventory))
                                            (= :exact-issued-wrapper
                                               (:final-probe-handler-slot inventory))
                                            (= :verified-on-selected-loopback-adapter
                                               (:one-real-http-consumption-challenge inventory))
                                            (set? (:checked-certified-route-ids inventory))
                                            (= :none (:authority inventory)
                                               (:transaction-authority inventory))
                                            (= :verified-configured-start @state)
                                            (identical? candidate-system @started))
                               (reject! :configured-instance-not-current))
                             ;; Recheck the actual listener object reported
                             ;; by the original trusted host callback.
                             (require-original-host-listener!
                              listener-instance! @original-listener-instance
                              candidate-system)
                             ;; Reuse the CURRENT original issued probe and
                             ;; checked host inventory without replaying the
                             ;; one-use read-only dispatch challenges. The
                             ;; complete selected set must remain identical
                             ;; to the set checked before this startup.
                             (require-running-selected-route-continuity!
                              (some? typed-trial) @selected-startup-routes inventory)
                             ;; A fresh original issued probe challenge is
                             ;; required for this exact local listener. The
                             ;; captured URL locator supplies the target only;
                             ;; the original wrapper owns the new nonce.
                             (let [fresh (try
                                           (pinned!)
                                           (original-probe-live
                                            issued-probe contract-source
                                            candidate-system (base-url! candidate-system))
                                           (catch Throwable _
                                             (reject! :configured-instance-http-liveness-not-observed)))]
                               (when-not (fresh-original-http-report? fresh)
                                 (reject! :configured-instance-http-liveness-not-observed)))
                             (require-original-host-listener!
                              listener-instance! @original-listener-instance
                              candidate-system)
                             ;; Java monitors are reentrant. Even though a
                             ;; competing thread cannot stop this instance
                             ;; during inspection, a trusted callback invoked
                             ;; above can synchronously call the issued stop!
                             ;; closure on THIS thread. That closes the real
                             ;; listener before we return the current report.
                             ;; Never publish a successful inspection after
                             ;; reentrant managed shutdown has invalidated
                             ;; its original state or returned system.
                             ;; Source callbacks can also rebind an original
                             ;; proof boundary Var after our entry pin. Re-pin
                             ;; AFTER the final callback, before reporting.
                             (pinned!)
                             (when-not (and (= :verified-configured-start @state)
                                            (identical? candidate-system @started))
                               (reject! :configured-instance-not-current))
                             (merge
                              {:type running-report-type :version report-version
                              :scope :one-original-managed-in-process-biff-instance
                              :status :current-at-inspection
                              :returned-system :exact-identity
                              :original-guarded-adapter :verified-not-stopped
                              :original-probe-handler-slot :exact-issued-wrapper
                              :selected-catalog-source
                              (if typed-trial :checked-at-startup-only :not-selected)
                              :selected-catalog-route-continuity
                              (if typed-trial :matches-current-original-host-route-set
                                  :not-selected)
                              :external-listener-liveness :not-established
                              :out-of-band-adapter-stops :not-established
                              :fresh-local-http-response :observed-at-current-inspection
                              :original-local-listener-endpoint
                              :matches-startup-verified-loopback-port
                              :physical-listener-instance
                              (if listener-instance!
                                :matches-original-host-reported-object
                                :not-established)
                              :physical-instance-provenance
                              (if listener-instance!
                                :trusted-host-callback-conditional
                                :not-established)
                              :future-local-listener-availability :not-established
                              :certificate :not-issued :admission :not-admitted
                              :retry-authority :none :transaction-authority :none
                              :authority :none}
                              (selected-open-evidence @selected-startup-evidence)))
                             (finally
                               (reset! inspection-in-progress? false)))))
        inspect-generation!
        (fn [candidate-generation candidate-system]
          ;; Keep the generation comparison and the fresh observation
          ;; serialized with the managed stop. The original inspection is
          ;; reentrant under the same JVM monitor, not a copied report.
          (locking lifecycle-monitor
            (when-not (and (instance? StartedInstanceGeneration
                                       candidate-generation)
                           (identical? candidate-generation instance-generation)
                           (= :verified-configured-start @state)
                           (identical? candidate-system @started))
              (reject! :configured-instance-generation-not-current))
            (assoc (inspect-running! candidate-system)
                   :generation-binding
                   :exact-original-issued-started-instance)))
        start! (fn [initial-system]
                 (when-not (.compareAndSet consumed false true)
                   (reject! :configured-start-already-attempted))
                 (when-not (compare-and-set! state :issued :starting)
                   (reject! :configured-start-stopped-before-use))
                 (try
                   (pinned!)
                   (when-not (and (plain-map? initial-system)
                                  (not (contains? initial-system
                                                  app/biff-ring-handler-key)))
                     (reject! :invalid-or-preinstalled-biff-initial-system))
                   ;; The route and exact issued handler coverage must be
                   ;; checked against CURRENT original sources before Biff
                   ;; initializes modules or installs the real HTTP listener.
                   (let [selected-coverage
                         (require-selected-route-coverage! contract-source typed-trial)
                         result (binding [*configured-modules* modules]
                                  (original-sealed-start
                                   (:application-assembly contract-source)
                                   initial-system issued-host issued-probe
                                   issued-guarded #'*configured-modules* order))]
                     (try
                       (when-not (and (= sealed/report-type
                                         (get-in result [:report :type]))
                                      (= :one-selected-verified-adapter-start
                                         (get-in result [:report :physical-server-start]))
                                      (= :none (get-in result [:report :authority])))
                         (reject! :unqualified-closed-http-start))
                       (when selected-coverage
                         ;; The probe runs AFTER the host, and intentionally
                         ;; wraps its checked handler. Verify that exact FINAL
                         ;; probe wrapper and the original host's pre-probe
                         ;; route inventory under a single issued handle.
                         (let [observed (try
                                          (pinned!)
                                          (original-probe-dispatch
                                           issued-probe contract-source
                                           (:system result)
                                           (:matched-certified-route-ids selected-coverage))
                                          (catch Throwable _
                                            (reject! :selected-catalog-route-inventory-not-adopted)))]
                           (require-matching-selected-host-routes!
                            selected-coverage observed)
                           ;; The v1797 probe's original closure owns the
                           ;; per-route HTTP response and exact router match.
                           ;; Missing even one route prevents startup success.
                           (require-selected-catalog-dispatch!
                            selected-coverage observed)))
                       ;; Capture after the real HTTP startup checks. Failure
                       ;; here is handled by the existing adapter cleanup.
                       (when listener-instance!
                         (reset! original-listener-instance
                                 (host-listener-instance!
                                  listener-instance! (:system result)))
                         ;; The trusted capture callback is app-owned: it can
                         ;; stop the real adapter AFTER the earlier startup
                         ;; HTTP proof. Require another original read-only
                         ;; loopback challenge after it completes. A closed
                         ;; listener must not be published as verified merely
                         ;; because the older consumption challenge succeeded.
                         (pinned!)
                         (let [fresh (try
                                       (original-probe-live
                                        issued-probe contract-source
                                        (:system result)
                                        (base-url! (:system result)))
                                       (catch Throwable _
                                         ;; Do not erase an original proof-root
                                         ;; substitution by reclassifying it as
                                         ;; a mere local HTTP reachability loss.
                                         (pinned!)
                                         (reject!
                                          :configured-start-post-capture-http-liveness-not-observed)))]
                           (when-not (fresh-original-http-report? fresh)
                             (reject!
                              :configured-start-post-capture-http-liveness-not-observed)))
                         ;; An app-owned URL locator may replace the original
                         ;; physical listener with another server on the SAME
                         ;; port before the fresh challenge. That replacement
                         ;; can reuse the issued Ring wrapper and answer the
                         ;; nonce, yet it is not the captured original server.
                         ;; Compare exact trusted object identity AFTER the
                         ;; final HTTP response, as current inspection does.
                         (require-original-host-listener!
                          listener-instance! @original-listener-instance
                          (:system result)))
                       ;; Publish all original issued startup handles
                       ;; before the verified state is externally visible.
                       ;; The same monitor serializes managed stop and both
                       ;; current-instance inspection entry points. No stop
                       ;; may consume a verified state with a nil system.
                       (locking lifecycle-monitor
                         ;; The last source-trusted listener callback above
                         ;; may change a pinned original proof Var root after
                         ;; all earlier startup checks succeeded. Never
                         ;; publish verified startup from such a boundary.
                         ;; Keep this inside the publication monitor and
                         ;; BEFORE any started handle becomes visible.
                         (pinned!)
                         (reset! selected-startup-routes
                                 (:matched-certified-route-ids selected-coverage))
                         ;; Retain unresolved source assumptions only after
                         ;; all original route/host/HTTP checks have passed.
                         (reset! selected-startup-evidence selected-coverage)
                         (reset! started (:system result))
                         (when-not (compare-and-set! state :starting
                                                      :verified-configured-start)
                           (reject! :configured-start-state-diverged))
                         {:system (:system result)
                          :instance-generation instance-generation
                          :report (report @state (some? typed-trial)
                                          @selected-startup-evidence)})
                       (catch Throwable error
                         ;; The adapter has already started. Any failed
                         ;; post-start coverage check MUST stop it before
                         ;; propagating failure; cleanup can still fail.
                         (when-let [system (:system result)]
                           (try (original-biff-stop system)
                                (catch Throwable _ nil)))
                         (throw error))))
                   (catch Throwable error
                     ;; A selected adapter that opened a port must implement
                     ;; its own stop cleanup on failed Biff startup.
                     (reset! state :start-failed-physical-state-unknown)
                     (throw error))))
        stop! (fn []
                (locking lifecycle-monitor
                (loop []
                  (let [current @state]
                    (case current
                      :starting (reject! :configured-start-in-progress)
                      :stopping (reject! :configured-stop-in-progress)
                      :issued
                      (if (compare-and-set! state :issued
                                            :stopped-without-successful-start)
                        (summary!) (recur))
                      :verified-configured-start
                      (if (compare-and-set! state :verified-configured-start
                                            :stopping)
                        (do
                          (try
                            ;; Source integrity may drift while the real
                            ;; listener is running. Still attempt the ORIGINAL
                            ;; captured physical stop: pin failure must not
                            ;; strand a live listener. Never claim a clean
                            ;; shutdown if the pinned boundary was altered.
                            (let [source-pinned?
                                  (try (pinned!) true
                                       (catch Throwable _ false))]
                            (original-biff-stop @started)
                            ;; A returned stop call is not enough: require
                            ;; original guarded adapter and probe cleanup.
                            (let [guard-status (try ((:summary! issued-guarded))
                                                    (catch Throwable _ nil))
                                  probe-status (try ((:summary! issued-probe))
                                                    (catch Throwable _ nil))
                                  ;; Biff's selected stop callbacks may mutate
                                  ;; original proof Var roots AFTER the entry
                                  ;; check. Re-pin after physical cleanup too;
                                  ;; neither clean listener closure nor stale
                                  ;; entry integrity proves source identity at
                                  ;; the time we publish the stop report.
                                  source-pinned-after-stop?
                                  (try (pinned!) true
                                       (catch Throwable _ false))]
                              (reset! state
                                      (if (and source-pinned?
                                               source-pinned-after-stop?
                                               (= :stopped (:status guard-status))
                                               (= :completed
                                                  (:selected-adapter-cleanup guard-status))
                                               (= :stopped (:module-state probe-status)))
                                        :stopped
                                        :stop-cleanup-not-established))))
                            (catch Throwable _
                              (reset! state :stop-cleanup-not-established)))
                          (summary!))
                        (recur))
                      ;; A failed startup may have unrolled partially and may
                      ;; still have an external listener: retain uncertainty.
                      (summary!))))))]
    {:start-once! start! :stop! stop! :summary! summary!
     :inspect-running! inspect-running!
     :inspect-generation! inspect-generation!}))

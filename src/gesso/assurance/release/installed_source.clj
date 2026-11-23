(ns gesso.assurance.release.installed-source
  "v2307: Verified original cleanup retry; source-RECHECKED selected-catalog observation through ONE original,
   guarded, physically started Biff HTTP generation.

   Join the v2261 ORIGINAL abstract catalog checks to the catalog supplied to
   v1784's original managed Biff startup, and its CURRENT physical listener,
   full selected route inventory and fresh HTTP challenge. The same frozen
   catalog must appear in both selections. It is not enough to combine two
   independently supplied 'green' reports.

   This is an opt-in LOCAL qualification helper for a host-owned callback and
   ONE physical listener. Even a successful report is time-of-inspection
   evidence, NOT admission, install/activation permission, a proof theorem,
   authenticated external deployment, writer coverage, or release authority.
   A second opt-in :assess-current-installation! evaluates the actual selected
   source and running local Biff listener as candidate/installation PHASES of
   v2259's diagnostic release evaluator. Its negative physical controls
   deliberately submit incorrect opaque-generation and system identities to
   the ORIGINAL configured Biff inspector, then recheck the healthy instance.
   Activation and completion remain blocked by unobserved subjects. This is
   a narrower LOCAL qualification, not Stage 11 release eligibility.
   v2307: Startup claims :starting under monitor, but the original checked
   Biff startup and source/HTTP inspection execute outside it. A configured
   original Biff callback may request this issuer's summary on another
   thread. External stops are refused throughout :starting; the startup
   owner alone claims failed-start cleanup, and publishes :running only
   after the final original source/HTTP inspection has passed.

   v2305: The one-shot selected-source cleanup retry must also release its
   lifecycle monitor before invoking the original configured Biff retry.
   An original physical retry callback may synchronously consult this
   issuer's summary from another thread. Complete source/issuer identity
   rechecks and the original retry-state prerequisite under monitor, then
   publish :retrying-cleanup before releasing it for original callbacks.
   Competing retries and stops remain excluded until terminal publication;
   failed callbacks consume the one original retry without reopening
   installation or release authority.

   v2303: A successful external stop must not retain this source issuer's
   lifecycle monitor while invoking the original checked Biff physical-stop
   callback. An original host callback may synchronously ask another thread
   to inspect this issuer's summary; holding monitor across that callback
   creates a cross-thread deadlock. Publish :stopping exclusively under
   monitor, perform physical cleanup outside it, then publish the terminal
   result and revoke the retained installation evidence under monitor.
   Concurrent stops are rejected during the in-flight original callback.
   Startup/assessment-internal failure cleanup is a separate path and does
   not gain general asynchronous or release authority from this revision.

   v2285: Optionally forward ONE verified original configured Biff cleanup
   retry to a previously unconfirmed local instance. The original selected
   host must explicitly declare an idempotent stop and an original-listener
   shutdown observer. Neither a returned callback nor a copied status map
   proves cleanup. Preserve original catalog identity and forbid reopening
   traffic or requalifying a stopped listener. No authority is issued.
   No application command is executed by these read-only checks. Host-owned
   callbacks, catalog completeness, the original Biff implementation, and
   anything outside this in-process instance remain explicit assumptions."
  (:require [gesso.assurance.release.operation-source :as source]
            [gesso.assurance.release.decision :as decision]
            [gesso.assurance.proof.checked-biff-http-application-configuration
             :as http]))

(def report-type :gesso.assurance.release.installed-source/current-local-instance)
(def report-version 1)
(def ^:private original-source-check source/assess-original-catalog!)
(def ^:private original-http-issuer http/issue-configured-checked-http-application!)
(def ^:private retained-statuses
  #{:issued :starting :running :stopping :stopped :cleanup-unconfirmed :retrying-cleanup})

(defn- reject! [reason]
  (throw (ex-info "Selected source is not bound to a current checked Biff HTTP instance."
                  {:release-installed-source/rejection true :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- pinned! []
  (when-not (and (identical? original-source-check
                             (var-get #'source/assess-original-catalog!))
                 (identical? original-http-issuer
                             (var-get #'http/issue-configured-checked-http-application!)))
    (reject! :substituted-original-source-or-http-issuer)))

(defn- checked-source! [catalog! config expected-catalog expected]
  (pinned!)
  (let [current (catalog!)
        r (original-source-check catalog!)
        selected (get-in r [:selected-release-diagnostic :decisions :candidate])
        inventory (get-in r [:selected-release-diagnostic :initial-inventory])
        final-inventory (get-in r [:selected-release-diagnostic :final-inventory])
        positive-rows (filter #(= :positive (:kind %)) (vals (:rows selected)))]
    (pinned!)
    (when-not (and (= current expected-catalog)
                   (= current (get-in config [:typed-trial :catalog-source :catalog]))
                   (= source/report-type (:type r))
                   (= source/report-version (:version r))
                   (= (:selected-catalog-fingerprint expected)
                      (:selected-catalog-fingerprint r))
                   (= (:operation-ids expected) (:operation-ids r))
                   (= (:invariant-ids expected) (:invariant-ids r))
                   (= (:source-open-obligations expected)
                      (:source-open-obligations r))
                   (= (:source-open-meta-obligations expected)
                      (:source-open-meta-obligations r))
                   (= :matched (:status inventory) (:status final-inventory))
                   (seq positive-rows)
                   (= (+ (count (:operation-ids r))
                         (count (:invariant-ids r)) 1)
                      (count positive-rows))
                   (every? #(= :passed (:status %)) positive-rows)
                   (= (:candidate-status expected) (:candidate-status r))
                   (= (:installation-status expected) (:installation-status r))
                   (contains? #{:checks-passed-only :blocked}
                              (:candidate-status r))
                   (contains? #{:blocked :not-examined}
                              (:installation-status r))
                   (= :blocked (:status r))
                   (= :not-examined (:activation-status r)
                      (:completion-status r))
                   (= :none (:release-authority r)
                      (:runtime-effect-permission r))
                   (= :not-issued (:candidate-admission r)
                      (:installation-eligibility r)
                      (:activation-eligibility r)
                      (:completion-claim r)))
      (reject! :selected-original-catalog-not-current))
    r))

(defn- check-running! [source-report inspection expected-routes]
  (pinned!)
  (let [ops (:operation-ids source-report)
        rows (:selected-catalog-operation-open-evidence inspection)
        routes (:selected-catalog-route-ids inspection)]
    (when-not
     (and (= http/running-report-type (:type inspection))
          (= http/report-version (:version inspection))
          (= :current-at-inspection (:status inspection))
          (= :exact-original-issued-started-instance
             (:generation-binding inspection))
          (= :exact-identity (:returned-system inspection))
          (= :verified-not-stopped (:original-guarded-adapter inspection))
          (= :exact-issued-wrapper (:original-probe-handler-slot inspection))
          (= :checked-at-startup-only (:selected-catalog-source inspection))
          (= :matches-current-original-host-route-set
             (:selected-catalog-route-continuity inspection))
          (= :matches-original-startup-probe-route-ids
             (:original-host-route-inventory-continuity inspection))
          (= :observed-at-current-inspection
             (:fresh-local-http-response inspection))
          (= :matches-startup-verified-loopback-port
             (:original-local-listener-endpoint inspection))
          (= :matches-original-host-reported-object
             (:physical-listener-instance inspection))
          (= :trusted-host-callback-conditional
             (:physical-instance-provenance inspection))
          (= ops (:selected-catalog-operation-ids inspection))
          (set? routes) (seq routes)
          (or (nil? expected-routes) (= expected-routes routes))
          (map? rows) (= ops (set (keys rows)))
          (= routes (into #{} (mapcat :route-ids) (vals rows)))
          (every? (fn [[_ row]]
                    (and (= #{:route-ids :open-premises :open-obligations}
                            (set (keys row)))
                         (set? (:route-ids row)) (seq (:route-ids row))
                         (set? (:open-premises row))
                         (set? (:open-obligations row))))
                  rows)
          (set? (:selected-catalog-uncertified-operation-ids inspection))
          (set? (:selected-catalog-uncertified-route-ids inspection))
          (= :original-startup-source-only
             (:selected-catalog-proof-evidence-scope inspection))
          (set? (:selected-catalog-open-premises inspection))
          (set? (:selected-catalog-open-obligations inspection))
          (set? (:selected-catalog-open-assumptions inspection))
          (= :not-established (:external-listener-liveness inspection)
             (:out-of-band-adapter-stops inspection)
             (:future-local-listener-availability inspection))
          (= :none (:authority inspection)
             (:transaction-authority inspection)
             (:retry-authority inspection))
          (= :not-issued (:certificate inspection))
          (= :not-admitted (:admission inspection)))
      (reject! :original-selected-running-http-evidence-not-current))
    routes))

(defn- observation [source-report inspection routes]
  {:type report-type :version report-version
   :status :selected-source-and-current-local-http-observed-only
   :scope :one-original-managed-in-process-biff-http-instance
   :selected-catalog-id (:selected-catalog-id source-report)
   :selected-catalog-fingerprint (:selected-catalog-fingerprint source-report)
   :selected-operation-ids (:operation-ids source-report)
   :selected-invariant-ids (:invariant-ids source-report)
   :selected-http-route-ids routes
   :uncertified-application-operation-ids
   (:selected-catalog-uncertified-operation-ids inspection)
   :uncertified-application-route-ids
   (:selected-catalog-uncertified-route-ids inspection)
   :selected-operation-open-evidence
   (:selected-catalog-operation-open-evidence inspection)
   :original-candidate-diagnostic-status (:candidate-status source-report)
   :original-candidate-blockers
   (get-in source-report [:selected-release-diagnostic :decisions :candidate :blockers])
   :original-source-obligations (:source-open-obligations source-report)
   :original-source-meta-obligations (:source-open-meta-obligations source-report)
   :installed-catalog-open-premises (:selected-catalog-open-premises inspection)
   :installed-catalog-open-obligations (:selected-catalog-open-obligations inspection)
   :current-local-listener :fresh-original-http-challenge-and-identity-observed
   :generation-binding :original-opaque-generation-rechecked
   :source-completeness :host-selected-catalog-only
   :physical-listener-authenticity :trusted-host-callback-conditional
   :installed-business-command-executed :not-established
   :external-deployment :not-established
   :all-application-writer-coverage :not-established
   :authenticated-principal :not-established
   :physical-xtdb-write-correspondence :not-established
   :browser-and-live-adoption :not-established
   :original-release-installation-decision
   (if (= :checks-passed-only (:candidate-status source-report))
     :blocked-on-physical-release-eligibility
     :not-examined-because-candidate-blocked)
   :accepted-premises #{:gesso.release.premise/source-obligations-open
                        :gesso.release.premise/trusted-host-listener-callback}
   :conditional? true
   :release-authority :none :candidate-admission :not-issued
   :installation-eligibility :not-issued
   :activation-eligibility :not-issued :completion-claim :not-issued
   :transaction-authority :none :runtime-effect-permission :none})

(def local-assessment-type
  :gesso.assurance.release.installed-source/selected-local-installation-diagnostic)
(def local-assessment-version 1)
(def ^:private local-source-premise
  :gesso.release.premise/source-obligations-open)
(def ^:private local-listener-premise
  :gesso.release.premise/trusted-host-listener-callback)
(def ^:private negative-identity-boundary
  :gesso.release.local/wrong-physical-identity-rejected)

(defn- local-gate [id requirement kind class mechanism version scope boundary premises]
  {:id id :requirement requirement :kind kind :class class
   :mechanism mechanism :mechanism-version version :scope scope
   :allowed-premises premises :expected-boundary boundary})

(defn- local-intent [source-report installation-id]
  ;; This is the declared denominator of ONE local installed-instance
  ;; observation, not the complete R5.1 application release denominator.
  (let [source-mechanism :gesso.assurance.proof/operation-set
        source-scope :gesso.release/declared-abstract-catalog-only
        http-mechanism
        :gesso.assurance.proof/checked-biff-http-application-configuration
        http-scope :gesso.release/one-current-in-process-http-instance
        unconfigured :gesso.release/physical-observation-not-configured
        requirements {:gesso.release.local/source
                      :gesso.release/selected-original-catalog
                      :gesso.release.local/installation
                      :gesso.release/one-current-in-process-http-instance
                      :gesso.release.local/activation
                      :gesso.release/installed-browser-not-established
                      :gesso.release.local/completion
                      :gesso.release/release-completion-not-established}
        gate (fn [id requirement kind class mechanism version scope boundary premises]
               (local-gate id requirement kind class mechanism version
                           scope boundary premises))
        source-positive (gate :gesso.release.local/source-positive
                              :gesso.release.local/source :positive :structural
                              source-mechanism "operation-set-v1" source-scope nil
                              #{local-source-premise})
        source-negative (gate :gesso.release.local/source-negative
                              :gesso.release.local/source :negative :structural
                              source-mechanism "operation-set-v1" source-scope
                              :gesso.release.source/omitted-original-declaration-detected
                              #{})
        install-positive (gate :gesso.release.local/loopback-positive
                               :gesso.release.local/installation :positive :physical
                               http-mechanism "selected-start-v1" http-scope nil
                               #{local-source-premise local-listener-premise})
        install-negative (fn [id]
                           (gate id :gesso.release.local/installation
                                 :negative :physical
                                 http-mechanism "selected-start-v1" http-scope
                                 negative-identity-boundary #{}))
        pending (fn [phase]
                  [(gate (keyword "gesso.release.local" (str (name phase) "-positive"))
                         (keyword "gesso.release.local" (name phase))
                         :positive :qualification unconfigured "unconfigured-v1"
                         :gesso.release/physical-activation-not-configured nil #{})
                   (gate (keyword "gesso.release.local" (str (name phase) "-negative"))
                         (keyword "gesso.release.local" (name phase))
                         :negative :qualification unconfigured "unconfigured-v1"
                         :gesso.release/physical-activation-not-configured
                         :gesso.release.local/adversarial-activation-blocked #{})])]
    {:claim-id :gesso.release.local/source-to-installed-observation
     :application-id (:selected-catalog-id source-report)
     :milestone :gesso.release.local/current-http-instance-only
     :profile-id :gesso.release.local/source-and-http-only
     :capabilities (set (vals requirements))
     :requirements requirements
     :subjects {:candidate
                (str "candidate/" (:selected-catalog-fingerprint source-report))
                :installation installation-id
                :activation (str "activation-not-observed/" installation-id)
                :completion (str "completion-not-observed/" installation-id)}
     :gates {:candidate [source-positive source-negative]
             :installation
             [install-positive
              (install-negative :gesso.release.local/wrong-generation-negative)
              (install-negative :gesso.release.local/wrong-system-negative)]
             :activation (pending :activation)
             :completion (pending :completion)}}))

(defn- local-inventory [intent]
  {:capabilities (:capabilities intent)
   :requirements (set (keys (:requirements intent)))
   :gates (set (map :id (mapcat identity (vals (:gates intent)))))})

(defn- source-negative-qualified? [r]
  (let [phase (get-in r [:selected-release-diagnostic :decisions :candidate])
        rows (:rows phase)]
    (and (= :checks-passed-only (:status phase))
         (= :checks-passed-only (:candidate-status r))
         (= :passed (get-in rows [:gesso.release.source/omitted-operation :status]))
         (= :negative (get-in rows [:gesso.release.source/omitted-operation :kind]))
         (= :passed (get-in rows [:gesso.release.source/omitted-invariant :status]))
         (= :negative (get-in rows [:gesso.release.source/omitted-invariant :kind]))
         (= (+ (count (:operation-ids r))
               (count (:invariant-ids r)) 3)
            (count rows))
         (every? #(= :passed (:status %)) (vals rows)))))

(defn- release-local-evidence [intent phase gate subject premises]
  {:status :passed
   :claim-id (:claim-id intent)
   :phase phase
   :gate-id (:id gate)
   :requirement-id (:requirement gate)
   :subject-id (:subject-id subject)
   :mechanism (:mechanism gate)
   :mechanism-version (:mechanism-version gate)
   :scope (:scope gate)
   :evidence-id (str (:subject-id subject) "/" (:id gate))
   :premises premises
   :observed-boundary (:expected-boundary gate)
   :forbidden-effects (when (= :negative (:kind gate)) :absent)})

(defn- original-http-rejects-wrong-identity? [issued generation running kind]
  (let [[candidate-generation candidate-system]
        (case kind
          :generation [(Object.) running]
          :system [generation (assoc running ::wrong-system-identity true)]
          (reject! :unknown-physical-identity-control))]
    (try
      ((:inspect-generation! issued) candidate-generation candidate-system)
      false
      (catch clojure.lang.ExceptionInfo e
        (and (true? (:checked-biff-http-application-configuration/rejection
                     (ex-data e)))
             (= :configured-instance-generation-not-current
                (:reason (ex-data e))))))))

(defn issue-selected-installed-catalog!
  "Issue a ONE-USE selected original catalog + closed Biff HTTP startup.

   catalog! returns the application's original abstract operation catalog;
   http-config is exactly the original checked Biff configuration with a
   required :typed-trial and :listener-instance! host callback. That callback
   must return the actual concrete server instance for the current system.
   :start-once! accepts the original initial Biff map and returns only a
   redacted observation, never the system, handler, or opaque generation.
   :inspect-current! reissues the real HTTP challenge and rechecks the same
   source/route selection. :assess-current-installation! additionally runs
   the original four-phase diagnostic evaluator with fresh candidate and
   installation observations, actual original-inspector identity negatives,
   and NO observed activation/completion. :stop! releases the instance.
   An explicit opted-in :retry-cleanup! may resolve an unconfirmed stop
   only through the original configured issuer and source rechecks.
   None of these callbacks grants release eligibility or permission."
  [catalog! http-config]
  (pinned!)
  (when-not (and (fn? catalog!)
                 (plain-map? http-config)
                 (contains? http-config :typed-trial)
                 (plain-map? (:typed-trial http-config))
                 (plain-map? (get-in http-config [:typed-trial :catalog-source]))
                 (plain-map? (get-in http-config
                                     [:typed-trial :catalog-source :catalog]))
                 (fn? (:listener-instance! http-config)))
    (reject! :missing-selected-original-source-or-listener))
  (let [initial-catalog (catalog!)
        source-report (original-source-check catalog!)
        _ (checked-source! catalog! http-config initial-catalog source-report)
        issued (original-http-issuer http-config)
        monitor (Object.)
        status (atom :issued)
        retry-enabled? (true? (:idempotent-stop-retry? http-config))
        cleanup-retry-attempted? (atom false)
        running (atom nil)
        generation (atom nil)
        route-ids (atom nil)
        local-intent (local-intent source-report
                                   (str "local-http/" (java.util.UUID/randomUUID)))]
    (letfn [(complete-claimed-stop! []
              ;; Only the thread that owns the published :stopping claim
              ;; invokes original physical cleanup. Never retain monitor
              ;; during any configured stop callback, even failed startup.
              (let [result (try ((:stop! issued))
                                (catch Throwable error
                                  (when (instance? InterruptedException error)
                                    (.interrupt (Thread/currentThread)))
                                  nil))
                    terminal (if (= :stopped (:status result))
                               :stopped :cleanup-unconfirmed)]
                (locking monitor
                  (reset! running nil)
                  (reset! generation nil)
                  (reset! route-ids nil)
                  (reset! status terminal))
                {:status terminal :authority :none}))
            (stop! []
              ;; An external stop cannot interrupt an original Biff startup
              ;; in progress. Its initiating thread retains the responsibility
              ;; to clean up any failed startup, including partial listeners.
              (let [claimed?
                    (locking monitor
                      (when (= :starting @status)
                        (reject! :local-original-start-in-progress))
                      (when (= :retrying-cleanup @status)
                        (reject! :local-cleanup-retry-in-progress))
                      (when (= :stopping @status)
                        (reject! :local-physical-stop-in-progress))
                      (when-not (contains? retained-statuses @status)
                        (reject! :invalid-local-instance-state))
                      (if (= :stopped @status)
                        false
                        (do (reset! status :stopping) true)))]
                (if claimed?
                  (complete-claimed-stop!)
                  {:status :stopped :authority :none})))
            (retry-cleanup! []
              ;; Validate source, original issuer and the one-use capability
              ;; under monitor. No original retry callback may run before
              ;; :retrying-cleanup becomes visible to all competitors.
              (locking monitor
                (when-not retry-enabled?
                  (reject! :local-cleanup-retry-not-enabled))
                (when @cleanup-retry-attempted?
                  (reject! :local-cleanup-retry-already-attempted))
                (when-not (= :cleanup-unconfirmed @status)
                  (reject! :local-cleanup-retry-not-eligible))
                ;; Never requalify a possibly stopped HTTP listener as live;
                ;; only recheck the original frozen catalog and issuer.
                (checked-source! catalog! http-config
                                 initial-catalog source-report)
                (when-not (fn? (:retry-cleanup! issued))
                  (reject! :original-configured-retry-unavailable))
                (let [before (try ((:summary! issued))
                                  (catch Throwable _ nil))]
                  (when-not (and (= http/report-type (:type before))
                                 (= http/report-version (:version before))
                                 (= :stop-cleanup-not-established (:status before))
                                 (= :none (:authority before))
                                 (= :not-issued (:certificate before)))
                    (reject! :original-configured-cleanup-not-retryable)))
                ;; Consume BEFORE any original callback, even when a later
                ;; callback fails. Both flags belong to the same claim.
                (reset! cleanup-retry-attempted? true)
                (reset! status :retrying-cleanup))
              ;; Never hold monitor during the original physical callback:
              ;; it may synchronously ask another thread for this wrapper's
              ;; lifecycle summary while the host is completing its stop.
              (let [confirmed?
                    (try
                      (let [result ((:retry-cleanup! issued))
                            after ((:summary! issued))]
                        (and (= :stopped (:status result) (:status after))
                             (= http/report-type (:type result) (:type after))
                             (= http/report-version (:version result) (:version after))
                             (= :none (:authority result) (:authority after))
                             (= :not-issued (:certificate result) (:certificate after))
                             (= :not-admitted (:admission result) (:admission after))
                             ;; Original issuer checked guarded/probe cleanup
                             ;; and original-listener physical shutdown. Also
                             ;; require selected catalog continuity afterward.
                             (do (checked-source! catalog! http-config
                                                  initial-catalog source-report)
                                 true)))
                      (catch Throwable error
                        (when (instance? InterruptedException error)
                          (.interrupt (Thread/currentThread)))
                        false))
                    outcome (if confirmed? :stopped :cleanup-unconfirmed)]
                (locking monitor (reset! status outcome))
                {:status outcome
                 :cleanup-retry :one-original-configured-attempt-consumed
                 :authority :none}))
            (inspect-locked!
              ([] (inspect-locked! false))
              ([startup-owner?]
               ;; The startup owner may inspect a still-private original
               ;; instance while :starting. Public inspection and assessment
               ;; cannot observe that provisional instance or requalify it.
               (when-not (or (= :running @status)
                             (and startup-owner? (= :starting @status)))
                 (reject! :no-current-issued-running-instance))
              (let [running-system @running
                    before (checked-source! catalog! http-config
                                            initial-catalog source-report)
                    result ((:inspect-generation! issued)
                            @generation running-system)
                    observed-routes (check-running! before result @route-ids)
                    after (checked-source! catalog! http-config
                                           initial-catalog source-report)]
                (when-not (and (= before after)
                               (or (= :running @status)
                                   (and startup-owner? (= :starting @status)))
                               (identical? @running running-system)
                               (or (nil? @route-ids)
                                   (= @route-ids observed-routes)))
                  (reject! :source-or-running-instance-drifted-during-inspection))
                (reset! route-ids observed-routes)
                (observation after result observed-routes))))
            (inspect-current! []
              (locking monitor
                (try
                  (inspect-locked!)
                  (catch Throwable error
                    ;; A failed recheck must not leave this wrapper capable
                    ;; of returning a historical green local observation.
                    (when (= :running @status) (stop!))
                    (throw error)))))
            (start-once! [initial-system]
              ;; Publish the one-use :starting claim under monitor. The
              ;; original Biff adapter and fresh HTTP inspections can call
              ;; into this issuer's diagnostics on another thread, so no
              ;; lifecycle monitor may be held over those operations.
              (locking monitor
                (when-not (compare-and-set! status :issued :starting)
                  (reject! :selected-original-http-start-not-once-only)))
              (try
                (checked-source! catalog! http-config initial-catalog
                                 source-report)
                (let [{:keys [system instance-generation report]}
                      ((:start-once! issued) initial-system)]
                  (when-not (and (= http/report-type (:type report))
                                 (= :verified-configured-start (:status report))
                                 (= :none (:authority report))
                                 (= :not-issued (:certificate report)))
                    (reject! :original-checked-http-start-not-established))
                  ;; Keep this original listener PRIVATE until its final
                  ;; source/HTTP inspection passes. All public stop/inspect
                  ;; calls reject during :starting.
                  (locking monitor
                    (reset! running system)
                    (reset! generation instance-generation))
                  (let [observed (inspect-locked! true)]
                    (locking monitor (reset! status :running))
                    observed))
                (catch Throwable error
                  ;; The startup owner, not an external stop racing the
                  ;; original adapter, exclusively owns failed-start cleanup.
                  (locking monitor (reset! status :stopping))
                  (try (complete-claimed-stop!)
                       (catch Throwable _
                         (locking monitor
                           (reset! running nil)
                           (reset! generation nil)
                           (reset! route-ids nil)
                           (reset! status :cleanup-unconfirmed))))
                  (throw error))))
            (assess-current-installation! []
              (locking monitor
                (try
                  (when-not (= :running @status)
                    (reject! :no-current-issued-running-instance))
                  ;; These observations and the original managed stop are
                  ;; serialized by this monitor, but the result is NOT a
                  ;; future liveness lease or permission to activate.
                  (let [fresh-source! #(checked-source! catalog! http-config
                                                         initial-catalog source-report)
                        source-ok! (fn []
                                     (let [r (fresh-source!)]
                                       (when-not (source-negative-qualified? r)
                                         (reject! :original-candidate-source-not-qualified))
                                       r))
                        discover! (fn []
                                    (source-ok!)
                                    (inspect-locked!)
                                    (local-inventory local-intent))
                        subject! (fn [phase]
                                   (case phase
                                     :candidate (do (source-ok!)
                                                    {:phase phase
                                                     :claim-id (:claim-id local-intent)
                                                     :subject-id (get-in local-intent
                                                                         [:subjects phase])})
                                     :installation (do (inspect-locked!)
                                                       {:phase phase
                                                        :claim-id (:claim-id local-intent)
                                                        :subject-id (get-in local-intent
                                                                            [:subjects phase])})
                                     ;; These phases have no observed physical
                                     ;; subject and MUST block rather than pass.
                                     nil))
                        run-gate! (fn [{:keys [phase gate subject]}]
                                    (let [id (:id gate)]
                                      (cond
                                        (= :candidate phase)
                                        (let [r (source-ok!)]
                                          (if (and (source-negative-qualified? r)
                                                   (contains?
                                                    #{:gesso.release.local/source-positive
                                                      :gesso.release.local/source-negative} id))
                                            (release-local-evidence
                                             local-intent phase gate subject
                                             (if (= id :gesso.release.local/source-positive)
                                               #{local-source-premise} #{}))
                                            {:status :failed}))

                                        (= :installation phase)
                                        (let [before (inspect-locked!)]
                                          (cond
                                            (= id :gesso.release.local/loopback-positive)
                                            (release-local-evidence
                                             local-intent phase gate subject
                                             #{local-source-premise local-listener-premise})

                                            (contains?
                                             #{:gesso.release.local/wrong-generation-negative
                                               :gesso.release.local/wrong-system-negative}
                                             id)
                                            (let [kind (if (= id :gesso.release.local/wrong-generation-negative)
                                                         :generation :system)
                                                  rejected? (original-http-rejects-wrong-identity?
                                                             issued @generation @running kind)
                                                  after (inspect-locked!)]
                                              (if (and rejected?
                                                       (= (:selected-http-route-ids before)
                                                          (:selected-http-route-ids after))
                                                       (= (:selected-catalog-fingerprint before)
                                                          (:selected-catalog-fingerprint after)))
                                                (release-local-evidence
                                                 local-intent phase gate subject #{})
                                                {:status :failed}))
                                            :else {:status :unsupported}))
                                        :else {:status :unsupported})))
                        evaluated (decision/assess!
                                   {:intent local-intent
                                    :discover! discover!
                                    :subject! subject!
                                    :run-gate! run-gate!})
                        phases (:decisions evaluated)
                        qualified? (and (= :matched (get-in evaluated
                                                           [:initial-inventory :status]))
                                        (= :matched (get-in evaluated
                                                           [:final-inventory :status]))
                                        (= :checks-passed-only (get-in phases
                                                                       [:candidate :status]))
                                        (= :checks-passed-only (get-in phases
                                                                       [:installation :status]))
                                        (= :blocked (get-in phases
                                                            [:activation :status]))
                                        (= :not-examined (get-in phases
                                                                 [:completion :status]))
                                        (= :blocked (:status evaluated))
                                        (= :none (:release-authority evaluated)
                                           (:runtime-effect-permission evaluated))
                                        (= :not-issued (:installation-eligibility evaluated)
                                           (:candidate-admission evaluated)
                                           (:activation-eligibility evaluated)
                                           (:completion-claim evaluated)))]
                    (when-not qualified?
                      (reject! :selected-local-installation-phase-not-qualified))
                    ;; A final fresh local HTTP challenge is necessary after
                    ;; the decision evaluator's callbacks and identity tests.
                    (let [installed (inspect-locked!)]
                      {:type local-assessment-type
                       :version local-assessment-version
                       :status :selected-local-source-and-installation-checks-only
                       :selected-source (:selected-catalog-fingerprint source-report)
                       :observed-http-routes (:selected-http-route-ids installed)
                       :current-local-instance installed
                       :release-diagnostic evaluated
                       :accepted-premises (:accepted-premises evaluated)
                       :conditional? true
                       :original-application-source-complete :not-established
                       :original-release-installation-eligibility :not-issued
                       :release-authority :none
                       :candidate-admission :not-issued
                       :installation-eligibility :not-issued
                       :activation-eligibility :not-issued
                       :completion-claim :not-issued
                       :runtime-effect-permission :none}))
                  (catch Throwable error
                    ;; An unavailable or inconsistent physical/source check
                    ;; cannot preserve a prior apparent success in this
                    ;; one-use local wrapper.
                    (when (= :running @status) (stop!))
                    (throw error)))))
            (summary! []
              (locking monitor
                {:type report-type :version report-version
                 :status @status
                 :current-installation-evidence :requires-fresh-inspection
                 :cleanup-retry-policy (if retry-enabled?
                                         :explicit-host-idempotence
                                         :one-shot-only)
                 :cleanup-retry-attempted? @cleanup-retry-attempted?
                 :selected-source-fingerprint
                 (:selected-catalog-fingerprint source-report)
                 :installation-eligibility :not-issued
                 :release-authority :none
                 :runtime-effect-permission :none}))]
      {:start-once! start-once!
       :inspect-current! inspect-current!
       :assess-current-installation! assess-current-installation!
       :stop! stop!
       :retry-cleanup! retry-cleanup!
       :summary! summary!})))

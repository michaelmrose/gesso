(ns gesso.live.optimistic.choreo
  "Built-in verified optimistic-command choreography for Gesso Live.

   This namespace is the single semantic definition of the normal optimistic
   lifecycle. It is intentionally free of DOM, HTMX, Ring, XTDB, SSE, and
   application-domain implementation details.

   The choreography is verified and projected on the JVM. ClojureScript should
   consume the browser plan through browser-plan-form, which expands to literal
   projected data so verifier/projector code does not become part of the
   production browser bundle."
  (:require
   [gesso.choreo.core :as choreo]
   [gesso.live.optimistic.protocol :as protocol]
   #?(:clj [gesso.choreo.project :as project])
   #?(:clj [gesso.choreo.verify :as verify])))

;; -----------------------------------------------------------------------------
;; Semantic identities
;; -----------------------------------------------------------------------------

(def protocol-name
  :gesso.live.optimistic/command)

(def browser-role
  :browser)

(def server-role
  :server)

(def command-event
  protocol/command-event)

(def settlement-event
  protocol/settlement-event)

(def request-failed-event
  :optimistic/request-failed)

(def timeout-event
  :optimistic/timeout)

(def canonical-superseded-event
  "Browser-runtime event meaning that authoritative canonical state which
   outranks the outstanding optimistic execution has already been installed.

   This is intentionally stronger than a raw Live invalidation or arbitrary DOM
   replacement. The browser runtime is responsible for revision/authority
   comparison before emitting this event."
  :optimistic/canonical-superseded)

(def continuity-restored-event
  "Browser-runtime event emitted only after continuity restoration has completed
   at its required post-layout boundary."
  :continuity/restored)

(def target-authority-resource
  :optimistic/target-authority)

(def snapshot-authority-resource
  :optimistic/snapshot-authority)

(def settlement-outcomes
  protocol/settlement-outcomes)

(def recovery-dispositions
  #{:recovered :canonical-wins})

(def canonical-dispositions
  #{:installed :canonical-wins})

;; -----------------------------------------------------------------------------
;; FX machine identities
;; -----------------------------------------------------------------------------

(def browser-acquire-target-machine
  :optimistic/acquire-target)

(def browser-capture-continuity-machine
  :continuity/capture)

(def browser-capture-snapshot-machine
  :optimistic/capture-snapshot)

(def browser-install-projection-machine
  :optimistic/install-projection)

(def browser-schedule-timeout-machine
  :optimistic/schedule-timeout)

(def server-execute-machine
  :optimistic/execute-command)

(def browser-install-canonical-machine
  :optimistic/install-canonical)

(def browser-discard-snapshot-machine
  :optimistic/discard-snapshot)

(def browser-recover-snapshot-machine
  :optimistic/recover)

(def browser-restore-continuity-machine
  :continuity/restore)

(def browser-cancel-timeout-machine
  :optimistic/cancel-timeout)

(def browser-clear-pending-machine
  :optimistic/clear-pending)

(def browser-release-target-machine
  :optimistic/release-target)

;; -----------------------------------------------------------------------------
;; Context / payload keys
;; -----------------------------------------------------------------------------

(def execution-id-key
  protocol/execution-id-key)

(def transition-key
  protocol/transition-key)

(def scope-key
  protocol/scope-key)

(def base-revision-key
  protocol/base-revision-key)

(def consistency-token-key
  protocol/consistency-token-key)

(def outcome-key
  protocol/outcome-key)

(def command-applied-key
  protocol/command-applied-key)

(def revision-key
  protocol/revision-key)

(def canonical-key
  protocol/canonical-key)

(def reason-key
  protocol/reason-key)

(def recovery-disposition-key
  :recovery-disposition)

(def canonical-disposition-key
  :canonical-disposition)

(def command-required-keys
  protocol/command-required-keys)

(def command-optional-keys
  protocol/command-optional-keys)

(def command-correlation-keys
  protocol/command-correlation-keys)

(def settlement-required-keys
  protocol/settlement-required-keys)

(def settlement-optional-keys
  protocol/settlement-optional-keys)

(def settlement-correlation-keys
  protocol/settlement-correlation-keys)

;; -----------------------------------------------------------------------------
;; Choreography definition
;; -----------------------------------------------------------------------------

(def optimistic-command
  "Global optimistic-command choreography.

   Important authority rules encoded by the graph:

   - The browser holds target authority for the entire provisional execution.
   - Snapshot authority is separately linear and must be consumed on every
     terminal path.
   - Semantic settlement always carries authoritative canonical content.
   - Request failure and timeout recover through the snapshot only when the DOM
     FX machine confirms that canonical authority has not already superseded it.
   - A canonical-superseded event means the authoritative replacement is already
     installed; the old execution only cleans up and terminates.
   - Settlement canonical installation may itself discover that newer canonical
     state already wins. In that case the execution cleans up without restoring
     stale pre-projection continuity over the newer replacement."
  (choreo/->choreography
   {:name protocol-name
    :roles #{browser-role server-role}
    :initial :browser/acquire-target-authority
    :environment-events
    #{request-failed-event
      timeout-event
      canonical-superseded-event
      continuity-restored-event}
    :resources
    {target-authority-resource
     (choreo/resource
      {:owner browser-role
       :linear? true
       :terminal-release? true})

     snapshot-authority-resource
     (choreo/resource
      {:owner browser-role
       :linear? true
       :terminal-release? true})}
    :states
    {;; Browser prepares one provisional execution.
     :browser/acquire-target-authority
     (choreo/acquire
      browser-role
      target-authority-resource
      :browser/acquire-target)

     :browser/acquire-target
     (choreo/fx
      browser-role
      browser-acquire-target-machine
      :browser/capture-continuity)

     :browser/capture-continuity
     (choreo/fx
      browser-role
      browser-capture-continuity-machine
      :browser/acquire-snapshot-authority)

     :browser/acquire-snapshot-authority
     (choreo/acquire
      browser-role
      snapshot-authority-resource
      :browser/capture-snapshot)

     :browser/capture-snapshot
     (choreo/fx
      browser-role
      browser-capture-snapshot-machine
      :browser/install-projection)

     :browser/install-projection
     (choreo/fx
      browser-role
      browser-install-projection-machine
      :browser/schedule-timeout)

     :browser/schedule-timeout
     (choreo/fx
      browser-role
      browser-schedule-timeout-machine
      :browser/send-command)

     ;; The command crosses to the server. Interrupts are terminal alternatives
     ;; to the outstanding command/settlement interaction.
     :browser/send-command
     (choreo/send
      browser-role
      server-role
      command-event
      :server/receive-command
      {:via :http
       :required command-required-keys
       :optional command-optional-keys
       :correlation command-correlation-keys
       :interrupts
       {request-failed-event :browser/recover-request-failed
        timeout-event :browser/recover-timeout
        canonical-superseded-event :browser/discard-superseded-snapshot}})

     :server/receive-command
     (choreo/receive
      browser-role
      server-role
      command-event
      :server/execute-command
      {:via :http
       :bind :command})

     :server/execute-command
     (choreo/fx
      server-role
      server-execute-machine
      :server/validate-outcome)

     ;; This choice validates that application execution produced one supported
     ;; semantic settlement outcome. Every outcome then uses the same settlement
     ;; message contract; the outcome value itself is carried in the payload.
     :server/validate-outcome
     (choreo/choice
      server-role
      outcome-key
      {:confirmed :server/send-settlement
       :reconciled :server/send-settlement
       :rejected :server/send-settlement
       :failed :server/send-settlement})

     :server/send-settlement
     (choreo/send
      server-role
      browser-role
      settlement-event
      :browser/receive-settlement
      {:via :http
       :required settlement-required-keys
       :optional settlement-optional-keys
       :correlation settlement-correlation-keys})

     :browser/receive-settlement
     (choreo/receive
      server-role
      browser-role
      settlement-event
      :browser/install-canonical
      {:via :http
       :bind :settlement})

     ;; install-canonical is the trusted DOM authority check. It either installs
     ;; this settlement's canonical rendering or reports that already-installed
     ;; canonical state wins.
     :browser/install-canonical
     (choreo/fx
      browser-role
      browser-install-canonical-machine
      :browser/canonical-disposition)

     :browser/canonical-disposition
     (choreo/choice
      browser-role
      canonical-disposition-key
      {:installed :browser/discard-settled-snapshot
       :canonical-wins :browser/discard-superseded-snapshot})

     ;; Successful authoritative installation: the old structural snapshot is no
     ;; longer valid, then the continuity captured for this optimistic execution
     ;; is restored across the replacement.
     :browser/discard-settled-snapshot
     (choreo/fx
      browser-role
      browser-discard-snapshot-machine
      :browser/release-settled-snapshot-authority)

     :browser/release-settled-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-settled-continuity)

     :browser/restore-settled-continuity
     (choreo/fx
      browser-role
      browser-restore-continuity-machine
      :browser/await-settled-continuity)

     :browser/await-settled-continuity
     (choreo/await
      browser-role
      {continuity-restored-event :browser/cancel-settled-timeout})

     :browser/cancel-settled-timeout
     (choreo/fx
      browser-role
      browser-cancel-timeout-machine
      :browser/clear-settled-pending)

     :browser/clear-settled-pending
     (choreo/fx
      browser-role
      browser-clear-pending-machine
      :browser/release-settled-target)

     :browser/release-settled-target
     (choreo/fx
      browser-role
      browser-release-target-machine
      :browser/release-settled-target-authority)

     :browser/release-settled-target-authority
     (choreo/release
      browser-role
      target-authority-resource
      :browser/settled-outcome)

     :browser/settled-outcome
     (choreo/choice
      browser-role
      outcome-key
      {:confirmed :browser/return-confirmed
       :reconciled :browser/return-reconciled
       :rejected :browser/return-rejected
       :failed :browser/return-failed})

     :browser/return-confirmed
     (choreo/return browser-role :confirmed)

     :browser/return-reconciled
     (choreo/return browser-role :reconciled)

     :browser/return-rejected
     (choreo/return browser-role :rejected)

     :browser/return-failed
     (choreo/return browser-role :failed)

     ;; Request failure and timeout share the same safe recovery primitive. The
     ;; primitive may discover a canonical replacement that won the race; that
     ;; distinction decides whether the original optimistic continuity should be
     ;; restored.
     :browser/recover-request-failed
     (choreo/fx
      browser-role
      browser-recover-snapshot-machine
      :browser/request-failed-recovery-disposition)

     :browser/request-failed-recovery-disposition
     (choreo/choice
      browser-role
      recovery-disposition-key
      {:recovered :browser/discard-request-failed-snapshot
       :canonical-wins :browser/discard-superseded-snapshot})

     :browser/discard-request-failed-snapshot
     (choreo/fx
      browser-role
      browser-discard-snapshot-machine
      :browser/release-request-failed-snapshot-authority)

     :browser/release-request-failed-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-request-failed-continuity)

     :browser/restore-request-failed-continuity
     (choreo/fx
      browser-role
      browser-restore-continuity-machine
      :browser/await-request-failed-continuity)

     :browser/await-request-failed-continuity
     (choreo/await
      browser-role
      {continuity-restored-event :browser/cancel-request-failed-timeout})

     :browser/cancel-request-failed-timeout
     (choreo/fx
      browser-role
      browser-cancel-timeout-machine
      :browser/clear-request-failed-pending)

     :browser/clear-request-failed-pending
     (choreo/fx
      browser-role
      browser-clear-pending-machine
      :browser/release-request-failed-target)

     :browser/release-request-failed-target
     (choreo/fx
      browser-role
      browser-release-target-machine
      :browser/release-request-failed-target-authority)

     :browser/release-request-failed-target-authority
     (choreo/release
      browser-role
      target-authority-resource
      :browser/return-request-failed)

     :browser/return-request-failed
     (choreo/return browser-role :request-failed)

     :browser/recover-timeout
     (choreo/fx
      browser-role
      browser-recover-snapshot-machine
      :browser/timeout-recovery-disposition)

     :browser/timeout-recovery-disposition
     (choreo/choice
      browser-role
      recovery-disposition-key
      {:recovered :browser/discard-timeout-snapshot
       :canonical-wins :browser/discard-superseded-snapshot})

     :browser/discard-timeout-snapshot
     (choreo/fx
      browser-role
      browser-discard-snapshot-machine
      :browser/release-timeout-snapshot-authority)

     :browser/release-timeout-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-timeout-continuity)

     :browser/restore-timeout-continuity
     (choreo/fx
      browser-role
      browser-restore-continuity-machine
      :browser/await-timeout-continuity)

     :browser/await-timeout-continuity
     (choreo/await
      browser-role
      {continuity-restored-event :browser/cancel-timeout-after-timeout})

     :browser/cancel-timeout-after-timeout
     (choreo/fx
      browser-role
      browser-cancel-timeout-machine
      :browser/clear-timeout-pending)

     :browser/clear-timeout-pending
     (choreo/fx
      browser-role
      browser-clear-pending-machine
      :browser/release-timeout-target)

     :browser/release-timeout-target
     (choreo/fx
      browser-role
      browser-release-target-machine
      :browser/release-timeout-target-authority)

     :browser/release-timeout-target-authority
     (choreo/release
      browser-role
      target-authority-resource
      :browser/return-timeout)

     :browser/return-timeout
     (choreo/return browser-role :timeout)

     ;; Canonical state already won. Do not restore the stale continuity capture
     ;; from before the optimistic projection; the canonical swap used the shared
     ;; continuity engine at the time it actually happened.
     :browser/discard-superseded-snapshot
     (choreo/fx
      browser-role
      browser-discard-snapshot-machine
      :browser/release-superseded-snapshot-authority)

     :browser/release-superseded-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/cancel-superseded-timeout)

     :browser/cancel-superseded-timeout
     (choreo/fx
      browser-role
      browser-cancel-timeout-machine
      :browser/clear-superseded-pending)

     :browser/clear-superseded-pending
     (choreo/fx
      browser-role
      browser-clear-pending-machine
      :browser/release-superseded-target)

     :browser/release-superseded-target
     (choreo/fx
      browser-role
      browser-release-target-machine
      :browser/release-superseded-target-authority)

     :browser/release-superseded-target-authority
     (choreo/release
      browser-role
      target-authority-resource
      :browser/return-superseded)

     :browser/return-superseded
     (choreo/return browser-role :superseded)}}))

;; -----------------------------------------------------------------------------
;; Compiler products
;; -----------------------------------------------------------------------------

#?(:clj
   (def verified-optimistic-command
     "Verified compiler representation of optimistic-command. Namespace loading
      fails immediately if the built-in protocol violates choreography rules."
     (verify/verify! optimistic-command)))

#?(:clj
   (def browser-plan
     "Projected browser plan used by JVM tests and compiler tooling. Production
      CLJS should obtain the same data through browser-plan-form."
     (project/project
      verified-optimistic-command
      browser-role)))

#?(:clj
   (def server-plan
     "Projected server plan consumed by the JVM optimistic adapter."
     (project/project
      verified-optimistic-command
      server-role)))

#?(:clj
   (defmacro browser-plan-form
     "Expand to the verified browser plan as literal data.

      Requiring this macro from CLJS keeps verify/project compiler machinery on
      the JVM side of compilation instead of making it reachable from the
      browser runtime."
     []
     browser-plan))

#?(:clj
   (defmacro server-plan-form
     "Expand to the verified server plan as literal data. Primarily useful for
      compile-time assertions or generated adapters."
     []
     server-plan))

#?(:clj
   (defn explain
     "Return compact compiler-facing information about the built-in optimistic
      choreography and both endpoint projections."
     []
     {:choreography (choreo/explain optimistic-command)
      :verification (verify/explain verified-optimistic-command)
      :browser (project/explain browser-plan)
      :server (project/explain server-plan)}))

(ns gesso.live.optimistic-choreo
  "Built-in verified optimistic-command choreography for Gesso Live.

   This namespace is the single semantic definition of the normal optimistic
   lifecycle. It is intentionally free of DOM, HTMX, Ring, XTDB, SSE, and
   application-domain implementation details.

   The choreography is verified and projected on the JVM. ClojureScript should
   consume the browser plan through browser-plan-form, which expands to literal
   projected data so verifier/projector code does not become part of the
   production browser bundle."
  (:require
   [gesso.live.choreo :as choreo]
   #?(:clj [gesso.live.choreo.project :as project])
   #?(:clj [gesso.live.choreo.verify :as verify])))

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
  :optimistic/command)

(def settlement-event
  :optimistic/settlement)

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

(def target-authority-resource
  :optimistic/target-authority)

(def snapshot-authority-resource
  :optimistic/snapshot-authority)

(def settlement-outcomes
  #{:confirmed :reconciled :rejected :failed})

(def recovery-dispositions
  #{:recovered :canonical-wins})

(def canonical-dispositions
  #{:installed :canonical-wins})

;; -----------------------------------------------------------------------------
;; Effect identities
;; -----------------------------------------------------------------------------

(def acquire-target-effect
  :optimistic/acquire-target)

(def capture-continuity-effect
  :continuity/capture)

(def capture-snapshot-effect
  :optimistic/capture-snapshot)

(def install-projection-effect
  :optimistic/install-projection)

(def schedule-timeout-effect
  :optimistic/schedule-timeout)

(def execute-command-effect
  :optimistic/execute-command)

(def install-canonical-effect
  :optimistic/install-canonical)

(def discard-snapshot-effect
  :optimistic/discard-snapshot)

(def recover-snapshot-effect
  :optimistic/recover)

(def restore-continuity-effect
  :continuity/restore)

(def cancel-timeout-effect
  :optimistic/cancel-timeout)

(def clear-pending-effect
  :optimistic/clear-pending)

(def release-target-effect
  :optimistic/release-target)

;; -----------------------------------------------------------------------------
;; Context / payload keys
;; -----------------------------------------------------------------------------

(def execution-id-key
  :execution-id)

(def transition-key
  :transition)

(def scope-key
  :scope)

(def base-revision-key
  :base-revision)

(def consistency-token-key
  :consistency-token)

(def outcome-key
  :outcome)

(def command-applied-key
  :command-applied?)

(def revision-key
  :revision)

(def canonical-key
  :canonical)

(def reason-key
  :reason)

(def recovery-disposition-key
  :recovery-disposition)

(def canonical-disposition-key
  :canonical-disposition)

(def command-required-keys
  #{execution-id-key
    transition-key
    scope-key})

(def command-optional-keys
  #{base-revision-key
    consistency-token-key})

(def command-correlation-keys
  #{execution-id-key
    scope-key})

(def settlement-required-keys
  #{execution-id-key
    scope-key
    outcome-key
    command-applied-key
    canonical-key})

(def settlement-optional-keys
  #{revision-key
    reason-key
    consistency-token-key})

(def settlement-correlation-keys
  #{execution-id-key
    scope-key})

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
     effect confirms that canonical authority has not already superseded it.
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
      canonical-superseded-event}
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
     (choreo/effect
      browser-role
      acquire-target-effect
      :browser/capture-continuity)

     :browser/capture-continuity
     (choreo/effect
      browser-role
      capture-continuity-effect
      :browser/acquire-snapshot-authority)

     :browser/acquire-snapshot-authority
     (choreo/acquire
      browser-role
      snapshot-authority-resource
      :browser/capture-snapshot)

     :browser/capture-snapshot
     (choreo/effect
      browser-role
      capture-snapshot-effect
      :browser/install-projection)

     :browser/install-projection
     (choreo/effect
      browser-role
      install-projection-effect
      :browser/schedule-timeout)

     :browser/schedule-timeout
     (choreo/effect
      browser-role
      schedule-timeout-effect
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
     (choreo/effect
      server-role
      execute-command-effect
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
     (choreo/effect
      browser-role
      install-canonical-effect
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
     (choreo/effect
      browser-role
      discard-snapshot-effect
      :browser/release-settled-snapshot-authority)

     :browser/release-settled-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-settled-continuity)

     :browser/restore-settled-continuity
     (choreo/effect
      browser-role
      restore-continuity-effect
      :browser/cancel-settled-timeout)

     :browser/cancel-settled-timeout
     (choreo/effect
      browser-role
      cancel-timeout-effect
      :browser/clear-settled-pending)

     :browser/clear-settled-pending
     (choreo/effect
      browser-role
      clear-pending-effect
      :browser/release-settled-target)

     :browser/release-settled-target
     (choreo/effect
      browser-role
      release-target-effect
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
     (choreo/effect
      browser-role
      recover-snapshot-effect
      :browser/request-failed-recovery-disposition)

     :browser/request-failed-recovery-disposition
     (choreo/choice
      browser-role
      recovery-disposition-key
      {:recovered :browser/discard-request-failed-snapshot
       :canonical-wins :browser/discard-superseded-snapshot})

     :browser/discard-request-failed-snapshot
     (choreo/effect
      browser-role
      discard-snapshot-effect
      :browser/release-request-failed-snapshot-authority)

     :browser/release-request-failed-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-request-failed-continuity)

     :browser/restore-request-failed-continuity
     (choreo/effect
      browser-role
      restore-continuity-effect
      :browser/cancel-request-failed-timeout)

     :browser/cancel-request-failed-timeout
     (choreo/effect
      browser-role
      cancel-timeout-effect
      :browser/clear-request-failed-pending)

     :browser/clear-request-failed-pending
     (choreo/effect
      browser-role
      clear-pending-effect
      :browser/release-request-failed-target)

     :browser/release-request-failed-target
     (choreo/effect
      browser-role
      release-target-effect
      :browser/release-request-failed-target-authority)

     :browser/release-request-failed-target-authority
     (choreo/release
      browser-role
      target-authority-resource
      :browser/return-request-failed)

     :browser/return-request-failed
     (choreo/return browser-role :request-failed)

     :browser/recover-timeout
     (choreo/effect
      browser-role
      recover-snapshot-effect
      :browser/timeout-recovery-disposition)

     :browser/timeout-recovery-disposition
     (choreo/choice
      browser-role
      recovery-disposition-key
      {:recovered :browser/discard-timeout-snapshot
       :canonical-wins :browser/discard-superseded-snapshot})

     :browser/discard-timeout-snapshot
     (choreo/effect
      browser-role
      discard-snapshot-effect
      :browser/release-timeout-snapshot-authority)

     :browser/release-timeout-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/restore-timeout-continuity)

     :browser/restore-timeout-continuity
     (choreo/effect
      browser-role
      restore-continuity-effect
      :browser/cancel-timeout-after-timeout)

     :browser/cancel-timeout-after-timeout
     (choreo/effect
      browser-role
      cancel-timeout-effect
      :browser/clear-timeout-pending)

     :browser/clear-timeout-pending
     (choreo/effect
      browser-role
      clear-pending-effect
      :browser/release-timeout-target)

     :browser/release-timeout-target
     (choreo/effect
      browser-role
      release-target-effect
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
     (choreo/effect
      browser-role
      discard-snapshot-effect
      :browser/release-superseded-snapshot-authority)

     :browser/release-superseded-snapshot-authority
     (choreo/release
      browser-role
      snapshot-authority-resource
      :browser/cancel-superseded-timeout)

     :browser/cancel-superseded-timeout
     (choreo/effect
      browser-role
      cancel-timeout-effect
      :browser/clear-superseded-pending)

     :browser/clear-superseded-pending
     (choreo/effect
      browser-role
      clear-pending-effect
      :browser/release-superseded-target)

     :browser/release-superseded-target
     (choreo/effect
      browser-role
      release-target-effect
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

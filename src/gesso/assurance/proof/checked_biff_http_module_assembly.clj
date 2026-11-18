(ns gesso.assurance.proof.checked-biff-http-module-assembly
  "v1783: A deliberately CLOSED, opt-in Biff module inventory for ONE
   locally issued checked HTTP adapter. Before ANY Biff initialization,
   require exact identities and complete ordered coverage for the canonical
   Gesso application module, issued checked host module, issued HTTP
   consumption probe, and issued fail-closed guarded HTTP adapter.

   No raw adapter or other module may appear in this particular modules Var.
   Unlike ordinary Biff startup, duplicate IDs, extra lifecycle callbacks,
   omitted modules, fabricated issuer maps and reordered execution fail
   before init/start callbacks have a chance to open a listener.

   This does NOT inspect independently started Biff instances, other module
   Vars or code that opens sockets outside Biff. A sealed local start is not
   independent deployment attestation, complete physical-writer coverage,
   application policy authentication, or admission authority."
  (:require [com.biffweb.core :as biff]
            [gesso.assurance.proof.checked-biff-host-adoption :as host]
            [gesso.assurance.proof.checked-biff-http-consumption-probe :as probe]
            [gesso.assurance.proof.checked-biff-http-adapter-start :as guarded]
            [gesso.live.application-preflight :as application]))

(def report-type
  :gesso.assurance.proof.checked-biff-http-module-assembly/sealed-local-start)
(def report-version 1)

(def ^:private original-host-check
  host/require-original-issued-biff-host-module!)
(def ^:private original-probe-check
  probe/require-current-issued-http-consumption-probe!)
(def ^:private original-guarded-check
  guarded/require-original-issued-http-adapter-start!)
(def ^:private original-start application/start-biff-application!)

(defn- reject! [reason]
  (throw (ex-info "Closed local checked HTTP adapter module startup is not established."
                  {:checked-biff-http-module-assembly/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-host-check
                             (var-get #'host/require-original-issued-biff-host-module!))
                 (identical? original-probe-check
                             (var-get #'probe/require-current-issued-http-consumption-probe!))
                 (identical? original-guarded-check
                             (var-get #'guarded/require-original-issued-http-adapter-start!))
                 (identical? original-start
                             (var-get #'application/start-biff-application!)))
    (reject! :substituted-original-start-or-issued-module-boundary)))

(defn- qualified! [f candidate reason expected-key]
  (let [evidence (try (f candidate)
                      (catch Exception _ (reject! reason)))]
    (when-not (and (= :exact-identity (get evidence expected-key))
                   (= :none (:authority evidence)))
      (reject! reason))
    true))

(defn- inspect!
  [issued-host issued-probe issued-guarded modules-var start-order]
  (pinned!)
  (qualified! original-host-check issued-host
              :original-host-module-not-established :original-host-module)
  (let [evidence (try (original-probe-check issued-probe)
                      (catch Exception _ (reject! :original-probe-module-not-established)))]
    (when-not (and (= :issued-original-module-and-callbacks
                      (:original-probe evidence))
                   (= :none (:authority evidence)))
      (reject! :original-probe-module-not-established)))
  (qualified! original-guarded-check issued-guarded
              :original-guarded-module-not-established :original-guarded-module)
  (when-not (and (var? modules-var) (vector? @modules-var)
                 (vector? start-order))
    (reject! :unsealed-or-nonvector-module-inventory))
  (let [expected [application/application-handler-module
                  (:module issued-host)
                  (:module issued-probe)
                  (:module issued-guarded)]
        required-ids [application/application-handler-module-id
                      host/module-id probe/module-id guarded/module-id]
        actual @modules-var
        actual-ids (mapv :biff.core/id actual)]
    (when-not (and (= 4 (count actual))
                   (= 4 (count (set actual-ids)))
                   (= required-ids actual-ids start-order)
                   (= 4 (count (set start-order))))
      (reject! :missing-extra-duplicate-or-reordered-http-modules))
    ;; Map equality is too weak: an attacker can copy a module map and change
    ;; its callbacks while retaining its module ID and apparently equal data.
    (when-not (every? true? (map identical? expected actual))
      (reject! :issued-module-object-identity-mismatch))
    (pinned!)
    {:type report-type :version report-version
     :scope :one-closed-in-process-four-module-biff-start
     :module-inventory :exact-original-four-modules-by-identity
     :start-order :complete-unique-and-canonical
     :additional-modules :rejected-in-selected-var
     :physical-server-start :not-established
     :independent-biff-instances :not-established
     :external-listeners :not-established
     :deployed-http-server :not-established
     :complete-physical-writer-coverage :not-established
     :certificate :not-issued :transaction-authority :none
     :authority :none}))

(defn start-closed-checked-http-application!
  "Perform the inventory check BEFORE calling Biff, using the EXACT supplied
   modules Var. The selected guarded adapter then performs its real HTTP
   consumption challenge before Biff startup completes. A post-start check
   requires the guard to be verified; if it is not, stop the returned Biff
   system and reject. If a module itself throws during startup, its own
   cleanup behavior remains an open obligation of Biff and that module.

   Returns {:system <actual started Biff system> :report <redacted>} so the
   caller can stop the real Biff system. This result is not a certificate."
  [application-assembly initial-system issued-host issued-probe
   issued-guarded modules-var start-order]
  (let [preflight (inspect! issued-host issued-probe issued-guarded
                            modules-var start-order)]
    (when-not (map? initial-system)
      (reject! :invalid-initial-biff-system))
    (let [system (original-start application-assembly initial-system
                                 modules-var start-order)]
      (try
        (let [verified ((:summary! issued-guarded))]
          (when-not (and (= guarded/report-type (:type verified))
                         (= guarded/report-version (:version verified))
                         (= :verified-selected-adapter-start (:status verified))
                         (= :one-real-loopback-challenge-observed-before-start-return
                            (:final-checked-handler-http-consumption verified))
                         (= :none (:authority verified)))
            (reject! :selected-http-adapter-not-verified-after-start))
          (inspect! issued-host issued-probe issued-guarded
                    modules-var start-order)
          {:system system
           :report (assoc preflight
                          :physical-server-start :one-selected-verified-adapter-start)})
        (catch Throwable error
          (try (biff/stop system)
               (catch Throwable _ nil))
          (throw error))))))

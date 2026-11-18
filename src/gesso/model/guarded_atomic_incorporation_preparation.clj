(ns gesso.model.guarded-atomic-incorporation-preparation
  "v1771: Build a complete, source-bound incorporated transaction through the
   REAL Gesso model.tx preparation path, and independently inspect the ordered
   formatted XTDB guard/assert/write footprint. There is no submit method.

   This is a local preparation prerequisite, not a replacement writer. The
   construction inputs remain application-supplied, and an exact preparation
   cannot authenticate a business policy, a DB prestate, XTDB execution, or
   Gesso Live delivery. No prepared plan, transaction ops, or writer capability
   leaves this API."
  (:require [gesso.model.atomic-incorporation-footprint :as footprint]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.model.guarded-atomic-incorporation-preparation/source-bound-preparation)
(def report-version 1)

(def ^:private original-incorporation resolution/incorporation-plan)
(def ^:private original-prepare tx/prepare)
(def ^:private original-footprint footprint/check-full-atomic-candidate!)
(def ^:private original-effective-guards tx/effective-guards)
(def ^:private original-command-op tx/command->tx-op)

(defn- reject! [reason]
  (throw (ex-info "Guarded full atomic preparation is not established."
                  {:guarded-atomic-incorporation-preparation/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-incorporation
                             (var-get #'resolution/incorporation-plan))
                 (identical? original-prepare (var-get #'tx/prepare))
                 (identical? original-footprint
                             (var-get #'footprint/check-full-atomic-candidate!))
                 (identical? original-effective-guards
                             (var-get #'tx/effective-guards))
                 (identical? original-command-op
                             (var-get #'tx/command->tx-op)))
    (reject! :substituted-incorporation-or-preparation-boundary)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- prepare! [plan]
  (try (original-prepare nil plan)
       (catch Exception _ (reject! :invalid-guarded-transaction-preparation))))

(defn- guard-layout!
  "Use isolated REAL tx/prepare invocations to derive each formatted physical
   assertion and command precondition, then compare their ordered concatenation
   to the full prepared vector. This catches a full-plan-only omission even
   when the source and candidate would both pass a single identical formatter."
  [{:keys [plan tx-ops]}]
  (let [commands (:commands plan)
        ledger (last commands)
        explicit (:assertions plan)
        remaining-guards (original-effective-guards commands (:guards plan))
        ;; Isolated plans all use :emit false, so they cannot publish Live
        ;; changes or invoke an :entry-fn during checking.
        isolated (fn [command guards assertions]
                   (:tx-ops (prepare! {:commands [command]
                                       :guards guards
                                       :assertions assertions
                                       :emit false})))
        assertions (mapv #(first (isolated ledger [] [%])) explicit)
        guards (mapv #(first (isolated ledger [%] [])) remaining-guards)
        command-pairs (mapv #(isolated % [] []) commands)
        _ (when-not (every? #(= 2 (count %)) command-pairs)
            (reject! :invalid-isolated-command-preparation))
        preconditions (mapv first command-pairs)
        writes (mapv second command-pairs)
        expected (into [] cat [assertions guards preconditions writes])
        ledger-write (original-command-op ledger)]
    (when-not (and (<= 2 (count commands))
                   (= :create (command/operation ledger))
                   (= ledger-write (last writes) (last tx-ops))
                   (= writes (mapv original-command-op commands))
                   (every? (fn [op]
                             (and (vector? op)
                                  (string? (first op))
                                  (.startsWith ^String (first op) "ASSERT")))
                           (into [] cat [assertions guards preconditions]))
                   (= expected tx-ops))
      (reject! :prepared-guard-assert-write-layout-diverged))
    true))

(defn- canonical! [source]
  (pinned!)
  (let [incorporated (try (original-incorporation source)
                          (catch Exception _
                            (reject! :invalid-canonical-incorporation)))
        prepared (prepare! (:plan incorporated))
        footprint-report (try (original-footprint (:binding source)
                                                 incorporated prepared)
                              (catch Exception _
                                (reject! :full-atomic-footprint-not-established)))]
    (when-not (and (= footprint/report-type (:type footprint-report))
                   (= footprint/report-version (:version footprint-report))
                   (= :exact (:complete-normalized-plan footprint-report))
                   (= :exact (:prepared-assert-and-write-operations footprint-report))
                   (= :checked (:original-atomic-command-incorporation
                                footprint-report))
                   (= :not-established (:typed-complete-cas-writer-adoption
                                        footprint-report))
                   (= :none (:transaction-authority footprint-report)))
      (reject! :unqualified-full-atomic-footprint))
    (guard-layout! prepared)
    (pinned!)
    {:incorporated incorporated :prepared prepared}))

(defn- report [status]
  {:type report-type
   :version report-version
   :status status
   :scope :one-canonical-source-relative-guarded-preparation
   :incorporated-ledger :included-in-same-atomic-plan
   :guard-order :explicit-then-dependencies-then-command-preconditions
   :write-order :domain-then-command-ledger
   :formatted-xtdb-footprint :checked-by-independent-isolated-preparation
   :live-options :preserved-without-publication
   :physical-submission :disabled
   :typed-complete-cas-gateway-adoption :not-established
   :actual-database-prestate :not-established
   :business-policy-authenticity :not-established
   :xtdb-commit :not-established
   :deployed-writer-coverage :not-established
   :certificate :not-issued :admission :not-admitted
   :transaction-authority :none :authority :none})

(defn issue-guarded-atomic-preparer!
  "Capture exactly one original canonical incorporation input with keys
   {:binding :outcome :now :plan}, not a caller-composed transaction result.
   Verify it using command-resolution/incorporation-plan, model.tx/prepare,
   the v1770 full footprint checker and independently prepared guard/write
   slices. :prepare-once! repeats those checks against the captured source,
   consumes its attempt before doing any further work, and returns ONLY a
   redacted report. It cannot execute or supply raw ops to a physical writer.

   Source authenticity, truth of declared rules, current XTDB state, and all
   execution and Live-delivery guarantees remain unestablished."
  [source]
  (when-not (and (plain-map? source)
                 (= #{:binding :outcome :now :plan} (set (keys source)))
                 (plain-map? (:plan source)))
    (reject! :invalid-original-incorporation-source))
  (let [initial (canonical! source)
        consumed (AtomicBoolean. false)
        recheck! (fn []
                   (when-not (= initial (canonical! source))
                     (reject! :canonical-preparation-source-changed))
                   true)
        summary! (fn []
                   (recheck!)
                   (report (if (.get consumed)
                             :preparation-attempt-consumed
                             :issued-not-submitted)))
        prepare-once! (fn []
                        (when-not (.compareAndSet consumed false true)
                          (reject! :preparation-already-attempted))
                        (recheck!)
                        (report :guarded-atomic-prepared-not-submitted))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :prepare-once! prepare-once!}))

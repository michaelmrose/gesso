(ns gesso.model.atomic-incorporation-footprint
  "v1770: Read-only preparation correspondence for a proposed FULL atomic
   incorporated model transaction. Unlike the current typed one-document CAS
   gateway, a future writer must retain the domain commands, durable command
   ledger CREATE, all guarded/explicit ASSERTs, and Gesso Live publication
   configuration together in the same authoritative transaction.

   This checker compares a candidate prepared plan to the ORIGINAL supplied
   incorporated plan, including the exact formatted XTDB transaction vector.
   It never installs, invokes, or certifies a writer. An application-supplied
   plan and a matching candidate may both be wrong about business policy;
   formatting equality does not establish physical persistence or causality."
  (:require [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]))

(def report-type
  :gesso.model.atomic-incorporation-footprint/full-plan-preparation-match)
(def report-version 1)

(defn- reject! [reason]
  ;; Do not leak document/command contents in a proof diagnostic.
  (throw (ex-info "Full atomic incorporation footprint is not established."
                  {:atomic-incorporation-footprint/rejection true
                   :reason reason})))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- prepared! [plan reason]
  (try
    (tx/prepare nil plan)
    (catch Exception _ (reject! reason))))

(defn- original-incorporation! [binding incorporated]
  (when-not (and (plain-map? incorporated)
                 (= #{:record :plan :scope} (set (keys incorporated)))
                 (= resolution/assurance-scope (:scope incorporated))
                 (plain-map? (:record incorporated))
                 (plain-map? (:plan incorporated)))
    (reject! :invalid-original-incorporation-shape))
  (let [binding (try (resolution/require-binding binding)
                     (catch Exception _ (reject! :invalid-original-binding)))
        record (:record incorporated)
        plan (:plan incorporated)
        _ (try (resolution/require-record record)
               (catch Exception _ (reject! :invalid-command-ledger-record)))
        commands (:commands plan)
        ledger (when (vector? commands)
                 (filterv #(= resolution/command-entity-type
                              (:model/entity-type %)) commands))]
    (when-not (and (= binding (:gesso.command/binding record))
                   (= (resolution/record-id binding) (:xt/id record))
                   (vector? commands)
                   (<= 2 (count commands))
                   (= 1 (count ledger))
                   (= (last commands) (first ledger))
                   (= :create (command/operation (first ledger)))
                   (= record (command/after (first ledger)))
                   (= [resolution/command-entity-type
                       (resolution/record-id binding)]
                      (tx/command-target (first ledger)))
                   (every? #(not= resolution/command-entity-type
                                  (:model/entity-type %))
                           (butlast commands)))
      (reject! :incomplete-or-nonatomic-original-ledger))
    (let [prepared (prepared! plan :invalid-original-transaction-plan)]
      (when-not (= (:plan prepared) plan)
        (reject! :noncanonical-original-transaction-plan))
      prepared)))

(defn check-full-atomic-candidate!
  "Compare one candidate {:plan normalized-plan :tx-ops formatted-ops} to
   the EXACT currently supplied incorporated source. Both plan and formatted
   XTDB operations must match the original, after independent preparation.

   The plan comparison includes ordered domain+ledger commands, all guards,
   explicit assertions, semantic changes, :emit, :entry, :entry-fn and
   :tx-options; the formatted comparison includes all ASSERT/write operations.
   No callback or writer is invoked here. A positive report is strictly a
   SOURCE-RELATIVE, UNINSTALLED preparation footprint, not a permit to submit.
   In particular it does not establish the supplied original's authenticity,
   Gesso Live notification success, real XTDB use, or typed CAS adoption."
  [binding incorporated candidate]
  (let [original (original-incorporation! binding incorporated)]
    (when-not (and (plain-map? candidate)
                   (= #{:plan :tx-ops} (set (keys candidate)))
                   (plain-map? (:plan candidate))
                   (vector? (:tx-ops candidate)))
      (reject! :invalid-full-atomic-candidate-shape))
    (let [candidate-prepared (prepared! (:plan candidate)
                                        :invalid-candidate-transaction-plan)]
      (when-not (and (= (:plan candidate-prepared) (:plan candidate))
                     (= (:plan candidate-prepared) (:plan original)))
        (reject! :candidate-changes-original-transaction-plan))
      (when-not (and (= (:tx-ops candidate-prepared) (:tx-ops candidate))
                     (= (:tx-ops candidate-prepared) (:tx-ops original)))
        (reject! :candidate-physical-operations-diverge))
      {:type report-type
       :version report-version
       :scope :source-relative-uninstalled-full-atomic-plan
       :original-atomic-command-incorporation :checked
       :complete-normalized-plan :exact
       :prepared-assert-and-write-operations :exact
       :live-publication-inputs :exact-plan-only
       :actual-live-publication :not-established
       :typed-complete-cas-writer-adoption :not-established
       :physical-xtdb-commit :not-established
       :original-business-policy-authenticity :not-established
       :complete-deployed-writer-coverage :not-established
       :certificate :not-issued
       :admission :not-admitted
       :transaction-authority :none
       :authority :none})))

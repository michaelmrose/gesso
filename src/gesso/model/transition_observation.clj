(ns gesso.model.transition-observation
  "v1298: controlled, bounded persistence witness for ONE authoritative command.

   begin-observation! takes the original canonical durable incorporation and
   explicitly declared inspected targets, and reads those documents at ONE
   pinned XTDB snapshot. The host independently submits that incorporation
   using its ordinary, trusted transaction path and supplies the definitive
   execute-tx key to complete-observation!. Completion reads ANOTHER pinned
   snapshot, including the transaction ledger and actual command record.

   No transaction is submitted here. A successful witness is a statement about
   sampled persistent documents, the matching command row, and one visible
   XTDB ledger entry. It is NOT a provenance attestation of the submitted
   transaction payload, the internal transaction read snapshot, authorization,
   the absence of undiscovered writers, or a general refinement theorem.

   This collector intentionally requires an isolated ledger window: the
   supplied transaction must be the next ledger ID after the before snapshot,
   and must remain the last ledger ID in the after snapshot. Other writes in
   the window fail closed. The two document observation phases are not one
   atomic before/after transaction snapshot. The caller must ensure that
   unrelated writes do not modify tracked targets around these samples.
   Only DEFINITIVELY committed incorporations qualify as accepted steps.

   The returned observations include potentially sensitive domain documents;
   callers should retain them only in controlled test environments. One-shot
   capture handles are process-local, not durable or independently attested."
  (:require [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx-model :as model]
            [gesso.model.tx-refinement :as refinement]
            [xtdb.api :as xt])
  (:import [java.time Instant ZonedDateTime]
           [java.util WeakHashMap]))

(def observation-version 1)
(def observation-type :gesso.model.transition-observation/committed-persisted-step)
(def observation-scope
  {:evidence-class :bounded-controlled-xtdb-commit-observation
   :collected #{:exact-declared-command-plan-and-footprint
                :phase-local-pinned-document-snapshots
                :visible-committed-xtdb-ledger-record
                :adjacent-and-terminal-transaction-ledger-window
                :persisted-canonical-command-record-and-binding
                :explicit-before-and-after-absence-or-presence}
   :assumed #{:trusted-execute-tx-key-from-actual-submission
              :declared-plan-is-the-actually-submitted-plan
              :supplied-node-is-the-authoritative-xtdb-node
              :single-controlled-ledger-observation-window
              :observed-read-targets-cover-all-application-dependencies
              :consistent-xtdb-snapshot-token-and-clock-semantics}
   :not-established #{:authenticated-transaction-payload-provenance
                      :transaction-internal-prestate-correspondence
                      :absence-of-untracked-physical-writers
                      :business-authorization-or-guard-completeness
                      :unbounded-concurrency-or-serializability
                      :independent-proof-checker-soundness
                      :abstract-to-physical-transition-refinement
                      :production-certification-or-execution-authority}})

(def ^:private request-keys #{:node :incorporation :targets})
(def ^:private incorporation-keys #{:plan :record :scope})
(def ^:private outcome-keys #{:tx-key})
(def ^:private command-table resolution/command-entity-type)
(def ^:private ^WeakHashMap live-captures (WeakHashMap.))

(defn- reject! [reason at]
  ;; Do not leak command bindings or persisted personal data into exceptions.
  (throw (ex-info "Invalid controlled XTDB transition observation."
                  {:error/type ::invalid-observation :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [value expected at]
  (when-not (and (plain-map? value)
                 (nil? (meta value))
                 (= expected (set (keys value))))
    (reject! :invalid-source-shape at))
  value)

(defn- checked-targets! [targets]
  (when-not (and (vector? targets)
                 (nil? (meta targets))
                 (seq targets)
                 (= (count targets) (count (distinct targets))))
    (reject! :invalid-targets [:targets]))
  (doseq [[table id :as target] targets]
    (when-not (and (vector? target) (= 2 (count target))
                   (nil? (meta target))
                   (keyword? table) (nil? (namespace table))
                   (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
                   (some? id))
      (reject! :invalid-target [:targets])))
  (refinement/require-targets targets))

(defn- plan-footprint [plan]
  ;; The original finite-model checker also recognizes exact canonical
  ;; document-field-count ASSERTs. Include their read targets: ignoring them
  ;; would make a nominally complete read domain omit a real dependency.
  (set (concat (map command/target (:commands plan))
               (map :target (:checks (model/step {} plan))))))

(defn- checked-incorporation! [incorporation targets]
  (exact-map! incorporation incorporation-keys [:incorporation])
  (when-not (= resolution/assurance-scope (:scope incorporation))
    (reject! :invalid-command-scope [:incorporation :scope]))
  (let [plan (model/admitted-plan (:plan incorporation))
        record (resolution/require-record (:record incorporation))
        record-target [command-table (:xt/id record)]
        matching (filterv #(= command-table (first (command/target %)))
                          (:commands plan))]
    (when-not (and (= 1 (count matching))
                   (command/create? (first matching))
                   (= record-target (command/target (first matching)))
                   (= record (command/after (first matching))))
      (reject! :missing-or-mismatched-command-creation [:incorporation :plan]))
    (when-not (= (set targets) (plan-footprint plan))
      (reject! :incomplete-or-extra-target-coverage [:targets]))
    {:record record :plan plan :record-target record-target}))

(defn- phase-basis! [node]
  (let [rows (vec (xt/q node "SHOW SNAPSHOT_TOKEN"))
        token (when (= 1 (count rows)) (:snapshot-token (first rows)))]
    (when-not (and (string? token) (seq token))
      (reject! :unavailable-snapshot-token [:xtdb :snapshot]))
    {:snapshot-token token :current-time (Instant/now)}))

(defn- watermark! [node basis]
  (let [rows (vec (xt/q node
                        "SELECT _id FROM xt.txs ORDER BY _id DESC LIMIT 1"
                        basis))]
    (cond
      (empty? rows) -1
      (and (= 1 (count rows))
           (integer? (:xt/id (first rows)))
           (<= 0 (:xt/id (first rows))))
      (:xt/id (first rows))
      :else (reject! :invalid-ledger-watermark [:xtdb :ledger]))))

(defn- read-target! [node basis [table id :as target]]
  (let [rows (vec (xt/q node [(str "SELECT * FROM " (name table)
                                    " WHERE _id = ? LIMIT 2") id] basis))]
    (when (> (count rows) 1)
      (reject! :duplicate-physical-document [:xtdb :documents]))
    (let [record (first rows)]
      (when (and record
                 (or (not (map? record))
                     (not= id (:xt/id record))
                     (some nil? (vals record))))
        (reject! :invalid-physical-document [:xtdb :documents]))
      record)))

(defn- read-phase! [node targets]
  (let [basis (phase-basis! node)
        watermark (watermark! node basis)
        docs (into {} (map (fn [target]
                             [target (read-target! node basis target)])) targets)]
    {:basis basis :watermark watermark :documents docs}))

(defn- checked-tx-key! [value]
  (when-not (and (map? value)
                 (nil? (meta value))
                 (= #{:tx-id :system-time} (set (keys value)))
                 (integer? (:tx-id value)) (<= 0 (:tx-id value))
                 (instance? Instant (:system-time value)))
    (reject! :missing-definitive-commit-key [:tx-key]))
  value)

(defn- committed-ledger! [node basis {:keys [tx-id system-time]}]
  (let [rows (vec (xt/q node
                        ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                         tx-id]
                        basis))
        ledger (when (= 1 (count rows)) (first rows))]
    (when-not (and (map? ledger)
                   (= tx-id (:xt/id ledger))
                   (true? (:committed ledger))
                   (instance? ZonedDateTime (:system-time ledger))
                   (= system-time (.toInstant ^ZonedDateTime
                                               (:system-time ledger))))
      (reject! :missing-or-noncommitted-ledger-proof [:xtdb :ledger]))
    ledger))

(defn begin-observation!
  "READ ONLY. Capture the full declared physical target footprint before an
   independently initiated, definitive canonical durable-command submission.
   Return an opaque, one-shot handle. Caller supplies no pre-state maps."
  [source]
  (exact-map! source request-keys [])
  (when (nil? (:node source))
    (reject! :missing-node [:node]))
  (let [targets (checked-targets! (:targets source))
        {:keys [plan record record-target]} (checked-incorporation!
                                            (:incorporation source) targets)
        before (read-phase! (:node source) targets)]
    (when (some? (get (:documents before) record-target))
      (reject! :command-already-present [:xtdb :command-record]))
    (let [handle (Object.)]
      (locking live-captures
        (.put live-captures handle
              {:node (:node source) :plan plan :record record
               :record-target record-target :targets targets :before before}))
      handle)))

(defn- consume! [handle]
  (locking live-captures
    (let [capture (.remove live-captures handle)]
      (when-not capture
        (reject! :unknown-or-consumed-capture [:capture]))
      capture)))

(defn complete-observation!
  "READ ONLY. Once the host has definitively received a successful XTDB
   execute-tx key for its real submitted incorporation, sample committed
   poststate and the system ledger. A failed, unknown, rejected, or repeated
   attempt is NOT an accepted persisted transition and must not call this
   method with invented status/tx-key. Consumes its handle even on failure."
  [handle outcome]
  (let [{:keys [node plan record record-target targets before]} (consume! handle)]
    (exact-map! outcome outcome-keys [:outcome])
    (let [{:keys [tx-id] :as tx-key} (checked-tx-key! (:tx-key outcome))
          after (read-phase! node targets)]
      (when-not (and (= tx-id (inc (:watermark before)))
                     (= tx-id (:watermark after)))
        (reject! :non-isolated-or-mismatched-ledger-window [:xtdb :ledger]))
      (committed-ledger! node (:basis after) tx-key)
      (when-not (= record (get (:documents after) record-target))
        (reject! :missing-or-mismatched-persisted-command [:xtdb :command-record]))
      {:type observation-type
       :version observation-version
       :scope observation-scope
       :evidence-class :bounded-controlled-xtdb-commit-observation
       :observed-targets (set targets)
       :declared-plan plan
       :observed-before (:documents before)
       :observed-after (:documents after)
       :command-target record-target
       :persisted-command-record (get (:documents after) record-target)
       :command-binding (:gesso.command/binding record)
       :physical-tx-id tx-id
       :physical-status :committed
       :ledger-window :adjacent-and-terminal-in-observed-snapshots
       :plan-footprint :exact-for-declared-incorporation
       :witness-provenance :caller-linked-not-independently-authenticated
       :abstract-transition-correspondence :not-established
       :untracked-writer-coverage :not-established
       :certification :none
       :authority :none})))

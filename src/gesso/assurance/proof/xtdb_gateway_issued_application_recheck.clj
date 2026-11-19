(ns gesso.assurance.proof.xtdb-gateway-issued-application-recheck
  "v1870: Read-only follow-up to a historical issued-executor observation.
   Invoke only the original plan's document and target acquisition callbacks,
   then independently compare their complete document to a pinned XTDB read.
   No constructor, issued executor, physical writer, or transaction is invoked.

   The returned value is a point-in-time SOURCE-RELATIVE comparison, NOT an
   equality check against the candidate captured at the earlier observation:
   original acquisition callbacks may change their answers or cause effects.
   The context node is untrusted, and the physical state may change again.

   v1871: An optional PRIVATE source-bound witness of the originally checked
   target and complete before-document permits a second comparison against
   the SAME later pinned row, without replaying a constructor. The accessor
   target must still equal the original target. Neither comparison implies
   freshness after the read, ABA exclusion, or transaction-time identity.

   v1872: Also compare the actual original pinned token's single database /
   system-time point with this later pinned token's point. An advanced basis
   only proves the reported database basis advanced, not a change to the row."
  (:require [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis]
            [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity :as snapshot])
  (:import (java.time Instant)
           (java.util UUID)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-application-recheck/point-in-time)
(def report-version 1)

(def ^:private original-require-plan plan/require-current-plan-operation!)
(def ^:private original-require-binding resolution/require-binding)
(def ^:private original-query xt/q)
(def ^:private original-decode basis/<-time-basis-str)

(defn- reject! [reason]
  (throw (ex-info "Read-only issued prestate comparison not established."
                  {:reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-require-plan
                             (var-get #'plan/require-current-plan-operation!))
                 (identical? original-require-binding
                             (var-get #'resolution/require-binding))
                 (identical? original-query (var-get #'xt/q))
                 (identical? original-decode (var-get #'basis/<-time-basis-str)))
    (reject! :substituted-original-read-only-recheck-function)))

(defn compare-original-acquired-prestate!
  "Reacquire the current document and target from the EXACT registered
   original plan source for a previously checked canonical invocation. Read
   the selected physical row on the caller's original context XTDB node using
   a pinned snapshot. A matching status means ONLY that this later source
   accessor value matched that read. No historical-candidate value is compared.

   Application-owned acquisition callbacks may have effects. No side effects
   from them are excluded or undone, and this does not perform authentication,
   original command construction, XTDB submission or source proof discharge."
  ([source executor operation ctx invocation]
   (compare-original-acquired-prestate!
    source executor operation ctx invocation nil))
  ([source executor operation ctx invocation original-before]
  (pinned!)
  ;; Validate before invoking arbitrary application-owned callbacks.
  (when-not (and (map? source) (map? ctx)
                 (some? (:biff.xtdb/node ctx))
                 (map? invocation) (nil? (meta invocation))
                 (= #{:binding :mode} (set (keys invocation)))
                 (= :submit (:mode invocation))
                 (qualified-keyword? operation)
                 (= operation (get-in source [:certified-source :operation]))
                 (map? (:binding invocation))
                 (nil? (meta (:binding invocation)))
                 (= operation (get-in invocation [:binding :operation]))
                 (try (= (:binding invocation)
                         (original-require-binding (:binding invocation)))
                      (catch Exception _ false)))
    (reject! :invalid-original-read-only-recheck-invocation))
  (try (original-require-plan executor source)
       (catch Exception _ (reject! :original-read-only-recheck-source-not-current)))
  (let [document! (:current-document! source)
        target! (:target! source)]
    (when-not (and (fn? document!) (fn? target!))
      (reject! :missing-original-read-only-recheck-accessors))
    (let [before (document! ctx (:binding invocation))
          target (target! ctx (:binding invocation))
          table (first target)
          id (second target)]
      (when-not (and (map? before) (nil? (meta before))
                     (vector? target) (= 2 (count target))
                     (= id (:xt/id before))
                     (keyword? table) (nil? (namespace table))
                     (re-matches #"[a-z][a-z0-9_]*" (name table))
                     (or (and (string? id) (seq id))
                         (instance? UUID id)))
        (reject! :invalid-original-read-only-recheck-target))
      (when (and original-before
                 (not (and (map? original-before)
                           (nil? (meta original-before))
                           (= #{:operation-id :target :before
                                :original-snapshot-point :source-identity}
                              (set (keys original-before)))
                           (= operation (:operation-id original-before))
                           (= target (:target original-before))
                           (map? (:before original-before))
                           (nil? (meta (:before original-before)))
                           (= id (get-in original-before [:before :xt/id]))
                           (map? (:source-identity original-before))
                           (let [p (:original-snapshot-point original-before)]
                             (or (nil? p) (snapshot/valid-private-point? p))))))
        (reject! :invalid-private-original-prestate-witness))
      (pinned!)
      (let [node (:biff.xtdb/node ctx)
            tokens (try (original-query node "SHOW SNAPSHOT_TOKEN")
                        (catch Throwable _ (reject! :read-only-recheck-snapshot-unavailable)))
            token (when (and (sequential? tokens)
                             (= 1 (count tokens))
                             (map? (first tokens))
                             (= #{:snapshot-token} (set (keys (first tokens)))))
                    (:snapshot-token (first tokens)))]
        (when-not (and (string? token) (seq token)
                       (try (map? (original-decode token))
                            (catch Throwable _ false)))
          (reject! :invalid-read-only-recheck-snapshot))
        (let [later-point (snapshot/comparable-point (original-decode token))
              rows (try (original-query
                         node [(str "SELECT * FROM " (name table)
                                    " WHERE _id = ? LIMIT 2") id]
                         {:snapshot-token token :current-time (Instant/now)})
                        (catch Throwable _
                          (reject! :read-only-recheck-query-unavailable)))
              comparison (cond
                           (and (sequential? rows)
                                (= 1 (count rows))
                                (map? (first rows))
                                (= before (first rows))) :source-document-matched-at-read
                           (and (sequential? rows)
                                (<= 0 (count rows) 2)
                                (every? map? rows)) :source-document-differed-at-read
                           :else (reject! :invalid-read-only-recheck-result))
              historical (if original-before
                           (if (and (= 1 (count rows))
                                    (= (:before original-before) (first rows)))
                             :historical-prestate-matched-at-read
                             :historical-prestate-differed-at-read)
                           :not-compared)]
          (pinned!)
          {:type report-type :version report-version
           :operation-id operation
           :comparison comparison
           :historical-prestate-at-read historical
           :reported-basis-relation
           (if original-before
             (snapshot/reported-basis-relation
              (:original-snapshot-point original-before) later-point)
             :not-compared)
           :reported-basis-scope :single-database-system-time-only
           :historical-target (if original-before
                                :same-original-execution-target
                                :not-compared)
           :source :same-original-registered-read-accessors
           :read :independent-pinned-xtdb-on-caller-context-node
           :historical-candidate-equality :not-established
           :read-time-state-only true
           :currentness-after-read :not-established
           :callback-purity :not-established
           :physical-submission :not-invoked-by-this-check
           :transaction-authority :none
           :certificate :not-issued :authority :none}))))))

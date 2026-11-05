(ns gesso.assurance.proof.physical-write-footprint-observation
  "v1380: Source-rechecked DECLARED write footprint and bounded, supplied
   pre/post frame comparison for one Gesso model transaction attempt.

   These are separate evidence levels:
   * The captured admitted plan declares which documents and fields it intends
     to replace through Gesso's currently supported :put-docs lowering.
   * The v1375 recorded submission is a copyable, caller-attributed vector,
     not authenticated bytes from XTDB.
   * The v1378 post-rows and these pre-rows are independently supplied maps,
     not authenticated transaction-internal snapshots or a full database scan.

   A matching finite target list CANNOT rule out writes to other documents,
   hidden writers, timing/basis changes, or a different attempt causing the
   observed state. Rejected attempts stay rejected. This API never certifies
   complete physical write coverage, frame refinement, or authority. Domain
   values and document IDs are deliberately absent from returned reports and
   typed errors; callers must protect source/observation inputs themselves."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gesso.assurance.proof.transaction-outcome-state-provenance :as state]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as tx-model]))

(def footprint-version 1)
(def footprint-type
  :gesso.assurance.proof.physical-write-footprint-observation/one-attempt)

(def ^:private input-keys
  #{:outcome-state-source :outcome-state-report :prestate-observation})
(def ^:private pre-observation-keys
  #{:attempt-id :operation-id :observation-origin :read-phase
    :reported-read-basis :pre-rows})
(def ^:private row-keys #{:target :document})
(def ^:private max-targets 64)
(def ^:private original-state-check state/require-current-outcome-state!)
(def ^:private original-admitted tx-model/admitted-plan)
(def ^:private original-target command/target)
(def ^:private original-before command/before)
(def ^:private original-after command/after)

(defn- reject! [reason at]
  (throw (ex-info "Invalid physical write/frame observation."
                  {:physical-write-footprint-observation/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact! [x allowed at]
  (when-not (and (plain-map? x) (nil? (meta x))
                 (= allowed (set (keys x))))
    (reject! :invalid-source-shape at))
  x)

(defn- current! [source report]
  (when-not (and (identical? original-state-check
                             (var-get #'state/require-current-outcome-state!))
                 (identical? original-admitted (var-get #'tx-model/admitted-plan))
                 (identical? original-target (var-get #'command/target))
                 (identical? original-before (var-get #'command/before))
                 (identical? original-after (var-get #'command/after)))
    (reject! :substituted-upstream-checker []))
  (let [checked (original-state-check source report)]
    (when-not (and (= state/provenance-version (:version checked))
                   (= state/provenance-type (:type checked))
                   (= :source-replayed-hook-submission-and-bounded-post-readback
                      (:checking checked))
                   (#{:committed :rejected} (:captured-physical-status checked))
                   (= :recorded-operations-agree (:comparison-status checked))
                   (false? (:actual-submission-authenticated? checked))
                   (false? (:post-read-origin-authenticated? checked))
                   (= :not-established (:complete-physical-write-footprint checked))
                   (= :outcome-and-state-diagnostics-only (:proof-status checked))
                   (= :not-admitted (:admission checked))
                   (= :not-issued (:certificate checked))
                   (= :none (:authority checked)))
      (reject! :unusable-upstream-state-report [:outcome-state-report]))
    checked))

(defn- declared! [source]
  (let [acquired (get-in source [:submission-source :acquisition-report])
        plan (original-admitted (:captured-prepared-plan acquired))
        commands (:commands plan)
        targets (mapv original-target commands)
        befores (mapv original-before commands)
        afters (mapv original-after commands)
        recorded (get-in source [:submission-source :recorded-tx-ops])
        recorded-writes (filterv #(and (vector? %) (= :put-docs (first %)))
                                 recorded)
        assert-sql? (fn [op]
                      (and (vector? op) (string? (first op))
                           (boolean (re-find #"(?is)^\s*ASSERT\b" (first op)))))]
    (when-not (and (vector? commands)
                   (<= 1 (count commands) max-targets)
                   (= (count targets) (count (set targets)))
                   (= (count commands) (count recorded-writes))
                   (every? #(or (and (vector? %) (= :put-docs (first %)))
                                (assert-sql? %))
                           recorded)
                   (every? true?
                           (map (fn [op target]
                                  (and (= 3 (count op))
                                       (plain-map? (nth op 2 nil))
                                       (= [(second op) (:xt/id (nth op 2))]
                                          target)))
                                recorded-writes targets)))
      (reject! :unsupported-or-divergent-declared-write-shape
               [:outcome-state-source]))
    (doseq [[i [before after]] (map-indexed vector (map vector befores afters))]
      (when-not (and (or (nil? before) (plain-map? before))
                     (plain-map? after)
                     (nil? (meta after))
                     (or (nil? before) (nil? (meta before))))
        (reject! :invalid-declared-document [:outcome-state-source i])))
    {:plan plan :targets targets :befores befores :afters afters
     :recorded-write-count (count recorded-writes)}))

(defn- exact-instant-equal? [expected observed]
  (cond
    (and (instance? java.time.Instant expected)
         (instance? java.time.ZonedDateTime observed))
    (= expected (.toInstant ^java.time.ZonedDateTime observed))

    (and (instance? java.time.ZonedDateTime expected)
         (instance? java.time.Instant observed))
    (= (.toInstant ^java.time.ZonedDateTime expected) observed)

    :else (= expected observed)))

(defn- comparable-document [target document]
  ;; SELECT * can omit :xt/id; correlation with target is itself claimed.
  ;; This is a bounded comparison convenience, not an authenticated row ID.
  (when document
    (if (= (second target) (:xt/id document))
      (dissoc document :xt/id)
      document)))

(defn- fields-changed [before after]
  (let [absent (Object.)]
    (->> (set/union (set (keys before)) (set (keys after)))
         (filter #(not (exact-instant-equal?
                         (get before % absent) (get after % absent))))
         set)))

(defn- document-agrees? [expected observed target]
  ;; Unlike v1378's expected-fields-only check, extra or missing fields here
  ;; count as divergence within the specifically supplied target projection.
  (let [expected (comparable-document target expected)
        observed (comparable-document target observed)]
    (or (and (nil? expected) (nil? observed))
        (and (map? expected) (map? observed)
             (= (set (keys expected)) (set (keys observed)))
             (every? (fn [[field value]]
                       (exact-instant-equal? value (get observed field)))
                     expected)))))

(defn- pre-rows! [observation upstream targets]
  (exact! observation pre-observation-keys [:prestate-observation])
  (when-not (and (= (:attempt-id upstream) (:attempt-id observation))
                 (= (:operation-id upstream) (:operation-id observation))
                 (= :caller-supplied-unattested (:observation-origin observation))
                 (= :before-attempt (:read-phase observation))
                 (or (nil? (:reported-read-basis observation))
                     (and (or (uuid? (:reported-read-basis observation))
                              (integer? (:reported-read-basis observation))
                              (string? (:reported-read-basis observation)))
                          (not (and (string? (:reported-read-basis observation))
                                    (str/blank? (:reported-read-basis observation)))))))
    (reject! :unbound-or-unsupported-prestate-observation
             [:prestate-observation]))
  (let [rows (:pre-rows observation)]
    (when-not (and (vector? rows) (nil? (meta rows))
                   (= (count targets) (count rows)))
      (reject! :missing-or-extra-prestate-targets [:prestate-observation :pre-rows]))
    (doseq [[i [target row]] (map-indexed vector (map vector targets rows))]
      (exact! row row-keys [:prestate-observation :pre-rows i])
      (when-not (= target (:target row))
        (reject! :reordered-or-unexpected-prestate-target
                 [:prestate-observation :pre-rows i]))
      (when-not (or (nil? (:document row))
                    (and (plain-map? (:document row))
                         (nil? (meta (:document row)))))
        (reject! :invalid-prestate-document
                 [:prestate-observation :pre-rows i])))
    rows))

(defn audit-declared-write-frame!
  "Compare a source-replayed v1378 report with CLAIMED prestate observations.

   Input {:outcome-state-source <original v1378 input>
          :outcome-state-report <matching v1378 report>
          :prestate-observation
          {:attempt-id UUID :operation-id keyword
           :observation-origin :caller-supplied-unattested
           :read-phase :before-attempt :reported-read-basis nil|token
           :pre-rows [{:target [entity-type id] :document doc-or-nil} ...]}}.

   Pre-rows and upstream post-rows MUST be in declared-command order. Reports
   name only mismatch indices and aggregate counts, never domain field names
   or values. Only the finite supplied targets are examined. A match does NOT
   establish physical write-set completeness, read basis, causality, or frame
   safety over unobserved document IDs." [input]
  (exact! input input-keys [])
  (let [{:keys [outcome-state-source outcome-state-report
                prestate-observation]} input
        upstream (current! outcome-state-source outcome-state-report)
        {:keys [plan targets befores afters recorded-write-count]}
        (declared! outcome-state-source)
        before-rows (pre-rows! prestate-observation upstream targets)
        after-rows (get-in outcome-state-source [:state-observation :post-rows])
        before-docs (mapv :document before-rows)
        after-docs (mapv :document after-rows)
        before-mismatches (->> (range (count targets))
                               (filterv #(not (document-agrees?
                                                (nth befores %) (nth before-docs %)
                                                (nth targets %)))))
        after-mismatches (->> (range (count targets))
                              (filterv #(not (document-agrees?
                                               (nth afters %) (nth after-docs %)
                                               (nth targets %)))))
        delta-mismatches
        (->> (range (count targets))
             (filterv
               (fn [i]
                 (let [target (nth targets i)
                       declared (fields-changed
                                  (comparable-document target (nth befores i))
                                  (comparable-document target (nth afters i)))
                       observed (fields-changed
                                  (comparable-document target (nth before-docs i))
                                  (comparable-document target (nth after-docs i)))]
                   (not= declared observed)))))
        extra-post-indices
        (->> (range (count targets))
             (filterv (fn [i]
                        (let [target (nth targets i)]
                          (seq (set/difference
                                 (set (keys (comparable-document target
                                                                  (nth after-docs i))))
                                 (set (keys (comparable-document target
                                                                  (nth afters i))))))))))
        mismatches (set/union (set before-mismatches)
                              (set after-mismatches)
                              (set delta-mismatches))
        outcome (:captured-physical-status upstream)
        status (cond
                 (= outcome :rejected) :rejected-attempt-no-write-attribution
                 (seq mismatches) :observed-declared-domain-frame-diverges
                 :else :bounded-claimed-frame-agrees)
        declared-changed-count
        (reduce + (map (fn [target before after]
                         (count (fields-changed
                                  (comparable-document target before)
                                  (comparable-document target after))))
                       targets befores afters))]
    {:type footprint-type
     :version footprint-version
     :status status
     :checking :source-replayed-declared-writes-and-supplied-bounded-frame
     :operation-id (:operation-id upstream)
     :attempt-id (:attempt-id upstream)
     :issued-application-fingerprint (:issued-application-fingerprint upstream)
     :issued-plan-transition-fingerprint
     (:issued-plan-transition-fingerprint upstream)
     :captured-physical-status outcome
     :declared-write-target-count (count targets)
     :declared-assigned-or-removed-field-count declared-changed-count
     :declared-create-command-count (count (filter nil? befores))
     :declared-update-command-count (count (remove nil? befores))
     :declared-guard-count (count (:guards plan))
     :declared-explicit-assertion-count (count (:assertions plan))
     :caller-recorded-put-docs-count recorded-write-count
     :caller-recorded-submission-authenticated? false
     :pre-read-origin-claim (:observation-origin prestate-observation)
     :pre-read-origin-authenticated? false
     :pre-read-basis-authenticated? false
     :post-read-origin-authenticated? false
     :bounded-prestate-mismatch-target-indices before-mismatches
     :bounded-poststate-mismatch-target-indices after-mismatches
     :bounded-field-delta-mismatch-target-indices delta-mismatches
     :bounded-extra-post-field-target-indices extra-post-indices
     :bounded-declared-domain-agreement? (empty? mismatches)
     :physical-write-targets-complete? :not-established
     :physical-write-set-complete? :not-established
     :physical-frame-over-unobserved-targets :not-established
     :transaction-internal-prestate :not-established
     :post-state-caused-by-this-attempt? :not-established
     :unobserved-writers-excluded? :not-established
     :universal-execution-refinement :not-established
     :physical-refinement-obligations (:physical-refinement-obligations upstream)
     :commit-unknown :not-reclassifiable-as-committed
     :proof-status :bounded-physical-frame-diagnostic-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :authority :none}))

(defn require-current-declared-write-frame!
  "Rebuild all source premises and reject altered or stale diagnostic results."
  [input report]
  (let [current (audit-declared-write-frame! input)]
    (when-not (= current report)
      (reject! :stale-or-altered-frame-report [:report]))
    current))

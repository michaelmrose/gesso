(ns gesso.assurance.proof.finite-physical-frame-observation
  "v1415: Compare CALLER-SUPPLIED pre/post observations for the finite set of
   protected documents that a canonical admitted Gesso transaction does not
   declare as writes (v1412). Recheck v1413's complete adapter-call vector
   before considering conditional agreement.

   A :fields :all declaration compares the entire document (including
   presence, absent fields, nested values and deleted keys); a finite field
   set compares exactly those named top-level fields, distinguishing absent
   from present-nil. Document existence is checked in both cases. The
   observations must contain one row for EVERY enumerated frame-only target,
   in declaration order. :document nil is an explicit observed absence, NOT
   a missing observation row. A zero-target domain is not a frame success.

   This does NOT authenticate either snapshot, its basis, the identity of
   the writer, XTDB reception/commit, or all physical writer paths. No
   unenumerated document is checked. Positive status means only equality
   among finite caller-provided observations coupled to a matching reported
   adapter call. It cannot authorize retry, settlement or certification.

   Detailed input/output retain private documents and IDs; never log them.
   redacted-summary returns only counts, categorical statuses and indices."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.captured-write-domain-correspondence :as captured]))

(def frame-version 1)
(def frame-type :gesso.assurance.proof.finite-physical-frame-observation/diagnostic)

(def ^:private input-fields
  #{:captured-source :captured-report :frame-observation})
(def ^:private observation-fields
  #{:observation-origin :captured-call-id
    :pre-read-phase :post-read-phase
    :reported-pre-read-basis :reported-post-read-basis
    :pre-rows :post-rows})
(def ^:private row-fields #{:target :document})
(def ^:private max-rows 256)
(def ^:private original-captured-check
  captured/require-current-captured-write-domain!)

(defn- reject! [reason at]
  (throw (ex-info "Invalid finite physical-frame observation."
                  {:finite-physical-frame-observation/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- plain-vector? [x] (instance? clojure.lang.APersistentVector x))

(defn- metadata-free? [x]
  (or (not (instance? clojure.lang.IObj x)) (nil? (meta x))))

(defn- exact-map! [value fields at]
  (when-not (and (plain-map? value)
                 (metadata-free? value)
                 (= fields (set (keys value))))
    (reject! :invalid-map-shape at))
  value)

(defn- read-basis? [x]
  (or (nil? x) (uuid? x) (integer? x)
      (and (string? x) (not (str/blank? x)))))

(defn- validate-rows! [phase rows expected]
  (when-not (and (plain-vector? rows)
                 (metadata-free? rows)
                 (<= (count rows) max-rows)
                 (= (count expected) (count rows)))
    (reject! :incomplete-or-extra-frame-observations [phase]))
  (doseq [[i [row entry]] (map-indexed vector (map vector rows expected))]
    (exact-map! row row-fields [phase i])
    (when-not (= (:target entry) (:target row))
      (reject! :reordered-or-unexpected-protected-target [phase i]))
    (let [doc (:document row)]
      (when-not (or (nil? doc)
                    (and (plain-map? doc)
                         (metadata-free? doc)
                         (every? keyword? (keys doc))))
        (reject! :invalid-observed-document [phase i]))
      ;; SELECT * may omit :xt/id. If present, it cannot contradict the
      ;; enumerated target's id. No implicit row association is inferred.
      (when (and (some? doc) (contains? doc :xt/id)
                 (not= (second (:target entry)) (:xt/id doc)))
        (reject! :contradictory-observed-target [phase i]))))
  rows)

(defn- field-differs? [before after field]
  (or (not= (contains? before field) (contains? after field))
      (and (contains? before field)
           (not= (get before field) (get after field)))))

(defn- frame-diff [entry before after]
  (let [fields (:fields entry)
        presence-diff? (not= (nil? before) (nil? after))
        changed-fields (if (= :all fields)
                         (->> (concat (keys before) (keys after))
                              set
                              (filter #(field-differs? before after %))
                              count)
                         (->> fields
                              (filter #(field-differs? before after %))
                              count))
        differs? (or presence-diff? (pos? changed-fields))]
    {:presence-changed? presence-diff?
     :protected-field-difference-count changed-fields
     :protection (if (= :all fields) :whole-document :partial-fields)
     :differs? differs?}))

(defn compare-finite-physical-frame!
  "Input:
   {:captured-source <original v1413 comparison input>
    :captured-report <matching v1413 report>
    :frame-observation
    {:observation-origin :caller-supplied-unattested
     :captured-call-id <single adapter call UUID>
     :pre-read-phase :before-attempt
     :post-read-phase :after-attempt
     :reported-pre-read-basis nil|UUID|integer|nonblank-string
     :reported-post-read-basis nil|UUID|integer|nonblank-string
     :pre-rows [{:target [table id] :document map-or-nil} ...]
     :post-rows [{:target [table id] :document map-or-nil} ...]}}

   A reported adapter invocation is needed to bind the two supplied row
   vectors to one *claimed* attempt. It does not prove when either read
   occurred. Missing rows, including when a document is absent, are rejected
   rather than treated as nil. Comparisons use strict Clojure equality;
   coercive database normalization is not silently assumed."
  [input]
  (exact-map! input input-fields [])
  (when-not (identical? original-captured-check
                        (var-get #'captured/require-current-captured-write-domain!))
    (reject! :substituted-upstream-checker []))
  (let [{:keys [captured-source captured-report frame-observation]} input
        checked (original-captured-check captured-source captured-report)
        _ (when-not (and (= captured/report-type (:type checked))
                         (= captured/report-version (:version checked))
                         (= :bounded-adapter-call-diagnostic-only (:proof-status checked))
                         (= :not-established (:physical-frame-outside-enumeration checked))
                         (= :not-established (:writer-path-closure checked))
                         (= :not-issued (:certificate checked))
                         (= :not-admitted (:admission checked))
                         (= :none (:authority checked))
                         (= :none (:transaction-authority checked)))
            (reject! :unusable-upstream-captured-report [:captured-report]))
        domain (get-in checked [:source :domain-report])
        entries (:frame-only-protected-documents domain)
        calls (quot (count (get-in checked [:source :capture-report :events])) 2)
        _ (when-not (= 1 calls)
            (reject! :ambiguous-or-absent-adapter-call [:captured-source]))
        expected-call-id (get-in checked [:source :capture-report :events 0 :call-id])
        observation (exact-map! frame-observation observation-fields
                                [:frame-observation])
        _ (when-not (and (= :caller-supplied-unattested (:observation-origin observation))
                         (= expected-call-id (:captured-call-id observation))
                         (uuid? (:captured-call-id observation))
                         (= :before-attempt (:pre-read-phase observation))
                         (= :after-attempt (:post-read-phase observation))
                         (read-basis? (:reported-pre-read-basis observation))
                         (read-basis? (:reported-post-read-basis observation)))
            (reject! :unbound-or-unsupported-frame-observation
                     [:frame-observation]))
        before (validate-rows! :pre-rows (:pre-rows observation) entries)
        after (validate-rows! :post-rows (:post-rows observation) entries)
        diffs (mapv (fn [entry b a]
                      (frame-diff entry (:document b) (:document a)))
                    entries before after)
        changed-indices (->> diffs
                             (keep-indexed (fn [i d] (when (:differs? d) i)))
                             vec)
        presence-indices (->> diffs
                              (keep-indexed (fn [i d] (when (:presence-changed? d) i)))
                              vec)
        whole-indices (->> diffs
                           (keep-indexed (fn [i d] (when (and (:differs? d)
                                                              (= :whole-document (:protection d))) i)))
                           vec)
        partial-indices (->> diffs
                             (keep-indexed (fn [i d] (when (and (:differs? d)
                                                                (= :partial-fields (:protection d))) i)))
                             vec)
        captured-agrees? (= :bounded-captured-adapter-operations-agree (:status checked))
        nonempty? (pos? (count entries))
        finite-equal? (and nonempty? (empty? changed-indices))
        status (cond
                 (not nonempty?) :no-frame-only-targets-enumerated
                 (seq changed-indices) :bounded-supplied-protected-frame-diverges
                 (not captured-agrees?) :adapter-plan-correspondence-unavailable
                 :else :bounded-supplied-protected-frame-agrees)]
    {:type frame-type
     :version frame-version
     :checking :source-rechecked-adapter-plan-and-caller-supplied-finite-frame
     :scope :only-caller-enumerated-frame-only-protected-documents
     :source input
     :status status
     :captured-domain-status (:status checked)
     :captured-adapter-call-count calls
     :protected-document-count (:protected-document-count domain)
     :declared-write-target-count (:declared-command-target-count domain)
     :frame-only-target-count (count entries)
     :whole-document-frame-target-count (count (filter #(= :all (:fields %)) entries))
     :partial-field-frame-target-count (count (remove #(= :all (:fields %)) entries))
     :changed-frame-target-indices changed-indices
     :document-presence-changed-indices presence-indices
     :changed-whole-document-indices whole-indices
     :changed-partial-field-indices partial-indices
     :different-protected-field-count (reduce + (map :protected-field-difference-count diffs))
     :enumerated-frame-observation-pairs-present? true
     :supplied-enumerated-frame-equal? finite-equal?
     :bounded-adapter-and-frame-agreement?
     (and finite-equal? captured-agrees?)
     :pre-read-origin-authenticated? false
     :post-read-origin-authenticated? false
     :pre-read-basis-authenticated? false
     :post-read-basis-authenticated? false
     :snapshot-order-authenticated? false
     :actual-xtdb-submission-authenticated? false
     :transaction-internal-prestate :not-established
     :physical-write-footprint-complete? :not-established
     :physical-frame-for-protected-documents :not-established
     :physical-frame-outside-enumeration :not-established
     :unobserved-writers-excluded? :not-established
     :transaction-outcome :not-established
     :snapshot-causality :not-established
     :proof-status :bounded-caller-supplied-frame-diagnostic-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :settlement-authority :none
     :retry-authority :none
     :authority :none}))

(defn require-current-finite-physical-frame! [input report]
  "Recompute both upstream contract and observed comparisons; reject altered
   reports. Replaying source cannot authenticate caller-provided snapshots."
  [input report]
  (let [current (compare-finite-physical-frame! input)]
    (when-not (and (plain-map? report) (metadata-free? report)
                   (= report current))
      (reject! :stale-or-forged-frame-report [:report]))
    current))

(defn redacted-summary [report]
  "Return only counts, positional mismatches, categories and open obligations;
   never source, target IDs, field names, document values or read tokens."
  (dissoc (require-current-finite-physical-frame!
           (:source report) report) :source))

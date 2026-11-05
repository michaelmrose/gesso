(ns gesso.assurance.proof.captured-write-domain-correspondence
  "v1413: Compare a v1412 caller-enumerated protected domain with the exact
   tx-ops recorded at ONE Gesso XTDB adapter call site (v1390).

   Agreement is bounded to the vector supplied to that captured adapter call.
   It is NOT authenticated XTDB execution, a committed write footprint, a
   transaction-internal snapshot, proof of frame for other documents, or
   evidence that all writer paths were captured. The detailed report embeds
   raw submitted documents and MUST NOT be logged. redacted-summary removes
   identifiers, payloads, and raw operations. Rechecks establish only internal
   consistency of caller-supplied observations, not their authenticity."
  (:require [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.live.consistency.xtdb :as live]
            [gesso.model.tx :as tx]))

(def report-type
  :gesso.assurance.proof.captured-write-domain-correspondence/diagnostic)
(def report-version 1)

(def ^:private input-keys #{:domain-source :domain-report :capture-report})
(def ^:private original-domain-check
  domain/require-current-protected-write-domain!)
(def ^:private original-prepare tx/prepare)
(def ^:private max-ops 256)
(def ^:private max-events 32)

(defn- reject! [reason at]
  (throw (ex-info "Invalid bounded captured-write-domain comparison."
                  {:captured-write-domain/rejection true
                   :reason reason :at at})))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- metadata-free? [x]
  (or (not (instance? clojure.lang.IObj x)) (nil? (meta x))))

(defn- exact-map! [value fields at]
  (when-not (and (plain-map? value) (metadata-free? value)
                 (= fields (set (keys value))))
    (reject! :invalid-map-shape at))
  value)

(defn- current! []
  (when-not (and (identical? original-domain-check
                             (var-get #'domain/require-current-protected-write-domain!))
                 (identical? original-prepare (var-get #'tx/prepare)))
    (reject! :substituted-upstream-checker [])))

(defn- checked-domain! [source report]
  (let [checked (original-domain-check source report)]
    (when-not (and (= domain/domain-type (:type checked))
                   (= domain/domain-version (:version checked))
                   (= :declared-finite-domain-only (:proof-status checked))
                   (= :not-established (:physical-write-footprint-complete? checked))
                   (= :not-established (:writer-path-closure checked))
                   (= :not-established (:physical-frame-outside-enumeration checked))
                   (= :not-issued (:certificate checked))
                   (= :not-admitted (:admission checked))
                   (= :none (:authority checked))
                   (= :none (:transaction-authority checked)))
      (reject! :unsupported-domain-contract [:domain-report]))
    checked))

(defn- checked-capture! [capture]
  ;; The v1390 capture does not sign its reports. Check syntax and its explicit
  ;; negative authority fields only; NEVER construe this as provenance.
  (when-not (and (plain-map? capture) (metadata-free? capture)
                 (= live/submission-capture-type (:type capture))
                 (= live/submission-capture-version (:version capture))
                 (= :instrumented-xtdb-adapter-call-site (:observation-origin capture))
                 (false? (:actual-xtdb-api-authenticated? capture))
                 (= :not-established (:complete-writer-coverage capture))
                 (= :not-established (:transaction-internal-prestate capture))
                 (= :submission-diagnostic-only (:proof-status capture))
                 (= :not-issued (:certificate capture))
                 (= :none (:authority capture))
                 (#{:returned :threw} (:outcome capture))
                 (if (= :returned (:outcome capture))
                   (and (contains? capture :result)
                        (not (contains? capture :error)))
                   (and (contains? capture :error)
                        (not (contains? capture :result))))
                 (vector? (:events capture))
                 (metadata-free? (:events capture))
                 (<= (count (:events capture)) max-events))
    (reject! :invalid-adapter-capture-envelope [:capture-report]))
  (let [events (:events capture)]
    (when (odd? (count events))
      (reject! :incomplete-adapter-call-pair [:capture-report :events]))
    (doseq [[i [started finished]] (map-indexed vector (partition 2 events))]
      (when-not (and (plain-map? started) (plain-map? finished)
                     (metadata-free? started) (metadata-free? finished)
                     (= #{:call-id :api :phase :tx-ops :tx-options}
                        (set (keys started)))
                     (= :invoked (:phase started))
                     (#{:submit-tx :execute-tx} (:api started))
                     (uuid? (:call-id started))
                     (vector? (:tx-ops started))
                     (metadata-free? (:tx-ops started))
                     (<= (count (:tx-ops started)) max-ops)
                     (plain-map? (:tx-options started))
                     (metadata-free? (:tx-options started))
                     (#{:returned :threw} (:phase finished))
                     (= (:api started) (:api finished))
                     (= (:call-id started) (:call-id finished))
                     (if (= :returned (:phase finished))
                       (= #{:call-id :api :phase :tx-result} (set (keys finished)))
                       (and (= #{:call-id :api :phase :exception-class}
                               (set (keys finished)))
                            (string? (:exception-class finished)))))
        (reject! :invalid-adapter-call-pair [:capture-report :events i])))
    (when-not (= (quot (count events) 2) (count (set (map :call-id (take-nth 2 events)))))
      (reject! :duplicate-adapter-call-identity [:capture-report :events])))
  capture)

(defn- op-class [op]
  (cond
    (and (vector? op) (= :put-docs (first op))
         (= 3 (count op)) (keyword? (second op))
         (plain-map? (nth op 2)) (metadata-free? (nth op 2))
         (contains? (nth op 2) :xt/id)
         (some? (:xt/id (nth op 2)))
         (not (coll? (:xt/id (nth op 2)))))
    :canonical-single-put

    (and (vector? op) (string? (first op))
         (boolean (re-find #"(?is)^\s*ASSERT\b" (first op))))
    :sql-assertion

    :else :unsupported))

(defn- write-like? [op]
  ;; Include malformed put, patch, delete and erase in the write-domain audit.
  ;; Non-ASSERT SQL may be DML: it is always unsupported and never ignored.
  (or (and (vector? op)
           (#{:put-docs :patch-docs :delete-docs :erase-docs}
            (first op)))
      (= :unsupported (op-class op))))

(defn- projection [ops]
  (reduce-kv
   (fn [acc i op]
     (let [kind (op-class op)]
       (cond-> acc
         (= :canonical-single-put kind)
         (update :writes conj {:index i :target [(second op) (:xt/id (nth op 2))]
                               :document (nth op 2)})
         (= :unsupported kind)
         (update :unsupported-operation-indices conj i)
         (write-like? op)
         (update :write-like-indices conj i))))
   {:writes [] :unsupported-operation-indices [] :write-like-indices []}
   ops))

(defn compare-captured-write-domain!
  "Input: {:domain-source <v1412 input>
          :domain-report <matching v1412 report>
          :capture-report <v1390 capture-tx-api-calls! output>}.

   Reprepare the admitted Gesso plan and compare the WHOLE formatted tx-op
   vector, including SQL guard/assertion forms and command payloads, to a
   single captured invocation. Report missing/extra/duplicate write targets,
   unexpected payloads, unsupported operations, partial protection, or
   multiple/absent calls without choosing a favorable subset. The capture is
   supplied by the caller and is NOT authenticated."
  [input]
  (exact-map! input input-keys [])
  (current!)
  (let [{:keys [domain-source domain-report capture-report]} input
        declared (checked-domain! domain-source domain-report)
        capture (checked-capture! capture-report)
        prepared (try
                   (original-prepare nil (:plan domain-source))
                   (catch Exception _
                     (reject! :unsupported-preparation [:domain-source :plan])))
        expected (:tx-ops prepared)
        _ (when-not (and (vector? expected) (<= 1 (count expected) max-ops))
            (reject! :unsupported-prepared-op-vector [:domain-source :plan]))
        _ (when-not (every? #(not= :unsupported (op-class %)) expected)
            (reject! :unsupported-canonical-preparation [:domain-source :plan]))
        expected-projection (projection expected)
        _ (when-not (= (mapv :target (:writes expected-projection))
                         (mapv :target (:declared-command-domains declared)))
            (reject! :prepared-write-domain-disagrees-with-admitted-plan
                     [:domain-source :plan]))
        calls (mapv first (partition 2 (:events capture)))
        one? (= 1 (count calls))
        call (when one? (first calls))
        terminal (when one? (second (:events capture)))
        observed (when one? (:tx-ops call))
        observed-projection (if one? (projection observed)
                                {:writes [] :unsupported-operation-indices []
                                 :write-like-indices []})
        observed-targets (mapv :target (:writes observed-projection))
        expected-targets (mapv :target (:writes expected-projection))
        expected-target-set (set expected-targets)
        observed-target-set (set observed-targets)
        missing-indices (if one?
                          (->> expected-targets
                               (keep-indexed (fn [i target]
                                               (when-not (contains? observed-target-set target) i)))
                               vec)
                          [])
        extra-indices (if one?
                        (->> (:writes observed-projection)
                             (keep (fn [{:keys [index target]}]
                                     (when-not (contains? expected-target-set target)
                                       index))) vec)
                        [])
        duplicate-indices (if one?
                            (->> (:writes observed-projection)
                                 (keep (fn [{:keys [index target]}]
                                         (when (< 1 (count (filter #(= target %) observed-targets)))
                                           index))) vec)
                            [])
        expected-by-target (into {} (map (juxt :target :document)
                                         (:writes expected-projection)))
        payload-mismatch-indices (if one?
                                   (->> (:writes observed-projection)
                                        (keep (fn [{:keys [index target document]}]
                                                (when (and (contains? expected-by-target target)
                                                           (not= document (get expected-by-target target)))
                                                  index))) vec)
                                   [])
        protected-targets (set (map :target (:protected-documents declared)))
        frame-only-targets (set (map :target (:frame-only-protected-documents declared)))
        frame-violating-indices (if one?
                                  (->> (:writes observed-projection)
                                       (keep (fn [{:keys [index target]}]
                                               (when (contains? frame-only-targets target) index)))
                                       vec)
                                  [])
        out-of-protection-indices (if one?
                                    (->> (:writes observed-projection)
                                         (keep (fn [{:keys [index target]}]
                                                 (when-not (contains? protected-targets target) index)))
                                         vec)
                                    [])
        full-vector-match? (and one? (= expected observed)
                                (= {} (:tx-options call)))
        domain-covered? (:declared-replacement-domain-covered? declared)
        matched? (and full-vector-match? domain-covered?
                      (empty? duplicate-indices)
                      (empty? (:unsupported-operation-indices observed-projection)))
        status (cond
                 (zero? (count calls)) :no-adapter-call-observed
                 (not one?) :multiple-adapter-calls-ambiguous
                 (not domain-covered?) :declared-domain-not-fully-protected
                 (not= :returned (:phase terminal)) :adapter-api-threw-outcome-unknown
                 (not= :returned (:outcome capture)) :capture-threw-after-adapter-call
                 matched? :bounded-captured-adapter-operations-agree
                 :else :captured-operations-diverge)]
    {:type report-type
     :version report-version
     :checking :declared-protected-domain-versus-one-caller-supplied-adapter-call
     :source input
     :captured-call-count (count calls)
     :captured-api (when one? (:api call))
     :captured-call-phase (when one? (:phase terminal))
     :declared-target-count (count expected-targets)
     :observed-single-put-count (when one? (count observed-targets))
     :expected-transaction-op-count (count expected)
     :observed-transaction-op-count (when one? (count observed))
     :missing-declared-command-indices missing-indices
     :extra-observed-write-op-indices extra-indices
     :duplicate-observed-write-op-indices duplicate-indices
     :payload-divergence-op-indices payload-mismatch-indices
     :unsupported-observed-op-indices (:unsupported-operation-indices observed-projection)
     :observed-write-like-op-indices (:write-like-indices observed-projection)
     :writes-outside-protection-op-indices out-of-protection-indices
     :frame-only-target-write-op-indices frame-violating-indices
     :captured-transaction-vector-exact? full-vector-match?
     :declared-domain-covered? domain-covered?
     :one-adapter-call-matches-declared-domain? matched?
     :status status
     :actual-xtdb-submission-authenticated? false
     :actual-xtdb-write-footprint-complete? :not-established
     :physical-frame-for-protected-documents :not-established
     :physical-frame-outside-enumeration :not-established
     :transaction-internal-prestate :not-established
     :writer-path-closure :not-established
     :commit-or-reject-outcome :not-established
     :proof-status :bounded-adapter-call-diagnostic-only
     :admission :not-admitted
     :certificate :not-issued
     :transaction-authority :none
     :authority :none}))

(defn require-current-captured-write-domain! [input report]
  "Recompute and compare to reject stale or manipulated diagnostic reports.
   Rechecking never establishes origin or actual physical completeness."
  [input report]
  (let [current (compare-captured-write-domain! input)]
    (when-not (and (plain-map? report) (metadata-free? report)
                   (= current report))
      (reject! :stale-or-forged-comparison-report [:report]))
    current))

(defn redacted-summary [report]
  "Return only categorical and positional results, never document identifiers,
   protected field names, submitted operations, or command arguments."
  (let [current (require-current-captured-write-domain! (:source report) report)]
    (dissoc current :source)))

(ns gesso.assurance.example.bounded-abstract-histories
  "v1503: Enumerate finite ABSTRACT operation histories from Gesso's ORIGINAL
   reservation catalog. No second reservation state machine is authored.

   The source catalog supplies guards, typed literal/read assignments, frames,
   initial condition and invariants. Each generated edge is independently
   checked with the existing abstract transition interpreter. All enabled
   prefixes through the requested depth are enumerated, or the bound rejects;
   the generator never silently truncates its output.

   This is UNADMITTED, copyable test input for later physical replay. It does
   not execute an application, authenticate initial state or operation inputs,
   establish a real history, prove invariant induction, certify an executor,
   or check implementation/XTDB correspondence. Histories contain state
   values: do not publish them; use redacted-summary for diagnostics."
  (:require [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.transition :as construction]))

(def report-type :gesso.assurance.example.bounded-abstract-histories/report)
(def report-version 1)
(def max-depth 8)
(def max-histories 512)

(def ^:private source-keys #{:initial-state :max-depth})
(def ^:private report-keys
  #{:type :version :source :catalog-id :catalog-fingerprint
    :operation-fingerprints :histories :history-count :max-depth
    :exhaustive-within-bound? :classification :initial-condition-satisfied?
    :declared-invariants-preserved-on-enumerated-states?
    :initial-state-origin :physical-replay :writer-coverage
    :proof-status :physical-refinement :certificate :admission :authority
    :open-obligations})
(def ^:private open-obligations
  #{:initial-state-origin-not-authenticated
    :concrete-to-abstract-physical-projection-not-certified
    :implementation-transition-correspondence-not-established
    :actual-xtdb-execution-and-serial-order-not-observed
    :authorization-and-actor-identity-not-established
    :all-physical-writers-not-mediated
    :bounded-enumeration-is-not-general-invariant-proof
    :universal-physical-refinement-not-established})

(defn- reject! [reason & [details]]
  (throw (ex-info "Invalid bounded abstract operation-history generation."
                  (merge {:bounded-abstract-histories/rejection true
                          :reason reason}
                         (or details {})))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map? [x keys*]
  (and (plain-map? x) (nil? (meta x)) (= keys* (set (keys x)))))

(defn- require-source! [source]
  (when-not (and (exact-map? source source-keys)
                 (exact-map? (:initial-state source)
                             #{:slot/status :slot/active-claims :slot/audit-tag})
                 (integer? (:max-depth source))
                 (<= 0 (:max-depth source) max-depth))
    (reject! :invalid-generation-source))
  source)

(defn- catalog-operations! [catalog]
  (let [ops (:operations catalog)
        declared (:declared-operation-ids catalog)
        ids (mapv :id ops)]
    (when-not (and (vector? ops)
                   (seq ops)
                   (= declared (set ids))
                   (= (count ids) (count (set ids))))
      (reject! :incomplete-or-duplicate-operation-catalog))
    (->> ops
         (mapv (fn [{:keys [id] :as op}]
                 (when-not (and (exact-map? op #{:id :construction})
                                (= id (get-in (:construction op) [:subject :id])))
                   (reject! :invalid-original-operation))
                 (let [built (construction/build-guarded-replacement!
                              (:construction op))
                       original (:transition built)
                       described (abstract/describe-transition! original)
                       semantics (:semantics described)]
                   (when-not (and (empty? (:input-types semantics))
                                  (= #{:slot/status :slot/active-claims
                                       :slot/audit-tag}
                                     (set (keys (:state-types semantics)))))
                     (reject! :unsupported-input-or-state-projection
                              {:operation-id id}))
                   {:id id
                    :transition original
                    :semantics semantics
                    :fingerprint (:fingerprint described)})))
         (sort-by (comp str :id))
         vec)))

(defn- typed-state! [state types]
  (let [fields (sort (keys types))
        present-checks (mapv (fn [field] [:present [:read field]]) fields)
        presence-rule {:readers (set fields)
                       :expression (if (= 1 (count fields))
                                     (first present-checks)
                                     (into [:and] present-checks))}]
    ;; Existing typed interpreter validates the complete exact state map,
    ;; including the non-guarded, framed audit field.
    (rule-eval/evaluate-rule! presence-rule types state)
    state))

(defn- require-rule! [rule types state reason]
  (let [readers (:readers rule)
        value (:value (rule-eval/evaluate-rule!
                       rule (select-keys types readers)
                       (select-keys state readers)))]
    (when-not (true? value)
      (reject! reason))
    true))

(defn- checked-state! [state catalog types]
  (typed-state! state types)
  (doseq [[_ invariant] (sort-by (comp str key) (:invariants catalog))]
    (require-rule! invariant types state :declared-invariant-violated))
  state)

(defn- enabled? [{:keys [transition semantics]} state]
  (let [rule (:guard transition)
        readers (:readers rule)]
    (:value (rule-eval/evaluate-rule!
             rule (select-keys (:state-types semantics) readers)
             (select-keys state readers)))))

(defn- applied-state! [{:keys [id transition semantics fingerprint]} state
                       catalog types]
  (let [after (reduce-kv
               (fn [current field [operator operand]]
                 (assoc current field
                        (case operator
                          :literal operand
                          :read (if (contains? state operand)
                                  (get state operand)
                                  (reject! :absent-assignment-reader
                                           {:operation-id id}))
                          (reject! :unsupported-assignment-kind
                                   {:operation-id id}))))
               state (:assignments semantics))
        _ (checked-state! after catalog types)
        observed (abstract/observe-transition! transition state {} after)]
    (when-not (and (:matches? observed)
                   (:guard-holds? observed)
                   (:assignments-hold? observed)
                   (:frame-holds? observed)
                   (= fingerprint (:transition-fingerprint observed)))
      (reject! :source-transition-interpreter-disagreement
               {:operation-id id}))
    {:after after
     :step {:operation-id id
            :transition-fingerprint fingerprint
            :before state
            :after after
            :abstract-observation observed}}))

(defn- enumerate! [catalog operations initial-state depth types]
  ;; Breadth-first, preserving operation-ID order within each frontier.
  ;; Histories are prefixes, including the zero-operation history. Checking
  ;; the count before growing each frontier prevents silent truncation.
  (loop [level 0
         frontier [{:operations [] :states [initial-state] :steps []}]
         histories []]
    (let [all (into histories frontier)]
      (when (> (count all) max-histories)
        (reject! :history-count-limit-exceeded
                 {:maximum max-histories :requested-depth depth}))
      (if (= level depth)
        all
        (let [next-frontier
              (into []
                    (mapcat
                     (fn [{:keys [states steps] :as prefix}]
                       (let [before (peek states)]
                         (for [op operations
                               :when (enabled? op before)
                               :let [{:keys [after step]}
                                     (applied-state! op before catalog types)]]
                           {:operations (conj (:operations prefix) (:id op))
                            :states (conj states after)
                            :steps (conj steps step)})))
                     frontier))]
          (if (empty? next-frontier)
            all
            (recur (inc level) next-frontier all)))))))

(defn- construct! [source]
  (require-source! source)
  (let [catalog (pilot/reservation-catalog)
        ops (catalog-operations! catalog)
        types (get-in (first ops) [:semantics :state-types])
        _ (when-not (every? #(= types (get-in % [:semantics :state-types]))
                            ops)
            (reject! :operation-state-types-disagree))
        initial (typed-state! (:initial-state source) types)
        _ (require-rule! (:initial-condition catalog) types initial
                         :initial-condition-violated)
        _ (checked-state! initial catalog types)
        histories (enumerate! catalog ops initial (:max-depth source) types)
        id (:fingerprint
            (subject/describe-subject
             {:kind :rule
              :id :gesso.assurance.example.bounded-abstract-histories/catalog
              :interface catalog}))]
    {:type report-type
     :version report-version
     :source source
     :catalog-id (:id catalog)
     :catalog-fingerprint id
     :operation-fingerprints (into (sorted-map) (map (juxt :id :fingerprint)) ops)
     :histories histories
     :history-count (count histories)
     :max-depth (:max-depth source)
     :exhaustive-within-bound? true
     :classification :source-derived-bounded-abstract-prefix-enumeration
     :initial-condition-satisfied? true
     :declared-invariants-preserved-on-enumerated-states? true
     :initial-state-origin :caller-supplied-untrusted
     :physical-replay :not-performed
     :writer-coverage :not-established
     :proof-status :bounded-abstract-check-only
     :physical-refinement :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none
     :open-obligations open-obligations}))

(defn generate-histories!
  "Generate ALL enabled operation prefixes through :max-depth from an explicit
   typed :initial-state. Only the original reservation-catalog is interpreted;
   unsupported inputs, changed source, or exceeding resource bounds reject.

   Source shape: {:initial-state {:slot/status :available,
                                  :slot/active-claims 0,
                                  :slot/audit-tag :preserved}
                  :max-depth 4}
   The returned pure histories are suitable as EXPECTED abstract traces for
   separately controlled physical replay, not as replay authorization."
  [source]
  (construct! source))

(defn require-current-histories!
  "Regenerate from the caller's original bounded source and today's original
   catalog. Reject copied/mutated report fields; this is not an origin proof."
  [report]
  (when-not (and (exact-map? report report-keys)
                 (= report (construct! (:source report))))
    (reject! :altered-or-stale-history-report))
  report)

(defn redacted-summary
  "Return counts and scope ONLY; the original report contains business state."
  [report]
  (require-current-histories! report)
  {:type :gesso.assurance.example.bounded-abstract-histories/summary
   :version report-version
   :catalog-id (:catalog-id report)
   :max-depth (:max-depth report)
   :history-count (:history-count report)
   :longest-history-length
   (reduce max 0 (map (comp count :operations) (:histories report)))
   :exhaustive-within-bound? true
   :classification (:classification report)
   :physical-replay :not-performed
   :physical-refinement :not-established
   :proof-status :bounded-abstract-check-only
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

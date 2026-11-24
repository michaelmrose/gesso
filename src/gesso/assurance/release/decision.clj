(ns gesso.assurance.release.decision
  "R5.1 Part XI selected-release *evaluation*, NOT a release credential.

   This strict, opt-in JVM boundary fixes the requested original requirement
   denominator and four different decision subjects BEFORE invoking any gate.
   It rejects a missing or malformed gate, semantic/profile downgrade, an
   unobserved required negative control, and subject changes during checks.

   The caller must supply INDEPENDENT original-source inventory, actual
   current-subject observation, and gate execution. These callbacks are
   trusted inputs to this provisional evaluator: this namespace does not
   independently authenticate their implementations, discover all writers,
   inspect real installed traffic, activate an assembly, or issue a release.
   Its report MUST NOT be used as application permission, a proof certificate,
   an activation capability or an automatically accepted release decision.

   The evaluation is deliberately stateless: every call rechecks the whole
   named scope. A historical green report is not a current authorization."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def phases [:candidate :installation :activation :completion])
(def gate-statuses
  #{:passed :failed :incomplete :unavailable :unsupported
    :skipped :stale :missing})
(def gate-classes
  #{:semantic :structural :proof :physical :browser :runtime
    :product :qualification})
(def ^:private kinds #{:positive :negative})
(def ^:private gate-fields
  #{:id :requirement :kind :class :mechanism :mechanism-version
    :scope :allowed-premises :expected-boundary})
(def ^:private intent-fields
  #{:claim-id :application-id :milestone :profile-id
    :capabilities :requirements :subjects :gates})
(def ^:private inventory-fields #{:capabilities :requirements :gates})
(def report-type :gesso.assurance.release.decision/selected-diagnostic)
(def report-version 1)

(defn- reject! [reason & [extra]]
  (throw (ex-info "Invalid selected release evaluation."
                  (merge {:release-decision/rejection true :reason reason}
                         extra))))

(defn- plain-map? [v]
  (and (or (instance? clojure.lang.PersistentArrayMap v)
           (instance? clojure.lang.PersistentHashMap v))
       (nil? (meta v))))

(defn- name-id? [v]
  (and (keyword? v) (namespace v) (seq (name v))))

(defn- subject-id? [v]
  (and (string? v) (not (str/blank? v)) (<= 3 (count v) 512)))

(defn- finite-set? [x pred]
  (and (set? x) (every? pred x) (nil? (meta x))))

(defn- check-gate! [requirements phase gate]
  (when-not (and (plain-map? gate)
                 (= gate-fields (set (keys gate)))
                 (name-id? (:id gate))
                 (contains? requirements (:requirement gate))
                 (contains? kinds (:kind gate))
                 (contains? gate-classes (:class gate))
                 (name-id? (:mechanism gate))
                 (subject-id? (:mechanism-version gate))
                 (name-id? (:scope gate))
                 (finite-set? (:allowed-premises gate) name-id?)
                 (if (= :negative (:kind gate))
                   (name-id? (:expected-boundary gate))
                   (nil? (:expected-boundary gate))))
    (reject! :invalid-gate-declaration {:phase phase :gate-id (:id gate)}))
  gate)

(defn- check-intent! [intent]
  (when-not (and (plain-map? intent)
                 (= intent-fields (set (keys intent)))
                 (name-id? (:claim-id intent))
                 (name-id? (:application-id intent))
                 (name-id? (:milestone intent))
                 (name-id? (:profile-id intent))
                 (finite-set? (:capabilities intent) name-id?)
                 (seq (:capabilities intent))
                 (plain-map? (:requirements intent))
                 (seq (:requirements intent))
                 (every? name-id? (keys (:requirements intent)))
                 (every? (:capabilities intent) (vals (:requirements intent)))
                 (= (set (vals (:requirements intent))) (:capabilities intent))
                 (plain-map? (:subjects intent))
                 (= (set phases) (set (keys (:subjects intent))))
                 (every? subject-id? (vals (:subjects intent)))
                 (= (count phases) (count (set (vals (:subjects intent)))))
                 (plain-map? (:gates intent))
                 (= (set phases) (set (keys (:gates intent))))
                 (every? #(and (vector? %) (seq %) (nil? (meta %)))
                         (vals (:gates intent))))
    (reject! :invalid-or-incomplete-intent))
  (let [requirements (:requirements intent)
        gates (mapcat (fn [phase]
                        (map (partial check-gate! requirements phase)
                             (get-in intent [:gates phase])))
                      phases)
        ids (map :id gates)]
    (when-not (= (count ids) (count (set ids)))
      (reject! :duplicate-required-gate))
    (when-not (= (set (keys requirements)) (set (map :requirement gates)))
      (reject! :uncovered-original-requirement))
    (when-not (= #{:positive :negative} (set (map :kind gates)))
      (reject! :missing-positive-or-adversarial-control))
    ;; The release has a preselected complete denominator; observations may
    ;; not generate, shrink, or amend it after evaluation begins.
    (assoc intent :gate-ids (set ids))))

(defn- declared-inventory [intent]
  {:capabilities (:capabilities intent)
   :requirements (set (keys (:requirements intent)))
   :gates (:gate-ids intent)})

(defn- source-inventory [discover! intent]
  (try
    (let [inventory (discover!)]
      (cond
        (not (and (plain-map? inventory)
                  (= inventory-fields (set (keys inventory)))
                  (every? #(finite-set? (get inventory %) name-id?)
                          inventory-fields)))
        {:status :invalid-inventory}

        (= inventory (declared-inventory intent))
        {:status :matched}

        :else
        {:status :inventory-diverged
         :missing-capabilities (set/difference (:capabilities intent)
                                               (:capabilities inventory))
         :unexpected-capabilities (set/difference (:capabilities inventory)
                                                  (:capabilities intent))
         :missing-requirements (set/difference
                                (set (keys (:requirements intent)))
                                (:requirements inventory))
         :unexpected-requirements (set/difference
                                   (:requirements inventory)
                                   (set (keys (:requirements intent))))
         :missing-gates (set/difference (:gate-ids intent)
                                        (:gates inventory))
         :unexpected-gates (set/difference (:gates inventory)
                                           (:gate-ids intent))}))
    (catch Throwable error
      (when (instance? InterruptedException error)
        (.interrupt (Thread/currentThread)))
      {:status :inventory-unavailable :error-class (.getName (class error))})))

(defn- current-subject [subject! intent phase]
  (try
    (let [observation (subject! phase)]
      (if (and (plain-map? observation)
               (= #{:phase :claim-id :subject-id} (set (keys observation)))
               (= phase (:phase observation))
               (= (:claim-id intent) (:claim-id observation))
               (= (get-in intent [:subjects phase])
                  (:subject-id observation)))
        {:status :matched :observation observation}
        {:status :subject-diverged}))
    (catch Throwable error
      (when (instance? InterruptedException error)
        (.interrupt (Thread/currentThread)))
      {:status :subject-unavailable :error-class (.getName (class error))})))

(defn- check-observation [intent phase gate subject result]
  (let [status (:status result)]
    (cond
      (not (and (plain-map? result) (contains? gate-statuses status)))
      {:status :invalid-result}

      (not= :passed status)
      ;; No nonpassing status can be silently upgraded by its extra fields.
      {:status status}

      (not (and (= #{:status :claim-id :phase :gate-id :requirement-id
                     :subject-id :mechanism :mechanism-version
                     :scope :evidence-id :premises :observed-boundary
                     :forbidden-effects}
                   (set (keys result)))
                (= (:claim-id intent) (:claim-id result))
                (= phase (:phase result))
                (= (:id gate) (:gate-id result))
                (= (:requirement gate) (:requirement-id result))
                (= (:subject-id subject) (:subject-id result))
                (= (:mechanism gate) (:mechanism result))
                (= (:mechanism-version gate) (:mechanism-version result))
                (= (:scope gate) (:scope result))
                (subject-id? (:evidence-id result))
                (finite-set? (:premises result) name-id?)
                (set/subset? (:premises result) (:allowed-premises gate))
                (if (= :negative (:kind gate))
                  (and (= (:expected-boundary gate)
                          (:observed-boundary result))
                       (= :absent (:forbidden-effects result)))
                  (and (nil? (:observed-boundary result))
                       (nil? (:forbidden-effects result))))))
      {:status :invalid-evidence}

      :else
      {:status :passed
       :evidence-id (:evidence-id result)
       :accepted-premises (:premises result)
       :kind (:kind gate)
       :class (:class gate)})))

(defn- run-check [run-gate! intent phase gate subject]
  (try
    (check-observation
     intent phase gate subject
     (run-gate! {:claim-id (:claim-id intent)
                 :profile-id (:profile-id intent)
                 :phase phase :gate gate :subject subject}))
    (catch Throwable error
      (when (instance? InterruptedException error)
        (.interrupt (Thread/currentThread)))
      {:status :unavailable
       :error-class (.getName (class error))})))

(defn- previous-subjects-current? [subject! intent completed]
  (every? (fn [{:keys [phase subject]}]
            (= {:status :matched :observation subject}
               (current-subject subject! intent phase)))
          completed))

(defn- phase-check [intent subject! run-gate! phase completed]
  (cond
    (not (previous-subjects-current? subject! intent completed))
    {:phase phase :status :blocked
     :blockers {:predecessor :prior-subject-changed} :rows {}}

    :else
    (let [{:keys [status observation] :as current}
          (current-subject subject! intent phase)]
      (if (not= :matched status)
        {:phase phase :status :blocked
         :blockers {:subject status} :rows {}
         :detail (dissoc current :observation)}
        (let [rows
              (into (sorted-map)
                    (for [gate (get-in intent [:gates phase])]
                      [(:id gate) (run-check run-gate! intent phase gate
                                             observation)]))
              after (current-subject subject! intent phase)
              changed? (not= current after)
              blockers (into (sorted-map)
                             (keep (fn [[id row]]
                                     (when-not (= :passed (:status row))
                                       [id (:status row)]))) rows)
              blockers (cond-> blockers
                         changed? (assoc :subject :subject-changed))
              accepted (into #{}
                             (mapcat :accepted-premises)
                             (vals rows))]
          {:phase phase
           :status (if (empty? blockers) :checks-passed-only :blocked)
           :subject observation
           :rows rows :blockers blockers
           :accepted-premises accepted
           :conditional? (boolean (seq accepted))})))))

(defn assess!
  "Run one FULL, fresh, non-authoritative four-decision evaluation.

   Input keys: :intent, :discover! (actual source inventory), :subject!
   (current actual phase subject), :run-gate! (real check/observation).
   All callbacks must be owned by the trusted integrating host; arbitrary
   callbacks can forge evidence and therefore CANNOT issue an installation
   or deployment right. This returns diagnostics ONLY. A separate future
   authority must re-admit genuine original-source, proof and physical
   evidence at the exact installed enforcement boundary.

   All four phases must have predeclared gates. No late-discovered gate can
   reduce the requested denominator. Stop at the first blocked phase; later
   phases are explicitly unexamined, not passed. Reacquire source inventory
   before and after checking, and reacquire subjects on each phase, to detect
   drift in this bounded invocation. Reobserve every passing phase after
   the final inventory read before reporting a fully passing evaluation.
   These reads are not atomic with one another or a real network activation;
   activation is never performed by assess!."
  [{:keys [intent discover! subject! run-gate!] :as options}]
  (when-not (and (plain-map? options)
                 (= #{:intent :discover! :subject! :run-gate!}
                    (set (keys options)))
                 (every? fn? [discover! subject! run-gate!]))
    (reject! :invalid-assessment-input))
  (let [intent (check-intent! intent)
        initial (source-inventory discover! intent)]
    (let [decisions
          (loop [remaining phases
                 completed []
                 stopped? (not= :matched (:status initial))]
            (if-not (seq remaining)
              completed
              (let [phase (first remaining)
                    decision (if stopped?
                               {:phase phase :status :not-examined
                                :blockers {:predecessor :not-qualified}
                                :rows {}}
                               (phase-check intent subject! run-gate! phase
                                            (filter #(= :checks-passed-only
                                                        (:status %))
                                                    completed)))]
                (recur (rest remaining)
                       (conj completed decision)
                       (or stopped?
                           (not= :checks-passed-only (:status decision)))))))
          final-inventory (source-inventory discover! intent)
          stable? (and (= :matched (:status initial))
                       (= initial final-inventory))
          phase-ok? (every? #(= :checks-passed-only (:status %)) decisions)
          ;; A completion gate (or the final inventory callback) may change
          ;; an earlier subject after its last predecessor check. Do not
          ;; treat historical passing rows as a current passing evaluation.
          final-subjects (when (and stable? phase-ok?)
                           (into {} (map (fn [{:keys [phase subject]}]
                                          (let [observed (current-subject subject! intent phase)]
                                            [phase (if (= {:status :matched :observation subject} observed)
                                                     :matched
                                                     (:status observed))]))
                                        decisions)))
          subjects-ok? (and (some? final-subjects)
                            (every? #{:matched} (vals final-subjects)))
          accepted (into #{} (mapcat :accepted-premises) decisions)]
      {:type report-type
       :version report-version
       :claim-id (:claim-id intent)
       :application-id (:application-id intent)
       :milestone (:milestone intent)
       :profile-id (:profile-id intent)
       :requirements (set (keys (:requirements intent)))
       :capabilities (:capabilities intent)
       :required-gates (:gate-ids intent)
       :initial-inventory initial
       :final-inventory final-inventory
       :final-subjects final-subjects
       :decisions (zipmap phases decisions)
       :status (if (and stable? phase-ok? subjects-ok?)
                 :selected-checks-passed-only
                 :blocked)
       :blockers (cond-> {}
                   (not stable?) (assoc :source-inventory :not-current-or-incomplete)
                   (and final-subjects (not subjects-ok?))
                   (assoc :final-subjects (into {} (remove (fn [[_ status]] (= :matched status)))
                                                final-subjects))
                   (not phase-ok?) (assoc :decision
                                          (into {} (keep (fn [row]
                                                           (when (not= :checks-passed-only
                                                                       (:status row))
                                                             [(:phase row) (:blockers row)])))
                                                decisions)))
       :accepted-premises accepted
       :conditional? (boolean (seq accepted))
       :release-authority :none
       :candidate-admission :not-issued
       :installation-eligibility :not-issued
       :activation-eligibility :not-issued
       :completion-claim :not-issued
       :runtime-effect-permission :none})))

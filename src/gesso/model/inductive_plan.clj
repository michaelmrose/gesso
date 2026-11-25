(ns gesso.model.inductive-plan
  "v1290: optional source-bound induction gate when ISSUING a v1251
   authoritative plan executor. This is deliberately not a new executor.

   Supply the ORIGINAL v1286 induction evidence package and the ORIGINAL
   v1251 certified-plan registration source. A plan may be issued through
   this adapter only when its complete guarded replacement construction is
   exactly the one named by the source-checked induction catalog. Compare
   the actual typed transition fingerprints as well as the operation IDs:
   merely using the same operation keyword is not sufficient.

   The existing v1251 builder still owns physical candidate checking, the
   wrapped resolved-operation executor, and the pre-submit boundary. This
   module does not intercept XTDB writes, promise the issued executor is
   installed at the live route, or prove transition refinement. An issued
   function is never a permission to bypass application authorization.
   The optional source recheck is evidence about a trusted supplied snapshot,
   not ongoing runtime enforcement or a certification of all physical writers.

   Within the trusted JVM only, the exact returned function is recorded in a
   weak identity registry. Copying a report or presenting a different issued
   plan executor is not a valid substitute."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gesso.live.outbox :as outbox]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.proof.initial-constructor :as constructor]
            [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.transition :as transition]
            [gesso.assurance.subject :as subject]
            [gesso.assurance.xtdb-envelope :as envelope]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.mapped-preparation :as preparation]
            [gesso.model.mapped-frame :as frame]
            [gesso.model.mapped-projection :as projection]
            [xtdb.util :as xt-util])
  (:import (java.util WeakHashMap)))

(def adapter-version 1)
(def report-type :gesso.model.inductive-plan/source-bound-plan-description)
(def ^:private source-keys #{:inductive-source :certified-plan-source})
(def ^:private inductive-keys
  #{:catalog :certificate-handle :dependency-handle :current-graph})
(def ^:private plan-keys
  #{:certified-source :construction :field-map :command-operation
    :current-document! :transition-inputs! :target!})
(def ^:private additional-open
  #{:runtime-inductive-gate-adoption
    :prestate-to-commit-atomic-guard-enforcement
    :physical-write-path-completeness
    :abstract-to-xtdb-transition-correspondence
    :authorization-and-callback-purity
    :deployed-handler-and-router-adoption
    :induction-checker-independent-soundness})
(def ^:private ^WeakHashMap issued (WeakHashMap.))

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso induction-bound authoritative plan."
                  (merge {:error/type ::invalid-inductive-plan
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [m allowed at]
  (when-not (and (plain-map? m) (nil? (meta m)))
    (reject! :expected-unannotated-source-map at {}))
  (let [actual (set (keys m))]
    (when-not (= allowed actual)
      (reject! :wrong-source-keys at
               {:missing (set/difference allowed actual)
                :unexpected (set/difference actual allowed)})))
  m)

;; Retain original source-derived checkers: redefining an outward-facing Var
;; cannot supply a made-up success report in place of the original verifier.
(def ^:private original-report inductive/build-inductive-report!)
(def ^:private original-operation-set operation-set/check-operation-set!)
(def ^:private original-plan-builder plan/build-certified-plan-operation!)
(def ^:private original-plan-freshness plan/require-current-plan-operation!)

(defn- checked-report! [inductive-source]
  (exact-map! inductive-source inductive-keys [:inductive-source])
  (when-not (every? some? (map inductive-source
                               [:catalog :certificate-handle
                                :dependency-handle :current-graph]))
    (reject! :complete-current-induction-evidence-required
             [:inductive-source] {}))
  (let [report (original-report inductive-source)
        catalog-report (original-operation-set (:catalog inductive-source))]
    (when-not (and (= inductive/report-type (:type report))
                   (= inductive/report-version (:version report))
                   (= :source-bound-conditional-abstract-safety
                      (:verdict report))
                   (= :original-sources-and-both-live-handles-rechecked
                      (:checking report))
                   (true? (:all-declared-invariants-conditionally-inductive?
                           report))
                   (= :checked-exact-source-relative
                      (:source-dependency-closure report))
                   (= :in-process-conditional-abstract-certificate
                      (get-in report [:classification :certificate :status]))
                   (= (:catalog-id report) (:catalog-id catalog-report))
                   (= (get-in report [:identity :catalog-fingerprint])
                      (:fingerprint catalog-report))
                   (= (:declared-operation-ids report)
                      (:declared-operation-ids catalog-report))
                   (= (:declared-invariant-ids report)
                      (:invariant-ids catalog-report))
                   (= :none (:authority report))
                   (= :none (:transaction-authority report)))
      (reject! :incomplete-or-mismatched-induction-evidence
               [:inductive-source] {}))
    {:induction report :catalog catalog-report}))

(defn- checked-plan-source! [source]
  (exact-map! source source-keys [])
  (let [{:keys [inductive-source certified-plan-source]} source
        _ (exact-map! certified-plan-source plan-keys
                      [:certified-plan-source])
        {:keys [induction catalog]} (checked-report! inductive-source)
        operation (get-in certified-plan-source [:construction :subject :id])
        entries (:operations (:catalog inductive-source))
        matching (filter #(= operation (:id %)) entries)
        row (first (filter #(= operation (:operation-id %))
                           (:operation-results catalog)))]
    (when-not (and (keyword? operation) (some? (namespace operation))
                   (= 1 (count matching))
                   (contains? (:declared-operation-ids catalog) operation)
                   (some? row))
      (reject! :operation-not-in-inductive-catalog
               [:certified-plan-source :construction :subject] {}))
    (when-not (= (:construction (first matching))
                 (:construction certified-plan-source))
      (reject! :plan-abstract-construction-mismatch
               [:certified-plan-source :construction]
               {:operation operation}))
    ;; v1251's eventual builder separately checks proof source, guards,
    ;; callback registration, field mapping, and the real pre-submit gate.
    (let [built (transition/build-guarded-replacement!
                 (:construction (first matching)))
          registered-operation (get-in certified-plan-source
                                       [:certified-source :operation])
          proof-transition (get-in certified-plan-source
                                   [:certified-source :proof-source :transition])]
      (when-not (and (= registered-operation operation)
                     (= (:transition built) proof-transition)
                     (= (:transition-fingerprint built)
                        (:transition-fingerprint row))
                     (= (:subject-fingerprint built)
                        (:subject-fingerprint row)))
        (reject! :registered-plan-transition-mismatch
                 [:certified-plan-source :certified-source]
                 {:operation operation}))
      {:operation operation
       :transition-fingerprint (:transition-fingerprint built)
       :subject-fingerprint (:subject-fingerprint built)
       :catalog-fingerprint (:fingerprint catalog)
       :induction-fingerprint (get-in induction
                                      [:identity :induction-fingerprint])
       :certificate-fingerprint
       (get-in induction [:identity :certificate-fingerprint])
       :dependency-fingerprint
       (get-in induction [:identity :dependency-fingerprint])
       :catalog-operation-ids (:declared-operation-ids catalog)
       :invariant-ids (:declared-invariant-ids induction)
       :original-open-obligations
       (get-in induction [:classification :original-obligations :by-operation])
       :open-premises
       (get-in induction [:classification :certificate :open-premises])
       :open-obligations
       (set/union additional-open
                  (get-in induction [:classification :open :assumptions]))})))

(defn- description [checked plan-description]
  (when-not (and (= plan/description-type (:type plan-description))
                 (= plan/plan-version (:version plan-description))
                 (= (:operation checked) (:operation plan-description))
                 (= (:transition-fingerprint checked)
                    (:transition-fingerprint plan-description))
                 (= :trusted-observation-only
                    (:prestate-assurance plan-description))
                 (= :not-established (:atomicity-assurance plan-description))
                 (= :not-proved (:invariant-preservation plan-description))
                 (= :not-established (:physical-correspondence
                                     plan-description))
                 (= :none (:authority plan-description)))
    (reject! :issued-plan-description-mismatch [:executor] {}))
  {:type report-type
   :version adapter-version
   :checking :original-induction-and-exact-plan-transition-rechecked
   :operation (:operation checked)
   :catalog-operation-ids (:catalog-operation-ids checked)
   :invariant-ids (:invariant-ids checked)
   :catalog-fingerprint (:catalog-fingerprint checked)
   :induction-fingerprint (:induction-fingerprint checked)
   :certificate-fingerprint (:certificate-fingerprint checked)
   :dependency-fingerprint (:dependency-fingerprint checked)
   :transition-fingerprint (:transition-fingerprint checked)
   :subject-fingerprint (:subject-fingerprint checked)
   :field-map (:field-map plan-description)
   :command-operation (:command-operation plan-description)
   :plan-gate :original-v1251-pre-submit-checker
   :source-join :exact-guarded-transition-construction
   :scope :one-registered-plan-of-declared-abstract-machine
   :original-open-obligations (:original-open-obligations checked)
   :open-premises (set/union (:open-premises checked)
                             (:open-premises plan-description))
   :open-obligations (set/union (:open-obligations checked)
                                (:open-obligations plan-description))
   :inductive-invariant :conditionally-checked-in-declared-abstract-machine
   :prestate-assurance :trusted-observation-only
   :runtime-induction-gating :not-established
   :actual-router-adoption :not-established
   :atomicity-assurance :not-established
   :physical-correspondence :not-established
   :physical-write-path-completeness :not-established
   :transaction-authority :none
   :authority :none})

(defn build-inductive-plan-operation!
  "Optionally issue a v1251 authoritative executor only after the entire
   induction evidence and THIS operation's exact original construction match.
   The returned callable is the genuine v1251 plan executor, NOT a new wrapper
   or a proof-backed transaction authorization."
  [source]
  (let [checked (checked-plan-source! source)
        executor (original-plan-builder (:certified-plan-source source))
        info (description checked (plan/describe-plan-operation executor))]
    (locking issued (.put issued executor {:source source :description info}))
    executor))

(defn inductive-plan-operation? [executor]
  (boolean (and (fn? executor)
                (locking issued (.get issued executor)))))

(defn- registered! [executor]
  (or (when (fn? executor) (locking issued (.get issued executor)))
      (reject! :unregistered-inductive-plan [:executor] {})))

(defn describe-inductive-plan [executor]
  (let [{:keys [description]} (registered! executor)]
    ;; Copyable, display-only information; not a freshness witness.
    description))

(defn require-current-inductive-plan!
  "Recheck the ORIGINAL induction source and v1251 plan source against the
   same issued executor. The supplied current source must be obtained from
   the CURRENT trusted assembly. Merely replaying a cached source does not
   prove the deployed router or current database state."
  [executor current-source]
  (let [{original :source prior :description} (registered! executor)]
    (exact-map! current-source source-keys [:current-source])
    (let [old (:inductive-source original)
          now (:inductive-source current-source)]
      (exact-map! now inductive-keys [:current-source :inductive-source])
      (when-not (and (= (:catalog old) (:catalog now))
                     (= (:current-graph old) (:current-graph now))
                     (identical? (:certificate-handle old)
                                 (:certificate-handle now))
                     (identical? (:dependency-handle old)
                                 (:dependency-handle now)))
        (reject! :changed-induction-registration
                 [:current-source :inductive-source] {})))
    (let [checked (checked-plan-source! current-source)]
      (original-plan-freshness executor
                               (:certified-plan-source current-source))
      (let [fresh (description checked
                               (plan/describe-plan-operation executor))]
        (when-not (= prior fresh)
          (reject! :stale-inductive-plan [:current-source] {}))
        fresh))))

(def ^:private original-target-mapping plan/check-target-mapping!)

(defn check-inductive-target-mapping!
  "Recheck both live abstract handles and the exact shared target projection
   against EVERY original catalog construction. No executor is issued."
  [{:keys [inductive-source target-map] :as source}]
  (exact-map! source #{:inductive-source :target-map} [])
  (when-not (identical? original-target-mapping plan/check-target-mapping!)
    (reject! :substituted-target-mapping-checker [] {}))
  (let [{:keys [induction catalog]} (checked-report! inductive-source)
        rows (into {} (map (juxt :operation-id identity)) (:operation-results catalog))
        mappings (into {} (map (fn [{:keys [id construction]}]
                                (let [checked (original-target-mapping construction target-map)
                                      row (get rows id)]
                                  (when-not (and (= (:transition-fingerprint checked) (:transition-fingerprint row))
                                                 (= (:subject-fingerprint checked) (:subject-fingerprint row)))
                                    (reject! :mapped-transition-mismatch [:target-map] {}))
                                  [id checked]))) (:operations (:catalog inductive-source)))]
    {:type ::inductive-target-mapping :version 1
     :catalog-fingerprint (:fingerprint catalog)
     :induction-fingerprint (get-in induction [:identity :induction-fingerprint])
     :target-map target-map :operation-mappings mappings
     :original-open-obligations (get-in induction [:classification :original-obligations :by-operation])
     :scope :declared-abstract-field-projection-only
     :executor :not-issued :physical-correspondence :not-established
     :certificate :not-issued :authority :none}))

(def ^:private original-mapped-check check-inductive-target-mapping!)
(def ^:private original-mapped-builder preparation/build-admission!)
(def ^:private ^WeakHashMap issued-mapped (WeakHashMap.))

(def ^:private mapped-source-keys
  #{:inductive-source :target-map :decode-outcome! :transaction-footprint! :auxiliary-tables :document-effects})

(defn- mapped-source-keys! [source]
  (exact-map! source (cond-> mapped-source-keys
                       (contains? source :input-derivations) (conj :input-derivations)
                       (contains? source :publication-derivations) (conj :publication-derivations)) []))

(defn- check-input-derivations! [catalog source]
  (let [operations (:operations catalog)
        derivations (if (contains? source :input-derivations)
                      (:input-derivations source)
                      (zipmap (map :id operations) (repeat {})))]
    (when-not (and (plain-map? derivations) (nil? (meta derivations))
                   (= (set (map :id operations)) (set (keys derivations))))
      (reject! :incomplete-mapped-input-derivations [] {}))
    (doseq [{:keys [id construction]} operations
            :let [selectors (get derivations id)]]
      (when-not (and (plain-map? selectors) (nil? (meta selectors))
                     (= (set (keys (:input-types construction))) (set (keys selectors))))
        (reject! :incomplete-mapped-input-derivations [id] {}))
      (doseq [[input selector] selectors]
        (when-not (and (vector? selector) (nil? (meta selector))
                       (case (first selector)
                         :argument (and (= 2 (count selector)) (keyword? (second selector)))
                         :state (and (= 2 (count selector)) (contains? (:state-types construction) (second selector)))
                         :principal (= [:principal] selector)
                         false))
          (reject! :invalid-mapped-input-selector [id input] {}))))
    derivations))

(defn- derive-inputs! [construction selectors binding before]
  (let [binding (resolution/require-binding binding)
        inputs (into {} (map (fn [[input [kind k]]]
                               (let [value (case kind
                                             :principal (identity/raw-value (:principal binding))
                                             :argument (if (contains? (:arguments binding) k)
                                                         (get (:arguments binding) k)
                                                         (reject! :missing-mapped-input-source [input] {}))
                                             :state (if (contains? before k) (get before k)
                                                        (reject! :missing-mapped-input-source [input] {})))]
                                 [input value]))) selectors)]
    (values/observe-state! (:input-types construction) inputs)
    inputs))

(defn- require-derived-inputs! [construction selectors binding before supplied]
  (when-not (= supplied (derive-inputs! construction selectors binding before))
    (reject! :mapped-input-derivation-mismatch [] {}))
  nil)

(defn- check-publication-derivations! [mappings source]
  (when (contains? source :publication-derivations)
    (let [declarations (:publication-derivations source)]
      (exact-map! declarations (set (keys mappings)) [:publication-derivations])
      (doseq [[alternative declaration] declarations]
        (when (some? declaration)
          (let [at [:publication-derivations alternative]
                _ (exact-map! declaration #{:purpose :contributions} at)
                {:keys [purpose contributions]} declaration
                targets (into #{} (filter #(= :document (first %)))
                              (get-in mappings [alternative :write-targets]))]
            (when-not (and (contains? (:auxiliary-tables source) outbox/intent-table)
                           (qualified-keyword? purpose) (vector? contributions)
                           (nil? (meta contributions)) (<= 1 (count contributions) 16384))
              (reject! :invalid-publication-derivation at {}))
            (doseq [entry contributions]
              (exact-map! entry #{:target :topic :field} at)
              (when-not (and (contains? targets (:target entry))
                             (keyword? (:topic entry)) (keyword? (:field entry)))
                (reject! :invalid-publication-contribution at {})))
            (when-not (and (= targets (set (map :target contributions)))
                           (= (count contributions) (count (set (map (juxt :target :topic) contributions))))
                           (every? #(apply = (map :field %)) (vals (group-by :topic contributions))))
              (reject! :incomplete-or-ambiguous-publication-contributions at {})))))
      declarations)))

(defn- checked-mapped-source! [source]
  (mapped-source-keys! source)
  (when-not (and (fn? (:decode-outcome! source)) (fn? (:transaction-footprint! source))
                 (identical? original-mapped-check check-inductive-target-mapping!)
                 (identical? original-mapped-builder preparation/build-admission!))
    (reject! :invalid-mapped-admission-checker-or-decoder [] {}))
  (let [checked (original-mapped-check (select-keys source [:inductive-source :target-map]))
        mapped-tables (into #{:gesso_commands}
                            (keep (fn [[kind table]] (when (#{:document :nullable-document} kind) table)))
                            (vals (:target-map checked)))
        auxiliary (:auxiliary-tables source)]
    (when-not (and (set? auxiliary)
                   (every? #(and (keyword? %) (nil? (namespace %))
                                 (re-matches #"[a-z][a-z0-9_]*" (name %))) auxiliary)
                   (empty? (set/intersection auxiliary mapped-tables)))
      (reject! :invalid-auxiliary-tables [:auxiliary-tables] {}))
    (let [effects (:document-effects source)
          mappings (:operation-mappings checked)]
      (when-not (and (map? effects) (= (set (keys mappings)) (set (keys effects))))
        (reject! :incomplete-document-effect-catalog [:document-effects] {}))
      (doseq [[id mapping] mappings] (frame/check-effects! mapping (get effects id)))
      (cond-> (assoc checked :auxiliary-tables auxiliary :document-effects effects
                     :input-derivations (check-input-derivations! (get-in source [:inductive-source :catalog]) source))
        (contains? source :publication-derivations)
        (assoc :publication-derivations (check-publication-derivations! mappings source))))))

(defn- inspect-transaction!
  [footprint! auxiliary {:keys [api tx-ops tx-options] :as submission}]
  (exact-map! submission #{:api :tx-ops :tx-options} [:submission])
  (when-not (and (= :execute-tx api) (or (nil? tx-options) (= {} tx-options))
                 (vector? tx-ops) (every? vector? tx-ops))
    (reject! :unsupported-mapped-submission [:submission] {}))
  (let [[guards puts] (split-with #(string? (first %)) tx-ops)]
    (when-not (and (seq guards) (seq puts)
                   (every? #(and (re-find #"(?is)^\s*ASSERT\s+" (first %))
                                 (not (re-find #";" (first %)))) guards)
                   (every? (fn [[op table & docs]]
                             (and (= :put-docs op) (keyword? table)
                                  (seq docs) (every? map? docs))) puts))
      (reject! :unsupported-mapped-transaction [:submission :tx-ops] {}))
    ;; The trusted footprint checker receives the entire original vector.
    ;; It identifies the command/alternative; it cannot choose which puts
    ;; reach the mapped checker. Only declared disjoint auxiliary tables
    ;; are excluded by the issuer itself.
    (let [{:keys [alternative command-id] :as identity} (footprint! tx-ops)]
      (exact-map! identity #{:alternative :command-id} [:footprint])
      (when-not (and (qualified-keyword? alternative) (string? command-id) (seq command-id))
        (reject! :invalid-transaction-identity [:footprint] {}))
      {:identity identity
       :puts (filterv #(not (contains? auxiliary (second %))) puts)})))

(defn- admit-transaction! [registry footprint! auxiliary submission]
  (let [{:keys [identity puts]} (inspect-transaction! footprint! auxiliary submission)]
    {:identity identity
     :candidate ((:admit! registry) (:alternative identity) (:command-id identity) puts)}))

(defn- frame-submission! [registry footprint! auxiliary submission]
  (let [{:keys [identity]} (inspect-transaction! footprint! auxiliary submission)
        prefix ((:frame-guards! registry) (:alternative identity) (:command-id identity))]
    (update submission :tx-ops #(into prefix %))))

(defn- admit-framed-transaction! [registry footprint! auxiliary original actual & [check-publication!]]
  ;; Only the unextended canonical transaction goes to the trusted footprint
  ;; checker. The extra SQL prefix has its own exact source-derived check.
  ;; No ordinary predicate, write, option or API may change in transit.
  (let [{:keys [identity puts]} (inspect-transaction! footprint! auxiliary original)
        base (:tx-ops original)
        ops (:tx-ops actual)]
    (exact-map! actual #{:api :tx-ops :tx-options} [:submission])
    (when-not (and (= (dissoc original :tx-ops) (dissoc actual :tx-ops))
                   (vector? ops) (<= (count base) (count ops))
                   (= base (subvec ops (- (count ops) (count base)))))
      (reject! :changed-framed-transaction [:submission] {}))
    {:identity identity
     :candidate ((:admit-framed! registry) (:alternative identity) (:command-id identity)
                 puts (subvec ops 0 (- (count ops) (count base)))
                 (when check-publication! (partial check-publication! identity base)))}))

(defn- mapped-description [checked]
  (assoc checked :type ::issued-mapped-admission
                 :admission :issued-local-preparation-registry
                 :checking :live-induction-handles-and-original-mappings-rechecked-at-issuance
                 :transaction-admission :exact-envelope-with-trusted-footprint-checker
                 :runtime-induction-gating :not-established
                 :transaction-authority :none))

(declare reconstruct-mapped-publication!)

(defn- check-candidate-publication! [admission source-id alternative ops before puts record]
  (when-not (and (string? source-id) (not (str/blank? source-id)))
    (reject! :invalid-publication-source [] {}))
  ;; All replacements have already passed target, relation and frame checks.
  ;; Use the exact consumed preparation, never a fresh observation or the
  ;; constructor's claimed before rows or change list.
  (let [after (reduce (fn [docs [_ table & rows]]
                        (if (= :gesso_commands table)
                          docs
                          (reduce (fn [d row]
                                    (update d table #(conj (filterv (fn [prior] (not= (:xt/id prior) (:xt/id row))) %) row)))
                                  docs rows))) before puts)
        {:keys [status intents]} (reconstruct-mapped-publication! admission alternative source-id record before after)
        actual (mapcat #(drop 2 %) (filter #(and (= :put-docs (first %)) (= outbox/intent-table (second %))) ops))
        guard-sql "ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)"
        guards (frequencies (map #(vector guard-sql (:xt/id %)) intents))
        observed-guards #(frequencies (filter (fn [op] (= guard-sql (first op))) %))]
    (when-not (= :derived status)
      (reject! :invalid-candidate-publication-expectation [] {}))
    (when-not (= (frequencies intents) (frequencies actual))
      (reject! :candidate-publication-expectation-mismatch [] {}))
    (when-not (= guards (observed-guards (take-while #(string? (first %)) ops)) (observed-guards ops))
      (reject! :candidate-publication-guard-mismatch [] {}))
    :matched))

(defn build-inductive-mapped-admission!
  "Issue a local callable preparation registry from the original catalog and
   both live induction handles. Admit only a complete synchronous transaction
   envelope via the captured trusted footprint checker and mapped registry.
   :frame-submission! constructs a separate, generated pre-write prefix
   over a canonical envelope without consuming its preparation.
   :admit-framed! takes the original and actual envelopes, checks their exact
   relationship and consumes the preparation with the exact prefix check.
   :admit-framed-publication! additionally checks declared publication
   against the consumed preparation and the actual complete candidate.
   The older :admit! is unframed; no path submits a transaction.
   Input-bearing alternatives require closed :input-derivations selectors
   for every declared input. Input-free catalogs may omit that key. The
   supplied preparation inputs must equal the derived typed valuation.
   The decoder, acquisition and footprint checker are trusted; no general SQL
   refinement or installed executor correspondence is established. Calls do not recheck the
   induction proof; use require-current-mapped-admission! at assembly review."
  [source]
  (let [checked (checked-mapped-source! source)
        registry (original-mapped-builder (get-in source [:inductive-source :catalog])
                                          (:target-map source) (:decode-outcome! source) (:document-effects source))
        footprint! (:transaction-footprint! source)
        auxiliary (:auxiliary-tables source)
        operations (into {} (map (juxt :id :construction)) (get-in source [:inductive-source :catalog :operations]))
        prepare! (fn [id binding snapshot inputs]
                   (let [original (or (get operations id) (reject! :unknown-operation [id] {}))]
                     (require-derived-inputs! original (get-in checked [:input-derivations id])
                                              binding (:state snapshot) inputs)
                     ;; The registry independently projects the supplied rows and
                     ;; checks the binding before storing this exact preparation.
                     ((:prepare! registry) id binding snapshot inputs)))
        admission (fn admission [action & args]
                    (case action
                      :admit! (apply admit-transaction! registry footprint! auxiliary args)
                      :frame-submission! (apply frame-submission! registry footprint! auxiliary args)
                      :admit-framed!
                      (do (when-not (= 2 (count args))
                            (reject! :invalid-framed-admission-arguments [] {}))
                          (apply admit-framed-transaction! registry footprint! auxiliary args))
                      :admit-framed-publication!
                      (let [[original actual source-id] args]
                        (when-not (= 3 (count args))
                          (reject! :invalid-publication-admission-arguments [] {}))
                        (admit-framed-transaction! registry footprint! auxiliary original actual
                          (when (contains? checked :publication-derivations)
                            (fn [identity ops before puts record]
                              (check-candidate-publication! admission source-id (:alternative identity)
                                                            ops before puts record)))))
                      :prepare! (apply prepare! args)
                      (:discard! :pending?) (apply (get registry action) args)
                      (reject! :unknown-mapped-admission-action [action] {})))]
    (locking issued-mapped
      (.put issued-mapped admission {:source source :description (mapped-description checked)}))
    admission))

(defn- registered-mapped! [admission]
  (or (when (fn? admission) (locking issued-mapped (.get issued-mapped admission)))
      (reject! :unregistered-mapped-admission [:admission] {})))

(defn describe-mapped-admission [admission]
  (:description (registered-mapped! admission)))

(def ^:private original-initial-constructor constructor/check-initial-constructor!)
(def ^:private original-initial-projection projection/project-state!)
(def ^:private original-initial-identity subject/describe-subject)

(defn check-mapped-initialization!
  "Check the supplied seed against the original issued catalog's shared Init
   and every invariant. Project all mapped tables (including absent nullable
   targets) and empty journal slots. The host must atomically guard emptiness
   of every returned table. This is a concrete constructor check, not a
   physical refinement proof or permission to submit a transaction."
  [admission seed]
  (let [{:keys [source description]} (registered-mapped! admission)
        {:keys [target-map decode-outcome!]} source
        catalog (get-in source [:inductive-source :catalog])
        construction (:construction (first (:operations catalog)))
        tables (into #{} (keep (fn [[kind table]]
                                 (when (#{:document :nullable-document} kind) table)))
                     (vals target-map))]
    (when-not (and (identical? original-initial-constructor constructor/check-initial-constructor!)
                   (identical? original-initial-projection projection/project-state!)
                   (identical? original-initial-identity subject/describe-subject))
      (reject! :substituted-initialization-checker [] {}))
    (when-not (and (vector? seed)
                   (every? (fn [[op table & rows :as entry]]
                             (and (vector? entry) (= :put-docs op)
                                  (simple-keyword? table)
                                  (re-matches #"[a-z][a-z0-9_]*" (name table))
                                  (not= :gesso_commands table)
                                  (seq rows) (every? map? rows))) seed))
      (reject! :invalid-mapped-initializer [] {}))
    (doseq [[_ table & rows] seed row rows]
      (when-not (and (string? (:xt/id row)) (seq (:xt/id row))
                     (every? keyword? (keys row)))
        (reject! :invalid-initial-document [table] {}))
      (let [columns (map xt-util/kw->normal-form-kw (keys row))]
        (when-not (= (count columns) (count (set columns)))
          (reject! :aliased-initial-document-key [table] {}))
        (when (some #(and (not= % (xt-util/kw->normal-form-kw :xt/id))
                          (or (.startsWith (name %) "xt$") (.startsWith (name %) "_"))) columns)
          (reject! :reserved-initial-document-key [table] {}))))
    (let [entries (mapcat (fn [[_ table & rows]] (map #(vector table %) rows)) seed)
          addresses (map (fn [[table row]] [table (:xt/id row)]) entries)
          _ (when-not (= (count addresses) (count (distinct addresses)))
              (reject! :duplicate-initial-document [] {}))
          docs (reduce (fn [m [table row]]
                         (if (contains? tables table) (update m table conj row) m))
                       (zipmap tables (repeat [])) entries)
          state (original-initial-projection construction target-map docs [] decode-outcome!)
          ;; Present nil fields have no established XTDB readback correspondence.
          _ (doseq [[_ [kind table id field]] target-map
                    :when (#{:document :nullable-document} kind)
                    row (get docs table) :when (= id (:xt/id row))]
              (when (nil? (get row (keyword (.replace (name field) "_" "-"))))
                (reject! :unsupported-null-initial-field [table id field] {})))
          checked (original-initial-constructor
                   (assoc (select-keys catalog [:initial-condition :invariants])
                          :construction construction)
                   {:id ::mapped-initializer
                    :fields (into {} (map (fn [[k v]] [k [:literal v]])) state)})]
      (when-not (and (= state (:constructed-state checked))
                     (true? (:initial-condition-holds? checked))
                     (true? (:all-declared-invariants-hold? checked)))
        (reject! :initial-invariant-violation [] {}))
      (let [binding {:catalog-fingerprint (:catalog-fingerprint description)
                     :case-fingerprint (:fingerprint checked)}
            fingerprint (:fingerprint
                         (original-initial-identity
                          {:kind :rule :id ::mapped-initializer-binding
                           :interface (assoc binding :target-map target-map :seed seed)}))]
        (merge binding {:tables tables :state state :initialization-fingerprint fingerprint
                        :authority :none :physical-correspondence :not-established})))))

(defn check-mapped-initialization-readback!
  "Compare supplied physical readback rows with the checked concrete initial
   constructor using the same original projection and issued mapping. The
   host supplies pinned rows; this function does not authenticate acquisition
   or prove physical refinement for arbitrary transactions."
  [admission seed document-rows journal-rows]
  (let [checked (check-mapped-initialization! admission seed)
        {:keys [source]} (registered-mapped! admission)
        {:keys [target-map decode-outcome!]} source
        construction (get-in source [:inductive-source :catalog :operations 0 :construction])
        actual (original-initial-projection construction target-map document-rows journal-rows decode-outcome!)]
    (when-not (and (= [] journal-rows) (= (:state checked) actual))
      (reject! :initialization-readback-state-mismatch [] {}))
    {:state actual :initialization-fingerprint (:initialization-fingerprint checked)
     :authority :none :physical-correspondence :checked-supplied-projection}))

;; String-vector entries avoid XTDB document-key normalization and preserve
;; absence, present nil, false and arbitrary admitted integral values.
(defn- encode-valuation [valuation]
  (when-not (and (map? valuation) (<= (count valuation) 64)
                 (every? #(and (qualified-keyword? %)
                               (<= (count (namespace %)) 2048)
                               (<= (count (name %)) 2048)) (keys valuation)))
    (reject! :invalid-transition-witness [] {}))
  (mapv (fn [[k v]]
          (into [(namespace k) (name k)]
                (case (values/value-kind! v)
                  :nil ["nil"]
                  :boolean ["boolean" (str v)]
                  :integer ["integer" (str v)]
                  :string ["string" v]
                  :keyword (if-let [ns (namespace v)]
                             ["qualified-keyword" ns (name v)]
                             ["keyword" (name v)]))))
        (sort-by (comp (juxt namespace name) key) valuation)))

(defn- decode-valuation! [encoded]
  (when-not (and (vector? encoded) (<= (count encoded) 64)
                 (every? #(and (vector? %) (<= 3 (count %) 5)
                               (every? (fn [s] (and (string? s) (<= (count s) 2048))) %)) encoded))
    (reject! :invalid-transition-witness [] {}))
  (let [decoded (into {} (map (fn [[ns k tag a b]]
                               [(keyword ns k)
                                (case tag
                                  "nil" nil
                                  "boolean" (case a "true" true "false" false
                                                    (reject! :invalid-transition-witness [] {}))
                                  "integer" (try (bigint a)
                                                 (catch Exception _ (reject! :invalid-transition-witness [] {})))
                                  "string" a
                                  "qualified-keyword" (keyword a b)
                                  "keyword" (keyword nil a)
                                  (reject! :invalid-transition-witness [] {}))])) encoded)]
    (when-not (= encoded (encode-valuation decoded))
      (reject! :invalid-transition-witness [] {}))
    decoded))

(def ^:private original-witness-relation projection/check-transition-relation!)

(defn- current-guard-evidence? [evidence]
  (and (= frame/guard-profile (:guard-profile evidence))
       (string? (:guard-prefix-fingerprint evidence))
       (re-matches #"sha256:[0-9a-f]{64}" (:guard-prefix-fingerprint evidence))))

(defn- transition-evidence [description alternative mapping command-id before inputs state guards]
  {:xt/id command-id :profile-version 4
   :guard-profile (:guard-profile guards)
   :guard-prefix-fingerprint (:guard-prefix-fingerprint guards)
   :catalog-fingerprint (:catalog-fingerprint description)
   :induction-fingerprint (:induction-fingerprint description)
   :mapping-fingerprint (:fingerprint mapping)
   :transition-fingerprint (:transition-fingerprint mapping)
   :alternative alternative
   :input-derivation-fingerprint (:fingerprint
                                  (original-initial-identity
                                   {:kind :rule :id ::mapped-input-derivation
                                    :interface {:selectors (get-in description [:input-derivations alternative])}}))
   :prestate-witness (encode-valuation before)
   :input-witness (encode-valuation inputs)
   :witness-fingerprint (:fingerprint
                         (original-initial-identity
                          {:kind :rule :id ::mapped-transition-witness
                           :interface {:before before :inputs inputs :after state}}))
   :poststate-fingerprint (:fingerprint
                          (original-initial-identity
                           {:kind :rule :id ::mapped-poststate
                            :interface {:state state}}))})

(defn mapped-transition-evidence!
  "Describe the checked abstract witness of an admitted framed candidate.
   The prefix identity describes the exact checked SQL and bound values.
   Its prestate and inputs are supplied observations, not physical proof.
   The executor must bind it to the same atomic transaction as the command journal."
  [admission {:keys [identity candidate]}]
  (let [description (describe-mapped-admission admission)
        alternative (:alternative identity)
        mapping (get-in description [:operation-mappings alternative])]
    (when-not (and mapping (string? (:command-id identity))
                   (= :exact-generated-prefix (:frame-prefix candidate))
                   (current-guard-evidence? candidate)
                   (= (:fingerprint mapping) (:mapping-fingerprint candidate))
                   (= (:transition-fingerprint mapping) (:transition-fingerprint candidate))
                   (map? (:before candidate)) (map? (:inputs candidate)) (map? (:after candidate))
                   (identical? original-initial-identity subject/describe-subject))
      (reject! :invalid-mapped-transition-evidence [] {}))
    (transition-evidence description alternative mapping (:command-id identity)
                         (:before candidate) (:inputs candidate) (:after candidate) candidate)))

(defn check-historical-mapped-poststate!
  "Recompute the expected evidence from the original mapping and supplied
   historical rows. The caller must establish the exact command commit time
   and same-transaction evidence. Re-evaluate the relation over the saved
   abstract prestate and inputs; their physical provenance is not established.
   The guard fingerprint is only shape-checked here; check-mapped-prestate!
   must separately reconstruct it from preceding rows."
  [admission record evidence document-rows journal-rows]
  (let [{:keys [source description]} (registered-mapped! admission)
        alternative (:alternative evidence)
        mapping (get-in description [:operation-mappings alternative])
        original (some (fn [{:keys [id construction]}]
                         (when (= id alternative) construction))
                       (get-in source [:inductive-source :catalog :operations]))
        binding (:gesso.command/binding record)
        slot [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]]
    (when-not (and original mapping (string? (:xt/id record))
                   (contains? (:write-targets mapping) slot)
                   (some #(= record %) journal-rows)
                   (= 4 (:profile-version evidence))
                   (not= :invalid (envelope/binding-status evidence))
                   (current-guard-evidence? evidence)
                   (identical? original-witness-relation projection/check-transition-relation!)
                   (identical? original-initial-projection projection/project-state!)
                   (identical? original-initial-identity subject/describe-subject))
      (reject! :invalid-historical-mapped-evidence [] {}))
    (let [actual (original-initial-projection original (:target-map source)
                                               document-rows journal-rows (:decode-outcome! source))
          before (decode-valuation! (:prestate-witness evidence))
          inputs (decode-valuation! (:input-witness evidence))]
      (when-not (= (apply dissoc evidence envelope/binding-keys)
                   (transition-evidence description alternative mapping (:xt/id record) before inputs actual evidence))
        (reject! :historical-mapped-poststate-mismatch [] {}))
      (require-derived-inputs! original (get-in description [:input-derivations alternative]) binding before inputs)
      (original-witness-relation original before inputs actual)
      {:authority :none :physical-correspondence :checked-historical-projection
       :abstract-relation :checked-supplied-witness
       :input-correspondence :checked-command-and-prestate-selectors
       :expected-envelope (envelope/binding-status evidence)})))

(defn reconstruct-historical-mapped-writes!
  "Internal: select complete replacement rows for exactly the admitted
   write targets from pinned poststate, checking the semantic relation and
   document frame against pinned prestate. Return a selected-table put
   expectation, not authority or a claim about auxiliary writes."
  [admission record evidence before-docs after-docs journal-rows]
  (check-historical-mapped-poststate! admission record evidence after-docs journal-rows)
  (let [{:keys [description]} (registered-mapped! admission)
        alternative (:alternative evidence)
        mapping (get-in description [:operation-mappings alternative])
        targets (:write-targets mapping)
        binding (:gesso.command/binding (resolution/require-record record))
        slot [:journal-slot (identity/raw-value (:principal binding)) (:operation binding)]
        document-targets (filter #(= :document (first %)) targets)
        tables (conj (set (keys after-docs)) :gesso_commands)
        puts (mapv (fn [[_ table id]]
                     (let [matches (filterv #(= id (:xt/id %)) (get after-docs table))]
                       (when-not (= 1 (count matches))
                         (reject! :historical-mapped-write-row-missing [] {}))
                       [:put-docs table (first matches)])) document-targets)]
    (when-not (= #{slot} (set (filter #(= :journal-slot (first %)) targets)))
      (reject! :historical-mapped-journal-target-mismatch [] {}))
    (frame/check-replacements! mapping (get-in description [:document-effects alternative]) before-docs puts)
    ;; Nonwritten complete rows must also be preserved, not just their
    ;; projected fields. Ignore query row ordering, not field presence.
    (let [index (fn [docs] (into {} (map (fn [[table rows]] [table (into {} (map (juxt :xt/id clojure.core/identity)) rows)])) docs))
          expected (reduce (fn [docs [_ table row]] (assoc-in docs [table (:xt/id row)] row))
                           (index before-docs) puts)]
      (when-not (= expected (index after-docs))
        (reject! :historical-mapped-document-frame-mismatch [] {})))
    {:tables tables :puts (conj puts [:put-docs :gesso_commands record])}))

(defn reconstruct-mapped-publication!
  "Derive ordered canonical publication intents from the issued alternative's
   declared purpose and ordered document/topic selectors. Before and after
   rows must already have passed the historical relation and frame checks.
   No observed outbox rows or caller change vector enter this calculation.
   Missing declaration is not equivalent to a declared empty publication.
   This is an expectation builder, not historical admission or authority."
  [admission alternative source-id record before-docs after-docs]
  (let [description (describe-mapped-admission admission)
        declarations (:publication-derivations description)]
    (when-not (contains? (:operation-mappings description) alternative)
      (reject! :unknown-operation [alternative] {}))
    (if-not (contains? declarations alternative)
      {:status :not-declared}
      (let [declaration (get declarations alternative)
            {:keys [principal operation command-id]}
            (:gesso.command/binding (resolution/require-record record))
            principal (identity/raw-value principal)
            command-id (identity/raw-value command-id)
            _ (when-not (contains? (get-in description [:operation-mappings alternative :write-targets])
                                   [:journal-slot principal operation])
                (reject! :publication-command-slot-mismatch [] {}))]
        (if (nil? declaration)
          {:status :derived :intents []}
          (let [row (fn [docs [_ table id] required?]
                      (let [matches (filterv #(= id (:xt/id %)) (get docs table))]
                        (when-not (if required? (= 1 (count matches)) (<= (count matches) 1))
                          (reject! :invalid-publication-document-rows [] {}))
                        (first matches)))
                subject (fn [doc field]
                          (let [id (get doc field)]
                            (when-not (or (and (string? id) (not (str/blank? id)))
                                          (keyword? id) (integer? id) (uuid? id))
                              (reject! :invalid-publication-document-subject [] {}))
                            id))
                changes (into [] (comp (mapcat (fn [{:keys [target topic field]}]
                                                  (let [before (row before-docs target false)
                                                        after (row after-docs target true)
                                                        new (subject after field)
                                                        ids (if before [(subject before field) new] [new])]
                                                    (map (fn [id] {:topic topic :id id}) ids))))
                                        (distinct)) (:contributions declaration))]
            (when-not (<= 1 (count changes) 16384)
              (reject! :invalid-derived-publication-count [] {}))
            {:status :derived
             :intents (:intents (outbox/prepare!
                                  {:intent-ids (mapv #(outbox/derived-intent-id source-id operation principal command-id %)
                                                     (range (count changes)))
                                   :source-id source-id :operation operation :purpose (:purpose declaration)
                                   :command-id command-id} changes))}))))))

(defn reconstruct-mapped-prestate-prefix!
  "Internal historical verifier: return the exact reconstructed guard vector.
   Do not expose it through operational reports. Compare the durable prestate witness with a supplied physical projection
   and reconstruct the exact current-profile guard prefix from those rows.
   The caller must independently establish the preceding system-time basis
   at the committing transaction's valid time. This is not an attestation
   of the provider's transaction-internal read basis or input derivation."
  [admission evidence document-rows journal-rows]
  (let [{:keys [source description]} (registered-mapped! admission)
        alternative (:alternative evidence)
        mapping (get-in description [:operation-mappings alternative])
        original (some (fn [{:keys [id construction]}]
                         (when (= id alternative) construction))
                       (get-in source [:inductive-source :catalog :operations]))]
    (when-not (and original mapping (= 4 (:profile-version evidence))
                   (not= :invalid (envelope/binding-status evidence))
                   (current-guard-evidence? evidence)
                   (identical? original-initial-projection projection/project-state!))
      (reject! :invalid-historical-mapped-evidence [] {}))
    (let [actual (original-initial-projection original (:target-map source)
                                              document-rows journal-rows (:decode-outcome! source))]
      (when-not (= (decode-valuation! (:prestate-witness evidence)) actual)
        (reject! :mapped-prestate-mismatch [] {}))
      (let [prefix (frame/formatted-prestate-assertions! mapping document-rows journal-rows)]
        (when-not (= (select-keys evidence [:guard-profile :guard-prefix-fingerprint])
                     (frame/prefix-evidence! prefix))
          (reject! :historical-guard-prefix-mismatch [] {}))
        prefix))))

(defn check-mapped-prestate!
  "Check the supplied prestate and reconstructed guard identity without
   returning the potentially sensitive SQL or bound values."
  [admission evidence document-rows journal-rows]
  (reconstruct-mapped-prestate-prefix! admission evidence document-rows journal-rows)
  {:authority :none :physical-correspondence :checked-supplied-prestate
   :guard-prefix-correspondence :checked-reconstructed-prefix})

(defn check-mapped-poststate!
  "Compare the complete supplied projection with the admitted transition's
   expected poststate, using the selected original operation and decoder.
   This is a data check; only the executor can bind it to its own receipt.
   It does not establish transaction-internal prestate or writer closure."
  [admission {:keys [identity candidate]} document-rows journal-rows]
  (let [{:keys [source description]} (registered-mapped! admission)
        alternative (:alternative identity)
        mapping (get-in description [:operation-mappings alternative])
        original (some (fn [{:keys [id construction]}]
                         (when (= id alternative) construction))
                       (get-in source [:inductive-source :catalog :operations]))]
    (when-not (and original mapping
                   (= :exact-generated-prefix (:frame-prefix candidate))
                   (= (:fingerprint mapping) (:mapping-fingerprint candidate))
                   (= (:transition-fingerprint mapping) (:transition-fingerprint candidate))
                   (identical? original-initial-projection projection/project-state!))
      (reject! :invalid-mapped-poststate-candidate [] {}))
    (let [actual (original-initial-projection original (:target-map source)
                                              document-rows journal-rows (:decode-outcome! source))]
      (when-not (= (:after candidate) actual)
        (reject! :mapped-poststate-mismatch [] {}))
      {:authority :none :physical-correspondence :checked-supplied-projection})))

(defn require-current-mapped-admission!
  "Recheck the original live handles and mapping of this exact issued callable
   against a trusted current assembly source, including decoder and footprint identity.
   A copied description or newly issued registry is not a substitute for it."
  [admission current-source]
  (let [{original :source prior :description} (registered-mapped! admission)
        old (:inductive-source original) now (:inductive-source current-source)]
    (mapped-source-keys! current-source)
    (exact-map! now inductive-keys [:inductive-source])
    (when-not (and (= (:catalog old) (:catalog now))
                   (= (:current-graph old) (:current-graph now))
                   (identical? (:certificate-handle old) (:certificate-handle now))
                   (identical? (:dependency-handle old) (:dependency-handle now))
                   (= (:target-map original) (:target-map current-source))
                   (identical? (:decode-outcome! original) (:decode-outcome! current-source))
                   (= (:auxiliary-tables original) (:auxiliary-tables current-source))
                   (= (:document-effects original) (:document-effects current-source))
                   (= (:input-derivations original) (:input-derivations current-source))
                   (= (:publication-derivations original) (:publication-derivations current-source))
                   (identical? (:transaction-footprint! original) (:transaction-footprint! current-source)))
      (reject! :changed-mapped-admission-registration [:current-source] {}))
    (let [fresh (mapped-description (checked-mapped-source! current-source))]
      (when-not (= prior fresh)
        (reject! :stale-mapped-admission [:current-source] {}))
      fresh)))

(defn recheck-mapped-induction!
  "Rebuild the conditional abstract report from the original source and
   live handles retained by this issued admission. This does not recheck
   the installed physical writer or establish current provider coverage."
  [admission]
  (let [{:keys [source description]} (registered-mapped! admission)
        {:keys [induction]} (checked-report! (:inductive-source source))]
    (when-not (and (= (select-keys description [:catalog-fingerprint :induction-fingerprint])
                     (select-keys (:identity induction) [:catalog-fingerprint :induction-fingerprint]))
                   (= (:original-open-obligations description)
                      (get-in induction [:classification :original-obligations :by-operation])))
      (reject! :stale-mapped-induction [:admission] {}))
    induction))

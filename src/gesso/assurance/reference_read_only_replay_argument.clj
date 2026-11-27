(ns gesso.assurance.reference-read-only-replay-argument
  "Conditional replay refinement for the original closed journal-slot catalog.
   A replay is an observation at a qualified cut, not a new transaction."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [gesso.assurance.reference-recovery-projection :as projection]
            [gesso.assurance.reference-source-transition-argument :as committed]
            [gesso.assurance.proof.independent-transition-semantics :as semantics]
            [gesso.assurance.transition :as construction]
            [gesso.model.command-resolution :as resolution]
            [gesso.choreo.identity :as identity])
  (:import (java.security MessageDigest) (java.util HexFormat)))

(def profile :conditional-read-only-replay-argument-v1)
(def ^:private original-catalog projection/check-catalog!)
(def ^:private original-committed committed/derive!)
(def ^:private original-build construction/build-guarded-replacement!)
(def ^:private original-project projection/project!)
(def ^:private original-observe semantics/observe-transition!)
(def ^:private original-binding resolution/require-binding)
(def ^:private original-record resolution/require-record)
(def ^:private original-reconcile resolution/reconcile)
(def ^:private reviewed-sources
  {"gesso/model/command_resolution.clj" "ff05e0e2962155e3f44fe5284971915ee3b48a508a69d0f6ec85f38582e820c4"
   "gesso/live/outbox/model_publication.clj" "fee1ccde4a0d4ca1dbee249bf55e5dcb27f85052ace536f8d3bde52cb7b00878"})

(def ^:private base
  {:profile profile :authority :none :certificate :not-issued
   :universal-refinement :not-established :writer-coverage :not-established
   :proof-discharge :not-performed})

(defn- reject! [reason]
  (throw (ex-info "Unqualified read-only replay argument" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-catalog projection/check-catalog!)
                 (identical? original-committed committed/derive!)
                 (identical? original-build construction/build-guarded-replacement!)
                 (identical? original-project projection/project!)
                 (identical? original-observe semantics/observe-transition!)
                 (identical? original-binding resolution/require-binding)
                 (identical? original-record resolution/require-record)
                 (identical? original-reconcile resolution/reconcile)
                 (projection/boundaries-current?))
    (reject! :replay-boundary-replaced))
  (doseq [[path digest] reviewed-sources]
    (let [resource (io/resource path)]
      (when-not (and resource (= digest (with-open [in (io/input-stream resource)]
                                          (.formatHex (HexFormat/of)
                                                      (.digest (MessageDigest/getInstance "SHA-256") (.readAllBytes in))))))
        (reject! :unreviewed-replay-source)))))

(defn- alternative! [checked writer-report {:keys [id construction]}]
  (let [{:keys [state-types input-types reads writes replacements frame guard]} construction
        fields (set (keys state-types))
        mapping (get-in checked [:operation-mappings id])
        [op [reader field :as lhs] [literal value :as rhs] :as expression] (:expression guard)
        [kind principal operation :as coordinate] (get-in checked [:field-coordinates field])
        ;; The guard must select exactly one declared non-nil journal value.
        ;; Select the originating writer by slot AND value, not by its name.
        matches (filterv (fn [[[p o] v]] (and (= p principal) (= o operation) (= v value)))
                         (:closed-outcome-cases checked))]
    (when-not (and (= id (get-in construction [:subject :id]))
                   (empty? input-types) (= #{} writes) (= {} replacements)
                   (= fields frame) (= #{field} reads (:readers guard))
                   (= expression [:= [:read field] [:literal value]])
                   (= :journal-slot kind) (keyword? value)
                   (= {field coordinate} (:read-coordinates mapping))
                   (= {} (:write-coordinates mapping))
                   (= (:field-coordinates checked) (:frame-coordinates mapping))
                   (= 1 (count matches)))
      (reject! :unsupported-replay-relation))
    (let [[case-key _] (first matches)
          writer-id (get-in checked [:case-alternatives case-key])
          writer (get-in writer-report [:alternatives writer-id])
          built (original-build construction)
          identity {:catalog-id (:catalog-id checked)
                    :catalog-fingerprint (:catalog-fingerprint checked)
                    :alternative-id id :transition-fingerprint (:transition-fingerprint built)
                    :mapping-fingerprint (:mapping-fingerprint mapping)}
          p (fn [image] [:project (:mapping-fingerprint mapping) image])
          fact [:abstract-transition identity {} (p :read-cut) (p :read-cut)]
          goal [:abstract-transition identity {} (p :actual-before) (p :actual-after)]
          equalities {:actual-before :read-cut :actual-after :read-cut}
          normalized (-> goal (update-in [3 2] equalities) (update-in [4 2] equalities))]
      (when-not (and writer (= case-key (:case-key writer))
                     (= writer-id (get-in writer [:identity :alternative-id]))
                     (= (:catalog-fingerprint checked) (get-in writer [:identity :catalog-fingerprint]))
                     (= fact normalized))
        (reject! :replay-origin-or-substitution-mismatch))
      {:identity identity :origin (:identity writer) :case-key case-key
       :journal-field field :journal-value value :guard guard :frame frame
       :frame-equations (into {} (map (fn [f] [f [:pre f]]) fields))
       :writes #{} :equalities equalities :source-fact fact :goal goal
       :normalized-goal normalized :status :conditional-read-only-transition-derivation})))

(defn derive!
  "Derive the replay relation for every remaining original alternative.
   At one qualified read cut, an exact permitted canonical record supplies
   the journal guard; empty replacements and the full frame imply R(s,{},s).
   The cut equality models the replay's own effect, not arbitrary endpoints
   of a concurrent history. No claim that the actual host meets the premises."
  [source declarations fingerprint cases]
  (try
    (boundaries!)
    (let [checked (original-catalog source declarations fingerprint cases)
          writer-report (original-committed source declarations fingerprint cases)
          operations (get-in source [:inductive-source :catalog :operations])
          outside (:outside-fragment writer-report)]
      (when-not (and (= :matched (:status checked))
                     (= :conditional-catalog-commit-composition (:status writer-report))
                     (= fingerprint (:catalog-fingerprint checked) (:catalog-fingerprint writer-report))
                     (= :not-performed (:proof-discharge checked) (:proof-discharge writer-report))
                     (seq outside))
        (reject! :unqualified-replay-catalog))
      (let [reports (into {} (keep (fn [{:keys [id] :as op}]
                                     (when (contains? outside id) [id (alternative! checked writer-report op)]))) operations)
            origins (map #(get-in % [:origin :alternative-id]) (vals reports))]
        (when-not (and (= outside (set (keys reports)))
                       (= (frequencies origins) (zipmap (keys (:alternatives writer-report)) (repeat 1))))
          (reject! :incomplete-or-ambiguous-replay-coverage))
        (boundaries!)
        (assoc base :status :conditional-catalog-replay-derivation
               :scope :original-declared-read-only-replay-alternatives
               :catalog-fingerprint fingerprint :catalog-id (:catalog-id checked)
               :alternatives reports :reviewed-sources reviewed-sources
               :remaining-conditions (set/union (:remaining-conditions checked)
                  #{:faithful-original-abstract-transition-semantics
                    :well-typed-complete-projected-endpoints
                    :faithful-reviewed-source-and-loaded-replay-code
                    :authenticated-exact-command-binding-and-permitted-disclosure
                    :faithful-authoritative-record-and-historical-evidence-readback
                    :faithful-single-cut-record-and-state-projection
                    :no-replay-attributed-mutations-or-submissions
                    :complete-admitted-writer-and-temporal-boundary-coverage}))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-replay)))))

(defn check-witness!
  "Check supplied same-cut images and an exact replay result. The submitted
   evidence is NOT an authenticated trace. An empty attributed submission
   list and equal images cannot prove the absence of hidden effects."
  [source declarations fingerprint cases id {:keys [before after binding result submissions disclosure-permitted?] :as witness}]
  (try
    (let [report (derive! source declarations fingerprint cases)
          alternative (get-in report [:alternatives id])
          original (:construction (first (filter #(= id (:id %)) (get-in source [:inductive-source :catalog :operations]))))
          [principal operation arguments outcome] (:case-key alternative)
          coordinates (:target-map source)
          tables (conj (set (keep (fn [[kind table]] (when (#{:document :nullable-document} kind) table)) (vals coordinates))) :gesso_commands)]
      (when-not (and (= :conditional-catalog-replay-derivation (:status report)) alternative
                     (= #{:before :after :binding :result :submissions :disclosure-permitted?} (set (keys witness)))
                     (true? disclosure-permitted?) (= [] submissions)
                     (map? before) (map? after) (= tables (set (keys before)) (set (keys after)))
                     (every? map? (vals before)) (= before after))
        (reject! :unqualified-read-only-images-or-effects))
      (original-binding binding)
      (let [record (original-record (get-in before [:gesso_commands (resolution/record-id binding)]))
            pre (original-project coordinates before cases)
            post (original-project coordinates after cases)
            observed (original-observe (:transition (original-build original)) pre {} post)]
        (when-not (and (= binding (:gesso.command/binding record))
                       (= [principal operation arguments] [(identity/raw-value (:principal binding)) (:operation binding) (:arguments binding)])
                       (= outcome (:gesso.command/outcome record) (:outcome result))
                       (= :replay (:status result))
                       (= (select-keys result [:status :outcome])
                          (select-keys (original-reconcile binding record (constantly true)) [:status :outcome]))
                       (= (:journal-value alternative) (get pre (:journal-field alternative)))
                       (= pre post) (true? (:matches? observed)))
          (reject! :replay-record-or-relation-mismatch))
        (boundaries!)
        (assoc base :status :conditional-selected-replay-correspondence :identity (:identity alternative)
               :origin (:origin alternative) :before pre :after post
               :remaining-conditions (:remaining-conditions report))))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-replay-witness)))))

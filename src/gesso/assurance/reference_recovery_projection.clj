(ns gesso.assurance.reference-recovery-projection
  "Independent coordinate interpretation over supplied visible rows.
   Shares the canonical record validator, not the mapped poststate decoder.
   No claim that the supplied rows exhaust authoritative provider state."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.certified-plan :as plan]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-independent-coordinate-projection-v1)
(def ^:private original-record resolution/require-record)
(def ^:private original-raw identity/raw-value)

(def ^:private original-catalog catalog/check-operation-set!)
(def ^:private original-mapping plan/check-target-mapping!)
(def ^:private original-cases outcome/derive-cases!)

(defn boundaries-current? []
  (and (identical? original-record resolution/require-record)
       (identical? original-raw identity/raw-value)
       (identical? original-catalog catalog/check-operation-set!)
       (identical? original-mapping plan/check-target-mapping!)
       (identical? original-cases outcome/derive-cases!)))

(defn- reject! [reason]
  (throw (ex-info "Unqualified independent coordinate projection" {:reason reason})))

(defn- physical-name? [x]
  (and (simple-keyword? x) (re-matches #"[a-z][a-z0-9_]*" (name x))))

(defn- coordinate! [[kind a b c :as coord]]
  (when-not (and (vector? coord)
                 (case kind
                   (:document :nullable-document)
                   (and (= 4 (count coord)) (physical-name? a) (not= :gesso_commands a)
                        (string? b) (seq b) (physical-name? c))
                   :journal-slot (and (= 3 (count coord)) (string? a) (seq a) (qualified-keyword? b))
                   false))
    (reject! :invalid-independent-coordinate))
  coord)

(defn- journal-records! [visible]
  (let [rows (get visible :gesso_commands {})]
    (when-not (map? rows) (reject! :invalid-independent-journal))
    (mapv (fn [[id raw]]
            (let [record (original-record raw)
                  binding (:gesso.command/binding record)]
              (when-not (= id (:xt/id record)) (reject! :independent-journal-identity-mismatch))
              {:principal (original-raw (:principal binding))
               :operation (:operation binding) :arguments (:arguments binding)
               :outcome (:gesso.command/outcome record)})) rows)))

(defn- slot-value! [records cases principal operation]
  (let [matches (filterv #(and (= principal (:principal %)) (= operation (:operation %))) records)]
    (case (count matches)
      0 nil
      1 (let [{:keys [arguments outcome]} (first matches)
              k [principal operation arguments outcome]]
          (when-not (and (contains? cases k) (keyword? (get cases k)))
            (reject! :independent-unmapped-outcome))
          (get cases k))
      (reject! :independent-ambiguous-slot))))

(defn- document-value! [visible kind table id column]
  (let [rows (get visible table {})
        field (keyword (str/replace (name column) "_" "-"))]
    (when-not (map? rows) (reject! :invalid-independent-document-table))
    (if-not (contains? rows id)
      (if (= :nullable-document kind) nil (reject! :independent-required-document-absent))
      (let [row (get rows id)]
        (when-not (and (map? row) (= id (:xt/id row)) (contains? row field))
          (reject! :independent-mapped-field-missing))
        (get row field)))))

(defn project!
  "Interpret each declared coordinate independently. Only a closed case map
   is allowed; journal matches use full principal/operation/arguments/outcome
   identity and must be unique. The caller supplies the already qualified
   original coordinates and closed decoder cases."
  [coordinates visible cases]
  (when-not (and (boundaries-current?) (map? coordinates) (seq coordinates)
                 (map? visible) (map? cases))
    (reject! :unqualified-independent-projection))
  (let [coords (mapv coordinate! (vals coordinates))
        records (when (some #(= :journal-slot (first %)) coords) (journal-records! visible))]
    (into {} (map (fn [[field [kind a b c]]]
                    [field (if (= :journal-slot kind)
                             (slot-value! records cases a b)
                             (document-value! visible kind a b c))])) coordinates)))

(def catalog-profile :original-catalog-coordinate-correspondence-v1)
(def binding-profile :selected-authoritative-projection-support-v1)

(defn- plain-map? [x] (and (map? x) (nil? (meta x))))

(defn- address [[kind a b c]]
  (if (= :journal-slot kind) [kind a b] [:document a b c]))

(defn- target [[kind a b]]
  (if (= :journal-slot kind) [kind a b] [:document a b]))

(defn- check-coordinates! [types coordinates]
  (when-not (and (plain-map? coordinates) (seq coordinates)
                 (= (set (keys types)) (set (keys coordinates))))
    (reject! :incomplete-coordinate-footprint))
  (doseq [[field coord] coordinates]
    (let [[kind] (coordinate! coord) descriptor (types field)]
      (when-not (and (nil? (meta coord)) (not (:optional? descriptor))
                     (or (= :document kind) (true? (:nullable? descriptor)))
                     (or (not= :journal-slot kind) (= :keyword (:type descriptor))))
        (reject! :coordinate-type-mismatch))))
  (when-not (and (= (count coordinates) (count (set (map address (vals coordinates)))))
                 (every? #(apply = (map first %)) (vals (group-by target (vals coordinates)))))
    (reject! :aliased-or-conflicting-coordinates)))

(defn- alternative! [coordinates {:keys [id construction]}]
  (let [{:keys [reads writes frame replacements state-types]} construction
        r (select-keys coordinates reads) w (select-keys coordinates writes)
        f (select-keys coordinates frame)
        checked (original-mapping construction coordinates)
        slots (filterv #(= :journal-slot (first (val %))) w)]
    (when-not (and (= id (get-in construction [:subject :id]))
                   (= writes (set (keys replacements)))
                   (= frame (set/difference (set (keys state-types)) writes))
                   (= coordinates (:target-map checked))
                   (= r (:read-coordinates checked)) (= w (:write-coordinates checked))
                   (= f (:frame-coordinates checked))
                   (= (set/difference reads (set (keys r))) (:input-read-fields checked))
                   (= (set (map target (vals w))) (:write-targets checked))
                   (string? (:fingerprint checked)) (<= (count slots) 1))
      (reject! :alternative-mapping-mismatch))
    (cond-> {:mapping-fingerprint (:fingerprint checked)
             :read-coordinates r :write-coordinates w :frame-coordinates f}
      (seq slots) (assoc :journal-write
                        (let [[field [_ principal operation]] (first slots)
                              replacement (replacements field)]
                          (when-not (and (vector? replacement) (= 2 (count replacement))
                                         (= :literal (first replacement)) (keyword? (second replacement)))
                            (reject! :unsupported-journal-replacement))
                          {:principal principal :operation operation :value (second replacement)})))))

(defn check-catalog!
  "Independently derive declared coordinate footprints and closed outcome
   keys from the original catalog, then compare the existing checkers.
   Concrete declaration meaning and real-world writer closure remain open."
  [source declarations expected-fingerprint cases]
  (let [base {:profile catalog-profile :scope :original-declared-mapping-and-cases-only
              :provider-refinement :not-established :universal-projection :not-established
              :proof-discharge :not-performed :authority :none :certificate :not-issued}]
    (try
      (when-not (boundaries-current?) (reject! :projection-boundary-replaced))
      (let [original (get-in source [:inductive-source :catalog])
            checked (original-catalog original)
            coordinates (:target-map source)
            types (get-in original [:operations 0 :construction :state-types])]
        (when-not (and (= catalog/report-type (:type checked)) (string? expected-fingerprint)
                       (= expected-fingerprint (:fingerprint checked))
                       (= (:id original) (:catalog-id checked))
                       (= (set (keys types)) (:state-projection checked)))
          (reject! :projection-catalog-mismatch))
        (check-coordinates! types coordinates)
        (let [alternatives (into {} (map (fn [op] [(:id op) (alternative! coordinates op)])) (:operations original))
              writers (into {} (keep (fn [[id a]] (when-let [w (:journal-write a)] [id w]))) alternatives)]
          (when-not (and (= (:declared-operation-ids checked) (set (keys alternatives)))
                         (plain-map? declarations) (= (set (keys writers)) (set (keys declarations))))
            (reject! :incomplete-journal-declarations))
          (let [derived (reduce-kv (fn [acc id {:keys [principal operation value]}]
                                     (let [{:keys [arguments outcome] :as d} (declarations id)
                                           k [principal operation arguments outcome]]
                                       (when-not (and (plain-map? d) (= #{:arguments :outcome} (set (keys d)))
                                                      (plain-map? arguments) (plain-map? outcome) (seq outcome)
                                                      (not (contains? (:cases acc) k)))
                                         (reject! :invalid-or-ambiguous-journal-case))
                                       (-> acc (assoc-in [:cases k] value) (assoc-in [:alternatives k] id))))
                                   {:cases {} :alternatives {}} writers)]
            (when-not (and (= derived (original-cases source declarations)) (= cases (:cases derived))
                           (boundaries-current?))
              (reject! :closed-outcome-correspondence-mismatch))
            (assoc base :status :matched :catalog-id (:id original) :catalog-fingerprint expected-fingerprint
                   :invariant-ids (:invariant-ids checked) :field-coordinates coordinates
                   :operation-mappings alternatives :closed-outcome-cases cases :case-alternatives (:alternatives derived)
                   :remaining-conditions #{:faithful-pinned-provider-readbacks :faithful-declared-coordinate-meaning
                                            :faithful-declared-concrete-outcome-meaning :faithful-canonical-record-validation
                                            :faithful-loaded-projection-semantics}))))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (or (:reason (ex-data e)) :unqualified-projection-catalog))))))

(defn bind-premise
  "Internal composition of retained issued-cut evidence with the original
   open projection and snapshot premises. No acceptance or discharge."
  [checked transfer inventory abstract]
  (let [base {:profile binding-profile :scope :original-selected-coordinate-projection-only
              :premise-acceptance :not-performed :proof-discharge :not-performed
              :universal-projection :not-established :authority :none :certificate :not-issued}
        evidence (:catalog-projection-correspondence checked)
        physical (:physical-readbacks checked)
        projection (:projection-correspondence physical)
        subject (:judgment-subject checked)
        fp (get-in transfer [:condition-premise-fingerprints :faithful-authoritative-state-projection])
        snapshot (get-in transfer [:condition-premise-fingerprints :faithful-pinned-provider-readbacks])
        premise (get-in inventory [:premises-by-fingerprint fp])
        snapshot-premise (get-in inventory [:premises-by-fingerprint snapshot])]
    (if (and (= :checked (:status checked)) (= :bound (:status transfer))
             (= catalog-profile (:profile evidence)) (= :matched (:status evidence))
             (= profile (:profile projection)) (= :matched (:status physical) (:status projection))
             (= :not-performed (:proof-discharge evidence) (:proof-discharge projection) (:proof-discharge transfer))
             (= :not-established (:universal-projection evidence) (:universal-projection projection))
             (string? (:recovery-cut-ref checked)) (= (:recovery-cut-ref checked) (:recovery-cut-ref transfer))
             (string? (:catalog-fingerprint checked))
             (= (:catalog-fingerprint checked) (:catalog-fingerprint subject) (:catalog-fingerprint evidence)
                (get-in abstract [:identity :catalog-fingerprint]))
             (= (:catalog-id subject) (:catalog-id evidence) (:catalog-id abstract))
             (= (:abstract-induction-fingerprint subject) (:abstract-report-fingerprint transfer)
                (:fingerprint abstract) (get-in inventory [:abstract :report-fingerprint]))
             (= (:invariant-ids checked) (:invariant-ids evidence) (:invariant-ids transfer) (:declared-invariant-ids abstract))
             (seq (:selected-operations subject))
             (every? (fn [{:keys [alternative-id mapping-fingerprint]}]
                       (and (string? mapping-fingerprint)
                            (= mapping-fingerprint (get-in evidence [:operation-mappings alternative-id :mapping-fingerprint]))))
                     (:selected-operations subject))
             (seq (:field-coordinates evidence)) (= (:field-coordinates evidence) (:field-coordinates projection))
             (= (:closed-outcome-cases evidence) (:closed-outcome-cases projection))
             (seq (:checked-coordinates physical))
             (= (:checked-coordinates physical) (:checked-coordinates projection)
                (get-in checked [:conditional-physical-invariant :checked-coordinates]))
             (seq (:remaining-conditions evidence)) (string? fp) (string? snapshot) (not= fp snapshot)
             (every? #(contains? (:required-open-premise-fingerprints transfer) %) [fp snapshot])
             (every? #(contains? (:unaccepted-premise-fingerprints inventory) %) [fp snapshot])
             (= {:id :reference-physical/authoritative-projection :family :physical
                 :dependency-fingerprints #{snapshot} :acceptance :not-performed :discharge-state :open} premise)
             (= :reference-physical/snapshot-continuity (:id snapshot-premise))
             (= :physical (:family snapshot-premise)) (= :open (:discharge-state snapshot-premise))
             (= :not-performed (:acceptance snapshot-premise)))
      (assoc base :status :conditional-premise-support :premise-fingerprint fp :premise-state :open
             :snapshot-premise-fingerprint snapshot :recovery-cut-ref (:recovery-cut-ref checked)
             :catalog-fingerprint (:catalog-fingerprint evidence) :invariant-ids (:invariant-ids evidence)
             :checked-coordinates (:checked-coordinates physical) :remaining-conditions (:remaining-conditions evidence))
      (assoc base :status :not-established :reason :unqualified-original-projection-premise))))

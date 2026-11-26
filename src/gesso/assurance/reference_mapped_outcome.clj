(ns gesso.assurance.reference-mapped-outcome
  "Closed source-relative outcome cases. Concrete declarations remain premises."
  (:require [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-mapped-outcome-derivation-v1)
(defn- plain-map? [x] (and (map? x) (nil? (meta x))))
(defn- reject! [reason] (throw (ex-info "Unsupported mapped outcome declaration." {:reason reason})))

(defn- journal-write! [targets {:keys [id construction]}]
  (when-not (and (qualified-keyword? id) (= id (get-in construction [:subject :id])))
    (reject! :invalid-outcome-alternative))
  (let [mapping (plan/check-target-mapping! construction targets)
        slots (filter (fn [[_ coord]] (= :journal-slot (first coord))) (:write-coordinates mapping))]
    (when (seq slots)
      (when-not (= 1 (count slots)) (reject! :unsupported-outcome-journal-writes))
      (let [[field [_ principal operation]] (first slots)
            replacement (get-in construction [:replacements field])]
        (when-not (and (vector? replacement) (= 2 (count replacement))
                       (= :literal (first replacement)) (keyword? (second replacement)))
          (reject! :unsupported-outcome-replacement))
        {:principal principal :operation operation :value (second replacement)}))))

(defn derive-cases!
  "Require one concrete arguments/outcome pair per journal-writing alternative.
   Derive abstract values and slot identity from the original construction.
   Reject ambiguous concrete cases even if their abstract values agree."
  [source declarations]
  (let [operations (get-in source [:inductive-source :catalog :operations])
        targets (:target-map source)]
    (when-not (and (plain-map? source) (plain-map? targets) (plain-map? declarations)
                   (vector? operations) (nil? (meta operations)) (<= 1 (count operations) 128)
                   (every? plain-map? operations)
                   (= (count operations) (count (set (map :id operations)))))
      (reject! :invalid-outcome-catalog))
    (let [writers (into {} (keep (fn [op] (when-let [w (journal-write! targets op)] [(:id op) w]))) operations)]
      (when-not (and (seq writers) (= (set (keys writers)) (set (keys declarations))))
        (reject! :incomplete-outcome-declarations))
      (reduce-kv
       (fn [{:keys [cases alternatives] :as acc} id {:keys [principal operation value]}]
         (let [{:keys [arguments outcome] :as declaration} (get declarations id)]
           (when-not (and (plain-map? declaration) (= #{:arguments :outcome} (set (keys declaration)))
                          (plain-map? arguments) (plain-map? outcome) (seq outcome))
             (reject! :invalid-concrete-outcome-declaration))
           (resolution/command-record {:command-id (identity/command-id "outcome-declaration")
                                       :principal (identity/principal principal)
                                       :operation operation :arguments arguments} outcome "declaration")
           (let [k [principal operation arguments outcome]]
             (when (contains? cases k) (reject! :ambiguous-concrete-outcome-case))
             (assoc acc :cases (assoc cases k value) :alternatives (assoc alternatives k id)))))
       {:cases {} :alternatives {}} writers))))

(defn compare-selected!
  "Bind a candidate record and finite decoder table to supplied original
   catalog and concrete declarations. Never invoke the opaque runtime decoder.
   No commitment, issuance or authentication claim."
  [source declarations original targets expectation spec]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established :source-provenance :not-established
              :outcome-provenance :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [{:keys [cases alternatives]} (derive-cases! source declarations)
            id (get-in original [:subject :id])
            selected (first (filter #(= id (:id %)) (get-in source [:inductive-source :catalog :operations])))
            ops (:ops expectation)
            puts (when (and (vector? ops) (nil? (meta ops)))
                   (filter #(and (vector? %) (= [:put-docs :gesso_commands] (vec (take 2 %)))) ops))]
        (cond
          (not (and (some? selected) (= original (:construction selected)) (= targets (:target-map source))
                    (= id (:alternative spec))))
          (fail :mismatch :selected-outcome-alternative-mismatch)
          (not (and (plain-map? spec) (plain-map? (:outcome-cases spec))
                    (not (contains? spec :outcome-key)) (= cases (:outcome-cases spec))))
          (fail :mismatch :selected-outcome-cases-mismatch)
          (not (and (= 1 (count puts)) (= 3 (count (first puts)))))
          (fail :unsupported :invalid-selected-outcome-record)
          :else
          (let [record (resolution/require-record (nth (first puts) 2))
                {:keys [principal operation arguments]} (:gesso.command/binding record)
                key [(identity/raw-value principal) operation arguments (:gesso.command/outcome record)]]
            (cond
              (not= record (resolution/command-record (:binding spec) (:outcome spec) (:now spec)))
              (fail :mismatch :selected-outcome-record-mismatch)
              (not= id (get alternatives key))
              (fail :mismatch :selected-concrete-outcome-mismatch)
              :else (assoc base :status :matched :outcome-derivation :matched-declared-journal-assignment
                                :candidate-command-record :matched)))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-mapped-outcome-source))))))

(ns gesso.assurance.reference-mapped-poststate
  "Independent abstract projection of one selected committed retained step.
   No expected puts, candidate evidence record or live head read is used."
  (:require [clojure.string :as str]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.assurance.transition :as construction]
            [gesso.choreo.identity :as identity]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-retained-abstract-transition-v1)

(defn- reject! [reason]
  (throw (ex-info "Unsupported retained abstract projection." {:reason reason})))

(defn- journal-index! [visible]
  (group-by (fn [[id row]]
              (let [record (resolution/require-record row)
                    binding (:gesso.command/binding record)]
                (when-not (= id (:xt/id record)) (reject! :journal-identity-mismatch))
                [(identity/raw-value (:principal binding)) (:operation binding)]))
            (get visible :gesso_commands)))

(defn- decode-outcome! [record {:keys [outcome-key outcome-cases]}]
  (let [{:keys [principal operation arguments]} (:gesso.command/binding record)
        outcome (:gesso.command/outcome record)
        value (if outcome-cases
                (get outcome-cases [(identity/raw-value principal) operation arguments outcome])
                (get outcome outcome-key))]
    (when-not (keyword? value) (reject! :unmapped-retained-outcome))
    value))

(defn- project! [coordinates visible spec]
  (when-not (map? visible) (reject! :invalid-retained-visible-state))
  (let [slots (when (some #(= :journal-slot (first %)) (vals coordinates))
                (journal-index! visible))]
    (into {} (map (fn [[field [kind a b c]]]
                    [field
                     (if (= :journal-slot kind)
                       (let [rows (get slots [a b])]
                         (case (count rows)
                           0 nil
                           1 (decode-outcome! (val (first rows)) spec)
                           (reject! :ambiguous-retained-journal-slot)))
                       (let [row (get-in visible [a b])
                             k (keyword (str/replace (name c) "_" "-"))]
                         (cond
                           (and (nil? row) (= :nullable-document kind)) nil
                           (not (and (map? row) (= b (:xt/id row)) (contains? row k)))
                           (reject! :missing-retained-mapped-field)
                           :else (get row k))))])) coordinates)))

(defn compare-selected!
  "Project both sides of the selected committed transaction at its own
   valid instant, using the predecessor and selected system snapshots.
   Check the original guarded relation over those values and supplied inputs.
   Retained history and decoder/input provenance remain premises. This
   diagnostic is not a provider comparison or a refinement certificate."
  [inputs expectation original targets spec]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [id (:tx-id expectation)
            selected (when (and (vector? inputs) (nil? (meta inputs)) (<= 1 (count inputs) 256))
                       (filterv #(= id (:tx-id %)) inputs))]
        (cond
          (not (and (integer? id) (<= 0 id (dec Long/MAX_VALUE)) (= 1 (count selected))
                    (= expectation (first selected))))
          (fail :mismatch :selected-abstract-transaction-mismatch)
          (not (and (map? spec) (map? (:inputs spec))
                    (not-any? :optional? (vals (:state-types original)))
                    (or (and (simple-keyword? (:outcome-key spec)) (not (contains? spec :outcome-cases)))
                        (and (map? (:outcome-cases spec)) (seq (:outcome-cases spec))
                             (not (contains? spec :outcome-key))))))
          (fail :unsupported :invalid-abstract-projection-source)
          :else
          (let [prior (vec (take-while #(not= id (:tx-id %)) inputs))
                through (conj prior expectation)
                prediction (temporal/replay (mapv #(dissoc % :tx-id) through))
                instant (:system-time expectation)]
            (cond
              (not= :predicted (:status prediction))
              (fail :unsupported :unsupported-abstract-transition-replay)
              (not (true? (:committed? (peek (:history prediction)))))
              (fail :mismatch :selected-abstract-transaction-not-committed)
              :else
              (let [coordinates (:target-map (plan/check-target-mapping! original targets))
                    before (project! coordinates (if (seq prior)
                                                   (temporal/visible-at prediction (:system-time (peek prior)) instant)
                                                   {}) spec)
                    after (project! coordinates (temporal/visible-at prediction instant instant) spec)
                    checked (independent/observe-transition!
                             (:transition (construction/build-guarded-replacement! original))
                             before (:inputs spec) after)]
                (if (:matches? checked)
                  (assoc base :status :matched :abstract-transition :matched-retained-projection)
                  (fail :mismatch :selected-retained-abstract-transition-mismatch)))))))
      (catch clojure.lang.ExceptionInfo _
        (fail :unsupported :invalid-retained-abstract-projection)))))

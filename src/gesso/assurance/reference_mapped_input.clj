(ns gesso.assurance.reference-mapped-input
  "Source-relative input derivation for a selected retained candidate.
   Does not authenticate the supplied mapped admission source or binding."
  (:require [gesso.assurance.proof.typed-value-semantics :as values]
            [gesso.assurance.reference-mapped-writes :as writes]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-mapped-input-derivation-v1)

(defn- plain-map? [x] (and (map? x) (nil? (meta x))))
(defn- reject! [reason] (throw (ex-info "Invalid selected mapped input source." {:reason reason})))

(defn- valid-selector? [construction selector]
  (and (vector? selector) (nil? (meta selector))
       (case (first selector)
         :principal (= [:principal] selector)
         :argument (and (= 2 (count selector)) (keyword? (second selector)))
         :state (and (= 2 (count selector)) (contains? (:state-types construction) (second selector)))
         false)))

(defn- selectors! [source original context]
  (let [operations (get-in source [:inductive-source :catalog :operations])
        id (get-in original [:subject :id])]
    (when-not (and (plain-map? source) (vector? operations) (nil? (meta operations))
                   (<= 1 (count operations) 256) (every? plain-map? operations))
      (reject! :invalid-input-catalog-selection))
    (let [ids (mapv :id operations)
          declarations (if (contains? source :input-derivations)
                         (:input-derivations source)
                         (zipmap ids (repeat {})))]
      (when-not (and (every? qualified-keyword? ids) (= (count ids) (count (set ids)))
                     (plain-map? declarations) (= (set ids) (set (keys declarations)))
                     (= original (:construction (first (filter #(= id (:id %)) operations)))))
        (reject! :invalid-input-catalog-selection))
      (doseq [{:keys [id construction]} operations
              :let [selectors (get declarations id)]]
        (when-not (and (plain-map? construction) (plain-map? (:input-types construction))
                       (plain-map? selectors) (= (set (keys (:input-types construction))) (set (keys selectors)))
                       (every? #(valid-selector? construction %) (vals selectors)))
          (reject! :invalid-source-input-selectors)))
      (let [selected (get declarations id)]
        (when-not (and (plain-map? context) (plain-map? (:selectors context)) (= selected (:selectors context)))
          (reject! :selected-input-selector-mismatch))
        selected))))

(defn- candidate-record! [expectation]
  (let [ops (:ops expectation)]
    (when-not (and (vector? ops) (nil? (meta ops)))
      (reject! :invalid-selected-command-record))
    (let [journal-puts (filter #(and (vector? %) (= [:put-docs :gesso_commands] (vec (take 2 %)))) ops)]
      (when-not (and (= 1 (count journal-puts)) (= 3 (count (first journal-puts))))
        (reject! :invalid-selected-command-record))
      (resolution/require-record (nth (first journal-puts) 2)))))

(defn- derive-inputs! [original selectors binding before]
  (let [inputs (into {} (map (fn [[id [kind k]]]
                              [id (case kind
                                    :principal (identity/raw-value (:principal binding))
                                    :argument (if (contains? (:arguments binding) k) (get (:arguments binding) k)
                                                  (reject! :missing-selected-input-argument))
                                    :state (if (contains? before k) (get before k)
                                               (reject! :missing-selected-input-state)))])) selectors)]
    (values/observe-state! (:input-types original) inputs)
    inputs))

(defn compare-selected!
  "Check exact selected construction and selector declarations from the
   supplied original mapped source, exact candidate command record, and
   independent selector evaluation over the projected prepared rows.
   No installed source, authentication, actual retained acquisition or
   outcome decoder provenance is asserted. A candidate may have aborted."
  [source original targets docs journal expectation spec context]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established :source-provenance :not-established
              :outcome-provenance :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [selectors (selectors! source original context)
            record (candidate-record! expectation)
            binding (:gesso.command/binding record)]
        (cond
          (not= targets (:target-map source)) (fail :mismatch :selected-input-target-map-mismatch)
          (not= record (resolution/command-record (:binding spec) (:outcome spec) (:now spec)))
          (fail :mismatch :selected-input-command-record-mismatch)
          :else
          (let [{:keys [before]} (writes/derive-transition! original targets docs journal spec)
                inputs (derive-inputs! original selectors binding before)]
            (if (= inputs (:inputs spec))
              (assoc base :status :matched :input-derivation :matched-source-relative-selectors
                          :candidate-command-record :matched)
              (fail :mismatch :selected-input-valuation-mismatch)))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-selected-mapped-input-source))))))

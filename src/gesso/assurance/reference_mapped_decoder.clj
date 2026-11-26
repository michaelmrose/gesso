(ns gesso.assurance.reference-mapped-decoder
  "Finite observation of the decoder bound to an issued mapped admission."
  (:require [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction]))

(def profile :issued-mapped-decoder-samples-v1)

(defn- case-key [record]
  (let [{:keys [principal operation arguments]} (:gesso.command/binding
                                                  (resolution/require-record record))]
    [(identity/raw-value principal) operation arguments (:gesso.command/outcome record)]))

(defn- sample-record [[principal operation arguments value] n variant]
  (resolution/command-record {:command-id (identity/command-id (str "decoder-sample-" n "-" variant))
                              :principal (identity/principal principal)
                              :operation operation :arguments arguments}
                             value (str "sample-time-" variant)))

(defn- observe [decode! record]
  (try
    {:kind :returned :value (decode! record)}
    (catch clojure.lang.ExceptionInfo _ {:kind :exception-info})
    (catch Exception _ {:kind :unexpected-exception})))

(defn compare-issued!
  "Recheck exact issued admission/source binding, then sample its decoder
   on each declared case (two record identities/clocks) and explicit valid
   out-of-domain records. The caller must trust callback execution. An
   ExceptionInfo is an observed throw, not proof of semantic rejection.
   No assertion about untested records or future invocations is made."
  [admission source declarations outside]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established :outcome-provenance :not-established
              :universal-decoder-correspondence :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [{:keys [cases]} (outcome/derive-cases! source declarations)]
        (if-not (and (vector? outside) (nil? (meta outside)) (<= (count outside) 128)
                     (= (count outside) (count (set outside)))
                     (every? #(not (contains? cases (case-key %))) outside))
          (fail :unsupported :invalid-outside-decoder-samples)
          (let [description (induction/require-current-mapped-admission! admission source)
                decode! (:decode-outcome! source)
                samples (mapcat (fn [[n [k expected]]]
                                  (map (fn [v] [(sample-record k n v) expected]) [0 1]))
                                (map-indexed vector (sort-by (comp pr-str key) cases)))
                positives (mapv (fn [[row expected]]
                                  (= {:kind :returned :value expected} (observe decode! row))) samples)
                negatives (mapv #(observe decode! %) outside)
                counts {:declared-case-count (count cases)
                        :positive-sample-count (count positives)
                        :positive-match-count (count (filter true? positives))
                        :outside-sample-count (count negatives)
                        :outside-throw-count (count (filter #(= :exception-info (:kind %)) negatives))
                        :outside-return-count (count (filter #(= :returned (:kind %)) negatives))
                        :outside-unexpected-exception-count (count (filter #(= :unexpected-exception (:kind %)) negatives))}
                result (merge base counts (select-keys description [:catalog-fingerprint :induction-fingerprint]))]
            (cond
              (not (every? true? positives)) (assoc result :status :mismatch :reason :declared-decoder-sample-mismatch)
              (some #(= :returned (:kind %)) negatives) (assoc result :status :mismatch :reason :outside-decoder-sample-returned)
              (some #(= :unexpected-exception (:kind %)) negatives) (assoc result :status :unsupported :reason :outside-decoder-sample-unexpected-exception)
              :else (assoc result :status :matched :decoder-observation :matched-finite-samples)))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-issued-decoder-sample-source))))))

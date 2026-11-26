(ns gesso.assurance.reference-mapped-decoder
  "Finite observation and closed construction binding for issued mapped decoders."
  (:require [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction])
  (:import [java.lang.ref WeakReference]
           [java.util WeakHashMap]))

(def profile :issued-mapped-decoder-samples-v1)

(defn- case-key [record]
  (let [{:keys [principal operation arguments]} (:gesso.command/binding
                                                  (resolution/require-record record))]
    [(identity/raw-value principal) operation arguments (:gesso.command/outcome record)]))

(def closed-profile :issued-mapped-closed-decoder-v1)
(def ^:private ^WeakHashMap closed-decoders (WeakHashMap.))

(defn- construction-source [source]
  {:inductive-source {:catalog (get-in source [:inductive-source :catalog])}
   :target-map (:target-map source)})

(defn build-closed!
  "Construct a local exact finite decoder from the original mapped catalog
   and concrete declarations. Register the actual callable and immutable
   construction inputs, not a caller-supplied decoder description. No
   admission, authentication or physical execution is implied."
  [source declarations]
  (let [original (construction-source source)
        {:keys [cases] :as derived} (outcome/derive-cases! original declarations)
        decode! (fn [record]
                  (let [k (case-key record)]
                    (if (contains? cases k)
                      (get cases k)
                      (throw (ex-info "Outside closed mapped outcome cases"
                                      {:reason :undeclared-mapped-outcome})))))]
    (locking closed-decoders
      (.put closed-decoders decode! {:callable (WeakReference. decode!)
                                    :source original :declarations declarations
                                    :derived derived}))
    decode!))

(defn compare-issued-closed!
  "Check that the exact decoder captured by the issued admission was built
   from this catalog, target map and concrete declarations. Re-derive the
   cases, then recheck the issued source and its live induction handles.
   This is local construction provenance, not a proof of translation,
   authentication, installed executor use or universal provider refinement."
  [admission source declarations]
  (let [base {:profile closed-profile :authority :none :certificate :not-issued
              :provider-refinement :not-established :outcome-provenance :not-established
              :universal-decoder-correspondence :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (let [decode! (:decode-outcome! source)
            entry (when (fn? decode!) (locking closed-decoders (.get closed-decoders decode!)))]
        (cond
          (not (and entry (identical? decode! (.get ^WeakReference (:callable entry)))))
          (fail :unsupported :unregistered-closed-decoder)
          (not (and (= (construction-source source) (:source entry))
                    (= declarations (:declarations entry))))
          (fail :mismatch :changed-closed-decoder-construction)
          (not= (:derived entry) (outcome/derive-cases! source declarations))
          (fail :mismatch :changed-closed-decoder-cases)
          :else
          (let [description (induction/require-current-mapped-admission! admission source)]
            (merge base (select-keys description [:catalog-fingerprint :induction-fingerprint])
                   {:status :matched :closed-decoder-binding :matched-issued-closed-construction
                    :declared-case-count (count (get-in entry [:derived :cases]))}))))
      (catch clojure.lang.ExceptionInfo e
        (fail :unsupported (or (:reason (ex-data e)) :invalid-issued-closed-decoder-source))))))

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

(ns gesso.assurance.reference-recovery-trajectory
  "Bounded linkage of a winning commit, competing abort and recovery commit."
  (:require [gesso.assurance.reference-temporal :as temporal]
            [gesso.model.command-resolution :as resolution]))

(def profile :selected-retained-recovery-linkage-v1)

(defn- candidate-record! [tx]
  (let [puts (filter #(and (vector? %) (= [:put-docs :gesso_commands]
                                          (vec (take 2 %)))) (:ops tx))]
    (when (and (= 1 (count puts)) (= 3 (count (first puts))))
      (resolution/require-record (nth (first puts) 2)))))

(defn compare-selected!
  "Check adjacent selected retained steps and command-journal linkage.
   Expected bindings and outcomes are supplied premises. This is a pure
   replay diagnostic, not a provider bracket or abstract relation check."
  [inputs selected expected]
  (let [base {:profile profile :authority :none :certificate :not-issued
              :provider-refinement :not-established}
        fail #(assoc base :status %1 :reason %2)]
    (try
      (if-not (and (vector? inputs) (nil? (meta inputs)) (<= 4 (count inputs) 256)
                   (every? #(and (map? %) (nil? (meta %))
                                 (= #{:tx-id :system-time :ops} (set (keys %)))
                                 (integer? (:tx-id %)) (<= 0 (:tx-id %) (dec Long/MAX_VALUE))) inputs)
                   (apply < (map :tx-id inputs))
                   (vector? selected) (nil? (meta selected)) (= 3 (count selected))
                   (map? expected) (= #{:winner :abort :recovery} (set (keys expected))))
        (fail :unsupported :invalid-recovery-linkage-source)
        (let [start (first (keep-indexed #(when (= %2 (first selected)) %1) inputs))]
          (if-not (and (some? start) (pos? start) (<= (+ start 3) (count inputs))
                       (= selected (subvec inputs start (+ start 3))))
            (fail :mismatch :nonadjacent-recovery-selection)
            (let [[winning abort recovery] selected
                  records (mapv candidate-record! selected)
                  [w a r] records
                  wb (:gesso.command/binding w) ab (:gesso.command/binding a)
                  rb (:gesso.command/binding r)
                  w-id (:xt/id w) a-id (:xt/id a)
                  replay (temporal/replay (mapv #(dissoc % :tx-id) (subvec inputs 0 (+ start 3))))
                  history (:history replay)]
              (cond
                (not (and (every? some? records) (= records (mapv expected [:winner :abort :recovery]))
                          (= ab rb) (not= w-id a-id) (not= (:principal wb) (:principal ab))
                          (= (select-keys wb [:operation :arguments]) (select-keys ab [:operation :arguments]))
                          (not= (:gesso.command/outcome a) (:gesso.command/outcome r))))
                (fail :mismatch :recovery-command-linkage-mismatch)
                (not= :predicted (:status replay))
                (fail :unsupported :unsupported-recovery-linkage-replay)
                (not= [true false true] (mapv :committed? (subvec history start (+ start 3))))
                (fail :mismatch :recovery-commitment-sequence-mismatch)
                (not= (:segments (nth history start)) (:segments (nth history (inc start))))
                (fail :mismatch :recovery-abort-effects-mismatch)
                :else
                (let [journal (fn [system valid] (get (temporal/visible-at replay system valid) :gesso_commands {}))
                      wt (:system-time winning) at (:system-time abort) rt (:system-time recovery)
                      initial (journal (:system-time (nth inputs (dec start))) wt)
                      won (journal wt wt) aborted (journal at at)
                      recovered (journal rt rt)]
                  (if (and (not-any? #(contains? initial %) [w-id a-id])
                           (= won (assoc initial w-id w))
                           (= aborted won)
                           (= recovered (assoc won a-id r)))
                    (assoc base :status :matched :recovery-linkage :matched-retained-journal-trajectory
                           :selected-tx-ids (mapv :tx-id selected))
                    (fail :mismatch :recovery-journal-trajectory-mismatch))))))))
      (catch clojure.lang.ExceptionInfo _
        (fail :unsupported :invalid-recovery-command-record)))))

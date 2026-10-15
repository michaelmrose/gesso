(ns gesso.model.formal-observation
  "Pure, generic comparison of controlled authoritative observations with a
   source-derived finite prepared-transaction machine.

   The physical collector supplies a prestate, one definitive status and
   fresh document snapshot per submission, and a terminal observation. The
   caller supplies the exact admitted plans that were submitted. This layer
   never writes to XTDB or invokes Recife; its predicates can be called by
   a model-checker invariant or by ordinary JVM tests.

   A nonmatching checker branch is not evidence of correspondence: prefix
   predicates intentionally ignore alternate execution orders. Call
   observed-path-report to check the complete ACTUAL order independently,
   and use the model checker to explore all generated orders and invariants.

   Passing comparisons establish agreement only for supplied serial traces.
   They do not establish provenance, concurrent XTDB refinement, application
   authorization, fairness, or general implementation correctness."
  (:require [clojure.walk :as walk]
            [gesso.model.formal-transition :as formal]))

(def assurance-scope
  {:subject :gesso.model.formal-observation/controlled-prefix-v1
   :derived-from #{:gesso.model.formal-transition/prepared-serial-v1
                   :gesso.model.resolution-observation/direct-xtdb-v1}
   :checked #{:physical-trace-completeness
              :physical-prestate-and-poststate-chain
              :reference-transition-for-observed-order
              :per-prefix-status-and-document-correspondence
              :terminal-status-and-document-correspondence}
   :assumed #{:exact-prepared-plans-used-in-physical-submissions
              :exclusive-control-of-observed-documents
              :consistent-series-of-xtdb-reads
              :definitive-reported-execute-tx-results}
   :not-established #{:authenticated-physical-observation-provenance
                      :soundness-of-model-checker-state-space-exploration
                      :unbounded-or-concurrent-xtdb-refinement
                      :application-policy-or-authorization
                      :browser-or-network-correspondence}})

(defn- fail! [kind message details]
  (throw (ex-info message {:error/type ::invalid-formal-observation
                           :error/kind kind
                           :error/details details})))

(defn- values-of-kind [pred data]
  (let [found (volatile! #{})]
    (walk/postwalk (fn [x] (when (pred x) (vswap! found conj x)) x) data)
    (sort-by pr-str @found)))

(defn- lossy-keyword? [x]
  ;; Recife 0.22.0 canonicalizes underscores in keywords crossing TLC.
  (and (keyword? x)
       (or (some-> (namespace x) (.contains "_"))
           (.contains (name x) "_"))))

(defn- token-map [values]
  (let [strings (values-of-kind string? values)
        keywords (values-of-kind lossy-keyword? values)
        all-keywords (set (values-of-kind keyword? values))
        allocate (fn [originals kind taken]
                   (loop [remaining originals index 0 used taken result {}]
                     (if (empty? remaining)
                       result
                       (let [candidate (keyword "gessoformalcodec"
                                                (str kind index))]
                         (if (contains? used candidate)
                           (recur remaining (inc index) used result)
                           (recur (rest remaining) (inc index)
                                  (conj used candidate)
                                  (assoc result (first remaining) candidate)))))))
        string-tokens (allocate strings "string" all-keywords)
        keyword-tokens (allocate keywords "keyword"
                                 (into all-keywords (vals string-tokens)))
        forward (merge string-tokens keyword-tokens)]
    {:forward forward
     :backward (into {} (map (fn [[source token]] [token source])) forward)}))

(defn- codec-for [values]
  (let [{:keys [forward backward]} (token-map values)]
    {:encode (fn [value]
               (walk/postwalk (fn [x]
                                (cond
                                  (string? x)
                                  (or (get forward x)
                                      (fail! :unknown-checker-string
                                             "String not admitted to finite checker vocabulary."
                                             {:string x}))
                                  (keyword? x) (get forward x x)
                                  :else x)) value))
     :decode (fn [value]
               (walk/postwalk (fn [x] (get backward x x)) value))}))

(defn prepared-case
  "Admit exact prepared plans and build a reversible finite TLC value codec.

   Physical evidence must have :initial-state, :submission-order,
   :submission-trace, and :observations. Intentionally do NOT reject
   internally inconsistent evidence here: completeness and correspondence
   predicates must be capable of producing TLC counterexamples for it.
   Plans must be supplied by the caller, never invented from observations.
   No provenance or observation-integrity guarantee is conferred."
  [physical plans]
  (when-not (and (map? physical)
                 (contains? physical :initial-state)
                 (contains? physical :submission-order)
                 (contains? physical :submission-trace)
                 (contains? physical :observations))
    (fail! :missing-observations
           "Expected physical prestate, order, submission trace and terminal observations."
           {:keys (when (map? physical) (set (keys physical)))}))
  (let [subject (formal/prepared-subject (:initial-state physical) plans)
        codec (codec-for [(:initial-state physical)
                          plans
                          (:submission-trace physical)
                          (:observations physical)])]
    {:subject subject :physical physical :codec codec :scope assurance-scope}))

(defn- transform-model [state transform]
  (-> state
      (update ::formal/documents transform)
      (update ::formal/observations transform)))

(defn initial-state
  "Model state with encoded Gesso-owned values and an observed-order tracker.
   Recife may attach unrelated process bookkeeping fields afterward."
  [case]
  (-> (formal/initial-state (:subject case))
      (transform-model (get-in case [:codec :encode]))
      (assoc ::order [])))

(defn advance
  "Perform the actual tx-model transition for a pending plan index.

   The model state stays codec-encoded on the TLC boundary, while Gesso's
   reference transition receives and returns original admitted Clojure data.
   Re-attempting an already-consumed action stutters (for fixed Recife procs);
   unknown indices are always rejected."
  [case state index]
  (let [subject (:subject case)
        actions (:actions subject)]
    (when-not (and (integer? index) (<= 0 index)
                   (< index (count actions)))
      (fail! :unknown-action "Unknown formal plan index." {:index index}))
    (let [action (nth actions index)]
      (if (some #{action} (formal/enabled-actions subject state))
        (-> (formal/transition subject
                             (transform-model state (get-in case [:codec :decode]))
                             action)
          (transform-model (get-in case [:codec :encode]))
            (update ::order conj index))
        state))))

(defn reference-rejection-atomic?
  "Invariant independent of physical evidence; applies to every modeled order."
  [state]
  (every? (fn [{:keys [status before after]}]
            (or (not= status :rejected) (= before after)))
          (::formal/observations state)))

(defn evidence-complete?
  "Check physical index permutation, trace length, contiguous physical
   states, definitive statuses, indexed terminal statuses and final state.
   Never accepts an empty trace as evidence for an admitted nonempty subject."
  [{:keys [subject physical]}]
  (let [order (:submission-order physical)
        trace (:submission-trace physical)
        observations (:observations physical)
        n (count (:actions subject))]
    (and (vector? order)
         (= n (count order))
         (= (set (range n)) (set order))
         (vector? trace)
         (= n (count trace))
         (every? map? trace)
         (= order (mapv :plan-index trace))
         (every? #{:committed :rejected} (map :status trace))
         (= (:initial-state physical) (:before (first trace)))
         (every? true? (map (fn [a b] (= (:after a) (:before b)))
                             trace (rest trace)))
         (map? observations)
         (vector? (:statuses observations))
         (= n (count (:statuses observations)))
         (= (reduce (fn [statuses {:keys [plan-index status]}]
                      (assoc statuses plan-index status))
                    (vec (repeat n nil)) trace)
            (:statuses observations))
         (= (:after (peek trace)) (:state observations)))))

(defn prefix-corresponds?
  "Recife invariant: compare a matching physical prefix to the exact
   source-derived reference observations. Alternate modeled orders return
   true intentionally; evidence-complete? and observed-path-report must
   also be checked to rule out vacuous success."
  [{:keys [physical codec]} state]
  (let [order (::order state)
        physical-prefix (vec (take (count order) (:submission-trace physical)))]
    (if (= order (mapv :plan-index physical-prefix))
      (and (<= (count order) (count (:submission-order physical)))
           (= (count order) (count (::formal/observations state)))
           (every? true?
                   (map (fn [modeled observed]
                          (and (= (:plan-index modeled) (:plan-index observed))
                               (= (:status modeled) (:status observed))
                               (= (:before modeled) ((:encode codec) (:before observed)))
                               (= (:after modeled) ((:encode codec) (:after observed)))))
                        (::formal/observations state) physical-prefix))
           (= (::formal/documents state)
              ((:encode codec)
               (if (seq physical-prefix)
                 (:after (peek physical-prefix))
                 (:initial-state physical)))))
      true)))

(defn terminal-corresponds?
  "Recife invariant for the fully consumed physical order."
  [{:keys [physical codec subject]} state]
  (if (and (empty? (::formal/pending state))
           (= (:submission-order physical) (::order state)))
    (let [indexed-statuses (reduce (fn [result {:keys [plan-index status]}]
                                     (assoc result plan-index status))
                                   (vec (repeat (count (:actions subject)) nil))
                                   (::formal/observations state))]
      (and (= indexed-statuses (get-in physical [:observations :statuses]))
           (= (::formal/documents state)
              ((:encode codec) (get-in physical [:observations :state])))))
    true))

(defn observed-path-report
  "Replay EXACTLY the physical order through Gesso's reference transition,
   comparing every prefix and the terminal observation without invoking TLC.

   This prevents a model checker limited to alternate paths from producing
   a misleading vacuous correspondence result. :ok is a deterministic
   finite replay result, NOT a mechanically checked theorem or independent
   proof of physical observation provenance."
  [case]
  (if-not (evidence-complete? case)
    {:status :evidence-incomplete :scope assurance-scope}
    (let [final (reduce (fn [state index] (advance case state index))
                        (initial-state case)
                        (get-in case [:physical :submission-order]))]
      {:status (if (and (reference-rejection-atomic? final)
                        (prefix-corresponds? case final)
                        (terminal-corresponds? case final))
                 :corresponds :mismatch)
       :checked-order (get-in case [:physical :submission-order])
       :scope assurance-scope})))

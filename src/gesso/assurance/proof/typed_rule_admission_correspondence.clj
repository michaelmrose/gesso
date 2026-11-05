(ns gesso.assurance.proof.typed-rule-admission-correspondence
  "Diagnostic S1 audit of the production typed-rule admission boundary (v1346).

   (audit-rule! {:readers #{:request/score}
                 :expression [:> [:read :request/score] [:literal 7]]}
                {:request/score {:type :integer}})

   Admission is checked by the actual rule/type checker. Independently, the
   v1344 branch-sensitive semantics checks the same *source* on three
   deliberately synthesized, well-typed partial states. This exercises the
   independent static checks on all branches and the interpreter on several
   meaningful runtime paths. It does NOT establish equivalence for every
   state, nor is a synthesized state an XTDB observation.

   This is a source-relative diagnostic: it does not issue certificates,
   amend admission, authenticate snapshots, or establish S1 soundness.

   A production-admitted source outside the independent interpreter's
   64-reader capacity is :not-checked, never silently reported as verified.
   Unexpected exceptions propagate instead of being mistaken for a rejection."
  (:require [gesso.assurance.rule-type :as production]
            [gesso.assurance.proof.typed-presence-semantics :as independent]))

(def audit-version 1)
(def audit-type :gesso.assurance.proof.typed-rule-admission-correspondence/audit)

(def ^:private independent-reader-limit 64)

(defn- production-rejection? [data]
  (contains? #{:gesso.assurance.rule/invalid-rule
               :gesso.assurance.rule-type/invalid-rule-type}
             (:error/type data)))

(defn- independent-rejection? [data]
  (or (true? (:typed-value-semantics/rejection data))
      (true? (:typed-presence-semantics/rejection data))))

(defn- attempt
  [f predicate]
  (try {:outcome :accepted :value (f)}
       (catch clojure.lang.ExceptionInfo e
         (let [data (ex-data e)]
           (if (predicate data)
             {:outcome :rejected :reason (:reason data) :at (:at data)}
             (throw e))))))

(defn- canonical-value
  [descriptor]
  (case (:type descriptor)
    :integer 0N
    :boolean false
    :string ""
    :keyword :gesso.assurance/probe
    :enum (first (sort-by str (:values descriptor)))))

(defn- witness-state
  [types witness]
  (into {}
        (keep (fn [[reader descriptor]]
                (cond
                  (and (= witness :optional-absent)
                       (true? (:optional? descriptor))) nil
                  (and (= witness :nullable-nil)
                       (true? (:nullable? descriptor))) [reader nil]
                  :else [reader (canonical-value descriptor)])))
        types))

(defn- report
  [declaration reader-types status extra]
  (merge {:type audit-type
          :version audit-version
          :status status
          :source-declaration declaration
          :source-reader-types reader-types
          :checking :production-admission-vs-independent-typed-semantics
          :scope :three-synthetic-typed-states
          :proof-status :not-proved
          :admission :not-admitted
          :runtime-enforcement :not-established
          :snapshot-provenance :not-established
          :jvm-cljs-equivalence :not-established
          :authority :none}
         extra))

(defn audit-rule!
  "Check production acceptance of the *original* declaration and compare
   successful admission with independent branch-sensitive semantic checks.

   Outcomes:
   :production-rejected  -- source was not admitted by production; independent
                            semantics are intentionally not invoked on it.
   :not-checked          -- source is admitted but outside the independent
                            reader-count capacity; no correspondence claim.
   :disagreement         -- source is admitted but independent semantics
                            rejects a supported synthetic typed state.
   :witness-supported    -- both accept source and the independent evaluator
                            interprets three synthetic states. This is bounded
                            diagnostic evidence, NOT universal correspondence.

   The original checked source and each synthetic state are returned for
   diagnostic reproducibility. No supplied acceptance report is trusted."
  [declaration reader-types]
  (let [admission (attempt #(production/check-rule-type! declaration reader-types)
                           production-rejection?)]
    (if (= :rejected (:outcome admission))
      (report declaration reader-types :production-rejected
              {:production-reason (:reason admission)
               :production-at (:at admission)
               :independent :not-executed})
      (let [checked (:value admission)
            source-readers (:readers checked)]
        (when-not (and (= (:expression declaration) (:expression checked))
                       (= (:readers declaration) source-readers)
                       (= reader-types (:reader-types checked))
                       (= :not-admitted (:admission checked))
                       (= :none (:authority checked)))
          (throw (ex-info "Production typed-rule report did not retain source."
                          {:error/type ::production-source-mismatch})))
        (if (> (count reader-types) independent-reader-limit)
          (report declaration reader-types :not-checked
                  {:reason :independent-reader-limit
                   :maximum independent-reader-limit
                   :actual (count reader-types)
                   :production :accepted
                   :independent :not-executed})
          (let [checks (mapv
                        (fn [witness]
                          (let [state (witness-state reader-types witness)
                                result (attempt
                                        #(independent/evaluate-expression!
                                          (:expression declaration)
                                          reader-types state)
                                        independent-rejection?)]
                            (if (= :rejected (:outcome result))
                              {:witness witness :state state
                               :outcome :rejected
                               :reason (:reason result) :at (:at result)}
                              (let [value (:value result)]
                                (when-not (and (= independent/result-type (:type value))
                                               (= independent/semantics-version (:version value))
                                               (= (:expression declaration)
                                                  (:expression value))
                                               (= source-readers (:source-readers value))
                                               (instance? Boolean (:value value))
                                               (= :not-proved (:proof-status value))
                                               (= :not-admitted (:admission value))
                                               (= :none (:authority value)))
                                  (throw (ex-info "Independent semantics report did not retain source."
                                                  {:error/type ::independent-source-mismatch
                                                   :witness witness})))
                                {:witness witness :state state
                                 :outcome :evaluated :value (:value value)}))))
                        [:all-present :optional-absent :nullable-nil])
                rejected (first (filter #(= :rejected (:outcome %)) checks))]
            (report declaration reader-types
                    (if rejected :disagreement :witness-supported)
                    {:production :accepted
                     :independent (if rejected :rejected :evaluated)
                     :witnesses checks
                     :reason (when rejected :independent-rejected)})))))))

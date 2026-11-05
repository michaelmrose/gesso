(ns gesso.assurance.proof.typed-portability-correspondence
  "v1348: source-bound JVM witness comparison between the production S1 type
   checker, independent branch-sensitive typed semantics (v1344), and the
   portable CLJC probe (v1325).

   (audit-witness!
     {:rule {:readers #{:request/score}
             :expression [:> [:read :request/score] [:literal 7]]}
      :reader-types {:request/score {:type :integer}}
      :bindings {:request/score 8}})

   The JVM can represent integers which JavaScript Number cannot. Before any
   portable evaluator is called, this audit independently rejects integer
   literals (including unreachable branches and set members) and bindings
   outside +/- (2^53 - 1). It never clips, rounds, or translates them.

   A matching JVM witness is a *candidate* for two-host comparison. It is
   NOT evidence that CLJS executed, that the two hosts agree, or that the
   snapshot is authoritative. The future independent Node/CLJS harness must
   execute the identical source and compare results to establish bounded
   cross-host correspondence.

   The full mathematical-integer rule language remains JVM-specific unless
   an explicitly supported portable subset is selected. This module is
   diagnostic only and never produces a certificate or admission handle."
  (:require [gesso.assurance.rule-type :as production]
            [gesso.assurance.proof.typed-presence-semantics :as independent]
            [gesso.assurance.proof.jvm-cljs-semantics :as portable]))

(def audit-version 1)
(def audit-type :gesso.assurance.proof.typed-portability-correspondence/witness)
(def ^:private max-portable-readers 16)
(def ^:private max-independent-readers 64)

(defn- rejected?
  [data]
  (contains? #{:gesso.assurance.rule/invalid-rule
               :gesso.assurance.rule-type/invalid-rule-type}
             (:error/type data)))

(defn- independent-rejected? [data]
  (or (true? (:typed-value-semantics/rejection data))
      (true? (:typed-presence-semantics/rejection data))))

(defn- attempt [f recognized?]
  (try {:outcome :accepted :value (f)}
       (catch clojure.lang.ExceptionInfo e
         (let [data (ex-data e)]
           (if (recognized? data)
             {:outcome :rejected :reason (:reason data) :at (:at data)
              :status (:status data)}
             (throw e))))))

(defn- unexpected! [reason data]
  (throw (ex-info "Typed portability checker received inconsistent evidence."
                  (assoc data :error/type ::invalid-evidence :reason reason))))

(defn- unsafe-integer?
  [v]
  (and (integer? v)
       (not (<= (- portable/max-safe-integer)
                v portable/max-safe-integer))))

(defn- expression-unsafe-integers
  "Find every unsafe numeric literal, even under a short-circuited branch.
   Only :literal nodes own scalar or set values; reader IDs and structure
   must not be interpreted as literal values."
  [expression]
  (letfn [(visit [expr at]
            (let [[operator argument] expr]
              (if (= :literal operator)
                (let [members (if (set? argument) argument [argument])]
                  (vec (for [member members :when (unsafe-integer? member)]
                         {:at (conj at 1) :value member})))
                (mapcat (fn [i]
                          (let [child (nth expr i)]
                            (if (vector? child)
                              (visit child (conj at i))
                              [])))
                        (range 1 (count expr))))))]
    (vec (visit expression [:rule :expression]))))

(defn- unsafe-integer-inputs
  [expression bindings]
  (let [literals (expression-unsafe-integers expression)
        bound (for [[reader value] bindings
                    :when (unsafe-integer? value)]
                {:at [:bindings reader] :value value})]
    ;; Stable ordering matters for comparing emitted case reports.
    (vec (sort-by (comp pr-str :at) (concat literals bound)))))

(defn- report [source status extra]
  (merge {:type audit-type
          :version audit-version
          :status status
          :source source
          :checking :production-vs-independent-vs-portable-jvm-witness
          :integer-scope :js-safe-integers-only
          :cross-host-correspondence :not-established
          :snapshot-provenance :not-established
          :proof-status :not-proved
          :admission :not-admitted
          :authority :none
          :transaction-authority :none}
         extra))

(defn audit-witness!
  "Compare the three implementations on *one supplied JVM state*.

   :production-rejected  the source rule is rejected by actual S1 typing.
   :not-checked          an admitted rule exceeds a checker reader limit.
   :invalid-witness      the independent typed state/semantics rejects it.
   :nonportable          a source literal or binding is outside JS-safe ints.
   :portable-unsupported the CLJC probe rejects a supported typed source.
   :disagreement         independent and portable JVM interpretations differ.
   :witness-agreement    both evaluate the same Boolean result on the JVM.

   Neither :witness-agreement nor :nonportable is a proof of cross-runtime
   equivalence. The returned original source may be replayed under CLJS by
   a separate harness; no copyable result is trusted as certification."
  [source]
  (when-not (and (map? source) (= #{:rule :reader-types :bindings}
                                  (set (keys source))))
    (unexpected! :invalid-source-envelope {}))
  (let [{:keys [rule reader-types bindings]} source
        admission (attempt #(production/check-rule-type! rule reader-types)
                           rejected?)]
    (if (= :rejected (:outcome admission))
      (report source :production-rejected
              {:reason (:reason admission) :at (:at admission)
               :production :rejected :independent :not-executed
               :portable :not-executed})
      (let [checked (:value admission)
            _ (when-not (and (= rule (select-keys checked [:readers :expression]))
                             (= reader-types (:reader-types checked))
                             (= :not-admitted (:admission checked))
                             (= :none (:authority checked)))
                (unexpected! :production-source-mismatch {}))
            count-readers (count (:readers rule))]
        (cond
          (> count-readers max-independent-readers)
          (report source :not-checked
                  {:reason :independent-reader-limit
                   :actual count-readers :maximum max-independent-readers
                   :production :accepted :independent :not-executed
                   :portable :not-executed})

          (> count-readers max-portable-readers)
          (report source :not-checked
                  {:reason :portable-reader-limit
                   :actual count-readers :maximum max-portable-readers
                   :production :accepted :independent :not-executed
                   :portable :not-executed})

          :else
          (let [ind (attempt #(independent/evaluate-expression!
                              (:expression rule) reader-types bindings)
                             independent-rejected?)]
            (if (= :rejected (:outcome ind))
              (report source :invalid-witness
                      {:reason (:reason ind) :at (:at ind)
                       :production :accepted :independent :rejected
                       :portable :not-executed})
              (let [ir (:value ind)
                    _ (when-not (and (= independent/result-type (:type ir))
                                     (= independent/semantics-version (:version ir))
                                     (= (:expression rule) (:expression ir))
                                     (= (:readers rule) (:source-readers ir))
                                     (instance? Boolean (:value ir))
                                     (= :not-proved (:proof-status ir))
                                     (= :not-admitted (:admission ir))
                                     (= :none (:authority ir)))
                        (unexpected! :independent-source-mismatch {}))
                    unsafe (unsafe-integer-inputs (:expression rule) bindings)]
                (if (seq unsafe)
                  (report source :nonportable
                          {:reason :unsafe-integer-precision
                           :unsafe-integers unsafe
                           :production :accepted :independent :evaluated
                           :independent-value (:value ir)
                           :portable :not-executed})
                  (let [pr (attempt #(portable/probe! source)
                                    #(true? (:jvm-cljs-semantics/failure %)))]
                    (if (= :rejected (:outcome pr))
                      (report source :portable-unsupported
                              {:reason (:reason pr) :at (:at pr)
                               :probe-status (:status pr)
                               :production :accepted :independent :evaluated
                               :independent-value (:value ir)
                               :portable :rejected})
                      (let [p (:value pr)]
                        (when-not (and (= portable/report-type (:type p))
                                       (= portable/probe-version (:version p))
                                       (= :portable-single-host-result (:status p))
                                       (= :exact-js-safe-integers-only (:integer-scope p))
                                       (= :not-established-by-single-host
                                          (:jvm-cljs-correspondence p))
                                       (= :none (:authority p))
                                       (= :none (:transaction-authority p))
                                       (instance? Boolean (:value p)))
                          (unexpected! :portable-report-mismatch {}))
                        (report source
                                (if (= (:value ir) (:value p))
                                  :witness-agreement :disagreement)
                                {:production :accepted
                                 :independent :evaluated
                                 :portable :evaluated
                                 :independent-value (:value ir)
                                 :portable-value (:value p)})))))))))))))

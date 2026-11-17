(ns gesso.assurance.typed-induction-dependency-closure-check
  "Opt-in Typed Clojure checks for source-relative induction graph handoff.

   The original exact-graph! must return a nominal CheckedGraph, and the
   original closure-entry constructor must place that handle in the exact
   structural closure descriptor. Registry issuance remains inside the
   existing source-relative checker; no new function may mint a closure.

   Types do NOT prove the CheckedGraph is live, the catalog is faithful, or
   external obligations are discharged. Source audits below bind our checks
   to the actual validation-to-registration path, which remains runtime.

   The registered closure consumer is checked only at its final graph
   handoff: the original registry lookup remains opaque, so this checker
   assumes the issued closure-entry HMap at that narrowed helper boundary.
   The full source-relative freshness algorithm is runtime-tested, not typed."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.induction-dependency :as induction]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.induction-dependency)
(def ^:private checked-graph 'gesso.assurance.dependency.CheckedGraph)
(def ^:private admission-handle 'gesso.assurance.admission.AdmissionHandle)
(def ^:private closure-handle
  'gesso.assurance.proof.induction_dependency.InductionDependencyHandle)
(def ^:private graph-argument
  '(typed.clojure/HMap
    :mandatory {:nodes (typed.clojure/Vec
                        (typed.clojure/HMap
                         :mandatory {:depends-on (typed.clojure/Vec typed.clojure/Any)}
                         :complete? false))}
    :complete? false))
(def ^:private closure-entry
  '(typed.clojure/HMap
    :mandatory {:graph-handle gesso.assurance.dependency.CheckedGraph
                :graph-snapshot typed.clojure/Any
                :source-fingerprint typed.clojure/Str
                :induction-fingerprint typed.clojure/Any
                :catalog-fingerprint typed.clojure/Any
                :catalog-id typed.clojure/Any
                :operation-ids typed.clojure/Any
                :invariant-ids typed.clojure/Any
                :open-meta-obligations typed.clojure/Any
                :original-open-obligation-ids-by-operation typed.clojure/Any
                :fingerprint typed.clojure/Str
                :node-count typed.clojure/Int
                :edge-count typed.clojure/Int}
    :complete? true))

(defn- original-definitions []
  (let [file (or (io/resource "gesso/assurance/proof/induction_dependency.clj")
                 (throw (ex-info "Induction source not on classpath" {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader file))]
      (loop [acc []]
        (let [form (read {:eof ::end} reader)]
          (if (= ::end form) acc (recur (conj acc form))))))))

(defn- original [forms function]
  (let [matching (filter #(and (seq? %)
                               (#{'defn 'defn-} (first %))
                               (= function (second %))) forms)]
    (when-not (= 1 (count matching))
      (throw (ex-info "Missing/duplicate original induction closure function"
                      {:function function :matches (count matching)})))
    (first matching)))

(defn- check! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original induction closure failed static checking"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Incorrect induction graph handoff escaped checker"
                      {:label label :actual kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype checked-graph '[]))
  (eval (list 'typed.clojure/ann-datatype admission-handle '[]))
  (eval (list 'typed.clojure/ann-datatype closure-handle '[]))
  (doseq [[name inputs]
          [['gesso.assurance.dependency/check-graph!
            ['typed.clojure/Any '-> checked-graph]]
           ['gesso.assurance.admission/admit-subject!
            ['typed.clojure/Any '-> admission-handle]]
           ['gesso.assurance.proof.induction-dependency/exact-graph!
            ['typed.clojure/Any 'typed.clojure/Any '-> checked-graph]]
           ['gesso.assurance.proof.induction-dependency/closure-entry
            [checked-graph 'typed.clojure/Any 'typed.clojure/Str
             'typed.clojure/Any 'typed.clojure/Str graph-argument
             '-> closure-entry]]
           ['gesso.assurance.dependency/require-current-graph!
            [checked-graph 'typed.clojure/Any '-> checked-graph]]
           ['gesso.assurance.proof.induction-dependency/revalidate-closure-graph!
            [closure-entry 'typed.clojure/Any '-> checked-graph]]]]
    (eval (list 'typed.clojure/ann name
                (list 'typed.clojure/IFn inputs)))))

(defn- source-linked-handoff! [forms]
  ;; One exactly source-relative check is essential: the original public
  ;; checker must still derive the graph handle by exact source comparison,
  ;; pass it into the checked entry constructor, and register the resulting
  ;; entry. Merely type-checking an unused helper would be misleading.
  (let [definition (original forms 'check-induction-dependencies!)
        hits (atom {})
        expected {'exact-graph! '(exact-graph! graph current-graph)
                  'closure-entry '(closure-entry graph-handle snap
                                                source-fingerprint report
                                                identity graph)
                  '.put '(.put live-handles handle entry)}]
    (walk/postwalk (fn [form]
                     (when (and (seq? form) (contains? expected (first form)))
                       (swap! hits update (first form) (fnil conj []) form))
                     form)
                   definition)
    (doseq [[name expression] expected]
      (when-not (= [expression] (get @hits name))
        (throw (ex-info "Production graph handoff/registration changed"
                        {:step name :expected expression
                         :actual (get @hits name)}))))
    ;; Require the graph-handle binding itself to come from exact-graph!;
    ;; simple textual occurrence of both calls is insufficient evidence.
    (let [bindings (-> definition last second)
          pairs (partition 2 bindings)
          by-name (into {} (map vec pairs))]
      (when-not (and (vector? bindings)
                     (= (get expected 'exact-graph!) (get by-name 'graph-handle))
                     (= (get expected 'closure-entry) (get by-name 'entry)))
        (throw (ex-info "Closure entry is not bound to validated graph"
                        {:graph-source (get by-name 'graph-handle)
                         :entry-source (get by-name 'entry)}))))
    true))

(defn- source-linked-revalidation! [forms]
  ;; A check of an unused private helper would be vacuous. Require the
  ;; production consumer to retrieve by its incoming handle, rederive
  ;; current source, check both source and graph freshness, and THEN invoke
  ;; the typed helper before returning the original closure handle.
  (let [definition (original forms 'require-current-induction-dependencies!)
        outer (last definition)
        outer-bindings (when (and (seq? outer) (= 'let (first outer)))
                         (second outer))
        inner (when (and (seq? outer) (= 'handle (last outer)))
                (nth outer (- (count outer) 2)))
        inner-bindings (when (and (seq? inner) (= 'let (first inner)))
                         (second inner))
        outer-map (when (vector? outer-bindings)
                    (into {} (map vec (partition 2 outer-bindings))))
        inner-map (when (vector? inner-bindings)
                    (into {} (map vec (partition 2 inner-bindings))))]
    (when-not (and (= '[handle current-catalog current-graph]
                      (some #(when (vector? %) %) definition))
                   (= '(registered handle) (get outer-map 'prior))
                   (= '(when-not prior
                         (fail! :unrecognized-dependency-handle [:handle] {}))
                      (nth outer 2))
                   (= '(semantic-graph! current-catalog)
                      (get inner-map '{:keys [graph report source-fingerprint]}))
                   (= '(graph-identity graph) (get inner-map 'now))
                   (= '(revalidate-closure-graph! prior current-graph)
                      (last inner))
                   (= 'handle (last outer)))
      (throw (ex-info "Induction closure consumer lost source-bound revalidation"
                      {:outer-bindings outer-bindings
                       :inner-bindings inner-bindings
                       :inner-final (some-> inner last)})))
    (let [guards (filter #(and (seq? %) (= 'when-not (first %)))
                         (drop 2 inner))]
      (when-not (= ['(when-not (and (= (:graph-snapshot prior) now)
                                   (= (:source-fingerprint prior) source-fingerprint)
                                   (= (:induction-fingerprint prior)
                                      (:fingerprint report))
                                   (= (:catalog-fingerprint prior)
                                      (:catalog-fingerprint report)))
                        (fail! :stale-source-dependency-closure [:catalog] {}))
                    '(when-not (= now (graph-identity current-graph))
                       (fail! :source-dependency-content-mismatch [:graph] {}))]
                   (vec guards))
        (throw (ex-info "Induction source/graph freshness guards changed"
                        {:guards guards}))))
    true))

(defn- mutate-graph-handoff [source replacement]
  ;; Change ONLY the original handle expression consumed by the original
  ;; generic graph revalidator. Any other structural changes fail closed.
  (let [target '(:graph-handle prior)
        count* (atom 0)
        modified (walk/postwalk
                  (fn [x]
                    (if (= target x)
                      (do (swap! count* inc) replacement)
                      x))
                  source)]
    (when-not (= 1 @count*)
      (throw (ex-info "Original closure graph-handle field moved"
                      {:matches @count*})))
    modified))

(defn- check-revalidation! [checker forms]
  (let [source (original forms 'revalidate-closure-graph!)]
    (check! checker :original-live-graph-revalidator source)
    (source-linked-revalidation! forms)
    (doseq [[label substituted]
            [[:raw-entry 'prior]
             [:raw-map {}]
             [:wrong-handle '(admission/admit-subject! {})]
             [:source-fingerprint '(:source-fingerprint prior)]
             [:graph-snapshot '(:graph-snapshot prior)]
             [:missing-graph-slot '(:unknown prior)]
             [:wrong-count '(:node-count prior)]
             [:nil-handle nil]
             [:string-handle "graph"]]]
      (reject! checker [:revalidation label]
               (mutate-graph-handoff source substituted)))
    (doseq [[label invalid-return]
            [[:returned-raw-entry 'prior]
             [:returned-source '(:source-fingerprint prior)]
             [:returned-nil nil]]]
      (reject! checker [:revalidation label]
               (apply list (concat (butlast source) [invalid-return]))))
    ;; This composition is only a nominal category check. It never executes
    ;; check-graph!, closure-entry, or require-current-graph! under the checker.
    (check! checker :legitimate-checked-closure-call
            '(revalidate-closure-graph!
              (closure-entry (exact-graph! {} {}) {} "src" {} "id"
                             {:nodes []}) {}))
    (doseq [[label expr]
            [[:raw-closure-map '(revalidate-closure-graph! {} {})]
             [:subject-admission-instead-of-closure-entry
              '(revalidate-closure-graph! (admission/admit-subject! {}) {})]
             [:checked-graph-instead-of-closure-entry
              '(revalidate-closure-graph! (exact-graph! {} {}) {})]
             [:nil-closure-entry '(revalidate-closure-graph! nil {})]]]
      (reject! checker [:revalidation-call label] expr))
    true))

(defn- change-return [source alter]
  (let [old (last source)]
    (when-not (map? old)
      (throw (ex-info "Original closure descriptor shape changed" {})))
    (let [updated (alter old)]
      (when (= old updated)
        (throw (ex-info "Closure mutation failed to change source" {})))
      (apply list (concat (butlast source) [updated])))))

(defn- check-originals! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (let [source (original forms 'exact-graph!)
          last-expression (last source)
          expected '(dependency/check-graph! supplied)]
      (when-not (= expected last-expression)
        (throw (ex-info "Original graph validator no longer issues checked graph"
                        {:actual last-expression})))
      ;; Only the original delegated return is checked here. The preceding
      ;; source-relative graph equality/shape logic is still runtime code.
      (check! checker :exact-graph-return
              '(typed.clojure/ann-form
                (fn [supplied] (dependency/check-graph! supplied))
                (typed.clojure/IFn [typed.clojure/Any :->
                                    gesso.assurance.dependency.CheckedGraph])))
      (reject! checker :raw-exact-graph-return
               '(typed.clojure/ann-form
                 (fn [supplied] {})
                 (typed.clojure/IFn [typed.clojure/Any :->
                                     gesso.assurance.dependency.CheckedGraph]))))
    (check! checker :original-closure-entry (original forms 'closure-entry))
    (source-linked-handoff! forms)
    (check-revalidation! checker forms)
    (doseq [[label alter]
            [[:raw-graph-handle #(assoc % :graph-handle {})]
             [:wrong-handle-class #(assoc % :graph-handle
                                          '(gesso.assurance.admission/admit-subject! {}))]
             [:wrong-handle-value #(assoc % :graph-handle 'source-fingerprint)]
             [:missing-graph-handle #(dissoc % :graph-handle)]
             [:fake-authority #(assoc % :authority :granted)]
             [:fake-certificate #(assoc % :certificate :issued)]
             [:wrong-fingerprint #(assoc % :fingerprint nil)]
             [:bad-source-fingerprint #(assoc % :source-fingerprint 31)]
             [:missing-snapshot #(dissoc % :graph-snapshot)]
             [:invalid-node-count #(assoc % :node-count "eight")]
             [:invalid-edge-count #(assoc % :edge-count false)]]]
      (reject! checker label
               (change-return (original forms 'closure-entry) alter)))
    (check! checker :correct-nominal-composition
            '(closure-entry (exact-graph! {} {}) {} "source" {}
                            "identity" {:nodes []}))
    (doseq [[label expr]
            [[:raw-graph '(closure-entry {} {} "source" {} "id" {:nodes []})]
             [:admission-handle
              '(closure-entry
                (gesso.assurance.admission/admit-subject! {})
                {} "source" {} "id" {:nodes []})]
             [:bad-graph-nodes
              '(closure-entry (exact-graph! {} {}) {} "source" {}
                              "identity" {:nodes [{}]})]
             [:missing-graph-nodes
              '(closure-entry (exact-graph! {} {}) {} "source" {}
                              "identity" {})]]]
      (reject! checker label expr))
    true))

(defn- check-runtime! []
  ;; Neither direct constructor instances nor detached descriptor data may
  ;; bypass the source-relative local registry. No fabricated catalog passes
  ;; through the original full closure checker in this test.
  (let [forged (gesso.assurance.proof.induction_dependency.InductionDependencyHandle.)
        reason (try (induction/describe-induction-dependencies forged)
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e))))]
    (when (or (induction/checked-induction-dependencies? forged)
              (not= :unrecognized-dependency-handle reason))
      (throw (ex-info "Forged induction closure bypassed live registry"
                      {:reason reason})))
    (let [revalidation-error
          (try (induction/require-current-induction-dependencies!
                forged {} {})
               nil
               (catch clojure.lang.ExceptionInfo e
                 (:reason (ex-data e))))]
      (when-not (= :unrecognized-dependency-handle revalidation-error)
        (throw (ex-info "Forged closure passed revalidation registry"
                        {:reason revalidation-error}))))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (original-definitions)]
    (check-originals! checker forms)
    (check-runtime!)
    (println "Typed source-relative induction closure passed: original")
    (println "exact-graph return and complete closure-entry source checked;")
    (println "original handoff/registration and current-source guards audited;")
    (println "32 wrong-field, wrong-return and wrong-call cases rejected;")
    (println "forged closure handles rejected by description and revalidation.")
    (println "Registry contents are opaque; semantic source freshness and")
    (println "proof/runtime authority remain separate runtime obligations.")
    :ok))

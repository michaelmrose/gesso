(ns gesso.assurance.typed-induction-dependency-closure-check
  "Opt-in Typed Clojure checks for source-relative induction graph handoff.

   The original exact-graph! must return a nominal CheckedGraph, and the
   original closure-entry constructor must place that handle in the exact
   structural closure descriptor. Registry issuance remains inside the
   existing source-relative checker; no new function may mint a closure.

   Types do NOT prove the CheckedGraph is live, the catalog is faithful, or
   external obligations are discharged. Source audits below bind our checks
   to the actual validation-to-registration path, which remains runtime."
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
             '-> closure-entry]]]]
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
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (original-definitions)]
    (check-originals! checker forms)
    (check-runtime!)
    (println "Typed source-relative induction closure passed: original")
    (println "exact-graph return and complete closure-entry source checked;")
    (println "original handoff/registration audited; 16 incorrect type cases")
    (println "rejected and forged closure handle rejected at runtime.")
    (println "Source-closure validity remains a runtime obligation, not a")
    (println "theorem or installed authorization claim.")
    :ok))

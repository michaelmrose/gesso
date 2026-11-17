(ns gesso.assurance.typed-application-dependency-closure-check
  "Opt-in Typed Clojure checking of source-relative application graph closure.

   The original revalidator must consume distinct, registry-backed induction
   closure and complete-graph handles; the public application closure consumer
   must obtain those handles from its own live entry, recheck the original
   source and exact graph, then invoke the checked revalidation helper.

   This is a NOMINAL static handoff check, not independent proof of live
   registry entries, external assumptions, graph completeness, installed
   application writers, or physical/runtime authorization. Raw source,
   registry contents, and the entire checking algorithm remain runtime-bound."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.application-dependency :as application]
            [gesso.assurance.proof.induction-dependency :as induction]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.application-dependency)
(def ^:private graph-handle 'gesso.assurance.dependency.CheckedGraph)
(def ^:private induction-handle
  'gesso.assurance.proof.induction_dependency.InductionDependencyHandle)
(def ^:private application-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)

(defn- definitions []
  (let [file (or (io/resource "gesso/assurance/proof/application_dependency.clj")
                 (throw (ex-info "Application dependency source not found" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (loop [forms []]
        (let [form (read {:eof ::end} r)]
          (if (= ::end form) forms (recur (conj forms form))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %)
                              (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate production closure definition"
                      {:name name :matches (count matches)})))
    (first matches)))

(defn- fn-ann! [name signature]
  (eval (list 'typed.clojure/ann name
              (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (doseq [handle [graph-handle induction-handle application-handle]]
    (eval (list 'typed.clojure/ann-datatype handle '[])))
  (doseq [[name signature]
          [['gesso.assurance.proof.induction-dependency/require-current-induction-dependencies!
            [induction-handle 'typed.clojure/Any 'typed.clojure/Any
             '-> induction-handle]]
           ['gesso.assurance.dependency/require-current-graph!
            [graph-handle 'typed.clojure/Any '-> graph-handle]]
           ['gesso.assurance.proof.application-dependency/inner-graph
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-dependency/revalidate-application-graphs!
            [induction-handle graph-handle 'typed.clojure/Any
             'typed.clojure/Any 'typed.clojure/Any '-> graph-handle]]
           ['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> application-handle]]
           ['gesso.assurance.proof.induction-dependency/check-induction-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> induction-handle]]
           ['gesso.assurance.dependency/check-graph!
            ['typed.clojure/Any '-> graph-handle]]]]
    (fn-ann! name signature)))

(defn- check! [checker label source]
  (let [{:keys [ex type-errors]} (checker source :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Application closure handoff failed source typing"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label source]
  (let [{:keys [ex type-errors]} (checker source :check-config config)
        types (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq types)
                   (every? #{:typed.clojure/app-type-error
                             :typed.clojure/type-mismatch-error} types))
      (throw (ex-info "Invalid application closure handoff not rejected"
                      {:label label :actual types
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- assert-source-link! [forms]
  ;; Ensure the checked helper is in the registered consumer path and still
  ;; follows the ORIGINAL source and exact-graph guards. The source audit
  ;; is necessary: Any-typed catalog/graph/base inputs are not distinguished
  ;; statically, and the checker cannot prove these validation algorithms.
  (let [issuer (original forms 'check-application-dependencies!)
        consumer (original forms 'require-current-application-dependencies!)
        issuer-let (last issuer)
        consumer-let (last consumer)
        issuer-bindings (second issuer-let)
        consumer-bindings (second consumer-let)
        issuer-map (into {} (map vec (partition 2 issuer-bindings)))
        consumer-map (into {} (map vec (partition 2 consumer-bindings)))
        issuer-entry (get issuer-map 'entry)
        required-call '(revalidate-application-graphs!
                        inner-handle graph-handle
                        current-catalog current-graph base)
        consumer-steps (vec (drop 2 consumer-let))]
    (when-not (and (= '(induction-dependency/check-induction-dependencies!
                        catalog inner) (get issuer-map 'inner-handle))
                   (= '(dependency/check-graph! current-graph)
                      (get issuer-map 'graph-handle))
                   (map? issuer-entry)
                   (= 'inner-handle (:inner-handle issuer-entry))
                   (= 'graph-handle (:graph-handle issuer-entry))
                   (= '(ApplicationDependencyHandle.) (get issuer-map 'handle))
                   (= '(locking live-handles (.put live-handles handle entry))
                      (first (drop 2 issuer-let)))
                   (= 'handle (last issuer-let))
                   (= '(entry! handle)
                      (get consumer-map '{:keys [graph-handle inner-handle
                                                 graph-content]}))
                   (= '(source-graph! current-catalog)
                      (get consumer-map '{:keys [base graph]}))
                   (= ['(when-not (= graph-content (content graph))
                          (reject! :stale-application-source {}))
                       '(exact! graph current-graph)
                       required-call
                       'handle]
                      consumer-steps))
      (throw (ex-info "Original application closure source handoff changed"
                      {:entry issuer-entry :issuer-bindings issuer-map
                       :consumer-bindings consumer-map
                       :consumer-steps consumer-steps})))
    true))

(defn- swap-original-call [source op new-call]
  (let [seen (atom 0)
        altered (walk/postwalk
                 (fn [form]
                   (if (and (seq? form) (= op (first form)))
                     (do (swap! seen inc) new-call)
                     form)) source)]
    (when-not (and (= 1 @seen) (not= source altered))
      (throw (ex-info "Original validator operation not uniquely found"
                      {:operation op :matches @seen})))
    altered))

(defn- check-production! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (let [source (original forms 'revalidate-application-graphs!)]
      (check! checker :original-two-handle-revalidation source)
      (assert-source-link! forms)
      ;; Call the original source-backed helper with the right nominal
      ;; categories and test incorrect substitutions in both original calls.
      (doseq [[label op bad]
              [[:inner-as-graph
                'induction-dependency/require-current-induction-dependencies!
                '(induction-dependency/require-current-induction-dependencies!
                  graph-handle current-catalog (inner-graph base current-graph))]
               [:inner-raw
                'induction-dependency/require-current-induction-dependencies!
                '(induction-dependency/require-current-induction-dependencies!
                  {} current-catalog (inner-graph base current-graph))]
               [:outer-as-induction
                'dependency/require-current-graph!
                '(dependency/require-current-graph! inner-handle current-graph)]
               [:outer-raw
                'dependency/require-current-graph!
                '(dependency/require-current-graph! {} current-graph)]]]
        (reject! checker label (swap-original-call source op bad)))
      (doseq [[label bad-return]
              [[:returns-induction-handle 'inner-handle]
               [:returns-raw-graph 'current-graph]
               [:returns-nil nil]
               [:returns-string "checked"]]]
        (reject! checker label
                 (apply list (concat (butlast source) [bad-return]))))
      (check! checker :valid-nominal-pair
              '(revalidate-application-graphs!
                (induction-dependency/check-induction-dependencies! {} {})
                (dependency/check-graph! {}) {} {} {}))
      (doseq [[label bad-call]
              [[:raw-induction '(revalidate-application-graphs!
                                 {} (dependency/check-graph! {}) {} {} {})]
               [:raw-graph '(revalidate-application-graphs!
                             (induction-dependency/check-induction-dependencies! {} {})
                             {} {} {} {})]
               [:reversed-handles '(revalidate-application-graphs!
                                    (dependency/check-graph! {})
                                    (induction-dependency/check-induction-dependencies! {} {})
                                    {} {} {})]
               [:application-handle-as-induction
                '(revalidate-application-graphs!
                  (check-application-dependencies! {} {})
                  (dependency/check-graph! {}) {} {} {})]
               [:nil-induction '(revalidate-application-graphs!
                                 nil (dependency/check-graph! {}) {} {} {})]
               [:nil-graph '(revalidate-application-graphs!
                             (induction-dependency/check-induction-dependencies! {} {})
                             nil {} {} {})]]]
        (reject! checker label bad-call))
      true)))

(defn- check-forgery! []
  ;; Neither a copyable description nor a class instance bypasses the live
  ;; application registry. This verifies the ORIGINAL runtime consumer.
  (let [forged (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        reason (try (application/require-current-application-dependencies!
                     forged {} {})
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e))))]
    (when-not (and (not (application/checked-application-dependencies? forged))
                   (= :unrecognized-closure-handle reason))
      (throw (ex-info "Forged application closure bypassed registry"
                      {:reason reason}))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (definitions)]
    (check-production! checker forms)
    (check-forgery!)
    (println "Typed application closure passed: original graph revalidation")
    (println "body, source-bound issuer/consumer audit, 14 negative type")
    (println "substitutions, and forged closure runtime rejection.")
    (println "Original graph correctness, proof and authority remain open.")
    :ok))

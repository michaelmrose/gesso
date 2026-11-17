(ns gesso.assurance.typed-dependency-graph-check
  "Opt-in Typed Clojure checks of the original structural graph handle source.

   A CheckedGraph class is not a checked graph until its private live registry
   authenticates it. Graph checking is structural and snapshot-relative; it
   does not prove semantics, discharge premises, or grant installed authority.
   This check does not certify the entire raw graph validator or downstream
   consumers: those still need runtime checks and future source-bound typing."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private graph-handle 'gesso.assurance.dependency.CheckedGraph)
(def ^:private admission-handle 'gesso.assurance.admission.AdmissionHandle)
(def ^:private general-map
  '(typed.clojure/Map typed.clojure/Any typed.clojure/Any))
(def ^:private graph-description
  '(typed.clojure/HMap
    :mandatory {:type typed.clojure/Kw
                :version typed.clojure/Int
                :nodes typed.clojure/Any
                :roots typed.clojure/Any
                :dependency-order typed.clojure/Any
                :checking (typed.clojure/Val :structural-only)
                :proof-status (typed.clojure/Val :not-proved)
                :assumption-discharge (typed.clojure/Val :not-established)
                :implementation-correspondence (typed.clojure/Val :not-established)
                :admission (typed.clojure/Val :not-a-proof)
                :authority (typed.clojure/Val :none)}
    :complete? true))

(defn- original-forms []
  (let [file (or (io/resource "gesso/assurance/dependency.clj")
                 (throw (ex-info "Dependency source missing" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (loop [acc []]
        (let [form (read {:eof ::end} r)]
          (if (= form ::end) acc (recur (conj acc form))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate original graph function"
                      {:name name :matches (count matches)})))
    (first matches)))

(defn- ann! [name type-form]
  (eval (list 'typed.clojure/ann name type-form)))

(defn- fn-ann! [name signature]
  (ann! name (list 'typed.clojure/IFn signature)))

(defn- check! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original graph source failed static checking"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label expression kind]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)
        found (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (= [kind] found))
      (throw (ex-info "Invalid graph expression escaped static checker"
                      {:label label :expected kind :found found
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- mutate-description [definition update-description]
  (let [matches (atom 0)
        altered (walk/postwalk
                 (fn [form]
                   (if (and (map? form)
                            (= :structural-only (:checking form))
                            (= :not-proved (:proof-status form)))
                     (do (swap! matches inc) (update-description form))
                     form))
                 definition)]
    (when-not (= @matches 1)
      (throw (ex-info "Production graph description map changed"
                      {:matches @matches})))
    (when (= altered definition)
      (throw (ex-info "Graph mutation had no effect" {})))
    altered))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype admission-handle '[]))
  (eval (list 'typed.clojure/ann-datatype graph-handle '[]))
  (ann! 'gesso.assurance.dependency/live-graphs 'java.util.WeakHashMap)
  (ann! 'gesso.assurance.dependency/graph-type 'typed.clojure/Kw)
  (ann! 'gesso.assurance.dependency/graph-version 'typed.clojure/Int)
  ;; The raw validator's output is a map, but its semantic correctness is
  ;; tested by runtime regressions; this pilot does not type-check that body.
  (fn-ann! 'gesso.assurance.dependency/build-snapshot!
           ['typed.clojure/Any '-> general-map])
  ;; WeakHashMap#get exposes an opaque value: do not assert trusted contents.
  (fn-ann! 'gesso.assurance.dependency/live-entry!
           [graph-handle '-> 'typed.clojure/Any])
  (fn-ann! 'gesso.assurance.dependency/graph-error!
           ['typed.clojure/Any general-map '-> 'typed.clojure/Nothing])
  (fn-ann! 'gesso.assurance.dependency/check-graph!
           ['typed.clojure/Any '-> graph-handle])
  (fn-ann! 'gesso.assurance.dependency/describe-graph
           [graph-handle '-> graph-description])
  ;; The following caller contracts are not yet production-body checked.
  (fn-ann! 'gesso.assurance.dependency/require-current-graph!
           [graph-handle 'typed.clojure/Any '-> graph-handle])
  (fn-ann! 'gesso.assurance.admission/admit-subject!
           ['typed.clojure/Any '-> admission-handle])
  (fn-ann! 'gesso.assurance.admission/require-admitted-subject!
           [admission-handle 'typed.clojure/Any '-> admission-handle]))

(defn- check-production! [checker forms]
  (binding [*ns* (the-ns 'gesso.assurance.dependency)]
    (doseq [name '[check-graph! live-entry! describe-graph]]
      (check! checker [:original name] (original forms name)))
    (check! checker :valid-issuer-to-consumer
            '(describe-graph (check-graph! {})))
    (doseq [[label expr]
            [[:raw-description '(describe-graph {:checking :structural-only})]
             [:raw-nil '(describe-graph nil)]
             [:copyable-description
              '(describe-graph (describe-graph (check-graph! {})))]
             [:wrong-handle-category
              '(describe-graph
                (gesso.assurance.admission/admit-subject! {}))]
             [:wrong-consumer-category
              '(gesso.assurance.admission/require-admitted-subject!
                (check-graph! {}) {})]
             [:raw-freshness-handle '(require-current-graph! {} {})]]]
      (reject! checker label expr :typed.clojure/app-type-error))
    ;; Mutate the returned map in the original describe-graph body. The
    ;; complete HMap prevents accidental invented claims of proof/authority.
    (doseq [[label mutator]
            [[:pretend-proof #(assoc % :proof-status :proved)]
             [:pretend-discharge #(assoc % :assumption-discharge :discharged)]
             [:pretend-authority #(assoc % :authority :granted)]
             [:pretend-certification #(assoc % :admission :certified)]
             [:missing-checking #(dissoc % :checking)]
             [:injected-certificate #(assoc % :certificate {:valid? true})]]]
      (reject! checker label
               (mutate-description (original forms 'describe-graph) mutator)
               :typed.clojure/type-mismatch-error))
    (let [source (original forms 'check-graph!)
          invalid (apply list (concat (butlast source) [{}]))]
      (reject! checker :issuer-returns-raw-map invalid
               :typed.clojure/type-mismatch-error))
    true))

(def ^:private sample-subject
  {:kind :rule :id :typed/dependency-root
   :interface {:name :typed/dependency-root}})

(defn- observed-error [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))

(defn- check-runtime! []
  (let [raw {:nodes [{:id :typed/dependency-root
                      :subject sample-subject
                      :subject-handle (admission/admit-subject! sample-subject)
                      :depends-on []}]
             :roots #{:typed/dependency-root}}
        handle (dependency/check-graph! raw)
        description (dependency/describe-graph handle)
        forged (gesso.assurance.dependency.CheckedGraph.)]
    (when-not (and (dependency/checked-graph? handle)
                   (identical? handle
                               (dependency/require-current-graph! handle raw))
                   (not (dependency/checked-graph? description))
                   (not (dependency/checked-graph? forged))
                   (= :not-proved (:proof-status description))
                   (= :none (:authority description)))
      (throw (ex-info "Graph runtime issuance/revalidation failed" {})))
    (let [modified-subject (assoc-in sample-subject [:interface :name]
                                      :typed/changed-dependency-root)
          changed (assoc raw :nodes
                         [(assoc (first (:nodes raw))
                                 :subject modified-subject
                                 :subject-handle
                                 (admission/admit-subject! modified-subject))])]
      (when-not (= :stale-graph
                   (observed-error
                    #(dependency/require-current-graph! handle changed)))
        (throw (ex-info "Changed graph failed to invalidate snapshot" {}))))
    (doseq [[label candidate]
            [[:copyable-map description]
             [:manual-instance forged]
             [:raw-graph raw]]]
      (let [got (observed-error
                 #(dependency/require-current-graph! candidate raw))]
        (when-not (= :unrecognized-graph got)
          (throw (ex-info "Unregistered graph was accepted"
                          {:label label :actual got}))))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)]
    (check-production! checker (original-forms))
    (check-runtime!)
    (println "Typed dependency graph passed: 3 original graph source bodies,")
    (println "6 wrong-handle call rejections, 7 source-map/return mutations,")
    (println "and forged/copied-handle and stale-graph runtime rejection.")
    (println "Structural graph checking only; not a proof or installation.")
    :ok))

(ns gesso.assurance.typed-operation-interface-provenance-check
  "Opt-in source-linked Typed Clojure checks for operation composition provenance.

   The ORIGINAL manifest-entry source is checked against a complete HMap
   with a structured lineage. The ORIGINAL freshness consumer is also
   checked; the large original producer's graph/source validation remains
   runtime code, but its validation-to-manifest path is source-audited.
   The manifest is copyable diagnostic data, not a proof or authority token."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.proof.operation-interface-provenance :as provenance]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.operation-interface-composition :as composition]
            [gesso.assurance.typed-operation-interface-composition-check :as comp-check]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.operation-interface-provenance)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
(def ^:private composed-type comp-check/composed-type)
(def ^:private lineage-type
  '(typed.clojure/HMap
    :mandatory {:versions typed.clojure/Any
                :catalog-id typed.clojure/Any
                :catalog-fingerprint typed.clojure/Any
                :induction-fingerprint typed.clojure/Any
                :induction-ledger-fingerprint typed.clojure/Any
                :operation-interface-fingerprint typed.clojure/Str
                :composition-fingerprint typed.clojure/Str
                :graph typed.clojure/Any
                :source-roots typed.clojure/Any
                :operations typed.clojure/Any
                :invariants typed.clojure/Any
                :original-open-obligation-ids-by-operation typed.clojure/Any
                :open-meta-obligations typed.clojure/Any
                :open-assumptions typed.clojure/Any}
    :complete? true))
(def ^:private identity-type
  '(typed.clojure/HMap
    :mandatory {:fingerprint typed.clojure/Str}
    :complete? false))
(def ^:private status-fields
  {:type :gesso.assurance.proof.operation-interface-provenance/source-relative-manifest
   :version 1
   :scope :closed-declared-abstract-machine-only
   :checking :source-relative-graph-and-composition-provenance
   :proof-status :conditional-structural-evidence
   :assumption-discharge :not-performed
   :installed-writer-coverage :not-established
   :implementation-correspondence :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})
;; Shared contract for BOTH the actual manifest constructor and downstream
;; application-assurance consumer. Does not validate the source of input data.
(def manifest-type
  (list 'typed.clojure/HMap :mandatory
        (merge {:fingerprint 'typed.clojure/Str
                :lineage lineage-type}
               (into {} (map (fn [[k v]] [k (list 'typed.clojure/Val v)])
                             status-fields)))
        :complete? true))

(defn- definitions []
  (let [path (or (io/resource "gesso/assurance/proof/operation_interface_provenance.clj")
                 (throw (ex-info "Original provenance source absent" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader path))]
      (loop [forms []]
        (let [f (read {:eof ::eof} r)]
          (if (= f ::eof) forms (recur (conj forms f))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %)
                              (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Provenance production source changed"
                      {:name name :count (count matches)})))
    (first matches)))

(defn- annotate! [name sig]
  (eval (list 'typed.clojure/ann name (list 'typed.clojure/IFn sig))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype app-handle '[]))
  (doseq [[name type]
          [['gesso.assurance.proof.operation-interface-provenance/provenance-type
            '(typed.clojure/Val :gesso.assurance.proof.operation-interface-provenance/source-relative-manifest)]
           ['gesso.assurance.proof.operation-interface-provenance/provenance-version
            '(typed.clojure/Val 1)]]]
    (eval (list 'typed.clojure/ann name type)))
  (doseq [[name sig]
          [['gesso.assurance.proof.operation-interface-provenance/manifest-entry
            [lineage-type identity-type '-> manifest-type]]
           ['gesso.assurance.proof.operation-interface-provenance/describe-composition-provenance!
            [composed-type 'typed.clojure/Any 'typed.clojure/Any app-handle
             '-> manifest-type]]
           ['gesso.assurance.proof.operation-interface-provenance/require-current-composition-provenance!
            ['typed.clojure/Any composed-type 'typed.clojure/Any
             'typed.clojure/Any app-handle '-> manifest-type]]
           ['gesso.assurance.proof.operation-interface-provenance/reject!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Nothing]]
           ['gesso.assurance.proof.operation-interface-composition/compose-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> composed-type]]
           ['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> app-handle]]]]
    (annotate! name sig)))

(defn- check! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original provenance contract failed type-check"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Invalid provenance escaped type-check"
                      {:label label :kinds kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- manifest-result [source]
  (let [result (last source)]
    (when-not (map? result)
      (throw (ex-info "Original manifest entry shape changed" {})))
    result))

(defn- mutate-result [source alteration]
  (let [result (manifest-result source)
        modified (alteration result)]
    (when (= result modified)
      (throw (ex-info "No-op provenance mutation" {})))
    (apply list (concat (butlast source) [modified]))))

(def ^:private expected-manifest-fields
  {:type 'provenance-type
   :version 'provenance-version
   :fingerprint '(:fingerprint identity)
   :lineage 'lineage})

(defn- audit-manifest! [source]
  (let [result (manifest-result source)]
    (when-not (and (= (set (keys result))
                      (into (set (keys status-fields))
                            (keys expected-manifest-fields)))
                   (= (dissoc status-fields :type :version)
                      (select-keys result (remove #{:type :version}
                                                  (keys status-fields))))
                   (every? (fn [[k v]] (= v (get result k)))
                           expected-manifest-fields))
      (throw (ex-info "Original manifest descriptor provenance changed"
                      {:result result})))
    true))

(defn- audit-producer! [forms]
  (let [source (original forms 'describe-composition-provenance!)
        body (last source)
        bindings (when (and (seq? body) (= 'let (first body))) (second body))
        pairs (when (vector? bindings) (partition 2 bindings))
        by-var (when pairs (into {} (map vec pairs)))
        expected-root-guard
        '(when-not (= (get-in lineage [:source-roots :application :id])
                      (first (get-in lineage [:graph :roots])))
           (reject! :application-root-mismatch {}))]
    (when-not (and (= '(comp/require-current-operation-composition!
                        composition catalog current-graph closure)
                      (get by-var 'current))
                   (= '(app/describe-application-dependencies closure)
                      (get by-var 'summary))
                   (= '(node-index! current-graph) (get by-var 'graph-index))
                   (= '(operation-provenance! current-graph current)
                      (get by-var 'operations))
                   (= '(invariant-provenance! current-graph current)
                      (get by-var 'invariants))
                   (= '(subject/describe-subject
                        {:kind :rule :id identity-id :interface lineage})
                      (get by-var 'identity))
                   (= expected-root-guard (nth body 2))
                   (= '(manifest-entry lineage identity) (last body)))
      (throw (ex-info "Original provenance source/graph handoff changed"
                      {:root-guard (nth body 2) :result (last body)})))
    ;; The original closure-composition equivalence checks must still run
    ;; between authentication and graph materialization; full semantic
    ;; completeness belongs to the runtime tests, not this source audit.
    (let [guard (get by-var '_)]
      (when-not (and (seq? guard)
                     (= 'when-not (first guard))
                     (= 'and (first (second guard)))
                     (= 7 (count (rest (second guard))))
                     (= '(reject! :source-closure-composition-mismatch {})
                        (nth guard 2)))
        (throw (ex-info "Composition/closure source guard changed"
                        {:guard guard}))))
    true))

(defn- audit-freshness! [forms]
  (let [body (last (original forms 'require-current-composition-provenance!))
        bindings (second body)]
    (when-not (and (= '(describe-composition-provenance!
                        composition catalog current-graph closure)
                      (second bindings))
                   (= '(when-not (and (map? prior) (not (record? prior))
                                     (nil? (meta prior)) (= prior now))
                         (reject! :stale-or-substituted-composition-provenance
                                  {:expected (:fingerprint now)
                                   :actual (:fingerprint prior)}))
                      (nth body 2))
                   (= 'now (last body)))
      (throw (ex-info "Original provenance stale-copy guard changed" {})))
    true))

(defn- checked-cases! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (let [source (original forms 'manifest-entry)]
      (check! checker :original-manifest-entry source)
      (audit-manifest! source)
      (doseq [[field _] status-fields]
        (reject! checker [:false-manifest-status field]
                 (mutate-result source #(assoc % field :proved))))
      (doseq [[label change]
              [[:missing-lineage #(dissoc % :lineage)]
               [:missing-fingerprint #(dissoc % :fingerprint)]
               [:wrong-fingerprint #(assoc % :fingerprint 12)]
               [:raw-lineage #(assoc % :lineage {})]
               [:invented-certificate #(assoc % :issued-proof :approved)]]]
        (reject! checker label (mutate-result source change)))
      ;; Both fingerprints are Str; static shape alone cannot prove which
      ;; one is a fingerprint of THIS manifestation. Source audit must catch.
      (let [mutated (mutate-result source
                                   #(assoc % :fingerprint
                                           '(:composition-fingerprint lineage)))]
        (check! checker :type-compatible-misbound-fingerprint mutated)
        (when-not (try (audit-manifest! mutated)
                       false (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Fingerprint source switch escaped audit" {})))))
    (check! checker :original-provenance-freshness
            (original forms 'require-current-composition-provenance!))
    (audit-producer! forms)
    (audit-freshness! forms)
    (check! checker :valid-provenance-call
            '(describe-composition-provenance!
              (comp/compose-operation-interfaces!
               {} {} (app/check-application-dependencies! {} {}))
              {} {} (app/check-application-dependencies! {} {})))
    (doseq [[label form]
            [[:raw-composition '(describe-composition-provenance! {} {} {}
                                  (app/check-application-dependencies! {} {}))]
             [:nil-composition '(describe-composition-provenance! nil {} {}
                                  (app/check-application-dependencies! {} {}))]
             [:raw-closure '(describe-composition-provenance!
                              (comp/compose-operation-interfaces!
                               {} {} (app/check-application-dependencies! {} {}))
                              {} {} {})]
             [:nil-closure '(describe-composition-provenance!
                              (comp/compose-operation-interfaces!
                               {} {} (app/check-application-dependencies! {} {}))
                              {} {} nil)]]]
      (reject! checker label form))
    true))

(defn- runtime-forgery! []
  (let [forged (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        bad-composition {:authority :none :proof-status :conditional-inference-or-open}
        reason (try (provenance/describe-composition-provenance!
                     bad-composition {} {} forged)
                    nil (catch clojure.lang.ExceptionInfo e
                          (:reason (ex-data e))))]
    (when-not (= :unrecognized-closure-handle reason)
      (throw (ex-info "Forged closure accepted as provenance source"
                      {:reason reason})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (definitions)]
    (checked-cases! checker forms)
    (runtime-forgery!)
    (println "Typed provenance passed: original descriptor/freshness source,")
    (println "16 malformed descriptor forms, 4 wrong composition/closure")
    (println "calls, runtime forged-closure rejection, and a type-compatible")
    (println "fingerprint mixup caught by source audit; no authority claim.")
    :ok))

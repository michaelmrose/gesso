(ns gesso.assurance.typed-application-dependency-closure-check
  "Opt-in Typed Clojure checking of source-relative application graph closure.

   The original revalidator must consume distinct, registry-backed induction
   closure and complete-graph handles; the public application closure consumer
   must obtain those handles from its own live entry, recheck the original
   source and exact graph, then invoke the checked revalidation helper.

   The original closure-entry constructor must retain both handles in distinct
   slots in its exact structural descriptor. Its source call is audited back
   to the original issuer, which remains the only live handle registration.

   This is a NOMINAL static handoff check, not independent proof of live
   registry entries, external assumptions, graph completeness, installed
   application writers, or physical/runtime authorization. Raw source,
   registry contents, and the entire checking algorithm remain runtime-bound.

   The original public description now has an exact typed HMap contract:
   its proof/admission/authority fields cannot silently become affirmative.
   The entry! registry result remains opaque, so this assumes the descriptor
   shape produced by our separately checked issuer rather than proving the
   registry populated it. Provenance of Any-typed ledger fields is audited
   against the original source, not misrepresented as static type safety."
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
(def ^:private registered-entry
  '(typed.clojure/HMap
    :mandatory {:graph-handle gesso.assurance.dependency.CheckedGraph
                :inner-handle
                gesso.assurance.proof.induction_dependency.InductionDependencyHandle
                :graph-content typed.clojure/Any
                :ledger typed.clojure/Any
                :catalog-id typed.clojure/Any
                :operation-ids (typed.clojure/Set typed.clojure/Kw)
                :invariant-ids (typed.clojure/Set typed.clojure/Kw)
                :node-count typed.clojure/Int
                :edge-count typed.clojure/Int}
    :complete? true))

;; Share the exact return HMap already checked against the original
;; describe-application-dependencies function with downstream report checks.
;; This copyable structure is NOT a proof or authenticated registry handle.
(def description-type
  '(typed.clojure/HMap
    :mandatory {:type typed.clojure/Kw
                :version typed.clojure/Int
                :catalog-id typed.clojure/Any
                :catalog-fingerprint typed.clojure/Any
                :induction-fingerprint typed.clojure/Any
                :ledger-fingerprint typed.clojure/Any
                :operation-ids (typed.clojure/Set typed.clojure/Kw)
                :invariant-ids (typed.clojure/Set typed.clojure/Kw)
                :node-count typed.clojure/Int
                :edge-count typed.clojure/Int
                :open-meta-obligations typed.clojure/Any
                :open-assumptions typed.clojure/Any
                :original-open-obligation-ids-by-operation typed.clojure/Any
                :checking (typed.clojure/Val
                           :original-source-and-exact-application-dependency-graph)
                :scope (typed.clojure/Val
                        :closed-declared-abstract-machine-only)
                :proof-status (typed.clojure/Val
                               :structural-source-dependency-closure-only)
                :assumption-discharge (typed.clojure/Val :not-performed)
                :installed-writer-coverage (typed.clojure/Val :not-established)
                :implementation-correspondence (typed.clojure/Val :not-established)
                :certificate (typed.clojure/Val :not-issued)
                :admission (typed.clojure/Val :not-admitted)
                :authority (typed.clojure/Val :none)}
    :complete? true))

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
  (eval '(typed.clojure/ann
           gesso.assurance.proof.application-dependency/closure-type typed.clojure/Kw))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.application-dependency/closure-version typed.clojure/Int))
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
            ['typed.clojure/Any '-> graph-handle]]
           ['gesso.assurance.proof.application-dependency/closure-entry
            [graph-handle induction-handle 'typed.clojure/Any
             'typed.clojure/Any 'typed.clojure/Any
             '(typed.clojure/Set typed.clojure/Kw)
             '(typed.clojure/Set typed.clojure/Kw)
             'typed.clojure/Int 'typed.clojure/Int
             '-> registered-entry]]
           ;; The registry's actual contents are checked by runtime entry!;
           ;; the checker assumes the shape guaranteed by our original issuer.
           ['gesso.assurance.proof.application-dependency/entry!
            [application-handle '-> registered-entry]]
           ['gesso.assurance.proof.application-dependency/describe-application-dependencies
            [application-handle '-> description-type]]]]
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
        expected-entry
        '(closure-entry graph-handle inner-handle (content graph) ledger
                        catalog-id operation-ids invariant-ids
                        (count (:nodes graph))
                        (reduce + (map (fn [n] (count (:depends-on n)))
                                       (:nodes graph))))
        required-call '(revalidate-application-graphs!
                        inner-handle graph-handle
                        current-catalog current-graph base)
        consumer-steps (vec (drop 2 consumer-let))]
    (when-not (and (= '(induction-dependency/check-induction-dependencies!
                        catalog inner) (get issuer-map 'inner-handle))
                   (= '(dependency/check-graph! current-graph)
                      (get issuer-map 'graph-handle))
                   (= expected-entry issuer-entry)
                   (= '(exact! graph current-graph) (get issuer-map '_))
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

(defn- change-descriptor [definition alter]
  (let [value (last definition)]
    (when-not (map? value)
      (throw (ex-info "Original application closure descriptor is not a map" {})))
    (let [changed (alter value)]
      (when (= changed value)
        (throw (ex-info "Application descriptor mutation did not change source" {})))
      (apply list (concat (butlast definition) [changed])))))

(defn- check-descriptor! [checker forms]
  (let [source (original forms 'closure-entry)]
    (check! checker :original-closure-entry-constructor source)
    ;; These mutate the ACTUAL original constructor, rather than a copied
    ;; parallel implementation. Exact HMap rejects invented proof fields.
    (doseq [[label alter]
            [[:graph-from-induction #(assoc % :graph-handle 'inner-handle)]
             [:induction-from-graph #(assoc % :inner-handle 'graph-handle)]
             [:graph-from-raw-map #(assoc % :graph-handle {})]
             [:induction-from-raw-map #(assoc % :inner-handle {})]
             [:graph-from-node-count #(assoc % :graph-handle 'node-count)]
             [:induction-from-ledger #(assoc % :inner-handle 'ledger)]
             [:missing-graph #(dissoc % :graph-handle)]
             [:missing-induction #(dissoc % :inner-handle)]
             [:wrong-node-count #(assoc % :node-count "five")]
             [:wrong-edge-count #(assoc % :edge-count false)]
             [:raw-operations #(assoc % :operation-ids {})]
             [:raw-invariants #(assoc % :invariant-ids [])]
             [:invented-proof #(assoc % :proof-status :proved)]
             [:invented-authority #(assoc % :authority :granted)]]]
      (reject! checker [:descriptor label] (change-descriptor source alter)))
    (check! checker :valid-descriptor-call
            '(closure-entry (dependency/check-graph! {})
                            (induction-dependency/check-induction-dependencies!
                             {} {})
                            {} {} :catalog #{:op} #{:inv} 2 1))
    (doseq [[label invalid]
            [[:raw-graph-argument
              '(closure-entry {} (induction-dependency/check-induction-dependencies!
                                  {} {}) {} {} :catalog #{} #{} 2 1)]
             [:raw-induction-argument
              '(closure-entry (dependency/check-graph! {}) {} {} {}
                              :catalog #{} #{} 2 1)]
             [:swapped-handles
              '(closure-entry
                (induction-dependency/check-induction-dependencies! {} {})
                (dependency/check-graph! {}) {} {} :catalog #{} #{} 2 1)]
             [:application-handle-instead-of-graph
              '(closure-entry (check-application-dependencies! {} {})
                              (induction-dependency/check-induction-dependencies!
                               {} {}) {} {} :catalog #{} #{} 2 1)]
             [:non-set-operation-ids
              '(closure-entry (dependency/check-graph! {})
                              (induction-dependency/check-induction-dependencies!
                               {} {}) {} {} :catalog [] #{} 2 1)]
             [:non-integer-edge-count
              '(closure-entry (dependency/check-graph! {})
                              (induction-dependency/check-induction-dependencies!
                               {} {}) {} {} :catalog #{} #{} 2 "one")]]]
      (reject! checker [:descriptor-call label] invalid))
    true))

(def ^:private description-provenance
  {:type 'closure-type
   :version 'closure-version
   :catalog-id 'catalog-id
   :operation-ids 'operation-ids
   :invariant-ids 'invariant-ids
   :node-count 'node-count
   :edge-count 'edge-count
   :catalog-fingerprint '(:catalog-fingerprint ledger)
   :induction-fingerprint '(:induction-fingerprint ledger)
   :ledger-fingerprint '(:fingerprint ledger)
   :open-meta-obligations '(:open-meta-obligations ledger)
   :open-assumptions '(:open-assumptions ledger)
   :original-open-obligation-ids-by-operation
   '(:original-open-obligation-ids-by-operation ledger)})

(defn- description-result [source]
  (let [body (last source)]
    (when-not (and (seq? body) (= 'let (first body)))
      (throw (ex-info "Original application description no longer uses let"
                      {:form body})))
    (let [bindings (second body)
          result (last body)]
      (when-not (and (= '[{:keys [ledger catalog-id operation-ids invariant-ids
                                   node-count edge-count]} (entry! handle)] bindings)
                     (map? result))
        (throw (ex-info "Original description no longer reads live registry"
                        {:bindings bindings :result result})))
      result)))

(defn- assert-description-provenance! [source]
  ;; These are same-typed values (mostly Any), so Typed Clojure itself
  ;; cannot prove their origins. Explicitly retain the source relationship
  ;; between the live registered entry and each public report field.
  (let [result (description-result source)]
    (doseq [[field expression] description-provenance]
      (when-not (= expression (get result field ::missing))
        (throw (ex-info "Copyable description changed its source field"
                        {:field field :expected expression
                         :actual (get result field ::missing)}))))
    true))

(defn- mutate-description [source alter]
  ;; Modify the *actual original* description map, not a test-only facade.
  (let [body (last source)
        prior (description-result source)
        changed (alter prior)]
    (when (= prior changed)
      (throw (ex-info "No-op closure description mutation" {})))
    (apply list (concat (butlast source)
                        [(apply list (concat (butlast body) [changed]))]))))

(defn- check-description! [checker forms]
  (let [original-source (original forms 'describe-application-dependencies)]
    (check! checker :original-application-closure-description original-source)
    (assert-description-provenance! original-source)
    (check! checker :correct-description-handle
            '(describe-application-dependencies
              (check-application-dependencies! {} {})))
    (doseq [[label altered]
            [[:false-proof #(assoc % :proof-status :proved)]
             [:false-certificate #(assoc % :certificate :issued)]
             [:false-authority #(assoc % :authority :granted)]
             [:false-admission #(assoc % :admission :admitted)]
             [:false-assumption-discharge #(assoc % :assumption-discharge :complete)]
             [:false-writer-coverage #(assoc % :installed-writer-coverage :complete)]
             [:false-correspondence #(assoc % :implementation-correspondence :proved)]
             [:false-scope #(assoc % :scope :installed-application)]
             [:false-checking #(assoc % :checking :independent-proof)]
             [:missing-open-premises #(dissoc % :open-assumptions)]
             [:missing-original-obligations
              #(dissoc % :original-open-obligation-ids-by-operation)]
             [:incorrect-node-count #(assoc % :node-count "two")]
             [:incorrect-edge-count #(assoc % :edge-count false)]
             [:invented-proof-field #(assoc % :proof-evidence :valid)]]]
      (reject! checker [:description label]
               (mutate-description original-source altered)))
    ;; A copied report, a raw map, or even another registered handle class
    ;; must not be a substitute for this application's live closure handle.
    (doseq [[label expression]
            [[:description-raw '(describe-application-dependencies {})]
             [:description-nil '(describe-application-dependencies nil)]
             [:description-graph-handle
              '(describe-application-dependencies (dependency/check-graph! {}))]
             [:description-induction-handle
              '(describe-application-dependencies
                (induction-dependency/check-induction-dependencies! {} {}))]
             [:description-copy
              '(describe-application-dependencies
                (describe-application-dependencies
                 (check-application-dependencies! {} {})))]]]
      (reject! checker [:description-call label] expression))
    ;; Prove these mutations are NOT rejected by the type system and MUST
    ;; instead be rejected by the source-provenance audit. Their values share
    ;; the same broad Any type but represent different original facts.
    (doseq [[label altered]
            [[:swapped-catalog-fingerprint
              #(assoc % :catalog-fingerprint '(:induction-fingerprint ledger))]
             [:swapped-open-assumptions
              #(assoc % :open-assumptions '(:open-meta-obligations ledger))]
             [:swapped-original-obligations
              #(assoc % :original-open-obligation-ids-by-operation
                      '(:open-assumptions ledger))]]]
      (let [changed (mutate-description original-source altered)
            caught? (try (assert-description-provenance! changed)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))]
        (when-not caught?
          (throw (ex-info "Incorrect description provenance escaped source audit"
                          {:case label})))
        (check! checker [:type-compatible-but-invalid-source label] changed)))
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
      (check-descriptor! checker forms)
      (check-description! checker forms)
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
                      (:reason (ex-data e))))
        description-reason
        (try (application/describe-application-dependencies forged)
             nil
             (catch clojure.lang.ExceptionInfo e
               (:reason (ex-data e))))
        copy-reason
        (try (application/describe-application-dependencies
              {:proof-status :structural-source-dependency-closure-only
               :authority :none})
             nil
             (catch clojure.lang.ExceptionInfo e
               (:reason (ex-data e))))]
    (when-not (and (not (application/checked-application-dependencies? forged))
                   (= :unrecognized-closure-handle reason)
                   (= :unrecognized-closure-handle description-reason)
                   (= :unrecognized-closure-handle copy-reason))
      (throw (ex-info "Forged or copyable application closure bypassed registry"
                      {:reason reason
                       :description-reason description-reason
                       :copy-reason copy-reason}))))
  ;; A pure descriptor containing nominally correct but unregistered handles
  ;; is still just copyable data. It is not an issued application closure.
  (let [ctor (or (ns-resolve source-ns 'closure-entry)
                 (throw (ex-info "Application closure-entry source missing" {})))
        graph (gesso.assurance.dependency.CheckedGraph.)
        induction
        (gesso.assurance.proof.induction_dependency.InductionDependencyHandle.)
        descriptor (ctor graph induction {:roots #{} :nodes {}} {}
                         :catalog #{:op} #{:inv} 2 1)]
    (when-not (and (= #{:graph-handle :inner-handle :graph-content
                        :ledger :catalog-id :operation-ids :invariant-ids
                        :node-count :edge-count} (set (keys descriptor)))
                   (identical? graph (:graph-handle descriptor))
                   (identical? induction (:inner-handle descriptor))
                   (not (application/checked-application-dependencies? descriptor)))
      (throw (ex-info "Pure application descriptor acquired authority" {}))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (definitions)]
    (check-production! checker forms)
    (check-forgery!)
    (println "Typed application closure passed: original graph revalidation")
    (println "and exact issuance descriptor source; source-bound handoffs;")
    (println "53 negative type cases, three Any-typed provenance mutations")
    (println "caught by source audit, forged handle and pure-data checks.")
    (println "Original registry validity, proof and authority remain open.")
    :ok))

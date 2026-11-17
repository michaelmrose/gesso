(ns gesso.assurance.typed-operation-interface-composition-check
  "Opt-in source-backed typing of conditional operation composition.

   Checks the ORIGINAL compose-derived!, public source acquisition, and
   freshness functions. The exact report HMap distinguishes descriptive
   conditional composition from proof, certification, and runtime authority.
   The derived-interface input and upstream helper contracts are assumptions
   at this boundary: source correctness, operation-interface validation,
   assumption discharge, and installed correspondence remain runtime or
   separate assurance obligations, not static consequences of this checker."
  (:require [clojure.java.io :as io]
            [gesso.assurance.proof.application-dependency]
            [gesso.assurance.proof.operation-interface]
            [gesso.assurance.proof.operation-interface-composition]
            [gesso.assurance.subject]
            [gesso.assurance.typed-application-dependency-closure-check :as dep-check]
            [gesso.assurance.typed-operation-interface-derivation-check :as iface-check]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.operation-interface-composition)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
;; Reuse the three distinct premise families already source-checked in the
;; application-closure description. Interfaces! checks them at runtime, but
;; its whole implementation is NOT type-checked by this gate.
(def ^:private meta-ids-type dep-check/obligation-id-set)
(def ^:private classified-assumptions-type dep-check/open-assumptions-type)
(def ^:private original-operations-type dep-check/original-obligations-type)
;; The upstream original interface-report-entry body is now checked against
;; this exact derived report type. The full source-derivation algorithm and
;; live source registry remain runtime obligations, not inferred credentials.
(def ^:private source-type iface-check/derived-type)
(def ^:private report-statuses
  {:type :gesso.assurance.proof.operation-interface-composition/conditional-abstract-catalog
   :version 1
   :scope :single-declared-abstract-machine
   :checking :source-derived-interface-compatibility
   :proof-status :conditional-inference-or-open
   :assumption-discharge :not-performed
   :physical-writer-coverage :not-established
   :implementation-correspondence :not-established
   :concurrency-refinement :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})
;; Shared precise contract for the ORIGINAL composition producer and its
;; application-assurance consumer. The upstream source checker validates its
;; original composition constructor against this exact structural HMap.
;; This is still a copyable report, NEVER a proof or authority token.
(def composed-type
  (list 'typed.clojure/HMap
        :mandatory
        (merge
         {:fingerprint 'typed.clojure/Str
          :catalog-id 'typed.clojure/Any
          :catalog-fingerprint 'typed.clojure/Any
          :induction-fingerprint 'typed.clojure/Any
          :interface-fingerprint 'typed.clojure/Str
          :operation-ids '(typed.clojure/Set typed.clojure/Kw)
          :invariant-ids '(typed.clojure/Set typed.clojure/Kw)
          :shared-state-types 'typed.clojure/Any
          :operation-contracts 'typed.clojure/Any
          :invariant-results 'typed.clojure/Any
          :interactions 'typed.clojure/Any
          :original-open-obligation-ids-by-operation original-operations-type
          :open-meta-obligations meta-ids-type
          :open-assumptions classified-assumptions-type}
         (into {} (map (fn [[field value]]
                         [field (list 'typed.clojure/Val value)])
                       report-statuses)))
        :complete? true))

(defn- source-forms []
  (let [path (or (io/resource "gesso/assurance/proof/operation_interface_composition.clj")
                 (throw (ex-info "Original operation composition source missing" {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader path))]
      (loop [acc []]
        (let [form (read {:eof ::end} reader)]
          (if (= ::end form) acc (recur (conj acc form))))))))

(defn- original [forms function]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= function (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Original composition function missing or duplicated"
                      {:function function :matches (count matches)})))
    (first matches)))

(defn- fn-ann! [symbol signature]
  (eval (list 'typed.clojure/ann symbol (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype app-handle '[]))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.operation-interface-composition/composition-type
           (typed.clojure/Val
            :gesso.assurance.proof.operation-interface-composition/conditional-abstract-catalog)))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.operation-interface-composition/composition-version
           (typed.clojure/Val 1)))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.operation-interface-composition/identity-id
           typed.clojure/Kw))
  ;; These annotations describe expected INTERNAL inputs to composition.
  ;; In particular, interfaces! validates maps at runtime; this checker
  ;; does not purport to prove its complete validator implementation.
  (doseq [[name signature]
          [['gesso.assurance.proof.operation-interface-composition/interfaces!
            ['typed.clojure/Any '->
             '(typed.clojure/Map typed.clojure/Any
                                 (typed.clojure/Map typed.clojure/Any typed.clojure/Any))]]
           ['gesso.assurance.proof.operation-interface-composition/common-state-types!
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.operation-interface-composition/invariant-results!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.operation-interface-composition/sorted-map*
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.operation-interface-composition/interactions
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.subject/describe-subject
            ['typed.clojure/Any '->
             '(typed.clojure/HMap :mandatory {:fingerprint typed.clojure/Str}
                                  :complete? false)]]
           ['gesso.assurance.proof.operation-interface/derive-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> source-type]]
           ['gesso.assurance.proof.operation-interface-composition/compose-derived!
            [source-type '-> composed-type]]
           ['gesso.assurance.proof.operation-interface-composition/compose-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> composed-type]]
           ['gesso.assurance.proof.operation-interface-composition/require-current-operation-composition!
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Any
             app-handle '-> composed-type]]
           ['gesso.assurance.proof.operation-interface-composition/reject!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Nothing]]]]
    (fn-ann! name signature)))

(defn- check! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original operation composition failed Typed Clojure"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Invalid composition expression escaped checking"
                      {:label label :errors kinds :exception (some-> ex ex-message)} ex)))
    true))

(def ^:private provenance
  {:fingerprint '(:fingerprint identity)
   :catalog-id '(:catalog-id derived)
   :catalog-fingerprint '(:catalog-fingerprint derived)
   :induction-fingerprint '(:induction-fingerprint derived)
   :interface-fingerprint '(:fingerprint derived)
   :operation-ids '(:operation-ids derived)
   :invariant-ids '(:invariant-ids derived)
   :original-open-obligation-ids-by-operation 'original
   :open-meta-obligations 'open-meta
   :open-assumptions '(:open-assumptions derived)})

(defn- original-result [source]
  (let [body (last source)]
    (when-not (and (seq? body) (= 'let (first body)) (map? (last body)))
      (throw (ex-info "Production composition result moved" {})))
    (last body)))

(defn- assert-provenance! [source]
  (let [result (original-result source)]
    (doseq [[k expression] provenance]
      (when-not (= expression (get result k ::missing))
        (throw (ex-info "Composition report source value changed"
                        {:field k :expected expression :actual (get result k)}))))
    true))

(defn- modify-result [source change]
  (let [body (last source)
        result (original-result source)
        mutated (change result)]
    (when (= mutated result)
      (throw (ex-info "No-op composition mutation" {})))
    (apply list (concat (butlast source)
                        [(apply list (concat (butlast body) [mutated]))]))))

(defn- audit-public! [forms]
  (let [public (original forms 'compose-operation-interfaces!)
        fresh (original forms 'require-current-operation-composition!)
        public-call '(compose-derived!
                      (iface/derive-operation-interfaces! catalog current-graph closure))
        body (last fresh)]
    (when-not (= public-call (last public))
      (throw (ex-info "Composition no longer derives original interfaces"
                      {:actual (last public)})))
    (when-not (and (= '(compose-operation-interfaces! catalog current-graph closure)
                      (second (second body)))
                   (= '(when-not (and (map? prior) (not (record? prior))
                                     (nil? (meta prior)) (= prior now))
                         (reject! :stale-or-substituted-operation-composition
                                  {:expected (:fingerprint now)
                                   :actual (:fingerprint prior)}))
                      (nth body 2))
                   (= 'now (last body)))
      (throw (ex-info "Composition freshness source comparison changed"
                      {:source body})))
    true))

(defn- checks! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (doseq [name '[compose-derived! compose-operation-interfaces!
                   require-current-operation-composition!]]
      (check! checker [:original name] (original forms name)))
    (audit-public! forms)
    (let [source (original forms 'compose-derived!)]
      (assert-provenance! source)
      ;; Status fields are NOT just comments. The exact checked HMap
      ;; rejects false claims of proof, authority, runtime coverage, etc.
      (doseq [field (keys report-statuses)]
        (reject! checker [:false-report-status field]
                 (modify-result source #(assoc % field :proved))))
      (doseq [[label update-map]
              [[:missing-operation-ids #(dissoc % :operation-ids)]
               [:missing-invariants #(dissoc % :invariant-ids)]
               [:missing-contracts #(dissoc % :operation-contracts)]
               [:wrong-report-fingerprint #(assoc % :fingerprint 17)]
               [:wrong-operation-ids #(assoc % :operation-ids [])]
               [:wrong-invariant-ids #(assoc % :invariant-ids {})]
               [:invented-authorization #(assoc % :transaction-authority :granted)]
               [:meta-as-map #(assoc % :open-meta-obligations {})]
               [:assumptions-as-set #(assoc % :open-assumptions #{:open})]
               [:original-obligations-as-set
                #(assoc % :original-open-obligation-ids-by-operation #{:op})]
               [:meta-from-assumptions
                #(assoc % :open-meta-obligations '(:open-assumptions derived))]
               [:assumptions-from-meta
                #(assoc % :open-assumptions '(:open-meta-obligations derived))]
               [:original-from-assumptions
                #(assoc % :original-open-obligation-ids-by-operation
                        '(:open-assumptions derived))]
               [:wrong-original-id-shape
                #(assoc % :original-open-obligation-ids-by-operation
                        '{:operation/x #{:not-a-vector}})]]]
        (reject! checker label (modify-result source update-map)))
      ;; Same-typed values can be improperly wired without violating their
      ;; current types. Treat this as a source-provenance audit, NOT typing.
      (doseq [[label change]
              [[:catalog-from-induction
                #(assoc % :catalog-fingerprint '(:induction-fingerprint derived))]
               [:interface-from-subject
                #(assoc % :interface-fingerprint '(:fingerprint identity))]]]
        (let [mutant (modify-result source change)]
          (check! checker [:type-compatible-wrong-provenance label] mutant)
          (when-not (try (assert-provenance! mutant)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Provenance mutation escaped source check"
                            {:label label}))))))
    ;; This source-backed constructor receives a structural derived-interface
    ;; HMap. Wrong premise family inputs to that ORIGINAL function must fail
    ;; even if all other interface fields are legitimate.
    (doseq [[label alteration]
            [[:meta-as-classified
              #(assoc % :open-meta-obligations '(:open-assumptions derived))]
             [:classified-as-meta
              #(assoc % :open-assumptions '(:open-meta-obligations derived))]
             [:operation-premises-as-classified
              #(assoc % :original-open-obligation-ids-by-operation
                      '(:open-assumptions derived))]]]
      (reject! checker [:wrong-composed-premise-family label]
               (modify-result (original forms 'compose-derived!) alteration)))
    (reject! checker :raw-closure-public
             '(compose-operation-interfaces! {} {} {}))
    (reject! checker :nil-closure-public
             '(compose-operation-interfaces! {} {} nil))
    (reject! checker :raw-closure-freshness
             '(require-current-operation-composition! {} {} {} {}))
    (reject! checker :nil-closure-freshness
             '(require-current-operation-composition! {} {} {} nil))
    true))

(defn- runtime-guard! []
  ;; Directly constructed nominal instances and copied reports remain
  ;; unregistered/untrusted. This function never issues a real closure.
  (let [closure (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        reason (try
                 (gesso.assurance.proof.operation-interface-composition/compose-operation-interfaces!
                  {} {} closure)
                 nil
                 (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))]
    (when-not (= :unrecognized-closure-handle reason)
      (throw (ex-info "Forged application closure used for composition"
                      {:reason reason})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (source-forms)]
    (checks! checker forms)
    (runtime-guard!)
    (println "Typed operation composition passed: 3 original source bodies,")
    (println "29 invalid composition-map mutations, 4 nominal closure calls,")
    (println "2 type-compatible provenance swaps caught by source audit,")
    (println "and runtime forged-closure rejection; no proof/authority claim.")
    :ok))

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
            [clojure.walk :as walk]
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
;; These literal-union alphabets were checked against the ORIGINAL per-
;; operation invariant-result-entry constructor in the upstream derivation gate.
;; The aggregate uses the same initial/step and induction status families.
(def ^:private inference-type iface-check/inference-status-type)
(def ^:private induction-type iface-check/induction-status-type)
(def aggregate-invariant-type
  (list 'typed.clojure/HMap
        :mandatory {:initial-status inference-type
                    :preservation-by-operation
                    (list 'typed.clojure/Map 'typed.clojure/Kw inference-type)
                    :induction-status induction-type
                    :open-reason 'typed.clojure/Any}
        :complete? true))
(def ^:private aggregate-results-type
  (list 'typed.clojure/Map 'typed.clojure/Kw aggregate-invariant-type))
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
          :invariant-results aggregate-results-type
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
             (list 'typed.clojure/Map 'typed.clojure/Kw
                   iface-check/operation-contract-type)]]
           ['gesso.assurance.proof.operation-interface-composition/common-state-types!
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ;; The runtime aggregator validates source status agreement; its
           ;; output category is audited against the ORIGINAL function. The
           ;; full for/reduce algorithm is NOT independently checked here.
           ['gesso.assurance.proof.operation-interface-composition/invariant-results!
            ['typed.clojure/Any 'typed.clojure/Any '-> aggregate-results-type]]
           ['gesso.assurance.proof.operation-interface-composition/aggregate-invariant-entry
            [inference-type
             (list 'typed.clojure/Map 'typed.clojure/Kw inference-type)
             induction-type 'typed.clojure/Any '-> aggregate-invariant-type]]
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

(defn- aggregate-form! [forms]
  ;; Check the exact ORIGINAL pure descriptor body, not a copied surrogate.
  (let [source (original forms 'aggregate-invariant-entry)]
    (when-not (= {:initial-status 'initial-status
                  :preservation-by-operation 'preservation-by-operation
                  :induction-status 'induction-status
                  :open-reason 'open-reason}
                 (last source))
      (throw (ex-info "Original aggregated invariant descriptor changed" {})))
    source))

(def ^:private expected-aggregate-call
  '(aggregate-invariant-entry
    (first initial)
    (sorted-map*
     (for [[op-id result] results]
       [op-id (:preservation-status result)]))
    (first induction)
    (first open-reasons)))

(def ^:private expected-aggregate-guard
  '(when-not (and (= 1 (count initial))
                  (= 1 (count induction))
                  (= 1 (count open-reasons))
                  (set/subset? steps
                               #{:checked-conditional-inference :open})
                  (= (first induction)
                     (if all-checked?
                       :checked-conditional-induction :open)))
     (reject! :incompatible-invariant-evidence
              {:invariant-id invariant-id})))

(defn- source-nodes [form predicate]
  (let [found (atom [])]
    (walk/postwalk (fn [node]
                     (when (predicate node) (swap! found conj node))
                     node) form)
    @found))

(def ^:private expected-status-acquisition
  {'results
   '(sorted-map*
     (for [[op-id op] interfaces]
       [op-id (get-in op [:conditional-invariant-results invariant-id])]))
   'values '(vals results)
   'initial '(set (map :initial-status values))
   'induction '(set (map :induction-status values))
   'open-reasons '(set (map :open-reason values))
   'steps '(into #{} (map :preservation-status) values)
   'all-checked? '(and (= initial #{:checked-conditional-inference})
                       (= steps #{:checked-conditional-inference}))})

(defn- check-aggregate-source! [forms]
  (let [source (original forms 'invariant-results!)
        body (last source)
        iteration (second body)
        let-body (when (and (seq? iteration) (= 'for (first iteration)))
                   (nth iteration 2))
        bindings (when (and (seq? let-body) (= 'let (first let-body)))
                   (second let-body))
        acquired (when (vector? bindings)
                   (into {} (map vec (partition 2 bindings))))
        calls (source-nodes source #(and (seq? %)
                                         (= 'aggregate-invariant-entry (first %))))
        guards (source-nodes source #(and (seq? %)
                                          (= 'when-not (first %))))]
    (when-not (and (= '[invariant-id (sorted-ids invariant-ids)] (second iteration))
                   (= expected-status-acquisition acquired)
                   (= [expected-aggregate-call] calls)
                   (= [expected-aggregate-guard] guards))
      (throw (ex-info "Original invariant aggregation lost source agreement"
                      {:acquired acquired :calls calls :guards guards})))
    true))

(defn- replace-unique-source [source original-expression substitution]
  (let [hits (atom 0)
        altered (walk/postwalk
                 (fn [node]
                   (if (= node original-expression)
                     (do (swap! hits inc) substitution)
                     node)) source)]
    (when-not (= 1 @hits)
      (throw (ex-info "Original invariant source expression changed"
                      {:expression original-expression :hits @hits})))
    altered))

(defn- check-aggregate! [checker forms]
  (let [source (aggregate-form! forms)
        aggregate-source (original forms 'invariant-results!)]
    (check! checker :original-aggregate-invariant-entry source)
    (check-aggregate-source! forms)
    (check! checker :valid-aggregate-invariant-call
            '(aggregate-invariant-entry
              :checked-conditional-inference
              {:operation/a :checked-conditional-inference :operation/b :open}
              :open :source-has-an-open-step))
    (doseq [[name transform]
            [[:wrong-initial-status #(assoc % :initial-status :proved)]
             [:wrong-step-status #(assoc % :preservation-by-operation
                                          '{:operation/a :proved})]
             [:wrong-step-map #(assoc % :preservation-by-operation #{:open})]
             [:wrong-step-map-key #(assoc % :preservation-by-operation
                                           '{"not-a-keyword" :open})]
             [:wrong-induction-status #(assoc % :induction-status :proved)]
             [:induction-from-initial #(assoc % :induction-status 'initial-status)]
             [:initial-from-induction #(assoc % :initial-status 'induction-status)]
             [:missing-initial #(dissoc % :initial-status)]
             [:missing-preservation #(dissoc % :preservation-by-operation)]
             [:missing-induction #(dissoc % :induction-status)]
             [:missing-open-reason #(dissoc % :open-reason)]
             [:invented-certificate #(assoc % :certificate :issued)]
             [:invented-proof-status #(assoc % :proof-status :proved)]]]
      (let [body (last source)
            mutated (transform body)]
        (when (= body mutated)
          (throw (ex-info "No-op aggregate test mutation" {:name name})))
        (reject! checker [:malformed-invariant-aggregate name]
                 (apply list (concat (butlast source) [mutated])))))
    (doseq [[name expression]
            [[:raw-preservation
              '(aggregate-invariant-entry :open {} :proved :reason)]
             [:wrong-initial-call
              '(aggregate-invariant-entry :checked-conditional-induction {} :open nil)]
             [:wrong-induction-call
              '(aggregate-invariant-entry :open {} :checked-conditional-inference nil)]
             [:wrong-preservation-call
              '(aggregate-invariant-entry :open {:operation/a :proved} :open nil)]]]
      (reject! checker [:aggregate-argument name] expression))
    ;; Same-typed operation keys can be switched or one operation's status
    ;; substituted for another without static error. The ORIGINAL source
    ;; audit must preserve exactly which status belongs to which operation.
    (doseq [[name old new]
            [[:wrong-preservation-value
              '(:preservation-status result) '(:initial-status result)]
             [:wrong-operation-key 'op-id 'invariant-id]
             [:wrong-overall-induction '(first induction) '(first initial)]
             [:disabled-consistency-guard expected-aggregate-guard true]
             [:initial-from-step-set
              '(set (map :initial-status values))
              '(set (map :preservation-status values))]
             [:preservation-from-initial-set
              '(into #{} (map :preservation-status) values)
              '(into #{} (map :initial-status) values)]
             [:induction-from-initial-set
              '(set (map :induction-status values))
              '(set (map :initial-status values))]
             [:wrong-source-invariant
              '(get-in op [:conditional-invariant-results invariant-id])
              '(get-in op [:conditional-invariant-results :unrelated-id])]]]
      (let [changed (if (or (= old expected-aggregate-guard)
                            (contains? (set (vals expected-status-acquisition)) old)
                            (= old '(get-in op [:conditional-invariant-results
                                                    invariant-id])))
                      (replace-unique-source aggregate-source old new)
                      (replace-unique-source aggregate-source
                       expected-aggregate-call
                       (walk/postwalk-replace {old new}
                                              expected-aggregate-call)))
            substituted (mapv #(if (= aggregate-source %) changed %) forms)]
        (when-not (try (check-aggregate-source! substituted)
                       false
                       (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Mutated source escaped aggregate audit"
                          {:case name})))))
    true))

(defn- checks! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (doseq [name '[compose-derived! compose-operation-interfaces!
                   require-current-operation-composition!]]
      (check! checker [:original name] (original forms name)))
    (audit-public! forms)
    (check-aggregate! checker forms)
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
               [:wrong-invariant-results-container
                #(assoc % :invariant-results [])]
               [:wrong-aggregated-initial-status
                #(assoc % :invariant-results
                        '(assoc invariant-results :invariant/x
                                {:initial-status :proved
                                 :preservation-by-operation {}
                                 :induction-status :open
                                 :open-reason nil}))]
               [:wrong-aggregated-induction-status
                #(assoc % :invariant-results
                        '(assoc invariant-results :invariant/x
                                {:initial-status :open
                                 :preservation-by-operation {}
                                 :induction-status :proved
                                 :open-reason nil}))]
               [:wrong-preservation-status-map
                #(assoc % :invariant-results
                        '(assoc invariant-results :invariant/x
                                {:initial-status :open
                                 :preservation-by-operation {:operation/a :proved}
                                 :induction-status :open
                                 :open-reason nil}))]
               [:invented-aggregate-proof
                #(assoc % :invariant-results
                        '(assoc invariant-results :invariant/x
                                {:initial-status :open
                                 :preservation-by-operation {}
                                 :induction-status :open
                                 :open-reason nil
                                 :proof-status :proved}))]
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
    (println "Typed operation composition passed: 4 original source bodies,")
    (println "34 invalid composition-map mutations, 17 aggregate mutations,")
    (println "8 original aggregate-source mutants and 4 nominal closure calls;")
    (println "2 report provenance swaps and runtime forged-closure rejection.")
    :ok))

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
(def ^:private open-reason-type iface-check/open-reason-type)
(def aggregate-invariant-type
  (list 'typed.clojure/HMap
        :mandatory {:initial-status inference-type
                    :preservation-by-operation
                    (list 'typed.clojure/Map 'typed.clojure/Kw inference-type)
                    :induction-status induction-type
                    :open-reason open-reason-type}
        :complete? true))
(def ^:private aggregate-results-type
  (list 'typed.clojure/Map 'typed.clojure/Kw aggregate-invariant-type))
;; The complete ORIGINAL per-operation reduction now uses this source-checked
;; collection shape instead of an assumed output from a nested for expression.
;; The empty sorted-map constructor's result type is assumed and its original
;; zero-element body is audited; the checker does not prove its implementation.
(def ^:private individual-results-type
  (list 'typed.clojure/Map 'typed.clojure/Kw iface-check/invariant-result-type))
(def ^:private preservation-statuses-type
  (list 'typed.clojure/Map 'typed.clojure/Kw inference-type))
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
           ['gesso.assurance.proof.operation-interface-composition/initial-status-row
            [(list 'typed.clojure/Set inference-type) 'typed.clojure/Kw
             iface-check/invariant-result-type '->
             (list 'typed.clojure/Set inference-type)]]
           ['gesso.assurance.proof.operation-interface-composition/initial-statuses
            [individual-results-type '-> (list 'typed.clojure/Set inference-type)]]
           ['gesso.assurance.proof.operation-interface-composition/induction-status-row
            [(list 'typed.clojure/Set induction-type) 'typed.clojure/Kw
             iface-check/invariant-result-type '->
             (list 'typed.clojure/Set induction-type)]]
           ['gesso.assurance.proof.operation-interface-composition/induction-statuses
            [individual-results-type '-> (list 'typed.clojure/Set induction-type)]]
           ['gesso.assurance.proof.operation-interface-composition/preservation-step-row
            [(list 'typed.clojure/Set inference-type) 'typed.clojure/Kw
             iface-check/invariant-result-type '->
             (list 'typed.clojure/Set inference-type)]]
           ['gesso.assurance.proof.operation-interface-composition/preservation-step-statuses
            [individual-results-type '-> (list 'typed.clojure/Set inference-type)]]
           ['gesso.assurance.proof.operation-interface-composition/operation-invariant-row
            [individual-results-type 'typed.clojure/Kw
             iface-check/invariant-result-type '-> individual-results-type]]
           ['gesso.assurance.proof.operation-interface-composition/operation-invariant-evidence!
            [iface-check/operation-contract-type 'typed.clojure/Kw
             '-> iface-check/invariant-result-type]]
           ['gesso.assurance.proof.operation-interface-composition/operation-invariant-result-row
            [individual-results-type 'typed.clojure/Kw 'typed.clojure/Kw
             iface-check/operation-contract-type '-> individual-results-type]]
           ['gesso.assurance.proof.operation-interface-composition/empty-operation-invariant-results
            ['-> individual-results-type]]
           ['gesso.assurance.proof.operation-interface-composition/operation-invariant-results
            [(list 'typed.clojure/Map 'typed.clojure/Kw
                   iface-check/operation-contract-type)
             'typed.clojure/Kw '-> individual-results-type]]
           ['gesso.assurance.proof.operation-interface-composition/open-reason-row
            [(list 'typed.clojure/Set open-reason-type)
             'typed.clojure/Kw iface-check/invariant-result-type
             '-> (list 'typed.clojure/Set open-reason-type)]]
           ['gesso.assurance.proof.operation-interface-composition/open-reasons
            [individual-results-type '-> (list 'typed.clojure/Set open-reason-type)]]
           ['gesso.assurance.proof.operation-interface-composition/consistent-invariant-statuses?
            [(list 'typed.clojure/Set inference-type)
             (list 'typed.clojure/Set induction-type)
             (list 'typed.clojure/Set open-reason-type)
             (list 'typed.clojure/Set inference-type)
             '-> 'typed.clojure/Bool]]
           ['gesso.assurance.proof.operation-interface-composition/empty-preservation-statuses
            ['-> preservation-statuses-type]]
           ['gesso.assurance.proof.operation-interface-composition/preservation-status-row
            [preservation-statuses-type 'typed.clojure/Kw
             iface-check/invariant-result-type '-> preservation-statuses-type]]
           ['gesso.assurance.proof.operation-interface-composition/preservation-statuses
            [individual-results-type '-> preservation-statuses-type]]
           ['gesso.assurance.proof.operation-interface-composition/aggregate-invariant-entry
            [inference-type
             (list 'typed.clojure/Map 'typed.clojure/Kw inference-type)
             induction-type open-reason-type '-> aggregate-invariant-type]]
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
    (preservation-statuses results)
    (first induction)
    (first open-reasons)))

(def ^:private expected-status-consistency
  '(let [all-checked? (and (= initial #{:checked-conditional-inference})
                           (= steps #{:checked-conditional-inference}))]
     (and (= 1 (count initial))
          (= 1 (count induction))
          (= 1 (count open-reasons))
          (set/subset? steps #{:checked-conditional-inference :open})
          (= (first induction)
             (if all-checked?
               :checked-conditional-induction :open)))))
(def ^:private expected-aggregate-guard
  '(when-not (consistent-invariant-statuses?
               initial induction open-reasons steps)
     (reject! :incompatible-invariant-evidence
              {:invariant-id invariant-id})))

(defn- source-nodes [form predicate]
  (let [found (atom [])]
    (walk/postwalk (fn [node]
                     (when (predicate node) (swap! found conj node))
                     node) form)
    @found))

(def ^:private expected-status-acquisition
  {'results '(operation-invariant-results interfaces invariant-id)
   'initial '(initial-statuses results)
   'induction '(induction-statuses results)
   'open-reasons '(open-reasons results)
   'steps '(preservation-step-statuses results)})

(def ^:private expected-preservation-row
  '(assoc status-map op-id (:preservation-status result)))
(def ^:private expected-preservation-reduction
  '(reduce-kv preservation-status-row (empty-preservation-statuses) results))

(defn- check-preservation-source! [forms]
  ;; Type-check the complete ORIGINAL pair insertion AND reduction functions.
  ;; An annotated empty-map constructor is a trusted helper contract here;
  ;; audit its actual body so a differently sorted or prefilled map rejects.
  (let [empty-source (original forms 'empty-preservation-statuses)
        row-source (original forms 'preservation-status-row)
        reduction (original forms 'preservation-statuses)]
    (when-not (and (= '(sorted-map* []) (last empty-source))
                   (= expected-preservation-row (last row-source))
                   (= expected-preservation-reduction (last reduction)))
      (throw (ex-info "Original operation preservation extraction changed"
                      {:empty (last empty-source)
                       :row (last row-source)
                       :reduction (last reduction)})))
    true))

(def ^:private expected-status-extractors
  {'initial-status-row '(conj statuses (:initial-status result))
   'initial-statuses '(reduce-kv initial-status-row #{} results)
   'induction-status-row '(conj statuses (:induction-status result))
   'induction-statuses '(reduce-kv induction-status-row #{} results)
   'preservation-step-row '(conj statuses (:preservation-status result))
   'preservation-step-statuses '(reduce-kv preservation-step-row #{} results)})

(defn- check-status-extraction-source! [forms]
  ;; The actual ORIGINAL per-operation reducers must extract their own
  ;; status families. Literal-union typing alone cannot distinguish initial
  ;; and preservation, which share the same status alphabet.
  (doseq [[name expected] expected-status-extractors]
    (let [actual (last (original forms name))]
      (when-not (= expected actual)
        (throw (ex-info "Original operation-status extraction changed"
                        {:name name :expected expected :actual actual})))))
  true)

(def ^:private expected-evidence-extractors
  {'operation-invariant-row
   '(assoc entries operation-id invariant-result)
   'operation-invariant-evidence!
   '(if-let [result (get (:conditional-invariant-results operation) invariant-id)]
      result
      (reject! :missing-operation-invariant-evidence
               {:operation-id (:operation-id operation)
                :invariant-id invariant-id}))
   'operation-invariant-result-row
   '(operation-invariant-row
     entries operation-id (operation-invariant-evidence! operation invariant-id))
   'empty-operation-invariant-results '(sorted-map* [])
   'operation-invariant-results
   '(reduce-kv (fn [entries operation-id operation]
                 (operation-invariant-result-row
                  entries invariant-id operation-id operation))
               (empty-operation-invariant-results) interfaces)
   'open-reason-row '(conj reasons (:open-reason result))
   'open-reasons '(reduce-kv open-reason-row #{} results)})

(defn- check-evidence-extraction-source! [forms]
  (doseq [[name expected] expected-evidence-extractors]
    (when-not (= expected (last (original forms name)))
      (throw (ex-info "Original operation/invariant evidence source changed"
                      {:name name :expected expected
                       :actual (last (original forms name))}))))
  true)

(defn- check-status-consistency-source! [forms]
  ;; An annotation can type-check a subtly WRONG predicate. Verify the
  ;; complete original definition of its agreement relation independently.
  (let [source (original forms 'consistent-invariant-statuses?)]
    (when-not (= expected-status-consistency (last source))
      (throw (ex-info "Original invariant status consistency rule changed"
                      {:source (last source)})))
    true))

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
    (doseq [name '[preservation-status-row preservation-statuses]]
      (check! checker [:original-preservation-extraction name]
              (original forms name)))
    (check-preservation-source! forms)
    (check-status-extraction-source! forms)
    (check-evidence-extraction-source! forms)
    (check-status-consistency-source! forms)
    (check-aggregate-source! forms)
    (doseq [name (keys expected-status-extractors)]
      (check! checker [:original-status-extraction name] (original forms name)))
    ;; Unlike the former nested get-in (typed Any), the original direct map
    ;; lookup has a source-backed contract and an explicit missing-evidence
    ;; rejection. Its row and COMPLETE reduce-kv aggregation are now checked.
    ;; The sorted-map* empty initializer remains an audited assumed contract.
    (doseq [name '[operation-invariant-row operation-invariant-evidence!
                   operation-invariant-result-row operation-invariant-results
                   open-reason-row open-reasons]]
      (check! checker [:original-evidence-extraction name] (original forms name)))
    (doseq [[label malformed]
            [[:invented-operation-reason
              '(open-reason-row #{} :operation/a
                {:initial-status :open :preservation-status :open
                 :induction-status :open :open-reason :proved})]
             [:nonkeyword-operation-key
              '(operation-invariant-row {} "operation/a"
                {:initial-status :open :preservation-status :open
                 :induction-status :open :open-reason nil})]
             [:invented-result-reason
              '(operation-invariant-row {} :operation/a
                {:initial-status :open :preservation-status :open
                 :induction-status :open :open-reason :unrecognized})]
             [:invalid-reason-input
              '(open-reasons
                {:operation/a {:initial-status :open :preservation-status :open
                               :induction-status :open :open-reason []}})]]]
      (reject! checker [:wrong-evidence-source-input label] malformed))
    (let [source (original forms 'operation-invariant-evidence!)]
      ;; Check mutations of the actual ORIGINAL lookup against its precise
      ;; four-field return HMap, including its failure-path return type.
      (doseq [[label replacement]
              [[:invented-proof '(if-let [result (get (:conditional-invariant-results operation) invariant-id)]
                                    :proved
                                    (reject! :missing-operation-invariant-evidence {}))]
               [:missing-returns-nil '(if-let [result (get (:conditional-invariant-results operation) invariant-id)]
                                         result nil)]
               [:return-result-map '(if-let [result (get (:conditional-invariant-results operation) invariant-id)]
                                      (:conditional-invariant-results operation)
                                      (reject! :missing-operation-invariant-evidence {}))]
               [:nested-any-get-in '(get-in operation [:conditional-invariant-results invariant-id])]]]
        (reject! checker [:wrong-invariant-lookup label]
                 (apply list (concat (butlast source) [replacement])))))
    (let [source (original forms 'operation-invariant-result-row)]
      (doseq [[label replacement]
              [[:string-operation-key
                '(operation-invariant-row
                  entries "operation" (operation-invariant-evidence! operation invariant-id))]
               [:wrong-row-value
                '(operation-invariant-row entries operation-id
                  (:preservation-status
                   (operation-invariant-evidence! operation invariant-id)))]]]
        (reject! checker [:wrong-operation-result-row label]
                 (apply list (concat (butlast source) [replacement])))))
    (doseq [[label malformed]
            [[:raw-operation '(operation-invariant-evidence! {} :invariant/a)]
             [:wrong-invariant-id '(operation-invariant-results {} "invariant/a")]
             [:wrong-operation-index '(operation-invariant-results {"operation" {}} :invariant/a)]]]
      (reject! checker [:malformed-operation-lookup-call label] malformed))
    (let [example {:initial-status :open
                   :preservation-status :checked-conditional-inference
                   :induction-status :open
                   :open-reason :some-preservation-open}
          other {:initial-status :open
                 :preservation-status :open
                 :induction-status :open
                 :open-reason :some-preservation-open}
          peer {:initial-status :open
                :preservation-status :open
                :induction-status :open
                :open-reason nil}
          interfaces (sorted-map
                      :operation/a {:conditional-invariant-results
                                    {:invariant/a example :invariant/b peer}}
                      :operation/z {:conditional-invariant-results
                                    {:invariant/a other :invariant/b peer}})
          mapped (var-get
                  #'gesso.assurance.proof.operation-interface-composition/operation-invariant-results)
          reasons (var-get
                   #'gesso.assurance.proof.operation-interface-composition/open-reasons)
          sorted* (var-get
                   #'gesso.assurance.proof.operation-interface-composition/sorted-map*)]
      ;; Compare LIVE extraction against the original nested for, for two
      ;; invariants, preserving all original operation keys and sorted order.
      (doseq [id [:invariant/a :invariant/b]]
        (let [expected (sorted*
                        (for [[op-id op] interfaces]
                          [op-id (get-in op [:conditional-invariant-results id])]))
              actual (mapped interfaces id)]
          (when-not (and (= expected actual)
                         (= (vec (keys expected)) (vec (keys actual)))
                         (= (set (map :open-reason (vals actual)))
                            (reasons actual)))
            (throw (ex-info "Original invariant evidence extraction changed"
                            {:invariant-id id :expected expected :actual actual}))))))
    ;; Substituting wrong original IDs still type-checks, but violates the
    ;; identity relationship and must be refused by source provenance audit.
    (doseq [[label name replacement same-type?]
            [[:wrong-operation-key 'operation-invariant-row
              '(assoc entries :operation/other invariant-result) true]
             [:wrong-lookup-invariant 'operation-invariant-evidence!
              '(if-let [result
                        (get (:conditional-invariant-results operation)
                             :invariant/other)]
                 result
                 (reject! :missing-operation-invariant-evidence
                          {:operation-id (:operation-id operation)
                           :invariant-id invariant-id})) true]
             [:wrong-rejection-operation 'operation-invariant-evidence!
              '(if-let [result
                        (get (:conditional-invariant-results operation) invariant-id)]
                 result
                 (reject! :missing-operation-invariant-evidence
                          {:operation-id invariant-id
                           :invariant-id invariant-id})) true]
             [:swapped-operation-id 'operation-invariant-result-row
              '(operation-invariant-row
                entries invariant-id
                (operation-invariant-evidence! operation invariant-id)) true]
             [:wrong-row-lookup-invariant 'operation-invariant-result-row
              '(operation-invariant-row
                entries operation-id
                (operation-invariant-evidence! operation :invariant/other)) true]
             [:wrong-loop-invariant 'operation-invariant-results
              '(reduce-kv
                (fn [entries operation-id operation]
                  (operation-invariant-result-row
                   entries :invariant/other operation-id operation))
                (empty-operation-invariant-results) interfaces) true]
             [:missing-operation-rows 'operation-invariant-results
              '(reduce-kv
                (fn [entries operation-id operation]
                  (operation-invariant-result-row
                   entries invariant-id operation-id operation))
                (empty-operation-invariant-results) {}) true]
             [:wrong-sorted-initializer 'empty-operation-invariant-results
              '(sorted-map* [[:operation/fake {:open-reason :proved}]]) false]
             [:wrong-reason-field 'open-reason-row
              '(conj reasons (:initial-status result)) false]
             [:omitted-reasons 'open-reasons
              '(reduce-kv open-reason-row #{} {}) false]]]
      (let [original-source (original forms name)
            altered (apply list (concat (butlast original-source) [replacement]))
            mutated-forms (mapv #(if (= % original-source) altered %) forms)]
        (when same-type?
          (check! checker [:type-compatible-wrong-lookup label] altered))
        (when-not (try (check-evidence-extraction-source! mutated-forms)
                       false
                       (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Incorrect operation/invariant lookup escaped audit"
                          {:label label})))))
    ;; A missing invariant was previously represented as a nil row. The new
    ;; original lookup now rejects it explicitly; no absent evidence can be
    ;; mistaken for a valid four-field conditional descriptor.
    (let [lookup (var-get
                  #'gesso.assurance.proof.operation-interface-composition/operation-invariant-evidence!)
          result {:initial-status :open :preservation-status :open
                  :induction-status :open :open-reason nil}
          operation {:operation-id :operation/a
                     :conditional-invariant-results {:invariant/a result}}]
      (when-not (= result (lookup operation :invariant/a))
        (throw (ex-info "Original invariant lookup changed" {})))
      (let [reason (try (lookup operation :invariant/missing)
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (:reason (ex-data e))))]
        (when-not (= :missing-operation-invariant-evidence reason)
          (throw (ex-info "Missing original invariant evidence not rejected"
                          {:reason reason})))))
    (check! checker :original-invariant-status-agreement
            (original forms 'consistent-invariant-statuses?))
    (check! checker :valid-open-reason-call
            '(open-reasons {:operation/a
                            {:initial-status :open
                             :preservation-status :open
                             :induction-status :open
                             :open-reason :some-preservation-open}}))
    (check! checker :valid-consistent-status-call
            '(consistent-invariant-statuses?
              #{:checked-conditional-inference}
              #{:checked-conditional-induction}
              #{nil} #{:checked-conditional-inference}))
    (let [live (the-ns source-ns)
          inputs (sorted-map
                  :operation/a {:initial-status :open
                                :induction-status :open
                                :preservation-status :checked-conditional-inference}
                  :operation/z {:initial-status :checked-conditional-inference
                                :induction-status :checked-conditional-induction
                                :preservation-status :open})]
      ;; Compare all three LIVE reducers to the original map/vals semantics.
      (doseq [[extractor status]
              [['initial-statuses :initial-status]
               ['induction-statuses :induction-status]
               ['preservation-step-statuses :preservation-status]]]
        (let [actual ((var-get (ns-resolve live extractor)) inputs)
              expected (set (map status (vals inputs)))]
          (when-not (= actual expected)
            (throw (ex-info "Status extraction changed at runtime"
                            {:extractor extractor :actual actual
                             :expected expected}))))))
    ;; Deliberately invalid statuses fail against the source-backed
    ;; per-operation contract before they reach agreement checking.
    (doseq [[label invalid]
            [[:initial-instead-of-induction
              '(induction-status-row #{} :operation/a
                {:initial-status :open :induction-status :checked-conditional-inference
                 :preservation-status :open :open-reason nil})]
             [:proof-initial '(initial-statuses
                               {:operation/a {:initial-status :proved
                                              :induction-status :open
                                              :preservation-status :open
                                              :open-reason nil}})]
             [:proof-step '(preservation-step-statuses
                             {:operation/a {:initial-status :open
                                            :induction-status :open
                                            :preservation-status :proved
                                            :open-reason nil}})]
             [:wrong-key '(induction-statuses
                            {"operation/a" {:initial-status :open
                                             :induction-status :open
                                             :preservation-status :open
                                             :open-reason nil}})]
             [:raw-vector '(initial-statuses [:open])]]]
      (reject! checker [:malformed-source-status-family label] invalid))
    ;; For both rows and reductions, audit their ORIGINAL status source and
    ;; map domain. Inference and preservation share the same literal types,
    ;; so those source swaps remain type-compatible and require source audit.
    (doseq [[label name alternate]
            [[:initial-uses-preservation 'initial-status-row
              '(conj statuses (:preservation-status result))]
             [:induction-uses-initial 'induction-status-row
              '(conj statuses (:initial-status result))]
             [:preservation-uses-initial 'preservation-step-row
              '(conj statuses (:initial-status result))]
             [:initial-drops-all-operations 'initial-statuses
              '(reduce-kv initial-status-row #{} {})]
             [:induction-uses-wrong-reducer 'induction-statuses
              '(reduce-kv initial-status-row #{} results)]
             [:preservation-drops-all-operations 'preservation-step-statuses
              '(reduce-kv preservation-step-row #{} {})]]]
      (let [source (original forms name)
            mutant (apply list (concat (butlast source) [alternate]))
            changed (mapv #(if (= source %) mutant %) forms)]
        (when (#{:initial-uses-preservation :preservation-uses-initial} label)
          (check! checker [:type-compatible-status-source label] mutant))
        (when-not (try (check-status-extraction-source! changed)
                       false
                       (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Wrong original status extraction escaped audit"
                          {:label label})))))
    (doseq [[label invalid]
            [[:invented-initial-proof
              '(consistent-invariant-statuses? #{:proved} #{:open} #{nil} #{:open})]
             [:induction-status-in-initial-set
              '(consistent-invariant-statuses?
                #{:checked-conditional-induction} #{:open} #{nil} #{:open})]
             [:inference-status-in-induction-set
              '(consistent-invariant-statuses?
                #{:open} #{:checked-conditional-inference} #{nil} #{:open})]
             [:invented-preservation-proof
              '(consistent-invariant-statuses?
                #{:open} #{:open} #{nil} #{:proved})]
             [:unstructured-initials
              '(consistent-invariant-statuses? [:open] #{:open} #{nil} #{:open})]
             [:wrong-reason-collection
              '(consistent-invariant-statuses? #{:open} #{:open} {} #{:open})]]]
      (reject! checker [:wrong-agreement-status-input label] invalid))
    (check! checker :valid-preservation-assembly
            '(preservation-statuses
              {:operation/a {:initial-status :open
                             :preservation-status :checked-conditional-inference
                             :induction-status :open :open-reason nil}
               :operation/b {:initial-status :open
                             :preservation-status :open
                             :induction-status :open :open-reason nil}}))
    (doseq [[label expression]
            [[:incorrect-nested-preservation
              '(preservation-statuses
                {:operation/a {:initial-status :open :preservation-status :proved
                               :induction-status :open :open-reason nil}})]
             [:incorrect-nested-induction
              '(preservation-statuses
                {:operation/a {:initial-status :open :preservation-status :open
                               :induction-status :checked-conditional-inference
                               :open-reason nil}})]
             [:non-keyword-operation-id
              '(preservation-statuses
                {"operation/a" {:initial-status :open :preservation-status :open
                                :induction-status :open :open-reason nil}})]
             [:not-operation-map '(preservation-statuses [:open])]]]
      (reject! checker [:malformed-preservation-input label] expression))
    (check! checker :valid-aggregate-invariant-call
            '(aggregate-invariant-entry
              :checked-conditional-inference
              {:operation/a :checked-conditional-inference :operation/b :open}
              :open :some-preservation-open))
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
    ;; The key/value insertion is now checked against the ORIGINAL individual
    ;; conditional-invariant HMap, rather than being hidden inside the for.
    (let [row (original forms 'preservation-status-row)]
      (doseq [[name bad-expression]
              [[:wrong-induction-status
                '(assoc status-map op-id (:induction-status result))]
               [:invented-proof-status '(assoc status-map op-id :proved)]
               [:wrong-map-key '(assoc status-map "operation" (:preservation-status result))]
               [:nil-status '(assoc status-map op-id nil)]
               [:raw-map '(assoc {} op-id :proved)]]]
        (reject! checker [:malformed-preservation-row name]
                 (apply list (concat (butlast row) [bad-expression]))))
      ;; Initial and preservation statuses share the same allowed literals.
      ;; The source audit, NOT static types, must catch substituting one.
      (doseq [[name altered]
              [[:initial-instead-of-preservation
                '(assoc status-map op-id (:initial-status result))]
               [:constant-operation-key
                '(assoc status-map :operation/other (:preservation-status result))]]]
        (let [mutant (apply list (concat (butlast row) [altered]))
              substituted (mapv #(if (= row %) mutant %) forms)]
          (check! checker [:same-type-wrong-preservation-source name] mutant)
          (when-not (try (check-preservation-source! substituted)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Invalid preservation row escaped source audit"
                            {:name name}))))))
    (let [reduction (original forms 'preservation-statuses)]
      (doseq [[name replacement]
              [[:omit-operation-set
                '(reduce-kv preservation-status-row (empty-preservation-statuses) {})]
               [:wrong-reduce-source
                '(reduce-kv preservation-status-row {} results)]
               [:ignore-operation-statuses
                '(reduce-kv (fn [m _ _] m) (empty-preservation-statuses) results)]]]
        (let [mutant (apply list (concat (butlast reduction) [replacement]))
              substituted (mapv #(if (= reduction %) mutant %) forms)]
          (when-not (try (check-preservation-source! substituted)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Invalid preservation reduction escaped audit"
                            {:name name}))))))
    ;; The live helper must reproduce the old source expression's full sorted
    ;; operation->status mapping, including open and conditionally checked rows.
    (let [inputs (sorted-map :operation/a {:preservation-status :open}
                             :operation/z {:preservation-status
                                           :checked-conditional-inference})
          actual ((var-get #'gesso.assurance.proof.operation-interface-composition/preservation-statuses)
                  inputs)
          expected ((var-get #'gesso.assurance.proof.operation-interface-composition/sorted-map*)
                    (for [[op-id result] inputs]
                      [op-id (:preservation-status result)]))]
      (when-not (and (= expected actual)
                     (= (vec (keys expected)) (vec (keys actual)))
                     (= (count inputs) (count actual)))
        (throw (ex-info "Original preservation mapping changed at runtime"
                        {:actual actual :expected expected}))))
    ;; A true/false matrix probes the original, LIVE relation, not the
    ;; annotated checker form (which is never evaluated by Typed Clojure).
    ;; A structurally type-correct altered predicate can still be wrong.
    (let [check-status (var-get
                        #'gesso.assurance.proof.operation-interface-composition/consistent-invariant-statuses?)
          checked :checked-conditional-inference
          open :open
          cases [[#{checked} #{:checked-conditional-induction} #{nil} #{checked} true]
                 [#{checked} #{open} #{nil} #{checked} false]
                 [#{checked} #{open} #{nil} #{checked open} true]
                 [#{open} #{open} #{nil} #{checked} true]
                 [#{open} #{:checked-conditional-induction} #{nil} #{checked} false]
                 [#{open} #{open} #{nil} #{open} true]
                 [#{checked open} #{open} #{nil} #{open} false]
                 [#{open} #{open :checked-conditional-induction} #{nil} #{open} false]
                 [#{open} #{open} #{nil :other} #{open} false]
                 [#{} #{open} #{nil} #{open} false]
                 [#{open} #{} #{nil} #{open} false]
                 [#{open} #{open} #{} #{open} false]
                 [#{open} #{open} #{nil} #{:proved} false]
                 [#{checked} #{:checked-conditional-induction} #{nil} #{} false]]]
      (doseq [[initial induction reasons steps expected] cases]
        (when-not (= expected (boolean (check-status initial induction reasons steps)))
          (throw (ex-info "Incorrect live conditional-induction relation"
                          {:initial initial :induction induction
                           :reasons reasons :steps steps :expected expected})))))
    ;; Even though all-checked? and the allowed-step guard are now in a
    ;; separately type-checked function, source audit must reject same-typed
    ;; status-family substitutions and the illicit change of the theorem rule.
    (let [source (original forms 'consistent-invariant-statuses?)]
      (doseq [[label from to]
              [[:initial-from-induction
                '(= initial #{:checked-conditional-inference})
                '(= induction #{:checked-conditional-induction})]
               [:step-from-initial
                '(= steps #{:checked-conditional-inference})
                '(= initial #{:checked-conditional-inference})]
               [:induction-always-open
                '(if all-checked? :checked-conditional-induction :open)
                :open]
               [:disabled-cardinality
                '(= 1 (count initial)) true]
               [:allowed-step-set-expanded
                '#{:checked-conditional-inference :open}
                '#{:checked-conditional-inference :open :proved}]
               [:accept-contrary-induction
                '(= (first induction)
                    (if all-checked? :checked-conditional-induction :open))
                true]]]
        (let [mutant (replace-unique-source source from to)
              substituted (mapv #(if (= source %) mutant %) forms)]
          (when-not (try (check-status-consistency-source! substituted)
                         false (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Invalid original consistency rule escaped audit"
                            {:case label}))))))
    ;; Same-typed operation keys can be switched or one operation's status
    ;; substituted for another without static error. The ORIGINAL source
    ;; audit must preserve exactly which status belongs to which operation.
    (doseq [[name old new]
            [[:wrong-overall-induction '(first induction) '(first initial)]
             [:disabled-consistency-guard expected-aggregate-guard true]
             [:initial-from-step-set
              '(initial-statuses results)
              '(preservation-step-statuses results)]
             [:preservation-from-initial-set
              '(preservation-step-statuses results)
              '(initial-statuses results)]
             [:induction-from-initial-set
              '(induction-statuses results)
              '(initial-statuses results)]]]
      (let [changed (if (or (= old expected-aggregate-guard)
                            (contains? (set (vals expected-status-acquisition)) old))
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
    (println "Typed operation composition passed: 19 original checked source")
    (println "bodies; 34 report, 17 aggregate, 9 preservation, 5 status-input")
    (println "and 4 nominal closure calls plus 6 malformed agreement inputs")
    (println "rejected by typing; 14 live agreement cases, 6 predicate mutants,")
    (println "6 status-extraction and 11 aggregate/preservation source mutants,")
    (println "4 malformed evidence calls, 9 further ill-typed lookup/row")
    (println "mutations and 10 evidence-source mutants; complete original")
    (println "operation-indexed lookup reduction and live equivalence checks;")
    (println "same-type source swaps, sorted-map equivalence tests, and")
    (println "2 report provenance swaps and runtime forged-closure rejection.")
    :ok))

(ns gesso.assurance.typed-operation-interface-derivation-check
  "Opt-in source-backed type check of the original operation-interface report.

   Checks the complete ORIGINAL pure report constructor and original public
   freshness consumer. The public issuer's original live closure validation,
   source-graph role coverage, and original premise checks are source-audited
   rather than claimed as Typed Clojure proofs. A report stays copyable data:
   no verified constructor makes a certificate or deployed authority."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.proof.operation-interface :as iface]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.typed-application-dependency-closure-check :as dep-check]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.operation-interface)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
(def ^:private iface-status
  {:type :gesso.assurance.proof.operation-interface/conditional-semantic-interfaces
   :version 1
   :scope :closed-declared-abstract-machine-only
   :proof-status :source-bound-conditional-operation-interfaces
   :assumption-discharge :not-performed
   :physical-writer-coverage :not-established
   :implementation-correspondence :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

;; This is the original producer's checked full output contract, reusable at
;; its downstream composition caller. Assumed nominal handle and closure
;; description contracts are checked in the upstream application-dependency
;; gate; the original derivation algorithm is NOT fully statically checked.
;; Original conditional operation contract, checked against the complete
;; original operation-contract-entry body. All subject and semantic source
;; correctness is still enforced by original-operation-interface! at runtime.
(def operation-contract-type
  (list 'typed.clojure/HMap
        :mandatory
        (merge {:operation-id 'typed.clojure/Kw
                :catalog-id 'typed.clojure/Any
                :catalog-fingerprint 'typed.clojure/Any
                :source-fingerprint 'typed.clojure/Str
                :subject-fingerprint 'typed.clojure/Str
                :transition-fingerprint 'typed.clojure/Str
                :state-types 'typed.clojure/Any
                :input-types 'typed.clojure/Any
                :guard 'typed.clojure/Any
                :reads '(typed.clojure/Set typed.clojure/Kw)
                :writes '(typed.clojure/Set typed.clojure/Kw)
                :frame '(typed.clojure/Set typed.clojure/Kw)
                :assignments 'typed.clojure/Any
                :conditional-invariant-results
                '(typed.clojure/Map typed.clojure/Kw typed.clojure/Any)
                :original-open-obligation-ids
                '(typed.clojure/Set (typed.clojure/Vec typed.clojure/Any))
                :open-meta-obligations dep-check/obligation-id-set}
               (into {} (map (fn [[k v]] [k (list 'typed.clojure/Val v)])
                             {:contract-scope :declared-abstract-guarded-transition-only
                              :guarantee-status :conditional-inference-or-open
                              :physical-writer-coverage :not-established
                              :implementation-correspondence :not-established
                              :certificate :not-issued
                              :admission :not-admitted
                              :authority :none})))
        :complete? true))
(def ^:private semantic-type
  '(typed.clojure/HMap
    :mandatory {:state-types typed.clojure/Any
                :input-types typed.clojure/Any
                :guard typed.clojure/Any
                :reads (typed.clojure/Set typed.clojure/Kw)
                :writes (typed.clojure/Set typed.clojure/Kw)
                :frame-fields (typed.clojure/Set typed.clojure/Kw)
                :assignments typed.clojure/Any}
    :complete? false))
(def ^:private transition-description-type
  '(typed.clojure/HMap
    :mandatory {:fingerprint typed.clojure/Str
                :subject-fingerprint typed.clojure/Str}
    :complete? false))

(def derived-type
  (list 'typed.clojure/HMap
        :mandatory
        (merge {:fingerprint 'typed.clojure/Str
                :catalog-id 'typed.clojure/Any
                :catalog-fingerprint 'typed.clojure/Any
                :induction-fingerprint 'typed.clojure/Any
                :operation-ids '(typed.clojure/Set typed.clojure/Kw)
                :invariant-ids '(typed.clojure/Set typed.clojure/Kw)
                :interfaces (list 'typed.clojure/Map 'typed.clojure/Kw operation-contract-type)
                :original-open-obligation-ids-by-operation dep-check/original-obligations-type
                :open-meta-obligations dep-check/obligation-id-set
                :open-assumptions dep-check/open-assumptions-type}
               (into {} (map (fn [[k v]] [k (list 'typed.clojure/Val v)])
                             iface-status)))
        :complete? true))
(def ^:private contract-status
  {:contract-scope :declared-abstract-guarded-transition-only
   :guarantee-status :conditional-inference-or-open
   :physical-writer-coverage :not-established
   :implementation-correspondence :not-established
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(def ^:private subject-type
  '(typed.clojure/HMap
    :mandatory {:fingerprint typed.clojure/Str}
    :complete? false))

(defn- source-forms []
  (let [f (or (io/resource "gesso/assurance/proof/operation_interface.clj")
              (throw (ex-info "Original interface source is missing" {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader f))]
      (loop [acc []]
        (let [form (read {:eof ::done} reader)]
          (if (= ::done form) acc (recur (conj acc form))))))))

(defn- original [forms function]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= function (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Original interface function changed"
                      {:name function :count (count matches)})))
    (first matches)))

(defn- fn-ann! [name sig]
  (eval (list 'typed.clojure/ann name (list 'typed.clojure/IFn sig))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype app-handle '[]))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.operation-interface/interface-type
           (typed.clojure/Val
            :gesso.assurance.proof.operation-interface/conditional-semantic-interfaces)))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.operation-interface/interface-version
           (typed.clojure/Val 1)))
  (doseq [[name signature]
          [['gesso.assurance.proof.operation-interface/operation-contract-entry
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Kw
             'typed.clojure/Any transition-description-type semantic-type
             '(typed.clojure/Map typed.clojure/Kw typed.clojure/Any)
             '(typed.clojure/Set (typed.clojure/Vec typed.clojure/Any))
             dep-check/obligation-id-set '-> operation-contract-type]]
           ['gesso.assurance.proof.operation-interface/raw-fingerprint
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Str]]
           ['gesso.assurance.proof.operation-interface/interface-report-entry
            [dep-check/description-type
             (list 'typed.clojure/Map 'typed.clojure/Kw operation-contract-type)
             subject-type '-> derived-type]]
           ['gesso.assurance.proof.operation-interface/derive-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> derived-type]]
           ['gesso.assurance.proof.operation-interface/require-current-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Any
             app-handle '-> derived-type]]
           ['gesso.assurance.proof.operation-interface/reject!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Nothing]]
           ['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> app-handle]]]]
    (fn-ann! name signature)))

(defn- check! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Interface original source failed static typing"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)
        codes (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq codes)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} codes))
      (throw (ex-info "Wrong interface contract was not statically rejected"
                      {:label label :codes codes
                       :exception (some-> ex ex-message)} ex)))
    true))

(def ^:private expected-source-fields
  {:catalog-id '(:catalog-id summary)
   :catalog-fingerprint '(:catalog-fingerprint summary)
   :induction-fingerprint '(:induction-fingerprint summary)
   :operation-ids '(:operation-ids summary)
   :invariant-ids '(:invariant-ids summary)
   :interfaces 'interfaces
   :original-open-obligation-ids-by-operation
   '(:original-open-obligation-ids-by-operation summary)
   :open-meta-obligations '(:open-meta-obligations summary)
   :open-assumptions '(:open-assumptions summary)
   :fingerprint '(:fingerprint identity)})

(defn- descriptor-result [source]
  (let [result (last source)]
    (when-not (map? result)
      (throw (ex-info "Original interface report descriptor moved" {})))
    result))

(defn- audit-entry! [source]
  (let [result (descriptor-result source)]
    (when-not (and (= (set (keys result))
                      (into (set (keys iface-status))
                            (keys expected-source-fields)))
                   (every? (fn [[k expected]] (= expected (get result k ::absent)))
                           expected-source-fields)
                   (= (dissoc iface-status :type :version)
                      (select-keys result
                                   (remove #{:type :version} (keys iface-status)))))
      (throw (ex-info "Interface descriptor source provenance changed"
                      {:result result})))
    true))

(defn- mutate-result [source change]
  (let [result (descriptor-result source)
        modified (change result)]
    (when (= modified result)
      (throw (ex-info "No-op interface mutation" {})))
    (apply list (concat (butlast source) [modified]))))

(defn- audit-public! [forms]
  ;; This validates the live source-to-entry connection, not the truth of
  ;; any catalog identity or open premise in the source-derived graph.
  (let [issuer (original forms 'derive-operation-interfaces!)
        freshness (original forms 'require-current-operation-interfaces!)
        outer-forms (drop 1 (drop-while (complement vector?) (drop 2 issuer)))
        first-step (first outer-forms)
        body (last issuer)
        bindings (when (and (seq? body) (= 'let (first body))) (second body))
        by-var (when (vector? bindings)
                 (into {} (map vec (partition 2 bindings))))
        proof-guard (nth body 2)
        inner (last body)
        inner-bindings (when (and (seq? inner) (= 'let (first inner)))
                         (second inner))
        inner-map (when (vector? inner-bindings)
                    (into {} (map vec (partition 2 inner-bindings))))
        last-issuer (when (seq? inner) (last inner))
        fresh-body (last freshness)]
    (when-not (and (= '(app/require-current-application-dependencies!
                        closure catalog current-graph) first-step)
                   (= '(app/describe-application-dependencies closure)
                      (get by-var 'summary))
                   (= '(:operation-ids summary) (get by-var 'operation-ids))
                   (= '(:invariant-ids summary) (get by-var 'invariant-ids))
                   (= '(:original-open-obligation-ids-by-operation summary)
                      (get by-var 'originals))
                   (= '(:open-meta-obligations summary)
                      (get by-var 'meta-obligations))
                   (= '(when-not (and (= operation-ids (set (keys originals)))
                                     (set? meta-obligations)
                                     (contains? meta-obligations
                                                :implementation-correspondence)
                                     (contains? meta-obligations
                                                :physical-write-path-coverage)
                                     (every? (fn [id]
                                               (= (get originals id)
                                                  (get-in evidence
                                                          [id :original-open-obligation-ids])))
                                             operation-ids))
                        (reject! :original-premise-coverage-mismatch {}))
                      proof-guard)
                   (= '(subject/describe-subject
                        {:kind :rule :id identity-id
                         :interface {:version interface-version
                                     :catalog-id (:catalog-id summary)
                                     :catalog-fingerprint fingerprint
                                     :induction-fingerprint
                                     (:induction-fingerprint summary)
                                     :interfaces interfaces
                                     :open-meta-obligations meta-obligations
                                     :open-assumptions (:open-assumptions summary)}})
                      (get inner-map 'identity))
                   (= '(interface-report-entry summary interfaces identity)
                      last-issuer)
                   (= '(derive-operation-interfaces!
                        current-catalog current-graph closure)
                      (second (second fresh-body)))
                   (= '(when-not (and (map? prior) (not (record? prior))
                                     (nil? (meta prior)) (= prior now))
                         (reject! :stale-or-forged-operation-interfaces
                                  {:expected (:fingerprint now)
                                   :actual (:fingerprint prior)}))
                      (nth fresh-body 2))
                   (= 'now (last fresh-body)))
      (throw (ex-info "Operation-interface source/closure chain changed"
                      {:step first-step :binding (get by-var 'summary)
                       :guard proof-guard :final last-issuer})))
    true))

(defn- audit-rejects-wrong-source! [forms]
  (let [issuer (original forms 'derive-operation-interfaces!)
        cases
        [[:swapped-source-graph-arguments
          '(app/require-current-application-dependencies!
            closure catalog current-graph)
          '(app/require-current-application-dependencies!
            closure current-graph catalog)]
         [:wrong-report-summary
          '(interface-report-entry summary interfaces identity)
          '(interface-report-entry {} interfaces identity)]
         [:misbound-original-premise-source
          '(:original-open-obligation-ids-by-operation summary)
          '(:open-assumptions summary)]]]
    (doseq [[label original-expr replacement] cases]
      (let [seen (atom 0)
            changed (walk/postwalk
                     (fn [form]
                       (if (= original-expr form)
                         (do (swap! seen inc) replacement)
                         form)) issuer)]
        (when-not (= 1 @seen)
          (throw (ex-info "Original source-audit target changed"
                          {:label label :matches @seen})))
        (let [replacement-forms (mapv #(if (= issuer %) changed %) forms)]
          (when-not (try (audit-public! replacement-forms)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Incorrect original source escaped issuer audit"
                            {:label label}))))))
    true))

(defn- contract-result [source]
  (let [result (last source)]
    (when-not (map? result)
      (throw (ex-info "Original operation contract constructor moved" {})))
    result))

(def ^:private contract-origins
  {:operation-id 'operation-id
   :catalog-id 'catalog-id
   :catalog-fingerprint 'catalog-fingerprint
   :source-fingerprint '(raw-fingerprint operation-id source)
   :subject-fingerprint '(:subject-fingerprint description)
   :transition-fingerprint '(:fingerprint description)
   :state-types '(:state-types semantics)
   :input-types '(:input-types semantics)
   :guard '(:guard semantics)
   :reads '(:reads semantics)
   :writes '(:writes semantics)
   :frame '(:frame-fields semantics)
   :assignments '(:assignments semantics)
   :conditional-invariant-results 'conditional
   :original-open-obligation-ids 'obligations
   :open-meta-obligations 'open-meta})

(defn- assert-contract-origins! [source]
  (let [result (contract-result source)]
    (when-not (and (= (set (keys result))
                      (into (set (keys contract-status))
                            (keys contract-origins)))
                   (every? (fn [[k v]] (= v (get result k ::missing)))
                           contract-origins)
                   (every? (fn [[k v]] (= v (get result k ::missing)))
                           contract-status))
      (throw (ex-info "Original operation contract source mapping changed"
                      {:result result})))
    true))

(defn- mutate-contract [source modify]
  (let [original-map (contract-result source)
        changed (modify original-map)]
    (when (= original-map changed)
      (throw (ex-info "Operation contract mutation is unchanged" {})))
    (apply list (concat (butlast source) [changed]))))

(defn- audit-operation-issuer! [forms]
  ;; The original validator must assemble the checked contract only after
  ;; reconstruction, independent original evidence, and correspondence checks.
  (let [issuer (original forms 'original-operation-interface!)
        body (last issuer)
        pairs (partition 2 (second body))
        bound (into {} (map vec pairs))
        guard (nth body 2)
        handoff (last body)
        expected-handoff
        '(operation-contract-entry
          catalog-id catalog-fingerprint operation-id source description semantics
          conditional obligations open-meta)
        expected-guard
        '(when-not (and (= operation-id (:operation-id original-op)
                                     (:operation-id evidence)
                                     (get-in source [:subject :id]))
                        (= (:reads built) (:reads semantics) (:reads source))
                        (= (:writes built) (:writes semantics) (:writes source))
                        (= (:frame built) (:frame-fields semantics) (:frame source))
                        (set? obligations)
                        (= catalog-fingerprint
                           (:source-catalog-fingerprint evidence))
                        (= invariant-ids (set (keys conditional))))
           (reject! :operation-evidence-correspondence-mismatch
                    {:operation-id operation-id}))]
    (when-not (and (= '(construction/build-guarded-replacement! source)
                      (get bound 'built))
                   (= '(abstract/describe-transition! (:transition built))
                      (get bound 'description))
                   (= '(:semantics description) (get bound 'semantics))
                   (= '(get operation-evidence operation-id) (get bound 'evidence))
                   (= '(:original-open-obligation-ids evidence)
                      (get bound 'obligations))
                   (= '(:construction original-op) (get bound 'source))
                   (= expected-guard guard)
                   (= expected-handoff handoff))
      (throw (ex-info "Original operation contract lost validated source handoff"
                      {:guard guard :handoff handoff :bound bound})))
    ;; Derivation must still use the original validator for EACH operation.
    (let [public (original forms 'derive-operation-interfaces!)
          hits (atom 0)]
      (walk/postwalk
       (fn [x]
         (when (= x '(original-operation-interface!
                       (:catalog-id summary) fingerprint invariant-ids
                       (get original-ops id) evidence invariants
                       meta-obligations id))
           (swap! hits inc))
         x) public)
      (when-not (= 1 @hits)
        (throw (ex-info "Original per-operation validator missing from issuer"
                        {:matches @hits}))))
    true))

(defn- audit-operation-issuer-rejections! [forms]
  (let [issuer (original forms 'original-operation-interface!)
        cases [[:bypass-evidence-guard
                '(= catalog-fingerprint
                    (:source-catalog-fingerprint evidence))
                '(= catalog-fingerprint catalog-fingerprint)]
               [:misbind-original-obligations
                '(:original-open-obligation-ids evidence)
                '(:open-meta-obligations evidence)]
               [:swap-checked-handoff-arguments
                '(operation-contract-entry
                  catalog-id catalog-fingerprint operation-id source description semantics
                  conditional obligations open-meta)
                '(operation-contract-entry
                  catalog-id catalog-fingerprint operation-id source description semantics
                  obligations conditional open-meta)]]]
    (doseq [[label original-expr replacement] cases]
      (let [hits (atom 0)
            changed (walk/postwalk
                     (fn [x]
                       (if (= original-expr x)
                         (do (swap! hits inc) replacement)
                         x)) issuer)]
        (when-not (= 1 @hits)
          (throw (ex-info "Original issuer mutation target moved"
                          {:label label :found @hits})))
        (when-not (try (audit-operation-issuer!
                        (mapv #(if (= % issuer) changed %) forms))
                       false
                       (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Invalid original operation guard escaped audit"
                          {:label label})))))
    true))

(defn- check-operation-contract! [checker forms]
  (let [source (original forms 'operation-contract-entry)]
    (check! checker :original-operation-contract-entry source)
    (assert-contract-origins! source)
    (audit-operation-issuer! forms)
    (audit-operation-issuer-rejections! forms)
    (doseq [[label replacement]
            [[:wrong-operation-id #(assoc % :operation-id "request/claim")]
             [:missing-conditional-results #(dissoc % :conditional-invariant-results)]
             [:wrong-conditional-results #(assoc % :conditional-invariant-results [])]
             [:wrong-reads #(assoc % :reads {})]
             [:wrong-writes #(assoc % :writes [])]
             [:wrong-frame #(assoc % :frame nil)]
             [:invalid-original-obligations
              #(assoc % :original-open-obligation-ids #{:not-vector})]
             [:invalid-open-meta #(assoc % :open-meta-obligations {})]
             [:wrong-subject-fingerprint #(assoc % :subject-fingerprint false)]
             [:wrong-transition-fingerprint #(assoc % :transition-fingerprint 1)]
             [:missing-guard #(dissoc % :guard)]
             [:invented-certification #(assoc % :transaction-authority :granted)]
             [:invalid-catalog-scope #(assoc % :contract-scope :installed-operation)]
             [:false-guarantee #(assoc % :guarantee-status :proved)]
             [:false-physical-coverage #(assoc % :physical-writer-coverage :complete)]
             [:false-correspondence #(assoc % :implementation-correspondence :proved)]
             [:false-certificate #(assoc % :certificate :issued)]
             [:false-admission #(assoc % :admission :admitted)]
             [:false-authority #(assoc % :authority :granted)]]]
      (reject! checker [:operation-contract label]
               (mutate-contract source replacement)))
    ;; A type-compatible but WRONG original source (eg, interpreting writes
    ;; as reads) remains detectable by the source-provenance audit alone.
    (let [wrong (mutate-contract source
                 #(assoc % :reads '(:writes semantics)))]
      (check! checker :type-compatible-wrong-operation-reads wrong)
      (when-not (try (assert-contract-origins! wrong) false
                     (catch clojure.lang.ExceptionInfo _ true))
        (throw (ex-info "Wrong original reads source escaped audit" {}))))
    true))

(defn- checks! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (let [entry (original forms 'interface-report-entry)]
      (check-operation-contract! checker forms)
      (check! checker :original-interface-entry entry)
      (check! checker :original-freshness-consumer
              (original forms 'require-current-operation-interfaces!))
      (audit-entry! entry)
      (audit-public! forms)
      (audit-rejects-wrong-source! forms)
      (doseq [field (keys iface-status)]
        (reject! checker [:false-status field]
                 (mutate-result entry #(assoc % field :proved))))
      (doseq [[name change]
              [[:missing-interfaces #(dissoc % :interfaces)]
               [:raw-operation-ids #(assoc % :operation-ids {})]
               [:raw-invariant-ids #(assoc % :invariant-ids [])]
               [:raw-meta #(assoc % :open-meta-obligations {})]
               [:raw-assumptions #(assoc % :open-assumptions #{:wrong})]
               [:raw-original #(assoc % :original-open-obligation-ids-by-operation #{})]
               [:premise-map-as-meta
                #(assoc % :open-meta-obligations '(:open-assumptions summary))]
               [:meta-as-assumption-map
                #(assoc % :open-assumptions '(:open-meta-obligations summary))]
               [:original-as-assumptions
                #(assoc % :original-open-obligation-ids-by-operation
                        '(:open-assumptions summary))]
               [:wrong-fingerprint #(assoc % :fingerprint 999)]]]
        (reject! checker [:bad-descriptor name] (mutate-result entry change)))
      ;; Fingerprint identities are still Any; their origin requires source
      ;; correspondence rather than the structural type system alone.
      (let [swapped (mutate-result entry
                    #(assoc % :catalog-fingerprint
                            '(:induction-fingerprint summary)))]
        (check! checker :type-compatible-fingerprint-swap swapped)
        (when-not (try (audit-entry! swapped) false
                       (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Wrong fingerprint provenance escaped source audit" {}))))
      (check! checker :valid-nominal-derivation
              '(derive-operation-interfaces!
                {} {} (app/check-application-dependencies! {} {})))
      (doseq [[label invalid]
              [[:raw-closure '(derive-operation-interfaces! {} {} {})]
               [:nil-closure '(derive-operation-interfaces! {} {} nil)]
               [:raw-freshness-closure
                '(require-current-operation-interfaces! {} {} {} {})]
               [:nil-freshness-closure
                '(require-current-operation-interfaces! {} {} {} nil)]]]
        (reject! checker label invalid))
      true)))

(defn- runtime-forgery! []
  (let [handle (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        reason (try (iface/derive-operation-interfaces! {} {} handle)
                    nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))]
    (when-not (= :unrecognized-closure-handle reason)
      (throw (ex-info "Unregistered application closure entered interface derivation"
                      {:reason reason})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (source-forms)]
    (checks! checker forms)
    (runtime-forgery!)
    (println "Typed operation interface derivation passed: original report")
    (println "constructor, per-operation contract and freshness consumer; 43")
    (println "negative type cases and a read/write provenance swap caught")
    (println "by source audit, six issuer source mutations, and forgery rejection.")
    (println "The complete derivation algorithm and live registry remain")
    (println "runtime assurance obligations, not statically established proof.")
    :ok))

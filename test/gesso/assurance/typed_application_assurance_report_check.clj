(ns gesso.assurance.typed-application-assurance-report-check
  "Opt-in source-backed Typed Clojure checks for final application reports.

   The original public functions must accept a nominal, registered-category
   ApplicationDependencyHandle rather than a raw or copied closure report.
   Their fresh upstream acquisition and recheck paths are source-audited.
   The negative report flags are checked against the ORIGINAL output map;
   this does NOT prove upstream source agreement, writer completeness,
   kernel soundness, authorization, or the trustworthiness of a copied map.

   Original upstream functions are independently checked and tested.
   Declared application closure, composition, provenance, and observed
   writer coverage now reuse the producer's independently checked exact HMap.
   All cross-source semantic correspondence is still runtime-validated.
   None of these copyable reports is a proof or authority token. The
   application closure handle is nominal: live registry authentication
   and source freshness remain runtime responsibilities."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.proof.application-assurance-report :as report]
            [gesso.assurance.proof.application-dependency :as dependency]
            [gesso.assurance.proof.application-writer-coverage :as writer]
            [gesso.assurance.proof.operation-interface-composition :as composition]
            [gesso.assurance.proof.operation-interface-provenance :as provenance]
            [gesso.assurance.typed-application-dependency-closure-check :as dep-check]
            [gesso.assurance.typed-operation-interface-composition-check :as comp-check]
            [gesso.assurance.typed-operation-interface-provenance-check :as prov-check]
            [gesso.assurance.typed-application-writer-coverage-check :as writer-check]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private report-ns 'gesso.assurance.proof.application-assurance-report)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
(def ^:private declared-evidence-type
  ;; The original public closure-description function is independently checked
  ;; against this EXACT complete HMap. It is not proof or live authority.
  dep-check/description-type)
(def ^:private composed-evidence-type
  ;; The SAME exact structural type checked against the original composition
  ;; constructor in the neighboring source-bound Typed Clojure gate.
  comp-check/composed-type)
(def ^:private provenance-evidence-type
  ;; Identical to the original manifest-entry return type in the standalone
  ;; provenance source checker. A copyable manifest is not proof authority.
  prov-check/manifest-type)
(def ^:private writer-evidence-type
  ;; Reuse the complete, source-checked writer descriptor; it remains an
  ;; observed-host snapshot report, not evidence of writer exhaustiveness.
  writer-check/report-type)

(def ^:private non-authority-fields
  {:checking :source-derived-composition-provenance-and-observed-writer-join
   :scope :declared-abstract-machine-and-supplied-host-observation
   :assumption-discharge :not-performed
   :physical-writer-coverage :not-established
   :abstract-to-physical-correspondence :not-established
   :browser-correspondence :not-established
   :universal-proof-kernel-soundness :not-established
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})
(def ^:private non-authority-type
  (list 'typed.clojure/HMap
        :mandatory (into {} (map (fn [[k v]] [k (list 'typed.clojure/Val v)])
                                 non-authority-fields))
        :complete? true))

(defn- definitions []
  (let [path (or (io/resource "gesso/assurance/proof/application_assurance_report.clj")
                 (throw (ex-info "Original application assurance source missing" {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader path))]
      (loop [forms []]
        (let [form (read {:eof ::end} reader)]
          (if (= ::end form) forms (recur (conj forms form))))))))

(defn- original [forms function]
  (let [matches (filter #(and (seq? %)
                              (#{'defn 'defn-} (first %))
                              (= function (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Original application assurance function changed"
                      {:function function :count (count matches)})))
    (first matches)))

(defn- ann! [sym signature]
  (eval (list 'typed.clojure/ann sym (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype app-handle '[]))
  ;; All four derived inputs share separately checked exact structural HMaps.
  ;; The source registry remains opaque and these types grant no authority.
  (doseq [[name signature]
          [['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> app-handle]]
           ['gesso.assurance.proof.application-dependency/describe-application-dependencies
            [app-handle '-> declared-evidence-type]]
           ['gesso.assurance.proof.operation-interface-composition/compose-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> composed-evidence-type]]
           ['gesso.assurance.proof.operation-interface-provenance/describe-composition-provenance!
            [composed-evidence-type 'typed.clojure/Any 'typed.clojure/Any
             app-handle '-> provenance-evidence-type]]
           ['gesso.assurance.proof.application-writer-coverage/describe-application-writer-coverage!
            ['typed.clojure/Any 'typed.clojure/Any app-handle
             'typed.clojure/Any '-> writer-evidence-type]]
           ['gesso.assurance.proof.application-assurance-report/checked-summary!
            [declared-evidence-type composed-evidence-type provenance-evidence-type
             writer-evidence-type '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-assurance-report/ordinary-map?
            ['typed.clojure/Any '-> 'typed.clojure/Bool]]
           ['gesso.assurance.proof.application-assurance-report/reject!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Nothing]]
           ['gesso.assurance.proof.application-assurance-report/describe-application-assurance!
            ['typed.clojure/Any 'typed.clojure/Any app-handle
             'typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-assurance-report/require-current-application-assurance!
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Any
             app-handle 'typed.clojure/Any '-> 'typed.clojure/Any]]]]
    (ann! name signature)))

(defn- check! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Application assurance source did not type-check"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/app-type-error
                             :typed.clojure/type-mismatch-error} kinds))
      (throw (ex-info "Invalid application assurance source escaped typing"
                      {:label label :actual kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(def ^:private acquisition-source
  {'declared '(dependency/describe-application-dependencies closure)
   'composed '(composition/compose-operation-interfaces!
               catalog current-graph closure)
   'manifest '(provenance/describe-composition-provenance!
               composed catalog current-graph closure)
   'observed '(writer/describe-application-writer-coverage!
               catalog current-graph closure writer-source)})

(defn- source-handoff! [forms]
  ;; A nominal type does not prove these four calls use the same original
  ;; catalog and graph or that the final map was computed from them. Tie the
  ;; checked function to its original production body and keep that source
  ;; provenance test separate from the type checks.
  (let [defn-form (original forms 'describe-application-assurance!)
        body (last defn-form)
        bindings (when (and (seq? body) (= 'let (first body))) (second body))
        actual (when (vector? bindings)
                 (into {} (map vec (partition 2 bindings))))]
    (when-not (and (= acquisition-source actual)
                   (= '(checked-summary! declared composed manifest observed)
                      (last body)))
      (throw (ex-info "Application assurance source provenance changed"
                      {:actual actual :result (last body)})))
    true))

(defn- require-current-handoff! [forms]
  (let [defn-form (original forms 'require-current-application-assurance!)
        body (last defn-form)
        bindings (when (and (seq? body) (= 'let (first body))) (second body))
        expected '(describe-application-assurance!
                   catalog current-graph closure writer-source)]
    (when-not (and (= '[now] (vec (take-nth 2 bindings)))
                   (= expected (second bindings))
                   (= 'now (last body)))
      (throw (ex-info "Application report freshness consumer changed"
                      {:bindings bindings :final (last body)})))
    ;; Both ordinary-map screening and fresh-result equality are required.
    ;; Just finding a reject! call somewhere would allow a vacuous guard.
    (when-not (= '(when-not (and (ordinary-map? prior) (= prior now))
                    (reject! :stale-or-substituted-application-assurance-report
                             {:expected (:fingerprint now)
                              :actual (:fingerprint prior)}))
                 (nth body 2))
      (throw (ex-info "Application report lost its exact stale-copy guard"
                      {:actual (nth body 2)})))
    true))

(defn- final-map [forms]
  (let [body (last (original forms 'checked-summary!))
        innermost (last body)
        result (when (and (seq? innermost) (= 'let (first innermost)))
                 (last innermost))]
    (when-not (and (map? result)
                   (= non-authority-fields
                      (select-keys result (keys non-authority-fields)))
                   (= (set (keys non-authority-fields))
                      (set (filter #(contains? result %) (keys non-authority-fields)))))
      (throw (ex-info "Original application report's negative guarantees changed"
                      {:reported-flags (select-keys result
                                                   (keys non-authority-fields))})))
    result))

(defn- check-final-flags! [checker forms]
  (let [report-map (final-map forms)
        flags (select-keys report-map (keys non-authority-fields))]
    ;; The fragment is extracted from the ORIGINAL final output map. This
    ;; checks exactly its constant negative status values, not the entire
    ;; checked-summary! body or relationships among its dynamic fields.
    (check! checker :original-final-non-authority-flags
            (list 'typed.clojure/ann-form flags non-authority-type))
    (doseq [[field _] non-authority-fields]
      (let [mutant (assoc flags field :proved)]
        (reject! checker [:wrong-final-report-flag field]
                 (list 'typed.clojure/ann-form mutant non-authority-type))))
    true))

(defn- mutate-original-call [source original-call alternative]
  (let [count* (atom 0)
        changed (walk/postwalk
                 (fn [x]
                   (if (= x original-call)
                     (do (swap! count* inc) alternative)
                     x))
                 source)]
    (when-not (= 1 @count*)
      (throw (ex-info "Original assurance call changed or duplicated"
                      {:original original-call :found @count*})))
    changed))

(defn- check-declared-closure-evidence! [checker forms]
  ;; Mutate the ORIGINAL source-bound public builder at its actual join call.
  ;; The exact descriptor type is checked at the original producer, and now
  ;; the downstream consumer refuses raw or inflated closure descriptions.
  (let [source (original forms 'describe-application-assurance!)
        genuine '(checked-summary! declared composed manifest observed)]
    (doseq [[label replacement]
            [[:raw-description '{}]
             [:nil-description nil]
             [:vector-description '[]]
             [:missing-operation-ids '(dissoc declared :operation-ids)]
             [:invalid-operation-ids '(assoc declared :operation-ids [])]
             [:invalid-invariant-ids '(assoc declared :invariant-ids {})]
             [:wrong-node-count '(assoc declared :node-count "five")]
             [:wrong-edge-count '(assoc declared :edge-count false)]
             [:false-proof-status '(assoc declared :proof-status :proved)]
             [:false-certificate '(assoc declared :certificate :issued)]
             [:false-authority '(assoc declared :authority :granted)]
             [:false-writer-coverage
              '(assoc declared :installed-writer-coverage :complete)]
             [:wrong-open-meta-kind
              '(assoc declared :open-meta-obligations {})]
             [:wrong-classified-assumptions
              '(assoc declared :open-assumptions #{:unclassified})]
             [:wrong-original-operation-obligations
              '(assoc declared :original-open-obligation-ids-by-operation #{})]
             [:premise-source-family-swap
              '(assoc declared :open-assumptions
                      (:open-meta-obligations declared))]
             [:swapped-composition 'composed]
             [:swapped-writer 'observed]]]
      (reject! checker [:invalid-declared-closure label]
               (mutate-original-call source genuine
                 (list 'checked-summary! replacement 'composed
                       'manifest 'observed))))
    ;; The catalog fingerprint and induction fingerprint are BOTH Any at
    ;; this contract boundary; swapping them remains type-compatible and
    ;; must instead be rejected by the original-source acquisition audit.
    (let [wrong-source (mutate-original-call
                        source genuine
                        '(checked-summary!
                          (assoc declared :catalog-fingerprint
                                 (:induction-fingerprint declared))
                          composed manifest observed))]
      (check! checker :type-compatible-wrong-declared-fingerprint wrong-source)
      (when-not (try (source-handoff!
                      (mapv #(if (= source %) wrong-source %) forms))
                     false
                     (catch clojure.lang.ExceptionInfo _ true))
        (throw (ex-info "Wrong closure-source fingerprint escaped audit" {}))))
    (check! checker :original-typed-declared-closure-handoff source)
    true))

(defn- check-composition-evidence! [checker forms]
  ;; Mutate the ORIGINAL production builder at its real summary invocation.
  ;; This tests static propagation from compose-operation-interfaces! through
  ;; the local `composed` binding to checked-summary!, not a copied function.
  (let [source (original forms 'describe-application-assurance!)
        genuine '(checked-summary! declared composed manifest observed)]
    (doseq [[label replacement]
            [[:raw-composition '{}]
             [:nil-composition nil]
             [:vector-composition '[]]
             [:partial-composition {:type :conditional}]
             [:wrong-proof-status '(assoc composed :proof-status :proved)]
             [:fabricated-authority '(assoc composed :authority :granted)]
             [:missing-operation-ids '(dissoc composed :operation-ids)]
             [:wrong-operation-ids '(assoc composed :operation-ids [])]
             [:wrong-invariant-ids '(assoc composed :invariant-ids {})]
             [:wrong-fingerprint '(assoc composed :fingerprint 42)]]]
      (reject! checker [:invalid-composition-input label]
               (mutate-original-call source genuine
                 (list 'checked-summary! 'declared replacement
                       'manifest 'observed))))
    ;; Distinguish what the HMap actually verifies: these upstream report
    ;; values are still Any, so swapping them is type-compatible and covered
    ;; ONLY by the source audit already in this checker.
    (check! checker :composition-evidence-source-bound-builder source)
    true))

(defn- check-provenance-evidence! [checker forms]
  ;; The ORIGINAL source-bound assurance builder already acquires a fresh
  ;; manifest from the original provenance function. Mutate only that actual
  ;; handoff to show the shared HMap catches malformed data here as well.
  (let [source (original forms 'describe-application-assurance!)
        genuine '(checked-summary! declared composed manifest observed)]
    (doseq [[label substitute]
            [[:raw-manifest '{}]
             [:nil-manifest nil]
             [:wrong-proof-status '(assoc manifest :proof-status :proved)]
             [:fake-authority '(assoc manifest :authority :granted)]
             [:wrong-manifest-type '(assoc manifest :type :inferred-proof)]
             [:missing-lineage '(dissoc manifest :lineage)]
             [:missing-lineage-graph '(assoc manifest :lineage {})]
             [:missing-lineage-composition-fingerprint
              '(assoc manifest :lineage {:operations {}})]
             [:wrong-fingerprint '(assoc manifest :fingerprint 0)]
             [:fake-certificate '(assoc manifest :certificate :issued)]]]
      (reject! checker [:invalid-provenance-input label]
               (mutate-original-call source genuine
                 (list 'checked-summary! 'declared 'composed
                       substitute 'observed))))
    (check! checker :source-bound-provenance-handoff source)
    true))

(defn- check-writer-evidence! [checker forms]
  ;; The ORIGINAL application report builder acquires a fresh writer snapshot
  ;; and passes it to the actual summary join. Static types now reject
  ;; impossible or affirmative coverage claims in that original call.
  (let [source (original forms 'describe-application-assurance!)
        genuine '(checked-summary! declared composed manifest observed)]
    (doseq [[label replacement]
            [[:raw-writer-report '{}]
             [:nil-writer-report nil]
             [:wrong-proof-status '(assoc observed :proof-status :proved)]
             [:fake-physical-coverage
              '(assoc observed :physical-writer-coverage :established)]
             [:fake-authority '(assoc observed :authority :granted)]
             [:fake-certificate '(assoc observed :certificate :issued)]
             [:missing-declared-ids '(dissoc observed :declared-operation-ids)]
             [:invalid-independent-ids
              '(assoc observed :independent-operation-ids [])]
             [:invalid-join-flag
              '(assoc observed :exact-observed-operation-id-join? :true)]
             [:missing-open-obligations '(dissoc observed :open-obligations)]
             [:wrong-rows '(assoc observed :operation-rows {})]
             [:wrong-status '(assoc observed :status :all-physical-writers-covered)]]]
      (reject! checker [:invalid-writer-coverage-input label]
               (mutate-original-call source genuine
                 (list 'checked-summary! 'declared 'composed 'manifest
                       replacement))))
    (check! checker :original-source-bound-writer-coverage-handoff source)
    true))

(defn- checks! [checker forms]
  (binding [*ns* (the-ns report-ns)]
    (check! checker :original-application-assurance-builder
            (original forms 'describe-application-assurance!))
    (check! checker :original-application-assurance-freshness
            (original forms 'require-current-application-assurance!))
    (source-handoff! forms)
    (require-current-handoff! forms)
    (check-final-flags! checker forms)
    (check-declared-closure-evidence! checker forms)
    (check-composition-evidence! checker forms)
    (check-provenance-evidence! checker forms)
    (check-writer-evidence! checker forms)
    ;; Catalog and graph remain Any: their reversal stays type-compatible
    ;; and is caught by source audit. Provenance and writer evidence NOW have
    ;; different static types, so their original source-map swap must FAIL.
    (let [source (original forms 'describe-application-assurance!)
          wrong-order (mutate-original-call source
                        '(composition/compose-operation-interfaces!
                          catalog current-graph closure)
                        '(composition/compose-operation-interfaces!
                          current-graph catalog closure))
          wrong-reports (mutate-original-call source
                          '(checked-summary! declared composed manifest observed)
                          '(checked-summary! declared composed observed manifest))]
      (check! checker :type-compatible-wrong-catalog-graph wrong-order)
      (reject! checker :statically-wrong-provenance-writer-order wrong-reports)
      (doseq [modified [wrong-order wrong-reports]]
        (when-not (try (source-handoff! (mapv #(if (= source %) modified %) forms))
                       false (catch clojure.lang.ExceptionInfo _ true))
          (throw (ex-info "Wrong-source report mutation escaped audit" {})))))
    (let [source (original forms 'describe-application-assurance!)]
      ;; The four actual producer expressions must all receive a nominal
      ;; application closure, not an ordinary map or a copyable report.
      (doseq [[name original-call] acquisition-source]
        (reject! checker [:raw-closure-in name]
                 (mutate-original-call source original-call
                   (walk/postwalk-replace {'closure {}} original-call)))
        (reject! checker [:nil-closure-in name]
                 (mutate-original-call source original-call
                   (walk/postwalk-replace {'closure nil} original-call)))))
    (reject! checker :raw-closure-public-builder
             '(describe-application-assurance! {} {} {} {}))
    (reject! checker :nil-closure-public-builder
             '(describe-application-assurance! {} {} nil {}))
    (reject! checker :raw-closure-freshness-consumer
             '(require-current-application-assurance! {} {} {} {} {}))
    (reject! checker :nil-closure-freshness-consumer
             '(require-current-application-assurance! {} {} {} nil {}))
    ;; Verifies legitimate nominal composition at the type level only:
    ;; :check-form-eval :never means no source graph or registry is forged.
    (check! checker :valid-source-bound-application-builder
            '(describe-application-assurance!
              {} {} (dependency/check-application-dependencies! {} {}) {}))
    true))

(defn- runtime-forgery! []
  (let [forged (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        copied {:checking :source-derived-composition-provenance-and-observed-writer-join
                :authority :none :certificate :not-issued}
        reason (fn [closure]
                 (try (report/describe-application-assurance! {} {} closure {})
                      nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))]
    (when-not (and (= :unrecognized-closure-handle (reason forged))
                   (= :unrecognized-closure-handle (reason copied)))
      (throw (ex-info "Forged or copied report bypassed application closure"
                      {:forged (reason forged) :copied (reason copied)})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (definitions)]
    (checks! checker forms)
    (runtime-forgery!)
    (println "Typed application assurance report passed: 2 original complete")
    (println "source bodies, 12 wrong-closure call/producer substitutions,")
    (println "11 false status mutations; 18 declared-closure, 10 composition,")
    (println "10 provenance, and 12 writer malformed handoffs rejected;")
    (println "two Any-typed original-source mistakes caught by audit;")
    (println "the provenance/writer swap rejected statically, plus")
    (println "copy/forgery runtime rejection. No theorem or authority claims.")
    :ok))

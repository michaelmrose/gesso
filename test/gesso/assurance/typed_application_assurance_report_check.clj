(ns gesso.assurance.typed-application-assurance-report-check
  "Opt-in source-backed Typed Clojure checks for final application reports.

   The original public functions must accept a nominal, registered-category
   ApplicationDependencyHandle rather than a raw or copied closure report.
   Their fresh upstream acquisition and recheck paths are source-audited.
   The negative report flags are checked against the ORIGINAL output map;
   this does NOT prove upstream source agreement, writer completeness,
   kernel soundness, authorization, or the trustworthiness of a copied map.

   Original upstream functions are independently checked and tested.
   Composition evidence reuses the producer's checked exact HMap; other
   reports remain opaque and their correspondence is runtime-validated.
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
            [gesso.assurance.typed-operation-interface-composition-check :as comp-check]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private report-ns 'gesso.assurance.proof.application-assurance-report)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
(def ^:private composed-evidence-type
  ;; The SAME exact structural type checked against the original composition
  ;; constructor in the neighboring source-bound Typed Clojure gate.
  comp-check/composed-type)

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
  ;; Composition uses the EXACT same checked structural return contract as
  ;; its original producer. Other external reports remain opaque; none of
  ;; these annotations certifies semantic correctness or live authority.
  (doseq [[name signature]
          [['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> app-handle]]
           ['gesso.assurance.proof.application-dependency/describe-application-dependencies
            [app-handle '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.operation-interface-composition/compose-operation-interfaces!
            ['typed.clojure/Any 'typed.clojure/Any app-handle '-> composed-evidence-type]]
           ['gesso.assurance.proof.operation-interface-provenance/describe-composition-provenance!
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Any
             app-handle '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-writer-coverage/describe-application-writer-coverage!
            ['typed.clojure/Any 'typed.clojure/Any app-handle
             'typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-assurance-report/checked-summary!
            ['typed.clojure/Any composed-evidence-type 'typed.clojure/Any
             'typed.clojure/Any '-> 'typed.clojure/Any]]
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

(defn- checks! [checker forms]
  (binding [*ns* (the-ns report-ns)]
    (check! checker :original-application-assurance-builder
            (original forms 'describe-application-assurance!))
    (check! checker :original-application-assurance-freshness
            (original forms 'require-current-application-assurance!))
    (source-handoff! forms)
    (require-current-handoff! forms)
    (check-final-flags! checker forms)
    (check-composition-evidence! checker forms)
    ;; Catalog/graph swaps and map-order swaps are type-compatible because
    ;; the upstream outputs and source arguments are intentionally Any.
    ;; Demonstrate that the SOURCE audit (not Typed Clojure) catches them.
    (let [source (original forms 'describe-application-assurance!)]
      (doseq [[label from to]
              [[:reversed-composition-sources
                '(composition/compose-operation-interfaces!
                  catalog current-graph closure)
                '(composition/compose-operation-interfaces!
                  current-graph catalog closure)]
               [:swapped-provenance-and-writer-reports
                '(checked-summary! declared composed manifest observed)
                '(checked-summary! declared composed observed manifest)]]]
        (let [modified (mutate-original-call source from to)
              changed-forms (mapv #(if (= source %) modified %) forms)]
          (check! checker [:type-compatible-wrong-source label] modified)
          (when-not (try (source-handoff! changed-forms)
                         false
                         (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Wrong-source report mutation escaped audit"
                            {:label label}))))))
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
    (println "11 false authority/status mutations, 10 malformed composition")
    (println "handoffs, two type-compatible source substitutions rejected")
    (println "by provenance audit, and")
    (println "copy/forgery runtime rejection. No theorem or authority claims.")
    :ok))

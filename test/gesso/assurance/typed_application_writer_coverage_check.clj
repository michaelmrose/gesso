(ns gesso.assurance.typed-application-writer-coverage-check
  "Opt-in source-backed typing of the copyable writer-coverage snapshot.

   Check the ORIGINAL final coverage-entry constructor, the public source
   acquisition function and the public freshness consumer. The original
   assembler and external v1366 writer checker still validate source ID
   correspondence at RUNTIME. This checker deliberately does NOT infer
   exhaustive deployed writer inventory, proof, certification, or authority."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.proof.application-dependency :as app]
            [gesso.assurance.proof.application-writer-coverage :as coverage]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.proof.application-writer-coverage)
(def ^:private app-handle
  'gesso.assurance.proof.application_dependency.ApplicationDependencyHandle)
(def ^:private id-set '(typed.clojure/Set typed.clojure/Kw))
(def ^:private diff-type
  `(typed.clojure/HMap
    :mandatory {:missing-from-right ~id-set
                :unmodeled-in-right ~id-set
                :exact? typed.clojure/Bool}
    :complete? true))
(def ^:private row-type
  `(typed.clojure/HMap
    :mandatory {:operation-id typed.clojure/Kw
                :in-declared-application? typed.clojure/Bool
                :in-independent-induction? typed.clojure/Bool
                :in-issued-application? typed.clojure/Bool
                :abstract-source-equivalence (typed.clojure/Val :not-established)
                :installed-handler-semantic-equivalence (typed.clojure/Val :not-established)
                :physical-transition-refinement (typed.clojure/Val :not-established)}
    :complete? true))
(def ^:private declared-type
  `(typed.clojure/HMap
    :mandatory {:operation-ids ~id-set
                :catalog-id typed.clojure/Any
                :catalog-fingerprint typed.clojure/Any
                :induction-fingerprint typed.clojure/Any
                :ledger-fingerprint typed.clojure/Any
                :original-open-obligation-ids-by-operation typed.clojure/Any
                :open-assumptions typed.clojure/Any}
    :complete? false))
(def ^:private original-writer-type
  `(typed.clojure/HMap
    :mandatory {:induction-catalog-id typed.clojure/Any
                :application-catalog-fingerprint typed.clojure/Any
                :observed-semantic-route-ids ~id-set
                :observed-entry-point-ids ~id-set}
    :complete? false))
(def ^:private snapshot-type
  `(typed.clojure/HMap
    :mandatory {:ids ~id-set :independent ~id-set :issued ~id-set
                :abstract-join ~diff-type
                :issued-join ~diff-type
                :independent-join ~diff-type
                :extras ~id-set
                :exact? typed.clojure/Bool
                :conditional? typed.clojure/Bool
                :open-obligations ~id-set
                :operation-rows (typed.clojure/Vec ~row-type)}
    :complete? true))
(def ^:private negative-claims
  {:type :gesso.assurance.proof.application-writer-coverage/source-relative-observed-join
   :version 1
   :checking :current-declared-source-and-independent-issued-host-snapshot
   :scope :supplied-live-declared-application-and-host-observation
   :assumption-discharge :not-performed
   :declaration-semantic-equivalence :not-established
   :host-enumerator-exhaustiveness :not-established
   :physical-writer-coverage :not-established
   :abstract-to-physical-correspondence :not-established
   :deployment-correspondence :not-established
   :universal-kernel-soundness :not-established
   :proof-status :conditional-snapshot-id-join-only
   :certificate :not-issued
   :admission :not-admitted
   :transaction-authority :none
   :authority :none})
(def ^:private status-type
  '(typed.clojure/U
    (typed.clojure/Val :open-operation-catalog-mismatch)
    (typed.clojure/Val :open-unclassified-observed-entry-points)
    (typed.clojure/Val :conditional-observed-operation-id-join)
    (typed.clojure/Val :open-independent-source-premises)))
(def ^:private host-status-type
  '(typed.clojure/U
    (typed.clojure/Val :contains-unclassified-observed-entries)
    (typed.clojure/Val :no-unclassified-entries-observed)))
;; Shared exact type: validated against the actual coverage-entry source and
;; reused by the application-assurance builder. It is an untrusted map.
(def report-type
  (list 'typed.clojure/HMap
        :mandatory
        (merge
         {:catalog-id 'typed.clojure/Any
          :catalog-fingerprint 'typed.clojure/Any
          :induction-fingerprint 'typed.clojure/Any
          :application-dependency-ledger-fingerprint 'typed.clojure/Any
          :writer-source-catalog-id 'typed.clojure/Any
          :writer-application-catalog-fingerprint 'typed.clojure/Any
          :declared-operation-ids id-set
          :independent-operation-ids id-set
          :issued-operation-ids id-set
          :declared-vs-independent diff-type
          :declared-vs-issued diff-type
          :independent-vs-issued diff-type
          :exact-observed-operation-id-join? 'typed.clojure/Bool
          :operation-rows (list 'typed.clojure/Vec row-type)
          :observed-semantic-route-ids id-set
          :observed-entry-point-ids id-set
          :observed-unclassified-entry-point-ids id-set
          :host-snapshot-status host-status-type
          :status status-type
          :original-open-obligation-ids-by-operation 'typed.clojure/Any
          :open-assumptions 'typed.clojure/Any
          :open-obligations id-set}
         (into {} (map (fn [[k v]] [k (list 'typed.clojure/Val v)])
                       negative-claims)))
        :complete? true))

(defn- definitions []
  (let [path (or (io/resource "gesso/assurance/proof/application_writer_coverage.clj")
                 (throw (ex-info "Writer coverage production source missing" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader path))]
      (loop [forms []]
        (let [form (read {:eof ::end} r)]
          (if (= ::end form) forms (recur (conj forms form))))))))

(defn- original [forms function]
  (let [matches (filter #(and (seq? %)
                              (#{'defn 'defn-} (first %))
                              (= function (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Original writer function missing/duplicated"
                      {:name function :count (count matches)})))
    (first matches)))

(defn- ann! [name signature]
  (eval (list 'typed.clojure/ann name (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype app-handle '[]))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.application-writer-coverage/coverage-type
           (typed.clojure/Val
            :gesso.assurance.proof.application-writer-coverage/source-relative-observed-join)))
  (eval '(typed.clojure/ann
           gesso.assurance.proof.application-writer-coverage/coverage-version
           (typed.clojure/Val 1)))
  (doseq [[name signature]
          [['gesso.assurance.proof.application-writer-coverage/row
            ['typed.clojure/Kw id-set id-set id-set '-> row-type]]
           ['gesso.assurance.proof.application-writer-coverage/coverage-entry
            [declared-type original-writer-type snapshot-type '-> report-type]]
           ['gesso.assurance.proof.application-writer-coverage/assemble-snapshot-report!
            [declared-type original-writer-type '-> report-type]]
           ['gesso.assurance.proof.application-writer-coverage/input!
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.proof.application-writer-coverage/checked-writer!
            ['typed.clojure/Any '-> original-writer-type]]
           ['gesso.assurance.proof.application-writer-coverage/plain-map?
            ['typed.clojure/Any '-> 'typed.clojure/Bool]]
           ['gesso.assurance.proof.application-writer-coverage/reject!
            ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Nothing]]
           ['gesso.assurance.proof.application-dependency/check-application-dependencies!
            ['typed.clojure/Any 'typed.clojure/Any '-> app-handle]]
           ['gesso.assurance.proof.application-dependency/require-current-application-dependencies!
            [app-handle 'typed.clojure/Any 'typed.clojure/Any '-> app-handle]]
           ['gesso.assurance.proof.application-dependency/describe-application-dependencies
            [app-handle '-> declared-type]]
           ['gesso.assurance.proof.application-writer-coverage/describe-application-writer-coverage!
            ['typed.clojure/Any 'typed.clojure/Any app-handle
             'typed.clojure/Any '-> report-type]]
           ['gesso.assurance.proof.application-writer-coverage/require-current-application-writer-coverage!
            ['typed.clojure/Any 'typed.clojure/Any 'typed.clojure/Any
             app-handle 'typed.clojure/Any '-> report-type]]]]
    (ann! name signature)))

(defn- check! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Writer source Typed Clojure check failed"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label expression]
  (let [{:keys [ex type-errors]} (checker expression :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Malformed writer report escaped type-check"
                      {:label label :types kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- map-result [source]
  (let [r (last source)]
    (when-not (map? r)
      (throw (ex-info "Original writer entry no longer returns a map" {})))
    r))

(defn- mutate-result [source alter]
  (let [old (map-result source)
        changed (alter old)]
    (when (= old changed)
      (throw (ex-info "No-op writer mutation" {})))
    (apply list (concat (butlast source) [changed]))))

(def ^:private original-provenance
  {:catalog-id '(:catalog-id declared)
   :catalog-fingerprint '(:catalog-fingerprint declared)
   :induction-fingerprint '(:induction-fingerprint declared)
   :application-dependency-ledger-fingerprint '(:ledger-fingerprint declared)
   :writer-source-catalog-id '(:induction-catalog-id writer-report)
   :writer-application-catalog-fingerprint
   '(:application-catalog-fingerprint writer-report)
   :declared-operation-ids 'ids
   :independent-operation-ids 'independent
   :issued-operation-ids 'issued
   :declared-vs-independent 'abstract-join
   :declared-vs-issued 'issued-join
   :independent-vs-issued 'independent-join
   :exact-observed-operation-id-join? 'exact?
   :operation-rows 'operation-rows
   :observed-semantic-route-ids '(:observed-semantic-route-ids writer-report)
   :observed-entry-point-ids '(:observed-entry-point-ids writer-report)
   :observed-unclassified-entry-point-ids 'extras
   :original-open-obligation-ids-by-operation
   '(:original-open-obligation-ids-by-operation declared)
   :open-assumptions '(:open-assumptions declared)
   :open-obligations 'open-obligations})

(defn- audit-entry! [source]
  (let [report (map-result source)]
    (when-not (and (= (set (keys report))
                      (into (set (keys original-provenance))
                            (concat (keys negative-claims)
                                    [:host-snapshot-status :status])))
                   (every? (fn [[k expected]] (= expected (get report k)))
                           original-provenance)
                   (every? (fn [[k expected]] (= expected (get report k)))
                           (dissoc negative-claims :type :version))
                   (= 'coverage-type (:type report))
                   (= 'coverage-version (:version report))
                   (= '(if (seq extras)
                         :contains-unclassified-observed-entries
                         :no-unclassified-entries-observed)
                      (:host-snapshot-status report))
                   (= '(cond
                         (not exact?) :open-operation-catalog-mismatch
                         (seq extras) :open-unclassified-observed-entry-points
                         conditional? :conditional-observed-operation-id-join
                         :else :open-independent-source-premises)
                      (:status report)))
      (throw (ex-info "Original writer descriptor provenance changed"
                      {:report report})))
    true))

(defn- audit-assembly! [forms]
  (let [source (original forms 'assemble-snapshot-report!)
        body (last source)
        bindings (second body)
        binding-map (into {} (map vec (partition 2 bindings)))
        args (last body)
        expected-map
        '{:ids ids :independent independent :issued issued
          :abstract-join abstract-join :issued-join issued-join
          :independent-join independent-join :extras extras
          :exact? exact? :conditional? conditional?
          :open-obligations open-obligations
          :operation-rows (mapv (fn [id] (row id ids independent issued))
                                (sort-by str all-ids))}]
    (when-not (and (= 'let (first body))
                   (= '(coverage-entry declared writer-report)
                      (take 3 args))
                   (= expected-map (nth args 3))
                   (= '(:operation-ids declared) (get binding-map 'ids))
                   (= '(:independent-operation-ids writer-report)
                      (get binding-map 'independent))
                   (= '(:issued-application-operation-ids writer-report)
                      (get binding-map 'issued))
                   (= '(set/union ids independent issued)
                      (get binding-map 'all-ids))
                   (= '(set/union permanent-open-obligations
                                  (:open-meta-obligations declared)
                                  (:open-obligations writer-report)
                                  (when-not exact?
                                    #{:declared-independent-and-issued-operation-id-mismatch})
                                  (when (seq extras)
                                    #{:observed-entry-points-outside-declared-application}))
                      (get binding-map 'open-obligations))
                   (= '(and (:exact? abstract-join)
                            (:exact? issued-join)
                            (:exact? independent-join))
                      (get binding-map 'exact?))
                   (= 'when-not (first (nth body 2)))
                   (= :unexpected-application-closure-contract
                      (second (nth (nth body 2) 2))))
      (throw (ex-info "Original writer assembly/source guard changed"
                      {:last-call args :bindings binding-map :body body})))
    true))

(defn- audit-public! [forms]
  (let [source (original forms 'describe-application-writer-coverage!)
        steps (vec (drop 4 source))
        fresh (original forms 'require-current-application-writer-coverage!)
        body (last fresh)]
    (when-not (= ['(input! writer-source)
                  '(application/require-current-application-dependencies!
                    closure catalog current-graph)
                  '(let [declared (application/describe-application-dependencies closure)
                         writer-report (checked-writer! writer-source)]
                     (assemble-snapshot-report! declared writer-report))]
                 steps)
      (throw (ex-info "Original writer acquisition handoff changed"
                      {:steps steps})))
    (when-not (and (= '(describe-application-writer-coverage!
                        catalog current-graph closure writer-source)
                      (second (second body)))
                   (= '(when-not (and (plain-map? report)
                                     (nil? (meta report))
                                     (= report now))
                         (reject! :stale-or-substituted-coverage-report {}))
                      (nth body 2))
                   (= 'now (last body)))
      (throw (ex-info "Original coverage freshness guard changed" {})))
    true))

(defn- checked! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (doseq [name '[row coverage-entry describe-application-writer-coverage!
                   require-current-application-writer-coverage!]]
      (check! checker [:original name] (original forms name)))
    (audit-entry! (original forms 'coverage-entry))
    (audit-assembly! forms)
    (audit-public! forms)
    (let [row-source (original forms 'row)]
      (doseq [[label alter]
              [[:wrong-operation-id #(assoc % :operation-id 12)]
               [:bad-declared-flag #(assoc % :in-declared-application? :yes)]
               [:bad-issued-flag #(assoc % :in-issued-application? 1)]
               [:false-abstract-equivalence
                #(assoc % :abstract-source-equivalence :proved)]
               [:false-handler-equivalence
                #(assoc % :installed-handler-semantic-equivalence :proved)]
               [:false-physical-refinement
                #(assoc % :physical-transition-refinement :proved)]
               [:missing-independent-flag
                #(dissoc % :in-independent-induction?)]
               [:invented-row-authority #(assoc % :transaction-authority :granted)]]]
        (reject! checker [:invalid-operation-row label]
                 (mutate-result row-source alter))))
    (let [source (original forms 'coverage-entry)]
      (doseq [[k _] negative-claims]
        (reject! checker [:false-status k]
                 (mutate-result source #(assoc % k :proved))))
      (doseq [[label alter]
              [[:missing-writer-ids #(dissoc % :issued-operation-ids)]
               [:missing-declared-ids #(dissoc % :declared-operation-ids)]
               [:raw-observed-ids #(assoc % :observed-entry-point-ids [])]
               [:raw-writer-rows #(assoc % :operation-rows {})]
               [:not-a-set #(assoc % :open-obligations nil)]
               [:wrong-exact-flag #(assoc % :exact-observed-operation-id-join? "true")]
               [:fake-authorization #(assoc % :deployed-authority :granted)]
               [:bad-status #(assoc % :status :proved)]
               [:bad-host-status #(assoc % :host-snapshot-status :all-writers-covered)]]]
        (reject! checker label (mutate-result source alter)))
      (doseq [[label change]
              [[:induction-as-catalog #(assoc % :catalog-fingerprint
                                             '(:induction-fingerprint declared))]
               [:writer-source-from-catalog
                #(assoc % :writer-source-catalog-id '(:catalog-id declared))]]]
        (let [modified (mutate-result source change)]
          (check! checker [:type-compatible-misbound-source label] modified)
          (when-not (try (audit-entry! modified)
                         false (catch clojure.lang.ExceptionInfo _ true))
            (throw (ex-info "Writer source mixup escaped audit" {:case label}))))))
    (check! checker :valid-writer-coverage-consumer
            '(describe-application-writer-coverage!
              {} {} (application/check-application-dependencies! {} {}) {}))
    (doseq [[label bad]
            [[:raw-closure '(describe-application-writer-coverage! {} {} {} {})]
             [:nil-closure '(describe-application-writer-coverage! {} {} nil {})]
             [:raw-closure-freshness
              '(require-current-application-writer-coverage! {} {} {} {} {})]
             [:nil-closure-freshness
              '(require-current-application-writer-coverage! {} {} {} nil {})]]]
      (reject! checker label bad))
    true))

(defn- runtime-forgery! []
  (let [forged (gesso.assurance.proof.application_dependency.ApplicationDependencyHandle.)
        source {:induction-catalog {} :application-induction-source {}}
        reason (try (coverage/describe-application-writer-coverage! {} {} forged source)
                    nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))]
    (when-not (= :unrecognized-closure-handle reason)
      (throw (ex-info "Forged application closure accepted by writer coverage"
                      {:reason reason})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (definitions)]
    (checked! checker forms)
    (runtime-forgery!)
    (println "Typed writer coverage passed: 4 complete original functions,")
    (println "37 invalid row/report/handle cases, two provenance swaps")
    (println "rejected by source audit, real three-set assembly guards,")
    (println "and runtime forged-closure rejection.")
    (println "Runtime still owns observed writer completeness and authority.")
    :ok))

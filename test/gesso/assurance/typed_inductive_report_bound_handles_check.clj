(ns gesso.assurance.typed-inductive-report-bound-handles-check
  "Opt-in source-backed typing of the two independent induction report handles.

   Checked-mode reporting must revalidate a live source-relative induction
   closure AND a distinct conditional CertificateHandle against the SAME
   catalog/graph before it examines their copyable descriptions. The actual
   original revalidation helper is checked under nominal handle types; the
   production caller and checked-mode branch are separately audited.

   This does not check the full report, establish soundness, discharge an
   assumption, or grant runtime authority. Registry contents, freshness,
   original proof correspondence, and mode validation remain runtime claims."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.assurance.inductive-report :as report]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.induction-dependency :as closure]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private source-ns 'gesso.assurance.inductive-report)
(def ^:private cert-handle 'gesso.assurance.proof.certificate.CertificateHandle)
(def ^:private closure-handle
  'gesso.assurance.proof.induction_dependency.InductionDependencyHandle)
(def ^:private validated-handles
  '(typed.clojure/HMap
    :mandatory {:certificate-handle gesso.assurance.proof.certificate.CertificateHandle
                :dependency-handle
                gesso.assurance.proof.induction_dependency.InductionDependencyHandle}
    :complete? true))

(defn- source-definitions []
  (let [path (or (io/resource "gesso/assurance/inductive_report.clj")
                 (throw (ex-info "Production inductive report source missing" {})))]
    (with-open [r (java.io.PushbackReader. (io/reader path))]
      (loop [all []]
        (let [form (read {:eof ::end} r)]
          (if (= ::end form) all (recur (conj all form))))))))

(defn- original [forms function]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                              (= function (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Expected exactly one production report definition"
                      {:function function :count (count matches)})))
    (first matches)))

(defn- annotate! [name signature]
  (eval (list 'typed.clojure/ann name (list 'typed.clojure/IFn signature))))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype cert-handle '[]))
  (eval (list 'typed.clojure/ann-datatype closure-handle '[]))
  (doseq [[name signature]
          [['gesso.assurance.proof.certificate/require-current-certificate!
            [cert-handle 'typed.clojure/Any 'typed.clojure/Any '-> cert-handle]]
           ['gesso.assurance.proof.induction-dependency/require-current-induction-dependencies!
            [closure-handle 'typed.clojure/Any 'typed.clojure/Any '-> closure-handle]]
           ['gesso.assurance.inductive-report/revalidate-bound-handles!
            ['typed.clojure/Any 'typed.clojure/Any
             cert-handle closure-handle '-> validated-handles]]]]
    (annotate! name signature)))

(defn- check! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original report handle source failed static checking"
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- reject! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error
                             :typed.clojure/app-type-error} kinds))
      (throw (ex-info "Invalid report handle flow escaped Typed Clojure"
                      {:label label :actual kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn- source-relative-paired-validation! [source]
  ;; Both source arguments have type Any in this pilot. Their equality and
  ;; order must therefore be checked against the ORIGINAL expression, not
  ;; attributed to Typed Clojure's nominal handle typing.
  (let [actual (last source)
        expected-dependency
        '(induction-dependency/require-current-induction-dependencies!
          dependency-handle catalog current-graph)
        expected-certificate
        '(certificate/require-current-certificate!
          certificate-handle catalog current-graph)]
    (when-not (and (map? actual)
                   (= #{:dependency-handle :certificate-handle}
                      (set (keys actual)))
                   (= expected-dependency (:dependency-handle actual))
                   (= expected-certificate (:certificate-handle actual)))
      (throw (ex-info "Report handle pair no longer revalidates same source"
                      {:actual actual})))
    true))

(defn- report-bound-call [forms]
  (let [definition (original forms 'require-bound-evidence!)
        body (last definition)
        bindings (when (and (seq? body) (= 'let (first body))) (second body))
        pairs (when (vector? bindings) (partition 2 bindings))
        expected-map '{checked-dependency :dependency-handle
                       checked-certificate :certificate-handle}
        expected-call '(revalidate-bound-handles!
                        catalog current-graph certificate-handle dependency-handle)
        expected-dependency '(induction-dependency/describe-induction-dependencies
                              checked-dependency)
        expected-cert '(certificate/describe-certificate checked-certificate)]
    (when-not (and (= expected-map (ffirst pairs))
                   (= expected-call (second (first pairs)))
                   (= 'd (first (second pairs)))
                   (= expected-dependency (second (second pairs)))
                   (= 'c (first (nth pairs 2)))
                   (= expected-cert (second (nth pairs 2)))
                   (seq (drop 3 pairs)))
      ;; The full bound-evidence predicate remains covered by runtime tests;
      ;; only the exact validated-handle handoff is checked above.
      (throw (ex-info "Report no longer describes validated, distinct handles"
                      {:bindings (vec (take 3 pairs))})))
    expected-call))

(defn- checked-mode-call! [forms]
  ;; The two-handle consumer must still run only in :checked-handles mode,
  ;; not in source-only mode or for caller-supplied copyable descriptions.
  (let [hits (atom [])]
    (walk/postwalk
     (fn [x]
       (when (and (seq? x) (= 'when (first x))
                  (= '(= :checked-handles mode) (second x)))
         (swap! hits conj x))
       x)
     (original forms 'build-inductive-report!))
    (when-not (= ['(when (= :checked-handles mode)
                     (require-bound-evidence!
                      catalog current-graph
                      certificate-handle dependency-handle l))] @hits)
      (throw (ex-info "Checked-mode induction report gate changed" {:found @hits})))
    true))

(defn- mutate-map [source alter]
  (let [old (last source)]
    (when-not (map? old)
      (throw (ex-info "Original paired-handle constructor changed" {})))
    (let [new (alter old)]
      (when (= old new) (throw (ex-info "No-op report handle mutation" {})))
      (apply list (concat (butlast source) [new])))))

(defn- check-production! [checker forms]
  (binding [*ns* (the-ns source-ns)]
    (let [source (original forms 'revalidate-bound-handles!)]
      (check! checker :original-revalidation-source source)
      (source-relative-paired-validation! source)
      ;; Demonstrate that the source audit, not the static type system,
      ;; catches catalog/graph substitutions of identical Any types.
      (doseq [[label mutator]
              [[:dependency-swapped-source
                #(assoc % :dependency-handle
                        '(induction-dependency/require-current-induction-dependencies!
                          dependency-handle current-graph catalog))]
               [:certificate-swapped-source
                #(assoc % :certificate-handle
                        '(certificate/require-current-certificate!
                          certificate-handle current-graph catalog))]]]
        (let [modified (mutate-map source mutator)
              caught (try (source-relative-paired-validation! modified)
                          false
                          (catch clojure.lang.ExceptionInfo _ true))]
          (when-not caught
            (throw (ex-info "Source argument mutation escaped audit"
                            {:label label})))))
      (report-bound-call forms)
      (checked-mode-call! forms)
      (doseq [[label alter]
              [[:raw-dependency #(assoc % :dependency-handle {})]
               [:wrong-dependency-category #(assoc % :dependency-handle
                                                   'certificate-handle)]
               [:raw-certificate #(assoc % :certificate-handle {})]
               [:wrong-certificate-category #(assoc % :certificate-handle
                                                    'dependency-handle)]
               [:missing-dependency #(dissoc % :dependency-handle)]
               [:missing-certificate #(dissoc % :certificate-handle)]
               [:invented-authority #(assoc % :authority :granted)]
               [:invented-proof #(assoc % :proof-status :proved)]
               [:copyable-descriptor #(assoc % :dependency-handle
                                              {:checking :structural-only})]]]
        (reject! checker label (mutate-map source alter)))
      (doseq [[label token substitute]
              [[:swap-validator-target 'induction-dependency/require-current-induction-dependencies!
                '(induction-dependency/require-current-induction-dependencies!
                  certificate-handle catalog current-graph)]
               [:swap-certificate-target 'certificate/require-current-certificate!
                '(certificate/require-current-certificate!
                  dependency-handle catalog current-graph)]]]
        (let [found (atom 0)
              modified (walk/postwalk
                        (fn [x]
                          (if (and (seq? x) (= token (first x)))
                            (do (swap! found inc) substitute)
                            x)) source)]
          (when-not (= 1 @found)
            (throw (ex-info "Production validator handoff changed"
                            {:label label :matches @found})))
          (reject! checker label modified)))
      (check! checker :correct-paired-call
              '(typed.clojure/ann-form
                (fn [catalog graph certificate-handle dependency-handle]
                  (revalidate-bound-handles!
                   catalog graph certificate-handle dependency-handle))
                (typed.clojure/IFn
                 [typed.clojure/Any typed.clojure/Any
                  gesso.assurance.proof.certificate.CertificateHandle
                  gesso.assurance.proof.induction_dependency.InductionDependencyHandle
                  :-> (typed.clojure/HMap
                       :mandatory {:certificate-handle
                                   gesso.assurance.proof.certificate.CertificateHandle
                                   :dependency-handle
                                   gesso.assurance.proof.induction_dependency.InductionDependencyHandle}
                       :complete? true)])))
      (doseq [[label expr]
              [[:raw-certificate-call '(revalidate-bound-handles! {} {} {} nil)]
               [:raw-dependency-call '(revalidate-bound-handles! {} {} nil {})]
               [:swapped-handle-call
                '(typed.clojure/ann-form
                  (fn [catalog graph certificate-handle dependency-handle]
                    (revalidate-bound-handles!
                     catalog graph dependency-handle certificate-handle))
                  (typed.clojure/IFn
                   [typed.clojure/Any typed.clojure/Any
                    gesso.assurance.proof.certificate.CertificateHandle
                    gesso.assurance.proof.induction_dependency.InductionDependencyHandle
                    :-> typed.clojure/Any]))]]]
        (reject! checker label expr))
      true)))

(defn- check-runtime-guard! []
  ;; No registration is created by direct instance construction. The first
  ;; closure check runs before certificate revalidation, as in the original
  ;; implementation, and rejects a forged or wrong-category dependency.
  (let [forged-certificate (gesso.assurance.proof.certificate.CertificateHandle.)
        forged-dependency
        (gesso.assurance.proof.induction_dependency.InductionDependencyHandle.)
        revalidate (or (ns-resolve source-ns 'revalidate-bound-handles!)
                       (throw (ex-info "Inductive report revalidator absent" {})))
        reason (try (revalidate {} {} forged-certificate forged-dependency)
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:reason (ex-data e))))]
    (when-not (= :unrecognized-dependency-handle reason)
      (throw (ex-info "Forged report dependency crossed source registry"
                      {:reason reason})))
    true))

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (source-definitions)]
    (check-production! checker forms)
    (check-runtime-guard!)
    (println "Typed inductive report handles passed: two independent nominal")
    (println "handle validators, original source, checked-mode source handoff,")
    (println "wrong-type mutations, and forged-handle runtime rejection.")
    (println "No claim of theorem soundness, assumption discharge, or authority.")
    :ok))

(ns gesso.assurance.typed-admission-boundary-check
  "Opt-in Typed Clojure checks for real Gesso structural admission source.

   A nominal AdmissionHandle is NOT evidence of a live registry entry, a
   discharged premise, a theorem, or production authorization. The checked
   consumers require the nominal class; their existing runtime registry and
   expected-subject checks remain essential. This gate deliberately treats
   the registry lookup as opaque rather than asserting a checked map shape."
  (:require [clojure.java.io :as io]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.assumption]
            [gesso.assurance.subject]
            [typed.clojure]))

(def ^:private check-config {:max-processors 1 :check-form-eval :never})
(def ^:private admission-ns 'gesso.assurance.admission)
(def ^:private handle-class 'gesso.assurance.admission.AdmissionHandle)
(def ^:private generic-map '(typed.clojure/Map typed.clojure/Any typed.clojure/Any))

(defn- original-definitions []
  (let [source (or (io/resource "gesso/assurance/admission.clj")
                   (throw (ex-info "Production admission source missing." {})))]
    (with-open [reader (java.io.PushbackReader. (io/reader source))]
      (loop [acc []]
        (let [form (read {:eof ::end} reader)]
          (if (= ::end form)
            acc
            (recur (conj acc form))))))))

(defn- original [forms name]
  (let [matches (filter #(and (seq? %)
                              (#{'defn 'defn-} (first %))
                              (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate production admission function."
                      {:function name :count (count matches)})))
    (first matches)))

(defn- annotate! [name signature]
  (eval (list 'typed.clojure/ann
              name (list 'typed.clojure/IFn signature))))

(defn- check-source! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config check-config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original admission implementation failed static checking."
                      {:label label :errors (mapv str type-errors)} ex)))
    true))

(defn- check-rejection!
  ([checker label modified expected-error]
   (let [{:keys [ex type-errors]} (checker modified :check-config check-config)
         kinds (mapv :type-error type-errors)]
     (when-not (and (nil? ex) (= [expected-error] kinds))
       (throw (ex-info "Invalid admission expression escaped static checking."
                       {:label label :expected expected-error :actual kinds
                        :exception (some-> ex ex-message)} ex)))
     true))
  ([checker label original modified expected-error]
   (when (= original modified)
     (throw (ex-info "Admission negative mutation has no target."
                     {:label label})))
   (check-rejection! checker label modified expected-error)))

(defn- replace-final [definition replacement]
  (apply list (concat (butlast definition) [replacement])))

(defn- install-contracts! []
  (eval (list 'typed.clojure/ann-datatype handle-class '[]))
  ;; These annotations apply to actual JVM source, not test-only facades.
  (eval '(typed.clojure/ann gesso.assurance.admission/admitted-kinds
                            (typed.clojure/Set typed.clojure/Kw)))
  (eval '(typed.clojure/ann gesso.assurance.admission/live-handles
                            java.util.WeakHashMap))
  ;; A WeakHashMap lookup returns Object/nil: do not invent a typed entry.
  ;; The consumer's existing runtime checks defend the structural boundary.
  (doseq [[name signature]
          [['gesso.assurance.admission/registered-entry
            ['typed.clojure/Any '-> 'typed.clojure/Any]]
           ['gesso.assurance.admission/admission-error!
            ['typed.clojure/Any generic-map '-> 'typed.clojure/Nothing]]
           ['gesso.assurance.subject/describe-subject
            ['typed.clojure/Any '-> generic-map]]
           ['gesso.assurance.assumption/match-premise!
            ['typed.clojure/Any 'typed.clojure/Any '-> generic-map]]
           ['gesso.assurance.admission/issue-structural-handle!
            [generic-map '-> handle-class]]
           ['gesso.assurance.admission/admit-subject!
            ['typed.clojure/Any '-> handle-class]]
           ['gesso.assurance.admission/admit-premise-match!
            ['typed.clojure/Any 'typed.clojure/Any '-> handle-class]]
           ['gesso.assurance.admission/require-admitted-subject!
            [handle-class 'typed.clojure/Any '-> handle-class]]
           ['gesso.assurance.admission/require-admitted-premise-match!
            [handle-class 'typed.clojure/Any 'typed.clojure/Any
             '-> handle-class]]]]
    (annotate! name signature)))

(defn- check-production! [checker forms]
  (binding [*ns* (the-ns admission-ns)]
    (doseq [name '[registered-entry issue-structural-handle!
                   admit-subject! admit-premise-match!
                   require-admitted-subject! require-admitted-premise-match!]]
      (check-source! checker [:original name] (original forms name)))
    ;; The direct typed call accepts an actual minted nominal handle.
    (check-source! checker :subject-valid-call
                   '(require-admitted-subject! (admit-subject! {}) {}))
    (check-source! checker :premise-valid-call
                   '(require-admitted-premise-match!
                     (admit-premise-match! {} {}) {} {}))
    ;; These are type errors even though ordinary Clojure would run the
    ;; function and let its runtime registry check reject the values.
    (doseq [[label function arguments]
            [[:raw-subject-map 'require-admitted-subject! '[{} {}]]
             [:raw-subject-nil 'require-admitted-subject! '[nil {}]]
             [:subject-description 'require-admitted-subject!
              '[{:kind :subject-identity :proof-status :not-proved} {}]]
             [:raw-premise-map 'require-admitted-premise-match!
              '[{} {} {}]]
             [:raw-premise-keyword 'require-admitted-premise-match!
              '[:made-up {} {}]]]]
      (let [expr (list* function arguments)]
        (check-rejection! checker label expr
                          :typed.clojure/app-type-error)))
    ;; Wrong source returns are rejected in the ORIGINAL definition body.
    (doseq [[label name bogus]
            [[:issuer-returns-data 'issue-structural-handle! {}]
             [:subject-issuer-returns-data 'admit-subject! {}]
             [:premise-issuer-returns-data 'admit-premise-match! {}]
             [:subject-consumer-returns-raw 'require-admitted-subject!
              'expected-subject]
             [:premise-consumer-returns-raw 'require-admitted-premise-match!
              'required]]]
      (let [source (original forms name)]
        (check-rejection! checker label source
                          (replace-final source bogus)
                          :typed.clojure/type-mismatch-error)))
    true))

(def ^:private sample-subject
  {:kind :operation :id :typed/structural-admission
   :interface {:operation :typed/structural-admission}})

(defn- check-runtime-guard! []
  ;; A caller can directly instantiate the exact nominal JVM class, but
  ;; that DOES NOT produce a checker-issued, authenticated handle.
  (let [issued (admission/admit-subject! sample-subject)
        forged (gesso.assurance.admission.AdmissionHandle.)
        description (admission/describe-handle issued)]
    (when-not (and (admission/admitted? issued)
                   (identical? issued
                               (admission/require-admitted-subject!
                                issued sample-subject))
                   (not (admission/admitted? forged))
                   (not (admission/admitted? description)))
      (throw (ex-info "Structural admission runtime contract failed." {})))
    (doseq [[label value] [[:direct-class-constructor forged]
                           [:copyable-description description]]]
      (let [reason (try
                     (admission/require-admitted-subject!
                      value sample-subject)
                     nil
                     (catch clojure.lang.ExceptionInfo e
                       (:reason (ex-data e))))]
        (when-not (= :unrecognized-handle reason)
          (throw (ex-info "Unregistered nominal/data handle was accepted."
                          {:label label :actual reason}))))))
  true)

(defn -main [& _]
  (install-contracts!)
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)]
    (check-production! checker (original-definitions))
    (check-runtime-guard!)
    (println "Typed structural admission passed: six real source bodies,")
    (println "two correctly typed calls, five raw-value rejections, five")
    (println "source-return mutations, and runtime forged-handle rejection.")
    (println "Scope: JVM structural handles only; no theorem or auth claim.")
    :ok))

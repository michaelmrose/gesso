(ns gesso.choreo.typed-machine-normalization-check
  "Source-backed optional Typed Clojure gate for production machine identity
   normalization. Invoked after the core nominal-type annotations are loaded.
   This file is test-only; ordinary downstream Clojure needs no annotations."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.choreo.machine]))

(def ^:private config {:max-processors 1 :check-form-eval :never})

(defn- source-forms [resource-path]
  (let [source (or (io/resource resource-path)
                   (throw (ex-info "Missing machine source file."
                                   {:resource resource-path})))]
    (with-open [reader (java.io.PushbackReader. (io/reader source))]
      (loop [acc []]
        (let [form (read {:read-cond :allow :features #{:clj} :eof ::done} reader)]
          (if (= ::done form) acc (recur (conj acc form))))))))

(defn- source-definition [forms name]
  (let [found (filter #(and (seq? %)
                            (#{'defn 'defn-} (first %))
                            (= name (second %))) forms)]
    (when-not (= 1 (count found))
      (throw (ex-info "Missing/duplicate original production function."
                      {:function name :count (count found)})))
    (first found)))

(defn- check-source! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Machine source body failed Typed Clojure."
                      {:label label
                       :type-errors (mapv :type-error type-errors)} ex)))
    true))

(defn- check-rejection!
  ([checker label form]
   (check-rejection! checker label form :typed.clojure/app-type-error))
  ([checker label form expected]
   (let [{:keys [ex type-errors]} (checker form :check-config config)
         actual (mapv :type-error type-errors)]
     (when-not (and (nil? ex) (= [expected] actual))
       (throw (ex-info "Expected static error was not produced."
                       {:label label :expected expected :actual actual
                        :exception (some-> ex ex-message)})))
     true)))

(defn- expect-audit-rejection! [audit label changed expected]
  (let [actual (try (audit changed) nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/kind (ex-data e))))]
    (when-not (= expected actual)
      (throw (ex-info "Source mutation escaped semantic audit."
                      {:label label :expected expected :actual actual})))
    true))

(defn- audit-machine-identity-require! [definition schema-kind error-kind]
  ;; A general Map type cannot express the category chosen by an existing
  ;; schema validator. Audit the *production* discriminator and explanation
  ;; against each specific required identity slot, not a hand-written model.
  (let [check-form (nth definition 3 nil)
        fail-call (when (and (seq? check-form)
                             (= 'when-not (first check-form)))
                    (nth check-form 2 nil))
        validator (when (seq? check-form) (second check-form))
        details (when (seq? fail-call) (nth fail-call 3 nil))]
    (when-not (and (= '(value) (list (first (nth definition 2))))
                   (= (list 'type/valid? schema-kind 'value) validator)
                   (= 'machine-error (first fail-call))
                   (= error-kind (second fail-call))
                   (= schema-kind (:schema-key details))
                   (= (list 'type/explain-data schema-kind 'value)
                      (:explanation details))
                   (= 'value (last definition)))
      (throw (ex-info "Machine identity validator changed its category."
                      {:error/kind :wrong-machine-identity-validator
                       :function (second definition)})))
    true))

(defn- audit-machine-explicit-merge! [definition]
  ;; The merging helper must not overwrite conflicting explicit identities.
  (let [body (last definition)
        branch (when (and (seq? body) (= 'if (first body)))
                 (nth body 3 nil))
        bindings (when (and (seq? branch) (= 'let (first branch)))
                   (second branch))
        check-form (when (seq? branch) (nth branch 2 nil))
        conflict (when (seq? check-form) (nth check-form 2 nil))]
    (when-not (and (= '(nil? value) (second body))
                   (= 'bindings (nth body 2 nil))
                   (= '[existing (get bindings key :gesso.choreo.machine/absent)] bindings)
                   (= '(and (not= :gesso.choreo.machine/absent existing)
                            (not= existing value))
                      (second check-form))
                   (= 'machine-error (first conflict))
                   (= :identity-binding-conflict (second conflict))
                   (= '(assoc bindings key value) (last branch)))
      (throw (ex-info "Machine identity merging lost conflict validation."
                      {:error/kind :wrong-machine-identity-merge})))
    true))

(defn- audit-machine-binding-normalization! [definition]
  ;; Typed Clojure proves a tagged map is returned, but not that the command
  ;; and execution *kinds* were checked against their respective schemas,
  ;; nor that conflicts and role correspondence survive. Check actual source.
  (let [body (last definition)
        pairs (when (and (seq? body) (= 'let (first body)))
                (second body))
        locals (when (vector? pairs)
                 (into {} (map vec (partition 2 pairs))))
        initial (get locals 'bindings')
        role-check (when (seq? body) (nth body 2 nil))]
    (when-not (and (= 'try (first initial))
                   (= '(identity/bindings (or bindings {}))
                      (second initial))
                   (= '(when (some? command-id)
                         (require-command-id! command-id))
                      (get locals 'command-id'))
                   (= '(when (some? execution-id)
                         (require-execution-id! execution-id))
                      (get locals 'execution-id'))
                   (= '(-> bindings'
                           (merge-explicit-binding :command-id command-id')
                           (merge-explicit-binding :execution-id execution-id'))
                      (get locals 'with-explicit))
                   (= '(:role with-explicit) (get locals 'bound-role))
                   (= '(:role plan) (get locals 'plan-role))
                   (= '(and (some? bound-role)
                            (not= bound-role plan-role))
                      (second role-check))
                   (= 'machine-error (first (nth role-check 2 nil)))
                   (= :identity-role-mismatch
                      (second (nth role-check 2 nil)))
                   (= '(identity/bindings
                         (assoc with-explicit :role plan-role))
                      (last body)))
      (throw (ex-info "Machine binding normalization changed its typed boundary."
                      {:error/kind :wrong-machine-binding-normalization})))
    true))

(defn check! [checker]
  ;; A verified Map argument/return discipline for the original production
  ;; normalizer. Valid tagged identity *kinds* remain runtime schema checks:
  ;; no proof that an arbitrary input Map is a CommandId is claimed here.
  (let [machine-ns 'gesso.choreo.machine
        generic-map '(typed.clojure/Map typed.clojure/Any typed.clojure/Any)
        tagged-map '(typed.clojure/Map
                     typed.clojure/Kw
                     (typed.clojure/U typed.clojure/Kw
                                      (typed.clojure/Map
                                       typed.clojure/Any typed.clojure/Any)))
        optional-id (list 'typed.clojure/U nil generic-map)
        forms (binding [*ns* (the-ns machine-ns)]
                (source-forms "gesso/choreo/machine.cljc"))
        machine-error (source-definition forms 'machine-error)
        command (source-definition forms 'require-command-id!)
        execution (source-definition forms 'require-execution-id!)
        merge-fn (source-definition forms 'merge-explicit-binding)
        normalize (source-definition forms 'normalize-machine-bindings)]
    ;; Gesso's existing schema validation wrappers are trusted here, backed
    ;; by the production type and machine tests; this does not checker-verify
    ;; the entire schema implementation or the whole machine/start function.
    (doseq [[name signature]
            [['gesso.choreo.machine/machine-error
              ['typed.clojure/Kw 'typed.clojure/Str generic-map
               '-> 'typed.clojure/Nothing]]
             ['gesso.choreo.type/valid?
              ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Bool]]
             ['gesso.choreo.type/explain-data
              ['typed.clojure/Any 'typed.clojure/Any '-> 'typed.clojure/Any]]
             ['gesso.choreo.machine/require-command-id!
              [generic-map '-> generic-map]]
             ['gesso.choreo.machine/require-execution-id!
              [generic-map '-> generic-map]]
             ['gesso.choreo.machine/merge-explicit-binding
              [tagged-map 'typed.clojure/Kw optional-id '-> tagged-map]]
             ['gesso.choreo.machine/normalize-machine-bindings
              [generic-map 'typed.clojure/Any optional-id optional-id
               '-> tagged-map]]]]
      (eval (list 'typed.clojure/ann name
                  (list 'typed.clojure/IFn signature))))
    (binding [*ns* (the-ns machine-ns)]
      (doseq [[name form]
              [[:machine-error machine-error]
               [:require-command-id command]
               [:require-execution-id execution]
               [:merge-explicit-binding merge-fn]
               [:normalize-machine-bindings normalize]]]
        (check-source! checker [:production-machine-binding-body name] form))
      (audit-machine-identity-require! command
                                       :gesso.choreo.type/command-id
                                       :invalid-command-id)
      (audit-machine-identity-require! execution
                                       :gesso.choreo.type/execution-id
                                       :invalid-execution-id)
      (audit-machine-explicit-merge! merge-fn)
      (audit-machine-binding-normalization! normalize)
      (check-source! checker :machine-normalization-valid-tagged-options
              '(normalize-machine-bindings
                {:role :server} {:role :server}
                (identity/command-id :command)
                (identity/execution-id :attempt)))
      (check-source! checker :machine-normalization-anonymous
              '(normalize-machine-bindings {:role :server} nil nil nil))
      ;; Non-nominal public maps can only be validated by runtime schemas.
      ;; An internal nominal object or scalar is not even a valid *input type*.
      (doseq [[label form]
              [[:wrong-command-nominal
                '(normalize-machine-bindings
                  {:role :server} nil (gesso.choreo.internal.nominal-identity/command-id :command) nil)]
               [:wrong-execution-nominal
                '(normalize-machine-bindings
                  {:role :server} nil nil (gesso.choreo.internal.nominal-identity/execution-id :attempt))]
               [:wrong-command-scalar
                '(normalize-machine-bindings {:role :server} nil "raw" nil)]
               [:wrong-execution-scalar
                '(normalize-machine-bindings {:role :server} nil nil :raw)]]]
        (check-rejection! checker label form))
      (doseq [[label form]
              [[:merge-bad-return merge-fn]
               [:normalization-bad-return normalize]]]
        (check-rejection!
         checker label
         (apply list (concat (butlast form) [:not-a-tagged-map]))
         :typed.clojure/type-mismatch-error))
      ;; Wrong-category validation is *not* distinguishable statically for
      ;; generic tagged maps. Require a production-source semantic audit.
      (expect-audit-rejection!
       (fn [mut] (audit-machine-identity-require!
                  mut :gesso.choreo.type/command-id :invalid-command-id))
       :wrong-command-schema
       (walk/postwalk-replace
        {:gesso.choreo.type/command-id :gesso.choreo.type/execution-id}
        command)
       :wrong-machine-identity-validator)
      (expect-audit-rejection!
       audit-machine-explicit-merge! :removed-conflict-guard
       (walk/postwalk-replace
        {'machine-error 'identity} merge-fn)
       :wrong-machine-identity-merge)
      (expect-audit-rejection!
       audit-machine-binding-normalization! :swapped-command-validator
       (walk/postwalk-replace
        {'require-command-id! 'require-execution-id!} normalize)
       :wrong-machine-binding-normalization)
      (expect-audit-rejection!
       audit-machine-binding-normalization! :swapped-identity-slot
       (walk/postwalk-replace
        {:command-id :execution-id} normalize)
       :wrong-machine-binding-normalization)
      (expect-audit-rejection!
       audit-machine-binding-normalization! :public-binding-validator-removed
       (walk/postwalk-replace
        {'identity/bindings 'identity} normalize)
       :wrong-machine-binding-normalization))
    true))


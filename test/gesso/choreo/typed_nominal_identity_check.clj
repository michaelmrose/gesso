(ns gesso.choreo.typed-nominal-identity-check
  "Optional Typed Clojure verification of the real nominal identity pilot.

   No replacement for defn: annotations describe ordinary functions, and
   source-backed checks verify their actual bodies. Separate native :pre
   assertions protect direct runtime calls from accidental wrong-category
   values. No production identity representation is changed by this pilot."
  (:require [clojure.java.io :as io]
            [gesso.choreo.typed-nominal-identity-pilot :as pilot]
            [gesso.choreo.typed-nominal-identity-bridge :as bridge]
            [typed.clojure :as t]))

(def ^:private pilot-namespace 'gesso.choreo.typed-nominal-identity-pilot)
(def ^:private class-prefix "gesso.choreo.typed_nominal_identity_pilot.")
(def ^:private categories '[Principal Actor Authority Host CommandId ExecutionId])
(def ^:private factories '[principal actor authority host command-id execution-id])
(def ^:private stages '[step-1 step-2 step-3 step-4 step-5 step-6 step-7])
(def ^:private config {:max-processors 1 :check-form-eval :never})

(defn- source-forms [resource-path]
  (let [source (or (io/resource resource-path)
                   (throw (ex-info "Nominal pilot source not found."
                                   {:resource resource-path})))]
    (with-open [reader (java.io.PushbackReader. (io/reader source))]
      (loop [acc []]
        (let [form (read {:read-cond :allow :features #{:clj} :eof ::done} reader)]
          (if (= ::done form)
            acc
            (recur (conj acc form))))))))

(defn- source-definition [forms name]
  (let [found (filter #(and (seq? %)
                            (= 'defn (first %))
                            (= name (second %))) forms)]
    (when-not (= 1 (count found))
      (throw (ex-info "Missing or duplicate nominal source function."
                      {:function name :count (count found)})))
    (first found)))

(defn- assert-precondition! [definition]
  ;; A hint does not enforce runtime checking. Insist on a native precondition
  ;; in every *independently invokable* stage. This audit fails closed if the
  ;; implementation changes rather than accidentally losing its guard.
  (let [args (nth definition 2 nil)
        first-body (nth definition 3 nil)
        param (first args)
        hint (when (symbol? param) (:tag (meta param)))]
    (when-not (and (vector? args)
                   (= 1 (count args))
                   (symbol? param)
                   (= 'Principal hint)
                   (= {:pre [(list 'instance? 'Principal param)]} first-body))
      (throw (ex-info "Nominal function lost its runtime entry contract."
                      {:error/kind :missing-runtime-contract
                       :function (second definition)
                       :arguments args :pre first-body})))
    true))

(defn- check! [checker label form]
  (let [result (checker form :check-config config)]
    (when (or (:ex result) (seq (:type-errors result)))
      (throw (ex-info "Valid nominal source failed static checking."
                      {:label label :type-errors (mapv str (:type-errors result))}
                      (:ex result))))
    true))

(defn- check-rejection!
  ([checker label form]
   (check-rejection! checker label form :typed.clojure/app-type-error))
  ([checker label form expected-kind]
   (let [{:keys [ex type-errors]} (checker form :check-config config)
         kinds (mapv :type-error type-errors)]
     (when-not (and (nil? ex)
                    (= [expected-kind] kinds))
       (throw (ex-info "Nominal mismatch did not fail statically."
                       {:label label :form form :expected expected-kind
                        :kinds kinds :exception (some-> ex ex-message)})))
     true)))

(defn- check-tagged-to-nominal-bridge! [checker]
  ;; These annotations are local to the opt-in checker. The public tagged
  ;; identity API and ordinary Gesso runtime do not load Typed Clojure.
  ;; Their runtime behavior remains covered by the production identity suite.
  (let [bridge-ns 'gesso.choreo.typed-nominal-identity-bridge
        class-types (mapv #(symbol (str class-prefix %)) categories)
        bridge-sources (source-forms
                        "gesso/choreo/typed_nominal_identity_bridge.cljc")
        original (source-definition bridge-sources 'tagged->nominal)]
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "canonical-keys")
                '(typed.clojure/Set typed.clojure/Any)))
    (doseq [[function signature]
            [['gesso.choreo.identity/identity?
              '[typed.clojure/Any -> typed.clojure/Bool]]
             ['gesso.choreo.identity/raw-value
              '[typed.clojure/Any -> typed.clojure/Any]]
             ['gesso.choreo.identity/kind
              '[typed.clojure/Any -> typed.clojure/Any]]]]
      (eval (list 'typed.clojure/ann function
                  (list 'typed.clojure/IFn signature))))
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "tagged->nominal")
                (list 'typed.clojure/IFn
                      [(list 'typed.clojure/Map
                             'typed.clojure/Any 'typed.clojure/Any)
                       '-> (cons 'typed.clojure/U class-types)])))
    (binding [*ns* (the-ns bridge-ns)]
      (check! checker :actual-tagged-to-nominal-body original)
      ;; A generic tagged map is not itself a nominal identity. The return
      ;; type must not silently widen to Any if the implementation regresses.
      (let [broken (apply list (concat (butlast original)
                                       [:not-a-nominal]))]
        (check-rejection! checker :wrong-tagged-to-nominal-return
                          broken :typed.clojure/type-mismatch-error)))
    true))

(defn -main [& _]
  ;; Use the actual classes, not structural maps. Register the six datatypes
  ;; with the Typed Clojure checker, including the fields' real names.
  (doseq [category categories]
    (eval (list 'typed.clojure/ann-datatype
                (symbol (str class-prefix category))
                '[raw :- typed.clojure/Any extensions :- typed.clojure/Any])))
  ;; The annotations are optional and only occur in this checker harness.
  ;; Production and downstream Clojure never need a Typed Clojure runtime.
  (doseq [[category factory] (map vector categories factories)]
    (eval (list 'typed.clojure/ann
                (symbol (str pilot-namespace) (str factory))
                (list 'typed.clojure/IFn
                      ['typed.clojure/Any '-> (symbol (str class-prefix category))]))))
  (doseq [name stages]
    (eval (list 'typed.clojure/ann
                (symbol (str pilot-namespace) (str name))
                (list 'typed.clojure/IFn
                      [(symbol (str class-prefix 'Principal))
                       '->
                       (symbol (str class-prefix 'Principal))]))))
  (let [checker (requiring-resolve 'typed.clj.checker/check-form-info)
        forms (source-forms "gesso/choreo/typed_nominal_identity_pilot.cljc")
        source-ns (the-ns pilot-namespace)]
    (binding [*ns* source-ns]
      (doseq [name (concat factories stages)]
        (let [source (source-definition forms name)]
          (check! checker [:original-body name] source)
          (when (some #{name} stages)
            (assert-precondition! source))))
      ;; An original call to an ordinary constructor has a known nominal type.
      ;; Each stage must accept a Principal and reject every other class,
      ;; plus an ordinary map and nil, with an actual checker type error.
      (doseq [name stages]
        (check! checker [:principal-call name] (list name '(principal :id)))
        (doseq [factory (rest factories)]
          (check-rejection! checker [:wrong-category name factory]
                            (list name (list factory :id))))
        (check-rejection! checker [:plain-map name]
                          (list name {:raw :id}))
        (check-rejection! checker [:nil name]
                          (list name nil)))
      ;; Mutation testing: the original body is checked, but a broken return
      ;; in an otherwise unchanged function must produce a static error.
      (let [original (source-definition forms 'step-7)
            wrong (apply list (concat (butlast original) [:not-a-principal]))]
        (check-rejection! checker :wrong-source-return wrong
                          :typed.clojure/type-mismatch-error))
      ;; A future refactor removing a stage guard must fail this gate even if
      ;; all checked callers are well-typed.
      (let [original (source-definition forms 'step-4)
            missing-guard (apply list (concat (take 3 original)
                                              (drop 4 original)))
            kind (try (assert-precondition! missing-guard) nil
                      (catch clojure.lang.ExceptionInfo e
                        (:error/kind (ex-data e))))]
        (when-not (= :missing-runtime-contract kind)
          (throw (ex-info "Missing runtime guard was not detected."
                          {:error/kind kind})))))
    (check-tagged-to-nominal-bridge! checker)
    (println "Nominal static gate passed: 6 constructors, 7 stage bodies,")
    (println "1 tagged-to-nominal bridge body, 7 valid calls, 49 invalid")
    (println "calls, 3 mutation checks (including bridge return type).")
    :ok))

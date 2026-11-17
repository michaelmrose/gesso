(ns gesso.choreo.typed-nominal-identity-check
  "Optional Typed Clojure verification of the real nominal identity pilot.

   No replacement for defn: annotations describe ordinary functions, and
   source-backed checks verify their actual bodies. Separate native :pre
   assertions protect direct runtime calls from accidental wrong-category
   values. No production identity representation is changed by this pilot."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
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
                            (#{'defn 'defn-} (first %))
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

(def ^:private correspondence
  ;; This is the intended *semantic* correspondence; static map types alone
  ;; cannot distinguish which tagged constructor corresponds to each class.
  [{:kind :principal :class 'gesso.choreo.typed_nominal_identity_pilot.Principal
    :nominal 'nominal/->Principal :tagged 'tagged/principal}
   {:kind :actor :class 'gesso.choreo.typed_nominal_identity_pilot.Actor
    :nominal 'nominal/->Actor :tagged 'tagged/actor}
   {:kind :authority :class 'gesso.choreo.typed_nominal_identity_pilot.Authority
    :nominal 'nominal/->Authority :tagged 'tagged/authority}
   {:kind :host :class 'gesso.choreo.typed_nominal_identity_pilot.Host
    :nominal 'nominal/->Host :tagged 'tagged/host}
   {:kind :command :class 'gesso.choreo.typed_nominal_identity_pilot.CommandId
    :nominal 'nominal/->CommandId :tagged 'tagged/command-id}
   {:kind :execution :class 'gesso.choreo.typed_nominal_identity_pilot.ExecutionId
    :nominal 'nominal/->ExecutionId :tagged 'tagged/execution-id}])

(defn- audit-forward-correspondence! [definition]
  ;; Audit the *actual original case expression*, not a separately authored
  ;; reconstruction that could keep passing after production pilot drift.
  (let [let-form (last definition)
        case-form (when (and (seq? let-form) (= 'let (first let-form)))
                    (last let-form))
        expected (vec (mapcat (fn [{:keys [kind nominal]}]
                                [kind (list nominal 'raw 'extensions)])
                              correspondence))]
    (when-not (and (seq? case-form)
                   (= 'case (first case-form))
                   (= '(tagged/kind reference) (second case-form))
                   (= expected (vec (drop 2 case-form))))
      (throw (ex-info "Forward identity category correspondence changed."
                      {:error/kind :wrong-forward-correspondence})))
    true))

(defn- audit-reverse-correspondence! [definition]
  ;; Cond/instance? is not a dependent-type proof: each arm returns the same
  ;; structural map type. Audit every class-to-constructor association and
  ;; both hinted field reads, including a fail-closed final error branch.
  (let [cond-form (last definition)
        branches (when (and (seq? cond-form)
                            (= 'cond (first cond-form)))
                   (vec (partition-all 2 (rest cond-form))))
        body-ok?
        (and (= 7 (count branches))
             (every?
              true?
              (map (fn [{:keys [class tagged]} [pred body]]
                     (and (= (list 'instance? class 'reference) pred)
                          (= 'restore-tagged (first body))
                          (= tagged (second body))
                          (= 4 (count body))
                          (every?
                           true?
                           (map (fn [field expr]
                                  (and (= (list field 'reference) expr)
                                       (= class (:tag (meta (second expr))))))
                                ['.-raw '.-extensions]
                                (drop 2 body)))))
                   correspondence (take 6 branches)))
             (= :else (first (last branches)))
             (let [error (second (last branches))]
               (and (seq? error) (= 'throw (first error)))))]
    (when-not body-ok?
      (throw (ex-info "Reverse identity category correspondence changed."
                      {:error/kind :wrong-reverse-correspondence})))
    true))

(defn- expect-audit-rejection! [audit label changed expected-kind]
  (let [kind (try (audit changed) nil
                  (catch clojure.lang.ExceptionInfo e
                    (:error/kind (ex-data e))))]
    (when-not (= expected-kind kind)
      (throw (ex-info "Semantic category mutation escaped bridge audit."
                      {:label label :expected expected-kind :actual kind})))
    true))

(defn- check-bridge! [checker]
  (let [bridge-ns 'gesso.choreo.typed-nominal-identity-bridge
        class-types (mapv #(symbol (str class-prefix %)) categories)
        map-type '(typed.clojure/Map typed.clojure/Any typed.clojure/Any)
        bridge-sources (source-forms
                        "gesso/choreo/typed_nominal_identity_bridge.cljc")
        forward (source-definition bridge-sources 'tagged->nominal)
        restore (source-definition bridge-sources 'restore-tagged)
        reverse (source-definition bridge-sources 'nominal->tagged)]
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
    (doseq [factory '[principal actor authority host command-id execution-id]]
      (eval (list 'typed.clojure/ann
                  (symbol "gesso.choreo.identity" (str factory))
                  (list 'typed.clojure/IFn ['typed.clojure/Any '-> map-type]))))
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "tagged->nominal")
                (list 'typed.clojure/IFn
                      [map-type '-> (cons 'typed.clojure/U class-types)])))
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "restore-tagged")
                (list 'typed.clojure/IFn
                      [(list 'typed.clojure/IFn
                             ['typed.clojure/Any '-> map-type])
                       'typed.clojure/Any
                       'typed.clojure/Any
                       '-> map-type])))
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "nominal->tagged")
                (list 'typed.clojure/IFn
                      [(cons 'typed.clojure/U class-types) '-> map-type])))
    (binding [*ns* (the-ns bridge-ns)]
      (doseq [[label form] [[:tagged-to-nominal forward]
                            [:restore-tagged restore]
                            [:nominal-to-tagged reverse]]]
        (check! checker [:actual-bridge-body label] form))
      (audit-forward-correspondence! forward)
      (audit-reverse-correspondence! reverse)
      (doseq [constructor '[principal actor authority host command-id execution-id]]
        (check! checker [:valid-reverse-call constructor]
                (list 'nominal->tagged (list (symbol "nominal" (str constructor)) :id))))
      (doseq [bad [nil {:raw :id} :role]]
        (check-rejection! checker [:invalid-reverse-call bad]
                          (list 'nominal->tagged bad)))
      (doseq [[label original bogus expected-error]
              [[:forward-return forward :not-a-nominal
                :typed.clojure/type-mismatch-error]
               [:reverse-return reverse :not-a-tagged-map
                :typed.clojure/type-mismatch-error]]]
        (check-rejection! checker label
                          (apply list (concat (butlast original) [bogus]))
                          expected-error))
      ;; Mutations that *preserve* type signatures but violate semantics:
      ;; neither the union nor the Map return can recognize these swaps.
      (expect-audit-rejection!
       audit-forward-correspondence! :forward-category-swap
       (walk/postwalk-replace {'nominal/->Principal 'nominal/->Actor} forward)
       :wrong-forward-correspondence)
      (expect-audit-rejection!
       audit-reverse-correspondence! :reverse-category-swap
       (walk/postwalk-replace {'tagged/principal 'tagged/actor} reverse)
       :wrong-reverse-correspondence)
      (expect-audit-rejection!
       audit-reverse-correspondence! :reverse-field-hint-removed
       (walk/postwalk
        (fn [node] (if (and (symbol? node)
                            (= 'reference node)
                            (:tag (meta node)))
                     (with-meta node nil)
                     node))
        reverse)
       :wrong-reverse-correspondence))
    true))

(defn- audit-binding-helper! [definition conversion role-predicate]
  ;; A structural map type does not associate its keys with individual
  ;; nominal identity categories. Audit the exact slot-preserving conversion
  ;; path, while separate runtime tests exercise the 6x6 category matrix.
  (let [body (last definition)
        outer-if (when (and (seq? body) (= 'assoc (first body))
                            (= ['acc 'key] (vec (take 2 (rest body)))))
                   (nth body 3 nil))
        role-branch (when (and (seq? outer-if) (= 'if (first outer-if)))
                      (nth outer-if 2 nil))
        identity-branch (when (and (seq? outer-if) (= 'if (first outer-if)))
                          (nth outer-if 3 nil))]
    (when-not (and (= '(= key :role) (second outer-if))
                   (= 'if (first role-branch))
                   (= '(keyword? value) (second role-branch))
                   (= 'value (nth role-branch 2 nil))
                   (= 'throw (first (nth role-branch 3 nil)))
                   (= 'if (first identity-branch))
                   (= role-predicate (second identity-branch))
                   (= conversion
                      (if (= conversion '(tagged->nominal value))
                        (nth identity-branch 2 nil)
                        (nth identity-branch 3 nil)))
                   (= 'throw
                      (first (if (= conversion '(tagged->nominal value))
                               (nth identity-branch 3 nil)
                               (nth identity-branch 2 nil)))))
      (throw (ex-info "Binding entry conversion bypasses a required slot guard."
                      {:error/kind :wrong-binding-entry
                       :function (second definition)})))
    true))

(defn- audit-reverse-binding-assembly! [reverse]
  ;; Keep the public validator on the nominal->tagged egress boundary.
  (when-not (= '(tagged/bindings
                 (reduce-kv tagged-binding-entry {} bindings))
               (last reverse))
    (throw (ex-info "Reverse binding adapter lost its public validator."
                    {:error/kind :wrong-binding-assembly :direction :reverse})))
  true)

(defn- check-bindings! [checker]
  ;; The forward conversion is verified separately against its precise HMap
  ;; return type; avoid a second broad annotation that would erase precision.
  (let [bridge-ns 'gesso.choreo.typed-nominal-identity-bridge
        class-types (mapv #(symbol (str class-prefix %)) categories)
        tagged-reference '(typed.clojure/Map typed.clojure/Any typed.clojure/Any)
        nominal-union (cons 'typed.clojure/U class-types)
        nominal-value (list 'typed.clojure/U 'typed.clojure/Kw nominal-union)
        tagged-value (list 'typed.clojure/U 'typed.clojure/Kw tagged-reference)
        tagged-map (list 'typed.clojure/Map 'typed.clojure/Kw tagged-value)
        nominal-map (list 'typed.clojure/Map 'typed.clojure/Kw nominal-value)
        sources (source-forms "gesso/choreo/typed_nominal_identity_bridge.cljc")
        reverse-helper (source-definition sources 'tagged-binding-entry)
        reverse (source-definition sources 'bindings->tagged)]
    ;; This existing runtime function validates the sparse map; its annotation
    ;; describes the broad result used to analyze the reverse body.
    (eval (list 'typed.clojure/ann 'gesso.choreo.identity/bindings
                (list 'typed.clojure/IFn ['typed.clojure/Any '-> tagged-map])))
    (doseq [[name signature]
            [['tagged-binding-entry [tagged-map 'typed.clojure/Kw nominal-value
                                    '-> tagged-map]]
             ['bindings->tagged [nominal-map '-> tagged-map]]]]
      (eval (list 'typed.clojure/ann
                  (symbol (str bridge-ns) (str name))
                  (list 'typed.clojure/IFn signature))))
    (binding [*ns* (the-ns bridge-ns)]
      (doseq [[label source]
              [[:reverse-entry reverse-helper] [:reverse-bindings reverse]]]
        (check! checker [:actual-binding-body label] source))
      (audit-binding-helper! reverse-helper '(nominal->tagged value)
                             '(keyword? value))
      (audit-reverse-binding-assembly! reverse)
      (check! checker :valid-sparse-nominal-binding-map
              '(bindings->tagged {:principal (nominal/principal :person)
                                  :role :browser}))
      (doseq [[label form]
              [[:reverse-tagged-in-map
                '(bindings->tagged {:principal (tagged/principal :person)})]
               [:reverse-string '(bindings->tagged "bad")]
               [:reverse-nil '(bindings->tagged nil)]]]
        (check-rejection! checker label form))
      (check-rejection! checker :reverse-binding-return
                        (apply list (concat (butlast reverse) [:not-a-map]))
                        :typed.clojure/type-mismatch-error)
      (expect-audit-rejection!
       audit-reverse-binding-assembly! :reverse-validation-removed
       (walk/postwalk-replace {'tagged/bindings 'identity} reverse)
       :wrong-binding-assembly)
      (expect-audit-rejection!
       (fn [candidate] (audit-binding-helper! candidate
                      '(nominal->tagged value) '(keyword? value)))
       :reverse-converter-swapped
       (walk/postwalk-replace {'nominal->tagged 'tagged->nominal} reverse-helper)
       :wrong-binding-entry))
    true))

(defn- check-heterogeneous-nominal-bindings! [checker]
  ;; Verify the actual ordinary defn against a *closed, sparse HMap* input.
  ;; Each optional key has its own nominal type. No replacement for defn or
  ;; source changes to the downstream map-based API are needed.
  ;;
  ;; The output is intentionally still a general tagged map. The existing
  ;; reduce-kv implementation does not statically derive a per-key HMap
  ;; result, and simply annotating it as such would invent a guarantee.
  (let [ns-sym 'gesso.choreo.typed-nominal-identity-bridge
        class-types (mapv #(symbol (str class-prefix %)) categories)
        field-types (into {:role 'typed.clojure/Kw}
                          (map vector
                               [:principal :actor :authority :host
                                :command-id :execution-id]
                               class-types))
        hmap (list 'typed.clojure/HMap :optional field-types :complete? true)
        tagged-map '(typed.clojure/Map
                     typed.clojure/Kw
                     (typed.clojure/U typed.clojure/Kw
                                      (typed.clojure/Map
                                       typed.clojure/Any typed.clojure/Any)))
        forms (source-forms "gesso/choreo/typed_nominal_identity_bridge.cljc")
        original (source-definition forms 'bindings->tagged)
        slots [:role :principal :actor :authority :host :command-id
               :execution-id]
        constructors ['nominal/principal 'nominal/actor 'nominal/authority
                      'nominal/host 'nominal/command-id 'nominal/execution-id]
        values (into {:role :server}
                     (map vector (rest slots)
                          (map #(list % :same-raw-id) constructors)))
        checked-form (fn [entries] (list 'bindings->tagged entries))]
    (eval (list 'typed.clojure/ann
                (symbol (str ns-sym) "bindings->tagged")
                (list 'typed.clojure/IFn [hmap '-> tagged-map])))
    (binding [*ns* (the-ns ns-sym)]
      ;; This verifies the *original source body* accepts the stronger input
      ;; declaration, not just that invented calls can be checked against it.
      (check! checker :actual-hmap-binding-adapter original)
      ;; All 2^7 sparse subsets: omission is allowed, nil in a present slot
      ;; isn't. One checker invocation covers every positive expression.
      (let [valid-expressions
            (mapv (fn [mask]
                    (checked-form
                     (into {}
                           (for [[index slot] (map-indexed vector slots)
                                 :when (bit-test mask index)]
                             [slot (get values slot)]))))
                  (range 128))]
        (check! checker :all-128-sparse-hmap-subsets valid-expressions))
      ;; For *every* of the six nominal slots, supplying any of the other
      ;; five nominal classes must be a *static* application error, even
      ;; though these values would all fit the old broad union map type.
      (doseq [[slot expected-ctor] (map vector (rest slots) constructors)
              actual-ctor constructors
              :when (not= expected-ctor actual-ctor)]
        (check-rejection!
         checker [:wrong-nominal-category slot actual-ctor]
         (checked-form {slot (list actual-ctor :same-raw-id)})))
      (doseq [slot (rest slots)]
        (check-rejection! checker [:nil-nominal-slot slot]
                          (checked-form {slot nil})))
      (check-rejection! checker :nominal-in-role
                        (checked-form {:role '(nominal/principal :id)}))
      (check-rejection! checker :unknown-binding-key
                        (checked-form {:unsupported :value}))
      (check-rejection! checker :nominal-from-unchecked-map
                        (checked-form {:principal {:raw :id}}))
      ;; A signature swapped in the wrong direction would reject this real
      ;; valid program; the positive suite detects that regression.
      (check! checker :principal-and-actor-distinct
              (checked-form {:principal '(nominal/principal :id)
                             :actor '(nominal/actor :id)})))
    true))

(defn- audit-canonical-output-validation! [definition]
  ;; A closed optional HMap cannot prove all present keys survive, nor that
  ;; untyped input was validated. Insist on the public admission
  ;; boundary; exhaustive runtime equivalence checks key preservation.
  (let [body (last definition)
        bindings (when (and (seq? body) (= 'let (first body)))
                   (second body))
        validation (when (vector? bindings)
                     (second bindings))]
    (when-not (= '(tagged/bindings bindings) validation)
      (throw (ex-info "Canonical binding converter bypasses public validation."
                      {:error/kind :missing-canonical-binding-validation})))
    true))

(defn- check-canonical-nominal-output! [checker]
  ;; The single canonical conversion uses fixed-key if-let construction,
  ;; allowing the checker to prove category-specific optional output slots.
  (let [bridge-ns 'gesso.choreo.typed-nominal-identity-bridge
        class-types (mapv #(symbol (str class-prefix %)) categories)
        hmap (list 'typed.clojure/HMap
                   :optional (into {:role 'typed.clojure/Kw}
                                   (map vector
                                        [:principal :actor :authority :host
                                         :command-id :execution-id]
                                        class-types))
                   :complete? true)
        tagged-map '(typed.clojure/Map
                     typed.clojure/Kw
                     (typed.clojure/U typed.clojure/Kw
                                      (typed.clojure/Map
                                       typed.clojure/Any typed.clojure/Any)))
        sources (source-forms "gesso/choreo/typed_nominal_identity_bridge.cljc")
        source (source-definition sources 'bindings->nominal)]
    (eval (list 'typed.clojure/ann
                (symbol (str bridge-ns) "bindings->nominal")
                (list 'typed.clojure/IFn [tagged-map '-> hmap])))
    (binding [*ns* (the-ns bridge-ns)]
      (check! checker :actual-precise-nominal-output-body source)
      (audit-canonical-output-validation! source)
      (check! checker :valid-precise-nominal-output-call
              '(bindings->nominal
                {:principal (tagged/principal :person)
                 :actor (tagged/actor :member)
                 :role :server}))
      ;; A client may read the type of an optional entry only after checking
      ;; presence. The value is statically Principal rather than the old union.
      (check! checker :canonical-output-flows-to-stage-one
              '(let [result (bindings->nominal
                             {:principal (tagged/principal :person)})]
                 (if-let [p (:principal result)]
                   (nominal/step-1 p)
                   nil)))
      ;; A deliberately wrong consumer type fails at the *next* function,
      ;; demonstrating that category precision is propagated downstream.
      (check-rejection! checker :canonical-output-rejects-wrong-consumer
                        '(let [result (bindings->nominal
                                       {:actor (tagged/actor :person)})]
                           (if-let [actor (:actor result)]
                             (nominal/step-1 actor)
                             nil)))
      (expect-audit-rejection!
       audit-canonical-output-validation! :canonical-public-validator-removed
       (walk/postwalk-replace {'tagged/bindings 'identity} source)
       :missing-canonical-binding-validation)
      (let [modified (walk/postwalk
                      (fn [node]
                        (if (= '(assoc out :principal nominal-value) node)
                          '(assoc out :actor nominal-value)
                          node))
                      source)]
        (when (= modified source)
          (throw (ex-info "Canonical source mutation did not modify its target."
                          {:error/kind :missing-mutation-target})))
        (check-rejection! checker :canonical-output-slot-swap
                          modified :typed.clojure/type-mismatch-error)))
    true))

(defn -main [& _]
  ;; Use the actual classes, not structural maps. Register the six datatypes
  ;; with the Typed Clojure checker, including the fields' real names.
  (doseq [category categories]
    (eval (list 'typed.clojure/ann-datatype
                (symbol (str class-prefix category))
                '[raw :- typed.clojure/Any extensions :- (typed.clojure/Map typed.clojure/Any typed.clojure/Any)])))
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
    (check-bridge! checker)
    (check-bindings! checker)
    (check-heterogeneous-nominal-bindings! checker)
    (check-canonical-nominal-output! checker)
    (println "Nominal static gate passed: 6 constructors, 7 stage bodies,")
    (println "6 bridge bodies including canonical sparse output,")
    (println "static source, category, return, and mutation checks passed.")
    (println "Closed sparse nominal HMap: 128 valid subsets, 39 invalid")
    (println "per-slot category/key cases; original adapter body checked.")
    (println "Canonical output HMap: real body checked, seven optional slots;")
    (println "wrong downstream consumer and slot-swap mutations rejected.")
    :ok))

(ns ^{:typed.clojure true} gesso.choreo.typed-binding-pilot
  "Opt-in source-backed type checks of Choreo's identity-binding dispatch.

   The input to this boundary is application-supplied data, not a typed
   downstream program. Each of the seven original `validate-binding` case
   branches is checked against its declared output category, alongside the
   original validator's broader body and the role helpers.

   Opaque raw identity values are intentionally not assumed to be strings.
   Valid identity references may retain extra keys. Their HMap types must
   remain OPEN: a closed HMap (:complete? true) is unsound as a refinement
   of identity?/kind?, because production identities accept extra keys.
   We test this with the real predicates and Typed Clojure's subtyping.
   All six original category-predicate bodies are checked for Boolean results
   and audited against the original kind dispatcher. Their occurrence-typing
   contracts are POSITIVE-ONLY: a true result can refine to the tagged HMap,
   but a false result must NOT exclude the shape, since nil or nested raw
   values can have that shape and still fail identity?. Prior (Pred HMap)
   annotations incorrectly declared both directions. Positive refinement
   claims remain trusted until source-backed semantic proofs establish them.
   This does not certify `bindings`, authorization, or full admission."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.choreo.identity :as identity]
            [gesso.choreo.typed-identity-pilot]
            [typed.clojure :as t]))

;; Unlike the string-restricted earlier pilot, these representations preserve
;; the deliberately opaque identity raw-value contract of production Gesso.
;; The tagged maps are OPEN because identity? requires keys but does not reject
;; extensions. The HMap is a safe overapproximation of admitted identities;
;; it DOES include malformed maps with nil or nested identity raw values.
;; A two-way Pred HMap would therefore be UNSOUND in its negative branch.
(t/defalias PrincipalRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :principal)
           :gesso.choreo.identity/value t/Any}
          :complete? false))
(t/defalias ActorRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :actor)
           :gesso.choreo.identity/value t/Any}
          :complete? false))
(t/defalias AuthorityRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :authority)
           :gesso.choreo.identity/value t/Any}
          :complete? false))
(t/defalias HostRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :host)
           :gesso.choreo.identity/value t/Any}
          :complete? false))
(t/defalias CommandRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :command)
           :gesso.choreo.identity/value t/Any}
          :complete? false))
(t/defalias ExecutionRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :execution)
           :gesso.choreo.identity/value t/Any}
          :complete? false))

;; Trusted one-way predicate refinements: success narrows to the open HMap,
;; while failure cannot exclude it. The original predicates can reject maps
;; that fit the HMap shape (nil or nested raw values). Unlike (Pred HMap), the
;; :else tt filter makes no unsound negative refinement. We check this with
;; real checker counterexamples below; positive implication remains trusted.
(t/ann identity/role? (t/Pred t/Kw))
(t/ann identity/require-role [t/Any -> t/Kw])
(t/ann identity/kind [t/Any -> t/Any])
(t/ann identity/kind? [t/Kw t/Any -> t/Bool])
(t/ann identity/principal?
       [t/Any -> t/Bool :filters {:then (is PrincipalRef 0) :else tt}])
(t/ann identity/actor?
       [t/Any -> t/Bool :filters {:then (is ActorRef 0) :else tt}])
(t/ann identity/authority?
       [t/Any -> t/Bool :filters {:then (is AuthorityRef 0) :else tt}])
(t/ann identity/host?
       [t/Any -> t/Bool :filters {:then (is HostRef 0) :else tt}])
(t/ann identity/command-id?
       [t/Any -> t/Bool :filters {:then (is CommandRef 0) :else tt}])
(t/ann identity/execution-id?
       [t/Any -> t/Bool :filters {:then (is ExecutionRef 0) :else tt}])

;; The original multi-key dispatcher has a broad, honest whole-form type.
;; Its key-specific value/result relations are checked per original case arm.
(t/ann identity/validate-binding [t/Kw t/Any -> t/Any])

(t/tc-ignore
 (defn -main [& _]
   (let [check-form-info (requiring-resolve 'typed.clj.checker/check-form-info)
         check-ns (requiring-resolve 'typed.clj.checker/check-ns)
         config {:max-processors 1 :check-form-eval :never}
         source (or (io/resource "gesso/choreo/identity.cljc")
                    (throw (ex-info "Original identity source unavailable." {})))
         source-forms
         (with-open [reader (java.io.PushbackReader. (io/reader source))]
           (loop [result []]
             (let [form (read {:read-cond :allow
                               :features #{:clj}
                               :eof ::end} reader)]
               (if (= ::end form)
                 result
                 (recur (conj result form))))))
         source-function
         (fn [name]
           (let [found (filter #(and (seq? %)
                                     (#{'defn 'defn-} (first %))
                                     (= name (second %)))
                               source-forms)]
             (when-not (= 1 (count found))
               (throw (ex-info "Missing or ambiguous source function."
                               {:name name :count (count found)})))
             (first found)))
         check-form (fn [name form]
                      (let [{:keys [ex type-errors ret]}
                            (check-form-info form :check-config config)]
                        (when (or ex (seq type-errors) (nil? ret))
                          (throw (ex-info "Original source failed type check."
                                          {:case name
                                           :exception (some-> ex ex-message)
                                           :errors (mapv str type-errors)})))
                        (println "Checked original source:" name)))
         check-negative
         (fn [name original mutated]
           (when (= original mutated)
             (throw (ex-info "Mutation left original unchanged." {:case name})))
           (let [{:keys [ex type-errors]}
                 (check-form-info mutated :check-config config)]
             (when (or ex
                       (not (seq type-errors))
                       (not-every? #(= :typed.clojure/type-mismatch-error
                                       (:type-error %))
                                   type-errors))
               (throw (ex-info "Expected static type mismatch, not exception."
                               {:case name
                                :exception (some-> ex ex-message)
                                :errors (mapv str type-errors)})))
             (println "Rejected static type-invalid source mutation:" name)))
         identity-cases
         [{:key :principal :predicate 'principal? :wrong 'actor?
           :type 'gesso.choreo.typed-binding-pilot/PrincipalRef}
          {:key :actor :predicate 'actor? :wrong 'authority?
           :type 'gesso.choreo.typed-binding-pilot/ActorRef}
          {:key :authority :predicate 'authority? :wrong 'host?
           :type 'gesso.choreo.typed-binding-pilot/AuthorityRef}
          {:key :host :predicate 'host? :wrong 'command-id?
           :type 'gesso.choreo.typed-binding-pilot/HostRef}
          {:key :command-id :predicate 'command-id? :wrong 'execution-id?
           :type 'gesso.choreo.typed-binding-pilot/CommandRef}
          {:key :execution-id :predicate 'execution-id? :wrong 'principal?
           :type 'gesso.choreo.typed-binding-pilot/ExecutionRef}]
         binding-definition (source-function 'validate-binding)
         case-form (last binding-definition)
         _ (when-not (and (= 'case (first case-form))
                          (= 'key (second case-form))
                          (= 14 (count (drop 2 case-form))))
             (throw (ex-info "Binding dispatcher shape has changed." {})))
         branches (apply hash-map (drop 2 case-form))
         expected-keys #{:role :principal :actor :authority :host
                         :command-id :execution-id}
         _ (when-not (= expected-keys (set (keys branches))
                        identity/binding-keys)
             (throw (ex-info "Identity binding branch coverage changed." {})))
         typed-branch
         (fn [expr output-type]
           (list 'typed.clojure/ann-form
                 (list 'fn ['value] expr)
                 (list 'typed.clojure/IFn
                       ['typed.clojure/Any '-> output-type])))
         kind-predicates
         [[:principal 'principal? identity/principal identity/principal?]
          [:actor 'actor? identity/actor identity/actor?]
          [:authority 'authority? identity/authority identity/authority?]
          [:host 'host? identity/host identity/host?]
          [:command 'command-id? identity/command-id identity/command-id?]
          [:execution 'execution-id? identity/execution-id identity/execution-id?]]]
     (when-not (= :ok (check-ns 'gesso.choreo.typed-binding-pilot
                               :check-config config))
       (throw (ex-info "Binding pilot namespace did not type-check." {})))
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [name ['identity? 'kind 'role? 'require-role 'kind?
                     'validate-binding]]
         (check-form name (source-function name)))
       ;; The original category predicates merely delegate to kind?. The
       ;; checker proves Boolean results under a truthful broad annotation;
       ;; their stronger Pred refinements remain separately trusted.
       (doseq [[expected-kind predicate-name] kind-predicates]
         (let [original (source-function predicate-name)
               _ (when-not (and (= 'defn (first original))
                                (= '[value] (nth original 2 nil))
                                (= 4 (count original)))
                   (throw (ex-info "Identity predicate source shape changed."
                                   {:predicate predicate-name :form original})))
               body (last original)
               expected (list 'kind? expected-kind 'value)
               checked (typed-branch body 'typed.clojure/Bool)
               bad (typed-branch 42 'typed.clojure/Bool)]
           ;; A swapped category literal is still a well-typed Boolean
           ;; expression. Audit this source-level semantic relationship
           ;; separately; do not pass it off as static type discrimination.
           (when-not (= expected body)
             (throw (ex-info "Identity predicate's original category mapping changed."
                             {:predicate predicate-name
                              :expected expected :actual body})))
           (check-form predicate-name checked)
           (check-negative predicate-name checked bad)))
       (let [original (source-function 'kind?)
             mutated (apply list (concat (butlast original) [42]))]
         (check-negative 'kind?-wrong-return original mutated))
       (let [role-branch (:role branches)]
         (when-not (= '(require-role value) role-branch)
           (throw (ex-info "Role branch no longer delegates to require-role." {})))
         (let [original (typed-branch role-branch 'typed.clojure/Kw)
               mutated (typed-branch '(command-id "role") 'typed.clojure/Kw)]
           (check-form :role original)
           (check-negative :role original mutated)))
       (doseq [{:keys [key predicate wrong type]} identity-cases]
         (let [branch (get branches key)
               guard (list predicate 'value)
               replacement (list wrong 'value)]
           (when-not (and (seq? branch)
                          (= 'if (first branch))
                          (= guard (second branch))
                          (= 'value (nth branch 2 nil))
                          (seq? (nth branch 3 nil)))
             (throw (ex-info "Unexpected identity binding branch shape."
                             {:key key :branch branch})))
           (let [original (typed-branch branch type)
                 mutated (typed-branch
                          (walk/postwalk #(if (= guard %) replacement %) branch)
                          type)]
             (check-form key original)
             (check-negative key original mutated)))))
     ;; Executing the real public validator covers the entire sparse-map
     ;; route; isolated branch checking alone cannot establish this behavior.
     (let [valid {:role :browser
                  :principal (identity/principal 17)
                  :actor (identity/actor "actor")
                  :authority (identity/authority "authority")
                  :host (identity/host "host")
                  :command-id (identity/command-id 83)
                  :execution-id (identity/execution-id "execution")}
           wrong {:role (identity/command-id :role)
                  :principal (identity/actor 17)
                  :actor (identity/authority "actor")
                  :authority (identity/host "authority")
                  :host (identity/command-id "host")
                  :command-id (identity/execution-id 83)
                  :execution-id (identity/principal "execution")}]
       (doseq [[k v] valid]
         (when-not (= {k v} (identity/bindings {k v}))
           (throw (ex-info "Valid runtime binding was not preserved." {:key k}))))
       (doseq [[k v] wrong]
         (let [error-kind (try
                            (identity/bindings {k v})
                            nil
                            (catch clojure.lang.ExceptionInfo e
                              (:error/kind (ex-data e))))]
           (when-not (= (if (= k :role) :invalid-role :invalid-binding)
                        error-kind)
             (throw (ex-info "Wrong runtime identity was not rejected."
                             {:key k :actual error-kind}))))))
     ;; Open-map refinements are essential to sound category predicates:
     ;; identity? recognizes maps with additional keys, and bindings preserves
     ;; them. For every kind, verify the original predicate accepts the
     ;; extension and the checker accepts the corresponding open HMap. We also
     ;; require the intentionally CLOSED HMap to reject the same static value.
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [[kind name constructor predicate] kind-predicates]
         (let [expected-type (symbol (str 'gesso.choreo.typed-binding-pilot)
                                     (case kind
                                       :principal "PrincipalRef"
                                       :actor "ActorRef"
                                       :authority "AuthorityRef"
                                       :host "HostRef"
                                       :command "CommandRef"
                                       :execution "ExecutionRef"))
               reference (assoc (constructor [17 :opaque]) :extension :present)
               literal {:gesso.choreo.identity/type :gesso.choreo.identity/ref
                        :gesso.choreo.identity/kind kind
                        :gesso.choreo.identity/value [17 :opaque]
                        :extension :present}
               open-form (list 'typed.clojure/ann-form literal expected-type)
               closed-type (list 'typed.clojure/HMap
                                 :mandatory
                                 {:gesso.choreo.identity/type
                                  '(typed.clojure/Val :gesso.choreo.identity/ref)
                                  :gesso.choreo.identity/kind
                                  (list 'typed.clojure/Val kind)
                                  :gesso.choreo.identity/value 'typed.clojure/Any}
                                 :complete? true)
               closed-form (list 'typed.clojure/ann-form literal closed-type)]
           (when-not (and (identity/identity? reference)
                          (predicate reference)
                          (= kind (identity/kind reference))
                          (= {(case kind
                                   :command :command-id
                                   :execution :execution-id
                                   kind) reference}
                             (identity/bindings {(case kind
                                                   :command :command-id
                                                   :execution :execution-id
                                                   kind)
                                                 reference})))
             (throw (ex-info "Gesso did not preserve an extended identity."
                             {:kind kind :identity name})))
           (check-form [:open-identity name] open-form)
           (check-negative [:closed-identity name] open-form closed-form))))
     ;; A tagged HMap shape is not sufficient evidence of admission. All
     ;; six aliases allow nil or nested raw values even though the *runtime*
     ;; predicates reject them. The prior two-way (Pred HMap) annotations
     ;; incorrectly made their false branches unreachable for HMap inputs.
     ;; These checks fail if our one-way :else tt refinements regress to Pred.
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [[kind name _ predicate] kind-predicates]
         (let [type-name (symbol (str 'gesso.choreo.typed-binding-pilot)
                                 (case kind
                                   :principal "PrincipalRef"
                                   :actor "ActorRef"
                                   :authority "AuthorityRef"
                                   :host "HostRef"
                                   :command "CommandRef"
                                   :execution "ExecutionRef"))
               key (case kind :command :command-id
                              :execution :execution-id kind)
               raw-nil {:gesso.choreo.identity/type :gesso.choreo.identity/ref
                        :gesso.choreo.identity/kind kind
                        :gesso.choreo.identity/value nil}
               raw-nested (assoc raw-nil :gesso.choreo.identity/value
                                       (identity/principal :nested))
               branch-test
               (fn [false-result]
                 (list 'typed.clojure/ann-form
                       (list 'fn ['value]
                             (list 'if (list name 'value)
                                   :accepted false-result))
                       [type-name '-> '(typed.clojure/Val :accepted)]))
               safe (branch-test :accepted)
               unsafe (branch-test :rejected)]
           ;; The shape includes malformed maps, which the checker permits
           ;; under the honest open-HMap alias.
           (check-form [:shape-with-nil name]
                       (list 'typed.clojure/ann-form raw-nil type-name))
           (doseq [bad [raw-nil raw-nested]]
             (when (or (identity/identity? bad)
                       (predicate bad)
                       (not= :invalid-binding
                             (try (identity/bindings {key bad})
                                  nil
                                  (catch clojure.lang.ExceptionInfo e
                                    (:error/kind (ex-data e))))))
               (throw (ex-info "Invalid identity shape was admitted."
                               {:kind kind :identity name :bad bad}))))
           ;; Unlike (Pred HMap), one-way refinements must not erase a
           ;; false branch merely because the input has the tagged shape.
           (check-form [:one-way-false-is-reachable name] safe)
           (check-negative [:one-way-excludes-unsound-bottom name]
                           safe unsafe))))
     ;; Check the real runtime predicates against all category combinations,
     ;; using opaque numeric, vector, and map raw identities. This is bounded
     ;; behavioral evidence, not a universal proof of the Pred refinements.
     (doseq [[kind _ constructor _] kind-predicates
             [checked-kind checked-name _ predicate] kind-predicates]
       (let [ref (constructor [17 {:opaque :value}])
             actual (boolean (predicate ref))
             expected (= kind checked-kind)]
         (when-not (= expected actual)
           (throw (ex-info "Original predicate category matrix mismatch."
                           {:identity kind :predicate checked-name
                            :expected expected :actual actual})))))
     (doseq [invalid [nil 42 :principal {} {:gesso.choreo.identity/kind :principal}
                      {:gesso.choreo.identity/type :gesso.choreo.identity/ref
                       :gesso.choreo.identity/kind :principal
                       :gesso.choreo.identity/value nil}]
             [_ pred-name _ predicate] kind-predicates]
       (when (predicate invalid)
         (throw (ex-info "Predicate accepted malformed/non-identity data."
                         {:predicate pred-name :value invalid}))))
     (println "Checked six original predicate bodies, seven negative return")
     (println "mutations, 36 kind comparisons and malformed-data matrix.")
     (println "Typed Choreo binding source gate passed (7 branches).")
     :ok)))

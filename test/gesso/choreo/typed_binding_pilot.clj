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
   The public `bindings` contract is a PARTIAL boundary over untrusted data:
   if it returns normally the result is a map, but it may reject malformed
   inputs by throwing. Its narrow command/execution overload preserves a
   shape for already well-typed input; neither overload by itself proves
   that an input has passed admission. The gate checks the original
   `bindings?` and `binding-value` consumers against the broad overload and
   audits that they actually call `bindings`. The original `bindings`
   implementation is also checked under a deliberately BROAD map-returning
   contract for untrusted inputs; the stronger command/execution overload
   remains a separately trusted assumption, not a proved implementation
   property. Checks of a broad result type do not prove the per-key runtime
   validation, authorization, or full application admission. In addition,
   the original validate-binding dispatcher is checked with a finite seven-
   key input domain and a seven-way union of possible successful results.
   This eliminates the previous Any-return assumption for that function,
   but does not correlate an individual input key with its specific result,
   nor prove any of the trusted predicate refinements. We therefore test
   that the WHOLE dispatcher still type-checks after each of seven unsafe
   cross-category branch swaps, while source-derived key-specific checks
   correctly reject those exact mutations as static type errors. An overloaded
   dispatcher signature has NOT been claimed: Typed Clojure cannot verify the
   original case body against seven correlated overloads. The public bindings
   caller's key validation is still separately flow-audited. This gate now
   also audits the exact flow from each reducer entry through validate-binding
   into the output map, and rejects a bypass mutation that is still well-typed
   and still mentions validate-binding. A separate failure-path audit requires
   every original per-kind guard to throw when validation fails: otherwise
   fabricated, correctly tagged identities pass the category type checker.
   The source-derived checks must also reject deliberate, well-typed forged
   fallbacks. These checks do not certify predicate truth or provenance.
   Bounded sparse-map enumeration tests
   admission behavior but is not an exhaustive proof for arbitrary raw values."
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

;; Restrict the *private* dispatcher to the keys that its public caller
;; admits. With any other key, the source `case` would return nil. That is
;; precisely why annotating it [Kw Any -> ValidatedBindingValue] would be
;; unsound. This finite input domain is part of the contract, not a claim
;; that arbitrary caller data already passed validation.
(t/defalias BindingKey
  (t/U (t/Val :role) (t/Val :principal) (t/Val :actor)
       (t/Val :authority) (t/Val :host)
       (t/Val :command-id) (t/Val :execution-id)))
(t/defalias ValidatedBindingValue
  (t/U t/Kw PrincipalRef ActorRef AuthorityRef HostRef
       CommandRef ExecutionRef))
;; If the original dispatcher returns normally, its value belongs to this
;; union, conditional on the explicit positive-only predicate assumptions.
;; The original body is checked below; case-specific correlation is still
;; checked separately for each exact source arm.
(t/ann identity/validate-binding
       [BindingKey t/Any -> ValidatedBindingValue])

;; `bindings` accepts arbitrary downstream data, but normal return means
;; a sparse map of supported keys. Rejection occurs by exception, not by a
;; value-level type. The narrow overload is only useful for typed callers;
;; it does NOT turn a structural HMap into evidence of runtime admission.
;; The original implementation is independently checked under a broad
;; [Any -> Map Any Any] contract at the end of this gate. The stronger
;; command/execution overload remains trusted and is NOT verified by that
;; source-body check.
(t/ann identity/binding-keys (t/Set t/Kw))
(t/ann identity/bindings
       (t/IFn
        [gesso.choreo.typed-identity-pilot/CommandExecutionBindings
         -> gesso.choreo.typed-identity-pilot/CommandExecutionBindings]
        [t/Any -> (t/Map t/Kw t/Any)]))
(t/ann identity/bindings? [t/Any -> t/Bool])
(t/ann identity/binding-value [t/Any t/Kw -> t/Any])

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
         ;; The type checker can prove only that `bindings` returns a map.
         ;; A source mutation could retain the validator in a dead branch,
         ;; return the original unvalidated value, and still type-check.
         ;; This narrowly scoped AST audit requires the validated result to
         ;; flow directly into the output map. Fail closed if this pipeline
         ;; changes so the audit is revisited rather than silently bypassed.
         audit-binding-flow!
         (fn [definition]
           (let [args (nth definition 3 nil)
                 forms (vec (drop 4 definition))
                 [shape-check keys-check reduction] forms
                 [_ reducer init input] (if (seq? reduction) reduction [])
                 parameters (when (seq? reducer) (second reducer))
                 [acc entry] parameters
                 validated (list 'validate-binding
                                 (list 'key entry)
                                 (list 'val entry))
                 expected (list 'assoc acc (list 'key entry) validated)
                 body (when (seq? reducer) (last reducer))
                 key-check-body (when (seq? keys-check)
                                  (nth keys-check 2 nil))]
             (when-not (and (= '[value] args)
                            (= 3 (count forms))
                            (= 'when-not (first shape-check))
                            (= '(map? value) (second shape-check))
                            (= 'identity-error (first (nth shape-check 2 nil)))
                            (= 'let (first keys-check))
                            (= '[unknown (remove binding-keys (keys value))]
                               (second keys-check))
                            (= 'when (first key-check-body))
                            (= '(seq unknown) (second key-check-body))
                            (= 'identity-error
                               (first (nth key-check-body 2 nil)))
                            (= 4 (count reduction))
                            (= 'reduce (first reduction))
                            (= 'fn (first reducer))
                            (= 3 (count reducer))
                            (vector? parameters)
                            (= 2 (count parameters))
                            (every? symbol? parameters)
                            (not= acc entry)
                            (= {} init)
                            (= 'value input)
                            (= expected body))
               (throw (ex-info "Unchecked binding validation flow changed."
                               {:error/kind :unsafe-validation-flow
                                :reducer-body body
                                :expected-output expected})))
             true))
         bypass-binding-validation
         (fn [definition]
           (let [reduction (last definition)
                 reducer (second reduction)
                 [acc entry] (second reducer)
                 validated (list 'validate-binding
                                 (list 'key entry) (list 'val entry))
                 bypass (list 'do (list 'when false validated)
                              (list 'assoc acc (list 'key entry)
                                    (list 'val entry)))
                 reducer' (apply list (concat (butlast reducer) [bypass]))
                 reduction' (list 'reduce reducer' (nth reduction 2)
                                  (nth reduction 3))]
             (apply list (concat (butlast definition) [reduction']))))
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
         substitute-case-arm
         (fn [definition target replacement]
           (let [original-case (last definition)
                 entries (partition 2 (drop 2 original-case))
                 hits (count (filter #(= target (first %)) entries))]
             ;; Never silently mutate an unrecognized or missing case arm.
             (when-not (and (= 'case (first original-case))
                            (= 'key (second original-case))
                            (= 7 (count entries))
                            (= 1 hits))
               (throw (ex-info "Cannot substitute original binding arm."
                               {:target target :hits hits})))
             (let [new-case (list* 'case 'key
                                   (mapcat (fn [[k body]]
                                             [k (if (= k target)
                                                  replacement body)])
                                           entries))]
               (apply list (concat (butlast definition) [new-case])))))
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
                     'validate-binding 'bindings? 'binding-value]]
         (check-form name (source-function name)))
       ;; Under the seven-key input domain, the original whole dispatcher
       ;; must return only a role keyword or one of the six category shapes.
       ;; Mutate an ACTUAL case arm, rather than a typed synthetic branch:
       ;; this must produce a genuine static type mismatch.
       (let [original binding-definition
             command-branch (:command-id branches)
             poisoned (walk/postwalk #(if (= % command-branch) 42 %) original)]
         (check-negative :validate-binding-wrong-original-arm
                         original poisoned))
       ;; An arbitrary keyword is not an admitted binding key. Confirm the
       ;; checker rejects the call specifically as a static application type
       ;; error, not as an exception. Nil is still an allowed *input value*
       ;; at the type boundary, since runtime validation rejects it.
       (check-form :validate-binding-supported-key
                   '(validate-binding :principal nil))
       (let [{:keys [ex type-errors]}
             (check-form-info '(validate-binding :unsupported nil)
                              :check-config config)]
         (when (or ex
                   (not= [:typed.clojure/app-type-error]
                         (mapv :type-error type-errors)))
           (throw (ex-info "Unsupported key not rejected statically."
                           {:exception (some-> ex ex-message)
                            :errors (mapv str type-errors)})))
         (println "Rejected unsupported validate-binding key statically."))
       ;; Whole-body union checking cannot preserve the correlation between
       ;; a particular key and its result kind. For each of the seven real
       ;; case arms, transplant a *different* existing source arm. The
       ;; original seven-way union still passes; the exact original arm,
       ;; checked at its expected type, must reject the transplant.
       ;;
       ;; This is a demonstration of the gap and a permanent guard on the
       ;; source-derived per-arm evidence. It relies on the declared one-way
       ;; predicate refinements; it is not a dependent-type proof of the
       ;; original dispatcher or an independent proof of these refinements.
       (doseq [[key wrong-key expected-type]
               [[:role :principal 'typed.clojure/Kw]
                [:principal :actor
                 'gesso.choreo.typed-binding-pilot/PrincipalRef]
                [:actor :authority
                 'gesso.choreo.typed-binding-pilot/ActorRef]
                [:authority :host
                 'gesso.choreo.typed-binding-pilot/AuthorityRef]
                [:host :command-id
                 'gesso.choreo.typed-binding-pilot/HostRef]
                [:command-id :execution-id
                 'gesso.choreo.typed-binding-pilot/CommandRef]
                [:execution-id :principal
                 'gesso.choreo.typed-binding-pilot/ExecutionRef]]]
         (let [original-arm (get branches key)
               wrong-arm (get branches wrong-key)
               mutant (substitute-case-arm binding-definition key wrong-arm)
               mutant-case (last mutant)
               mutant-arm (get (apply hash-map (drop 2 mutant-case)) key)
               original-typed (typed-branch original-arm expected-type)
               mutant-typed (typed-branch mutant-arm expected-type)]
           (when-not (and (not= original-arm wrong-arm)
                          (not= binding-definition mutant)
                          (= wrong-arm mutant-arm))
             (throw (ex-info "Cross-kind dispatcher mutation ineffective."
                             {:binding key :replacement wrong-key})))
           (check-form [:specific-original key] original-typed)
           (check-form [:union-accepts-wrong-kind key] mutant)
           (check-negative [:specific-rejects-wrong-kind key]
                           original-typed mutant-typed)))
       (println "Seven whole-case wrong-category mutations accepted by union")
       (println "and rejected by source-derived key-specific typing.")

       ;; Typed output categories are NOT sufficient to establish admission.
       ;; An invalid value could be replaced with a newly fabricated identity
       ;; of the correct kind. Its source type checks under both the broad
       ;; union and the key-specific HMap, yet it violates the obligation to
       ;; reject invalid downstream input. Audit the ORIGINAL failure path,
       ;; then require the audit to reject these well-typed mutations.
       (let [audit-identity-guard!
             (fn [key expected-predicate branch]
               (let [error-expr (nth branch 3 nil)]
                 (when-not (and (seq? branch)
                                (= 4 (count branch))
                                (= 'if (first branch))
                                (= (list expected-predicate 'value)
                                   (second branch))
                                (= 'value (nth branch 2 nil))
                                (seq? error-expr)
                                (= 4 (count error-expr))
                                (= 'identity-error (first error-expr))
                                (= :invalid-binding (second error-expr))
                                (string? (nth error-expr 2 nil))
                                (= {:binding 'key :value 'value}
                                   (nth error-expr 3 nil)))
                   (throw (ex-info "Identity validation failure path changed."
                                   {:error/kind :unsafe-binding-failure-path
                                    :binding key :form branch})))
                 true))
             audit-role-guard!
             (fn [definition]
               (let [args (nth definition 3 nil)
                     guard (nth definition 4 nil)
                     error-form (nth guard 2 nil)]
                 (when-not (and (= '[role] args)
                                (= 6 (count definition))
                                (= 'role (last definition))
                                (= 'when-not (first guard))
                                (= '(role? role) (second guard))
                                (= 'identity-error (first error-form))
                                (= 4 (count error-form))
                                (= :invalid-role (second error-form))
                                (string? (nth error-form 2 nil))
                                (= {:role 'role} (nth error-form 3 nil)))
                   (throw (ex-info "Role validator's rejection path changed."
                                   {:error/kind :unsafe-binding-failure-path
                                    :form definition})))
                 true))
             original-role (source-function 'require-role)
             ;; Replace rejection with a fabricated keyword. The altered
             ;; function still returns Kw for EVERY input, including invalid
             ;; input that the real function would reject.
             forged-role (apply list (concat (take 4 original-role)
                                             ['(if (role? role)
                                                 role :forged-role)]))]
         (audit-role-guard! original-role)
         (check-form :forged-role-validator-still-typed forged-role)
         (let [error-kind (try (audit-role-guard! forged-role) nil
                               (catch clojure.lang.ExceptionInfo e
                                 (:error/kind (ex-data e))))]
           (when-not (= :unsafe-binding-failure-path error-kind)
             (throw (ex-info "Role guard audit accepted forged role."
                             {:actual error-kind}))))
         (doseq [{:keys [key predicate type]} identity-cases]
           (let [original-arm (get branches key)
                 actual-kind (case key :command-id :command
                                       :execution-id :execution
                                       key)
                 forged {:gesso.choreo.identity/type :gesso.choreo.identity/ref
                         :gesso.choreo.identity/kind actual-kind
                         :gesso.choreo.identity/value :fabricated}
                 mutant-arm (list 'if (list predicate 'value)
                                  'value forged)
                 mutant (substitute-case-arm binding-definition key mutant-arm)
                 mutant-typed (typed-branch mutant-arm type)
                 original-typed (typed-branch original-arm type)]
             (audit-identity-guard! key predicate original-arm)
             (check-form [:valid-guarded-arm key] original-typed)
             (check-form [:forged-arm-still-specific-type key] mutant-typed)
             (check-form [:forged-dispatch-still-union-typed key] mutant)
             (let [error-kind
                   (try (audit-identity-guard! key predicate mutant-arm)
                        nil
                        (catch clojure.lang.ExceptionInfo e
                          (:error/kind (ex-data e))))]
               (when-not (= :unsafe-binding-failure-path error-kind)
                 (throw (ex-info "Guard audit accepted a forged identity."
                                 {:binding key :actual error-kind}))))))
         (println "Rejected six well-typed forged identity failure paths")
         (println "and a well-typed role fallback through source-flow audits."))
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
       ;; The broad signature does not certify the body of `bindings`.
       ;; Demand that the original functions still route untrusted data
       ;; through it, and that it still invokes per-key validation.
       ;; This source check is intentionally fail-closed on refactoring.
       (let [count-calls (fn [form symbol]
                           (count (filter #(and (seq? %)
                                                (= symbol (first %)))
                                          (tree-seq coll? seq form))))
             checks [['bindings 'validate-binding 1]
                     ['bindings 'map? 1]
                     ['bindings 'remove 1]
                     ['bindings? 'bindings 1]
                     ['binding-value 'bindings 1]]]
         (doseq [[name invoked expected] checks]
           (let [actual (count-calls (source-function name) invoked)]
             (when-not (= expected actual)
               (throw (ex-info "Original admission call path changed."
                               {:source name :call invoked
                                :expected expected :actual actual}))))))
       (let [original (source-function 'bindings?)
             mutated (apply list (concat (butlast original) [:admitted]))]
         (check-negative 'bindings?-wrong-return original mutated))
       (let [binding-keys-defs
             (filter #(and (seq? %) (= 'def (first %))
                           (= 'binding-keys (second %))) source-forms)]
         (when-not (= 1 (count binding-keys-defs))
           (throw (ex-info "Missing or ambiguous original binding keys." {})))
         (check-form 'binding-keys (first binding-keys-defs)))
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
           (check-form [:shape-with-nested name]
                       (list 'typed.clojure/ann-form raw-nested type-name))
           (doseq [bad [raw-nil raw-nested]]
             (let [binding-map {key bad}
                   error-kind
                   (fn [thunk]
                     (try
                       (thunk)
                       nil
                       (catch clojure.lang.ExceptionInfo e
                         (:error/kind (ex-data e)))))]
               (when (or (identity/identity? bad)
                         (predicate bad)
                         (identity/bindings? binding-map)
                         (not= :invalid-binding
                               (error-kind #(identity/bindings binding-map)))
                         (not= :invalid-binding
                               (error-kind #(identity/binding-value binding-map key)))
                         (not= :invalid-binding
                               (error-kind #(identity/explain binding-map))))
                 (throw (ex-info "Structurally typed but invalid data escaped admission."
                                 {:kind kind :identity name :bad bad})))))
           ;; Unlike (Pred HMap), one-way refinements must not erase a
           ;; false branch merely because the input has the tagged shape.
           (check-form [:one-way-false-is-reachable name] safe)
           (check-negative [:one-way-excludes-unsound-bottom name]
                           safe unsafe))))
     ;; Successful sparse inputs must pass all public access paths, while
     ;; unsupported map keys and non-map inputs must not pass admission.
     (let [command (identity/command-id [:opaque 17])
           valid {:command-id command}]
       (when-not (and (= valid (identity/bindings valid))
                      (identity/bindings? valid)
                      (= command (identity/binding-value valid :command-id))
                      (= {:command-id {:kind :command :value [:opaque 17]}}
                         (identity/explain valid))
                      (= {} (identity/bindings {}))
                      (identity/bindings? {}))
         (throw (ex-info "Valid sparse admission paths disagreed." {})))
       (doseq [[bad expected-error]
               [[{:unsupported command} :unknown-binding]
                [[:command-id command] :invalid-bindings]
                [{:command-id (identity/execution-id [:opaque 17])}
                 :invalid-binding]]]
         (let [kind (try (identity/bindings bad) nil
                         (catch clojure.lang.ExceptionInfo e
                           (:error/kind (ex-data e))))]
           (when (or (not= expected-error kind)
                     (identity/bindings? bad))
             (throw (ex-info "Malformed downstream bindings were admitted."
                             {:value bad :expected expected-error :actual kind}))))))
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
     ;; Verify the validated input-to-output flow independently from map
     ;; return typing. A validation call merely appearing somewhere in the
     ;; AST is insufficient: the checked output must use its result.
     (let [original (source-function 'bindings)
           bypass (bypass-binding-validation original)
           calls (fn [form]
                   (count (filter #(and (seq? %)
                                        (= 'validate-binding (first %)))
                                  (tree-seq coll? seq form))))]
       (audit-binding-flow! original)
       (when-not (and (not= original bypass)
                      (= (calls original) (calls bypass)))
         (throw (ex-info "Validation bypass mutation was ineffective."
                         {:original-calls (calls original)
                          :mutated-calls (calls bypass)})))
       (let [kind (try (audit-binding-flow! bypass) nil
                       (catch clojure.lang.ExceptionInfo e
                         (:error/kind (ex-data e))))]
         (when-not (= :unsafe-validation-flow kind)
           (throw (ex-info "Bypass mutation defeated binding-flow audit."
                           {:actual kind}))))
       (println "Binding flow audit rejects validator-preserving bypass."))

     ;; A finite model of the seven sparse binding keys. Each key is absent,
     ;; present with a valid value, or present with a wrong-category value.
     ;; Reject every map with any invalid member; otherwise require exact
     ;; preservation. This is bounded behavioral evidence, not an invariant
     ;; proof over all possible opaque values or execution environments.
     (let [columns
           [[:role :browser (identity/principal :not-a-role)]
            [:principal (identity/principal :p) (identity/actor :p)]
            [:actor (identity/actor :a) (identity/authority :a)]
            [:authority (identity/authority :u) (identity/host :u)]
            [:host (identity/host :h) (identity/command-id :h)]
            [:command-id (identity/command-id :c) (identity/execution-id :c)]
            [:execution-id (identity/execution-id :e) (identity/principal :e)]]
           combinations (reduce (fn [states [k valid invalid]]
                                  (for [[m bad?] states
                                        variant [0 1 2]]
                                    (case variant
                                      0 [m bad?]
                                      1 [(assoc m k valid) bad?]
                                      2 [(assoc m k invalid) true])))
                                [[{} false]]
                                columns)]
       (when-not (= 2187 (count combinations))
         (throw (ex-info "Sparse binding universe changed." {})))
       (doseq [[candidate bad?] combinations]
         (let [outcome (try {:accepted (identity/bindings candidate)}
                            (catch clojure.lang.ExceptionInfo e
                              {:error-kind (:error/kind (ex-data e))}))]
           (if bad?
             (when-not (#{:invalid-role :invalid-binding}
                        (:error-kind outcome))
               (throw (ex-info "Invalid sparse bindings admitted."
                               {:input candidate :actual outcome})))
             (when-not (= {:accepted candidate} outcome)
               (throw (ex-info "Valid sparse bindings not preserved."
                               {:input candidate :actual outcome}))))))
       (println "Validated 2,187 sparse binding combinations."))

     ;; Verify the exact current production source body for bindings. The
     ;; earlier into/map transducer could not be checked by Typed Clojure;
     ;; the current source uses a locally annotated direct reducer. Here we
     ;; temporarily install BROAD interfaces in the checker environment to
     ;; avoid claiming that the reducer proves the stronger command/execution
     ;; HMap overload. Do this LAST: previous checks used the original public
     ;; signatures. The override affects only this opt-in checker process and
     ;; does not alter any production Var or runtime behavior.
     (t/ann identity/bindings [t/Any -> (t/Map t/Any t/Any)])
     (t/ann identity/validate-binding [t/Any t/Any -> t/Any])
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (let [original (source-function 'bindings)
             ;; Reject a concrete wrong return type with a static checker
             ;; mismatch, not an exception or a runtime evaluation failure.
             mutated (apply list (concat (butlast original) [:not-a-map]))]
         (check-form :bindings-original-broad-contract original)
         ;; The bypass remains fully type-correct at the broad boundary;
         ;; source-flow assurance must catch it, not the map-return checker.
         (check-form :bindings-bypass-still-broadly-typed
                     (bypass-binding-validation original))
         (check-negative :bindings-wrong-return original mutated)))
     (println "Checked original bindings body under broad map contract;")
     (println "the command/execution overload is still trusted.")
     (println "Checked original bindings? and binding-value bodies;")
     (println "validated sparse maps and rejected malformed typed shapes.")
     (println "Checked six original predicate bodies, seven negative return")
     (println "mutations, 36 kind comparisons and malformed-data matrix.")
     (println "Typed Choreo binding source gate passed (7 branches).")
     :ok)))

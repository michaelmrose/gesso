(ns ^{:typed.clojure true} gesso.choreo.typed-binding-pilot
  "Opt-in source-backed type checks of Choreo's identity-binding dispatch.

   The input to this boundary is application-supplied data, not a typed
   downstream program. Each of the seven original `validate-binding` case
   branches is checked against its declared output category, alongside the
   original validator's broader body and the role helpers.

   Opaque raw identity values are intentionally not assumed to be strings.
   Predicate annotations remain trusted until their implementation/refinement
   semantics are established; the checked case branches are conditional on
   those declarations. In particular this does not type-check `bindings`,
   establish authorization, or certify the entire admission pipeline."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.choreo.identity :as identity]
            [gesso.choreo.typed-identity-pilot]
            [typed.clojure :as t]))

;; Unlike the string-restricted earlier pilot, these representations preserve
;; the deliberately opaque identity raw-value contract of production Gesso.
(t/defalias PrincipalRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :principal)
           :gesso.choreo.identity/value t/Any}
          :complete? true))
(t/defalias ActorRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :actor)
           :gesso.choreo.identity/value t/Any}
          :complete? true))
(t/defalias AuthorityRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :authority)
           :gesso.choreo.identity/value t/Any}
          :complete? true))
(t/defalias HostRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :host)
           :gesso.choreo.identity/value t/Any}
          :complete? true))
(t/defalias CommandRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :command)
           :gesso.choreo.identity/value t/Any}
          :complete? true))
(t/defalias ExecutionRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :execution)
           :gesso.choreo.identity/value t/Any}
          :complete? true))

;; Trusted predicate refinement signatures; checked case forms rely on these.
;; Runtime cases below exercise each category but do not prove them universally.
(t/ann identity/role? (t/Pred t/Kw))
(t/ann identity/require-role [t/Any -> t/Kw])
(t/ann identity/principal? (t/Pred PrincipalRef))
(t/ann identity/actor? (t/Pred ActorRef))
(t/ann identity/authority? (t/Pred AuthorityRef))
(t/ann identity/host? (t/Pred HostRef))
(t/ann identity/command-id? (t/Pred CommandRef))
(t/ann identity/execution-id? (t/Pred ExecutionRef))

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
             (println "Rejected wrong-category source mutation:" name)))
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
                       ['typed.clojure/Any '-> output-type])))]
     (when-not (= :ok (check-ns 'gesso.choreo.typed-binding-pilot
                               :check-config config))
       (throw (ex-info "Binding pilot namespace did not type-check." {})))
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [name ['role? 'require-role 'validate-binding]]
         (check-form name (source-function name)))
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
     (println "Typed Choreo binding source gate passed (7 branches).")
     :ok)))

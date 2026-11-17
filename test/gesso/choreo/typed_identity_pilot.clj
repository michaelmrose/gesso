(ns ^{:typed.clojure true} gesso.choreo.typed-identity-pilot
  "Opt-in Typed Clojure feasibility gate for existing Choreo identity kinds.

   Run with :typed:test; do not include this namespace in the normal Gesso
   production or JVM test paths. The HMap aliases below describe the current
   three-key, string-valued subset of gesso.choreo.identity, not a new identity
   implementation or a certification of its unchecked constructors.

   Consumers are checked, and the current command-id and execution-id
   implementation forms are read from production source and checked directly.
   The generic identity constructor and binding validator retain restricted,
   trusted signatures; source coverage is deliberately limited to two
   string-valued constructor overloads. This is not a proof of all identity
   implementations, admission provenance, or arbitrary raw identity values."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [gesso.choreo.identity :as identity]
            [typed.clojure :as t]))

(t/defalias CommandRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :command)
           :gesso.choreo.identity/value t/Str}
          :complete? true))

(t/defalias ExecutionRef
  (t/HMap :mandatory
          {:gesso.choreo.identity/type (t/Val :gesso.choreo.identity/ref)
           :gesso.choreo.identity/kind (t/Val :execution)
           :gesso.choreo.identity/value t/Str}
          :complete? true))

(t/defalias CommandExecutionBindings
  (t/HMap :mandatory
          {:command-id CommandRef
           :execution-id ExecutionRef}
          :complete? true))

;; The generic identity constructor is still assumed here. The two specialized
;; constructor BODIES below are checked against its restricted overloads using
;; their actual source forms. This does not establish the generic implementation.
(t/ann identity/identity
       (t/IFn [(t/Val :command) t/Str -> CommandRef]
              [(t/Val :execution) t/Str -> ExecutionRef]))

;; Restricted signatures for the two constructors and binding validator.
;; The constructors' original source bodies are checked by the opt-in gate,
;; but the binding validator remains an unchecked foreign assumption.
(t/ann identity/command-id [t/Str -> CommandRef])
(t/ann identity/execution-id [t/Str -> ExecutionRef])
(t/ann identity/bindings
       [CommandExecutionBindings -> CommandExecutionBindings])

(t/ann assemble-command-execution [t/Str t/Str -> CommandExecutionBindings])
(defn assemble-command-execution [command-raw execution-raw]
  (identity/bindings
   {:command-id (identity/command-id command-raw)
    :execution-id (identity/execution-id execution-raw)}))

(t/ann command-kind [CommandRef -> (t/Val :command)])
(defn command-kind [ref]
  (:gesso.choreo.identity/kind ref))

(t/ann command-raw-value [CommandRef -> t/Str])
(defn command-raw-value [ref]
  (:gesso.choreo.identity/value ref))

(t/ann execution-kind [ExecutionRef -> (t/Val :execution)])
(defn execution-kind [ref]
  (:gesso.choreo.identity/kind ref))

;; This is test-harness code, not a typed semantic function. Restrict the
;; explicit unchecked region to the gate driver; never use it on the bodies
;; of the checked consumer functions above.
(t/tc-ignore
 (defn -main [& _]
   (let [check-ns (requiring-resolve 'typed.clj.checker/check-ns)
         check-form-info (requiring-resolve
                          'typed.clj.checker/check-form-info)
         check-config {:max-processors 1 :check-form-eval :never}
         source (io/resource "gesso/choreo/identity.cljc")
         _ (when-not source
             (throw (ex-info "Original identity source was not found." {})))
         source-forms (with-open [r (java.io.PushbackReader. (io/reader source))]
                        (loop [acc []]
                          (let [form (read {:read-cond :allow
                                            :features #{:clj}
                                            :eof ::eof} r)]
                            (if (= ::eof form)
                              acc
                              (recur (conj acc form))))))
         original-definition
         (fn [name]
           (let [matches (filter #(and (seq? %)
                                       (= 'defn (first %))
                                       (= name (second %)))
                                 source-forms)]
             (when-not (= 1 (count matches))
               (throw (ex-info "Original identity definition missing or ambiguous."
                               {:name name :matches (count matches)})))
             (first matches)))
         command (identity/command-id "same-raw-value")
         execution (identity/execution-id "same-raw-value")
         checked-ns 'gesso.choreo.typed-identity-pilot]
     (when-not (and (= true (:typed.clojure (meta (the-ns checked-ns))))
                    (identity/command-id? command)
                    (identity/execution-id? execution)
                    (not (identity/command-id? execution))
                    (not (identity/execution-id? command)))
       (throw (ex-info "Identity pilot's runtime preconditions failed." {})))
     (when-not (= :ok (check-ns checked-ns :check-config check-config))
       (throw (ex-info "Typed namespace did not pass actual checking." {})))
     (when-not (= (identity/bindings {:command-id command
                                     :execution-id execution})
                  (assemble-command-execution "same-raw-value"
                                              "same-raw-value"))
       (throw (ex-info "Real identity/bindings disagrees with typed consumer." {})))
     (when-not (= :invalid-binding
                  (try
                    (identity/bindings {:command-id execution
                                        :execution-id command})
                    nil
                    (catch clojure.lang.ExceptionInfo e
                      (:error/kind (ex-data e)))))
       (throw (ex-info "Real identity/bindings accepted swapped identities." {})))
     ;; Check the actual source forms (not rewritten copies of their bodies).
     ;; Mutation tests must fail as static type errors, not as exceptions.
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [[name wrong-kind]
               [['command-id :execution]
                ['execution-id :command]]]
         (let [original (original-definition name)
               changed (walk/postwalk (fn [x]
                                        (if (= x (if (= name 'command-id)
                                                   :command :execution))
                                          wrong-kind x))
                                      original)
               good (check-form-info original :check-config check-config)
               bad (check-form-info changed :check-config check-config)]
           (when (or (= original changed)
                     (:ex good)
                     (seq (:type-errors good))
                     (:ex bad)
                     (not (seq (:type-errors bad)))
                     (not-every? #(= :typed.clojure/type-mismatch-error
                                     (:type-error %))
                                 (:type-errors bad)))
             (throw (ex-info "Original constructor type check failed."
                             {:name name
                              :changed? (not= original changed)
                              :good-exception (some-> (:ex good) ex-message)
                              :good-errors (mapv str (:type-errors good))
                              :bad-exception (some-> (:ex bad) ex-message)
                              :bad-errors (mapv str (:type-errors bad))})))
           (println "Original constructor:" name
                    "checked; wrong-kind mutation rejected"))))
     (binding [*ns* (the-ns checked-ns)]
       (doseq [[description form expect-error?]
               [["command accepted" (list 'command-raw-value command) false]
                ["command kind accepted" (list 'command-kind command) false]
                ["execution kind accepted" (list 'execution-kind execution) false]
                ["execution rejected as command"
                 (list 'command-raw-value execution) true]
                ["execution rejected by command-kind"
                 (list 'command-kind execution) true]
                ["command rejected by execution-kind"
                 (list 'execution-kind command) true]
                ["real constructors and binding accepted"
                 '(assemble-command-execution "command" "execution") false]
                ["assembled command accepted"
                 '(command-kind
                   (:command-id (assemble-command-execution "command" "execution")))
                 false]
                ["assembled execution rejected as command"
                 '(command-kind
                   (:execution-id (assemble-command-execution "command" "execution")))
                 true]
                ["tagged command rejected as raw string"
                 '(assemble-command-execution
                   (identity/command-id "command") "execution") true]
                ["swapped runtime bindings rejected by checker"
                 '(identity/bindings
                   {:command-id (identity/execution-id "command")
                    :execution-id (identity/command-id "execution")}) true]]]
         (let [{:keys [ex type-errors]}
               (check-form-info form :check-config check-config)
               rejected? (boolean (seq type-errors))]
           (when (or ex (not= expect-error? rejected?))
             (throw (ex-info "Typed identity gate disagreed with expectation."
                             {:case description
                              :expected-rejection? expect-error?
                              :exception (some-> ex ex-message)
                              :type-errors (mapv str type-errors)})))
           (println "Typed identity:" description
                    (if rejected? "rejected" "accepted")))))
     (println "Typed Choreo identity feasibility gate passed.")
     :ok)))

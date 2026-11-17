(ns ^{:typed.clojure true} gesso.choreo.typed-identity-pilot
  "Opt-in Typed Clojure feasibility gate for existing Choreo identity kinds.

   Run with :typed:test; do not include this namespace in the normal Gesso
   production or JVM test paths. The HMap aliases below describe the current
   three-key, string-valued subset of gesso.choreo.identity, not a new identity
   implementation or a certification of its unchecked constructors.

   The gate checks the original shared identity constructor, its relevant
   private helpers, identity marker/kind constants, and two specialized
   constructors alongside the typed consumer functions. The contract covers
   string-valued command and execution references only; other identity kinds
   and opaque raw values are not statically certified here.

   Checking bodies against handwritten annotations is not a proof that the
   annotations describe every runtime behavior. The binding validator is
   still an unchecked foreign assumption. Admission provenance and the
   broader Gesso/XTDB correctness obligations remain separate."
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

;; This narrow, explicitly incomplete interpretation of the original Gesso
;; identity vocabulary covers string-valued command and execution references.
;; The gate below checks these *original source forms* under these signatures.
(t/ann identity/identity-type (t/Val :gesso.choreo.identity/ref))
(t/ann identity/identity-kinds (t/Set t/Kw))
(t/ann identity/identity-error
       [t/Kw t/Str (t/Map t/Any t/Any) -> t/Nothing])
(t/ann identity/identity? [t/Any -> t/Bool])
(t/ann identity/require-kind
       (t/IFn [(t/Val :command) -> (t/Val :command)]
              [(t/Val :execution) -> (t/Val :execution)]))
(t/ann identity/require-raw-value [t/Str -> t/Str])
(t/ann identity/identity
       (t/IFn [(t/Val :command) t/Str -> CommandRef]
              [(t/Val :execution) t/Str -> ExecutionRef]))

;; The binding validator remains an unchecked foreign assumption. Its actual
;; implementation will need separate source checking and preflight evidence.
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

;; Only the gate driver is deliberately unchecked. It invokes the checker
;; on original production source forms; the consumer functions stay checked.
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
         source-definition
         (fn [name tags]
           (let [matches (filter #(and (seq? %)
                                       (contains? tags (first %))
                                       (= name (second %)))
                                 source-forms)]
             (when-not (= 1 (count matches))
               (throw (ex-info "Original identity source missing or ambiguous."
                               {:name name :tags tags
                                :matches (count matches)})))
             (first matches)))
         original-function (fn [name]
                             (source-definition name #{'defn 'defn-}))
         original-constant (fn [name]
                             (source-definition name #{'def}))
         check-form! (fn [name form]
                       (let [{:keys [ex type-errors ret]}
                             (check-form-info form :check-config check-config)]
                         (when (or ex (seq type-errors) (nil? ret))
                           (throw (ex-info "Original identity source did not type-check."
                                           {:name name
                                            :exception (some-> ex ex-message)
                                            :type-errors (mapv str type-errors)
                                            :returned? (some? ret)})))
                         (println "Original identity source:" name "checked")))
         require-type-mismatch!
         (fn [name original mutated]
           (when (= original mutated)
             (throw (ex-info "Negative mutation did not change source."
                             {:name name})))
           (let [{:keys [ex type-errors]}
                 (check-form-info mutated :check-config check-config)]
             (when (or ex
                       (not (seq type-errors))
                       (not-every? #(= :typed.clojure/type-mismatch-error
                                       (:type-error %))
                                   type-errors))
               (throw (ex-info "Source mutation did not yield static type mismatches."
                               {:name name
                                :exception (some-> ex ex-message)
                                :type-errors (mapv str type-errors)})))
             (println "Original identity mutation:" name "rejected")))
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
     ;; Check source constants, private helpers, and constructors as they exist
     ;; in Gesso. A missing defn- must never count as a successful nil check.
     (binding [*ns* (the-ns 'gesso.choreo.identity)]
       (doseq [name ['identity-type 'identity-kinds]]
         (check-form! name (original-constant name)))
       (doseq [name ['identity? 'identity-error 'require-kind
                     'require-raw-value 'identity 'command-id 'execution-id]]
         (check-form! name (original-function name)))

       ;; Mutate each relevant source boundary. We require a *static type
       ;; mismatch*, never a runtime exception or a missing form.
       (doseq [[name wrong-kind]
               [['command-id :execution]
                ['execution-id :command]]]
         (let [original (original-function name)
               mutated (walk/postwalk (fn [x]
                                        (if (= x (if (= name 'command-id)
                                                   :command :execution))
                                          wrong-kind x))
                                      original)]
           (require-type-mismatch! name original mutated)))
       (let [original (original-function 'identity)]
         (doseq [[description mutated]
                 [[:wrong-kind-key
                   (walk/postwalk #(if (= % :gesso.choreo.identity/kind)
                                     :gesso.choreo.identity/wrong-kind %) original)]
                  [:wrong-kind-result
                   (walk/postwalk #(if (= % '(require-kind kind))
                                     '(require-kind :execution) %) original)]
                  [:wrong-raw-type
                   (walk/postwalk #(if (= % '(require-raw-value value))
                                     42 %) original)]]]
           (require-type-mismatch! description original mutated)))
       (doseq [[name replacement]
               [['require-kind :execution]
                ['require-raw-value 42]]]
         (let [original (original-function name)
               mutated (apply list (concat (butlast original) [replacement]))]
           (require-type-mismatch! name original mutated)))
       (let [original (original-constant 'identity-type)]
         (require-type-mismatch!
          'identity-type original
          '(def identity-type :gesso.choreo.identity/incorrect))))
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

(ns ^{:typed.clojure true} gesso.choreo.typed-identity-pilot
  "Opt-in Typed Clojure feasibility gate for existing Choreo identity kinds.

   Run with :typed:test; do not include this namespace in the normal Gesso
   production or JVM test paths. The HMap aliases below describe the current
   three-key, string-valued subset of gesso.choreo.identity, not a new identity
   implementation or a certification of its unchecked constructors.

   The typed functions' bodies are checked. The external constructors and the
   gate's checker-driving code are NOT. Source-derived annotations, runtime
   provenance and arbitrary raw identity values remain open obligations."
  (:require [gesso.choreo.identity :as identity]
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
;; of the three functions above.
(t/tc-ignore
 (defn -main [& _]
   (let [check-ns (requiring-resolve 'typed.clj.checker/check-ns)
         check-form-info (requiring-resolve
                          'typed.clj.checker/check-form-info)
         command (identity/command-id "same-raw-value")
         execution (identity/execution-id "same-raw-value")
         checked-ns 'gesso.choreo.typed-identity-pilot]
     (when-not (and (= true (:typed.clojure (meta (the-ns checked-ns))))
                    (identity/command-id? command)
                    (identity/execution-id? execution)
                    (not (identity/command-id? execution))
                    (not (identity/execution-id? command)))
       (throw (ex-info "Identity pilot's runtime preconditions failed." {})))
     (when-not (= :ok (check-ns checked-ns :check-config {:max-processors 1}))
       (throw (ex-info "Typed namespace did not pass actual checking." {})))
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
                 (list 'execution-kind command) true]]]
         (let [{:keys [ex type-errors]} (check-form-info
                                         form :check-config {:max-processors 1})
               rejected? (or (some? ex) (seq type-errors))]
           (when-not (= (boolean expect-error?) (boolean rejected?))
             (throw (ex-info "Typed identity gate disagreed with expectation."
                             {:case description
                              :expected-rejection? expect-error?
                              :exception (some-> ex .getMessage)
                              :type-errors (mapv str type-errors)})))
           (println "Typed identity:" description
                    (if rejected? "rejected" "accepted")))))
     (println "Typed Choreo identity feasibility gate passed.")
     :ok)))

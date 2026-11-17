(ns gesso.choreo.typed-machine-suspension-check
  "Type-check the original receive/environment suspension producers.

   This is an opt-in JVM assurance gate, not a replacement for runtime
   execution? validation or a proof of semantic state correspondence."
  (:require [clojure.walk :as walk]
            [gesso.choreo.machine]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})

(defn- original [forms name]
  (let [matches (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                               (= name (second %))) forms)]
    (when-not (= 1 (count matches))
      (throw (ex-info "Missing or duplicate production suspension source."
                      {:name name :count (count matches)})))
    (first matches)))

(defn- check-original! [checker label form]
  (let [{:keys [ex type-errors]} (checker form :check-config config)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Production suspension body failed Typed Clojure."
                      {:label label :kinds (mapv :type-error type-errors)} ex)))
    true))

(defn- check-rejected! [checker label original mutated]
  (when (= original mutated)
    (throw (ex-info "Suspension mutation did not modify its target."
                    {:label label})))
  (let [{:keys [ex type-errors]} (checker mutated :check-config config)
        kinds (mapv :type-error type-errors)]
    ;; A single bad literal may produce more than one subtype error as the
    ;; checker propagates the inconsistent HMap through the original body.
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error} kinds))
      (throw (ex-info "Suspension mutation escaped static checking."
                      {:label label :kinds kinds
                       :exception (some-> ex ex-message)} ex)))
    true))

(defn check! [checker forms]
  (let [map-type '(typed.clojure/Map typed.clojure/Any typed.clojure/Any)
        plan '(typed.clojure/HMap
               :mandatory {:role typed.clojure/Kw}
               :complete? false)
        receive-state '(typed.clojure/HMap
                        :mandatory {:alternatives
                                    (typed.clojure/AVec
                                     (typed.clojure/Map typed.clojure/Any
                                                        typed.clojure/Any))}
                        :complete? false)
        environment-state '(typed.clojure/HMap
                            :mandatory {:events
                                        (typed.clojure/Map typed.clojure/Any
                                                           typed.clojure/Any)}
                            :optional {:event-contracts
                                       (typed.clojure/Map typed.clojure/Any
                                                          typed.clojure/Any)}
                            :complete? false)
        receive-descriptor '(typed.clojure/HMap
                             :mandatory {:kind (typed.clojure/Val :receive)
                                         :role typed.clojure/Kw
                                         :alternatives
                                         (typed.clojure/AVec
                                          (typed.clojure/Map typed.clojure/Any
                                                             typed.clojure/Any))}
                             :complete? false)
        environment-descriptor '(typed.clojure/HMap
                                 :mandatory {:kind (typed.clojure/Val :environment)
                                             :role typed.clojure/Kw
                                             :events (typed.clojure/Set
                                                      typed.clojure/Any)}
                                 :complete? false)
        helper (original forms 'non-role-bindings)
        receive (original forms 'receive-awaiting)
        environment (original forms 'environment-awaiting)]
    ;; Check the original helper body too: the producers must not rely on a
    ;; wholly unchecked Any->Map annotation for non-role bindings.
    (eval (list 'typed.clojure/ann
                'gesso.choreo.machine/non-role-bindings
                (list 'typed.clojure/IFn [map-type '-> map-type])))
    (doseq [[name state result]
            [['receive-awaiting receive-state receive-descriptor]
             ['environment-awaiting environment-state environment-descriptor]]]
      (eval (list 'typed.clojure/ann
                  (symbol "gesso.choreo.machine" (str name))
                  (list 'typed.clojure/IFn
                        [plan map-type state '-> result]))))
    (binding [*ns* (the-ns 'gesso.choreo.machine)]
      (check-original! checker :non-role-bindings helper)
      (check-original! checker :receive-awaiting receive)
      (check-original! checker :environment-awaiting environment)
      ;; A swapped kind in either original constructor must fail statically.
      ;; This would otherwise feed the wrong branch into the machine's
      ;; action-versus-suspension behavior.
      (check-rejected! checker :receive-labeled-environment receive
                       (walk/postwalk-replace {:receive :environment} receive))
      (check-rejected! checker :environment-labeled-receive environment
                       (walk/postwalk-replace {:environment :receive} environment))
      (check-rejected! checker :helper-returns-nil helper
                       (apply list (concat (butlast helper) [nil]))))
    true))

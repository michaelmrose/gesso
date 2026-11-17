(ns gesso.choreo.typed-machine-record-check
  "Source-backed optional Typed Clojure checking of machine execution record
   construction and accessors. Invoked by typed-machine-normalization-check;
   the normalizer gate supplies its common checking and audit callbacks."
  (:require [clojure.walk :as walk]
            [gesso.choreo.machine]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})

(defn- audit-machine-execution-record! [definition]
  ;; A structural map type does not enforce command/execution category.
  ;; Audit the original production projection into persisted machine fields.
  (let [arguments (nth definition 2 nil)
        body (last definition)
        bindings (when (and (seq? body) (= 'let (first body)))
                   (second body))
        locals (when (vector? bindings)
                 (into {} (map vec (partition 2 bindings))))
        assembly (when (seq? body) (last body))
        fields (when (and (seq? assembly) (= 'cond-> (first assembly)))
                 (second assembly))]
    (when-not (and (= '[{:keys [plan identity-bindings status state action
                                  awaiting knowledge history max-immediate-steps
                                  result]}]
                      arguments)
                   (= '(:command-id identity-bindings)
                      (get locals 'command-id))
                   (= '(:execution-id identity-bindings)
                      (get locals 'execution-id))
                   (map? fields)
                   (= 'identity-bindings (:identity-bindings fields))
                   (= '(:role plan) (:role fields))
                   (= 'command-id (:command-id fields))
                   (= 'execution-id (:execution-id fields))
                   (= '(vec history) (:history fields))
                   (= 'execution-type (:gesso.choreo.machine/type fields))
                   (= '(action (assoc :action action)
                         awaiting (assoc :awaiting awaiting)
                         (= :completed status) (assoc :result result))
                      (drop 2 assembly)))
      (throw (ex-info "Machine execution record identity projection changed."
                      {:error/kind :wrong-execution-record-projection})))
    true))

(defn- check-machine-execution-record! [checker definition {:keys [check-source! expect-audit-rejection!]}]
  ;; Public identity maps retain a literal :kind discriminator. Typed
  ;; Clojure HMaps can distinguish the command and execution categories
  ;; without changing the CLJ/CLJS wire representation. Bindings are sparse:
  ;; an absent identity is valid, while execution records always store both
  ;; keys (possibly nil).
  (let [command-tag '(typed.clojure/HMap
                      :mandatory {:gesso.choreo.identity/type
                                  (typed.clojure/Val :gesso.choreo.identity/ref)
                                  :gesso.choreo.identity/kind
                                  (typed.clojure/Val :command)
                                  :gesso.choreo.identity/value typed.clojure/Any}
                      :complete? false)
        execution-tag '(typed.clojure/HMap
                        :mandatory {:gesso.choreo.identity/type
                                    (typed.clojure/Val :gesso.choreo.identity/ref)
                                    :gesso.choreo.identity/kind
                                    (typed.clojure/Val :execution)
                                    :gesso.choreo.identity/value typed.clojure/Any}
                        :complete? false)
        command-opt (list 'typed.clojure/U nil command-tag)
        execution-opt (list 'typed.clojure/U nil execution-tag)
        binding-type (list 'typed.clojure/HMap :optional
                           {:command-id command-opt
                            :execution-id execution-opt}
                           :complete? false)
        input (list 'typed.clojure/HMap :mandatory
                    {:identity-bindings binding-type
                     :history '(typed.clojure/Seqable typed.clojure/Any)}
                    :complete? false)
        output (list 'typed.clojure/HMap :mandatory
                     {:gesso.choreo.machine/type
                      '(typed.clojure/Val :gesso.choreo.machine/execution)
                      :identity-bindings binding-type
                      :command-id command-opt
                      :execution-id execution-opt
                      :history '(typed.clojure/AVec typed.clojure/Any)}
                     :complete? false)
        replace-fields (fn [f transform]
                         (let [body (last f)
                               record (last body)
                               changed (list* 'cond-> (transform (second record))
                                              (drop 2 record))
                               new-body (apply list
                                               (concat (butlast body) [changed]))]
                           (apply list (concat (butlast f) [new-body]))))
        require-rejected! (fn [label mutated]
                            (when (= mutated definition)
                              (throw (ex-info "Mutation had no target." {:label label})))
                            (expect-audit-rejection!
                             audit-machine-execution-record! label mutated
                             :wrong-execution-record-projection))]
    ;; Typed Clojure's default core annotations omit compiler-desugared
    ;; to-array in Clojure map destructuring. This deliberately broad type
    ;; is an interop assumption, not an assurance claim about that function.
    (eval '(typed.clojure/ann clojure.core/to-array
             (typed.clojure/IFn [typed.clojure/Any :-> typed.clojure/Any])))
    (eval '(typed.clojure/ann gesso.choreo.machine/execution-type
             (typed.clojure/Val :gesso.choreo.machine/execution)))
    (eval (list 'typed.clojure/ann
                'gesso.choreo.machine/execution-record
                (list 'typed.clojure/IFn [input '-> output])))
    (binding [*ns* (the-ns 'gesso.choreo.machine)]
      (check-source! checker :production-execution-record-body definition)
      (audit-machine-execution-record! definition)
      (let [no-vector
            (replace-fields definition #(assoc % :history 'history))
            wrong-vector
            (replace-fields definition #(assoc % :history ':not-a-vector))]
        ;; Check the actual function body with only the changed expression.
        (doseq [[label mutated]
                [[:execution-history-not-vectorized no-vector]
                 [:execution-history-wrong-type wrong-vector]
                 [:execution-record-wrong-marker
                  (replace-fields definition
                                  #(assoc % :gesso.choreo.machine/type
                                            :not-an-execution))]]]
          (let [{:keys [ex type-errors]}
                (checker mutated :check-config config)
                kinds (mapv :type-error type-errors)]
            (when-not (and (nil? ex)
                           (seq kinds)
                           (every? #{:typed.clojure/type-mismatch-error} kinds))
              (throw (ex-info "History mutation escaped Typed Clojure."
                              {:label label :errors kinds} ex))))))
      ;; These are STATIC type failures in the original production defn,
      ;; not merely failures of the source-shape audit below.
      (doseq [[label mutation]
              [[:typed-command-reader-swapped
                (walk/postwalk-replace
                 {'(:command-id identity-bindings)
                  '(:execution-id identity-bindings)} definition)]
               [:typed-execution-reader-swapped
                (walk/postwalk-replace
                 {'(:execution-id identity-bindings)
                  '(:command-id identity-bindings)} definition)]
               [:typed-command-field-swapped
                (replace-fields definition #(assoc % :command-id 'execution-id))]
               [:typed-execution-field-swapped
                (replace-fields definition #(assoc % :execution-id 'command-id))]]]
        (when (= mutation definition)
          (throw (ex-info "Typed category mutation had no target."
                          {:label label})))
        (let [{:keys [ex type-errors]}
              (checker mutation :check-config config)
              kinds (mapv :type-error type-errors)]
          (when-not (and (nil? ex) (seq kinds)
                         (every? #{:typed.clojure/type-mismatch-error} kinds))
            (throw (ex-info "Category swap escaped Typed Clojure."
                            {:label label :errors kinds} ex)))))
      (doseq [[label changed]
              [[:command-reader-swapped
                (walk/postwalk-replace
                 {'(:command-id identity-bindings)
                  '(:execution-id identity-bindings)} definition)]
               [:execution-reader-swapped
                (walk/postwalk-replace
                 {'(:execution-id identity-bindings)
                  '(:command-id identity-bindings)} definition)]
               [:command-slot-swapped
                (replace-fields definition #(assoc % :command-id 'execution-id))]
               [:execution-slot-swapped
                (replace-fields definition #(assoc % :execution-id 'command-id))]
               [:raw-bindings-stored
                (replace-fields definition #(assoc % :identity-bindings '{}))]]]
        (require-rejected! label changed)))
    true))


(defn- check-machine-execution-accessors! [checker forms {:keys [check-source! check-rejection! source-definition]}]
  ;; Check original machine API bodies against the SAME discriminated map
  ;; representation used for the execution-record producer. This proves
  ;; correct field selection for typed inputs, including the disjoint
  ;; action/awaiting :kind families and vector history. Runtime execution?
  ;; validation continues to protect untyped public entry points and
  ;; establishes state-to-descriptor consistency that these types cannot.
  (let [command-tag '(typed.clojure/HMap
                      :mandatory {:gesso.choreo.identity/type
                                  (typed.clojure/Val :gesso.choreo.identity/ref)
                                  :gesso.choreo.identity/kind
                                  (typed.clojure/Val :command)
                                  :gesso.choreo.identity/value typed.clojure/Any}
                      :complete? false)
        execution-tag '(typed.clojure/HMap
                        :mandatory {:gesso.choreo.identity/type
                                    (typed.clojure/Val :gesso.choreo.identity/ref)
                                    :gesso.choreo.identity/kind
                                    (typed.clojure/Val :execution)
                                    :gesso.choreo.identity/value typed.clojure/Any}
                        :complete? false)
        command-opt (list 'typed.clojure/U nil command-tag)
        execution-opt (list 'typed.clojure/U nil execution-tag)
        bindings-tag (list 'typed.clojure/HMap :optional
                           {:command-id command-opt
                            :execution-id execution-opt}
                           :complete? false)
        ;; Executable actions and suspended alternatives have disjoint
        ;; semantic :kind values despite both remaining portable maps.
        action-tag (apply list 'typed.clojure/U
                          (for [kind [:local :authoritative :send]]
                            (list 'typed.clojure/HMap :mandatory
                                  {:kind (list 'typed.clojure/Val kind)}
                                  :complete? false)))
        awaiting-tag (apply list 'typed.clojure/U
                            (for [kind [:receive :environment]]
                              (list 'typed.clojure/HMap :mandatory
                                    {:kind (list 'typed.clojure/Val kind)}
                                    :complete? false)))
        action-opt (list 'typed.clojure/U nil action-tag)
        awaiting-opt (list 'typed.clojure/U nil awaiting-tag)
        history-tag '(typed.clojure/AVec typed.clojure/Any)
        record-tag (list 'typed.clojure/HMap :mandatory
                         {:gesso.choreo.machine/type
                          '(typed.clojure/Val :gesso.choreo.machine/execution)
                          :identity-bindings bindings-tag
                          :command-id command-opt
                          :execution-id execution-opt
                          :history history-tag}
                         :optional {:action action-opt
                                    :awaiting awaiting-opt}
                         :complete? false)
        names '[require-execution! identity-bindings command-id execution-id
                pending-action awaiting execution-history]
        originals (into {} (map (fn [n] [n (source-definition forms n)]) names))
        helper (get originals 'require-execution!)]
    (eval '(typed.clojure/ann gesso.choreo.machine/execution?
             (typed.clojure/IFn [typed.clojure/Any :-> typed.clojure/Bool])))
    (doseq [[name return-type]
            [['require-execution! record-tag]
             ['identity-bindings bindings-tag]
             ['command-id command-opt]
             ['execution-id execution-opt]
             ['pending-action action-opt]
             ['awaiting awaiting-opt]
             ['execution-history history-tag]]]
      (eval (list 'typed.clojure/ann
                  (symbol "gesso.choreo.machine" (str name))
                  (list 'typed.clojure/IFn [record-tag '-> return-type]))))
    (binding [*ns* (the-ns 'gesso.choreo.machine)]
      (doseq [name names]
        (check-source! checker [:production-machine-accessor name]
                       (get originals name)))
      (let [mutations
            [[:command-returns-execution
              (get originals 'command-id)
              (walk/postwalk-replace
               {:command-id :execution-id}
               (get originals 'command-id))]
             [:execution-returns-command
              (get originals 'execution-id)
              (walk/postwalk-replace
               {:execution-id :command-id}
               (get originals 'execution-id))]
             [:bindings-return-command
              (get originals 'identity-bindings)
              (walk/postwalk-replace
               {:identity-bindings :command-id}
               (get originals 'identity-bindings))]
             [:validator-returns-nil
              helper
              (apply list (concat (butlast helper) [nil]))]
             [:action-returns-awaiting
              (get originals 'pending-action)
              (walk/postwalk-replace
               {:action :awaiting}
               (get originals 'pending-action))]
             [:awaiting-returns-action
              (get originals 'awaiting)
              (walk/postwalk-replace
               {:awaiting :action}
               (get originals 'awaiting))]
             [:history-returns-awaiting
              (get originals 'execution-history)
              (walk/postwalk-replace
               {:history :awaiting}
               (get originals 'execution-history))]]]
        (doseq [[label original mutated] mutations]
          (when (= original mutated)
            (throw (ex-info "Machine accessor mutation had no target."
                            {:label label})))
          (check-rejection! checker label mutated
                            :typed.clojure/type-mismatch-error))))
    true))


(defn check! [checker forms helpers]
  ;; Extract and inspect the ORIGINAL production body, never a shadow copy.
  (let [record ((:source-definition helpers) forms 'execution-record)]
    (check-machine-execution-record! checker record helpers)
    (check-machine-execution-accessors! checker forms helpers))
  true)

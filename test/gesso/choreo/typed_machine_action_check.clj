(ns gesso.choreo.typed-machine-action-check
  "Opt-in Typed Clojure checks for original local/authoritative/send action
   producers. The source-derived descriptor literal is annotated in a temporary
   checking form; the production defn and runtime representation are unchanged."
  (:require [clojure.walk :as walk]
            [gesso.choreo.machine]
            [typed.clojure]))

(def ^:private config {:max-processors 1 :check-form-eval :never})
(def ^:private machine-namespace 'gesso.choreo.machine)
(def ^:private map-type '(typed.clojure/Map typed.clojure/Any typed.clojure/Any))

(def ^:private descriptor-contracts
  {:local
   {:kind '(typed.clojure/Val :local)
    :state 'typed.clojure/Any
    :role 'typed.clojure/Kw
    :action 'typed.clojure/Any}
   :authoritative
   {:kind '(typed.clojure/Val :authoritative)
    :state 'typed.clojure/Any
    :role 'typed.clojure/Kw
    :operation 'typed.clojure/Any}
   :send
   {:kind '(typed.clojure/Val :send)
    :state 'typed.clojure/Any
    :from 'typed.clojure/Kw
    :to 'typed.clojure/Kw
    :event 'typed.clojure/Kw}})

(defn- source-definition [forms name]
  (let [found (filter #(and (seq? %)
                            (#{'defn 'defn-} (first %))
                            (= name (second %))) forms)]
    (when-not (= 1 (count found))
      (throw (ex-info "Missing or duplicate production action constructor."
                      {:name name :count (count found)})))
    (first found)))

(defn- annotate-descriptor-literal [source category]
  ;; Unlike a handwritten example, this checks the actual map expression
  ;; passed to descriptor-identities by the original production function.
  ;; Fail closed if the original call or its literal argument disappears.
  (let [found (atom 0)
        type (list 'typed.clojure/HMap
                   :mandatory (get descriptor-contracts category)
                   :complete? false)
        modified
        (walk/postwalk
         (fn [form]
           (if (and (seq? form) (= 'descriptor-identities (first form)))
             (let [[_ descriptor bindings & more] form]
               (when-not (and (map? descriptor) (some? bindings)
                              (empty? more))
                 (throw (ex-info "Action descriptor source has changed shape."
                                 {:category category})))
               (swap! found inc)
               (list 'descriptor-identities
                     (list 'typed.clojure/ann-form descriptor type)
                     bindings))
             form))
         source)]
    (when-not (= 1 @found)
      (throw (ex-info "Expected exactly one original descriptor projection."
                      {:category category :count @found})))
    modified))

(defn- result [checker form]
  (binding [*ns* (the-ns machine-namespace)]
    (checker form :check-config config)))

(defn- check-source! [checker name form]
  (let [{:keys [ex type-errors]} (result checker form)]
    (when (or ex (seq type-errors))
      (throw (ex-info "Original action producer failed static checking."
                      {:name name :errors (mapv :type-error type-errors)} ex)))
    true))

(defn- check-mutation! [checker label original modified]
  (when (= original modified)
    (throw (ex-info "Action mutation had no target." {:label label})))
  (let [{:keys [ex type-errors]} (result checker modified)
        kinds (mapv :type-error type-errors)]
    (when-not (and (nil? ex) (seq kinds)
                   (every? #{:typed.clojure/type-mismatch-error} kinds))
      (throw (ex-info "Action mutation escaped Typed Clojure."
                      {:label label :errors kinds} ex)))
    true))

(defn- mutate-literal [original change]
  (walk/postwalk
   (fn [form]
     (if (and (seq? form) (= 'descriptor-identities (first form)))
       (list 'descriptor-identities (change (second form)) (nth form 2))
       form))
   original))

(defn check! [checker forms]
  (let [plan '(typed.clojure/HMap
               :mandatory {:role typed.clojure/Kw} :complete? false)
        values '(typed.clojure/Map typed.clojure/Kw typed.clojure/Any)
        nullable-keys '(typed.clojure/U
                        nil (typed.clojure/Set typed.clojure/Kw))
        local-state (list 'typed.clojure/HMap
                          :mandatory {:action 'typed.clojure/Any}
                          :optional {:requires nullable-keys
                                     :outputs nullable-keys}
                          :complete? false)
        authoritative-state (list 'typed.clojure/HMap
                                  :mandatory {:operation 'typed.clojure/Any}
                                  :optional {:requires nullable-keys
                                             :outputs nullable-keys}
                                  :complete? false)
        send-state '(typed.clojure/HMap
                     :mandatory {:to typed.clojure/Kw
                                 :event typed.clojure/Kw}
                     :optional {:via typed.clojure/Kw
                                :required (typed.clojure/Set typed.clojure/Kw)
                                :optional (typed.clojure/Set typed.clojure/Kw)
                                :correlation (typed.clojure/Set typed.clojure/Kw)
                                :open-payload? typed.clojure/Bool}
                     :complete? false)
        kw-set '(typed.clojure/Set typed.clojure/Kw)
        signatures [['local-action :local
                     [plan map-type 'typed.clojure/Any local-state values]]
                    ['authoritative-action :authoritative
                     [plan map-type 'typed.clojure/Any authoritative-state values]]
                    ['send-action :send
                     [plan map-type 'typed.clojure/Any send-state]]]
        sources (into {} (map (fn [[name _ _]]
                                [name (source-definition forms name)])
                              signatures))]
    ;; This general mapping matches the existing source-backed identity gate.
    ;; It is NOT assumed to preserve the action category. Instead we check
    ;; the actual source literal before it crosses that general map boundary.
    (eval (list 'typed.clojure/ann
                'gesso.choreo.machine/descriptor-identities
                (list 'typed.clojure/IFn [map-type map-type '-> map-type])))
    (doseq [[name return]
            [['contract-required kw-set]
             ['contract-optional kw-set]
             ['contract-correlation kw-set]
             ['contract-open-payload? 'typed.clojure/Bool]]]
      (eval (list 'typed.clojure/ann
                  (symbol "gesso.choreo.machine" (str name))
                  (list 'typed.clojure/IFn [send-state '-> return]))))
    (doseq [[name category params] signatures]
      (eval (list 'typed.clojure/ann
                  (symbol "gesso.choreo.machine" (str name))
                  (list 'typed.clojure/IFn
                        (vec (concat params ['-> map-type])))))
      (let [original (get sources name)
            instrumented (annotate-descriptor-literal original category)]
        ;; First check the untouched function body; then check the same
        ;; function with a type assertion around its ORIGINAL map literal.
        (check-source! checker [name :original-body] original)
        (check-source! checker [name :descriptor-fields] instrumented)
        (let [wrong-kind (case category
                           :local :send
                           :authoritative :receive
                           :send :authoritative)
              required (case category
                         :local :action
                         :authoritative :operation
                         :send :to)]
          (check-mutation!
           checker [name :wrong-kind] original
           (annotate-descriptor-literal
            (mutate-literal original #(assoc % :kind wrong-kind))
            category))
          (check-mutation!
           checker [name :missing-required-field] original
           (annotate-descriptor-literal
            (mutate-literal original #(dissoc % required))
            category))
          ;; Static contracts must reject invalid role and message labels,
          ;; rather than accepting every value as Any. State identifiers stay
          ;; opaque EDN values, as prescribed by Choreo's semantic model.
          (doseq [field (case category
                         :local [:role]
                         :authoritative [:role]
                         :send [:from :to :event])]
            (check-mutation!
             checker [name :non-keyword-descriptor-field field] original
             (annotate-descriptor-literal
              (mutate-literal original #(assoc % field "not-a-keyword"))
              category))))))
    true))

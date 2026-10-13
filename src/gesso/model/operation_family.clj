(ns gesso.model.operation-family
  "Explicit finite operation-family expansion for bounded authoritative
   invariant checking.

   A family declares an operation identifier, finite principal and argument
   domains, and ordinary Clojure enablement/preparation functions. Every
   principal/argument combination becomes a distinct operation *instance*
   for gesso.model.invariant-check/check-reachable. Plans are still rebuilt
   from each reached state; no transaction is executed here.

   This is a deliberately small source-owned coverage surface, not a domain
   language, operation registry, or proof that the chosen domains include all
   production principals, arguments, or operations. Functions, their policy
   dependencies, and their fidelity to real application execution remain
   trusted. A passing result is only a bounded finite-model finding.

   Example:

     (check-families state
       [{:id :request/claim
         :principals [:helper/a :helper/b]
         :arguments [{:request-id :request/one}]
         :enabled? (fn [state principal args] ...)
         :prepare  (fn [state principal args] ... tx plan ...)}]
       [{:id :request/owner-active :holds? (fn [state] ...)}]
       {:max-depth 3 :max-transitions 100})

   This expands to two inspectable, deterministic operation instances. Larger
   finite domains MUST be deliberately modeled in smaller checks or supported
   by a future checker with a higher explicit bound: they are not truncated."
  (:require [gesso.model.invariant-check :as invariant]))

(def maximum-instances 6)

(def family-assurance-scope
  {:checker :gesso.model.operation-family/finite-cross-product-v1
   :checked #{:explicit-finite-principal-domains
              :explicit-finite-argument-domains
              :full-declared-cartesian-product
              :distinct-choice-identities
              :state-dependent-callback-binding
              :bounded-reachable-rule-checking}
   :assumed #{:truthful-complete-declared-operation-families
              :truthful-complete-declared-principals-and-arguments
              :pure-total-truthful-callbacks
              :complete-finite-state-universe
              :serial-atomic-commit-semantics}
   :not-established #{:completeness-of-real-application-operation-registry
                      :completeness-of-real-principals-or-arguments
                      :authorization-of-declared-principals
                      :authenticity-of-supplied-identities
                      :source-derived-business-policy-dependencies
                      :truth-of-authored-business-rules
                      :correctness-of-arbitrary-clojure-callbacks
                      :unbounded-invariant-preservation
                      :real-xtdb-execution-correspondence
                      :application-certification}})

(defn- fail! [kind message details]
  (throw (ex-info message
                  {:error/type ::invalid-operation-family
                   :error/kind kind
                   :error/details details})))

(defn- portable-value?
  "Restrict enumerated choices to a finite, inspectable subset of EDN.
   Floats, records, host objects and functions are intentionally excluded.
   Depth and size bounds avoid pathological choice descriptions."
  [value depth]
  (and (<= depth 8)
       (cond
         (or (nil? value) (boolean? value) (string? value)
             (keyword? value) (integer? value)
             (instance? java.util.UUID value))
         true

         (vector? value)
         (and (<= (count value) 64)
              (every? #(portable-value? % (inc depth)) value))

         (map? value)
         (and (<= (count value) 64)
              (every? keyword? (keys value))
              (every? #(portable-value? % (inc depth)) (vals value)))

         :else false)))

(defn- require-domain! [family-id label domain value?]
  (when-not (and (vector? domain)
                 (seq domain)
                 (every? value? domain))
    (fail! :invalid-domain
           "An operation family needs a nonempty vector of inspectable choices."
           {:family-id family-id :domain label :values domain}))
  (when-not (= (count domain) (count (distinct domain)))
    (fail! :duplicate-domain-choice
           "Duplicate principal or argument choices make enumeration misleading."
           {:family-id family-id :domain label :values domain})))

(defn- require-families! [families]
  (when-not (and (vector? families) (seq families)
                 (<= (count families) maximum-instances))
    (fail! :invalid-families
           "Specify a nonempty vector of at most six operation families."
           {:maximum maximum-instances :families families}))
  (doseq [[index family] (map-indexed vector families)]
    (when-not (and (map? family)
                   (= #{:id :principals :arguments :enabled? :prepare}
                      (set (keys family)))
                   (qualified-keyword? (:id family))
                   (fn? (:enabled? family))
                   (fn? (:prepare family)))
      (fail! :invalid-family
             "Each family requires exactly :id, :principals, :arguments, :enabled?, and :prepare."
             {:index index :family family}))
    (require-domain! (:id family) :principals (:principals family)
                     #(portable-value? % 0))
    (require-domain! (:id family) :arguments (:arguments family)
                     #(and (map? %) (portable-value? % 0))))
  (when-not (= (count families) (count (set (map :id families))))
    (fail! :duplicate-family
           "Every operation family must have a unique qualified identifier."
           {:ids (mapv :id families)}))
  (let [size (reduce + (map #(* (count (:principals %))
                                (count (:arguments %))) families))]
    (when (> size maximum-instances)
      (fail! :instance-bound-exceeded
             "The full principal/argument product exceeds the reachable checker's six-instance bound; no choices were omitted or checked."
             {:expanded-instances size :maximum maximum-instances})))
  families)

(defn expand-families
  "Validate and fully expand finite operation families into instances for
   invariant/check-reachable. Result is {:operations [...], :choices [...]},
   with corresponding entries in the same deterministic order.

   Each choice records its declared family, principal, and argument map. Its
   synthetic :instance-id is deterministic for the supplied ordered domains;
   it is NOT a durable application command ID. Reordering input domains may
   change instance IDs. This function does not invoke any callbacks."
  [families]
  (require-families! families)
  (let [pairs (vec
               (for [[family-index family] (map-indexed vector families)
                     [principal-index principal] (map-indexed vector (:principals family))
                     [argument-index arguments] (map-indexed vector (:arguments family))]
                 (let [instance-id (keyword "gesso.model.operation-family"
                                            (str "choice-" family-index "-"
                                                 principal-index "-" argument-index))]
                   {:choice {:instance-id instance-id
                             :family-id (:id family)
                             :principal principal
                             :arguments arguments
                             :family-index family-index
                             :principal-index principal-index
                             :argument-index argument-index}
                    :operation {:id instance-id
                                :enabled? (fn [state]
                                            ((:enabled? family) state principal arguments))
                                :prepare (fn [state]
                                           ((:prepare family) state principal arguments))}})))]
    {:operations (mapv :operation pairs)
     :choices (mapv :choice pairs)}))

(defn check-families
  "Run bounded state-dependent invariant checking for every declared family
   choice. Returns the existing reachable verdict plus :family-expansion,
   which maps each synthetic instance ID back to its family and inputs, and
   :family-scope documenting the limited coverage claim.

   Counterexample :family-trace entries correspond to :trace positions and
   identify the actual declared principal and arguments. The result includes
   potentially sensitive input values and entire abstract states: it is a
   local diagnostic, NOT a redacted public application-assurance report.

   The underlying :max-depth and :max-transitions bounds remain mandatory.
   No verdict is returned if family expansion or reachability checking fails."
  [initial-state families rules options]
  (let [{:keys [operations choices]} (expand-families families)
        verdict (invariant/check-reachable initial-state operations rules options)
        by-id (into {} (map (juxt :instance-id identity)) choices)
        violations (mapv (fn [violation]
                           (assoc violation
                                  :family-trace
                                  (mapv (fn [step]
                                          (get by-id (:operation-id step)))
                                        (:trace violation))))
                         (:violations verdict))]
    (assoc verdict
           :violations violations
           :family-expansion {:instances choices
                              :instance-count (count choices)
                              :family-ids (mapv :id families)}
           :family-scope family-assurance-scope)))

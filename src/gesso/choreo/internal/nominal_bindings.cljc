(ns gesso.choreo.internal.nominal-bindings
  "Internal conversion boundary between portable Choreo tagged identity maps
   and distinct nominal identity categories. A machine execution still stores
   and exports tagged maps; this module constructs nominal values exclusively
   for checked internal consumers. The downstream authoring API is unchanged.

   The public tagged validators remain authoritative on ingress and egress;
   identity extensions are preserved in memory, not added to the closed wire
   representation. Nominal identity is not an authentication assertion."
  (:require [gesso.choreo.identity :as tagged]
            [gesso.choreo.internal.nominal-identity :as internal]))

(def ^:private canonical-keys
  #{:gesso.choreo.identity/type
    :gesso.choreo.identity/kind
    :gesso.choreo.identity/value})

(defn tagged->nominal
  "Validate and preserve every field of an open tagged identity map.

   The nominal category remains distinct; extension keys stay opaque and
   cannot overwrite the three mandatory public identity fields."
  [reference]
  (when-not (tagged/identity? reference)
    (throw (ex-info "Expected a valid tagged Choreo identity."
                    {:error/kind :invalid-tagged-identity
                     :value reference})))
  (let [raw (tagged/raw-value reference)
        extensions (reduce dissoc reference canonical-keys)]
    (case (tagged/kind reference)
      :principal (internal/->Principal raw extensions)
      :actor (internal/->Actor raw extensions)
      :authority (internal/->Authority raw extensions)
      :host (internal/->Host raw extensions)
      :command (internal/->CommandId raw extensions)
      :execution (internal/->ExecutionId raw extensions))))

(defn- restore-tagged
  "Reject an invalid nominal extension payload instead of overwriting kind."
  [constructor raw extensions]
  (when-not (and (map? extensions)
                 (empty? (select-keys extensions canonical-keys)))
    (throw (ex-info "Nominal identity extensions must be an independent map."
                    {:error/kind :invalid-nominal-extensions
                     :extensions extensions})))
  ;; The existing public constructor rechecks nil and nested-identity raw
  ;; values before we restore the independent extension fields.
  (into (constructor raw) extensions))

(defn nominal->tagged
  "Turn any of the six nominal identities into the original open tagged map.

   Opaque extension fields survive. Direct nominal construction cannot
   overwrite the identity category or bypass the original raw validation."
  [reference]
  (cond
    (instance? gesso.choreo.internal.nominal_identity.Principal reference)
    (restore-tagged tagged/principal (.-raw ^gesso.choreo.internal.nominal_identity.Principal reference) (.-extensions ^gesso.choreo.internal.nominal_identity.Principal reference))

    (instance? gesso.choreo.internal.nominal_identity.Actor reference)
    (restore-tagged tagged/actor (.-raw ^gesso.choreo.internal.nominal_identity.Actor reference) (.-extensions ^gesso.choreo.internal.nominal_identity.Actor reference))

    (instance? gesso.choreo.internal.nominal_identity.Authority reference)
    (restore-tagged tagged/authority (.-raw ^gesso.choreo.internal.nominal_identity.Authority reference) (.-extensions ^gesso.choreo.internal.nominal_identity.Authority reference))

    (instance? gesso.choreo.internal.nominal_identity.Host reference)
    (restore-tagged tagged/host (.-raw ^gesso.choreo.internal.nominal_identity.Host reference) (.-extensions ^gesso.choreo.internal.nominal_identity.Host reference))

    (instance? gesso.choreo.internal.nominal_identity.CommandId reference)
    (restore-tagged tagged/command-id (.-raw ^gesso.choreo.internal.nominal_identity.CommandId reference) (.-extensions ^gesso.choreo.internal.nominal_identity.CommandId reference))

    (instance? gesso.choreo.internal.nominal_identity.ExecutionId reference)
    (restore-tagged tagged/execution-id (.-raw ^gesso.choreo.internal.nominal_identity.ExecutionId reference) (.-extensions ^gesso.choreo.internal.nominal_identity.ExecutionId reference))

    :else
    (throw (ex-info "Expected one of the six nominal identity categories."
                    {:error/kind :invalid-nominal-identity
                     :value reference}))))

(defn- tagged-binding-entry
  "Convert one nominal entry, without bypassing the final public validator."
  [acc key value]
  (assoc acc key (if (= key :role)
                   (if (keyword? value)
                     value
                     (throw (ex-info "Expected a keyword for :role."
                                     {:error/kind :invalid-role
                                      :binding key :value value})))
                   (if (keyword? value)
                     (throw (ex-info "Expected a nominal identity in binding."
                                     {:error/kind :invalid-nominal-identity
                                      :binding key :value value}))
                     (nominal->tagged value)))))

(defn bindings->nominal
  "Convert validated tagged bindings to nominal
   values with independently checkable category-specific output slots.
   Uses ordinary defn and fixed-key if-let, not special function syntax.
   The existing tagged/bindings validator remains authoritative on input."
  [bindings]
  (let [validated (tagged/bindings bindings)
        out (if-let [role (:role validated)]
              (if (keyword? role)
                (assoc {} :role role)
                (throw (ex-info "Invalid binding role."
                                {:error/kind :invalid-role})))
              {})
        out (if-let [value (:principal validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.Principal
                               nominal-value)
                  (assoc out :principal nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :principal}))))
              out)
        out (if-let [value (:actor validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.Actor
                               nominal-value)
                  (assoc out :actor nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :actor}))))
              out)
        out (if-let [value (:authority validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.Authority
                               nominal-value)
                  (assoc out :authority nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :authority}))))
              out)
        out (if-let [value (:host validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.Host
                               nominal-value)
                  (assoc out :host nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :host}))))
              out)
        out (if-let [value (:command-id validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.CommandId
                               nominal-value)
                  (assoc out :command-id nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :command-id}))))
              out)
        out (if-let [value (:execution-id validated)]
              (let [nominal-value
                    (tagged->nominal
                     (if (map? value)
                       value
                       (throw (ex-info "Invalid tagged binding value."
                                       {:error/kind :invalid-tagged-identity}))))]
                (if (instance? gesso.choreo.internal.nominal_identity.ExecutionId
                               nominal-value)
                  (assoc out :execution-id nominal-value)
                  (throw (ex-info "Binding category mismatch."
                                  {:error/kind :invalid-binding
                                   :binding :execution-id}))))
              out)
        ]
    out))

(defn bindings->tagged
  "Convert a sparse internal binding map back into public tagged data.

   Revalidate the completed public binding map: a nominal Actor in the
   :principal slot is still a category error, not automatically accepted."
  [bindings]
  (when-not (map? bindings)
    (throw (ex-info "Nominal identity bindings must be a map."
                    {:error/kind :invalid-nominal-bindings
                     :value bindings})))
  (tagged/bindings
   (reduce-kv tagged-binding-entry
              {}
              bindings)))


;; These are deliberately category-specific internal consumers, not generic
;; tag inspectors. Ordinary defn preconditions ensure independently invoked
;; functions reject accidental substitution even from unchecked Clojure.
(defn command-id->tagged
  [^gesso.choreo.internal.nominal_identity.CommandId command-id]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.CommandId
                    command-id)]}
  (nominal->tagged command-id))

(defn execution-id->tagged
  [^gesso.choreo.internal.nominal_identity.ExecutionId execution-id]
  {:pre [(instance? gesso.choreo.internal.nominal_identity.ExecutionId
                    execution-id)]}
  (nominal->tagged execution-id))

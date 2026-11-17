(ns gesso.choreo.typed-nominal-identity-bridge
  "Isolated compatibility experiment between current tagged Choreo identity
   maps and the optional six-type nominal pilot. No production representation,
   serialized format, or downstream public API is changed.

   Existing tagged identities may contain arbitrary extension fields. The
   nominal pilot retains those fields as opaque immutable data and uses them
   in equality/hashing, so no accidental data loss occurs during migration.
   The closed public wire format remains unchanged and deliberately does not
   serialize identity extension fields."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as tagged]
            [gesso.choreo.typed-nominal-identity-pilot :as nominal]))

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
      :principal (nominal/->Principal raw extensions)
      :actor (nominal/->Actor raw extensions)
      :authority (nominal/->Authority raw extensions)
      :host (nominal/->Host raw extensions)
      :command (nominal/->CommandId raw extensions)
      :execution (nominal/->ExecutionId raw extensions))))

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
    (instance? gesso.choreo.typed_nominal_identity_pilot.Principal reference)
    (restore-tagged tagged/principal (.-raw ^gesso.choreo.typed_nominal_identity_pilot.Principal reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.Principal reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Actor reference)
    (restore-tagged tagged/actor (.-raw ^gesso.choreo.typed_nominal_identity_pilot.Actor reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.Actor reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Authority reference)
    (restore-tagged tagged/authority (.-raw ^gesso.choreo.typed_nominal_identity_pilot.Authority reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.Authority reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Host reference)
    (restore-tagged tagged/host (.-raw ^gesso.choreo.typed_nominal_identity_pilot.Host reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.Host reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.CommandId reference)
    (restore-tagged tagged/command-id (.-raw ^gesso.choreo.typed_nominal_identity_pilot.CommandId reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.CommandId reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.ExecutionId reference)
    (restore-tagged tagged/execution-id (.-raw ^gesso.choreo.typed_nominal_identity_pilot.ExecutionId reference) (.-extensions ^gesso.choreo.typed_nominal_identity_pilot.ExecutionId reference))

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
  "Canonical pilot adapter converting validated tagged bindings to nominal
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.Principal
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.Actor
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.Authority
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.Host
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.CommandId
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
                (if (instance? gesso.choreo.typed_nominal_identity_pilot.ExecutionId
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

(defn- error-kind [thunk]
  (try
    (thunk)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) e
      (:error/kind (ex-data e)))))

(def ^:private cases
  [[:principal tagged/principal nominal/principal]
   [:actor tagged/actor nominal/actor]
   [:authority tagged/authority nominal/authority]
   [:host tagged/host nominal/host]
   [:command tagged/command-id nominal/command-id]
   [:execution tagged/execution-id nominal/execution-id]])

(deftest canonical-identities-round-trip-with-category-preserved
  (doseq [[kind tag-fn nominal-fn] cases
          raw ["text" 42 [:opaque 7] {:opaque :data}]]
    (testing (str "round trip " kind " with raw " (pr-str raw))
      (let [tagged (tag-fn raw)
            converted (tagged->nominal tagged)
            recovered (nominal->tagged converted)]
        (is (instance? (type (nominal-fn raw)) converted))
        (is (= tagged recovered))
        (is (= raw (gesso.choreo.identity/raw-value recovered)))
        (is (= tagged
               (gesso.choreo.identity/decode-wire
                (gesso.choreo.identity/encode-wire recovered))))
        (is (gesso.choreo.identity/wire-identity?
             (gesso.choreo.identity/encode-wire recovered)))))))

(deftest invalid-identities-and-extension-collisions-are-rejected
  (doseq [[kind tag-fn _] cases]
    (let [base (tag-fn [:opaque kind])
          extended (assoc base :extension :must-not-disappear)
          nil-raw (assoc base :gesso.choreo.identity/value nil)
          nested-raw (assoc base :gesso.choreo.identity/value
                            (tagged/principal :inner))]
      (is (tagged/identity? extended))
      (is (= extended (nominal->tagged (tagged->nominal extended))))
      (is (= :invalid-tagged-identity
             (error-kind #(tagged->nominal nil-raw))))
      (is (= :invalid-tagged-identity
             (error-kind #(tagged->nominal nested-raw))))))
  (doseq [invalid [nil :role "raw" 12 {}
                   {:gesso.choreo.identity/kind :principal}]]
    (is (= :invalid-tagged-identity
           (error-kind #(tagged->nominal invalid))))
    (is (= :invalid-nominal-identity
           (error-kind #(nominal->tagged invalid)))))
  ;; The public constructor remains the authority for raw-value validity.
  (is (= :nested-identity
         (error-kind #(nominal->tagged
                       (nominal/principal (tagged/actor :nested)))))))


(deftest extension-fields-and-equality-survive-losslessly
  (let [extensions [{:extension :kept}
                    {:extension nil :notes {:nested [:preserved]}}
                    {[:arbitrary :map-key] 7 :flag false}
                    {:extension (tagged/actor :also-allowed)}]]
    (doseq [[kind tag-fn _] cases
            extra extensions]
      (testing (str "open " kind " keys " (pr-str (keys extra)))
        (let [open (merge (tag-fn :same) extra)
              nominal-id (tagged->nominal open)
              roundtrip (nominal->tagged nominal-id)]
          (is (tagged/identity? open))
          (is (= open roundtrip))
          (is (= (hash open) (hash roundtrip)))
          (is (= (hash nominal-id) (hash (tagged->nominal open))))
          (is (= nominal-id (tagged->nominal open)))
          (is (not= nominal-id (tagged->nominal (tag-fn :same))))
          ;; The *existing* closed wire protocol intentionally omits extras.
          (is (= (tag-fn :same)
                 (tagged/decode-wire (tagged/encode-wire roundtrip))))))))
  ;; Equal raw identity, different extension data => unequal maps/nominals.
  (doseq [[_ tag-fn _] cases]
    (let [a (tagged->nominal (assoc (tag-fn 42) :extension 1))
          b (tagged->nominal (assoc (tag-fn 42) :extension 2))]
      (is (not= a b))
      (is (= 2 (count (hash-set a b))))))
  ;; Check the *full* extension-bearing binding map and correct category.
  (let [open (assoc (tagged/principal :one) :preserved {:a 1})]
    (is (= {:principal open :role :browser}
           (-> {:principal open :role :browser}
               bindings->nominal
               bindings->tagged)))))

(deftest invalid-nominal-extension-payloads-are-rejected
  (let [raw :id]
    (doseq [extensions [nil 7 []
                        {:gesso.choreo.identity/kind :actor}
                        {:gesso.choreo.identity/value :override}
                        {:gesso.choreo.identity/type :wrong}]]
      (is (= :invalid-nominal-extensions
             (error-kind #(nominal->tagged
                           (nominal/->Principal raw extensions))))))
    ;; Empty or arbitrary extension maps are allowed, including nil values.
    (is (= (tagged/principal raw)
           (nominal->tagged (nominal/->Principal raw {}))))
    (is (= (assoc (tagged/principal raw) :extra nil)
           (nominal->tagged (nominal/->Principal raw {:extra nil}))))))

(deftest sparse-binding-maps-convert-without-confusing-roles-or-identities
  (let [all {:role :server
             :principal (tagged/principal "user-42")
             :actor (tagged/actor "member-7")
             :authority (tagged/authority :requests)
             :host (tagged/host "node-3")
             :command-id (tagged/command-id "claim-21")
             :execution-id (tagged/execution-id "attempt-1")}
        sparse [{}, {:role :server}, {:principal (:principal all)}
                {:command-id (:command-id all)
                 :execution-id (:execution-id all)}
                all]]
    (doseq [input sparse]
      (let [internal (bindings->nominal input)]
        (is (= input (bindings->tagged internal)))
        (is (= (set (keys input)) (set (keys internal))))
        (when (contains? input :role)
          (is (= (:role input) (:role internal))))))
    (is (= :invalid-binding
           (error-kind #(bindings->nominal
                          {:principal (tagged/actor "member-7")}))))
    (is (= :invalid-binding
           (error-kind #(bindings->tagged
                          {:principal (nominal/actor "member-7")}))))
    (is (= :invalid-binding
           (error-kind #(bindings->tagged
                          {:execution-id (nominal/command-id "claim-21")}))))
    (is (= :invalid-role
           (error-kind #(bindings->tagged {:role 42}))))
    (is (= :unknown-binding
           (error-kind #(bindings->nominal {:unsupported :value}))))
    (is (= :unknown-binding
           (error-kind #(bindings->tagged {:unsupported (nominal/actor :value)}))))
    (let [extended (assoc (:principal all) :extra 1)]
      (is (= {:principal extended}
             (bindings->tagged
              (bindings->nominal {:principal extended})))))))

(deftest every-nominal-category-must-match-its-binding-slot
  (let [slots [:principal :actor :authority :host :command-id :execution-id]
        factories [nominal/principal nominal/actor nominal/authority
                   nominal/host nominal/command-id nominal/execution-id]]
    (doseq [[slot expected-ctor] (map vector slots factories)
            [source-slot ctor] (map vector slots factories)]
      (let [value (ctor :same-raw-id)
            result (error-kind #(bindings->tagged {slot value}))]
        (if (= slot source-slot)
          (do
            (is (nil? result))
            (is (= {slot (nominal->tagged (expected-ctor :same-raw-id))}
                   (bindings->tagged {slot value}))))
          (is (= :invalid-binding result)))))))

(deftest all-sparse-binding-subsets-survive-round-trip
  ;; Seven independent binding slots means 128 sparse presence combinations.
  ;; This checks that neither converter invents missing bindings, drops a
  ;; present role, or strips extensions from an individual identity.
  (let [all {:role :server
             :principal (assoc (tagged/principal :user) :metadata {:a 1})
             :actor (tagged/actor :member)
             :authority (tagged/authority :requests)
             :host (tagged/host :node)
             :command-id (tagged/command-id :intent)
             :execution-id (assoc (tagged/execution-id :attempt) :note nil)}
        slots [:role :principal :actor :authority :host
               :command-id :execution-id]]
    (doseq [mask (range 128)]
      (let [selected (into {}
                           (for [[idx slot] (map-indexed vector slots)
                                 :when (bit-test mask idx)]
                             [slot (get all slot)]))
            internal (bindings->nominal selected)
            restored (bindings->tagged internal)]
        (is (= selected restored) (str "sparse subset " mask))
        (is (= (set (keys selected)) (set (keys internal))))
        (is (= (count selected) (count internal)))))))

(deftest canonical-nominal-output-matches-independent-reference
  ;; Independently compute the former generic reduce-kv result *in the test*
  ;; for all 128 presence patterns. Only the precise implementation remains
  ;; in the actual pilot adapter, so the behavior cannot silently regress.
  (let [all {:role :server
             :principal (assoc (tagged/principal :user) :extra {:a 1})
             :actor (tagged/actor :member)
             :authority (tagged/authority :requests)
             :host (tagged/host :node)
             :command-id (tagged/command-id :intent)
             :execution-id (assoc (tagged/execution-id :attempt) :extra nil)}
        slots [:role :principal :actor :authority :host
               :command-id :execution-id]]
    (doseq [mask (range 128)]
      (let [input (into {}
                        (for [[index slot] (map-indexed vector slots)
                              :when (bit-test mask index)]
                          [slot (get all slot)]))
            canonical (bindings->nominal input)
            reference (reduce-kv
                       (fn [acc key value]
                         (assoc acc key
                                (if (= key :role)
                                  value
                                  (tagged->nominal value))))
                       {}
                       (tagged/bindings input))]
        (is (= reference canonical))
        (is (= input (bindings->tagged canonical)))
        (is (= (set (keys input)) (set (keys canonical))))))))

(deftest canonical-nominal-output-rejects-incorrect-binding-categories
  (let [slots [:principal :actor :authority :host :command-id :execution-id]
        factories [tagged/principal tagged/actor tagged/authority
                   tagged/host tagged/command-id tagged/execution-id]]
    (doseq [[slot expected] (map vector slots factories)
            [other-slot actual] (map vector slots factories)]
      (let [input {slot (actual :id)}
            error (error-kind #(bindings->nominal input))]
        (if (= slot other-slot)
          (is (= nil error))
          (is (= :invalid-binding error)))))
    (is (= :invalid-role
           (error-kind #(bindings->nominal {:role 42}))))
    (is (= :unknown-binding
           (error-kind #(bindings->nominal {:unknown :value}))))))

(deftest tagged-identity-categories-are-not-interchangeable-in-bindings
  (let [slots [:principal :actor :authority :host :command-id :execution-id]
        taggers [tagged/principal tagged/actor tagged/authority
                 tagged/host tagged/command-id tagged/execution-id]
        nominal-factories [nominal/principal nominal/actor nominal/authority
                           nominal/host nominal/command-id nominal/execution-id]]
    (doseq [[slot nominal-fn] (map vector slots nominal-factories)
            [source-slot tag-fn] (map vector slots taggers)]
      (let [input {slot (tag-fn :shared-id)}
            result (error-kind #(bindings->nominal input))]
        (if (= slot source-slot)
          (do
            (is (nil? result))
            (is (= (type (nominal-fn :shared-id))
                   (type (get (bindings->nominal input) slot)))))
          (is (= :invalid-binding result)))))))

(deftest malformed-binding-containers-fail-closed
  (doseq [invalid [nil [] :role 42 "text" [[:role :server]]]]
    (is (= :invalid-bindings
           (error-kind #(bindings->nominal invalid))))
    (is (= :invalid-nominal-bindings
           (error-kind #(bindings->tagged invalid)))))
  (doseq [invalid [nil 42 {} (nominal/principal :id)]]
    (is (= :invalid-role
           (error-kind #(bindings->tagged {:role invalid}))))))

#?(:clj
   (defn -main [& _]
     (let [result (clojure.test/run-tests
                   'gesso.choreo.typed-nominal-identity-bridge)]
       (when (pos? (+ (:fail result) (:error result)))
         (throw (ex-info "Nominal bridge pilot failed." result)))
       :ok)))

#?(:cljs
   (defn -main [& _]
     (cljs.test/run-tests 'gesso.choreo.typed-nominal-identity-bridge)))

#?(:cljs (set! *main-cli-fn* -main))

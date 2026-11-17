(ns gesso.choreo.typed-nominal-identity-bridge
  "Isolated compatibility experiment between current tagged Choreo identity
   maps and the optional six-type nominal pilot. No production representation,
   serialized format, or downstream public API is changed.

   An existing tagged identity may contain extra fields. The prototype's
   nominal values only retain raw data, so converting such a map would lose
   information. Reject that conversion explicitly until a lossless migration
   contract is chosen; never silently discard the extension."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as tagged]
            [gesso.choreo.typed-nominal-identity-pilot :as nominal]))

(def ^:private canonical-keys
  #{:gesso.choreo.identity/type
    :gesso.choreo.identity/kind
    :gesso.choreo.identity/value})

(defn tagged->nominal
  "Accept only a valid *canonical* tagged reference; preserve category/raw.

   Extension fields accepted by today's public identity? predicate cannot
   survive the existing one-field nominal prototype. Fail before converting.
   This is an explicit compatibility blocker, not a restriction on the public
   tagged-map API."
  [reference]
  (when-not (tagged/identity? reference)
    (throw (ex-info "Expected a valid tagged Choreo identity."
                    {:error/kind :invalid-tagged-identity
                     :value reference})))
  (when-not (= canonical-keys (set (keys reference)))
    (throw (ex-info "Identity extension fields cannot be represented losslessly."
                    {:error/kind :unrepresentable-identity-extension
                     :value reference})))
  (let [raw (tagged/raw-value reference)]
    (case (tagged/kind reference)
      :principal (nominal/principal raw)
      :actor (nominal/actor raw)
      :authority (nominal/authority raw)
      :host (nominal/host raw)
      :command (nominal/command-id raw)
      :execution (nominal/execution-id raw))))

(defn nominal->tagged
  "Turn a nominal identity into its original canonical tagged representation.

   Deliberately accepts no untyped maps, roles, or other nominal categories.
   The original constructor rechecks raw-value restrictions (including a
   nested tagged identity), even if someone used a direct nominal constructor."
  [reference]
  (cond
    (instance? gesso.choreo.typed_nominal_identity_pilot.Principal reference)
    (tagged/principal (.-raw reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Actor reference)
    (tagged/actor (.-raw reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Authority reference)
    (tagged/authority (.-raw reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.Host reference)
    (tagged/host (.-raw reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.CommandId reference)
    (tagged/command-id (.-raw reference))

    (instance? gesso.choreo.typed_nominal_identity_pilot.ExecutionId reference)
    (tagged/execution-id (.-raw reference))

    :else
    (throw (ex-info "Expected one of the six nominal identity categories."
                    {:error/kind :invalid-nominal-identity
                     :value reference}))))

(defn bindings->nominal
  "Convert a validated, sparse public binding map to internal nominal values.

   A role is already a keyword and remains unchanged. Every other binding
   passes through the public validator before a category-specific conversion.
   Extended identity maps are rejected explicitly, never truncated."
  [bindings]
  (reduce-kv (fn [acc key value]
               (assoc acc key (if (= key :role)
                                value
                                (tagged->nominal value))))
             {}
             (tagged/bindings bindings)))

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
   (reduce-kv (fn [acc key value]
                (assoc acc key (if (= key :role)
                                 value
                                 (nominal->tagged value))))
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

(deftest invalid-or-lossy-identities-are-rejected
  (doseq [[kind tag-fn _] cases]
    (let [base (tag-fn [:opaque kind])
          extended (assoc base :extension :must-not-disappear)
          nil-raw (assoc base :gesso.choreo.identity/value nil)
          nested-raw (assoc base :gesso.choreo.identity/value
                            (tagged/principal :inner))]
      (is (tagged/identity? extended))
      (is (= :unrepresentable-identity-extension
             (error-kind #(tagged->nominal extended))))
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
    (is (= :unrepresentable-identity-extension
           (error-kind #(bindings->nominal
                          {:principal (assoc (:principal all) :extra 1)}))))))

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

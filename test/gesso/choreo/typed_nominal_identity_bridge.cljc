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

(defn bindings->nominal
  "Convert a validated, sparse public binding map to internal nominal values.

   A role is already a keyword and remains unchanged. Every other binding
   passes through the public validator before a category-specific conversion.
   Extension fields are preserved instead of discarded."
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

(ns gesso.choreo.typed-nominal-identity-bridge
  "Cross-platform behavior tests for the production-internal nominal binding
   adapter. The adapter itself lives under src/, not in this namespace."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.identity :as tagged]
            [gesso.choreo.internal.nominal-identity :as internal]
            [gesso.choreo.internal.nominal-bindings
             :refer [tagged->nominal nominal->tagged
                     bindings->nominal bindings->tagged]]
            [gesso.choreo.typed-nominal-identity-pilot :as nominal]))

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
                           (internal/->Principal raw extensions))))))
    ;; Empty or arbitrary extension maps are allowed, including nil values.
    (is (= (tagged/principal raw)
           (nominal->tagged (internal/->Principal raw {}))))
    (is (= (assoc (tagged/principal raw) :extra nil)
           (nominal->tagged (internal/->Principal raw {:extra nil}))))))

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

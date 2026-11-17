(ns gesso.choreo.typed-nominal-machine-boundary-test
  "Verify the production-internal nominal conversion boundary against real
   Choreo machine executions, without changing their portable map format."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.core :as choreo]
            [gesso.choreo.identity :as tagged]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.choreo.internal.nominal-bindings :as internal-bindings]
            [gesso.choreo.internal.machine-nominal-bindings :as machine-bindings]
            [gesso.choreo.typed-nominal-identity-pilot :as nominal]))

(defn- machine-plan []
  (project/project
   (choreo/->choreography
    {:initial :prepare
     :states {:prepare (choreo/local :server :prepare :done)
              :done (choreo/return :done)}})
   :server))

(defn- error-kind [thunk]
  (try
    (thunk)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) ex
      (:error/kind (ex-data ex)))))

(def ^:private slots
  [:role :principal :actor :authority :host :command-id :execution-id])

(defn- reference-bindings []
  {:role :server
   :principal (assoc (tagged/principal "user-1") :extra {:source :app})
   :actor (tagged/actor "helper-2")
   :authority (tagged/authority :request-model)
   :host (tagged/host "aleph-node-1")
   :command-id (assoc (tagged/command-id "command-3") :label :intent)
   :execution-id (assoc (tagged/execution-id "attempt-4") :label :attempt)})

(deftest machine-start-to-nominal-is-lossless-for-all-sparse-bindings
  (let [plan (machine-plan)
        all (reference-bindings)]
    (doseq [mask (range 128)]
      (let [input (into {}
                        (for [[index slot] (map-indexed vector slots)
                              :when (bit-test mask index)]
                          [slot (get all slot)]))
            execution (machine/start plan {:identity-bindings input})
            public (machine/identity-bindings execution)
            internals (machine-bindings/execution->nominal-bindings execution)
            action (machine/pending-action execution)]
        (testing (str "real machine boundary, sparse subset " mask)
          (is (machine/execution? execution))
          (is (= (assoc input :role :server) public))
          (is (= public (internal-bindings/bindings->tagged internals)))
          (is (= (set (keys public)) (set (keys internals))))
          (is (= (:command-id public) (machine/command-id execution)))
          (is (= (:execution-id public) (machine/execution-id execution)))
          (is (machine/waiting-local? execution))
          (is (= (:execution-id public) (:execution-id action)))
          (is (= (:command-id public) (:command-id action)))
          (is (= (not= #{:role} (set (keys public)))
                 (contains? action :identity-bindings)))
          (when (contains? action :identity-bindings)
            (is (= public (:identity-bindings action))))
          (is (not (machine/has-execution-value? execution :principal))))))))

(deftest machine-boundary-preserves-nominal-category-distinction
  (let [plan (machine-plan)
        all (reference-bindings)
        execution (machine/start plan {:identity-bindings all})
        internal (machine-bindings/execution->nominal-bindings execution)]
    (is (instance? gesso.choreo.internal.nominal_identity.Principal
                   (:principal internal)))
    (is (instance? gesso.choreo.internal.nominal_identity.Actor
                   (:actor internal)))
    (is (instance? gesso.choreo.internal.nominal_identity.Authority
                   (:authority internal)))
    (is (instance? gesso.choreo.internal.nominal_identity.Host
                   (:host internal)))
    (is (instance? gesso.choreo.internal.nominal_identity.CommandId
                   (:command-id internal)))
    (is (instance? gesso.choreo.internal.nominal_identity.ExecutionId
                   (:execution-id internal)))
    (is (identical? (:principal internal)
                    (nominal/step-1 (:principal internal))))
    (is (= all (internal-bindings/bindings->tagged internal)))
    (is (not= (:principal internal) (:actor internal)))
    (is (not= (:command-id internal) (:execution-id internal)))))

(deftest completed-machine-keeps-tagged-state-and-checkable-nominal-projection
  (let [plan (machine-plan)
        started (machine/start plan {:identity-bindings (reference-bindings)})
        completed (machine/complete-local started)
        tagged-before (machine/identity-bindings started)
        tagged-after (machine/identity-bindings completed)
        nominal-after (machine-bindings/execution->nominal-bindings completed)]
    (is (machine/execution? completed))
    (is (machine/completed? completed))
    (is (= tagged-before tagged-after))
    (is (= tagged-after (internal-bindings/bindings->tagged nominal-after)))
    (is (every? tagged/identity? (vals (dissoc tagged-after :role))))
    (is (instance? gesso.choreo.internal.nominal_identity.Principal
                   (:principal nominal-after)))
    (is (instance? gesso.choreo.internal.nominal_identity.ExecutionId
                   (:execution-id nominal-after)))
    (is (= (machine/command-id started) (machine/command-id completed)))
    (is (= (machine/execution-id started) (machine/execution-id completed)))))

(deftest machine-public-input-does-not-accept-internal-nominal-objects
  (let [plan (machine-plan)
        slots [:principal :actor :authority :host :command-id :execution-id]
        factories [nominal/principal nominal/actor nominal/authority
                   nominal/host nominal/command-id nominal/execution-id]]
    (doseq [[slot factory] (map vector slots factories)]
      (is (= :invalid-identity-bindings
             (error-kind #(machine/start
                            plan {:identity-bindings {slot (factory :id)}})))
          (str "public machine slot requires tagged map: " slot)))
    (is (= :invalid-command-id
           (error-kind #(machine/start plan
                                       {:command-id (nominal/command-id :id)}))))
    (is (= :invalid-execution-id
           (error-kind #(machine/start plan
                                       {:execution-id (nominal/execution-id :id)}))))))

(deftest machine-start-rejects-wrong-identity-at-every-binding-slot
  (let [plan (machine-plan)
        all (reference-bindings)
        identity-slots (vec (rest slots))]
    (doseq [target identity-slots
            source identity-slots]
      (let [input {:identity-bindings {target (get all source)}}]
        (if (= target source)
          (is (machine/execution? (machine/start plan input)))
          (is (= :invalid-identity-bindings
                 (error-kind #(machine/start plan input)))))))
    (is (= :invalid-command-id
           (error-kind #(machine/start plan
                                       {:command-id (get all :execution-id)}))))
    (is (= :invalid-execution-id
           (error-kind #(machine/start plan
                                       {:execution-id (get all :command-id)}))))
    (is (= :identity-role-mismatch
           (error-kind #(machine/start plan
                                       {:identity-bindings {:role :browser}}))))
    (is (= :identity-binding-conflict
           (error-kind #(machine/start
                          plan {:command-id (tagged/command-id "other")
                                :identity-bindings
                                {:command-id (:command-id all)}}))))))

(deftest internal-machine-seam-rejects-invalid-executions-independently
  (let [plan (machine-plan)
        started (machine/start plan {:identity-bindings (reference-bindings)})
        forged (assoc started :identity-bindings
                      {:principal (tagged/actor :wrong)})]
    ;; Even when the internal adapter is called directly, it cannot interpret
    ;; arbitrary maps or a malformed machine execution as validated bindings.
    (doseq [invalid [nil {} :execution "execution"
                     {:identity-bindings {:role :server}}
                     (tagged/principal :user)
                     forged
                     (dissoc started :state)
                     (assoc started :role :browser)]]
      (is (= :invalid-execution
             (error-kind #(machine-bindings/execution->nominal-bindings
                           invalid)))))))

(deftest production-descriptor-consumers-reject-category-substitutions
  (let [tagged-command (tagged/command-id :same-id)
        tagged-execution (tagged/execution-id :same-id)
        nominal-command (internal-bindings/tagged->nominal tagged-command)
        nominal-execution (internal-bindings/tagged->nominal tagged-execution)]
    (is (= tagged-command
           (internal-bindings/command-id->tagged nominal-command)))
    (is (= tagged-execution
           (internal-bindings/execution-id->tagged nominal-execution)))
    ;; The nominal wrappers reject all five wrong categories, plain maps,
    ;; and nil when invoked *directly*, regardless of their upstream caller.
    (doseq [invalid [(nominal/principal :same-id)
                     (nominal/actor :same-id)
                     (nominal/authority :same-id)
                     (nominal/host :same-id)
                     (nominal/execution-id :same-id)
                     {:raw :same-id} nil]]
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (internal-bindings/command-id->tagged invalid))))
    (doseq [invalid [(nominal/principal :same-id)
                     (nominal/actor :same-id)
                     (nominal/authority :same-id)
                     (nominal/host :same-id)
                     (nominal/command-id :same-id)
                     {:raw :same-id} nil]]
      (is (thrown? #?(:clj AssertionError :cljs js/Error)
                   (internal-bindings/execution-id->tagged invalid))))))

(deftest real-machine-descriptors-remain-exactly-portable
  (let [plan (machine-plan)
        sample (reference-bindings)]
    ;; The new production descriptor path must not leak nominal classes into
    ;; machine outputs, and it must not lose extension-bearing identity maps.
    (doseq [mask (range 128)]
      (let [bindings (into {}
                           (for [[idx slot] (map-indexed vector slots)
                                 :when (bit-test mask idx)]
                             [slot (get sample slot)]))
            execution (machine/start plan {:identity-bindings bindings})
            public (machine/identity-bindings execution)
            descriptor (machine/pending-action execution)
            expected-non-role (dissoc public :role)]
        (is (= (:command-id public) (:command-id descriptor)))
        (is (= (:execution-id public) (:execution-id descriptor)))
        (is (= (contains? descriptor :command-id)
               (contains? public :command-id)))
        (is (= (boolean (seq expected-non-role))
               (contains? descriptor :identity-bindings)))
        (when (seq expected-non-role)
          (is (= public (:identity-bindings descriptor))))))))

(deftest machine-option-normalization-preserves-both-identity-categories
  (let [plan (machine-plan)
        command (assoc (tagged/command-id :shared-raw) :extra {:purpose :claim})
        execution (assoc (tagged/execution-id :shared-raw) :extra {:attempt 7})
        original {:command-id command :execution-id execution}
        variants [{:command-id command :execution-id execution}
                  {:identity-bindings original}
                  {:identity-bindings original
                   :command-id command :execution-id execution}
                  {:identity-bindings {:role :server :principal (tagged/principal :p)}
                   :command-id command :execution-id execution}]]
    (doseq [options variants]
      (let [started (machine/start plan options)
            bound (machine/identity-bindings started)
            action (machine/pending-action started)
            internal (machine-bindings/execution->nominal-bindings started)]
        (is (machine/execution? started))
        (is (= :server (:role bound)))
        (is (= command (:command-id bound)))
        (is (= execution (:execution-id bound)))
        (is (= command (:command-id action)))
        (is (= execution (:execution-id action)))
        (is (= bound (internal-bindings/bindings->tagged internal)))
        (is (instance? gesso.choreo.internal.nominal_identity.CommandId
                       (:command-id internal)))
        (is (instance? gesso.choreo.internal.nominal_identity.ExecutionId
                       (:execution-id internal)))
        (is (not= (:command-id internal) (:execution-id internal)))))))

(deftest machine-option-normalization-rejects-every-wrong-category
  (let [plan (machine-plan)
        identities [(tagged/principal :same-raw)
                    (tagged/actor :same-raw)
                    (tagged/authority :same-raw)
                    (tagged/host :same-raw)
                    (tagged/command-id :same-raw)
                    (tagged/execution-id :same-raw)]]
    (doseq [identity identities]
      (if (tagged/command-id? identity)
        (is (machine/execution?
             (machine/start plan {:command-id identity})))
        (is (= :invalid-command-id
               (error-kind #(machine/start plan {:command-id identity})))))
      (if (tagged/execution-id? identity)
        (is (machine/execution?
             (machine/start plan {:execution-id identity})))
        (is (= :invalid-execution-id
               (error-kind #(machine/start plan {:execution-id identity}))))))
    ;; Similar raw values do not imply equal semantic command identities:
    ;; extensions are preserved in the public tagged map and conflicts count.
    (is (= :identity-binding-conflict
           (error-kind #(machine/start
                          plan {:identity-bindings
                                {:command-id (tagged/command-id :same)}
                                :command-id (assoc (tagged/command-id :same)
                                                   :extension :different)}))))
    (is (= :identity-binding-conflict
           (error-kind #(machine/start
                          plan {:identity-bindings
                                {:execution-id (tagged/execution-id :same)}
                                :execution-id (assoc (tagged/execution-id :same)
                                                     :extension :different)}))))
    (is (= :identity-role-mismatch
           (error-kind #(machine/start plan
                                       {:identity-bindings {:role :browser}}))))))

#?(:clj
   (defn -main [& _]
     (let [result (clojure.test/run-tests
                   'gesso.choreo.typed-nominal-machine-boundary-test)]
       (when (pos? (+ (:fail result) (:error result)))
         (throw (ex-info "Machine/nominal boundary pilot failed." result)))
       :ok)))

#?(:cljs
   (defn -main [& _]
     (cljs.test/run-tests 'gesso.choreo.typed-nominal-machine-boundary-test)))

#?(:cljs (set! *main-cli-fn* -main))

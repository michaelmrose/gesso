(ns gesso.choreo.typed-nominal-machine-boundary-test
  "Verify the production-internal nominal conversion boundary against real
   Choreo machine executions, without changing their portable map format."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.core :as choreo]
            [gesso.choreo.identity :as tagged]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.choreo.internal.nominal-bindings :as internal-bindings]
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
            internals (internal-bindings/execution->nominal-bindings execution)
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
        internal (internal-bindings/execution->nominal-bindings execution)]
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
        nominal-after (internal-bindings/execution->nominal-bindings completed)]
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
             (error-kind #(internal-bindings/execution->nominal-bindings
                           invalid)))))))

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

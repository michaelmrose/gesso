(ns gesso.choreo.typed-nominal-transition-boundary-test
  "Verify sparse, extension-bearing public identities survive actual machine
   transitions and remain faithfully convertible to internal nominal values.
   Portable CLJC: the same assertions run on JVM and Node/CLJS."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.core :as choreo]
            [gesso.choreo.identity :as identity]
            [gesso.choreo.internal.machine-nominal-bindings :as machine-bindings]
            [gesso.choreo.internal.nominal-bindings :as nominal-bindings]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]))

(def ^:private identity-slots
  [:role :principal :actor :authority :host :command-id :execution-id])

(defn- sample-bindings []
  {:role :server
   :principal (assoc (identity/principal :same-value) :custom :principal)
   :actor (assoc (identity/actor :same-value) :custom :actor)
   :authority (assoc (identity/authority :same-value) :custom :authority)
   :host (assoc (identity/host :same-value) :custom :host)
   :command-id (assoc (identity/command-id :same-value) :custom :command)
   :execution-id (assoc (identity/execution-id :same-value) :custom :execution)})

(defn- two-local-plan []
  (project/project
   (choreo/->choreography
    {:initial :first
     :states {:first (choreo/local :server :first-step :second)
              :second (choreo/local :server :second-step :done)
              :done (choreo/return :done)}})
   :server))

;; This observation corpus is deliberately independent of test reports. JVM
;; and CLJS can pass the same assertions while disagreeing on actual outputs.
;; Export only portable public observations and re-tagged nominal projections;
;; internal nominal classes must never escape into the comparison.
(defn portable-identity-corpus []
  (let [plan (two-local-plan)
        sample (sample-bindings)]
    (mapv
     (fn [mask]
       (let [bindings (into {}
                            (for [[n slot] (map-indexed vector identity-slots)
                                  :when (bit-test mask n)]
                              [slot (get sample slot)]))
             first-state (machine/start plan {:identity-bindings bindings})
             second-state (machine/complete-local first-state)
             final-state (machine/complete-local second-state)]
         {:mask mask
          :states
          (mapv
           (fn [execution]
             {:bindings (machine/identity-bindings execution)
              :nominal-bindings
              (nominal-bindings/bindings->tagged
               (machine-bindings/execution->nominal-bindings execution))
              :command-id (machine/command-id execution)
              :execution-id (machine/execution-id execution)
              :pending (machine/pending-action execution)
              :completed? (machine/completed? execution)})
           [first-state second-state final-state])}))
     (range 128))))

;; Cross-runtime admission/rejection is also observable behavior: two hosts
;; must reject the same category swaps, not merely preserve valid identities.
(def ^:private runtime-slots
  [:principal :actor :authority :host :command-id :execution-id])

(defn- observed-start [plan case-id options]
  (try
    (let [execution (machine/start plan options)]
      {:case case-id
       :status :accepted
       :bindings (machine/identity-bindings execution)
       :pending (machine/pending-action execution)})
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) error
      (let [data (ex-data error)]
        {:case case-id
         :status :rejected
         :error/kind (:error/kind data)
         :cause/kind (get-in data [:cause :error/kind])}))))

(defn portable-admission-corpus []
  (let [plan (two-local-plan)
        sample (sample-bindings)]
    (vec
     (concat
      ;; Each runtime category must be accepted only in its matching slot;
      ;; all 36 pairs use the same raw identity value, by design.
      (for [slot runtime-slots
            category runtime-slots]
        (observed-start plan [:binding slot category]
                        {:identity-bindings {slot (get sample category)}}))
      ;; Convenience options cannot silently interchange semantic commands
      ;; and concrete executions, even if their raw identifiers are equal.
      (for [slot [:command-id :execution-id]
            category runtime-slots]
        (observed-start plan [:explicit slot category]
                        {slot (get sample category)}))
      (for [slot [:command-id :execution-id]]
        (observed-start plan [:agreement slot]
                        {:identity-bindings {slot (get sample slot)}
                         slot (get sample slot)}))
      (for [slot [:command-id :execution-id]]
        (observed-start plan [:conflict slot]
                        {:identity-bindings {slot (get sample slot)}
                         slot (assoc (get sample slot) :conflicting-extension true)}))
      [(observed-start plan [:role-mismatch]
                       {:identity-bindings {:role :browser}})
       (observed-start plan [:unknown-option]
                       {:misspelled-identity-binding :server})]
      (for [slot [:command-id :execution-id]]
        (observed-start plan [:raw-option slot] {slot :same-value}))))))

(deftest identity-category-admission-corpus-is-complete
  (let [cases (portable-admission-corpus)]
    (is (= 56 (count cases)))
    (is (= 56 (count (distinct (map :case cases)))))
    (is (= 10 (count (filter #(= :accepted (:status %)) cases))))
    (is (= 46 (count (filter #(= :rejected (:status %)) cases))))))

#?(:clj
   (defn -main [& _]
     (println "GESSO_CHOREO_IDENTITY_PARITY" (pr-str (portable-identity-corpus)))
     (println "GESSO_CHOREO_ADMISSION_PARITY" (pr-str (portable-admission-corpus)))))

(defn- assert-identity-continuity! [execution expected]
  (is (machine/execution? execution))
  (is (= expected (machine/identity-bindings execution)))
  (is (= (:command-id expected) (machine/command-id execution)))
  (is (= (:execution-id expected) (machine/execution-id execution)))
  (let [nominals (machine-bindings/execution->nominal-bindings execution)]
    (is (= (set (keys expected)) (set (keys nominals))))
    (is (= expected (nominal-bindings/bindings->tagged nominals))))
  ;; No internal nominal classes may escape through the exported bindings.
  (is (every? identity/identity? (vals (dissoc expected :role)))))

(defn- assert-pending-identities! [execution expected]
  (let [descriptor (machine/pending-action execution)]
    (is (= :local (:kind descriptor)))
    (is (= (:command-id expected) (:command-id descriptor)))
    (is (= (:execution-id expected) (:execution-id descriptor)))
    (is (= (contains? expected :command-id)
           (contains? descriptor :command-id)))
    ;; Execution descriptors always expose the execution-id slot, including
    ;; nil for anonymous executions; command-id is present only if bound.
    (is (contains? descriptor :execution-id))
    (let [has-non-role? (seq (dissoc expected :role))]
      (is (= (boolean has-non-role?)
             (contains? descriptor :identity-bindings)))
      (when has-non-role?
        (is (= expected (:identity-bindings descriptor)))))))

(deftest all-sparse-identities-survive-two-real-local-transitions
  (let [plan (two-local-plan)
        sample (sample-bindings)]
    (doseq [mask (range 128)]
      (let [sparse (into {}
                         (for [[n slot] (map-indexed vector identity-slots)
                               :when (bit-test mask n)]
                           [slot (get sample slot)]))
            expected (assoc sparse :role :server)
            start (machine/start plan {:identity-bindings sparse})
            second (machine/complete-local start)
            completed (machine/complete-local second)]
        (testing (str "sparse mask " mask)
          (is (machine/waiting-local? start))
          (is (machine/waiting-local? second))
          (is (machine/completed? completed))
          (doseq [execution [start second completed]]
            (assert-identity-continuity! execution expected))
          (assert-pending-identities! start expected)
          (assert-pending-identities! second expected)
          (is (nil? (machine/pending-action completed))))))))

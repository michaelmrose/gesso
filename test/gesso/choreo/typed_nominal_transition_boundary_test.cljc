(ns gesso.choreo.typed-nominal-transition-boundary-test
  "Verify sparse, extension-bearing public identities survive actual machine
   transitions and remain faithfully convertible to internal nominal values.
   Portable CLJC: the same assertions run on JVM and Node/CLJS."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.choreo.core :as choreo]
            [gesso.choreo.identity :as identity]
            [gesso.choreo.identity-test :as identity-test]
            [gesso.choreo.internal.machine-nominal-bindings :as machine-bindings]
            [gesso.choreo.internal.nominal-bindings :as nominal-bindings]
            [gesso.choreo.machine :as machine]
            [gesso.choreo.project :as project]
            [gesso.choreo.verify :as verify]))

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

;; In addition to local completions, verify both suspended-machine kinds:
;; participant receive and environment await. All public observations are
;; portable EDN; no internal nominal objects are exposed by either host.
(defn- suspension-plan [kind]
  (case kind
    :receive
    (project/project
     (choreo/->choreography
      {:initial :incoming
       :states {:incoming (choreo/communicate :client :server :sync/arrived :done)
                :done (choreo/return :done)}})
     :server)

    :environment
    (project/project
     (choreo/->choreography
      {:initial :waiting
       :states {:waiting (choreo/await :server {:sync/ready :done})
                :done (choreo/return :done)}})
     :server)))

(defn- nominal-projection [execution]
  (nominal-bindings/bindings->tagged
   (machine-bindings/execution->nominal-bindings execution)))

(defn- rejected-resume-kind [execution envelope]
  (try
    (machine/resume execution envelope)
    :unexpected-acceptance
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) error
      (:error/kind (ex-data error)))))

(defn portable-suspension-corpus []
  (let [plans {:receive (suspension-plan :receive)
               :environment (suspension-plan :environment)}
        sample (sample-bindings)
        envelopes
        {:receive
         [(machine/message :client :server :sync/arrived {})
          (machine/message :other :server :sync/arrived {})
          (machine/environment-event :server :sync/arrived)]
         :environment
         [(machine/environment-event :server :sync/ready)
          (machine/environment-event :client :sync/ready)
          (machine/message :client :server :sync/ready {})]}]
    (vec
     (for [kind [:receive :environment]
           mask (range 128)]
       (let [sparse (into {}
                          (for [[n slot] (map-indexed vector identity-slots)
                                :when (bit-test mask n)]
                            [slot (get sample slot)]))
             waiting (machine/start (get plans kind)
                                    {:identity-bindings sparse})
             [valid wrong-role wrong-kind] (get envelopes kind)
             accepted? (machine/accepts? waiting valid)
             wrong-role-accepted? (machine/accepts? waiting wrong-role)
             wrong-kind-accepted? (machine/accepts? waiting wrong-kind)
             wrong-role-error (rejected-resume-kind waiting wrong-role)
             wrong-kind-error (rejected-resume-kind waiting wrong-kind)
             completed (machine/resume waiting valid)]
         {:kind kind
          :mask mask
          :waiting {:bindings (machine/identity-bindings waiting)
                    :nominal-bindings (nominal-projection waiting)
                    :status (:status waiting)
                    :suspended? (machine/suspended? waiting)
                    :awaiting (machine/awaiting waiting)
                    :pending (machine/pending-action waiting)
                    :history (machine/execution-history waiting)}
          :acceptance {:valid accepted?
                       :wrong-role wrong-role-accepted?
                       :wrong-kind wrong-kind-accepted?}
          :rejections {:wrong-role wrong-role-error
                       :wrong-kind wrong-kind-error}
          :completed {:bindings (machine/identity-bindings completed)
                      :nominal-bindings (nominal-projection completed)
                      :status (:status completed)
                      :result (machine/result completed)
                      :awaiting (machine/awaiting completed)
                      :pending (machine/pending-action completed)
                      :history (machine/execution-history completed)}})))))

(deftest sparse-identities-survive-receive-and-environment-resumption
  (let [cases (portable-suspension-corpus)]
    (is (= 256 (count cases)))
    (is (= (vec (for [kind [:receive :environment]
                       mask (range 128)] [kind mask]))
           (mapv (juxt :kind :mask) cases)))
    (doseq [{:keys [kind waiting acceptance rejections completed]} cases]
      (is (= (:bindings waiting) (:bindings completed)))
      (is (= (:bindings waiting) (:nominal-bindings completed)))
      (is (= {:valid true :wrong-role false :wrong-kind false}
             acceptance))
      (is (= {:wrong-role (if (= kind :receive)
                             :message-not-enabled
                             :environment-event-not-enabled)
              :wrong-kind (if (= kind :receive)
                            :invalid-message
                            :invalid-environment-event)}
             rejections)))))

;; Production action boundaries: send constructs a transport message from
;; established knowledge; authoritative completion establishes authoritative
;; knowledge. Their rejection classifications must agree on JVM and Node.
(defn- action-boundary-plan [kind]
  (case kind
    :send
    (project/project
     (verify/verify!
      (choreo/->choreography
       {:initial :send
        :states {:send (choreo/communicate
                         :server :client :request/result :done
                         {:via :http :required #{:outcome}})
                 :done (choreo/return :done)}})
      {:entry-value-keys #{:outcome}})
     :server)

    :authoritative
    (project/project
     (choreo/->choreography
      {:initial :claim
       :states {:claim (choreo/authoritative
                         :server :request/claim :done
                         {:outputs #{:outcome}})
                :done (choreo/return :done)}})
     :server)))

(defn- rejected-action-kind [f]
  (try
    (f)
    :unexpected-acceptance
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs cljs.core.ExceptionInfo) error
      (:error/kind (ex-data error)))))

(defn portable-action-corpus []
  (let [plans {:send (action-boundary-plan :send)
               :authoritative (action-boundary-plan :authoritative)}
        sample (sample-bindings)
        valid {:outcome :confirmed}
        extra {:outcome :confirmed :unexpected true}]
    (vec
     (for [kind [:send :authoritative]
           mask (range 128)]
       (let [sparse (into {}
                          (for [[n slot] (map-indexed vector identity-slots)
                                :when (bit-test mask n)]
                            [slot (get sample slot)]))
             waiting (machine/start
                      (get plans kind)
                      (cond-> {:identity-bindings sparse}
                        (= kind :send) (assoc :values valid)))
             complete! (if (= kind :send) machine/complete-send
                           machine/complete-authoritative)
             rejected-missing (rejected-action-kind #(complete! waiting {}))
             rejected-extra (rejected-action-kind #(complete! waiting extra))
             rejected-wrong-value
             (when (= kind :send)
               (rejected-action-kind
                #(complete! waiting {:outcome :different})))
             envelope (when (= kind :send)
                        (machine/pending-message waiting valid))
             outcome (complete! waiting valid)
             completed (if (= kind :send) (:execution outcome) outcome)
             rejected-replay (rejected-action-kind #(complete! completed valid))]
         {:kind kind
          :mask mask
          :waiting {:bindings (machine/identity-bindings waiting)
                    :nominal-bindings (nominal-projection waiting)
                    :status (:status waiting)
                    :pending (machine/pending-action waiting)
                    :history (machine/execution-history waiting)
                    :values (machine/execution-values waiting)}
          :rejections {:missing rejected-missing
                       :extra rejected-extra
                       :wrong-value rejected-wrong-value
                       :replay rejected-replay}
          :message envelope
          :sent-envelope (when (= kind :send) (:message outcome))
          :completed {:bindings (machine/identity-bindings completed)
                      :nominal-bindings (nominal-projection completed)
                      :status (:status completed)
                      :result (machine/result completed)
                      :pending (machine/pending-action completed)
                      :history (machine/execution-history completed)
                      :values (machine/execution-values completed)
                      :provenance-kinds (machine/execution-provenance-kinds completed :outcome)}})))))

(deftest sparse-identities-survive-send-and-authoritative-completion
  (let [cases (portable-action-corpus)]
    (is (= 256 (count cases)))
    (is (= (vec (for [kind [:send :authoritative] mask (range 128)]
                  [kind mask]))
           (mapv (juxt :kind :mask) cases)))
    (doseq [{:keys [kind waiting rejections completed]} cases]
      (is (= (:bindings waiting) (:bindings completed)))
      (is (= (:bindings waiting) (:nominal-bindings completed)))
      (is (= :completed (:status completed)))
      (is (= {:missing (if (= kind :send) :invalid-message-payload
                           :authoritative-output-mismatch)
              :extra (if (= kind :send) :invalid-message-payload
                         :authoritative-output-mismatch)
              :wrong-value (when (= kind :send) :invalid-message-knowledge)
              :replay (if (= kind :send) :not-waiting-send
                          :not-waiting-authoritative)}
             rejections)))))

#?(:clj
   (defn -main [& _]
     (println "GESSO_CHOREO_IDENTITY_PARITY" (pr-str (portable-identity-corpus)))
     (println "GESSO_CHOREO_ADMISSION_PARITY" (pr-str (portable-admission-corpus)))
     (println "GESSO_CHOREO_SUSPENSION_PARITY" (pr-str (portable-suspension-corpus)))
     (println "GESSO_CHOREO_ACTION_PARITY" (pr-str (portable-action-corpus)))
     (println "GESSO_CHOREO_WIRE_PARITY" (pr-str (identity-test/portable-wire-corpus)))))

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

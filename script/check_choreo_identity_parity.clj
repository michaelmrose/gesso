(ns gesso.script.check-choreo-identity-parity
  "Compare actual portable machine observations from independent JVM and Node
   execution. Separate passing test suites alone cannot establish parity."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker "GESSO_CHOREO_IDENTITY_PARITY ")
(def ^:private admission-marker "GESSO_CHOREO_ADMISSION_PARITY ")
(def ^:private suspension-marker "GESSO_CHOREO_SUSPENSION_PARITY ")
(def ^:private action-marker "GESSO_CHOREO_ACTION_PARITY ")
(def ^:private runtime-slots
  [:principal :actor :authority :host :command-id :execution-id])
(def ^:private slots
  [:role :principal :actor :authority :host :command-id :execution-id])

(defn- fail! [message data]
  (throw (ex-info message (assoc data :identity-parity/failure true))))

(defn- read-corpus! [path prefix]
  (let [matches (->> (str/split-lines (slurp path))
                     (filter #(str/starts-with? % prefix))
                     vec)]
    (when-not (= 1 (count matches))
      (fail! "Missing or duplicated portable identity corpus marker."
             {:file path :marker-count (count matches)}))
    (try
      (let [body (subs (first matches) (count prefix))]
        (with-open [reader (java.io.PushbackReader.
                            (java.io.StringReader. body))]
          (let [form (edn/read {:eof ::eof} reader)
                trailing (edn/read {:eof ::eof} reader)]
            (when-not (= ::eof trailing)
              (fail! "Trailing data after portable identity corpus."
                     {:file path}))
            form)))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (fail! "Invalid portable identity corpus EDN."
               {:file path :cause (ex-message e)})))))

(defn- validate-corpus! [host cases]
  (when-not (and (vector? cases)
                 (= 128 (count cases))
                 (= (vec (range 128)) (mapv :mask cases)))
    (fail! "Portable identity corpus lacks its complete ordered 128-case matrix."
           {:host host :case-count (count cases)}))
  (doseq [{:keys [mask states]} cases]
    (when-not (and (vector? states) (= 3 (count states)))
      (fail! "Expected exactly three observed machine states."
             {:host host :mask mask :state-count (count states)}))
    (let [expected-keys (conj (into #{}
                                    (for [[n slot] (map-indexed vector slots)
                                          :when (bit-test mask n)] slot)) :role)
          bindings (:bindings (first states))]
      (when-not (and (= expected-keys (set (keys bindings)))
                     (= :server (:role bindings)))
        (fail! "Machine identity slots differ from the supplied sparse mask."
               {:host host :mask mask :observed-keys (keys bindings)}))
      (doseq [[step state] (map-indexed vector states)]
        (when-not (and (= bindings (:bindings state))
                       (= bindings (:nominal-bindings state))
                       (= (:command-id bindings) (:command-id state))
                       (= (:execution-id bindings) (:execution-id state))
                       (= (= step 2) (:completed? state)))
          (fail! "Machine identity or nominal observation changed."
                 {:host host :mask mask :step step})))
      (doseq [[step expected-action] [[0 :first-step] [1 :second-step]]]
        (let [pending (:pending (nth states step))]
          (when-not (and (= :local (:kind pending))
                         (= :server (:role pending))
                         (= step (:state pending))
                         (= expected-action (:action pending))
                         (contains? pending :execution-id)
                         (= (:execution-id bindings) (:execution-id pending))
                         (= (contains? bindings :command-id)
                            (contains? pending :command-id))
                         (= (:command-id bindings) (:command-id pending))
                         (= (boolean (seq (dissoc bindings :role)))
                            (contains? pending :identity-bindings))
                         (or (not (contains? pending :identity-bindings))
                             (= bindings (:identity-bindings pending))))
            (fail! "Pending action projection differs from its machine state."
                   {:host host :mask mask :step step}))))
      (when-not (nil? (:pending (nth states 2)))
        (fail! "Terminal machine unexpectedly has a pending action."
               {:host host :mask mask}))))
  cases)

(defn- compare-corpora! [jvm node]
  (validate-corpus! :jvm jvm)
  (validate-corpus! :node node)
  (when-not (= jvm node)
    (let [index (first (keep-indexed (fn [n [a b]]
                                      (when (not= a b) n))
                                    (map vector jvm node)))]
      (fail! "JVM and Node identity observations disagree."
             {:first-mismatch-mask index})))
  true)

;; Admission parity is checked independently of the successful transition
;; corpus. A rejected category swap must have the same *typed* rejection kind
;; and, for binding-validation failures, the same nested identity cause.
(defn- admission-case-ids []
  (vec
   (concat
    (for [slot runtime-slots category runtime-slots]
      [:binding slot category])
    (for [slot [:command-id :execution-id] category runtime-slots]
      [:explicit slot category])
    (for [slot [:command-id :execution-id]] [:agreement slot])
    (for [slot [:command-id :execution-id]] [:conflict slot])
    [[:role-mismatch] [:unknown-option]]
    (for [slot [:command-id :execution-id]] [:raw-option slot]))))

(defn- expected-rejection-kind [mode slot category]
  (case mode
    :binding (when-not (= slot category)
               [:invalid-identity-bindings :invalid-binding])
    :explicit (when-not (= slot category)
                [(if (= slot :command-id) :invalid-command-id
                     :invalid-execution-id) nil])
    :agreement nil
    :conflict [:identity-binding-conflict nil]
    :role-mismatch [:identity-role-mismatch nil]
    :unknown-option [:unknown-start-option-keys nil]
    :raw-option [(if (= slot :command-id) :invalid-command-id
                     :invalid-execution-id) nil]))

(defn- expected-identity-kind [slot]
  (case slot
    :command-id :command
    :execution-id :execution
    slot))

(defn- validate-admission! [host cases]
  (let [expected-ids (admission-case-ids)]
    (when-not (and (vector? cases)
                   (= 56 (count cases))
                   (= expected-ids (mapv :case cases)))
      (fail! "Identity admission matrix is missing, reordered, or duplicated."
             {:host host :case-count (count cases)}))
    (doseq [{:keys [case status bindings pending] :as observed} cases]
      (let [[mode slot category] case
            rejection (expected-rejection-kind mode slot category)]
        (if rejection
          (when-not (and (= #{:case :status :error/kind :cause/kind}
                            (set (keys observed)))
                         (= :rejected status)
                         (= rejection [(:error/kind observed)
                                       (:cause/kind observed)]))
            (fail! "Identity category rejection differs from expected typed error."
                   {:host host :case case :observed observed :expected rejection}))
          (let [effective-slot (if (= mode :agreement) slot slot)
                tagged (get bindings effective-slot)]
            (when-not (and (= #{:case :status :bindings :pending}
                              (set (keys observed)))
                           (= :accepted status)
                           (= #{:role effective-slot} (set (keys bindings)))
                           (= :server (:role bindings))
                           (= :gesso.choreo.identity/ref
                              (:gesso.choreo.identity/type tagged))
                           (= (expected-identity-kind effective-slot)
                              (:gesso.choreo.identity/kind tagged))
                           (= :same-value (:gesso.choreo.identity/value tagged))
                           (= :local (:kind pending))
                           (= :first-step (:action pending))
                           (= :server (:role pending))
                           (= (get bindings :command-id) (:command-id pending))
                           (= (get bindings :execution-id) (:execution-id pending))
                           (= (contains? bindings :command-id)
                              (contains? pending :command-id))
                           (contains? pending :execution-id)
                           (= bindings (:identity-bindings pending)))
              (fail! "Accepted identity case lost its typed binding or descriptor."
                     {:host host :case case :observed observed}))))))
    cases))

(defn- compare-admissions! [jvm node]
  (validate-admission! :jvm jvm)
  (validate-admission! :node node)
  (when-not (= jvm node)
    (let [index (first (keep-indexed (fn [n [a b]]
                                      (when (not= a b) n))
                                    (map vector jvm node)))]
      (fail! "JVM and Node typed admission or rejection observations disagree."
             {:case-index index
              :case (get-in jvm [index :case])})))
  true)

(defn- expect-rejected! [label f]
  (try
    (f)
    (throw (ex-info "Parity mutation unexpectedly accepted."
                    {:mutation label :unexpected-pass true}))
    (catch clojure.lang.ExceptionInfo e
      (when-not (and (:identity-parity/failure (ex-data e))
                     (not (:unexpected-pass (ex-data e))))
        (throw e)))))

(defn- negative-checks! [cases]
  (expect-rejected! :missing-case
                    #(compare-corpora! cases (pop cases)))
  (expect-rejected! :changed-action
                    #(compare-corpora!
                      cases (assoc-in cases [0 :states 0 :pending :action] :mutated)))
  (expect-rejected! :changed-identity
                    #(compare-corpora!
                      cases (assoc-in cases [127 :states 1 :command-id] :mutated)))
  (expect-rejected! :changed-nominal
                    #(compare-corpora!
                      cases (assoc-in cases [127 :states 0 :nominal-bindings :role]
                                      :mutated)))
  (expect-rejected! :duplicate-mask
                    #(compare-corpora! cases (assoc-in cases [1 :mask] 0)))
  (println "JVM/Node identity parity mutations: PASS (5 rejected)"))

(defn- admission-negative-checks! [cases]
  (expect-rejected! :missing-admission
                    #(compare-admissions! cases (pop cases)))
  (expect-rejected! :wrong-error-kind
                    #(compare-admissions!
                      cases (assoc-in cases [1 :error/kind] :invalid-command-id)))
  (expect-rejected! :wrong-error-cause
                    #(compare-admissions!
                      cases (assoc-in cases [1 :cause/kind] :invalid-kind)))
  (expect-rejected! :forged-acceptance
                    #(compare-admissions!
                      cases (assoc-in cases [1 :status] :accepted)))
  (expect-rejected! :substituted-binding
                    #(compare-admissions!
                      cases (assoc-in cases [0 :bindings :principal] :wrong)))
  (expect-rejected! :reordered-admission
                    #(compare-admissions!
                      cases (-> cases
                                (assoc 0 (nth cases 1))
                                (assoc 1 (nth cases 0)))))
  (println "JVM/Node admission mutations: PASS (6 rejected)"))

;; Observe actual suspension and resumption at two independent machine
;; boundaries, not just local completion. These checks independently validate
;; the known contract before comparing JVM and Node serialized outcomes.
(defn- expected-suspension-bindings [mask observed]
  (let [expected-keys
        (conj (into #{}
                    (for [[n slot] (map-indexed vector slots)
                          :when (bit-test mask n)] slot)) :role)]
    (when-not (and (map? observed)
                   (= expected-keys (set (keys observed)))
                   (= :server (:role observed)))
      (fail! "Suspension identity binding mask differs from expected slots."
             {:mask mask :observed observed}))
    (doseq [[slot identity] (dissoc observed :role)]
      (let [expected-kind (expected-identity-kind slot)]
        (when-not (and (map? identity)
                       (= :gesso.choreo.identity/ref
                          (:gesso.choreo.identity/type identity))
                       (= expected-kind (:gesso.choreo.identity/kind identity))
                       (= :same-value (:gesso.choreo.identity/value identity))
                       (= expected-kind (:custom identity)))
          (fail! "Suspended machine identity is not the original tagged category."
                 {:mask mask :slot slot :observed identity}))))
    observed))

(defn- expected-awaiting [kind bindings]
  (cond-> (case kind
            :receive {:kind :receive
                      :role :server
                      :alternatives [{:from :client :event :sync/arrived}]}
            :environment {:kind :environment
                          :role :server
                          :events #{:sync/ready}})
    (seq (dissoc bindings :role))
    (assoc :identity-bindings bindings)))

(defn- expected-completed-history [kind]
  [(case kind
     :receive {:kind :receive :state 0 :from :client :to :server
               :event :sync/arrived :payload {}}
     :environment {:kind :environment :state 0 :role :server
                   :event :sync/ready :data nil})
   {:kind :terminal :state 1 :outcome :gesso.choreo/complete}])

(defn- validate-suspensions! [host cases]
  (let [ids (vec (for [kind [:receive :environment]
                       mask (range 128)]
                   [kind mask]))]
    (when-not (and (vector? cases)
                   (= 256 (count cases))
                   (= ids (mapv (juxt :kind :mask) cases)))
      (fail! "Suspension corpus must have exactly 256 ordered kind/mask cases."
             {:host host :observed-count (count cases)}))
    (doseq [{:keys [kind mask waiting acceptance rejections completed] :as entry} cases]
      (when-not (= #{:kind :mask :waiting :acceptance :rejections :completed}
                   (set (keys entry)))
        (fail! "Suspension observation has unexpected or missing fields."
               {:host host :kind kind :mask mask}))
      (let [bindings (expected-suspension-bindings mask (:bindings waiting))
            expected-errors {:wrong-role (if (= kind :receive)
                                            :message-not-enabled
                                            :environment-event-not-enabled)
                             :wrong-kind (if (= kind :receive)
                                           :invalid-message
                                           :invalid-environment-event)}]
        (when-not (= {:bindings bindings
                      :nominal-bindings bindings
                      :status (if (= kind :receive)
                                :waiting-receive :waiting-environment)
                      :suspended? true
                      :awaiting (expected-awaiting kind bindings)
                      :pending nil
                      :history []}
                     waiting)
          (fail! "Suspended machine observation violates the boundary contract."
                 {:host host :kind kind :mask mask :waiting waiting}))
        (when-not (= {:valid true :wrong-role false :wrong-kind false}
                     acceptance)
          (fail! "Suspended-machine acceptance predicates are wrong."
                 {:host host :kind kind :mask mask :acceptance acceptance}))
        (when-not (= expected-errors rejections)
          (fail! "Suspended-machine rejection kinds differ from expected errors."
                 {:host host :kind kind :mask mask :rejections rejections}))
        (when-not (= {:bindings bindings
                      :nominal-bindings bindings
                      :status :completed
                      :result {:outcome :gesso.choreo/complete}
                      :awaiting nil
                      :pending nil
                      :history (expected-completed-history kind)}
                     completed)
          (fail! "Resumed machine lost identity or changed deterministic history."
                 {:host host :kind kind :mask mask :completed completed})))))
  cases)

(defn- compare-suspensions! [jvm node]
  (validate-suspensions! :jvm jvm)
  (validate-suspensions! :node node)
  (when-not (= jvm node)
    (let [first-mismatch
          (first (keep-indexed (fn [i [a b]] (when (not= a b) i))
                               (map vector jvm node)))]
      (fail! "JVM and Node suspended-machine observations disagree."
             {:first-mismatch first-mismatch
              :kind-mask (select-keys (get jvm first-mismatch) [:kind :mask])})))
  true)

(defn- suspension-negative-checks! [cases]
  (expect-rejected! :missing-suspension
                    #(compare-suspensions! cases (pop cases)))
  (expect-rejected! :reordered-suspension
                    #(compare-suspensions! cases
                      (-> cases (assoc 0 (nth cases 1))
                                (assoc 1 (nth cases 0)))))
  (expect-rejected! :wrong-awaiting-kind
                    #(compare-suspensions! cases
                      (assoc-in cases [0 :waiting :awaiting :kind] :environment)))
  (expect-rejected! :wrong-sender-rejection
                    #(compare-suspensions! cases
                      (assoc-in cases [0 :rejections :wrong-role] :invalid-message)))
  (expect-rejected! :forged-acceptance
                    #(compare-suspensions! cases
                      (assoc-in cases [0 :acceptance :wrong-kind] true)))
  (expect-rejected! :mutated-nominal
                    #(compare-suspensions! cases
                      (assoc-in cases [255 :completed :nominal-bindings :role]
                                :client)))
  (expect-rejected! :forged-terminal-history
                    #(compare-suspensions! cases
                      (assoc-in cases [127 :completed :history 1 :outcome] :failed)))
  (expect-rejected! :dropped-identity
                    #(compare-suspensions! cases
                      (update-in cases [2 :waiting :bindings] dissoc :principal)))
  (println "JVM/Node suspension mutations: PASS (8 rejected)"))

;; A separate oracle for the remaining endpoint-actuated boundaries. Both
;; hosts must produce the specified portable action/message/knowledge history,
;; not merely agree with each other on an incorrect serialization.
(defn- expected-action-descriptor [kind bindings]
  (cond->
   (merge
    (case kind
      :send {:kind :send :state 0 :from :server :to :client
             :event :request/result :via :http :required #{:outcome}}
      :authoritative {:kind :authoritative :state 0 :role :server
                      :operation :request/claim :outputs #{:outcome}})
    {:execution-id (:execution-id bindings)})
    (contains? bindings :command-id)
    (assoc :command-id (:command-id bindings))
    (seq (dissoc bindings :role))
    (assoc :identity-bindings bindings)))

(defn- expected-action-history [kind]
  [(case kind
     :send {:kind :send :state 0 :from :server :to :client
            :event :request/result :payload {:outcome :confirmed} :via :http}
     :authoritative {:kind :authoritative :state 0 :role :server
                     :operation :request/claim :outputs {:outcome :confirmed}})
   {:kind :terminal :state 1 :outcome :gesso.choreo/complete}])

(defn- validate-actions! [host cases]
  (let [ids (vec (for [kind [:send :authoritative] mask (range 128)]
                   [kind mask]))]
    (when-not (and (vector? cases)
                   (= 256 (count cases))
                   (= ids (mapv (juxt :kind :mask) cases)))
      (fail! "Action-boundary corpus must contain 256 ordered kind/mask cases."
             {:host host :observed-count (count cases)}))
    (doseq [{:keys [kind mask waiting completed message sent-envelope rejections] :as entry} cases]
      (when-not (= #{:kind :mask :waiting :rejections :message :sent-envelope :completed}
                   (set (keys entry)))
        (fail! "Action-boundary observation has missing or unknown fields."
               {:host host :kind kind :mask mask}))
      (let [bindings (expected-suspension-bindings mask (:bindings waiting))
            input-values (if (= kind :send) {:outcome :confirmed} {})
            expected-rejections
            {:missing (if (= kind :send) :invalid-message-payload
                          :authoritative-output-mismatch)
             :extra (if (= kind :send) :invalid-message-payload
                        :authoritative-output-mismatch)
             :wrong-value (when (= kind :send) :invalid-message-knowledge)
             :replay (if (= kind :send) :not-waiting-send
                         :not-waiting-authoritative)}
            expected-message
            (when (= kind :send)
              {:kind :message :from :server :to :client :event :request/result
               :payload {:outcome :confirmed} :via :http})]
        (when-not (= {:bindings bindings
                      :nominal-bindings bindings
                      :status (if (= kind :send)
                                :waiting-send :waiting-authoritative)
                      :pending (expected-action-descriptor kind bindings)
                      :history []
                      :values input-values}
                     waiting)
          (fail! "Pending send/authoritative descriptor is inconsistent."
                 {:host host :kind kind :mask mask :waiting waiting}))
        (when-not (= expected-rejections rejections)
          (fail! "Send/authoritative typed rejection differs from the contract."
                 {:host host :kind kind :mask mask :rejections rejections}))
        (when-not (and (= expected-message message)
                       (= expected-message sent-envelope))
          (fail! "Send completion envelope differs from the pending message."
                 {:host host :kind kind :mask mask
                  :message message :sent-envelope sent-envelope}))
        (when-not (= {:bindings bindings
                      :nominal-bindings bindings
                      :status :completed
                      :result {:outcome :gesso.choreo/complete}
                      :pending nil
                      :history (expected-action-history kind)
                      :values {:outcome :confirmed}
                      :provenance-kinds (if (= kind :send) #{:input}
                                            #{:authoritative})}
                     completed)
          (fail! "Completed action lost identity, values or semantic history."
                 {:host host :kind kind :mask mask :completed completed})))))
  cases)

(defn- compare-actions! [jvm node]
  (validate-actions! :jvm jvm)
  (validate-actions! :node node)
  (when-not (= jvm node)
    (let [index (first (keep-indexed
                       (fn [i [a b]] (when (not= a b) i))
                       (map vector jvm node)))]
      (fail! "JVM and Node action-boundary observations disagree."
             {:first-mismatch index
              :kind-mask (select-keys (get jvm index) [:kind :mask])})))
  true)

(defn- action-negative-checks! [cases]
  (expect-rejected! :missing-action
                    #(compare-actions! cases (pop cases)))
  (expect-rejected! :reordered-action
                    #(compare-actions! cases
                      (-> cases (assoc 0 (nth cases 1))
                                (assoc 1 (nth cases 0)))))
  (expect-rejected! :wrong-send-kind
                    #(compare-actions! cases
                      (assoc-in cases [0 :waiting :pending :kind] :authoritative)))
  (expect-rejected! :wrong-message-payload
                    #(compare-actions! cases
                      (assoc-in cases [0 :message :payload :outcome] :rejected)))
  (expect-rejected! :forged-sender-knowledge
                    #(compare-actions! cases
                      (assoc-in cases [0 :rejections :wrong-value] nil)))
  (expect-rejected! :missing-descriptor-command
                    #(compare-actions! cases
                      (update-in cases [32 :waiting :pending] dissoc :command-id)))
  (expect-rejected! :changed-nominal-binding
                    #(compare-actions! cases
                      (assoc-in cases [255 :completed :nominal-bindings :role] :client)))
  (expect-rejected! :wrong-authoritative-history
                    #(compare-actions! cases
                      (assoc-in cases [128 :completed :history 0 :operation] :forged)))
  (expect-rejected! :wrong-replay-error
                    #(compare-actions! cases
                      (assoc-in cases [128 :rejections :replay] :not-waiting-send)))
  (expect-rejected! :forged-sent-envelope
                    #(compare-actions! cases
                      (assoc-in cases [0 :sent-envelope :to] :attacker)))
  (expect-rejected! :false-authority-provenance
                    #(compare-actions! cases
                      (assoc-in cases [128 :completed :provenance-kinds] #{:input})))
  (println "JVM/Node action boundary mutations: PASS (11 rejected)"))

(defn -main [& args]
  (when-not (= 2 (count args))
    (fail! "Usage: bb script/check_choreo_identity_parity.clj JVM_LOG NODE_LOG"
           {:args args}))
  (let [[jvm-file node-file] args
        jvm (read-corpus! jvm-file marker)
        node (read-corpus! node-file marker)
        jvm-admissions (read-corpus! jvm-file admission-marker)
        node-admissions (read-corpus! node-file admission-marker)
        jvm-suspensions (read-corpus! jvm-file suspension-marker)
        node-suspensions (read-corpus! node-file suspension-marker)
        jvm-actions (read-corpus! jvm-file action-marker)
        node-actions (read-corpus! node-file action-marker)]
    (compare-corpora! jvm node)
    (negative-checks! jvm)
    (compare-admissions! jvm-admissions node-admissions)
    (admission-negative-checks! jvm-admissions)
    (compare-suspensions! jvm-suspensions node-suspensions)
    (suspension-negative-checks! jvm-suspensions)
    (compare-actions! jvm-actions node-actions)
    (action-negative-checks! jvm-actions)
    (println "JVM/Node identity transition observations: PASS (128 identical cases)")
    (println "JVM/Node typed identity admissions: PASS (56 identical cases)")
    (println "JVM/Node receive/await suspension observations: PASS (256 identical cases)")
    (println "JVM/Node send/authoritative action observations: PASS (256 identical cases)")))

(apply -main *command-line-args*)

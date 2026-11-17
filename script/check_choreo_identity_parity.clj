(ns gesso.script.check-choreo-identity-parity
  "Compare actual portable machine observations from independent JVM and Node
   execution. Separate passing test suites alone cannot establish parity."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker "GESSO_CHOREO_IDENTITY_PARITY ")
(def ^:private admission-marker "GESSO_CHOREO_ADMISSION_PARITY ")
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

(defn -main [& args]
  (when-not (= 2 (count args))
    (fail! "Usage: bb script/check_choreo_identity_parity.clj JVM_LOG NODE_LOG"
           {:args args}))
  (let [[jvm-file node-file] args
        jvm (read-corpus! jvm-file marker)
        node (read-corpus! node-file marker)
        jvm-admissions (read-corpus! jvm-file admission-marker)
        node-admissions (read-corpus! node-file admission-marker)]
    (compare-corpora! jvm node)
    (negative-checks! jvm)
    (compare-admissions! jvm-admissions node-admissions)
    (admission-negative-checks! jvm-admissions)
    (println "JVM/Node identity transition observations: PASS (128 identical cases)")
    (println "JVM/Node typed identity admissions: PASS (56 identical cases)")))

(apply -main *command-line-args*)

(ns gesso.script.check-choreo-identity-parity
  "Compare actual portable machine observations from independent JVM and Node
   execution. Separate passing test suites alone cannot establish parity."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker "GESSO_CHOREO_IDENTITY_PARITY ")
(def ^:private slots
  [:role :principal :actor :authority :host :command-id :execution-id])

(defn- fail! [message data]
  (throw (ex-info message (assoc data :identity-parity/failure true))))

(defn- read-corpus! [path]
  (let [matches (->> (str/split-lines (slurp path))
                     (filter #(str/starts-with? % marker))
                     vec)]
    (when-not (= 1 (count matches))
      (fail! "Missing or duplicated portable identity corpus marker."
             {:file path :marker-count (count matches)}))
    (try
      (let [body (subs (first matches) (count marker))]
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

(defn -main [& args]
  (when-not (= 2 (count args))
    (fail! "Usage: bb script/check_choreo_identity_parity.clj JVM_LOG NODE_LOG"
           {:args args}))
  (let [[jvm-file node-file] args
        jvm (read-corpus! jvm-file)
        node (read-corpus! node-file)]
    (compare-corpora! jvm node)
    (negative-checks! jvm)
    (println "JVM/Node identity transition observations: PASS (128 identical cases)")))

(apply -main *command-line-args*)

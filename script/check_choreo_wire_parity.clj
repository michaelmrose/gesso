(ns gesso.script.check-choreo-wire-parity
  "Compare independently observed JVM/Node public identity wire behavior.
   Both hosts must also satisfy the explicit closed, versioned wire contract."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker "GESSO_CHOREO_WIRE_PARITY ")
(def ^:private kinds
  [:principal :actor :authority :host :command :execution])
(def ^:private raw-samples
  [[:keyword :same-value]
   [:string "same-value"]
   [:record {:tenant :shop/id-17
             :subject ["user" 42]
             :basis {:stamp 9}}]
   [:vector ["user" 42 {:flags #{:open :closed}}]]])
(def ^:private wire-type-key :gesso.choreo.identity.wire/type)
(def ^:private wire-version-key :gesso.choreo.identity.wire/version)
(def ^:private wire-kind-key :gesso.choreo.identity.wire/kind)
(def ^:private wire-value-key :gesso.choreo.identity.wire/value)

(defn- fail! [message data]
  (throw (ex-info message (assoc data :wire-parity/failure true))))

(defn- read-corpus! [file]
  (let [lines (->> (str/split-lines (slurp file))
                   (filter #(str/starts-with? % marker))
                   vec)]
    (when-not (= 1 (count lines))
      (fail! "Missing or duplicated wire parity marker."
             {:file file :marker-count (count lines)}))
    (try
      (with-open [reader (java.io.PushbackReader.
                          (java.io.StringReader.
                           (subs (first lines) (count marker))))]
        (let [cases (edn/read {:eof ::end} reader)
              trailing (edn/read {:eof ::end} reader)]
          (when-not (= ::end trailing)
            (fail! "Trailing wire corpus data is forbidden." {:file file}))
          cases))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (fail! "Wire corpus is not valid EDN."
               {:file file :cause (ex-message e)})))))

(defn- runtime-identity [kind raw]
  {:gesso.choreo.identity/type :gesso.choreo.identity/ref
   :gesso.choreo.identity/kind kind
   :gesso.choreo.identity/value raw})

(defn- versioned-wire [kind raw]
  {wire-type-key :gesso.choreo.identity/wire
   wire-version-key 1
   wire-kind-key kind
   wire-value-key raw})

;; This oracle is authored separately from gesso.choreo.identity. In
;; particular, extension fields are intentionally absent from both wire and
;; decoded identities, and unsupported future versions are distinct errors.
(defn- malformed-versions [wire]
  [[:missing-value (dissoc wire wire-value-key) :invalid-wire-identity]
   [:extra-field (assoc wire :unexpected true) :invalid-wire-identity]
   [:wrong-type (assoc wire wire-type-key :other/wire) :invalid-wire-identity]
   [:future-version (assoc wire wire-version-key 2) :unsupported-wire-version]
   [:unknown-kind (assoc wire wire-kind-key :session) :invalid-kind]
   [:nil-value (assoc wire wire-value-key nil) :invalid-value]
   [:nested-raw (assoc wire wire-value-key (runtime-identity :principal :nested))
    :nested-identity]
   [:missing-version (dissoc wire wire-version-key) :invalid-wire-identity]])

(defn- expected-corpus []
  (vec
   (concat
    (for [kind kinds [raw-id raw] raw-samples
          :let [tagged (runtime-identity kind raw)
                wire (versioned-wire kind raw)]]
      {:case [:roundtrip kind raw-id]
       :status :accepted
       :original (assoc tagged :custom {:source :wire-corpus :kind kind})
       :wire wire
       :decoded tagged
       :wire-valid? true})
    (for [kind kinds
          [variant input error-kind] (malformed-versions
                                      (versioned-wire kind :same-value))]
      {:case [:decode kind variant]
       :status :rejected
       :input input
       :wire-valid? false
       :error/kind error-kind})
    (for [[variant input] [[:raw-keyword :same-value]
                           [:nil nil]
                           [:incomplete-tag
                            {:gesso.choreo.identity/type :gesso.choreo.identity/ref
                             :gesso.choreo.identity/kind :principal}]]]
      {:case [:encode variant]
       :status :rejected
       :input input
       :error/kind :invalid-wire-identity}))))

(defn- validate! [host observed]
  (let [expected (expected-corpus)]
    (when-not (and (vector? observed)
                   (= 75 (count observed))
                   (= (mapv :case expected) (mapv :case observed)))
      (fail! "Wire observations are incomplete, reordered or duplicated."
             {:host host :observed-count (count observed)}))
    (when-not (= expected observed)
      (let [i (first (keep-indexed
                     (fn [n [exp got]] (when (not= exp got) n))
                     (map vector expected observed)))]
        (fail! "Observed identity wire behavior differs from the independent contract."
               {:host host :case-index i :case (get-in expected [i :case])
                :expected (get expected i) :observed (get observed i)})))
    observed))

(defn- compare! [jvm node]
  (validate! :jvm jvm)
  (validate! :node node)
  (when-not (= jvm node)
    (fail! "JVM and Node identity wire observations disagree."
           {:jvm-count (count jvm) :node-count (count node)}))
  true)

(defn- expect-rejected! [name f]
  (try
    (f)
    (throw (ex-info "Wire corpus mutation was silently accepted."
                    {:mutation name :unexpected-pass true}))
    (catch clojure.lang.ExceptionInfo e
      (when-not (and (:wire-parity/failure (ex-data e))
                     (not (:unexpected-pass (ex-data e))))
        (throw e)))))

(defn- mutation-checks! [cases]
  (expect-rejected! :missing-case #(compare! cases (pop cases)))
  (expect-rejected! :reordered
                    #(compare! cases (-> cases
                                         (assoc 0 (nth cases 1))
                                         (assoc 1 (nth cases 0)))))
  (expect-rejected! :forged-original-extension
                    #(compare! cases (assoc-in cases [0 :original :custom] nil)))
  (expect-rejected! :changed-wire-kind
                    #(compare! cases (assoc-in cases [0 :wire wire-kind-key]
                                               :actor)))
  (expect-rejected! :changed-decoded-kind
                    #(compare! cases (assoc-in cases [0 :decoded
                                                      :gesso.choreo.identity/kind]
                                               :actor)))
  (expect-rejected! :leaked-wire-extension
                    #(compare! cases (assoc-in cases [0 :wire :custom] true)))
  (expect-rejected! :forged-wire-predicate
                    #(compare! cases (assoc-in cases [24 :wire-valid?] true)))
  (expect-rejected! :future-version-misclassified
                    #(compare! cases (assoc-in cases [27 :error/kind]
                                               :invalid-wire-identity)))
  (expect-rejected! :nested-raw-misclassified
                    #(compare! cases (assoc-in cases [30 :error/kind]
                                               :invalid-value)))
  (expect-rejected! :forged-encode-success
                    #(compare! cases (assoc-in cases [72 :status] :accepted)))
  (println "JVM/Node wire parity mutations: PASS (10 rejected)"))

(defn -main [& args]
  (when-not (= 2 (count args))
    (fail! "Usage: bb script/check_choreo_wire_parity.clj JVM_LOG NODE_LOG"
           {:args args}))
  (let [jvm (read-corpus! (first args))
        node (read-corpus! (second args))]
    (compare! jvm node)
    (mutation-checks! jvm)
    (println "JVM/Node closed identity wire observations: PASS (75 identical cases)")))

(apply -main *command-line-args*)

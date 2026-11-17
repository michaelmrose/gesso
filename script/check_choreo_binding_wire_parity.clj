(ns gesso.script.check-choreo-binding-wire-parity
  "Compare full sparse identity maps after explicit per-identity wire transport,
   decoding and machine admission across JVM and Node. No Gesso runtime is
   loaded: this contract oracle constructs expected EDN independently."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def ^:private marker "GESSO_CHOREO_BINDING_WIRE_PARITY ")
(def ^:private slots
  [:role :principal :actor :authority :host :command-id :execution-id])
(def ^:private categories (vec (rest slots)))
(def ^:private identity-kind
  {:principal :principal :actor :actor :authority :authority :host :host
   :command-id :command :execution-id :execution})

(defn- fail! [message data]
  (throw (ex-info message (assoc data :binding-wire-parity/failure true))))

(defn- read-corpus! [path]
  (let [lines (->> (str/split-lines (slurp path))
                   (filter #(str/starts-with? % marker))
                   vec)]
    (when-not (= 1 (count lines))
      (fail! "Expected exactly one binding-wire observation marker."
             {:file path :found (count lines)}))
    (try
      (with-open [reader (java.io.PushbackReader.
                          (java.io.StringReader.
                           (subs (first lines) (count marker))))]
        (let [value (edn/read {:eof ::done} reader)
              more (edn/read {:eof ::done} reader)]
          (when-not (= ::done more)
            (fail! "Binding-wire corpus has trailing EDN." {:file path}))
          value))
      (catch clojure.lang.ExceptionInfo e (throw e))
      (catch Throwable e
        (fail! "Binding-wire corpus is not valid EDN."
               {:file path :cause (ex-message e)})))))

(defn- tagged [slot]
  {:gesso.choreo.identity/type :gesso.choreo.identity/ref
   :gesso.choreo.identity/kind (get identity-kind slot)
   :gesso.choreo.identity/value :same-value})

(defn- extended [slot]
  (assoc (tagged slot) :custom (get identity-kind slot)))

(defn- wire [slot]
  {:gesso.choreo.identity.wire/type :gesso.choreo.identity/wire
   :gesso.choreo.identity.wire/version 1
   :gesso.choreo.identity.wire/kind (get identity-kind slot)
   :gesso.choreo.identity.wire/value :same-value})

(defn- sparse-map [mask make-value]
  (into {}
        (for [[index slot] (map-indexed vector slots)
              :when (bit-test mask index)]
          [slot (if (= slot :role) :server (make-value slot))])))

(defn- expected-pending [bindings]
  (cond-> {:kind :local :state 0 :role :server :action :first-step
           :execution-id (:execution-id bindings)}
    (contains? bindings :command-id)
    (assoc :command-id (:command-id bindings))
    (seq (dissoc bindings :role))
    (assoc :identity-bindings bindings)))

(defn- expected-roundtrip [mask]
  (let [original (sparse-map mask extended)
        transported (sparse-map mask wire)
        decoded (sparse-map mask tagged)
        admitted decoded
        machine-bindings (assoc admitted :role :server)]
    {:mask mask :original original :wire transported :decoded decoded
     :admitted admitted
     :machine {:bindings machine-bindings
               :nominal-bindings machine-bindings
               :pending (expected-pending machine-bindings)}}))

(defn- expected-category [slot category]
  (let [decoded (tagged category)
        compatible? (= slot category)]
    {:case [slot category]
     :wire (wire category)
     :decoded decoded
     :binding-admission
     (if compatible?
       {:status :accepted :bindings {slot decoded}}
       {:status :rejected :error/kind :invalid-binding :cause/kind nil})
     :machine-admission
     (if compatible?
       {:status :accepted :bindings {:role :server slot decoded}}
       {:status :rejected :error/kind :invalid-identity-bindings
        :cause/kind :invalid-binding})}))

(defn- expected-corpus []
  {:roundtrips (mapv expected-roundtrip (range 128))
   :categories (vec (for [slot categories category categories]
                      (expected-category slot category)))})

(defn- validate! [host observed]
  (let [expected (expected-corpus)]
    (when-not (and (map? observed)
                   (= #{:roundtrips :categories} (set (keys observed)))
                   (vector? (:roundtrips observed))
                   (vector? (:categories observed))
                   (= 128 (count (:roundtrips observed)))
                   (= 36 (count (:categories observed)))
                   (= (vec (range 128)) (mapv :mask (:roundtrips observed)))
                   (= (mapv :case (:categories expected))
                      (mapv :case (:categories observed))))
      (fail! "Incomplete or reordered sparse binding-wire corpus."
             {:host host :roundtrips (count (:roundtrips observed))
              :category-cases (count (:categories observed))}))
    (when-not (= expected observed)
      (let [group (if (not= (:roundtrips expected) (:roundtrips observed))
                    :roundtrips :categories)
            i (first (keep-indexed
                      (fn [n [a b]] (when (not= a b) n))
                      (map vector (get expected group) (get observed group))))]
        (fail! "Wire-decoded bindings or machine admission violate the contract."
               {:host host :group group :index i
                :expected (get-in expected [group i])
                :observed (get-in observed [group i])})))
    observed))

(defn- compare! [jvm node]
  (validate! :jvm jvm)
  (validate! :node node)
  (when-not (= jvm node)
    (fail! "JVM and Node binding-wire observations disagree." {}))
  true)

(defn- expect-rejected! [label thunk]
  (try
    (thunk)
    (throw (ex-info "Binding-wire mutation passed unexpectedly."
                    {:mutation label :unexpected-pass true}))
    (catch clojure.lang.ExceptionInfo e
      (when-not (and (:binding-wire-parity/failure (ex-data e))
                     (not (:unexpected-pass (ex-data e))))
        (throw e)))))

(defn- negative-checks! [cases]
  (expect-rejected! :missing-mask
                    #(compare! cases (update cases :roundtrips pop)))
  (expect-rejected! :reordered-mask
                    #(compare! cases (-> cases
                                         (assoc-in [:roundtrips 0]
                                                   (get-in cases [:roundtrips 1]))
                                         (assoc-in [:roundtrips 1]
                                                   (get-in cases [:roundtrips 0])))))
  (expect-rejected! :lost-role
                    #(compare! cases (update-in cases [:roundtrips 0 :machine :bindings]
                                                dissoc :role)))
  (expect-rejected! :leaked-runtime-extension
                    #(compare! cases (assoc-in cases
                                               [:roundtrips 2 :wire :principal :custom]
                                               :principal)))
  (expect-rejected! :wrong-wire-kind
                    #(compare! cases (assoc-in cases
                                               [:roundtrips 2 :wire :principal
                                                :gesso.choreo.identity.wire/kind]
                                               :actor)))
  (expect-rejected! :lost-decoded-category
                    #(compare! cases (assoc-in cases
                                               [:roundtrips 2 :decoded :principal
                                                :gesso.choreo.identity/kind]
                                               :actor)))
  (expect-rejected! :missing-descriptor-command
                    #(compare! cases (update-in cases [:roundtrips 32 :machine :pending]
                                                dissoc :command-id)))
  (expect-rejected! :invented-identity
                    #(compare! cases (assoc-in cases
                                               [:roundtrips 0 :admitted :actor]
                                               (tagged :actor))))
  (expect-rejected! :forged-category-admission
                    #(compare! cases (assoc-in cases
                                               [:categories 1 :binding-admission :status]
                                               :accepted)))
  (expect-rejected! :wrong-machine-error-cause
                    #(compare! cases (assoc-in cases
                                               [:categories 1 :machine-admission :cause/kind]
                                               :invalid-kind)))
  (expect-rejected! :reordered-category-cases
                    #(compare! cases (-> cases
                                         (assoc-in [:categories 0]
                                                   (get-in cases [:categories 1]))
                                         (assoc-in [:categories 1]
                                                   (get-in cases [:categories 0])))))
  (println "JVM/Node binding-wire mutations: PASS (11 rejected)"))

(defn -main [& args]
  (when-not (= 2 (count args))
    (fail! "Usage: bb script/check_choreo_binding_wire_parity.clj JVM_LOG NODE_LOG"
           {:args args}))
  (let [jvm (read-corpus! (first args))
        node (read-corpus! (second args))]
    (compare! jvm node)
    (negative-checks! jvm)
    (println "JVM/Node complete sparse binding wire observations: PASS (164 identical cases)")))

(apply -main *command-line-args*)

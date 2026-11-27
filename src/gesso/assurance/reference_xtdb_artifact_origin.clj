(ns gesso.assurance.reference-xtdb-artifact-origin
  "Local selected provider origin observation, not executing-code attestation."
  (:require [gesso.assurance.reference-xtdb-provider-contract :as contract]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import (java.io File)
           (java.net JarURLConnection URL)
           (java.nio.file Files)
           (java.security MessageDigest)
           (java.util HexFormat)))

(def profile :selected-xtdb-artifact-origin-v1)
(def ^:private target contract/reviewed-provider)
(def ^:private derive-contract contract/derive!)
(def ^:private api-roots {#'xt/execute-tx xt/execute-tx #'xt/submit-tx xt/submit-tx
                          #'xt/q xt/q #'xtn/start-node xtn/start-node})
(def ^:private markers
  {"com.xtdb/xtdb-api" {:classes [xtdb.api.DataSource xtdb.api.TransactionResult]
                       :resources ["xtdb/api.clj"]}
   "com.xtdb/xtdb-core" {:classes [xtdb.indexer.TxIndexer]
                        :resources ["xtdb/node.clj"]}})

(defn- reject! [reason]
  (throw (ex-info "Unqualified provider artifact origin" {:reason reason})))

(defn- local-file! [^URL url]
  (when-not (and url (= "file" (.getProtocol url)))
    (reject! :nonlocal-provider-artifact))
  (let [f (.getCanonicalFile (File. (.toURI url)))]
    (when-not (and (.isFile f) (.endsWith (.getName f) ".jar"))
      (reject! :nonjar-provider-artifact))
    f))

(defn- unique-resource! [urls]
  (when-not (= 1 (count urls)) (reject! :ambiguous-provider-resource))
  (let [url (first urls)]
    (when-not (= "jar" (.getProtocol ^URL url)) (reject! :nonjar-provider-resource))
    (let [^JarURLConnection conn (.openConnection ^URL url)]
      (.setUseCaches conn false)
      (local-file! (.getJarFileURL conn)))))

(defn- resource-origin! [path]
  (unique-resource! (vec (enumeration-seq (.getResources (clojure.lang.RT/baseLoader) path)))))

(defn- class-origin! [^Class c]
  (local-file! (some-> c .getProtectionDomain .getCodeSource .getLocation)))

(defn- digest [^File f]
  (with-open [in (Files/newInputStream (.toPath f) (make-array java.nio.file.OpenOption 0))]
    (let [md (MessageDigest/getInstance "SHA-256") buf (byte-array 65536)]
      (loop []
        (let [n (.read in buf)]
          (when (pos? n) (.update md buf 0 n) (recur))))
      (.formatHex (HexFormat/of) (.digest md)))))

(defn- check-artifact! [expected origins]
  (when-not (and (seq origins) (apply = origins)) (reject! :split-provider-artifact-origin))
  (when-not (= expected (digest (first origins))) (reject! :provider-artifact-digest-mismatch))
  expected)

(defn- boundaries! []
  (when-not (and (= target contract/reviewed-provider)
                 (identical? derive-contract contract/derive!)
                 (every? (fn [[v f]] (identical? @v f)) api-roots))
    (reject! :provider-origin-boundary-replaced)))

(defn observe!
  "Observe representative loaded JVM class code sources and unique visible
   Clojure source resources, requiring matching local reviewed JAR bytes.
   The Clojure function classes need not have CodeSources. Their Var roots
   are pinned locally, not authenticated as compilations of those resources."
  []
  (let [base {:profile profile :provider-correspondence :not-established
              :loaded-provider-identity :not-established :proof-discharge :not-performed
              :premise-acceptance :not-performed :authority :none :certificate :not-issued}]
    (try
      (boundaries!)
      (let [artifacts (into {} (map (fn [[id {:keys [classes resources]}]]
                                    (let [class-files (mapv class-origin! classes)
                                          resource-files (mapv resource-origin! resources)]
                                      [id {:sha256 (check-artifact! (get-in target [:artifacts id])
                                                                   (into class-files resource-files))
                                           :classes (mapv #(.getName ^Class %) classes)
                                           :resources resources}])) markers))]
        (boundaries!)
        (merge base {:status :matched-selected-local-artifact-origins :provider target
                     :artifacts artifacts :scope :selected-code-sources-and-visible-resources
                     :unverified #{:generated-clojure-function-bytecode :complete-dependency-closure
                                   :runtime-integrity :provider-semantics :continuous-artifact-identity}}))
      (catch clojure.lang.ExceptionInfo e
        (assoc base :status :not-established :reason (:reason (ex-data e))))
      (catch Exception _
        (assoc base :status :not-established :reason :provider-origin-unavailable)))))

(def ^:private original-observe observe!)

(defn derive!
  "Re-derive the exact original open contract, bracketed by the same local
   origin observations. Neither origin matching nor finite experiments
   accept or discharge a provider premise."
  [admission source declarations fingerprint cases]
  (let [before (original-observe)]
    (if-not (identical? original-observe observe!)
      (assoc (select-keys before [:profile :proof-discharge :premise-acceptance :authority :certificate
                                  :provider-correspondence :loaded-provider-identity])
             :status :not-established :reason :provider-origin-boundary-replaced)
      (if-not (= :matched-selected-local-artifact-origins (:status before)) before
      (let [c (derive-contract admission source declarations fingerprint cases target)
            after (original-observe)]
        (if (and (identical? original-observe observe!) (= before after) (= :open-provider-contract (:status c))
                 (= fingerprint (get-in c [:scope :catalog-fingerprint]))
                 (= target (get-in c [:scope :provider]))
                 (every? #(= :open (:discharge-state %)) (vals (:premises c))))
          (assoc after :status :open-contract-with-selected-artifact-origins
                 :contract c :open-premise-fingerprints (set (map :premise-fingerprint (vals (:premises c)))))
          (assoc (select-keys before [:profile :proof-discharge :premise-acceptance :authority :certificate
                                      :provider-correspondence :loaded-provider-identity])
                 :status :not-established :reason :provider-origin-or-contract-mismatch)))))))

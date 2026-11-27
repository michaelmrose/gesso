(ns gesso.assurance.reference-xtdb-provider-trust
  "Explicit selection of the exact open provider contract, not attestation."
  (:require [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-xtdb-artifact-origin :as origin]))

(def profile :declared-xtdb-provider-trust-v1)
(deftype ProviderScope [])
(def ^:private scopes (java.util.WeakHashMap.))
(def ^:private original-derive origin/derive!)
(def ^:private original-match assumption/match-premise!)
(def ^:private base
  {:profile profile :provider-correspondence :not-established
   :loaded-provider-identity :not-established :writer-coverage :not-established
   :universal-refinement :not-established :proof-discharge :not-performed
   :premise-acceptance :not-performed :authority :none :certificate :not-issued})

(defn- reject! [reason]
  (throw (ex-info "Unqualified provider trust declaration" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-derive origin/derive!)
                 (identical? original-match assumption/match-premise!))
    (reject! :provider-trust-boundary-replaced)))

(defn- qualified! [args]
  (let [report (apply original-derive args)
        c (:contract report)
        premises (:premises c)]
    (when-not (and (= :open-contract-with-selected-artifact-origins (:status report))
                   (= :open-provider-contract (:status c))
                   (= :not-performed (:proof-discharge report) (:proof-discharge c)
                      (:premise-acceptance report) (:premise-acceptance c))
                   (= :not-established (:provider-correspondence report) (:loaded-provider-identity report))
                   (= (nth args 3) (get-in c [:scope :catalog-fingerprint]))
                   (= (:provider report) (get-in c [:scope :provider]))
                   (seq premises)
                   (every? (fn [[id row]] (and (= id (get-in row [:assumption :id]))
                                              (= :open (:discharge-state row)))) premises)
                   (= (count premises) (count (:open-premise-fingerprints report)))
                   (= (:open-premise-fingerprints report) (set (map :premise-fingerprint (vals premises)))))
      (reject! :unqualified-open-provider-contract))
    report))

(defn- scope-binding [report]
  (let [c (:contract report)]
    {:contract-scope (:scope c) :artifact-origins (:artifacts report)
     :premise-fingerprints (into {} (map (fn [[id row]] [id (:premise-fingerprint row)])) (:premises c))
     :condition-links (:condition-links c) :remaining-conditions (:remaining-conditions c)
     :unverified (:unverified report)}))

(defn prepare!
  "Issue a process-local scope for the exact original admission and open
   contract. No physical node, owner identity or deployment trust is inferred."
  [admission source declarations fingerprint cases]
  (try
    (boundaries!)
    (let [args [admission source declarations fingerprint cases]
          report (qualified! args)
          scope (assoc (scope-binding report) :profile profile :scope-ref (str (java.util.UUID/randomUUID)))
          handle (ProviderScope.)]
      (boundaries!)
      (locking scopes (.put scopes handle {:args args :scope scope :premises (get-in report [:contract :premises])}))
      (merge base {:status :open-provider-trust-requirements :scope scope :handle handle
                   :premises (get-in report [:contract :premises]) :origin-report report}))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (:reason (ex-data e))))))

(defn- plain-map? [x] (and (map? x) (not (record? x)) (nil? (meta x))))

(defn- select! [scope required declaration]
  (let [offered (:premises declaration) ids (set (keys required))]
    (when-not (and (plain-map? declaration)
                   (= #{:scope :acceptance :premises} (set (keys declaration)))
                   (= scope (:scope declaration)) (= :trust-open-premises (:acceptance declaration))
                   (vector? offered) (nil? (meta offered)) (every? plain-map? offered)
                   (= (count ids) (count offered)) (= ids (set (map :id offered))))
      (reject! :incorrect-provider-premise-selection))
    (let [by-id (into {} (map (juxt :id identity)) offered)]
      (doseq [[id row] required]
        (let [raw (:assumption row)]
          (original-match raw (by-id id))
          (when-not (every? ids (:dependencies raw))
            (reject! :missing-provider-premise-dependency)))))
    (set (map :premise-fingerprint (vals required)))))

(defn declare!
  "Record explicit trust in every exact open provider premise. Re-derive
   the original issued contract and artifact observation before returning.
   The declaration is not evidence of semantic truth or loaded bytecode,
   does not establish deployment isolation, and confers no authority."
  [admission handle declaration]
  (try
    (boundaries!)
    (let [entry (when (instance? ProviderScope handle) (locking scopes (.get scopes handle)))]
      (when-not (and entry (identical? admission (first (:args entry))))
        (reject! :unissued-provider-trust-scope))
      (let [{:keys [args scope premises]} entry
            accepted (select! scope premises declaration)
            report (qualified! args)]
        (when-not (and (= (dissoc scope :profile :scope-ref) (scope-binding report))
                       (= premises (get-in report [:contract :premises]))
                       (= accepted (:open-premise-fingerprints report)))
          (reject! :provider-trust-contract-changed))
        (boundaries!)
        (merge base {:status :conditional-declared-provider-trust :scope scope
                     :premises premises :origin-report report :trusted-assumptions accepted
                     :premise-acceptance :explicit-declaration-only
                     :deployment-isolation :not-established :declarant-authentication :not-established})))
    (catch clojure.lang.ExceptionInfo e
      (assoc base :status :not-established :reason (:reason (ex-data e))))))

(ns gesso.live.outbox.command-authority-owner
  "Local installed command authority qualification and explicit environmental adoption."
  (:require [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.assurance.reference-configured-recovery-inventory :as inventory]
            [gesso.live.outbox.http-operation :as http]
            [gesso.live.outbox.http-route :as route]))

(def token-key :gesso.live.outbox.host/command-authority-token)
(def ^:private owners (java.util.WeakHashMap.))
(def ^:private original-read http/read-command-authority-binding!)
(def ^:private original-describe recovery/describe-command-authority-contract!)

(def ^:private original-select recovery/select-command-authority-contract!)
(def ^:private original-acceptance? route/authority-acceptance?)

(defn- reject! [reason]
  (throw (ex-info "Unqualified installed command authority" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-read http/read-command-authority-binding!)
                 (identical? original-describe recovery/describe-command-authority-contract!)
                 (identical? original-select recovery/select-command-authority-contract!)
                 (identical? original-acceptance? route/authority-acceptance?))
    (reject! :command-authority-owner-boundary-replaced)))

(defn- register! [system lock active handler assembly options]
  (boundaries!)
  (let [binding (original-read handler assembly options)]
    (when-not binding (reject! :unissued-command-authority-installation))
    (locking owners
      (.put owners (get system token-key)
            {:system (java.lang.ref.WeakReference. system)
             :lock lock :active active :handler handler :assembly assembly
             :options options :binding binding}))))

(defn- with-current-owner [system f]
  (let [entry (locking owners (.get owners (get system token-key)))]
    (when-not (and entry (identical? system (.get ^java.lang.ref.WeakReference (:system entry))))
      (reject! :unissued-command-authority-host))
    (locking (:lock entry)
      (when-not @(:active entry) (reject! :command-authority-host-retired))
      (boundaries!)
      (when-not (= (:binding entry) (original-read (:handler entry) (:assembly entry) (:options entry)))
        (reject! :command-authority-installation-changed))
      (let [result (f (:binding entry) (:options entry))]
        (boundaries!)
        (when-not (and @(:active entry)
                       (= (:binding entry) (original-read (:handler entry) (:assembly entry) (:options entry))))
          (reject! :command-authority-installation-changed))
        result))))

(defn read-binding!
  "Return the exact current local host's installation identity, not proof."
  [system]
  (with-current-owner system (fn [binding _] binding)))

(defn- match-contract! [binding contract]
  (let [origins (get-in contract [:scope :command-subject :selected-command-origins])]
    (when-not (and (= :open-command-authority-contract (:status contract))
                   (= :matched-original-issued-cut (:subject-provenance contract))
                   (= (:policy binding) (get-in contract [:scope :policy]))
                   (= 3 (count origins))
                   (every? #(= binding (get-in % [:http-route :authority-binding])) origins))
      (reject! :command-authority-original-installation-mismatch))
    (assoc contract :configured-authority :matched-current-local-installation
           :installation-binding binding)))

(defn- qualify-cut! [system comparison binding]
  (let [admission (:gesso.live.outbox.host/mapped-admission system)]
    (when-not admission (reject! :command-authority-requires-mapped-admission))
    (match-contract! binding (original-describe comparison admission (:policy binding)))))

(defn qualify!
  "Describe the original issued cut only for the exact currently installed
   owner and its configured callbacks. No acceptance or proof discharge."
  [system comparison]
  (with-current-owner system (fn [binding _] (qualify-cut! system comparison binding))))

(defn- adopt-selected! [contract declaration selected]
  (when-not (and (original-acceptance? declaration)
                 (= :selected-open-command-authority-contract (:status selected))
                 (= :exact-structural-only (:selection selected))
                 (= (dissoc contract :configured-authority :installation-binding :status)
                    (dissoc selected :configured-authority :status :selection))
                 (= (:premise-ids declaration) (set (map #(get-in % [:assumption :id]) (:obligations contract)))))
    (reject! :command-authority-selection-mismatch))
  (assoc contract :status :conditional-configured-command-authority
         :deployment-adoption :configured-local-host
         :premise-acceptance :explicit-declaration-only
         :trusted-assumptions (:open-premise-fingerprints contract)
         :deployment-acceptance declaration))

(defn adopt!
  "Adopt only startup-selected environmental premises for the original cut.
   No supplied premise report, request metadata, authentication or discharge."
  [system comparison]
  (with-current-owner
    system
    (fn [binding options]
      (let [declaration (:authority-acceptance options)]
        (when-not (original-acceptance? declaration)
          (reject! :command-authority-adoption-not-selected))
        (let [contract (qualify-cut! system comparison binding)
              selected (original-select comparison (:gesso.live.outbox.host/mapped-admission system)
                                        (:policy binding) (mapv :assumption (:obligations contract)))]
          (adopt-selected! contract declaration selected))))))

(def ^:private original-adopt adopt!)
(def ^:private original-inventory-extension inventory/extend-command-authority!)

(defn extend-inventory!
  "Join only explicitly startup-selected command trust, under the same live
   owner lock as adoption. A policy-only host retains the original inventory."
  [system comparison base refined]
  (if-not (get system token-key)
    base
    (with-current-owner
      system
      (fn [_ options]
        (when-not (and (identical? original-adopt adopt!)
                       (identical? original-inventory-extension inventory/extend-command-authority!))
          (reject! :command-authority-owner-boundary-replaced))
        (if (contains? options :authority-acceptance)
          (original-inventory-extension base refined (original-adopt system comparison))
          base)))))

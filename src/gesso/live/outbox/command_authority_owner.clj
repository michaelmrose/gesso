(ns gesso.live.outbox.command-authority-owner
  "Local installed command authority qualification. Never adopts premises."
  (:require [gesso.assurance.reference-mapped-observed-recovery :as recovery]
            [gesso.live.outbox.http-operation :as http]))

(def token-key :gesso.live.outbox.host/command-authority-token)
(def ^:private owners (java.util.WeakHashMap.))
(def ^:private original-read http/read-command-authority-binding!)
(def ^:private original-describe recovery/describe-command-authority-contract!)

(defn- reject! [reason]
  (throw (ex-info "Unqualified installed command authority" {:reason reason})))

(defn- boundaries! []
  (when-not (and (identical? original-read http/read-command-authority-binding!)
                 (identical? original-describe recovery/describe-command-authority-contract!))
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
      (let [result (f (:binding entry))]
        (boundaries!)
        (when-not (and @(:active entry)
                       (= (:binding entry) (original-read (:handler entry) (:assembly entry) (:options entry))))
          (reject! :command-authority-installation-changed))
        result))))

(defn read-binding!
  "Return the exact current local host's installation identity, not proof."
  [system]
  (with-current-owner system identity))

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

(defn qualify!
  "Describe the original issued cut only for the exact currently installed
   owner and its configured callbacks. No acceptance or proof discharge."
  [system comparison]
  (with-current-owner
    system
    (fn [binding]
      (let [admission (:gesso.live.outbox.host/mapped-admission system)]
        (when-not admission (reject! :command-authority-requires-mapped-admission))
        (match-contract! binding (original-describe comparison admission (:policy binding)))))))

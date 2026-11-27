(ns gesso.assurance.reference-command-authority
  "Open application authority contract for a selected command subject."
  (:require [gesso.assurance.assumption :as assumption]
            [gesso.live.outbox.http-route :as route]))

(def profile :selected-command-authority-contract-v2)

(defn- reject! [reason]
  (throw (ex-info "Unqualified command authority contract" {:reason reason})))

(defn- plain-map? [x]
  (and (map? x) (not (record? x)) (nil? (meta x))))

(defn describe!
  "Describe two environmental premises for the selected HTTP command subject.
   Policy names are declarations, not authenticated configuration identities.
   This structural function cannot establish that its input report was issued."
  [command policy]
  (let [scope (:scope command)
        origins (:selected-command-origins command)
        required (:obligations command)
        fingerprints (:open-premise-fingerprints command)]
    (when-not (route/authority-policy? policy)
      (reject! :invalid-command-authority-policy))
    (when-not (and (= :matched-issued-command-bindings (:origin-correspondence scope))
                   (= :not-established (:command-authentication command))
                   (= :not-performed (:premise-acceptance command))
                   (= 3 (count origins))
                   (= (:selected-tx-ids scope) (mapv :selected-tx-id origins))
                   (every? #(and (= :registered-server-invocation (:origin %))
                                 (= :conditional-on-trusted-principal-resolver (:authentication %))
                                 (= :locally-accepted (get-in % [:http-route :request-gate]))
                                 (= :matched-canonical-htmx-handoff (get-in % [:http-route :evidence]))) origins)
                   (= [:reference-command/principal-resolution :reference-command/command-attribution]
                      (mapv #(get-in % [:assumption :id]) required))
                   (every? #(and (= :open (:discharge-state %))
                                 (= scope (get-in % [:assumption :subject :interface]))) required)
                   (= fingerprints (set (map :premise-fingerprint required))))
      (reject! :incomplete-issued-http-command-subject))
    (let [bindings (mapv #(get-in % [:http-route :authority-binding]) origins)
          installed (first bindings)]
      (when (and (some some? bindings)
                 (not (and (route/authority-binding? installed)
                           (every? #(= installed %) bindings)
                           (= policy (:policy installed)))))
        (reject! :command-authority-installation-mismatch)))
    (let [id :reference-command-authority/selected-http-execution
          interface {:profile profile :command-subject scope :policy policy
                     :original-command-premises fingerprints}
          describe (fn [premise proposition boundary]
                     (assumption/describe-assumption
                      {:id premise :subject {:kind :runtime :id id :interface interface}
                       :scope {:kind :runtime :id id} :proposition [proposition]
                       :parameters {} :semantic-context {:profile profile}
                       :dependencies #{} :origin :environment :implementation-boundary boundary}))
          obligations [(describe :reference-command-authority/request-gate
                                 :reference-command-authority/faithful-request-gate-acceptance
                                 :gesso.live.outbox.http-operation/authorize-request?)
                       (describe :reference-command-authority/principal-session
                                 :reference-command-authority/authenticated-principal-session-resolution
                                 :gesso.live.optimistic.server/principal-fn)]]
      {:profile profile :status :open-command-authority-contract :scope interface
       :obligations obligations :open-premise-fingerprints (set (map :premise-fingerprint obligations))
       :premise-acceptance :not-performed :configured-authority :not-established
       :command-authentication :not-established :proof-discharge :not-performed
       :authority :none :certificate :not-issued})))

(defn select!
  "Select the exact two open environmental assumptions, not the original
   command implementation premise. Selection alone grants no host adoption."
  [command policy offered]
  (let [contract (describe! command policy)
        required (mapv :assumption (:obligations contract))]
    (when-not (and (vector? offered) (nil? (meta offered)) (every? plain-map? offered)
                   (= (count required) (count offered))
                   (= (set (map :id required)) (set (map :id offered))))
      (reject! :incorrect-command-authority-premise-selection))
    (let [by-id (into {} (map (juxt :id identity)) offered)]
      (doseq [premise required]
        (assumption/match-premise! premise (get by-id (:id premise))))
      (assoc contract :status :selected-open-command-authority-contract
             :selection :exact-structural-only))))

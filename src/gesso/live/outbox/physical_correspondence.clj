(ns gesso.live.outbox.physical-correspondence
  "Data-only observations for the selected mapped writer profile. Compose
   checked initialization and a checked physical transition without claiming
   coverage of intervening history or granting any submission authority."
  (:require [gesso.live.consistency.xtdb :as xtdb]))

;; A weak generation key survives only while the actual policy is installed
;; (or its opaque generation is otherwise retained). Never store a node,
;; registration token, or raw valuation in the exposed report.
(def ^:private activations (java.util.WeakHashMap.))

(defn record-activation!
  "Trusted host owner records the successful pinned initial observation for
   its exact installed policy and issued admission. Not an activation gate."
  [node policy token admission observation]
  (when-not (and token (identical? token (xtdb/submission-policy-token node policy)))
    (throw (ex-info "Physical observation has no installed policy."
                    {:reason :uninstalled-correspondence-policy})))
  (locking activations
    (.put activations token {:admission admission :observation observation}))
  nil)

(defn initialization-observation
  "Observe only the exact admission's current policy-generation evidence.
   A different registry or a retired policy supplies no such observation."
  [connectable admission]
  (when-let [token (xtdb/current-submission-policy-token connectable)]
    (let [entry (locking activations (.get activations token))]
      (when (identical? admission (:admission entry))
        (:observation entry)))))

(def ^:private residual-obligations
  #{:complete-intervening-history-coverage
    :transaction-internal-read-basis-refinement
    :unrestricted-physical-writer-closure
    :principal-authentication-and-business-authorization
    :complete-application-refinement})

(defn transition-report
  "Combine successful checks from the executor's pinned observations.
   A matching initial observation is optional for a standalone executor;
   it must precede the observed step. This is diagnostic evidence, never
   a certificate or proof of all reachable states."
  [connectable admission evidence commit-time prestate semantic]
  (let [initial (initialization-observation connectable admission)
        initial? (and initial
                      (= (:catalog-fingerprint initial) (:catalog-fingerprint evidence))
                      (not (pos? (compare (:system-time initial) (:system-time prestate))))
                      (neg? (compare (:system-time initial) commit-time)))]
    {:profile :selected-mapped-physical-correspondence-v1
     :authority :none
     :status (if initial? :initialized-transition-checked :transition-checked-only)
     :initialization (if initial? initial :not-established-for-step)
     :transition {:command-id (:xt/id evidence)
                  :alternative (:alternative evidence)
                  :system-time commit-time
                  :fingerprints (select-keys evidence [:catalog-fingerprint :induction-fingerprint
                                                       :mapping-fingerprint :transition-fingerprint
                                                       :input-derivation-fingerprint :witness-fingerprint
                                                       :poststate-fingerprint])
                  :prestate prestate
                  :poststate (:physical-correspondence semantic)
                  :relation (:abstract-relation semantic)
                  :inputs (:input-correspondence semantic)}
     :open-obligations (cond-> residual-obligations
                         (not initial?) (conj :initialization-correspondence-for-step))}))

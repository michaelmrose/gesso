(ns gesso.assurance.proof.xtdb-gateway-local-key-lifecycle
  "v1899: Fail-closed process-local root-key rotation and revocation for
   the v1898 role-scoped, signed XTDB witness CANDIDATE handshake.

   A revision-checked process-local policy serializes issued challenges,
   inspection, root revocation and configured-key rotation. Revoking or
   rotating invalidates ALL challenges issued through this policy; reused
   roots and stale revisions are rejected. A newly configured root must
   still provide its own valid, bounded v1898 delegation signature.

   This is NOT independently authenticated root provisioning, an external
   revocation registry, durable key history, cross-process enforcement,
   actual physical observation, transaction causality or certification.
   A caller controlling configuration can replace and sign its own roots.
   The restricted controls govern ONLY this process-local wrapper; no
   certificate, witness admission, retry or transaction authority follows."
  (:require [gesso.assurance.proof.xtdb-gateway-delegated-candidate-key
             :as delegated])
  (:import [java.util IdentityHashMap]))

(def lifecycle-type
  :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/local-root-policy)
(def checked-type
  :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/local-checked-candidate)
(def ^:private max-generations 32)
(def ^:private max-pending 16)
(def ^:private original-prepare delegated/prepare-delegated-candidate-verifier!)

(defn- reject! [reason]
  (throw (ex-info "Invalid local candidate root-key lifecycle operation."
                  {:xtdb-local-key-lifecycle/rejection true :reason reason})))

(defn- prepare-delegation! [config]
  (when-not (identical? original-prepare
                         (var-get #'delegated/prepare-delegated-candidate-verifier!))
    (reject! :substituted-delegation-preparer))
  (original-prepare config))

(defn prepare-process-local-root-policy!
  "Configure a single process-local root delegation and four transitions:

   :status!  []                           -> redacted policy state
   :issue!   [original-gateway exact-return] -> owned challenge
   :inspect! [exact-challenge signed-response] -> non-authoritative check
   :revoke!  [expected-policy-revision]   -> revocation status
   :rotate!  [expected-policy-revision new-signed-delegation-config]
                                      -> rotated active status

   Every revocation/rotation clears outstanding challenges. Operations on
   one policy are serialized, including the independent pinned XTDB read on
   issue and the signature verification on inspect. The expected revision
   prevents lost updates and stale rotations, not unauthorized operators.
   No root/provisioning identity or external revoke source is authenticated;
   the policy cannot discharge any physical assurance obligation."
  [initial-config]
  (let [initial (prepare-delegation! initial-config)
        monitor (Object.)
        pending (IdentityHashMap.)
        first-root (:configured-root-spki-fingerprint initial)
        state (atom {:policy-revision 1
                     :root-generation 1
                     :lifecycle :active
                     :active-verifier initial
                     :root-fingerprint first-root
                     :delegate-fingerprint (:delegated-key-spki-fingerprint initial)
                     :previous-root-fingerprints #{first-root}})
        summary (fn []
                  (let [{:keys [policy-revision root-generation lifecycle
                                root-fingerprint delegate-fingerprint]} @state]
                    {:type lifecycle-type
                     :version 1
                     :policy-revision policy-revision
                     :root-generation root-generation
                     :lifecycle lifecycle
                     :configured-root-spki-fingerprint root-fingerprint
                     :delegated-key-spki-fingerprint delegate-fingerprint
                     :outstanding-challenge-count (.size pending)
                     :max-root-generations max-generations
                     :challenge-cap max-pending
                     :revocation-scope :this-wrapper-and-process-only
                     :root-configuration-origin :process-supplied-not-authenticated
                     :root-provisioning-independence :not-established
                     :cross-process-revocation :not-established
                     :durable-revocation-history :not-established
                     :physical-event-correspondence :not-established
                     :witness-admission :not-admitted
                     :proof-discharge :none
                     :certificate :not-issued
                     :transaction-authority :none
                     :retry-authority :none
                     :authority :none}))
        check-revision! (fn [expected]
                          (when-not (and (integer? expected)
                                         (= expected (:policy-revision @state)))
                            (reject! :stale-local-policy-revision)))
        issue! (fn [gateway original-return]
                 (locking monitor
                   (let [{:keys [lifecycle active-verifier policy-revision]} @state]
                     (when-not (= :active lifecycle)
                       (reject! :local-root-revoked))
                     (when (>= (.size pending) max-pending)
                       (reject! :too-many-local-challenges))
                     (let [issued ((:issue! active-verifier) gateway original-return)]
                       (.put pending issued policy-revision)
                       issued))))
        inspect! (fn [issued response]
                   (locking monitor
                     (let [issued-revision (.remove pending issued)
                           {:keys [lifecycle active-verifier policy-revision
                                   root-generation]} @state]
                       (when-not issued-revision
                         (reject! :not-a-current-local-challenge))
                       (when-not (and (= :active lifecycle)
                                      (= policy-revision issued-revision))
                         (reject! :local-challenge-revoked-or-rotated))
                       (let [checked ((:inspect! active-verifier) issued response)]
                         (assoc checked
                                :type checked-type
                                :version 1
                                :local-policy-revision policy-revision
                                :local-root-generation root-generation
                                :revocation-check :active-at-local-inspection
                                :independent-revocation :not-established
                                :independent-root-provisioning :not-established
                                :physical-event-correspondence :not-established
                                :witness-admission :not-admitted
                                :proof-discharge :none
                                :certificate :not-issued
                                :transaction-authority :none
                                :retry-authority :none
                                :authority :none)))))
        revoke! (fn [expected]
                  (locking monitor
                    (check-revision! expected)
                    (when-not (= :active (:lifecycle @state))
                      (reject! :local-root-already-revoked))
                    (swap! state (fn [s]
                                   (-> s
                                       (update :policy-revision inc)
                                       (assoc :lifecycle :revoked
                                              :active-verifier nil))))
                    (.clear pending)
                    (summary)))
        rotate! (fn [expected new-config]
                  ;; Reject stale plans before signature verification, then
                  ;; check again while serialized to avoid rotate/revoke ABA.
                  (locking monitor (check-revision! expected))
                  (let [new-verifier (prepare-delegation! new-config)
                        new-root (:configured-root-spki-fingerprint new-verifier)]
                    (locking monitor
                      (check-revision! expected)
                      (when (>= (:root-generation @state) max-generations)
                        (reject! :local-root-generation-limit))
                      (when (contains? (:previous-root-fingerprints @state) new-root)
                        (reject! :previously-configured-root-not-reusable))
                      (swap! state
                             (fn [s]
                               (-> s
                                   (update :policy-revision inc)
                                   (update :root-generation inc)
                                   (assoc :lifecycle :active
                                          :active-verifier new-verifier
                                          :root-fingerprint new-root
                                          :delegate-fingerprint
                                          (:delegated-key-spki-fingerprint new-verifier))
                                   (update :previous-root-fingerprints conj new-root))))
                      (.clear pending)
                      (summary))))]
    {:type lifecycle-type
     :version 1
     :status! (fn [] (locking monitor (summary)))
     :issue! issue!
     :inspect! inspect!
     :revoke! revoke!
     :rotate! rotate!
     :root-provisioning-independence :not-established
     :cross-process-revocation :not-established
     :durable-revocation-history :not-established
     :witness-admission :not-admitted
     :proof-discharge :none
     :certificate :not-issued
     :transaction-authority :none
     :retry-authority :none
     :authority :none}))

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

   v1900: Within the same root generation a new root-signed delegation
   can replace a retired delegate. A separate delegated-key revocation
   transitions to :delegation-revoked without revoking the root. Grant IDs
   and delegate key fingerprints retire monotonically across subsequent
   local root rotations. Outstanding v1899 challenges are consumed in the
   original candidate verifier as well as removed from this wrapper, so
   repeated rotations cannot strand one-use global challenge capacity.
   None of these facts proves independent signer identity or cross-process
   and durable revocation completeness.

   v1901: Expose strictly read-only, canonicalizable checkpoint material
   containing the ACTUAL local revision, generations and complete retirement
   sets. It can be signed out-of-band and checked against the current policy;
   exporting or signing it does not independently authenticate this process
   or establish persistent revocation.
   The restricted controls govern ONLY this process-local wrapper; no
   certificate, witness admission, retry or transaction authority follows."
  (:require [gesso.assurance.proof.xtdb-gateway-delegated-candidate-key
             :as delegated])
  (:import [java.util IdentityHashMap UUID]))

(def lifecycle-type
  :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/local-root-policy)
(def checked-type
  :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/local-checked-candidate)
(def ^:private max-generations 32)
(def ^:private max-delegations 128)
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

(defn- drain-pending!
  "Invalidate all locally owned pending candidate challenges, and consume
   their underlying v1898 single-use slots even on local key retirement."
  [^IdentityHashMap pending]
  (doseq [entry (iterator-seq (.iterator (.entrySet pending)))]
    (let [owned (.getValue entry)
          challenge (.getKey entry)]
      (try ((:inspect! (:verifier owned)) challenge
            {:signature-base64 "invalid-on-local-policy-retirement$"})
           (catch clojure.lang.ExceptionInfo _))))
  (.clear pending))

(defn prepare-process-local-root-policy!
  "Configure a single process-local root delegation and four transitions:

   :status!  []                           -> redacted policy state
   :issue!   [original-gateway exact-return] -> owned challenge
   :inspect! [exact-challenge signed-response] -> non-authoritative check
   :revoke!  [expected-policy-revision]   -> revocation status
   :rotate!  [expected-policy-revision new-signed-delegation-config]
                                      -> rotated active status
   :revoke-delegation! [expected-policy-revision]
                                      -> delegate disabled, root retained
   :replace-delegation! [expected-policy-revision new-root-signed-config]
                                      -> fresh delegate under SAME root

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
        state (atom {:policy-id (str (UUID/randomUUID))
                     :policy-revision 1
                     :root-generation 1
                     :delegation-generation 1
                     :lifecycle :active
                     :delegation-lifecycle :active
                     :delegation-id (get-in initial-config [:delegation :delegation-id])
                     :retired-delegation-ids #{}
                     :retired-delegate-fingerprints #{}
                     :active-verifier initial
                     :root-fingerprint first-root
                     :delegate-fingerprint (:delegated-key-spki-fingerprint initial)
                     :previous-root-fingerprints #{first-root}})
        summary (fn []
                  (let [{:keys [policy-revision root-generation lifecycle
                                delegation-generation delegation-lifecycle
                                retired-delegation-ids retired-delegate-fingerprints
                                root-fingerprint delegate-fingerprint]} @state]
                    {:type lifecycle-type
                     :version 1
                     :policy-revision policy-revision
                     :root-generation root-generation
                     :delegation-generation delegation-generation
                     :lifecycle lifecycle
                     :delegation-lifecycle delegation-lifecycle
                     :retired-delegation-count (count retired-delegation-ids)
                     :retired-delegate-key-count (count retired-delegate-fingerprints)
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
        checkpoint-material (fn []
                              (let [st @state]
                                ;; Portable content, not an authenticated log,
                                ;; secret key, or independently verified source.
                                {:type :gesso.assurance.proof.xtdb-gateway-local-key-lifecycle/checkpoint-material
                                 :version 1
                                 :policy-id (:policy-id st)
                                 :policy-revision (:policy-revision st)
                                 :root-generation (:root-generation st)
                                 :delegation-generation (:delegation-generation st)
                                 :root-lifecycle (:lifecycle st)
                                 :delegation-lifecycle (:delegation-lifecycle st)
                                 :root-fingerprint (:root-fingerprint st)
                                 :delegate-fingerprint (:delegate-fingerprint st)
                                 :delegation-id (:delegation-id st)
                                 :seen-root-fingerprints (vec (sort (:previous-root-fingerprints st)))
                                 :retired-delegate-fingerprints (vec (sort (:retired-delegate-fingerprints st)))
                                 :retired-delegation-ids (vec (sort (:retired-delegation-ids st)))}))
        check-revision! (fn [expected]
                          (when-not (and (integer? expected)
                                         (= expected (:policy-revision @state)))
                            (reject! :stale-local-policy-revision)))
        issue! (fn [gateway original-return]
                 (locking monitor
                   (let [{:keys [lifecycle active-verifier policy-revision]} @state]
                     (when-not (= :active lifecycle)
                       (reject! :local-root-revoked))
                     (when-not (= :active (:delegation-lifecycle @state))
                       (reject! :local-delegation-revoked))
                     (when (>= (.size pending) max-pending)
                       (reject! :too-many-local-challenges))
                     (let [issued ((:issue! active-verifier) gateway original-return)]
                       (.put pending issued
                             {:revision policy-revision :verifier active-verifier})
                       issued))))
        inspect! (fn [issued response]
                   (locking monitor
                     (let [owned (.remove pending issued)
                           issued-revision (:revision owned)
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
                                :local-delegation-generation
                                (:delegation-generation @state)
                                :local-delegation-revocation
                                :active-at-local-inspection
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
                    (drain-pending! pending)
                    (swap! state (fn [s]
                                   (-> s
                                       (update :policy-revision inc)
                                       (update :retired-delegation-ids
                                               conj (:delegation-id s))
                                       (update :retired-delegate-fingerprints
                                               conj (:delegate-fingerprint s))
                                       (assoc :lifecycle :revoked
                                              :delegation-lifecycle :revoked
                                              :active-verifier nil))))
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
                      (when (>= (:delegation-generation @state) max-delegations)
                        (reject! :local-delegation-generation-limit))
                      (when (or (= (:delegation-id @state)
                                   (get-in new-config [:delegation :delegation-id]))
                                (contains? (:retired-delegation-ids @state)
                                           (get-in new-config [:delegation :delegation-id])))
                        (reject! :previously-retired-delegation-not-reusable))
                      (when (or (= (:delegate-fingerprint @state)
                                   (:delegated-key-spki-fingerprint new-verifier))
                                (contains? (:retired-delegate-fingerprints @state)
                                           (:delegated-key-spki-fingerprint new-verifier)))
                        (reject! :previously-retired-delegate-key-not-reusable))
                      (drain-pending! pending)
                      (swap! state
                             (fn [s]
                               (-> s
                                   (update :policy-revision inc)
                                   (update :root-generation inc)
                                   (update :delegation-generation inc)
                                   (update :retired-delegation-ids
                                           conj (:delegation-id s))
                                   (update :retired-delegate-fingerprints
                                           conj (:delegate-fingerprint s))
                                   (assoc :lifecycle :active
                                          :delegation-lifecycle :active
                                          :delegation-id
                                          (get-in new-config [:delegation :delegation-id])
                                          :active-verifier new-verifier
                                          :root-fingerprint new-root
                                          :delegate-fingerprint
                                          (:delegated-key-spki-fingerprint new-verifier))
                                   (update :previous-root-fingerprints conj new-root))))
                      (summary))))
        revoke-delegation! (fn [expected]
                             (locking monitor
                               (check-revision! expected)
                               (when-not (= :active (:lifecycle @state))
                                 (reject! :local-root-revoked))
                               (when-not (= :active (:delegation-lifecycle @state))
                                 (reject! :local-delegation-already-revoked))
                               (drain-pending! pending)
                               (swap! state
                                      (fn [s]
                                        (-> s
                                            (update :policy-revision inc)
                                            (update :retired-delegation-ids
                                                    conj (:delegation-id s))
                                            (update :retired-delegate-fingerprints
                                                    conj (:delegate-fingerprint s))
                                            (assoc :delegation-lifecycle :revoked
                                                   :active-verifier nil))))
                               (summary)))
        replace-delegation! (fn [expected new-config]
                              (locking monitor (check-revision! expected))
                              (let [new-verifier (prepare-delegation! new-config)
                                    new-root (:configured-root-spki-fingerprint
                                              new-verifier)
                                    new-delegate (:delegated-key-spki-fingerprint
                                                  new-verifier)
                                    new-id (get-in new-config
                                                   [:delegation :delegation-id])]
                                (locking monitor
                                  (check-revision! expected)
                                  (when-not (= :active (:lifecycle @state))
                                    (reject! :local-root-revoked))
                                  (when (>= (:delegation-generation @state) max-delegations)
                                    (reject! :local-delegation-generation-limit))
                                  (when-not (= (:root-fingerprint @state) new-root)
                                    (reject! :replacement-must-use-current-root))
                                  (when (or (= new-id (:delegation-id @state))
                                            (contains? (:retired-delegation-ids @state)
                                                       new-id))
                                    (reject! :previously-retired-delegation-not-reusable))
                                  (when (or (= new-delegate (:delegate-fingerprint @state))
                                            (contains? (:retired-delegate-fingerprints @state)
                                                       new-delegate))
                                    (reject! :previously-retired-delegate-key-not-reusable))
                                  (drain-pending! pending)
                                  (swap! state
                                         (fn [s]
                                           (-> s
                                               (update :policy-revision inc)
                                               (update :delegation-generation inc)
                                               (update :retired-delegation-ids
                                                       conj (:delegation-id s))
                                               (update :retired-delegate-fingerprints
                                                       conj (:delegate-fingerprint s))
                                               (assoc :delegation-lifecycle :active
                                                      :active-verifier new-verifier
                                                      :delegation-id new-id
                                                      :delegate-fingerprint new-delegate))))
                                  (summary))))]
    {:type lifecycle-type
     :version 1
     :status! (fn [] (locking monitor (summary)))
     :checkpoint-material! (fn [] (locking monitor (checkpoint-material)))
     :issue! issue!
     :inspect! inspect!
     :revoke! revoke!
     :rotate! rotate!
     :revoke-delegation! revoke-delegation!
     :replace-delegation! replace-delegation!
     :root-provisioning-independence :not-established
     :cross-process-revocation :not-established
     :durable-revocation-history :not-established
     :witness-admission :not-admitted
     :proof-discharge :none
     :certificate :not-issued
     :transaction-authority :none
     :retry-authority :none
     :authority :none}))

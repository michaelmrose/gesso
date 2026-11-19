(ns gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint-test
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.assurance.proof.xtdb-gateway-signed-policy-checkpoint :as signed]
            [gesso.assurance.proof.xtdb-gateway-local-key-lifecycle :as lifecycle]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test]
            [xtdb.api :as xt])
  (:import [java.security KeyPairGenerator Signature]
           [java.time Instant]
           [java.util Base64 UUID]))

(defn- pair [] (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519")))
(defn- spki [kp] (.encodeToString (Base64/getEncoder) (.getEncoded (.getPublic kp))))
(defn- sign [^bytes bytes kp]
  (let [s (Signature/getInstance "Ed25519")]
    (.initSign s (.getPrivate kp))
    (.update s bytes)
    (.encodeToString (Base64/getEncoder) (.sign s))))

(defn- grant [delegate]
  (let [now (Instant/now)]
    {:version 1 :delegation-id (str (UUID/randomUUID))
     :audience "gesso.xtdb.transaction-causality-candidate.v1"
     :obligation :exclusive-transaction-causality
     :witness-family :transaction-coupled-apply-provenance
     :delegated-ed25519-spki-base64 (spki delegate)
     :valid-from (str (.minusSeconds now 60))
     :valid-until (str (.plusSeconds now 3600))}))

(defn- delegation [root delegate]
  (let [g (grant delegate)
        bytes ((requiring-resolve
                'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)
               g)]
    {:root-ed25519-spki-base64 (spki root)
     :delegation g
     :root-signature-base64 (sign bytes root)}))

(defn- local-policy []
  (let [root (pair)
        a (pair)
        b (pair)]
    {:root root :a a :b b
     :policy (lifecycle/prepare-process-local-root-policy!
              (delegation root a))
     :replacement (delegation root b)}))

(defn- verifier [policy signer & {:keys [revision previous]
                                  :or {revision 1 previous "genesis"}}]
  (signed/prepare-checkpoint-candidate-verifier!
   {:policy policy
    :authority-ed25519-spki-base64 (spki signer)
    :next-revision revision
    :expected-previous-digest previous}))

(defn- checkpoint [policy previous]
  {:type signed/checkpoint-type
   :version 1
   :previous-digest previous
   :snapshot ((:checkpoint-material! policy))})

(defn- envelope [checkpoint signer]
  {:checkpoint checkpoint
   :signature-base64 (sign (signed/checkpoint-signing-bytes checkpoint)
                           signer)})

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(deftest v1901-actual-revocation-and-delegation-replacement-link-signed-checkpoints
  (let [{:keys [policy replacement]} (local-policy)
        authority (pair)
        v (verifier policy authority)
        c1 (checkpoint policy "genesis")
        e1 (envelope c1 authority)
        first-check ((:accept! v) e1)]
    (is (= signed/report-type (:type first-check)))
    (is (= 1 (:checked-policy-revision first-check)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:checkpoint-digest first-check)))
    (is (= :valid-for-process-configured-key (:signature-status first-check)))
    (is (= :at-time-of-inspection (:checkpoint-local-match first-check)))
    (is (= :contiguous-from-caller-provided-anchor (:chain-continuity first-check)))
    (is (= :not-established
           (:past-anchor-integrity first-check)
           (:signer-independent-provisioning first-check)
           (:durable-storage first-check)
           (:cross-process-revocation first-check)
           (:physical-transaction-causality first-check)))
    (is (= :none (:authority first-check) (:proof-discharge first-check)))
    (is (= :not-issued (:certificate first-check)))
    (is (not-any? #(contains? first-check %)
                  [:snapshot :retired-delegation-ids :retired-delegate-fingerprints
                   :authority-public-key :secret-key :root-key :policy]))
    (is (= :checkpoint-rollback-gap-or-wrong-anchor
           (reason #((:accept! v) e1)))
        "An already accepted signed checkpoint is a replay")
    (let [revoked ((:revoke-delegation! policy) 1)
          c2 (checkpoint policy (:checkpoint-digest first-check))
          e2 (envelope c2 authority)
          second-check ((:accept! v) e2)]
      (is (= :revoked (:delegation-lifecycle revoked)))
      (is (= 2 (:checked-policy-revision second-check)))
      (is (= 1 (count (get-in c2 [:snapshot :retired-delegation-ids]))))
      (is (= 1 (count (get-in c2 [:snapshot :retired-delegate-fingerprints]))))
      (is (= 3 (:next-revision ((:status! v)))))
      (is (= (:checkpoint-digest second-check)
             (:expected-previous-digest ((:status! v)))))
      (let [new-policy ((:replace-delegation! policy) 2 replacement)
            c3 (checkpoint policy (:checkpoint-digest second-check))
            third-check ((:accept! v) (envelope c3 authority))]
        (is (= 3 (:policy-revision new-policy)))
        (is (= :active (:delegation-lifecycle new-policy)))
        (is (= 3 (:checked-policy-revision third-check)))
        (is (= 2 (get-in c3 [:snapshot :delegation-generation])))
        (is (= (get-in c2 [:snapshot :retired-delegation-ids])
               (get-in c3 [:snapshot :retired-delegation-ids])))
        (is (= :none (:transaction-authority third-check)))))))

(deftest v1901-rejects-unsigned-fabricated-stale-and-altered-checkpoints
  (let [{:keys [policy]} (local-policy)
        trusted (pair)
        other (pair)
        v (verifier policy trusted)
        correct (checkpoint policy "genesis")
        signed (envelope correct trusted)
        alternate-digest (str "sha256:" (apply str (repeat 64 "0")))
        fail (fn [record] (reason #((:accept! v) record)))]
    (is (= :checkpoint-signature-not-verified
           (fail (envelope correct other))))
    (is (= :checkpoint-signature-not-verified
           (fail (assoc signed :signature-base64
                        (:signature-base64 (envelope
                          (assoc correct :previous-digest alternate-digest)
                          trusted))))))
    (is (= :invalid-signed-checkpoint
           (fail (assoc signed :claim-approved true))))
    (is (= :invalid-checkpoint-envelope
           (fail (assoc-in signed [:checkpoint :authority] :certified))))
    (is (= :malformed-or-inconsistent-checkpoint-snapshot
           (fail (assoc-in signed [:checkpoint :snapshot
                                   :retired-delegation-ids]
                           ["forged-non-uuid"]))))
    (is (= :checkpoint-rollback-gap-or-wrong-anchor
           (fail (envelope (assoc correct :previous-digest alternate-digest)
                           trusted))))
    (is (= :malformed-or-inconsistent-checkpoint-snapshot
           (fail (assoc signed :checkpoint
                        (assoc-in correct [:snapshot :root-lifecycle]
                                  :revoked)))))
    (is (= 1 (:checked-policy-revision ((:accept! v) signed))))
    (is (= :checkpoint-rollback-gap-or-wrong-anchor (fail signed)))
    (is (= :not-established (:durable-storage ((:status! v)))))
    (is (= :caller-supplied-not-independently-verified
           (:anchor-provenance ((:status! v)))))))

(deftest v1901-checkpoint-acceptance-has-an-explicit-externally-supplied-anchor
  (let [{:keys [policy]} (local-policy)
        signer (pair)
        v (verifier policy signer)
        one ((:accept! v) (envelope (checkpoint policy "genesis") signer))
        digest (:checkpoint-digest one)
        restarted (verifier policy signer :revision 1 :previous "genesis")
        pinned (verifier policy signer :revision 2 :previous digest)]
    (is (= :checkpoint-rollback-gap-or-wrong-anchor
           (reason #((:accept! pinned)
                     (envelope (checkpoint policy "genesis") signer)))))
    (is (= :checkpoint-rollback-gap-or-wrong-anchor
           (reason #((:accept! v)
                     (envelope (checkpoint policy "genesis") signer)))))
    ;; The verifier itself cannot tell a freshly created instance to use
    ;; the latest anchor; a caller is also allowed to SUPPLY an old anchor.
    ;; This is deliberately NOT a claim of rollback protection on restart.
    (is (= 1 (:checked-policy-revision
              ((:accept! restarted)
               (envelope (checkpoint policy "genesis") signer)))))
    (is (= :caller-supplied-not-independently-verified
           (:anchor-provenance ((:status! restarted)))))
    (is (= 2 (:next-revision ((:status! pinned)))))
    (is (= :invalid-checkpoint-verifier-configuration
           (reason #(verifier policy signer :revision 2 :previous "genesis"))))))

(deftest v1901-genuine-xtdb-handshake-does-not-confer-policy-authority
  (physical/with-proving-application!
   (fn [app]
     (let [{:keys [policy a]} (local-policy)
           signer (pair)
           v (verifier policy signer)
           fixture 'gesso.assurance.proof.xtdb-restricted-submission-gateway-test
           source-for (var-get (ns-resolve fixture 'source-for))
           before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source-for [before]))
           returned ((:submit! g))
           physical-report (intent/inspect-pinned-intent! g returned)
           challenge ((:issue! policy) g returned)
           raw (.decode (Base64/getDecoder)
                        ^String (:signing-bytes-base64 challenge))
           checked ((:inspect! policy) challenge
                    {:signature-base64 (sign raw a)})
           report ((:accept! v) (envelope (checkpoint policy "genesis") signer))]
       (is (= :bounded-private-before-after-intent-ledger-coobservation
              (:protected-before-after-intent-ledger-correlation physical-report)))
       (is (= 7 (get-in physical-report
                       [:correspondence-premises :assurance-frontier :open-count])))
       (is (= :not-established (:physical-event-correspondence checked)
              (:independent-root-provisioning checked)
              (:physical-transaction-causality report)))
       (is (= :not-admitted (:witness-admission checked)
              (:witness-admission report)))
       (is (= :none (:proof-discharge checked) (:proof-discharge report)
              (:authority checked) (:authority report)))
       (is (= :not-issued (:certificate checked) (:certificate report)))))))

(deftest v1901-fail-closed-on-skipped-revision-and-erased-retirement-sets
  (let [{:keys [policy replacement]} (local-policy)
        signer (pair)
        v (verifier policy signer)
        one ((:accept! v) (envelope (checkpoint policy "genesis") signer))]
    ;; Deliberately skip revision 2; the verifier cannot skip a policy
    ;; transition and silently claim continuous signed history.
    ((:revoke-delegation! policy) 1)
    ((:replace-delegation! policy) 2 replacement)
    (let [at-three (checkpoint policy (:checkpoint-digest one))]
      (is (= :checkpoint-rollback-gap-or-wrong-anchor
             (reason #((:accept! v) (envelope at-three signer)))))
      (let [missing-retired (assoc-in at-three [:snapshot :retired-delegation-ids] [])]
        ;; The payload is structurally plausible but changes the real
        ;; source record; its signature cannot make the forged record true.
        (is (= :checkpoint-does-not-match-current-local-policy
               (reason #((:accept! (verifier policy signer
                                            :revision 3
                                            :previous (:checkpoint-digest one)))
                          (envelope missing-retired signer))))))
      (let [bad-typed (assoc-in at-three [:snapshot :root-fingerprint] 42)]
        (is (= :malformed-or-inconsistent-checkpoint-snapshot
               (reason #(signed/checkpoint-signing-bytes bad-typed)))))
      (is (= :checkpoint-rollback-gap-or-wrong-anchor
             (reason #((:accept! v) (envelope at-three signer))))))))

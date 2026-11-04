(ns gesso.assurance.proof.certificate
  "Source-relative, checker-backed proof admission (v1237).

   A certificate is an opaque, JVM-local handle, NOT a signed document, runtime
   authorization, universal invariant, or proof of a Clojure/XTDB/CLJS effect.
   This module accepts TWO DISTINCT theorem languages, without identifying
   them with one another:

   * :propositional       -> v1231 kernel/check-derivation! from original source
   * :guarded-transition  -> v1235 derivation/check-transition-derivation!

   Usage:
     (admit-certificate!
       {:proof-kind :guarded-transition
        :proof-source <original v1235 derivation source>
        :graph-handle <live v1217 dependency graph handle>
        :current-graph <fresh graph declaration with admitted subject handles>
        :expected-open-premises
          #{:gesso.assurance.proof.derivation/abstract-relation-match}
        :expected-open-obligations
          #{:production-transition-correspondence
            :current-runtime-authorization
            :atomic-transaction-enforcement
            :checker-meta-soundness}})

   The root of the dependency graph MUST be the EXACT declared proof subject
   and the only root. Each edge must match the admitted graph's current
   identity; all graph nodes must be reachable. This only closes the DECLARED
   structural dependency graph, not unknown code or installed-handler edges.

   The certificate's proved scope is no stronger than its source checker:
   transition conclusions hold for all states matching the declared abstract
   relation; this does NOT discharge the matching-relation premise. The
   proposer MUST enumerate all checker-reported premises and all remaining
   external obligations exactly. Nothing here lets a caller mark an opaque
   premise or an implementation boundary discharged. A valid certificate
   closes only local checking, certificate admission, and declared dependency
   accounting. Meta-soundness remains unproved.

   require-current-certificate! independently reruns the source proof checker
   AND current dependency check; it never trusts a copied report or fingerprint.
   The 'current' graph must be obtained from a trusted consumer's fresh
   construction. Passing an attacker-selected graph only certifies that
   attacker-selected abstraction. Neither a hash nor a handle is durable or
   transferable. Reflection/Var replacement in the same trusted JVM lies
   outside this module's adversary model."
  (:require [clojure.set :as set]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.derivation :as derivation]
            [gesso.assurance.proof.kernel :as kernel]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.subject :as subject])
  (:import (java.util WeakHashMap)))

(def certificate-version 1)
(def certificate-type :gesso.assurance.proof.certificate/in-process-checked-certificate)
(def ^:private certificate-id :gesso.assurance.proof/certificate-v1)
(def ^:private source-id :gesso.assurance.proof/certificate-source-v1)
(def ^:private dependency-id :gesso.assurance.proof/certificate-dependencies-v1)
(def ^:private input-keys
  #{:proof-kind :proof-source :graph-handle :current-graph
    :expected-open-premises :expected-open-obligations})

;; These are NOT dischargeable from an abstract derivation. New obligations
;; must be added here when a checker grows new claims or physical boundaries.
(def ^:private external-transition-obligations
  #{:production-transition-correspondence
    :current-runtime-authorization
    :atomic-transaction-enforcement
    :checker-meta-soundness})
(def ^:private external-proposition-obligations
  #{:implementation-correspondence
    :current-runtime-authorization
    :checker-meta-soundness})
(def ^:private expected-transition-missing-obligations
  (conj (disj external-transition-obligations :checker-meta-soundness)
        :certificate-admission))
(def ^:private abstract-relation-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)

(deftype CertificateHandle []
  Object
  (toString [_] "#<gesso.assurance/abstract-proof-certificate>"))

;; Unlike copyable EDN reports, a handle is an object with identity equality
;; registered only after all independent checks succeed. No public registry
;; injection or caller-supplied success callback exists.
(def ^:private ^WeakHashMap issued-certificates (WeakHashMap.))

(defn- fail!
  [reason at details]
  (throw (ex-info "Invalid Gesso checked abstract proof certificate."
                  (merge {:error/type ::invalid-certificate
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-keys! [value expected at]
  (when-not (plain-map? value)
    (fail! :expected-plain-map at {}))
  (when (some? (meta value))
    (fail! :metadata-not-allowed at {}))
  (let [actual (set (keys value))]
    (when-not (= expected actual)
      (fail! :wrong-keys at
             {:missing (set/difference expected actual)
              :unexpected (set/difference actual expected)}))))

(defn- exact-obligation-set! [provided required at]
  (when-not (and (instance? clojure.lang.PersistentHashSet provided)
                 (nil? (meta provided))
                 (= provided required))
    (fail! :obligation-closure-mismatch at
           {:missing (set/difference required (if (set? provided) provided #{}))
            :unexpected (set/difference (if (set? provided) provided #{})
                                        required)})))

(defn- source-fingerprint!
  [kind proof-source]
  ;; Canonical semantic hashing rejects executable payloads, unsupported Java
  ;; objects, and quoted/cached certificate flags anywhere in the interface.
  ;; The kernel/derivation independently validate the ORIGINAL source shape.
  (:fingerprint
   (subject/describe-subject
    {:kind :rule :id source-id
     :interface {:kind kind :source proof-source}})))

(defn- graph-fingerprint!
  [graph-description]
  (:fingerprint
   (subject/describe-subject
    {:kind :rule :id dependency-id
     :interface {:nodes (:nodes graph-description)
                 :roots (:roots graph-description)
                 :order (:dependency-order graph-description)}})))

(defn- check-proof!
  [kind proof-source]
  ;; Source rechecking is mandatory at issue time and every freshness check.
  ;; Never accept :checked?/:valid? or a previous report as a proof input.
  (case kind
    :propositional
    (let [report (kernel/check-derivation! proof-source)]
      (when-not (and (= kernel/report-type (:type report))
                     (= kernel/kernel-version (:version report))
                     (= :propositional-derivation-checked (:checking report))
                     (= :not-issued (:certificate report))
                     (= :not-admitted (:admission report))
                     (= :none (:authority report)))
        (fail! :unsafe-propositional-judgment [:proof-source] {}))
      {:report report
       :subject (:subject proof-source)
       :open-premises (:open-premises report)
       :open-obligations external-proposition-obligations
       :scope :typed-propositional-derivation-only
       :checker-version kernel/kernel-version})

    :guarded-transition
    (let [report (derivation/check-transition-derivation! proof-source)]
      (when-not (and (= derivation/report-type (:type report))
                     (= derivation/derivation-version (:checker-version report))
                     (= :abstract-transition-derivation-checked (:checking report))
                     (= :matching-abstract-transition-only (:claim-scope report))
                     (= :conditional-on-matching-abstract-relation
                        (:conclusion-status report))
                     (= #{abstract-relation-premise} (:open-premises report))
                     (= #{} (:trusted-assumptions report))
                     (= expected-transition-missing-obligations
                        (:missing-obligations report))
                     (= :not-issued (:certificate report))
                     (= :not-admitted (:admission report))
                     (= :none (:authority report)))
        (fail! :unsafe-transition-judgment [:proof-source] {}))
      {:report report
       :subject (get-in proof-source [:transition :subject])
       :open-premises (:open-premises report)
       :open-obligations external-transition-obligations
       :scope :matching-abstract-transition-only
       :checker-version derivation/derivation-version})

    (fail! :unsupported-proof-kind [:proof-kind] {:kind kind})))

(defn- checked-graph!
  [graph-handle current-graph raw-subject]
  (when-not (dependency/checked-graph? graph-handle)
    (fail! :unadmitted-dependency-graph [:graph-handle] {}))
  ;; This reruns v1217's complete graph check: every node's live subject
  ;; handle, roots, edges, cycles, unreachable nodes and stale fingerprints.
  (dependency/require-current-graph! graph-handle current-graph)
  (let [description (dependency/describe-graph graph-handle)
        subject-id (subject/describe-subject raw-subject)
        root (get-in subject-id [:subject :id])
        nodes (:nodes description)]
    (when-not (= #{root} (:roots description))
      (fail! :wrong-certificate-roots [:current-graph :roots]
             {:expected #{root} :actual (:roots description)}))
    (when-not (= (:fingerprint subject-id)
                 (get-in nodes [root :fingerprint]))
      (fail! :wrong-certificate-subject [:current-graph :nodes]
             {:expected (:fingerprint subject-id)
              :actual (get-in nodes [root :fingerprint])}))
    description))

(defn- check-package!
  [source]
  (exact-keys! source input-keys [])
  (let [{:keys [proof-kind proof-source graph-handle current-graph
                expected-open-premises expected-open-obligations]} source
        {:keys [report subject open-premises open-obligations scope checker-version]}
        (check-proof! proof-kind proof-source)
        graph (checked-graph! graph-handle current-graph subject)
        source-fp (source-fingerprint! proof-kind proof-source)
        graph-fp (graph-fingerprint! graph)]
    (exact-obligation-set! expected-open-premises open-premises
                           [:expected-open-premises])
    (exact-obligation-set! expected-open-obligations open-obligations
                           [:expected-open-obligations])
    (let [identity (subject/describe-subject
                    {:kind :rule :id certificate-id
                     :interface
                     {:certificate-version certificate-version
                      :proof-kind proof-kind
                      :checker-version checker-version
                      :proof-fingerprint (:fingerprint report)
                      :proof-source-fingerprint source-fp
                      :subject-fingerprint (:subject-fingerprint report)
                      :dependency-graph-fingerprint graph-fp
                      :scope scope
                      :open-premises open-premises
                      :open-obligations open-obligations}})]
      {:proof-kind proof-kind
       :scope scope
       :subject-fingerprint (:subject-fingerprint report)
       :derivation-fingerprint (:fingerprint report)
       :source-fingerprint source-fp
       :dependency-graph-fingerprint graph-fp
       :fingerprint (:fingerprint identity)
       :goal (:goal proof-source)
       :checker-version checker-version
       :open-premises open-premises
       :open-obligations open-obligations
       :graph-handle graph-handle})))

(defn- registered-entry
  [handle]
  (when (instance? CertificateHandle handle)
    (locking issued-certificates
      (.get issued-certificates handle))))

(defn checked-certificate?
  "True for an IN-PROCESS checker-issued abstract certificate, not a claim of
   executable correspondence, discharge of open assumptions, or authority."
  [handle]
  (boolean (registered-entry handle)))

(defn admit-certificate!
  "Issue a nontransferable handle ONLY after rechecking original proof source,
   matching the sole dependency root to the exact subject, validating a live
   current DAG, and requiring exact acknowledgement of open obligations.

   :proof-kind is either :propositional or :guarded-transition.
   :graph-handle is produced by dependency/check-graph!, never a copied map.
   :current-graph must be the consuming party's FRESH declaration; any claim
   of completeness is relative to that declared DAG, not arbitrary runtime code.

   This is LOCAL ABSTRACT certificate admission, never runtime authorization."
  [source]
  (let [entry (check-package! source)
        handle (CertificateHandle.)]
    (locking issued-certificates
      (.put issued-certificates handle entry))
    handle))

(defn describe-certificate
  "Copyable non-authoritative summary. This data cannot recreate a handle.
   The open premise(s) and external obligations remain OPEN after admission."
  [handle]
  (let [entry (registered-entry handle)]
    (when-not entry
      (fail! :unrecognized-certificate [:certificate] {}))
    (-> (dissoc entry :graph-handle)
        (assoc :type certificate-type
               :version certificate-version
               :certificate-scope :source-relative-abstract-proof-only
               :checking :original-source-independently-rechecked
               :proof-status (if (seq (:open-premises entry))
                               :checked-conditional-abstract-derivation
                               :checked-abstract-derivation)
               :closed-local-obligations
               #{:derivation-checked :declared-dependencies-checked
                 :certificate-admission}
               :assumption-discharge :not-performed
               :implementation-correspondence :not-established
               :runtime-enforcement :not-established
               :meta-soundness :not-established
               :admission :in-process-checker-backed
               :authority :none))))

(defn require-current-certificate!
  "Return the SAME handle only when the original checked proof and its
   dependency graph still match fresh source. The second argument is ORIGINAL
   proof source, never a report. The third is the CURRENT dependency graph
   declaration, not the earlier snapshot. Always re-run the proof checker.

   A successful check establishes the same SOURCE-RELATIVE abstract theorem.
   It says nothing about a deployed executor unless a later trusted integration
   authenticates the installed operation and discharges the relevant obligations."
  [handle current-proof-source current-graph]
  (let [prior (registered-entry handle)]
    (when-not prior
      (fail! :unrecognized-certificate [:certificate] {}))
    (let [source {:proof-kind (:proof-kind prior)
                  :proof-source current-proof-source
                  :graph-handle (:graph-handle prior)
                  :current-graph current-graph
                  :expected-open-premises (:open-premises prior)
                  :expected-open-obligations (:open-obligations prior)}
          now (check-package! source)]
      (when-not (= (dissoc now :graph-handle)
                   (dissoc prior :graph-handle))
        (fail! :stale-certificate [:certificate]
               {:expected (:fingerprint prior) :actual (:fingerprint now)})))
    handle))

(ns gesso.model.certified-operation
  "Checker-backed registration of ONE Gesso authoritative operation (v1249).

   This namespace ties a live v1237 ABSTRACT checked-derivation certificate to
   the exact qualified operation id and to a resolved-operation executor built
   HERE from the trusted application callback configuration. It does not accept
   an arbitrary supplied callable or copied executor metadata as registration.

   (build-certified-operation!
     {:operation :example/claim
      :certificate <live v1237 guarded-transition CertificateHandle>
      :proof-source <the ORIGINAL checked derivation source>
      :current-graph <the current v1217 dependency graph declaration>
      :resolved-configuration
      {:operation :example/claim
       :read-record! <trusted callback>
       :may-disclose? <trusted callback>
       :construct! <trusted pure callback>
       ;; optional resolved-operation coverage contracts ...
       }})

   The result is a callable [ctx {:binding ... :mode :submit|:reconcile}] that
   preserves resolved-operation's existing invocation and commit semantics.
   This is a REGISTRATION of a proof-linked executor, not an invariant theorem,
   an assertion that Clojure implements the abstract transition, an enforcement
   of the declared effect footprint, or runtime transaction authorization.

   Checked locally: original proof independently rechecked; current declared
   dependency graph; matching proof subject, interface operation name, and
   resolved configuration operation name; new executor built by Gesso's trusted
   resolved-operation constructor; object-identity registration of the wrapper.

   Still open: soundness of the abstract checker, source-to-deployed-handler
   completeness, callback authorization and purity, plan/effect correspondence,
   atomic XTDB enforcement, application invariant preservation, and all of the
   v1237 certificate's original open premises and obligations. The next
   revisions may check plans and assembled routes, but those will not be
   silently discharged by this adapter.

   Security boundary: in-process, cooperative trusted JVM. Reflection, private
   Var mutation, arbitrary code with the same JVM privileges, and mutable
   callback captures cannot be authenticated by an in-process registry. The
   current-configuration argument to require-current-operation! must come
   from the actual trusted assembly, not a caller's old cached description."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.subject :as subject]
            [gesso.model.resolved-operation :as resolved])
  (:import (java.util WeakHashMap)))

(def registration-version 1)
(def registration-type
  :gesso.model.certified-operation/in-process-proof-linked-operation)
(def report-type
  :gesso.model.certified-operation/copyable-proof-link-description)

(def ^:private input-keys
  #{:operation :certificate :proof-source :current-graph
    :resolved-configuration})
(def ^:private freshness-keys
  #{:current-proof-source :current-graph :current-configuration})
(def ^:private callback-keys
  #{:read-record! :may-disclose? :construct! :target-contract!
    :predicate-contract! :command-operation-contract!
    :post-state-contract! :outcome-contract!})
(def ^:private proof-obligations
  #{:deployed-handler-and-route-registration
    :current-callback-authorization-and-purity
    :authoritative-plan-and-effect-correspondence
    :source-to-production-executor-correspondence
    :physical-transaction-and-recovery-evidence
    :application-invariant-preservation})
(def ^:private approved-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)

;; A generated function's metadata is copyable. Only the EXACT function object
;; issued and placed into this private registry is a registered operation.
;; The registry value deliberately does not refer back to its key.
(def ^:private ^WeakHashMap registered-operations (WeakHashMap.))

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso proof-linked authoritative operation."
                  (merge {:error/type ::invalid-certified-operation
                          :reason reason :at at}
                         details))))

(defn- plain-map? [v]
  (or (instance? clojure.lang.PersistentArrayMap v)
      (instance? clojure.lang.PersistentHashMap v)))

(defn- exact-map! [v required at]
  (when-not (plain-map? v)
    (reject! :expected-plain-map at {}))
  (when (some? (meta v))
    (reject! :metadata-not-allowed at {}))
  (let [actual (set (keys v))]
    (when-not (= required actual)
      (reject! :wrong-keys at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual required)})))
  v)

(defn- qualified-operation? [v]
  (and (keyword? v) (some? (namespace v))))

(defn- subject-of! [operation proof-source]
  (let [raw (get-in proof-source [:transition :subject])]
    ;; The certificate itself also checks that this is original, valid
    ;; transition proof source, but the adapter requires an OPERATION subject
    ;; with an explicit operation id in its named interface.
    (when-not (and (plain-map? raw)
                   (= :operation (:kind raw))
                   (= operation (:id raw))
                   (= operation (get-in raw [:interface :operation])))
      (reject! :operation-subject-mismatch [:proof-source :transition :subject]
               {:operation operation}))
    (:fingerprint (subject/describe-subject raw))))

(defn- require-certificate!
  [operation certificate proof-source current-graph]
  (when-not (certificate/checked-certificate? certificate)
    (reject! :unadmitted-proof-certificate [:certificate] {}))
  ;; Never substitute a copyable report for a live handle or a cached :valid?.
  ;; This invokes v1237's ORIGINAL source checker and live DAG checker again.
  (certificate/require-current-certificate!
   certificate proof-source current-graph)
  (let [report (certificate/describe-certificate certificate)
        subject-fingerprint (subject-of! operation proof-source)]
    (when-not (and (= :guarded-transition (:proof-kind report))
                   (= :matching-abstract-transition-only (:scope report))
                   (= :source-relative-abstract-proof-only
                      (:certificate-scope report))
                   (= :original-source-independently-rechecked (:checking report))
                   (= :checked-conditional-abstract-derivation
                      (:proof-status report))
                   (= #{approved-premise} (:open-premises report))
                   (contains? (:open-obligations report)
                              :production-transition-correspondence)
                   (= subject-fingerprint (:subject-fingerprint report))
                   (= :none (:authority report))
                   (= :not-established (:implementation-correspondence report))
                   (= :not-established (:runtime-enforcement report)))
      (reject! :unsafe-or-mismatched-proof [:certificate]
               {:operation operation}))
    report))

(defn- require-configuration! [operation configuration]
  (when-not (plain-map? configuration)
    (reject! :expected-plain-map [:resolved-configuration] {}))
  (when (some? (meta configuration))
    (reject! :metadata-not-allowed [:resolved-configuration] {}))
  (when-not (= operation (:operation configuration))
    (reject! :executor-operation-mismatch [:resolved-configuration :operation]
             {:expected operation :actual (:operation configuration)}))
  configuration)

(defn- same-registration-configuration?
  [original current]
  ;; Callbacks are trusted only as the EXACT objects originally registered;
  ;; claiming equivalent code/metadata is not executable correspondence.
  (and (plain-map? current)
       (nil? (meta current))
       (= (set (keys original)) (set (keys current)))
       (every? (fn [[key value]]
                 (if (contains? callback-keys key)
                   (identical? value (get current key))
                   (= value (get current key))))
               original)))

(defn- registration-entry [registered]
  (when (fn? registered)
    (locking registered-operations
      (.get registered-operations registered))))

(defn registered-operation?
  "True only for the exact in-process callable issued by this adapter.
   This is not evidence of installed route adoption or physical correctness."
  [registered]
  (boolean (registration-entry registered)))

(defn- require-registration! [registered]
  (or (registration-entry registered)
      (reject! :unregistered-executor [:executor] {})))

(defn- description [entry]
  (let [proof (:certificate-description entry)]
    {:type report-type
     :version registration-version
     :registration registration-type
     :operation (:operation entry)
     :proof-fingerprint (:fingerprint proof)
     :proof-source-fingerprint (:source-fingerprint proof)
     :proof-subject-fingerprint (:subject-fingerprint proof)
     :dependency-graph-fingerprint (:dependency-graph-fingerprint proof)
     :checked #{:live-abstract-certificate-rechecked
                :exact-operation-subject-identity
                :resolved-executor-created-from-configuration
                :in-process-executor-identity-registered}
     :open-premises (:open-premises proof)
     :open-obligations (set/union (:open-obligations proof)
                                  proof-obligations)
     :proof-scope :matching-abstract-transition-only
     :registration-scope :in-process-proof-linked-executor-only
     :assumption-discharge :not-performed
     :invariant-preservation :not-proved
     :business-correctness :not-proved
     :implementation-correspondence :not-established
     :runtime-effect-enforcement :not-established
     :atomicity-assurance :not-established
     :deployed-route-adoption :not-established
     :certificate :source-relative-abstract-proof-only
     :authority :none}))

(defn describe-operation
  "Return a COPYABLE report for one registered callable. Never use this
   report in place of the registered function or original proof source."
  [registered]
  (description (require-registration! registered)))

(defn require-current-operation!
  "Check the ORIGINAL registered callable against FRESH proof, dependency,
   and trusted executor configuration. Return the same function on success.

   This detects a changed callback identity, contract configuration, source,
   or declared graph. It does NOT discover a separate application handler
   that has been replaced after this operation was registered. An assembled
   application checker must enumerate actual installed handlers independently."
  [registered current]
  (exact-map! current freshness-keys [:current])
  (let [{:keys [operation certificate configuration certificate-description]}
        (require-registration! registered)
        {:keys [current-proof-source current-graph current-configuration]} current]
    (when-not (same-registration-configuration? configuration
                                                 current-configuration)
      (reject! :stale-executor-configuration
               [:current :current-configuration] {}))
    (let [now (require-certificate! operation certificate
                                    current-proof-source current-graph)]
      (when-not (= certificate-description now)
        (reject! :stale-proof-registration [:current] {})))
    registered))

(defn build-certified-operation!
  "Build and register a trusted resolved-operation executor tied to an exact
   live guarded-transition certificate and the same semantic operation name.

   This local registration is a PRECONDITION for later stronger assembly gates.
   It never upgrades an abstract proof into authorization or certified XTDB
   execution. The caller is responsible for obtaining configuration and fresh
   graph from the trustworthy application's actual assembly."
  [source]
  (exact-map! source input-keys [])
  (let [{:keys [operation certificate proof-source current-graph
                resolved-configuration]} source]
    (when-not (qualified-operation? operation)
      (reject! :invalid-operation-id [:operation] {}))
    (require-configuration! operation resolved-configuration)
    (let [proof-report (require-certificate! operation certificate
                                              proof-source current-graph)
          ;; No arbitrary :executor argument is accepted. Resolved-operation
          ;; checks its own original closed configuration and constructs a new
          ;; callable with the canonical durable-command execution path.
          executor (resolved/build-operation resolved-configuration)]
      (when-not (and (fn? executor)
                     (= operation (:gesso.model/operation-id (meta executor)))
                     (= :durable-command-v1
                        (:gesso.model/resolution (meta executor))))
        (reject! :invalid-resolved-executor [:resolved-configuration] {}))
      (let [registered (with-meta
                         (fn proof-linked-operation [ctx input]
                           (executor ctx input))
                         (assoc (meta executor)
                                :gesso.model/proof-link-scope
                                :source-relative-abstract-proof-only))
            entry {:operation operation
                   :certificate certificate
                   :certificate-description proof-report
                   :configuration resolved-configuration
                   :executor executor}]
        (locking registered-operations
          (.put registered-operations registered entry))
        registered))))

(ns gesso.assurance.proof.xtdb-gateway-writer-coverage
  "v1549: bind host-OBSERVED writer registrations to Gesso-ISSUED one-shot
   monotonic XTDB gateway callbacks. This is an execution-capability identity
   check for the surveyed host table, not a closed-world writer proof.

   Each issued writer is a zero-argument callable that can submit ONLY its
   source-checked v1547 one-shot transaction. Callers register that EXACT
   callable in their actual host writer table. Inspection independently
   rederives v1489's operation/write-path inventory, freshly enumerates the
   actual host registrations twice, and checks exact callable identity,
   path, kind, operation ID, and source-derived operation coverage.

   A clean local report does not authenticate the host enumerator, prove that
   a deployed route invokes the registered callback, identify the production
   XTDB node, exclude unregistered/direct/external writers, or establish
   complete monotonic revision history. No inspected writer is invoked.
   The registered operation ID is a host label; this module does NOT prove
   that the prepared command implements that operation's abstract semantics.
   Handles and diagnostics are NOT certificates or transaction authority."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.physical-writer-inventory :as inventory]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic])
  (:import (java.util WeakHashMap)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-writer-coverage/registered-one-shots)
(def report-version 1)

(deftype WriterHandle []
  Object
  (toString [_] "#<gesso.assurance/issued-xtdb-writer>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-prepare monotonic/prepare-monotonic-revision-one-shot!)
(def ^:private original-inventory inventory/describe-physical-writer-inventory!)
(def ^:private max-writers 512)
(def ^:private open-obligations
  #{:host-enumerator-exhaustiveness-and-post-inspection-stability
    :deployed-route-invokes-the-registered-writer
    :other-processes-and-external-xtdb-clients
    :direct-and-unregistered-xtdb-write-bypasses
    :application-startup-and-initializer-write-coverage
    :xtdb-node-deployment-and-backend-identity
    :all-writers-preserve-monotonic-revision-history
    :host-operation-id-matches-prepared-command-semantics
    :transaction-internal-prestate-and-write-causality
    :physical-refinement-and-certification})

(defn- reject! [reason]
  ;; No writer functions, document data, operation IDs or paths in errors.
  (throw (ex-info "Invalid Gesso registered XTDB writer coverage."
                  {:xtdb-gateway-writer-coverage/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- seams! []
  ;; An ordinary Var-seam sanity check, not hostile-JVM attestation.
  (when-not (and (identical? original-prepare
                             (var-get #'monotonic/prepare-monotonic-revision-one-shot!))
                 (identical? original-inventory
                             (var-get #'inventory/describe-physical-writer-inventory!)))
    (reject! :substituted-issuer-or-inventory)))

(defn- issued-entry [handle]
  (when (instance? WriterHandle handle)
    (locking issued (.get issued handle))))

(defn issue-monotonic-registered-writer!
  "Issue a single locally owned, zero-argument host writer and opaque handle.

   [path-id operation-id node gateway-source revision-field]. Both IDs must
   be qualified keywords. `gateway-source` is the original one-command,
   one-protected-document source accepted by v1547. No operation, transaction
   or writer is executed at issuance; prestate acquisition DOES read XTDB.

   Register the returned :writer itself (not a delegating wrapper) in the
   host's actual writer registry. It can submit exactly once, and only the
   prepared source-bound transaction. The handle must not be serialized."
  [path-id operation-id node gateway-source revision-field]
  (seams!)
  (when-not (and (qualified-id? path-id) (qualified-id? operation-id))
    (reject! :invalid-path-or-operation-id))
  (let [gateway (original-prepare node gateway-source revision-field)]
    (seams!)
    (when-not (and (= monotonic/monotonic-type (:type gateway))
                   (= monotonic/monotonic-version (:version gateway))
                   (fn? (:submit! gateway))
                   (fn? (:summary! gateway))
                   (= :prepared (:status ((:summary! gateway)))))
      (reject! :unexpected-monotonic-gateway-contract))
    (let [submit! (:submit! gateway)
          handle (WriterHandle.)
          writer (fn [& args]
                   (when (seq args)
                     (reject! :registered-writer-requires-zero-arguments))
                   (submit!))]
      (locking issued
        (.put issued handle {:path-id path-id
                             :operation-id operation-id
                             :writer writer}))
      {:handle handle :writer writer})))

(defn- enumerate-twice! [source expected-paths]
  (let [enumerate! (:enumerate-write-paths! source)
        a (try (enumerate!) (catch Throwable _ (reject! :host-enumeration-failed)))
        b (try (enumerate!) (catch Throwable _ (reject! :host-enumeration-failed)))]
    (when-not (and (plain-map? a) (plain-map? b)
                   (= expected-paths (set (keys a)))
                   (= expected-paths (set (keys b)))
                   (every? (fn [path]
                             (let [x (get a path) y (get b path)]
                               (and (plain-map? x) (plain-map? y)
                                    (= #{:kind :operation-id :writer}
                                       (set (keys x)) (set (keys y)))
                                    (= (select-keys x [:kind :operation-id])
                                       (select-keys y [:kind :operation-id]))
                                    (identical? (:writer x) (:writer y)))))
                           expected-paths))
      (reject! :unstable-or-inconsistent-host-registrations))
    a))

(defn inspect-registered-writer-coverage!
  "Classify only the currently SURVEYED host writer paths.

   `inventory-source` is the genuine v1489 source including its live
   :enumerate-write-paths! function. `registered-handles` maps the actual
   qualified host path IDs to handles returned by this issuer. Neither
   enumerated callbacks nor gateway submission are invoked. Extra, missing,
   foreign, non-issued or substituted registered callbacks remain OPEN.

   Call require-registered-writer-coverage! to reject any open local result;
   even that strict result is not proof of host/deployment completeness."
  [inventory-source registered-handles]
  (seams!)
  (when-not (and (plain-map? registered-handles)
                 (<= (count registered-handles) max-writers)
                 (every? qualified-id? (keys registered-handles)))
    (reject! :invalid-issued-writer-handle-table))
  (let [upstream (original-inventory inventory-source)
        required (:observed-writer-path-ids upstream)
        paths (enumerate-twice! inventory-source required)
        declared (:independent-operation-ids upstream)
        known (set/intersection declared (:issued-operation-ids upstream))
        observed-ids (:observed-issued-operation-ids upstream)
        full-identity? (and (= declared (:issued-operation-ids upstream))
                            (= known observed-ids)
                            (:declared-to-observed-issued-ids-match? upstream)
                            (empty? (:observed-unclassified-entry-point-ids upstream)))
        matched (set (for [[path-id entry] paths
                           :let [local (issued-entry (get registered-handles path-id))]
                           :when (and local
                                      (= path-id (:path-id local))
                                      (= :issued-operation (:kind entry))
                                      (= (:operation-id local) (:operation-id entry))
                                      (contains? known (:operation-id local))
                                      (identical? (:writer local) (:writer entry)))]
                       path-id))
        nonissued (set (for [[path-id entry] paths
                             :when (not= :issued-operation (:kind entry))]
                         path-id))
        unbound (set/difference required matched)
        unregistered (set/difference (set (keys registered-handles)) required)
        local-clean? (and full-identity?
                          (= :conditional-observed-writer-path-id-join
                             (:status upstream))
                          (seq required)
                          (empty? nonissued)
                          (empty? unbound)
                          (empty? unregistered))]
    {:type report-type
     :version report-version
     :checking :rederived-operation-sources-and-fresh-host-callback-identities
     :scope :current-process-enumerated-registered-writers-only
     :status (if local-clean?
               :conditional-all-surveyed-writers-issued-monotonic-one-shots
               :open-unverified-or-unaccounted-writer-paths)
     :observed-writer-count (count required)
     :matched-issued-monotonic-writer-count (count matched)
     :unbound-or-foreign-writer-count (count unbound)
     :nonissued-writer-count (count nonissued)
     :issued-handle-not-registered-count (count unregistered)
     :independent-issued-operation-ids-match? (boolean full-identity?)
     :all-currently-surveyed-writers-locally-bound? (boolean local-clean?)
     :registered-callback-execution-policy
     :exact-local-zero-argument-one-shot-monotonic-gateway
     :host-enumerator-exhaustiveness :not-established
     :actual-handler-calls-registered-callback :not-established
     :all-xtdb-writers-mediated :not-established
     :writer-history-monotonicity :not-established
     :declared-operation-to-prepared-command-semantics :not-established
     :physical-refinement :not-established
     :open-obligations (set/union open-obligations
                                  (:open-obligations upstream))
     :proof-status :conditional-process-local-writer-registration-only
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

(defn require-registered-writer-coverage!
  "Fail closed unless every observed writer is an exactly matching locally
   issued monotonic one-shot. This checks the CURRENT enumerator output; it
   never locks the registry or claims complete deployed writer coverage."
  [inventory-source registered-handles]
  (let [report (inspect-registered-writer-coverage!
                inventory-source registered-handles)]
    (when-not (:all-currently-surveyed-writers-locally-bound? report)
      (reject! :unverified-or-unaccounted-registered-writers))
    report))

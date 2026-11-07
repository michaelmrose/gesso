(ns gesso.assurance.proof.xtdb-gateway-writer-coverage
  "v1553: bind host-OBSERVED writer registrations to Gesso-ISSUED one-shot
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
             :as monotonic]
            [gesso.assurance.proof.installed-route-provenance :as routes]
            [gesso.assurance.proof.checked-biff-host-adoption :as biff-host]
            [gesso.live.application-preflight :as application])
  (:import (java.util WeakHashMap)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-writer-coverage/registered-one-shots)
(def report-version 1)
(def route-observation-type
  :gesso.assurance.proof.xtdb-gateway-writer-coverage/route-invocation)
(def route-observation-version 1)
(def biff-observation-type
  :gesso.assurance.proof.xtdb-gateway-writer-coverage/biff-host-invocation)
(def biff-observation-version 1)

(deftype WriterHandle []
  Object
  (toString [_] "#<gesso.assurance/issued-xtdb-writer>"))

(def ^:private ^WeakHashMap issued (WeakHashMap.))
(def ^:private original-prepare monotonic/prepare-monotonic-revision-one-shot!)
(def ^:private original-inventory inventory/describe-physical-writer-inventory!)
(def ^:private original-route-observe routes/observe-checked-ring-route!)
(def ^:private original-route-recheck routes/require-current-route-dispatch!)
(def ^:private original-route-description routes/describe-checked-ring-router)
(def ^:private original-operation-summary application/operation-summary)
(def ^:private original-biff-adoption
  biff-host/require-current-biff-host-adoption!)
(def ^:dynamic ^:private *route-writer-observation* nil)
(def ^:dynamic ^:private *observing-route-writer* false)
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
    :route-writer-observation-is-local-and-post-dispatch
    :biff-host-module-start-not-deployed-http-server-proof
    :biff-handler-slot-not-immutable-after-observation
    :unchecked-direct-and-asynchronous-writes-during-route-dispatch
    :callback-result-not-xtdb-commit-authentication
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
  ;; Preserve v1549's original issuer/inventory boundary: unrelated Ring
  ;; function replacement must not affect writer issuance or host inspection.
  (when-not (and (identical? original-prepare
                             (var-get #'monotonic/prepare-monotonic-revision-one-shot!))
                 (identical? original-inventory
                             (var-get #'inventory/describe-physical-writer-inventory!)))
    (reject! :substituted-issuer-or-inventory)))

(defn- route-seams! []
  (seams!)
  (when-not (and (identical? original-route-observe
                             (var-get #'routes/observe-checked-ring-route!))
                 (identical? original-route-recheck
                             (var-get #'routes/require-current-route-dispatch!))
                 (identical? original-route-description
                             (var-get #'routes/describe-checked-ring-router))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-route-observation-boundary)))

(defn- biff-seams! []
  ;; Only the Biff-specific observer depends on this additional host
  ;; boundary. Existing issuance/inspection/Ring observation retain v1551
  ;; compatibility when host adoption is independently redefined.
  (route-seams!)
  (when-not (identical? original-biff-adoption
                        (var-get #'biff-host/require-current-biff-host-adoption!))
    (reject! :substituted-biff-host-adoption-boundary)))

(defn- issued-entry [handle]
  (when (instance? WriterHandle handle)
    (locking issued (.get issued handle))))

(defn- trace-invocation! [token phase]
  ;; The tracer stores ONLY private object identity, phase and thread agreement;
  ;; never a transaction result, protected row, request, or exception.
  (when *route-writer-observation*
    (swap! (:events *route-writer-observation*) conj
           {:token token
            :phase phase
            :same-thread? (identical? (Thread/currentThread)
                                      (:thread *route-writer-observation*))})))

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
          token (Object.)
          writer (fn [& args]
                   (when (seq args)
                     (reject! :registered-writer-requires-zero-arguments))
                   (trace-invocation! token :invoked)
                   (try
                     (let [result (submit!)]
                       (trace-invocation! token :returned)
                       result)
                     (catch Throwable t
                       (trace-invocation! token :threw)
                       (throw t))))]
      (locking issued
        (.put issued handle {:path-id path-id
                             :operation-id operation-id
                             :writer writer
                             :trace-token token}))
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


(defn observe-checked-route-registered-writer!
  "Invoke ONE actual locally checked Ring route, and check that during that
   synchronous dispatch it invoked the exact Gesso-issued, host-registered
   monotonic writer callback once and returned normally from that callback.

   [router route-source request inventory-source registered-handles path-id]
   The Ring route must be a *literal* method/path declaration (dynamic route
   matching is a separate, not-yet-admitted case). The operation declared for
   the route must equal that of the selected issued writer; neither caller-
   supplied route labels nor a copied writer report are sufficient.

   This is deliberately a POST-DISPATCH verification: a bypass or side effect
   may already have occurred when it rejects. The host enumerator and route
   installation are not attested as exhaustive or deployed. A callback RETURN
   is not an XTDB commit receipt, and an additional raw XTDB call remains
   invisible. A handler can also do unrelated work before or after the
   registered callback. The result provides no new transaction authority."
  [router route-source request inventory-source registered-handles path-id]
  (route-seams!)
  (when *observing-route-writer*
    (reject! :nested-route-writer-observation))
  (when-not (and (qualified-id? path-id)
                 (plain-map? request)
                 (keyword? (:request-method request))
                 (string? (:uri request))
                 (plain-map? route-source)
                 (plain-map? registered-handles))
    (reject! :invalid-route-writer-observation-source))
  (let [local (issued-entry (get registered-handles path-id))]
    (when-not (and local (= path-id (:path-id local))
                   (fn? (:writer local)))
      (reject! :unknown-or-foreign-route-writer))
    ;; This authenticates the checked in-process router and rederives all
    ;; certified route/handler identities before any handler is executed.
    (original-route-description router route-source)
    (let [summary (original-operation-summary
                   (:operation-acquisition-assembly
                    (:application-assembly route-source)))
          matches (vec (for [[operation entry] summary
                             route (:routes entry)
                             :when (= [(:request-method request) (:uri request)]
                                      [(:method route) (:path route)])]
                         {:operation operation :route-id (:route-id route)}))]
      (when-not (and (= 1 (count matches))
                     (= (:operation-id local) (:operation (first matches))))
        (reject! :route-not-literal-or-wrong-operation))
      ;; Recheck the actual host writer table independently of the route
      ;; source. No writer is called during this preflight.
      (require-registered-writer-coverage! inventory-source registered-handles)
      (let [observer {:thread (Thread/currentThread) :events (atom [])}
            observed (binding [*observing-route-writer* true
                               *route-writer-observation* observer]
                       (original-route-observe router route-source request))
            events @(:events observer)
            token (:trace-token local)
            expected [{:token token :phase :invoked :same-thread? true}
                      {:token token :phase :returned :same-thread? true}]]
        ;; Invalid evidence never becomes a success report. The prior route
        ;; dispatch may have performed writes and cannot be rolled back here.
        (when-not (= expected events)
          (reject! :missing-extra-or-foreign-writer-invocation))
        (when-not (= (:route-id (first matches))
                     (get-in observed [:report :observed-route-id]))
          (reject! :route-dispatch-identity-mismatch))
        (original-route-recheck (:witness observed) router route-source request)
        (require-registered-writer-coverage! inventory-source registered-handles)
        {:response (:response observed)
         :route-witness (:witness observed)
         :report
         {:type route-observation-type
          :version route-observation-version
          :status :observed-checked-ring-dispatch-invoked-exact-registered-writer
          :scope :one-synchronous-in-process-route-dispatch
          :observed-route-id (:route-id (first matches))
          :observed-operation-id (:operation (first matches))
          :observed-writer-path-id path-id
          :observed-registered-callback-invocations 1
          :registered-callback-returned? true
          :post-dispatch-route-and-host-registration-rechecked? true
          :host-deployment :not-established
          :other-xtdb-writer-bypasses :not-established
          :abstract-operation-correspondence :not-established
          :physical-commit-and-causality :not-established
          :open-obligations open-obligations
          :certificate :not-issued
          :admission :not-admitted
          :transaction-authority :none
          :authority :none}}))))


(defn observe-checked-biff-host-registered-writer!
  "Invoke the ACTUAL :biff.ring/handler installed by Gesso's one-shot
   checked Biff module, and observe its synchronous call to an exact issued
   and host-registered guarded writer.

   [adoption-handle contract-source system request inventory-source
    registered-handles path-id]

   Before dispatch, recheck the issued Biff module's completed start, exact
   handler-slot identity, source-checked certified literal route, and the
   host's fresh writer registrations. Invoke that Biff HANDLER (including
   its canonical Gesso response wrapper), not a separately installed Ring
   router. After dispatch, require exactly one same-thread call+normal return
   from the expected issued callback and recheck both host boundaries.

   A successful result establishes only this process-local handler SLOT and
   this observed invocation. It does NOT authenticate a deployed HTTP server,
   an exhaustive host registry, route-to-command semantic equivalence,
   absence of raw XTDB writes, or transaction commit. Rejection is
   POST-DISPATCH and cannot roll back any physical side effect."
  [adoption-handle contract-source system request
   inventory-source registered-handles path-id]
  (biff-seams!)
  (when *observing-route-writer*
    (reject! :nested-route-writer-observation))
  (when-not (and (qualified-id? path-id)
                 (plain-map? contract-source)
                 (plain-map? request)
                 (keyword? (:request-method request))
                 (string? (:uri request))
                 (plain-map? registered-handles))
    (reject! :invalid-biff-writer-observation-source))
  (let [local (issued-entry (get registered-handles path-id))]
    (when-not (and local (= path-id (:path-id local))
                   (fn? (:writer local)))
      (reject! :unknown-or-foreign-route-writer))
    ;; This is the original Biff module's local start-and-slot evidence, not
    ;; the identity of a caller-chosen router or a copied adoption report.
    (let [host-report (original-biff-adoption
                       adoption-handle contract-source system)
          summary (original-operation-summary
                   (:operation-acquisition-assembly
                    (:application-assembly contract-source)))
          matches (vec (for [[operation entry] summary
                             route (:routes entry)
                             :when (= [(:request-method request) (:uri request)]
                                      [(:method route) (:path route)])]
                         {:operation operation :route-id (:route-id route)}))
          selected (first matches)
          handler (get system application/biff-ring-handler-key)]
      (when-not (and (= 1 (count matches))
                     (= (:operation-id local) (:operation selected))
                     (contains? (:checked-certified-route-ids host-report)
                                (:route-id selected))
                     (fn? handler))
        (reject! :biff-route-not-literal-certified-or-wrong-operation))
      ;; No registered writer or checked Ring handler is invoked by preflight.
      (require-registered-writer-coverage! inventory-source registered-handles)
      (let [observer {:thread (Thread/currentThread) :events (atom [])}
            response (binding [*observing-route-writer* true
                               *route-writer-observation* observer]
                       (handler request))
            expected [{:token (:trace-token local)
                       :phase :invoked :same-thread? true}
                      {:token (:trace-token local)
                       :phase :returned :same-thread? true}]]
        (when-not (= expected @(:events observer))
          (reject! :missing-extra-or-foreign-writer-invocation))
        ;; A copied system map with this exact function can still pass; the
        ;; Biff module and system-map slot are process-local observations only.
        (original-biff-adoption adoption-handle contract-source system)
        (require-registered-writer-coverage! inventory-source registered-handles)
        {:response response
         :report
         {:type biff-observation-type
          :version biff-observation-version
          :status :observed-checked-biff-handler-invoked-exact-registered-writer
          :scope :one-synchronous-in-process-biff-handler-dispatch
          :observed-route-id (:route-id selected)
          :observed-operation-id (:operation selected)
          :observed-writer-path-id path-id
          :observed-registered-callback-invocations 1
          :registered-callback-returned? true
          :biff-module-start :locally-observed
          :biff-handler-slot :exact-checked-html-wrapped-handler
          :biff-handler-and-writer-registrations-rechecked? true
          :host-deployment :not-established
          :host-writer-enumeration-exhaustiveness :not-established
          :other-xtdb-writer-bypasses :not-established
          :abstract-operation-correspondence :not-established
          :physical-commit-and-causality :not-established
          :open-obligations (set/union open-obligations
                                       (:open-obligations host-report))
          :certificate :not-issued
          :admission :not-admitted
          :transaction-authority :none
          :authority :none}}))))

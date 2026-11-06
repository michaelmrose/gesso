(ns gesso.assurance.example.physical-single-node-proving
  "v1494: A runnable, generic SINGLE-NODE reservation application built with
   Gesso's model transaction boundary and a real embedded XTDB2 node.

   The existing single-node-proving catalog owns abstract business intent:
   :slot/reserve and :slot/release preserve status/claim-count invariants and
   frame the audit tag. This module supplies a deliberately small physical
   realization, a Biff-compatible context, resource management, and actual
   guarded XTDB writes. It does not issue certified executors or HTTP routes:
   those belong to subsequent host/application assembly work.

   IMPORTANT: These example callbacks are NOT certified operations. The
   abstract-to-physical mapping, authorization, installed writer closure,
   execution observation, and full physical refinement remain unproved.
   The seeded document is an application initializer and therefore a writer
   path that future completeness checks must account for. No hidden route,
   deployment identity, principal, or certificate is asserted here."
  (:require [gesso.assurance.example.single-node-proving :as pilot]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def table :gesso_v1494_reservation_slots)
(def operation-ids #{:slot/reserve :slot/release})
(def version-keys
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(def ^:private open-obligations
  #{:physical-value-mapping-not-independently-certified
    :model-transition-to-issued-executor-binding
    :installed-biff-handler-and-route-provenance
    :authorization-and-principal-provenance
    :complete-writer-coverage-including-bootstrap
    :xtdb-internal-prestate-and-unmodeled-fields
    :physical-transaction-outcome-and-poststate-provenance
    :browser-projection-and-settlement-correspondence
    :universal-refinement})

(defn- reject! [reason]
  (throw (ex-info "Invalid physical single-node proving application."
                  {:physical-single-node-proving/rejection true
                   :reason reason})))

(defn- require-slot-id! [slot-id]
  (when-not (and (string? slot-id) (<= 1 (count slot-id) 128)
                 (re-matches #"[A-Za-z0-9_-]+" slot-id))
    (reject! :unsupported-slot-id))
  slot-id)

(defn initial-document
  "A real XTDB document. Version metadata is separate from the three typed
   business fields. Persisted strings are translated to abstract enum values
   only by the unproved physical mapper below."
  [slot-id]
  {:xt/id (require-slot-id! slot-id)
   :revision 0
   :created-at "v1494-created"
   :updated-at "v1494-created"
   :status "available"
   :claims 0
   :audit-tag "preserved"})

(defn physical->abstract
  "Example-only physical-to-abstract projection. Reject malformed states,
   do not silently interpret an unexpected database value as valid business
   state. Correctness of this translation is not independently proved."
  [row]
  (when-not (map? row)
    (reject! :missing-physical-slot))
  (let [status (case (:status row)
                 "available" :available
                 "reserved" :reserved
                 (reject! :unexpected-physical-status))
        claims (:claims row)
        tag (:audit-tag row)]
    (when-not (and (string? tag)
                   (re-matches #"[A-Za-z][A-Za-z0-9_-]{0,31}" tag)
                   (integer? claims)
                   (<= 0 claims 1)
                   (or (and (= status :available) (zero? claims))
                       (and (= status :reserved) (= claims 1)))
                   (<= (count tag) 32))
      (reject! :invalid-physical-slot))
    {:slot/status status
     :slot/active-claims claims
     :slot/audit-tag (keyword tag)}))

(defn- read-document! [node slot-id]
  (let [rows (xt/q node ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                         slot-id])]
    (when-not (= 1 (count rows))
      (reject! :missing-or-duplicate-slot))
    (first rows)))

(defn- checked-current! [node slot-id]
  (let [row (read-document! node slot-id)]
    (physical->abstract row)
    (when-not (and (= slot-id (:xt/id row))
                   (integer? (:revision row))
                   (<= 0 (:revision row))
                   (string? (:created-at row))
                   (string? (:updated-at row)))
      (reject! :invalid-physical-version))
    row))

(defn- transition-plan
  "Use the real Gesso update-command and atomic prewrite assertions. The
   application-owned guard enforces status and claim count at XTDB's write
   transaction boundary; the Gesso version precondition rejects stale reads."
  [before op]
  (let [[from to from-claims to-claims]
        (case op
          :slot/reserve ["available" "reserved" 0 1]
          :slot/release ["reserved" "available" 1 0]
          (reject! :unknown-operation))]
    (when-not (and (= from (:status before))
                   (= from-claims (:claims before)))
      (reject! :operation-precondition-not-met))
    (let [after (-> before
                    (assoc :status to :claims to-claims
                           :updated-at (str "v1494-revision-" (inc (:revision before))))
                    (update :revision inc))]
      {:commands [(command/update-command table op before after version-keys)]
       :assertions [(tx/assert-one table
                                    [:and [:= :xt/id (:xt/id before)]
                                     [:= :status from]
                                     [:= :claims from-claims]
                                     [:= :audit-tag (:audit-tag before)]])]
       :emit false})))

(defn- application-report [slot-id]
  {:type :gesso.assurance.example.physical-single-node-proving/real-xtdb-pilot
   :version 1
   :scope :one-embedded-xtdb-node-and-unissued-gesso-model-operations
   :catalog-id (:id (pilot/reservation-catalog))
   :operation-ids operation-ids
   :slot-id slot-id
   :physical-table table
   :declared-example-writer-paths #{:initializer/bootstrap :operation/reserve :operation/release}
   :observed-writer-completeness :not-established
   :installed-executor-binding :not-established
   :physical-refinement :not-established
   :open-obligations open-obligations
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})

(defn with-proving-application!
  "Run `f` with a small real embedded-XTDB application and close its node.

   A seed transaction is submitted via Gesso model.tx; it must not be hidden
   from writer accounting. Returned application has :read! (physical row),
   :abstract! (unproved but checked projection), and :execute! (one Gesso
   model transaction for :slot/reserve or :slot/release). The caller may
   inspect the returned :catalog, :biff-context, :biff-model-module, and
   :report. No live notification or HTTP route is installed here.

   Do not retain the callbacks or node after the supplied function returns.
   This fixture is for a single test/application invocation, not production
   lifecycle management or authenticated writer issuance."
  ([f] (with-proving-application! (str "v1494-" (random-uuid)) f))
  ([slot-id f]
   (require-slot-id! slot-id)
   (when-not (fn? f)
     (reject! :invalid-application-callback))
   (let [catalog (pilot/reservation-catalog)]
     (when-not (= operation-ids (:declared-operation-ids catalog))
       (reject! :changed-abstract-catalog))
     (with-open [node (xtn/start-node {})]
       (let [ctx {:biff.xtdb/node node}
             seed (initial-document slot-id)
             _ (tx/transact! ctx
                             {:commands [(command/create table seed version-keys)]
                              :emit false})
             read! (fn [] (checked-current! node slot-id))
             abstract! (fn [] (physical->abstract (read!)))
             execute! (fn [op]
                        (when-not (contains? operation-ids op)
                          (reject! :unknown-operation))
                        (let [before (read!)
                              plan (transition-plan before op)]
                          ;; One actual submission. A conflicting XTDB ASSERT
                          ;; propagates as an error: never reissue automatically.
                          (tx/transact! ctx plan)))
             app {:node node
                  :slot-id slot-id
                  :biff-context ctx
                  :biff-model-module (tx/module)
                  :catalog catalog
                  :report (application-report slot-id)
                  :read! read!
                  :abstract! abstract!
                  :execute! execute!}]
         (f app))))))

;; =============================================================================
;; v1501: adversarial two-actor reservation scenario
;; =============================================================================

(def competing-actor-ids #{:actor/one :actor/two})

(defn with-two-actor-reservation!
  "Construct two independent, one-shot :slot/reserve attempts against ONE real
   embedded XTDB document. Both Gesso model plans are prepared from the SAME
   observed prestate before either attempt can submit. The scenario never
   auto-retries a failed or uncertain transaction. A caller may submit the
   attempts sequentially or concurrently, but must join all work before this
   callback returns and the shared node closes.

   Actor IDs are diagnostic labels ONLY. The physical document has no owner ID
   and this example neither authenticates the actors nor establishes which
   actor is the rightful owner. The state invariant is at most one active
   claim, not authenticated owner uniqueness. Source/host/writer completeness,
   transaction-internal prestate, and universal refinement remain OPEN.

   Exposed :prepared-tx-ops are descriptive, not a certified operation source;
   :submit! always uses its own closed-over, originally prepared plan. Each
   actor is allowed one attempted submission, including if it throws. A
   rejected XTDB assertion propagates unchanged, without automatic retry."
  ([f]
   (with-two-actor-reservation! (str "v1501-" (random-uuid)) f))
  ([slot-id f]
   (when-not (fn? f)
     (reject! :invalid-scenario-callback))
   (with-proving-application!
     slot-id
     (fn [app]
       (let [before ((:read! app))
             original-state (physical->abstract before)
             _ (when-not (and (= :available (:slot/status original-state))
                              (zero? (:slot/active-claims original-state)))
                 (reject! :scenario-not-in-initial-state))
             ctx (:biff-context app)
             ;; Derive both plans using the SAME internal builder as :execute!
             ;; rather than duplicating the reservation guard or write shape.
             plans (into {}
                         (map (fn [actor]
                                [actor (transition-plan before :slot/reserve)]))
                         competing-actor-ids)
             prepared-ops (into {}
                                (map (fn [[actor plan]]
                                       [actor (:tx-ops (tx/prepare ctx plan))]))
                                plans)
             _ (when-not (apply = (vals prepared-ops))
                 (reject! :different-competing-transaction-plans))
             attempts (atom (zipmap competing-actor-ids
                                    (repeat :prepared)))
             submit! (fn [actor]
                       (when-not (contains? competing-actor-ids actor)
                         (reject! :unknown-competing-actor))
                       ;; swap-vals! atomically reserves this actor's single
                       ;; submission permission before calling the database.
                       (let [[previous _]
                             (swap-vals! attempts
                                         (fn [state]
                                           (if (= :prepared (get state actor))
                                             (assoc state actor :attempted)
                                             state)))]
                         (when-not (= :prepared (get previous actor))
                           (reject! :competing-actor-already-attempted))
                         (tx/transact! ctx (get plans actor))))
             report {:type :gesso.assurance.example.physical-single-node-proving/two-actor-scenario
                     :version 1
                     :operation-id :slot/reserve
                     :catalog-id (:id (:catalog app))
                     :actor-ids competing-actor-ids
                     :actor-identity :diagnostic-label-only
                     :observed-revision (:revision before)
                     :prepared-from-one-observed-state? true
                     :one-attempt-per-actor? true
                     :scope :one-embedded-xtdb-node-one-slot-two-prepared-plans
                     :complete-writer-coverage :not-established
                     :authorization :not-established
                     :transaction-internal-prestate :not-authenticated
                     :physical-refinement :not-established
                     :proof-status :not-proved
                     :certificate :not-issued
                     :admission :not-admitted
                     :authority :none}]
         (f {:actor-ids competing-actor-ids
             :submit! submit!
             :attempt-state! (fn [] @attempts)
             :prepared-tx-ops prepared-ops
             :before-abstract original-state
             :before-revision (:revision before)
             :read! (:read! app)
             :abstract! (:abstract! app)
             :report report}))))))

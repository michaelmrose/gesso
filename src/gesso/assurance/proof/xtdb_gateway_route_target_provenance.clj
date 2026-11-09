(ns gesso.assurance.proof.xtdb-gateway-route-target-provenance
  "v1578/v1582/v1584: source-linked request target resolution and optional
   sealed certified-plan target callback correspondence
   from a COMPLETE static route/physical-target declaration.

   Unlike a freely supplied request->expected-target callback, this resolver
   takes operation identity and literal route selection from the current
   certified application contract, and takes the physical document target
   from an immutable, independently declared startup table. The declaration
   of the *command operation* must also agree with the actual current
   certified-plan source bound to that operation by the certified contract.
   The candidate XTDB command is never an input to the resolver. Each lookup rechecks the
   certified contract and exact route index before returning an expectation.

   This is a deliberately NARROW first provenance step: literal routes with
   fixed document targets only. The application still owns the correctness
   and authorization of its static document-target declarations and the host
   request. Matching a certified source's command-operation declaration does
   not establish the independently intended document ID. This does not prove
   business semantics, deployed installation, XTDB causality, or exclusion of
   raw writers. The v1584 plan-bound path additionally checks that its
   immutable route targets are the SAME targets selected by the exact :target!
   callable installed in each current certified plan. The application still
   supplies that callback's table; this is correspondence, not a proof of
   the table's business truth. v1580's source-backed mediator requires the
   issued resolver; legacy/callback-based paths retain narrower guarantees."
  (:require [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified])
  (:import (java.util UUID WeakHashMap)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-route-target-provenance/issued-resolver)
(def report-version 1)

(def ^:private original-check certified/check-certified-contracts!)
(def ^:private original-summary application/operation-summary)
(def ^:private ^WeakHashMap issued (WeakHashMap.))
;; A plan's original :target! callback can be bound to an issued literal
;; route/target table BEFORE the certified plan is assembled. This separate
;; registry is identity-based: a wrapper or a function returning the same
;; target is not the originally issued plan callback.
(def ^:private ^WeakHashMap issued-plan-targets (WeakHashMap.))

(defn- reject! [reason]
  (throw (ex-info "Invalid source-linked XTDB route target provenance."
                  {:xtdb-gateway-route-target-provenance/rejection true
                   :reason reason})))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- plain-vector? [x]
  (and (instance? clojure.lang.APersistentVector x)
       (nil? (meta x))))

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- physical-target? [target]
  ;; Intentionally the same narrow physical-target vocabulary as v1574.
  (and (plain-vector? target)
       (= 2 (count target))
       (let [[table id] target]
         (and (keyword? table)
              (nil? (namespace table))
              (re-matches #"[A-Za-z_][A-Za-z0-9_]*" (name table))
              (or (and (string? id) (not (empty? id)) (<= (count id) 512))
                  (instance? UUID id)
                  (and (integer? id)
                       (not (instance? clojure.lang.BigInt id))
                       (not (instance? java.math.BigInteger id))))))))

(defn- pinned! []
  ;; JVM Var substitution guard; not a sandbox or bytecode attestation.
  (when-not (and (identical? original-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-summary
                             (var-get #'application/operation-summary)))
    (reject! :substituted-certification-or-route-checker)))

(defn- current-literal-route-index! [contract-source]
  (pinned!)
  (when-not (and (plain-map? contract-source)
                 (= #{:application-assembly :semantic-route-handlers
                      :certified-operations}
                    (set (keys contract-source))))
    (reject! :invalid-certified-route-source))
  ;; Recheck the real certified handlers/executors, not a caller-fabricated
  ;; report or a redundant list of claimed operation names.
  (let [verified (original-check contract-source)
        summary (original-summary
                 (get-in contract-source
                         [:application-assembly :operation-acquisition-assembly]))
        ;; Source of the command-operation identity is the CURRENT certified
        ;; plan binding already rechecked by original-check, never a second
        ;; application-authored route/target table. No candidate transaction
        ;; participates in this extraction.
        certified-sources (:certified-operations contract-source)
        rows (vec (for [[operation entry] summary
                        route (:routes entry)]
                    {:id (:route-id route)
                     :operation operation
                     :command-operation
                     (get-in certified-sources
                             [operation :current-source :command-operation])
                     :method (:method route)
                     :uri (:path route)}))
        ids (mapv :id rows)
        selectors (mapv (juxt :method :uri) rows)]
    (when-not (and (seq rows)
                   (<= (count rows) 512)
                   (= (count ids) (count (set ids)))
                   (= (count selectors) (count (set selectors)))
                   (= (set ids) (:route-ids verified))
                   (= (:route-ids verified)
                      (:matched-certified-route-ids verified))
                   (every? (fn [{:keys [id operation command-operation method uri]}]
                             (and (qualified-id? id)
                                  (qualified-id? operation)
                                  (keyword? command-operation)
                                  (not= :create command-operation)
                                  (keyword? method)
                                  (string? uri)
                                  (.startsWith ^String uri "/")
                                  (not (re-find #"[{}:*]" uri))))
                           rows))
      (reject! :requires-complete-unique-certified-literal-routes))
    (pinned!)
    (into {} (map (fn [row] [[(:method row) (:uri row)]
                             {:route-id (:id row)
                              :operation (:operation row)
                              :command-operation (:command-operation row)}])
                  rows))))

(defn- checked-static-target-declarations! [route-index declarations]
  (let [route-ids (set (map :route-id (vals route-index)))]
    (when-not (and (plain-map? declarations)
                   (= route-ids (set (keys declarations)))
                   (every? qualified-id? (keys declarations))
                   (every? (fn [entry]
                             (and (plain-map? entry)
                                  (= #{:command-operation :target}
                                     (set (keys entry)))
                                  (keyword? (:command-operation entry))
                                  (physical-target? (:target entry))))
                           (vals declarations)))
      (reject! :invalid-or-incomplete-static-route-target-declarations))
    ;; A well-formed startup declaration is not evidence of its own command
    ;; semantics. Join it to the independently issued/rechecked certified-plan
    ;; operation that owns the installed route. Every route is covered; a
    ;; second table cannot invent or override the canonical command operation.
    (when-not (every? (fn [[_ {:keys [route-id command-operation]}]]
                        (= command-operation
                           (get-in declarations [route-id :command-operation])))
                      route-index)
      (reject! :declared-command-operation-disagrees-with-certified-plan))
    ;; The resulting closed declaration cannot derive a new target from any
    ;; candidate command, user-supplied :params, or mutable host callback.
    declarations))

(defn- entry [resolver]
  (when (fn? resolver)
    (locking issued (.get issued resolver))))

(defn- plan-target-entry [target!]
  (when (fn? target!)
    (locking issued-plan-targets (.get issued-plan-targets target!))))

(defn issue-literal-route-plan-target!
  "Issue a callback suitable for a v1251 certified plan's :target! slot.

   The source is an immutable, application-declared route table:
     route-id -> {:method :post :uri <literal-path> :target [table id]}

   The resulting callback accepts [Ring-context binding] as expected by the
   certified plan and selects a fixed target using ONLY the Ring method and
   literal URI. It does not examine candidate commands, mutable request
   parameters, or the binding. Unknown routes fail closed. This issuance
   does not certify the truth or authorization of the declared document IDs.

   To obtain stronger *cross-layer correspondence*, install this EXACT
   callback as :target! when building the v1251 certified plan, then use
   issue-certified-plan-bound-literal-route-target-resolver! after assembling
   the application's checked route contract. This opt-in path cannot safely
   retrofit an arbitrary existing application-supplied :target! function."
  [declarations]
  (when-not (and (plain-map? declarations)
                 (seq declarations)
                 (<= (count declarations) 512)
                 (every? qualified-id? (keys declarations))
                 (every? (fn [row]
                           (and (plain-map? row)
                                (= #{:method :uri :target} (set (keys row)))
                                (keyword? (:method row))
                                (string? (:uri row))
                                (.startsWith ^String (:uri row) "/")
                                (not (re-find #"[{}:*]" (:uri row)))
                                (physical-target? (:target row))))
                         (vals declarations))
                 (= (count declarations)
                    (count (set (map (juxt :method :uri)
                                     (vals declarations))))))
    (reject! :invalid-or-ambiguous-literal-plan-target-declarations))
  (let [by-selector (into {}
                          (map (fn [[_ {:keys [method uri target]}]]
                                 [[method uri] target])
                               declarations))
        target! (fn [ctx _binding]
                  (when-not (plain-map? ctx)
                    (reject! :unmatched-issued-literal-plan-target))
                  (or (get by-selector [(:request-method ctx) (:uri ctx)])
                      (reject! :unmatched-issued-literal-plan-target)))]
    (locking issued-plan-targets
      (.put issued-plan-targets target! {:declarations declarations}))
    target!))

(defn- checked-plan-target-declarations!
  "Reconstruct target expectations from EXACT issued certified-plan :target!
   callables. Never infer a target from a candidate transaction."
  [contract-source route-index plan-targets]
  (let [operations (set (map :operation (vals route-index)))]
    (when-not (and (plain-map? plan-targets)
                   (= operations (set (keys plan-targets)))
                   (every? fn? (vals plan-targets)))
      (reject! :invalid-or-incomplete-issued-plan-targets))
    (reduce-kv
     (fn [acc operation target!]
       (let [sealed (or (plan-target-entry target!)
                        (reject! :unissued-certified-plan-target-callback))
             actual-target! (get-in contract-source
                                    [:certified-operations operation
                                     :current-source :target!])
             expected (into {}
                            (for [[[method uri] row] route-index
                                  :when (= operation (:operation row))]
                              [(:route-id row)
                               (assoc row :method method :uri uri)]))
             declarations (:declarations sealed)]
         (when-not (identical? target! actual-target!)
           (reject! :certified-plan-target-callback-identity-mismatch))
         (when-not (= (set (keys expected)) (set (keys declarations)))
           (reject! :issued-plan-target-route-coverage-mismatch))
         (reduce-kv
          (fn [result route-id {:keys [method uri target]}]
            (let [row (get expected route-id)]
              (when-not (and (= method (:method row))
                             (= uri (:uri row)))
                (reject! :issued-plan-target-selector-mismatch))
              (assoc result route-id
                     {:command-operation (:command-operation row)
                      :target target})))
          acc declarations)))
     {} plan-targets)))

(defn- issue-resolver! [contract-source routes declarations plan-targets]
  (let [declarations (checked-static-target-declarations! routes declarations)
        resolver (fn [request]
                   (pinned!)
                   (when-not (plain-map? request)
                     (reject! :unmatched-certified-literal-request))
                   (let [current (current-literal-route-index! contract-source)]
                     (when-not (= routes current)
                       (reject! :certified-route-index-changed))
                     (when plan-targets
                       (when-not (= declarations
                                    (checked-plan-target-declarations!
                                     contract-source current plan-targets))
                         (reject! :certified-plan-target-expectation-changed)))
                     (let [{:keys [route-id operation]}
                           (get current [(:request-method request)
                                         (:uri request)])]
                       (when-not route-id
                         (reject! :unmatched-certified-literal-request))
                       (let [{:keys [command-operation target]}
                             (get declarations route-id)]
                         {:operation-id operation
                          :command-operation command-operation
                          :target target}))))]
    (pinned!)
    (locking issued
      (.put issued resolver {:source contract-source
                             :routes routes
                             :declarations declarations
                             :plan-targets plan-targets}))
    {:expected-target! resolver}))

(defn issue-certified-literal-route-target-resolver!
  "Issue a process-local resolver from one CURRENT certified contract and a
   complete startup map of route ID -> {:command-operation :target}.
   The command operation in each row is cross-checked against the current,
   issued certified-plan source for that route's application operation.

   Physical target IDs are fixed per literal route. This does not support
   routes with per-request document identifiers; those need a separately
   specified and checked request-binding model. The target map must come from
   application construction, not the candidate XTDB source builder.

   Returns {:expected-target! callable}. The callable is registered by
   identity. Its argument is a Ring request; it selects only by exact method
   and URI and returns {:operation-id :command-operation :target}, suitable
   for v1574/v1576 structural comparison. No writer or certificate is issued.

   This is NOT an authentication of the startup declarations, request, user,
   physical document, or deployment; the caller must discharge those premises."
  [contract-source route-target-declarations]
  (let [routes (current-literal-route-index! contract-source)]
    (issue-resolver! contract-source routes route-target-declarations nil)))

(defn issue-certified-plan-bound-literal-route-target-resolver!
  "Opt-in stronger static target correspondence for certified literal routes.

   `plan-targets` is a COMPLETE map of application operation ID -> EXACT
   Gesso-issued :target! callable returned by issue-literal-route-plan-target!.
   Each callable MUST be the callback installed in that operation's current,
   reverified v1251 certified-plan source. Its closed route target table must
   cover exactly that operation's certified literal routes and selectors.

   Returns {:expected-target! issued-resolver}, compatible with the v1580
   source-backed Biff mediator. Each lookup rechecks the current certified
   source, callback identities, and sealed static targets. This establishes
   agreement of the route target with the installed certified PLAN callback,
   NOT independent truth of a business document ID or its authorization."
  [contract-source plan-targets]
  (let [routes (current-literal-route-index! contract-source)
        declarations (checked-plan-target-declarations!
                      contract-source routes plan-targets)]
    (issue-resolver! contract-source routes declarations plan-targets)))

(defn require-current-certified-literal-route-target-resolver!
  "Inspect an EXACT issued resolver against its original contract source.
   The returned redacted report is not a certificate, writer capability, or
   proof of the host's physical target declaration. The v1580 source-backed
   mediator uses this resolver; legacy and callback-based issuers do not."
  [resolver contract-source]
  (pinned!)
  (let [{:keys [source routes declarations plan-targets] :as issued-entry}
        (or (entry resolver) (reject! :unissued-target-resolver))]
    (when-not (and (identical? source contract-source)
                   issued-entry)
      (reject! :changed-target-resolver-source))
    (let [current (current-literal-route-index! contract-source)]
      (when-not (= routes current)
        (reject! :certified-route-index-changed))
      (checked-static-target-declarations! current declarations)
      (when plan-targets
        (when-not (= declarations
                     (checked-plan-target-declarations!
                      contract-source current plan-targets))
          (reject! :certified-plan-target-expectation-changed))))
    (pinned!)
    {:type report-type
     :version report-version
     :status :conditional-certified-literal-route-static-target-resolver
     :certified-route-count (count routes)
     :every-declared-route-certified? true
     :operation-identity :rederived-from-current-certified-contract
     :command-operation-identity :joined-to-current-certified-plan-source
     :target-identity (if plan-targets
                        :closed-issued-certified-plan-target-callback
                        :closed-startup-route-declaration)
     :certified-plan-target-callback-identity
     (if plan-targets :exact-issued-source-callback-checked :not-established)
     :candidate-command-used-to-derive-target? false
     :request-document-parameters-used? false
     :request-match :exact-ring-method-and-literal-uri
     :static-declaration-authenticity :application-premise-not-proved
     :physical-target-authorization :not-established
     :target-bound-biff-mediator-adoption :not-established
     :abstract-operation-to-command-refinement :not-established
     :physical-transaction-causality :not-established
     :open-obligations
     #{:startup-target-declarations-are-independently-correct-and-authorized
       :ring-method-and-uri-provenance
       :no-dynamic-document-target-binding-supported
       :target-bound-biff-mediator-must-require-issued-resolver
       :transaction-commit-causality-and-physical-refinement
       :external-writer-coverage-not-established}
     :certificate :not-issued
     :admission :not-admitted
     :transaction-authority :none
     :authority :none}))

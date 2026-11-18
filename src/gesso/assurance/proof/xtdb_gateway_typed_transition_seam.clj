(ns gesso.assurance.proof.xtdb-gateway-typed-transition-seam
  "v1755: Opt-in, one-document, pre-submission join from Gesso's original
   ABSTRACT typed guarded-assignment interpreter to the EXISTING real XTDB
   certified-plan, acquired-before, complete-top-level-CAS one-shot gateway.

   The caller supplies a transition declaration and input snapshot. Require
   the original and independently implemented abstract interpreters to agree
   that the COMPLETE, explicitly bijective before/after document projections in the
   candidate UPDATE satisfy the
   typed guard, simultaneous assignments and full declared frame. Require the
   selected route operation and canonical model command operation to match
   the declaration's source subject. These checks run BEFORE the physical
   gateway is prepared and AGAIN on the wrapper's sole submit attempt.

   The underlying existing gateway independently checks the current certified
   route target, acquires a matching external XTDB before, and submits a
   complete top-level CAS. It is the ONLY physical writer used here. The
   external prestate read is not transaction-internal; the transaction CAS
   rejects changed top-level fields but does not exclude ABA or raw writers.

   This is conditional physical gate COMPOSITION, NOT source authenticity,
   complete proof-premise coverage, business authorization, global writer
   completeness, a commit-causality theorem or formal-to-production transfer.
   v1756 binds caller-declared source-relative read invariant premises and
   reevaluates their values at preparation and submission, but does NOT
   authenticate the original invariant inventory or its kernel proof. A forged declaration can describe
   a different business policy. No certificate or application admission is
   issued. A submitted result is not automatically a committed result."
  (:require [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.transition :as constructor]
            [gesso.assurance.rule-eval :as rule-eval]
            [gesso.assurance.proof.transition :as transition]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate :as physical]
            [gesso.model.command :as command])
  (:import (java.util.concurrent.atomic AtomicBoolean)))

(def report-type :gesso.assurance.proof.xtdb-gateway-typed-transition-seam/one-shot)
(def report-version 1)

(def ^:private original-describe transition/describe-transition!)
(def ^:private original-observe transition/observe-transition!)
(def ^:private independent-observe independent/observe-transition!)
(def ^:private original-physical physical/prepare-plan-bound-acquired-one-shot!)
(def ^:private original-operation command/operation)
(def ^:private original-rule-eval rule-eval/evaluate-rule!)
(def ^:private original-catalog-check operation-set/check-operation-set!)
(def ^:private original-constructor constructor/build-guarded-replacement!)

(defn- reject! [reason]
  ;; Do not include documents, inputs, target IDs, plans, tokens or exception causes.
  (throw (ex-info "Unsupported typed transition / guarded XTDB join."
                  {:xtdb-gateway-typed-transition-seam/rejection true
                   :reason reason})))

(defn- seams! []
  ;; Reject accidental JVM var substitution, not hostile bytecode/redefinitions.
  (when-not (and (identical? original-describe (var-get #'transition/describe-transition!))
                 (identical? original-observe (var-get #'transition/observe-transition!))
                 (identical? independent-observe
                             (var-get #'independent/observe-transition!))
                 (identical? original-physical
                             (var-get #'physical/prepare-plan-bound-acquired-one-shot!))
                 (identical? original-operation (var-get #'command/operation))
                 (identical? original-rule-eval (var-get #'rule-eval/evaluate-rule!))
                 (identical? original-catalog-check (var-get #'operation-set/check-operation-set!))
                 (identical? original-constructor (var-get #'constructor/build-guarded-replacement!)))
    (reject! :substituted-source-or-physical-interpreter)))

(defn- plain-map? [m]
  (and (or (instance? clojure.lang.PersistentArrayMap m)
           (instance? clojure.lang.PersistentHashMap m))
       (nil? (meta m))))

(defn- source-relation!
  [selected-operation candidate-source declaration inputs field-map]
  (seams!)
  (when-not (and (keyword? selected-operation) (namespace selected-operation)
                 (plain-map? declaration)
                 (plain-map? inputs)
                 (plain-map? field-map)
                 (= selected-operation (get-in declaration [:subject :id]))
                 (plain-map? candidate-source)
                 (plain-map? (:plan candidate-source))
                 (= 1 (count (get-in candidate-source [:plan :commands])))
                 (false? (get-in candidate-source [:plan :emit])))
    (reject! :operation-or-candidate-source-mismatch))
  (let [cmd (first (get-in candidate-source [:plan :commands]))
        before (:model/before cmd)
        after (:model/after cmd)]
    (when-not (and (plain-map? before) (plain-map? after)
                   (keyword? (:model/operation cmd))
                   (= (:model/operation cmd)
                      (get-in declaration [:subject :interface :operation])))
      (reject! :canonical-command-or-source-interface-mismatch))
    (let [state-ids (set (keys (:state-types declaration)))
          physical-ids (set (keys before))
          mapped (vals field-map)
          _ (when-not (and (= state-ids (set (keys field-map)))
                           (= (count mapped) (count (set mapped)))
                           (= physical-ids (set mapped))
                           (= physical-ids (set (keys after)))
                           (every? keyword? mapped))
              (reject! :incomplete-or-nonbijective-physical-field-map))
          abstract-before (into {} (map (fn [[field physical-id]]
                                          [field (get before physical-id)])) field-map)
          abstract-after (into {} (map (fn [[field physical-id]]
                                         [field (get after physical-id)])) field-map)
          description (try (original-describe declaration)
                           (catch Exception _
                             (reject! :unsupported-abstract-declaration)))
          observed (try (original-observe declaration abstract-before inputs abstract-after)
                        (catch Exception _
                          (reject! :invalid-abstract-transition-values)))
          independent (try (independent-observe declaration abstract-before inputs abstract-after)
                           (catch Exception _
                             (reject! :invalid-independent-transition-values)))]
      (when-not (and (= transition/description-type (:type description))
                     (= transition/observation-type (:type observed))
                     (= independent/report-type (:type independent))
                     (= (:fingerprint description) (:transition-fingerprint observed))
                     (true? (:matches? observed))
                     (true? (:matches? independent))
                     (= (mapv #(get observed %) [:guard-holds? :assignments-hold? :frame-holds?])
                        (mapv #(get independent %) [:guard-holds? :assignments-hold? :frame-holds?]))
                     (= :none (:authority description) (:authority observed)
                        (:authority independent))
                     (= :not-admitted (:admission description) (:admission observed)
                        (:admission independent)))
        (reject! :original-and-independent-abstract-relation-not-established))
      (seams!)
      true)))

(defn- screened-read-premises!
  "Reconstruct every claimed read obligation from ALL declared invariant rules
   and the ORIGINAL guarded assignment relation. A copied caller inventory is
   never authority: it must match the derived pairs exactly. A :read target
   without a supplied invariant is unsupported here (even if the abstract
   transition alone is valid). Invariants themselves remain caller supplied;
   this check CANNOT establish that the application's source inventory is
   complete, authentic, or theorem-proved.

   Both prestate and poststate invariant predicates are checked as concrete
   values. Each read-value premise is checked directly on its original input
   or prestate source, not inferred from the target field's type. Re-run on
   each submission attempt; no check is a persistent prestate lock."
  [selected-operation candidate-source declaration inputs field-map proof-context]
  (seams!)
  (let [invariants (:invariants proof-context)
        claimed (:read-premises proof-context)
        state-types (:state-types declaration)
        input-types (:input-types declaration)
        assignments (:assignments declaration)
        cmd (first (get-in candidate-source [:plan :commands]))
        before (into {} (map (fn [[field phys]]
                               [field (get-in cmd [:model/before phys])]) field-map))
        after (into {} (map (fn [[field phys]]
                              [field (get-in cmd [:model/after phys])]) field-map))]
    (when-not (and (plain-map? proof-context)
                   (= #{:invariants :read-premises} (set (keys proof-context)))
                   (plain-map? invariants) (seq invariants)
                   (<= (count invariants) 32)
                   (vector? claimed) (seq claimed) (<= (count claimed) 128)
                   (every? plain-map? claimed))
      (reject! :invalid-or-absent-read-proof-context))
    (let [checked-invariants
          (into (sorted-map)
                (for [[id rule] invariants]
                  (let [field (first (:readers rule))
                        typ (get state-types field)]
                    (when-not (and (keyword? id) (namespace id)
                                   (plain-map? rule)
                                   (= #{:readers :expression} (set (keys rule)))
                                   (set? (:readers rule))
                                   (= 1 (count (:readers rule)))
                                   (contains? state-types field)
                                   (not (true? (:optional? typ)))
                                   (not (true? (:nullable? typ))))
                      (reject! :unsupported-read-invariant-declaration))
                    (try (original-rule-eval rule {field typ} {field (get before field)})
                         (catch Exception _
                           (reject! :unsupported-or-invalid-read-invariant)))
                    [id {:field field :rule rule :type typ}])))
          read-targets (into {}
                             (for [[target [kind reader]] assignments
                                   :when (= :read kind)]
                               [target reader]))
          expected (vec (for [[target reader] (sort-by key read-targets)
                              [inv-id {:keys [field]}] checked-invariants
                              :when (= target field)]
                          {:operation-id selected-operation
                           :invariant-id inv-id :field target
                           :reader reader
                           :reader-origin (if (contains? input-types reader)
                                            :input :prestate)}))]
      (when-not (and (seq expected)
                     (= (set (keys read-targets)) (set (map :field expected)))
                     (= (count expected) (count claimed))
                     (= (frequencies expected) (frequencies claimed))
                     (every? (fn [{:keys [field reader]}]
                               (= (get state-types field)
                                  (get (merge state-types input-types) reader)))
                             expected))
        (reject! :read-premise-inventory-not-source-derived))
      (letfn [(holds? [rule field typ value]
                (let [report (try (original-rule-eval rule {field typ} {field value})
                                  (catch Exception _
                                    (reject! :invalid-read-invariant-value)))]
                  (and (= rule-eval/evaluated-type (:type report))
                       (= :syntax-and-types (:checking report))
                       (= :pure-interpreted (:evaluation report))
                       (= :none (:authority report))
                       (true? (:value report)))))]
        (doseq [[_ {:keys [field rule type]}] checked-invariants]
          (when-not (holds? rule field type (get before field))
            (reject! :source-prestate-invariant-failed)))
        (doseq [{:keys [invariant-id field reader reader-origin]} expected
                :let [{:keys [rule type]} (get checked-invariants invariant-id)
                      source (if (= :input reader-origin) inputs before)]]
          (when-not (holds? rule field type (get source reader))
            (reject! :source-read-premise-failed)))
        (doseq [[_ {:keys [field rule type]}] checked-invariants]
          (when-not (holds? rule field type (get after field))
            (reject! :source-poststate-invariant-failed))))
      (seams!)
      true)))

;; v1773: Reuse the original typed semantic interpreters and read-premise
;; screening WITHOUT coupling them to the one-document physical gateway.
;; The full atomic typed preparer needs precisely this pure domain check while
;; retaining the separate durable ledger and every original transaction guard.
(def source-check-type
  :gesso.assurance.proof.xtdb-gateway-typed-transition-seam/domain-source-check)
(def source-check-version 1)

(defn check-source-relative-typed-domain!
  "Check the EXACT one-domain-command source against the original AND
   independent typed transition interpreters and original declared read
   premise values. Does not touch or prepare an XTDB writer. The declaration,
   read invariant inventory and document projection remain caller supplied,
   so the result is SOURCE RELATIVE, never business-policy authentication."
  [selected-operation domain-source declaration inputs field-map proof-context]
  (source-relation! selected-operation domain-source declaration inputs field-map)
  (screened-read-premises! selected-operation domain-source declaration inputs
                           field-map proof-context)
  (seams!)
  {:type source-check-type :version source-check-version
   :operation-id selected-operation
   :abstract-interpreters :original-and-independent-agree
   :original-read-premises :source-relative-values-screened
   :complete-domain-field-projection :bijective
   :source-policy-authenticity :not-established
   :physical-submission :disabled
   :certificate :not-issued :admission :not-admitted :authority :none})

(defn- prepare-typed-transition-one-shot!
  "Join the original abstract guarded transition to an actual plan-bound,
   externally acquired complete-CAS one-shot XTDB gateway. `declaration` and
   `inputs` are caller supplied and NOT authenticated as source-owned business
   meaning; nor are theorem read-premise inventories checked. Source before
   and after MUST be the full physical documents. `field-map` bijectively
   maps every qualified abstract state key to one physical document key;
   no unmodeled physical keys or aliases are permitted.

   Returns only {:type :version :submit! :summary!}, exposing no raw writer.
   A first submission attempt consumes the wrapper permit before rechecking
   current abstract/physical seams. Any intervening physical write is refused
   by the existing transaction-time full-top-level CAS. Neither a copied
   abstract report nor a preceding preflight can submit a transaction."
  [node contract-source resolver-issuance selected-operation request
   candidate-source declaration inputs field-map proof-context]
  (source-relation! selected-operation candidate-source declaration inputs field-map)
  (screened-read-premises! selected-operation candidate-source declaration inputs
                           field-map proof-context)
  (let [prepared (try (original-physical node contract-source resolver-issuance
                                        selected-operation request candidate-source)
                      (catch Exception _
                        (reject! :physical-gateway-preparation-rejected)))
        summary (try ((:summary! prepared))
                     (catch Exception _ (reject! :invalid-physical-gateway-summary)))]
    (when-not (and (= physical/report-type (:type prepared) (:type summary))
                   (= physical/report-version (:version prepared) (:version summary))
                   (= :prepared (:status summary))
                   (= :canonical-complete-top-level-cas (:transaction-preconditions summary))
                   (= :same-node-pinned-read-matched-at-preparation
                      (:external-before summary))
                   (fn? (:submit! prepared)) (fn? (:summary! prepared)))
      (reject! :unexpected-physical-gateway-contract))
    (source-relation! selected-operation candidate-source declaration inputs field-map)
    (screened-read-premises! selected-operation candidate-source declaration inputs
                             field-map proof-context)
    (let [attempted (AtomicBoolean. false)
          submit! (fn []
                    (when-not (.compareAndSet attempted false true)
                      (reject! :one-shot-already-attempted))
                    (source-relation! selected-operation candidate-source
                                      declaration inputs field-map)
                    (screened-read-premises! selected-operation candidate-source
                                             declaration inputs field-map proof-context)
                    ;; The underlying prepared gateway rechecks current route
                    ;; correspondence and consumes its own one-use permit.
                    ;; Its XTDB transaction, not this preflight, guards against
                    ;; physical changes after external prestate acquisition.
                    ((:submit! prepared)))
          summary! (fn []
                     {:type report-type
                      :version report-version
                      :status (if (.get attempted) :wrapper-permit-consumed :prepared)
                      :original-and-independent-abstract-relations :checked-per-attempt
                      :prestate-source :candidate-complete-bijective-document
                      :external-prestate :same-node-pinned-read-matched-at-preparation
                      :transaction-time-guard :existing-complete-top-level-cas
                      :source-declaration-provenance :caller-supplied-not-authenticated
                      :source-read-theorem-premises :source-relative-values-screened-not-proof-established
                      :claimed-read-premise-count (count (:read-premises proof-context))
                      :original-invariant-source-authenticity :not-established
                      :principal-authorization :not-established
                      :whole-application-writer-coverage :not-established
                      :aba-history-detection :not-established
                      :commit-causality :not-established
                      :physical-proof-refinement :not-established
                      :submission-capability :one-use
                      :proof-status :conditional-abstract-and-physical-gateway-join
                      :certificate :not-issued :admission :not-admitted
                      :transaction-authority :one-shot-submission-capability})]
      (seams!)
      {:type report-type :version report-version
       :submit! submit! :summary! summary!})))

;; v1757: A narrower PUBLIC entry point. Source/catalog selection happens once
;; at issuance, not on each physical submission. This is a source RELATIVE
;; contract and not an authenticated application installation or trusted key.
(def binding-type :gesso.assurance.proof.xtdb-gateway-typed-transition-seam/source-bound)
(def binding-version 1)

;; v1765: An optional, synchronous DIAGNOSTIC capture. Only the actual
;; source-bound :submit! closure publishes these private events. This is
;; process-local and does not authenticate application callbacks, JVM code,
;; a durable XTDB receipt, or the deployed handler path.
(def ^:dynamic ^:private *source-bound-submission-trace* nil)
(def source-bound-trace-type
  :gesso.assurance.proof.xtdb-gateway-typed-transition-seam/source-bound-trace)
(def source-bound-trace-version 1)

(defn- publish-source-bound-submission! [operation token phase]
  (when-let [{:keys [thread events]} *source-bound-submission-trace*]
    (swap! events conj {:operation operation :token token :phase phase
                        :same-thread? (identical? thread (Thread/currentThread))})))

(defn observe-source-bound-typed-submission!
  "Run invoke! ONCE under an opt-in, process-local trace. Only genuine
   v1757 source-bound gateway submit closures can publish the private pair of
   :entered/:returned events. The trace positively reports ONE same-thread
   matching operation's gateway return; no copied report, success-like Ring
   status or other adapter call can replace the private event pair.

   Returns {:execution {:outcome :returned/:threw :result/:error ...}
            :report redacted-map}. :execution may contain sensitive results
   or Throwables; never expose it as certification evidence. A thrown or
   ambiguous execution must NOT be retried on the strength of this trace.
   This is NOT proof that the Biff handler invoked the gateway or that a
   transaction committed, nor evidence that XTDB receipts are authentic."
  [selected-operation invoke!]
  (when-not (and (keyword? selected-operation)
                 (namespace selected-operation)
                 (fn? invoke!))
    (reject! :invalid-source-bound-submission-observer))
  (when *source-bound-submission-trace*
    (reject! :nested-source-bound-submission-observer))
  (let [events (atom [])
        observation {:thread (Thread/currentThread) :events events}
        execution (binding [*source-bound-submission-trace* observation]
                    (try {:outcome :returned :result (invoke!)}
                         (catch Throwable e {:outcome :threw :error e})))
        captured @events
        exactly-one-return?
        (and (= :returned (:outcome execution))
             (= 2 (count captured))
             (let [[entered returned] captured]
               (and (= :entered (:phase entered))
                    (= :returned (:phase returned))
                    (= selected-operation (:operation entered)
                       (:operation returned))
                    (identical? (:token entered) (:token returned))
                    (true? (:same-thread? entered))
                    (true? (:same-thread? returned)))))
        status (cond
                 exactly-one-return? :one-source-bound-typed-submit-return-observed
                 (empty? captured) :no-source-bound-typed-submit-observed
                 (= :threw (:outcome execution)) :submission-or-invocation-threw
                 :else :ambiguous-or-mismatched-source-bound-submission)]
    {:execution execution
     :report {:type source-bound-trace-type
              :version source-bound-trace-version
              :operation-id selected-operation
              :scope :one-synchronous-process-local-invocation
              :status status
              :source-bound-gateway-return
              (if exactly-one-return? :observed :not-established)
              :installed-biff-handler-invocation :not-established
              :gateway-to-adapter-causality :not-established
              :xtdb-transaction-committed :not-established
              :xtdb-receipt-authenticity :not-established
              :original-business-policy-authenticity :not-established
              :complete-deployed-writer-coverage :not-established
              :certificate :not-issued :admission :not-admitted
              :authority :none}}))

(defn- check-bound-source!
  [catalog selected-operation]
  (seams!)
  (when-not (and (plain-map? catalog)
                 (= #{:id :declared-operation-ids :initial-condition
                      :invariants :operations} (set (keys catalog)))
                 (keyword? selected-operation)
                 (namespace selected-operation))
    (reject! :invalid-closed-declared-source-catalog))
  (let [report (try (original-catalog-check catalog)
                    (catch Exception _
                      (reject! :invalid-closed-declared-source-catalog)))
        entries (filter #(= selected-operation (:id %)) (:operations catalog))]
    (when-not (and (= operation-set/report-type (:type report))
                   (= operation-set/operation-set-version (:version report))
                   (= :closed-declared-abstract-operation-set-only (:scope report))
                   (= :checked (:exact-declared-operation-coverage report))
                   (= :none (:authority report))
                   (= :not-issued (:certificate report))
                   (= :not-admitted (:admission report))
                   (= 1 (count entries))
                   (contains? (:declared-operation-ids catalog) selected-operation)
                   (= (set (keys (:invariants catalog))) (:invariant-ids report)))
      (reject! :source-bound-operation-coverage-not-established))
    (let [construction (:construction (first entries))
          checked (try (original-constructor construction)
                       (catch Exception _ (reject! :invalid-bound-original-construction)))
          declaration (:transition checked)
          invariants (:invariants catalog)
          read-targets (for [[target [kind reader]] (:assignments declaration)
                             :when (= kind :read)]
                         [target reader])
          required (vec (for [[target reader] (sort-by first read-targets)
                              [inv-id rule] (sort-by key invariants)
                              :when (= #{target} (:readers rule))]
                          {:operation-id selected-operation
                           :invariant-id inv-id
                           :field target
                           :reader reader
                           :reader-origin (if (contains? (:input-types declaration) reader)
                                            :input :prestate)}))
          covered (set (map :field required))]
      (when-not (and (= constructor/construction-type (:type checked))
                     (= declaration (:transition checked))
                     (= selected-operation (get-in declaration [:subject :id]))
                     (seq required)
                     (= covered (set (map first read-targets)))
                     (every? #(= 1 (count (:readers (val %)))) invariants))
        ;; The existing per-read predicate evaluator only accepts singleton
        ;; readers. Do not silently drop compound invariants or read targets.
        (reject! :unsupported-or-uncovered-bound-invariants))
      {:declaration declaration
       :proof-context {:invariants invariants :read-premises required}
       :report report})))

(defn issue-source-bound-typed-transition-gateway!
  "Issue a process-local, source-relative physical gateway PREPARER for one
   operation of an exact, rederived CLOSED DECLARED catalog. The resulting
   :prepare! accepts no declaration, operation ID, invariant inventory, or
   read-premise list: those are captured from the checked original catalog.

   The issuer's catalog remains an independent APPLICATION-SOURCE PREMISE:
   no signature, installer audit, total business-policy inventory, proof,
   physical writer exhaustiveness, or release admission is established. The
   returned closure is not a general authority credential. Its :prepare!
   may issue a bounded one-shot writer only through the existing gateway."
  [source-catalog selected-operation]
  (let [bound (check-bound-source! source-catalog selected-operation)
        stable-source source-catalog
        stable-report (:report bound)
        prepare! (fn [node contract-source resolver-issuance request
                      candidate-source inputs field-map]
                   (let [now (check-bound-source! stable-source selected-operation)]
                     (when-not (= stable-report (:report now))
                       (reject! :bound-source-report-changed))
                     (let [prepared (prepare-typed-transition-one-shot!
                                     node contract-source resolver-issuance
                                     selected-operation request candidate-source
                                     (:declaration now) inputs field-map
                                     (:proof-context now))
                           original-submit (:submit! prepared)
                           first-attempt (AtomicBoolean. false)
                           trace-token (Object.)
                           submit! (fn []
                                     (when-not (.compareAndSet first-attempt false true)
                                       (reject! :source-bound-one-shot-already-attempted))
                                     (when-not (= stable-report
                                                  (:report (check-bound-source!
                                                            stable-source selected-operation)))
                                       (reject! :bound-source-report-changed))
                                     ;; A trace is emitted by THIS genuine
                                     ;; source-bound closure, not by a supplied
                                     ;; adapter report or synthetic response.
                                     (publish-source-bound-submission!
                                      selected-operation trace-token :entered)
                                     (try
                                       (let [result (original-submit)]
                                         (publish-source-bound-submission!
                                          selected-operation trace-token :returned)
                                         result)
                                       (catch Throwable e
                                         (publish-source-bound-submission!
                                          selected-operation trace-token :threw)
                                         (throw e))))
                           summary! (fn []
                                      (assoc ((:summary! prepared))
                                             :source-catalog-binding :closed-declared-source-checked
                                             :source-catalog-identity :copyable-not-authenticating
                                             :caller-selected-read-premises :not-accepted
                                             :source-completeness :declared-only-not-production-proved))]
                       {:type report-type :version report-version
                        :submit! submit! :summary! summary!})))
        summary! (fn []
                   {:type binding-type :version binding-version
                    :operation-id selected-operation
                    :source-catalog-binding :closed-declared-source-checked
                    :source-catalog-identity :copyable-not-authenticating
                    :caller-selected-read-premises :not-accepted
                    :source-completeness :declared-only-not-production-proved
                    :kernel-proof :not-established
                    :installed-source-provenance :not-established
                    :certificate :not-issued :admission :not-admitted
                    :authority :none})]
    (seams!)
    {:type binding-type :version binding-version
     :prepare! prepare! :summary! summary!}))

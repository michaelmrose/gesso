(ns gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial
  "v1855: Bind the v1775 checked-catalog physical trial to the EXACT issued
   v1251 certified-plan executor and the CURRENT assembled application's
   certified route/operation table. An observed physical transaction comes
   from calling the registered executor, not an arbitrary invocation thunk.

   Only local, source-relative identities are established. The caller still
   provides the contract source, application assembly, typed catalog,
   physical field projection, request context and command invocation. This
   does NOT independently attest the deployed Biff handler or prove that
   arbitrary registered callbacks implement business policy correctly.

   v1848: One selected operation must own exactly its own routes; it must
   NOT be required to own every certified route in a multi-operation app.
   Independently verify that the disjoint certified operation route sets
   form the complete original declared route inventory. Never relax exact
   issued executor, construction, command identity or physical field checks.

   v1849: Reuse the genuine catalog-wide original source/issued-handler
   checker. Selected-operation checks alone do not establish full catalog
   construction and route correspondence when other certified operations
   coexist. Its report remains source-relative and non-authoritative.

   v1851: Preserve the genuine full catalog's unresolved proof assumptions,
   premises and per-operation obligations in issued physical trial reports.
   Recheck this evidence with the original source before summarizing or
   observing. A physical acknowledgment does NOT discharge proof obligations.

   v1852: A physical trial scoped to one certified operation may coexist with
   unrelated uncertified application operations/routes. Check the ORIGINAL
   contract's complete semantic route inventory and the exact certified /
   uncertified partition rather than requiring every app route to be certified.
   This does not grant the unselected routes certification or transaction
   authority.

   v1853: Carry the original selected/uncertified operation and route scope
   into the one-use trial identity and public reports. A source-relative
   acknowledged Claim write must never hide an uncertified Cancel route or
   promote it to certified or deployed transaction authority.

   v1854: Include the selected original catalog, transition and proof source
   fingerprints in the bounded physical trial report. They are rechecked
   source identifiers, not discharged proof or physical receipt authority.

   v1855: Report the original plan's otherwise-valid operation-ID interface
   alias as a specific unsupported typed-physical seam. The downstream typed
   source-relative interpreter currently requires the concrete domain command
   to equal the declared interface operation. Never broaden that equality
   without an explicit original source-bound alias correspondence.

   v1857: Derive a separate, read-only alias relation from the ORIGINAL
   registered executor plus fully checked source/catalog/route inventory.
   The alias is no longer merely caller-declared, but its report remains
   local source-relative evidence. This does NOT opt the alias into the
   original physical issuance or submit path, which remains fail-closed.

   v1858: Check the SAME issued-plan alias against original and independent
   abstract interpreters, using the source's own transition and field map and
   requiring the full original invariant inventory. A candidate domain plan,
   input values and read-premise pairs are still caller supplied; they are
   screened, NOT authenticated or physically adopted. No original physical
   issuance or submit path changes.

   v1859: Opt-in evaluation of the ORIGINAL registered plan constructor and
   acquisition callbacks, with the original certified-plan candidate checker
   and the v1858 typed check. It does not call the issued executor, Gesso's
   physical writer, or authorize submission. Arbitrary application callback
   effects remain OUTSIDE this check's guarantees; only a trusted, known-pure
   registered constructor should be evaluated this way.

   v1860: Observe one candidate IN the exact original issued executor's
   guarded :construct! pre-submit gate (with a second canonical-writer abort
   hook). The check still runs trusted application callbacks, which may have
   external effects. Typed correspondence is only conditional, local, and
   source-relative. No XTDB submission or retry authority is granted.

   v1861: An explicit read-only mode independently compares the candidate's
   SAME acquired complete prestate to a parameterized, pinned XTDB snapshot
   on the request-context node. Mismatches and absent documents fail closed;
   this is a point-in-time external read, not a transaction-time assertion,
   authenticated node, callback-purity theorem, or history/ABA proof.

   v1862: A stronger explicit pinned mode derives EVERY typed read premise
   from the ORIGINAL fully checked catalog and selected operation instead of
   accepting a caller-produced proof-context map. It checks the derivation's
   declaration against this same issued executor, then tests the captured
   prestate and inputs. This is still source-relative, not persistent custody
   of the physical read, transaction-time freshness or proof admission.

   v1863: Retain the EXACT original checked full-catalog open assumptions,
   premises, per-operation obligations and selected source identity in the
   source-derived/pinned executor observation. A successful abstract or
   physical prestate check neither closes nor discharges any obligation."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.operation-set :as catalog]
            [gesso.assurance.proof.checked-authorization-assembly
             :as coverage]
            [gesso.assurance.proof.xtdb-gateway-catalog-bound-atomic-canonical-trial
             :as canonical]
            [gesso.assurance.proof.xtdb-gateway-typed-transition-seam
             :as typed-seam]
            [gesso.assurance.transition :as constructor]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.basis :as basis])
  (:import (java.time Instant)
           (java.util UUID)
           (java.util.concurrent.atomic AtomicBoolean)))

(def report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/issued-executor)
(def report-version 1)
(def issued-alias-report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/issued-command-alias)
(def issued-alias-report-version 1)
(def issued-typed-alias-report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/issued-typed-command-alias)
(def issued-typed-alias-report-version 1)
(def issued-constructed-alias-report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/registered-constructor-typed-alias)
(def issued-constructed-alias-report-version 1)
(def issued-executor-candidate-alias-report-type
  :gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial/original-executor-typed-candidate)
(def issued-executor-candidate-alias-report-version 1)

(def ^:private original-contract-check certified/check-certified-contracts!)
(def ^:private original-operation-summary application/operation-summary)
(def ^:private original-catalog-check catalog/check-operation-set!)
(def ^:private original-full-coverage-check
  coverage/check-declared-route-coverage!)
(def ^:private original-construct constructor/build-guarded-replacement!)
(def ^:private original-plan-require plan/require-current-plan-operation!)
(def ^:private original-plan-describe plan/describe-plan-operation)
(def ^:private original-plan-candidate-check plan/check-candidate!)
(def ^:private original-executor-candidate-observer
  plan/observe-original-executor-candidate-before-submit!)
(def ^:private original-binding-check resolution/require-binding)
(def ^:private original-external-query xt/q)
(def ^:private original-snapshot-decode basis/<-time-basis-str)
(def ^:private original-canonical-issuer
  canonical/issue-catalog-bound-canonical-trial!)
(def ^:private original-typed-alias-screen
  typed-seam/check-source-relative-typed-domain-with-command-binding!)
(def ^:private original-source-premises
  typed-seam/check-source-bound-typed-read-premises!)

(defn- reject! [reason]
  (throw (ex-info "Issued catalog/canonical executor binding is not established."
                  {:issued-catalog-canonical-trial/rejection true
                   :reason reason})))

(defn- pinned! []
  (when-not (and (identical? original-contract-check
                             (var-get #'certified/check-certified-contracts!))
                 (identical? original-operation-summary
                             (var-get #'application/operation-summary))
                 (identical? original-catalog-check
                             (var-get #'catalog/check-operation-set!))
                 (identical? original-full-coverage-check
                             (var-get #'coverage/check-declared-route-coverage!))
                 (identical? original-construct
                             (var-get #'constructor/build-guarded-replacement!))
                 (identical? original-plan-require
                             (var-get #'plan/require-current-plan-operation!))
                 (identical? original-plan-describe
                             (var-get #'plan/describe-plan-operation))
                 (identical? original-plan-candidate-check
                             (var-get #'plan/check-candidate!))
                 (identical? original-executor-candidate-observer
                             (var-get #'plan/observe-original-executor-candidate-before-submit!))
                 (identical? original-binding-check
                             (var-get #'resolution/require-binding))
                 (identical? original-external-query (var-get #'xt/q))
                 (identical? original-snapshot-decode
                             (var-get #'basis/<-time-basis-str))
                 (identical? original-canonical-issuer
                             (var-get #'canonical/issue-catalog-bound-canonical-trial!))
                 (identical? original-source-premises
                             (var-get #'typed-seam/check-source-bound-typed-read-premises!)))
    (reject! :substituted-original-assembly-or-issuer)))

(defn- plain-map? [x]
  (and (or (instance? clojure.lang.PersistentArrayMap x)
           (instance? clojure.lang.PersistentHashMap x))
       (nil? (meta x))))

(defn- closed-selected-route-scope?
  "Validate one operation's exact certified routes while retaining the
   disjoint source-closed route inventory of every certified operation.
   The original certified-contract check authenticates the report; this
   predicate does not authenticate an independent deployment or handler."
  [selected-operation selected-route-ids certified-rows
   complete-certified-route-ids uncertified-route-ids declared-route-ids]
  (let [route-sets (when (map? certified-rows)
                     (mapv :route-ids (vals certified-rows)))
        flattened (vec (mapcat identity route-sets))]
    (and (qualified-keyword? selected-operation)
         (set? selected-route-ids)
         (seq selected-route-ids)
         (every? qualified-keyword? selected-route-ids)
         (map? certified-rows)
         (seq certified-rows)
         (every? qualified-keyword? (keys certified-rows))
         (every? #(and (set? %) (seq %)
                       (every? qualified-keyword? %)) route-sets)
         (= selected-route-ids
            (get-in certified-rows [selected-operation :route-ids]))
         (set? complete-certified-route-ids)
         (set? uncertified-route-ids)
         (every? qualified-keyword? uncertified-route-ids)
         (set? declared-route-ids)
         (seq complete-certified-route-ids)
         (= (count flattened) (count (set flattened)))
         (= (set flattened) complete-certified-route-ids)
         ;; The exact complement must be reported by the genuine original
         ;; catalog closure. Missing, duplicated and invented semantic route
         ;; coordinates cannot be excused as unrelated uncertified operations.
         (empty? (set/intersection complete-certified-route-ids
                                   uncertified-route-ids))
         (= declared-route-ids
            (set/union complete-certified-route-ids uncertified-route-ids))
         (set/subset? selected-route-ids complete-certified-route-ids))))

(defn- current-binding!
  ([contract-source catalog-source operation]
   (current-binding! contract-source catalog-source operation false))
  ([contract-source catalog-source operation source-only-alias?]
  (pinned!)
  (when-not (and (plain-map? contract-source)
                 (plain-map? catalog-source)
                 (= #{:catalog :physical-field-map} (set (keys catalog-source)))
                 (plain-map? (:catalog catalog-source))
                 (plain-map? (:physical-field-map catalog-source))
                 (qualified-keyword? operation))
    (reject! :invalid-current-assembled-catalog-source))
  (let [checked (try (original-catalog-check (:catalog catalog-source))
                     (catch Exception _ (reject! :invalid-original-catalog)))
        contracts (try (original-contract-check contract-source)
                       (catch Exception _
                         (reject! :invalid-current-certified-operation-table)))
        full-coverage
        (try (original-full-coverage-check
              {:contract-source contract-source
               :catalog-source catalog-source})
             (catch Exception _
               (reject! :catalog-executor-or-assembled-operation-diverged)))
        summary (try (original-operation-summary
                      (get-in contract-source
                              [:application-assembly :operation-acquisition-assembly]))
                     (catch Exception _ (reject! :invalid-current-operation-summary)))
        ids (:declared-operation-ids checked)
        operation-row (get summary operation)
        catalog-row (first (filter #(= operation (:id %))
                                   (get-in catalog-source [:catalog :operations])))
        bound (get-in contract-source [:certified-operations operation])
        executor (:executor bound)
        source (:current-source bound)
        description (when (and executor source)
                      (try (original-plan-require executor source)
                           (original-plan-describe executor)
                           (catch Exception _
                             (reject! :current-issued-plan-executor-not-established))))
        transition (when catalog-row
                     (try (original-construct (:construction catalog-row))
                          (catch Exception _
                            (reject! :invalid-original-catalog-construction))))
        routes (get operation-row :routes)
        route-ids (set (map :route-id routes))
        certified-row (get-in contracts [:certified-operations operation])
        open-evidence
        (select-keys full-coverage
                     [:open-assumptions :open-premises :open-obligations
                      :certified-operation-open-evidence])
        route-scope
        (select-keys full-coverage
                     [:catalog-operations :matched-certified-route-ids
                      :uncertified-application-operations
                      :uncertified-application-route-ids])]
    ;; The original v1251 plan builder permits an interface operation to
    ;; equal either the *domain command* or the certified operation ID. This
    ;; upper binding cannot treat those forms as physically interchangeable:
    ;; the current lower v1755 typed interpreter checks that the actual domain
    ;; command is exactly the declaration's interface operation. Fail before
    ;; issuing a one-shot physical trial, and distinguish this otherwise-valid
    ;; original plan from an arbitrary malformed catalog.
    (when (and (not source-only-alias?)
               (= operation (:operation description))
               (= operation (get-in source [:certified-source :operation]))
               (= operation (get-in (:construction source)
                                    [:subject :interface :operation]))
               (= (:command-operation source)
                  (:command-operation description))
               (not= operation (:command-operation source)))
      (reject! :typed-physical-command-interface-alias-not-established))
    (when-not (and (= catalog/report-type (:type checked))
                   (= :checked (:exact-declared-operation-coverage checked))
                   (= certified/report-type (:type contracts))
                   (= certified/adapter-version (:version contracts))
                   ;; The genuine original catalog-scoped source check compares
                   ;; EVERY declared construction to its issued plan source
                   ;; and EVERY certified route to its exact issued handler.
                   ;; This may contain other operations; it does not turn an
                   ;; application into a business-policy certificate.
                   (= coverage/coverage-report-type (:type full-coverage))
                   (= coverage/report-version (:version full-coverage))
                   (= :source-relative-checked
                      (:current-assembly-closure full-coverage))
                   (= :not-established
                      (:installed-handler-adoption full-coverage))
                   (= :not-issued (:certificate full-coverage))
                   (= :none (:transaction-authority full-coverage))
                   (set? (:open-assumptions open-evidence))
                   (set? (:open-premises open-evidence))
                   (set? (:open-obligations open-evidence))
                   (map? (:certified-operation-open-evidence open-evidence))
                   (= ids (set (keys (:certified-operation-open-evidence
                                       open-evidence))))
                   (every?
                    (fn [[_ row]]
                      (and (= #{:route-ids :open-premises :open-obligations}
                              (set (keys row)))
                           (set? (:route-ids row))
                           (set? (:open-premises row))
                           (set? (:open-obligations row))))
                    (:certified-operation-open-evidence open-evidence))
                   ;; Capture all four current original selected/unselected
                   ;; scope sets in the identity rechecked before and after
                   ;; the physical trial. This does not authenticate their
                   ;; installation in an external router.
                   (every? set? (vals route-scope))
                   (every? (fn [routes] (every? qualified-keyword? routes))
                           (vals route-scope))
                   (= ids (:catalog-operations full-coverage))
                   (= (:matched-certified-route-ids contracts)
                      (:matched-certified-route-ids full-coverage))
                   ;; The genuine original contract enumerates the FULL app
                   ;; route table, while this trial binds one selected
                   ;; certified catalog only. Recheck the complement, never
                   ;; silently invent certification for unrelated operations.
                   (= (:uncertified-operations contracts)
                      (:uncertified-application-operations full-coverage)
                      (set/difference (set (keys summary)) ids))
                   (= (set/difference (:route-ids contracts)
                                      (:matched-certified-route-ids contracts))
                      (:uncertified-application-route-ids full-coverage))
                   (empty? (set/intersection
                            (:matched-certified-route-ids contracts)
                            (:uncertified-application-route-ids full-coverage)))
                   (= (:route-ids contracts)
                      (set/union (:matched-certified-route-ids contracts)
                                 (:uncertified-application-route-ids full-coverage)))
                   (= ids (set (keys (:certified-operations contract-source)))
                      (set (keys (:certified-operations contracts))))
                   (= :none (:transaction-authority contracts))
                   (= :none (:certificate contracts))
                   (contains? ids operation)
                   (= (:construction catalog-row) (:construction source))
                   (= (:physical-field-map catalog-source) (:field-map source))
                   (= operation (:operation description)
                      (:operation certified-row))
                   ;; The genuine v1253 checker also records the proof's
                   ;; origin and dependency graph; keep those identities
                   ;; together, not just the flattened proof fingerprint.
                   (every? some? (map certified-row
                                      [:proof-source-fingerprint
                                       :proof-subject-fingerprint
                                       :dependency-graph-fingerprint]))
                   (= (get-in source [:certified-source :proof-source :transition])
                      (:transition transition))
                   (= (:transition-fingerprint transition)
                      (:transition-fingerprint description)
                      (:transition-fingerprint certified-row))
                   (or (= (:command-operation source)
                          (get-in (:construction source) [:subject :interface :operation]))
                       (and source-only-alias?
                            (= operation
                               (get-in (:construction source)
                                       [:subject :interface :operation]))
                            (= (:command-operation source)
                               (:command-operation description))
                            (keyword? (:command-operation source))
                            (not= :create (:command-operation source))
                            (not= operation (:command-operation source))))
                   (vector? routes) (seq routes)
                   (= (count routes) (count route-ids))
                   (closed-selected-route-scope?
                    operation route-ids (:certified-operations contracts)
                    (:matched-certified-route-ids contracts)
                    (:uncertified-application-route-ids full-coverage)
                    (:route-ids contracts)))
      (reject! :catalog-executor-or-assembled-operation-diverged))
    ;; The alternate validation mode is ONLY for a read-only alias report:
    ;; reject even an ordinary strict command/interface match here. It never
    ;; reaches the canonical trial issuer or an executor invocation.
    (when (and source-only-alias?
               (not (and (= operation
                            (get-in (:construction source)
                                    [:subject :interface :operation]))
                         (keyword? (:command-operation source))
                         (not= :create (:command-operation source))
                         (not= operation (:command-operation source)))))
      (reject! :original-issued-command-alias-not-established))
    (pinned!)
    {:operation-id operation
     :command-operation (:command-operation source)
     :catalog-fingerprint (:fingerprint checked)
     :transition-fingerprint (:transition-fingerprint transition)
     :proof-fingerprint (:proof-fingerprint certified-row)
     :proof-source-fingerprint (:proof-source-fingerprint certified-row)
     :proof-subject-fingerprint (:proof-subject-fingerprint certified-row)
     :dependency-graph-fingerprint (:dependency-graph-fingerprint certified-row)
     :route-ids route-ids
     ;; Include unresolved evidence in the binding identity checked at
     ;; issue, pre-observation, post-observation and every summary. It cannot
     ;; silently change while the trial still reports the original source.
     :catalog-open-evidence open-evidence
     :catalog-route-scope route-scope
     ;; The issued callable is captured privately for invocation, never
     ;; included in a report or user-provided to the physical trial.
     :executor executor})))

(defn check-current-issued-command-alias!
  "Read-only, source-relative check for a legitimate certified-plan interface
   operation-ID alias. Derive the concrete domain command from the SAME
   original registered plan executor, matched construction, certified
   operation table and complete checked route catalog. No caller-supplied
   alias map, transition replacement, executor, or invocation is accepted.

   The result is COPYABLE local provenance, not an authenticated deployed
   command, a checked physical transition, an issued transaction capability,
   a proof certificate or admission. The existing physical trial MUST keep
   rejecting this alias until its lower typed relation is separately bound."
  [contract-source catalog-source operation]
  (let [bound (current-binding! contract-source catalog-source operation true)]
    {:type issued-alias-report-type
     :version issued-alias-report-version
     :operation-id operation
     :command-binding {:operation-id operation
                       :interface-operation operation
                       :domain-command-operation (:command-operation bound)}
     :command-binding-provenance :current-original-issued-plan-and-catalog
     :scope :process-local-source-relative-only
     :source-identity (select-keys bound
                                   [:catalog-fingerprint
                                    :transition-fingerprint
                                    :proof-fingerprint :route-ids])
     :complete-catalog-scope :original-checked-application-assembly
     :deployed-source-authenticity :not-established
     :typed-abstract-alias-correspondence :not-established
     :canonical-physical-alias-execution :disabled
     :original-business-policy-authenticity :not-established
     :certificate :not-issued :admission :not-admitted
     :transaction-authority :none :authority :none}))

(defn check-current-issued-typed-command-alias!
  "Extend the read-only ORIGINAL issued-plan alias check through the original
   and independent typed abstract interpreters. The declaration, command
   binding, and physical field map are derived from the checked issued plan,
   never provided as caller-replaceable arguments. The supplied invariant
   inventory MUST equal the checked original catalog inventory; the original
   typed checker derives and screens every read-premise value.

   The candidate domain plan and transition inputs are still caller supplied.
   The result establishes only conditional abstract source correspondence,
   NOT issuer-produced candidate authenticity, physical writer adoption,
   business-policy authentication, kernel proof, admission or transaction
   authority. All physical issuance paths remain strict and unchanged."
  [contract-source catalog-source operation domain-source inputs proof-context]
  (let [initial (current-binding! contract-source catalog-source operation true)
        original-source (get-in contract-source
                                [:certified-operations operation :current-source])
        construction (:construction original-source)
        original-invariants (get-in catalog-source [:catalog :invariants])
        binding {:operation-id operation
                 :interface-operation operation
                 :domain-command-operation (:command-operation initial)}]
    (when-not (and (plain-map? proof-context)
                   (plain-map? original-invariants)
                   (seq original-invariants)
                   (= original-invariants (:invariants proof-context)))
      (reject! :issued-typed-read-invariants-not-original))
    (when-not (identical? original-typed-alias-screen
                          (var-get #'typed-seam/check-source-relative-typed-domain-with-command-binding!))
      (reject! :substituted-original-typed-alias-screen))
    ;; This constructor is the original checked semantic projection of the
    ;; issued plan's construction, not a caller supplied declaration.
    (let [declaration (:transition (original-construct construction))
          screening (original-typed-alias-screen
                     operation domain-source declaration inputs
                     (:field-map original-source) proof-context binding)]
      (when-not (and (= typed-seam/source-check-type (:type screening))
                     (= typed-seam/source-check-version (:version screening))
                     (= operation (:operation-id screening))
                     (= :original-and-independent-agree
                        (:abstract-interpreters screening))
                     (= :source-relative-values-screened
                        (:original-read-premises screening))
                     (= :bijective (:complete-domain-field-projection screening))
                     (= :explicit-domain-command-operation-id-alias
                        (:command-interface-relation screening))
                     (= :caller-declared-not-authenticated
                        (:command-binding-provenance screening))
                     (= :disabled (:physical-submission screening))
                     (= :not-issued (:certificate screening))
                     (= :not-admitted (:admission screening))
                     (= :none (:authority screening)))
        (reject! :original-typed-alias-source-relation-not-established))
      (when-not (and (identical? original-typed-alias-screen
                                (var-get #'typed-seam/check-source-relative-typed-domain-with-command-binding!))
                     (= initial (current-binding! contract-source catalog-source
                                                  operation true)))
        (reject! :current-issued-typed-alias-source-changed))
      (assoc (check-current-issued-command-alias!
              contract-source catalog-source operation)
             :type issued-typed-alias-report-type
             :version issued-typed-alias-report-version
             :typed-abstract-alias-correspondence
             :original-and-independent-source-relative
             :typed-declaration-source :original-issued-plan-construction
             :typed-field-map-source :original-issued-plan-field-map
             :typed-invariant-inventory :entire-original-checked-catalog
             :typed-read-premises :source-relative-values-screened
             :candidate-domain-source :caller-supplied-not-authenticated
             :physical-submission :disabled
             :typed-screening-physical-submission :disabled))))

(defn check-current-issued-constructor-typed-command-alias!
  "Opt-in, source-relative evaluation of the exact ORIGINAL registered plan
   constructor, followed by both the original plan candidate check and the
   independent typed transition screen. The caller provides ctx, one command
   binding and invariant read premises, but NOT the domain candidate, abstract
   inputs, current document, target, operation alias, construction or field map.

   This intentionally invokes the application-owned original acquisition and
   constructor callbacks OUTSIDE the issued executor. Those callbacks MAY
   perform arbitrary side effects, including physical writes; Gesso does not
   prove purity or isolate them. Use this only with a trusted, known-pure
   application fixture. The check itself never calls tx/transact!, and does
   not demonstrate that an installed handler executed this candidate.
   Never interpret this copyable report as an authorization or retry permit."
  [contract-source catalog-source operation ctx invocation proof-context]
  (let [initial (current-binding! contract-source catalog-source operation true)
        source (get-in contract-source
                       [:certified-operations operation :current-source])
        binding (:binding invocation)
        original-invariants (get-in catalog-source [:catalog :invariants])]
    ;; Reject invalid calls BEFORE running arbitrary application callbacks.
    (when-not (and (map? ctx)
                   (plain-map? invocation)
                   (= #{:binding :mode} (set (keys invocation)))
                   (= :submit (:mode invocation))
                   (plain-map? binding)
                   (= operation (:operation binding))
                   (try (= binding (original-binding-check binding))
                        (catch Exception _ false)))
      (reject! :invalid-registered-constructor-alias-invocation))
    (when-not (and (plain-map? proof-context)
                   (plain-map? original-invariants)
                   (seq original-invariants)
                   (= original-invariants (:invariants proof-context)))
      (reject! :issued-typed-read-invariants-not-original))
    (let [construct! (get-in source [:certified-source
                                    :resolved-configuration :construct!])
          document! (:current-document! source)
          inputs! (:transition-inputs! source)
          target! (:target! source)]
      (when-not (every? fn? [construct! document! inputs! target!])
        (reject! :missing-original-registered-constructor-or-acquisition))
      (let [document (document! ctx binding)
            inputs (inputs! ctx binding)
            target (target! ctx binding)
            constructed (construct! ctx binding)]
        (when-not (and (plain-map? constructed)
                       (= #{:now :plan :outcome} (set (keys constructed))))
          (reject! :invalid-original-registered-construction))
        (let [witness (original-plan-candidate-check
                       {:construction (:construction source)
                        :field-map (:field-map source)
                        :command-operation (:command-operation source)
                        :target target :current-document document
                        :inputs inputs :plan (:plan constructed)})]
          (when-not (and (= plan/witness-type (:type witness))
                         (= plan/plan-version (:version witness))
                         (= operation (:operation witness))
                         (= (:command-operation initial)
                            (:command-operation witness))
                         (= :pre-submit-candidate-checked (:checking witness))
                         (= false (:transaction-submitted? witness))
                         (= :none (:authority witness)))
            (reject! :original-registered-candidate-check-not-qualified))
          (let [screen (check-current-issued-typed-command-alias!
                        contract-source catalog-source operation
                        {:plan (:plan constructed)} inputs proof-context)]
            (when-not (= initial (current-binding!
                                  contract-source catalog-source operation true))
              (reject! :original-registered-constructor-source-changed))
            (assoc screen
                   :type issued-constructed-alias-report-type
                   :version issued-constructed-alias-report-version
                   :candidate-domain-source
                   :same-registered-source-constructor-evaluated
                   :candidate-input-source
                   :original-registered-input-acquisition-evaluated
                   :candidate-prestate-check
                   :original-certified-plan-pre-submit-checked
                   :registered-executor-invocation :not-invoked
                   :callback-purity :not-established
                   :callback-physical-side-effects :not-excluded
                   :installed-handler-execution :not-established
                   :physical-submission :not-invoked-by-this-check)))))))

(defn- pinned-executor-before!
  "Read exactly the candidate's whole acquired prestate on the SAME caller
   context node, using a single XTDB pinned external read. No state, token,
   SQL text or physical identity is returned; this is NOT a CAS or lock."
  [node candidate]
  (pinned!)
  (let [cmd (first (get-in candidate [:plan :commands]))
        target (:target candidate)
        table (first target)
        id (second target)
        before (:current-document candidate)
        declared-before (:model/before cmd)]
    (when-not (and (some? node)
                   (plain-map? candidate)
                   (plain-map? before)
                   (= before declared-before)
                   (= 1 (count (get-in candidate [:plan :commands])))
                   (plain-map? cmd)
                   (= table (:model/entity-type cmd))
                   (= id (:model/id cmd) (:xt/id before))
                   (keyword? table)
                   (nil? (namespace table))
                   (re-matches #"[a-z][a-z0-9_]*" (name table))
                   (or (and (string? id) (seq id))
                       (instance? UUID id)))
      (reject! :invalid-issued-executor-external-before-target))
    (let [token-rows (try (original-external-query node "SHOW SNAPSHOT_TOKEN")
                          (catch Throwable _
                            (reject! :issued-executor-snapshot-unavailable)))
          token (when (and (sequential? token-rows)
                           (= 1 (count token-rows))
                           (plain-map? (first token-rows))
                           (= #{:snapshot-token} (set (keys (first token-rows)))))
                  (:snapshot-token (first token-rows)))]
      (when-not (and (string? token) (seq token)
                     (try (map? (original-snapshot-decode token))
                          (catch Throwable _ false)))
        (reject! :invalid-issued-executor-snapshot))
      (let [sql (str "SELECT * FROM " (name table) " WHERE _id = ? LIMIT 2")
            rows (try (original-external-query node [sql id]
                                               {:snapshot-token token
                                                :current-time (Instant/now)})
                      (catch Throwable _
                        (reject! :issued-executor-external-before-read-unavailable)))]
        (when-not (and (sequential? rows)
                       (= 1 (count rows))
                       (plain-map? (first rows)))
          (reject! :issued-executor-external-before-missing-or-ambiguous))
        (when-not (= before (first rows))
          (reject! :issued-executor-external-before-mismatch))
        (pinned!)
        true))))

(defn- original-executor-open-proof-evidence!
  "Select original, already-checked, UNRESOLVED proof evidence from the
   same issued-plan binding as the candidate. The source check in
   current-binding! already rejects incomplete catalog/route coverage; this
   secondary shape/selected-row check prevents diagnostic reports from
   silently omitting the selected operation or presenting copied proof
   evidence as discharge. No proof kernel or receipt is invoked here."
  [initial operation]
  (let [evidence (:catalog-open-evidence initial)
        rows (:certified-operation-open-evidence evidence)
        selected (get rows operation)
        catalog-operations (get-in initial [:catalog-route-scope
                                            :catalog-operations])
        complete-route-ids (get-in initial [:catalog-route-scope
                                            :matched-certified-route-ids])
        row-premises (into #{} (mapcat :open-premises) (vals rows))
        row-obligations (into #{} (mapcat :open-obligations) (vals rows))
        row-routes (vec (mapcat :route-ids (vals rows)))
        proof-source-identity
        (select-keys initial [:proof-source-fingerprint
                              :proof-subject-fingerprint
                              :dependency-graph-fingerprint])]
    (when-not (and (map? evidence)
                   (= #{:open-assumptions :open-premises :open-obligations
                        :certified-operation-open-evidence}
                      (set (keys evidence)))
                   (every? set? ((juxt :open-assumptions :open-premises
                                       :open-obligations) evidence))
                   (map? rows)
                   (set? catalog-operations)
                   (= catalog-operations (set (keys rows)))
                   (every? (fn [row]
                             (and (map? row)
                                  (= #{:route-ids :open-premises :open-obligations}
                                     (set (keys row)))
                                  (every? set? (map row
                                                    [:route-ids :open-premises
                                                     :open-obligations]))
                                  (seq (:route-ids row))))
                           (vals rows))
                   (set? complete-route-ids)
                   (= (count row-routes) (count (set row-routes)))
                   (= complete-route-ids (set row-routes))
                   (= (:open-premises evidence) row-premises)
                   (= (:open-obligations evidence)
                      (set/union (:open-assumptions evidence)
                                 row-obligations))
                   (= #{:proof-source-fingerprint
                        :proof-subject-fingerprint
                        :dependency-graph-fingerprint}
                      (set (keys proof-source-identity)))
                   (every? some? (vals proof-source-identity))
                   (map? selected)
                   (= #{:route-ids :open-premises :open-obligations}
                      (set (keys selected)))
                   (= (:route-ids initial) (:route-ids selected))
                   (every? set? ((juxt :route-ids :open-premises
                                       :open-obligations) selected)))
      (reject! :original-issued-open-proof-evidence-not-established))
    {:catalog-open-evidence evidence
     :selected-operation-open-evidence selected
     :selected-operation-proof-source-identity proof-source-identity
     :catalog-global-assumptions-scope :whole-original-application-not-per-operation
     :selected-operation-obligations-scope :exact-original-selected-operation
     :open-proof-ledger-composition :original-global-union-of-selected-operation-rows
     :selected-operation-source-identity
     (select-keys initial [:catalog-fingerprint
                           :transition-fingerprint
                           :proof-fingerprint :route-ids])
     :catalog-proof-evidence-scope :original-source-rechecked
     :proof-obligations-disposition :retained-open-not-discharged
     :original-kernel-proof-discharge :not-established
     :physical-refinement-proof-discharge :not-established}))

(defn check-current-issued-executor-typed-command-alias!
  "Run the EXACT original registered executor as far as its guarded, checked
   candidate boundary, and abort BEFORE the canonical Gesso transaction
   writer. Reuse the original and independent typed source-relative checks.
   The caller supplies a real trusted request context, canonical invocation
   and read-premise values, but neither candidate nor transition projection.

   This executes application-owned callbacks and cannot prove their purity,
   authentication, independent physical effects or installed handler adoption.
   Reports remain copyable, unadmitted and carry no transaction authority.

   With a seventh argument :pinned-external-before, additionally require an
   exact physical document match at ONE independent, pinned XTDB read on the
   caller's original context node. This observation occurs after the guarded
   constructor capture, so application callbacks may already have effects.

   The opt-in mode :pinned-source-derived-premises requires a NIL caller
   proof-context and derives the exact invariant/read-premise inventory from
   the fully checked original catalog. It also compares the derived semantic
   declaration to the original issued construction BEFORE calling any
   application callbacks. Its physical read remains only point-in-time.

   Any successful diagnostic retains the original whole-catalog open proof
   evidence and exact selected operation evidence without claiming that a
   typed check, snapshot read or constructor capture discharged it.

   v1864: Verify the original catalog proof ledger's exact global/set union,
   and associate the selected operation's original proof source, subject and
   dependency graph identities with the same physical-prestate diagnostic.
   Global assumptions remain explicitly global, not selected-operation proofs."
  ([contract-source catalog-source operation ctx invocation proof-context]
   (check-current-issued-executor-typed-command-alias!
    contract-source catalog-source operation ctx invocation proof-context
    :no-external-read))
  ([contract-source catalog-source operation ctx invocation proof-context
    read-mode]
  (when-not (#{:no-external-read :pinned-external-before
               :pinned-source-derived-premises} read-mode)
    (reject! :unsupported-issued-executor-before-observation-mode))
  (let [source-derived? (= :pinned-source-derived-premises read-mode)
        pinned-read? (#{:pinned-external-before
                        :pinned-source-derived-premises} read-mode)]
  (when (and pinned-read? (nil? (:biff.xtdb/node ctx)))
    (reject! :missing-issued-executor-external-before-node))
  ;; Under source-derived mode, caller-supplied proof data are forbidden, not
  ;; silently ignored. Reject before any application-owned callback is run.
  (when (and source-derived? (some? proof-context))
    (reject! :caller-supplied-executor-premises-not-accepted))
  (let [initial (current-binding! contract-source catalog-source operation true)
        source (get-in contract-source
                       [:certified-operations operation :current-source])
        original-invariants (get-in catalog-source [:catalog :invariants])
        derived (when source-derived?
                  (let [provenance (original-source-premises
                                    (:catalog catalog-source) operation)
                        current-transition
                        (:transition (original-construct (:construction source)))]
                    (when-not (and (= typed-seam/source-bound-read-premises-type
                                      (:type provenance))
                                   (= typed-seam/source-bound-read-premises-version
                                      (:version provenance))
                                   (= operation (:operation-id provenance)
                                      (get-in (:declaration provenance)
                                              [:subject :id]))
                                   (= current-transition (:declaration provenance))
                                   (= (:catalog-fingerprint initial)
                                      (:catalog-fingerprint provenance))
                                   (= original-invariants
                                      (get-in provenance [:proof-context :invariants]))
                                   (= :closed-declared-source-relative
                                      (:scope provenance))
                                   (= :not-issued (:certificate provenance))
                                   (= :not-admitted (:admission provenance))
                                   (= :none (:authority provenance)))
                      (reject! :source-derived-executor-premises-not-established))
                    provenance))
        proof-context (if source-derived?
                        (:proof-context derived)
                        proof-context)
        binding (:binding invocation)]
    ;; Fail before calling any application-owned callback on bad input.
    (when-not (and (map? ctx)
                   (plain-map? invocation)
                   (= #{:binding :mode} (set (keys invocation)))
                   (= :submit (:mode invocation))
                   (plain-map? binding)
                   (= operation (:operation binding))
                   (try (= binding (original-binding-check binding))
                        (catch Exception _ false)))
      (reject! :invalid-original-executor-alias-invocation))
    (when-not (and (plain-map? proof-context)
                   (plain-map? original-invariants)
                   (seq original-invariants)
                   (= original-invariants (:invariants proof-context)))
      (reject! :issued-typed-read-invariants-not-original))
    (let [source (get-in contract-source
                         [:certified-operations operation :current-source])
          captured (original-executor-candidate-observer
                    (:executor initial) source ctx invocation)
          candidate (:candidate captured)
          witness (:witness captured)]
      (when-not (and (= plan/captured-candidate-type (:type captured))
                     (= plan/captured-candidate-version (:version captured))
                     (= :actual-registered-executor-guarded-construct
                        (:capture captured))
                     (= :aborted-before-canonical-writer (:submit-path captured))
                     (= :none (:authority captured))
                     (= plan/witness-type (:type witness))
                     (= plan/plan-version (:version witness))
                     (= operation (:operation witness))
                     (= (:command-operation initial) (:command-operation witness))
                     (= :pre-submit-candidate-checked (:checking witness))
                     (= false (:transaction-submitted? witness))
                     (= :none (:authority witness))
                     (= (:construction source) (:construction candidate))
                     (= (:field-map source) (:field-map candidate))
                     (= (:command-operation source) (:command-operation candidate)))
        (reject! :original-executor-checked-candidate-not-qualified))
      (when pinned-read?
        (pinned-executor-before! (:biff.xtdb/node ctx) candidate))
      (let [screen (check-current-issued-typed-command-alias!
                    contract-source catalog-source operation
                    {:plan (:plan candidate)} (:inputs candidate) proof-context)]
        (when-not (= initial
                     (current-binding! contract-source catalog-source operation true))
          (reject! :original-executor-source-changed-after-capture))
        (merge screen
               (original-executor-open-proof-evidence! initial operation)
               {:type issued-executor-candidate-alias-report-type
               :version issued-executor-candidate-alias-report-version
               :candidate-domain-source :original-executor-guarded-construct
               :candidate-input-source :original-executor-acquisition
               :candidate-prestate-check :original-certified-plan-pre-submit-checked
               :registered-executor-invocation :aborted-after-checked-candidate
               :callback-purity :not-established
               :callback-physical-side-effects :not-excluded
               :canonical-physical-alias-execution :disabled
               :physical-submission :blocked-before-canonical-writer
               :installed-handler-execution :not-established
               :external-before (if pinned-read?
                                  :pinned-same-node-document-match
                                  :not-observed)
               :typed-read-premise-provenance
               (if source-derived?
                 :original-checked-catalog-derived
                 :caller-declared-original-validated)
               :typed-read-premise-subject
               (if source-derived?
                 :original-issued-operation-id
                 :caller-context-checked)
               :external-basis-authenticity :caller-context-node-unattested
               :transaction-time-prestate :not-established
               :prestate-aba-exclusion :not-established
               :certificate :not-issued :admission :not-admitted
               :transaction-authority :none :authority :none})))))))

(defn- report [operation status invoked? catalog-open-evidence
               catalog-route-scope selected-operation-source-identity]
  {:type report-type :version report-version :operation-id operation
   :status status
   :catalog-open-evidence catalog-open-evidence
   :catalog-proof-evidence-scope :original-source-rechecked
   :catalog-route-scope catalog-route-scope
   :catalog-route-scope-evidence :original-source-rechecked
   ;; These original source fingerprints are checked in current-binding! at
   ;; issue, before/after the one-use observation and every later summary.
   ;; They identify which source-level proof/transition the trial relates to;
   ;; they are not a proof of physical effect or authorization.
   :selected-operation-source-identity selected-operation-source-identity
   :selected-operation-identity-scope :original-source-rechecked
   :catalog-to-checked-assembly :exact-closed-operation-coverage
   :catalog-to-issued-executor :exact-original-construction-and-field-map
   :certified-routes :current-issued-handlers-in-assembly
   :issued-executor-return (if invoked?
                             :one-original-plan-executor-returned
                             :not-established)
   :canonical-physical-execution
   (if (= status :bounded-issued-executor-catalog-atomic-acknowledgment)
     :one-source-relative-model-transaction-acknowledged
     :not-established)
   :deployed-biff-handler-adoption :not-established
   :original-business-policy-authenticity :not-established
   :current-principal-authorization :not-established
   :deployed-writer-completeness :not-established
   :physical-xtdb-receipt-authenticity :not-established
   :typed-complete-cas-writer-adoption :not-established
   :actual-live-delivery :not-established
   :retry-authority :none :certificate :not-issued
   :admission :not-admitted :transaction-authority :none :authority :none})

(defn issue-issued-catalog-canonical-trial!
  "Issue a one-use trial bound to the current certified assembly's EXACT
   original plan executor. Only [ctx invocation] are accepted at observation;
   the caller cannot substitute an invocation callback for the executor.
   The v1775 catalog-derived typed pre-submit validation remains in the same
   call. Recheck original plan, catalog and routes before and after execution.

   Never infer actual deployed Ring/Biff handler adoption or independent
   authenticity of callback effects from this local result."
  [contract-source catalog-source operation inputs]
  (when-not (plain-map? inputs)
    (reject! :invalid-typed-inputs))
  (let [initial (current-binding! contract-source catalog-source operation)
        original-executor (:executor initial)
        underlying (original-canonical-issuer catalog-source operation inputs)
        permit (AtomicBoolean. false)
        status (atom :not-attempted)
        returned? (atom false)
        recheck! (fn []
                   (when-not (= initial
                                (current-binding! contract-source catalog-source operation))
                     (reject! :current-issued-catalog-binding-changed))
                   (let [s ((:summary! underlying))]
                     (when-not (and (= canonical/report-type (:type s))
                                    (= canonical/report-version (:version s))
                                    (= operation (:operation-id s))
                                    (= :not-issued (:certificate s))
                                    (= :none (:authority s)))
                       (reject! :unqualified-original-catalog-trial)))
                   true)
        summary! (fn [] (recheck!)
                   (report operation @status @returned?
                           (:catalog-open-evidence initial)
                           (:catalog-route-scope initial)
                           (select-keys initial
                                        [:catalog-fingerprint
                                         :transition-fingerprint
                                         :proof-fingerprint
                                         :route-ids])))
        observe! (fn [ctx invocation]
                   (when-not (.compareAndSet permit false true)
                     (reject! :issued-executor-trial-already-attempted))
                   (reset! status :attempt-consumed-without-qualified-result)
                   (recheck!)
                   (when-not (and (map? ctx) (plain-map? invocation)
                                  (= #{:binding :mode} (set (keys invocation)))
                                  (= operation (get-in invocation [:binding :operation])))
                     (reject! :invalid-bound-executor-invocation))
                   (let [called? (atom false)
                         result ((:observe-once! underlying)
                                 (fn []
                                   (let [outcome (original-executor ctx invocation)]
                                     (reset! called? true)
                                     outcome)))
                         accepted? (and @called?
                                        (= canonical/report-type (:type result))
                                        (= canonical/report-version (:version result))
                                        (= operation (:operation-id result))
                                        (= :not-issued (:certificate result))
                                        (= :none (:authority result)))
                         qualified? (and accepted?
                                         (= :bounded-catalog-bound-atomic-acknowledgment
                                            (:status result)))]
                     (recheck!)
                     (when-not accepted?
                       (reject! :issued-executor-or-canonical-result-not-established))
                     (reset! returned? true)
                     (let [outcome (cond
                                     qualified?
                                     :bounded-issued-executor-catalog-atomic-acknowledgment
                                     (= :catalog-bound-canonical-rejected (:status result))
                                     :issued-executor-canonical-submission-rejected
                                     (= :catalog-bound-typed-check-not-established
                                        (:status result))
                                     :issued-executor-typed-check-rejected
                                     (= :no-canonical-submission-observed
                                        (:status result))
                                     :no-canonical-submission-observed
                                     :else :not-qualified-or-uncertain)]
                       (reset! status outcome)
                       (report operation outcome @returned?
                               (:catalog-open-evidence initial)
                               (:catalog-route-scope initial)
                               (select-keys initial
                                            [:catalog-fingerprint
                                             :transition-fingerprint
                                             :proof-fingerprint
                                             :route-ids])))))]
    (recheck!)
    {:type report-type :version report-version
     :summary! summary! :observe-once! observe!}))

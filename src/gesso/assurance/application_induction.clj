(ns gesso.assurance.application-induction
  "v1294: source-relative join of checked whole-catalog induction, actual
   issued Ring handlers, observable authoritative entry points, and the exact
   induction-bound authoritative plans used by those handlers.

   Input (all sources are ORIGINAL live objects, not copied reports):
   {:authoritative-coverage-source
     {:inductive-contract-source <v1288 original contract source>
      :enumerate-entry-points! <host-owned current snapshot enumerator>}
    :inductive-plans
     {qualified-operation-id
      {:executor <exact issued v1290 callable>
       :current-source {:inductive-source <same live proof package>
                        :certified-plan-source <same v1251 source>}}}}

   Every operation in the exact abstract catalog must be present, and each
   v1290-issued executor must be IDENTICAL to the executor in the actual v1253
   handler binding. All reports are rederived from original sources. A valid
   result is a COPYABLE explanation, never a certificate, authorization, or
   proof of deployed-router adoption or physical XTDB refinement. The host
   enumerator can omit writers; no such inventory proves its own completeness.

   The trust boundary remains the existing in-process JVM and source-owned
   checker registries. An attacker able to replace private JVM state or
   compiled code is outside the assurance claimed here."
  (:require [clojure.set :as set]
            [gesso.live.application-preflight.authoritative-coverage :as coverage]
            [gesso.live.application-preflight.inductive-contract :as contract]
            [gesso.model.inductive-plan :as plan]))

(def report-version 1)
(def report-type :gesso.assurance.application-induction/source-bound-application-report)

(def ^:private input-keys #{:authoritative-coverage-source :inductive-plans})
(def ^:private plan-binding-keys #{:executor :current-source})
(def ^:private plan-source-keys #{:inductive-source :certified-plan-source})
(def ^:private source-bound-keys
  #{:catalog :certificate-handle :dependency-handle :current-graph})
(def ^:private extra-open
  #{:host-authoritative-entry-point-inventory-is-complete
    :non-route-xtdb-writers-are-covered
    :deployed-router-and-executors-adopt-checked-snapshot
    :post-check-runtime-bindings-remain-stable
    :physical-prestate-and-initialization-correspondence
    :xtdb-commit-atomicity-and-authorization
    :abstract-to-physical-transition-refinement
    :independent-induction-checker-soundness})

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso application-wide inductive assurance source."
                  (merge {:error/type ::invalid-application-induction
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [value expected at]
  (when-not (and (plain-map? value) (nil? (meta value)))
    (reject! :expected-unannotated-plain-map at {}))
  (let [actual (set (keys value))]
    (when-not (= expected actual)
      (reject! :wrong-source-keys at
               {:missing (set/difference expected actual)
                :unexpected (set/difference actual expected)})))
  value)

(defn- qualified-id? [id]
  (and (keyword? id) (some? (namespace id))))

;; Public Var replacement cannot inject a preapproved report. Upstream
;; checkers independently rederive their own inputs and verify live handles.
(def ^:private original-coverage coverage/check-authoritative-coverage!)
(def ^:private original-plan-check plan/require-current-inductive-plan!)

(defn- recheck-upstream! []
  (when-not (and (identical? original-coverage
                             (var-get #'coverage/check-authoritative-coverage!))
                 (identical? original-plan-check
                             (var-get #'plan/require-current-inductive-plan!)))
    (reject! :substituted-upstream-checker [] {})))

(defn- checked-coverage! [source]
  (let [r (original-coverage source)]
    (when-not (and (= coverage/report-type (:type r))
                   (= coverage/coverage-version (:version r))
                   (= :fresh-source-derived-contract-and-host-enumeration
                      (:checking r))
                   (= :exact-for-supplied-snapshot
                      (:observable-semantic-route-coverage r))
                   (= (:declared-semantic-route-ids r)
                      (:observed-matched-semantic-route-ids r))
                   (set? (:abstract-operation-ids r))
                   (seq (:abstract-operation-ids r))
                   (every? qualified-id? (:abstract-operation-ids r))
                   (set? (:observed-unclassified-entry-point-ids r))
                   (= :all-original-obligations-remain-open
                      (:original-obligation-status r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :not-established
                      (:abstract-to-physical-transition-correspondence r))
                   (= :not-established (:active-router-adoption r))
                   (= :none (:transaction-authority r))
                   (= :none (:authority r)))
      (reject! :unexpected-authoritative-coverage-contract
               [:authoritative-coverage-source] {}))
    r))

(defn- checked-plan! [operation binding source-bound coverage-result]
  (exact-map! binding plan-binding-keys [:inductive-plans operation])
  (let [{:keys [executor current-source]} binding]
    (exact-map! current-source plan-source-keys
                [:inductive-plans operation :current-source])
    (let [inductive-source (:inductive-source current-source)]
      (exact-map! inductive-source source-bound-keys
                  [:inductive-plans operation :current-source :inductive-source])
      ;; Identity of in-process evidence handles matters in addition to
      ;; equal source data: a different certificate is a different admission.
      (when-not (and (= (:catalog source-bound) (:catalog inductive-source))
                     (= (:current-graph source-bound)
                        (:current-graph inductive-source))
                     (identical? (:certificate-handle source-bound)
                                 (:certificate-handle inductive-source))
                     (identical? (:dependency-handle source-bound)
                                 (:dependency-handle inductive-source)))
        (reject! :mismatched-induction-source
                 [:inductive-plans operation :current-source] {})))
    (when-not (plan/inductive-plan-operation? executor)
      (reject! :unissued-inductive-plan [:inductive-plans operation :executor]
               {}))
    (let [r (original-plan-check executor current-source)]
      (when-not (and (= plan/report-type (:type r))
                     (= plan/adapter-version (:version r))
                     (= :original-induction-and-exact-plan-transition-rechecked
                        (:checking r))
                     (= operation (:operation r))
                     (= (:abstract-operation-ids coverage-result)
                        (:catalog-operation-ids r))
                     (= (:catalog-fingerprint coverage-result)
                        (:catalog-fingerprint r))
                     (= (:induction-fingerprint coverage-result)
                        (:induction-fingerprint r))
                     (= (:certificate-fingerprint coverage-result)
                        (:certificate-fingerprint r))
                     (= (:dependency-fingerprint coverage-result)
                        (:dependency-fingerprint r))
                     (= :exact-guarded-transition-construction (:source-join r))
                     (= :original-v1251-pre-submit-checker (:plan-gate r))
                     (= :all-original-obligations-remain-open
                        (:original-obligation-status coverage-result))
                     (= (:original-open-obligation-ids-by-operation
                         coverage-result)
                        (:original-open-obligations r))
                     (= :not-established (:physical-correspondence r))
                     (= :not-established (:atomicity-assurance r))
                     (= :not-established (:physical-write-path-completeness r))
                     (= :none (:transaction-authority r))
                     (= :none (:authority r)))
        (reject! :mismatched-current-inductive-plan
                 [:inductive-plans operation] {}))
      r)))

(defn check-application-induction!
  "Recheck the CURRENT supplied assembly/route/enumerator snapshot and the
   exact induction-bound plan used by every certified route handler.

   Checks source-relative equality and issued object identities only. Never
   substitutes a copied report for proof evidence. Extra observed entry points
   are disclosed as uncovered; a missing declared route fails closed."
  [source]
  (exact-map! source input-keys [])
  (recheck-upstream!)
  (let [{:keys [authoritative-coverage-source inductive-plans]} source
        coverage-result (checked-coverage! authoritative-coverage-source)
        contract-source (:inductive-contract-source
                         authoritative-coverage-source)
        source-bound (:inductive-source contract-source)
        certified-operations (:certified-operations contract-source)
        expected (:abstract-operation-ids coverage-result)]
    (when-not (and (plain-map? inductive-plans)
                   (nil? (meta inductive-plans)))
      (reject! :invalid-inductive-plan-table [:inductive-plans] {}))
    (let [actual (set (keys inductive-plans))]
      (when-not (= expected actual)
        (reject! :incomplete-or-extra-inductive-plans [:inductive-plans]
                 {:missing (set/difference expected actual)
                  :unexpected (set/difference actual expected)})))
    (when-not (= expected (set (keys certified-operations)))
      (reject! :plan-and-certified-operation-set-mismatch
               [:authoritative-coverage-source] {}))
    (let [rows
          (into (sorted-map)
                (for [operation (sort expected)]
                  (let [{:keys [executor current-source] :as binding}
                        (get inductive-plans operation)
                        certified (get certified-operations operation)]
                    ;; Same executor must have been authenticated by v1253
                    ;; while checking its issued Ring handler, not merely a
                    ;; separately issued executor bearing the same operation ID.
                    (when-not (and (plain-map? certified)
                                   (= plan-binding-keys (set (keys certified)))
                                   (identical? executor (:executor certified))
                                   (= (:certified-plan-source current-source)
                                      (:current-source certified)))
                      (reject! :issued-handler-plan-identity-mismatch
                               [:inductive-plans operation] {}))
                    (let [checked (checked-plan! operation binding
                                                 source-bound coverage-result)]
                      [operation
                       (select-keys checked
                                    [:operation :transition-fingerprint
                                     :subject-fingerprint :source-join
                                     :plan-gate :scope :open-premises
                                     :open-obligations])]))))
          premises (reduce set/union (or (:open-premises coverage-result) #{})
                           (map :open-premises (vals rows)))
          assumptions (reduce set/union
                              (set/union extra-open
                                         (:open-obligations coverage-result))
                              (map :open-obligations (vals rows)))
          extras (:observed-unclassified-entry-point-ids coverage-result)]
      {:type report-type
       :version report-version
       :checking :fresh-induction-route-coverage-and-issued-plans-rechecked
       :scope :current-supplied-application-and-host-observation-only
       :catalog-fingerprint (:catalog-fingerprint coverage-result)
       :induction-fingerprint (:induction-fingerprint coverage-result)
       :certificate-fingerprint (:certificate-fingerprint coverage-result)
       :dependency-fingerprint (:dependency-fingerprint coverage-result)
       :abstract-operation-ids expected
       :issued-inductive-plan-ids (set (keys rows))
       :semantic-route-ids (:declared-semantic-route-ids coverage-result)
       :observed-entry-point-ids (:observed-entry-point-ids coverage-result)
       :observed-unclassified-entry-point-ids extras
       :observational-coverage :exact-for-supplied-host-snapshot
       :unclassified-entry-point-status (if (seq extras)
                                          :observed-uncovered :none-observed)
       :plan-correspondence :exact-issued-executor-and-abstract-construction
       :checked-inductive-plans rows
       :original-open-obligation-ids-by-operation
       (:original-open-obligation-ids-by-operation coverage-result)
       :original-obligation-status :all-original-obligations-remain-open
       :open-premises premises
       :open-obligations assumptions
       :abstract-invariant :source-bound-conditionally-inductive
       :physical-write-path-completeness :not-established
       :initialization-correspondence :not-established
       :abstract-to-physical-transition-refinement :not-established
       :deployed-router-adoption :not-established
       :independent-checker-soundness :not-established
       :production-certification :not-established
       :transaction-authority :none
       :authority :none})))

(defn require-current-application-induction!
  "Reject a stale or fabricated COPYABLE report by rebuilding from ORIGINAL
   current sources and their live handles. No runtime admission is issued."
  [prior current-source]
  (let [now (check-application-induction! current-source)]
    (when-not (and (plain-map? prior) (nil? (meta prior)) (= prior now))
      (reject! :stale-or-forged-application-induction-report [] {}))
    now))

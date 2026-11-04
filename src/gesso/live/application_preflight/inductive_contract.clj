(ns gesso.live.application-preflight.inductive-contract
  "v1288: source-relative join between a checked abstract induction catalog and
   a CURRENT assembled Gesso semantic route/issued-handler snapshot.

   This module does NOT generate a second operation registry. It requires the
   original abstract catalog and its two live source-bound evidence handles,
   then delegates route-to-issued-handler/plan checking to the existing v1253
   certified-contract checker. The exact set of abstract operation IDs must
   match BOTH the assembled operation summary and the issued certified-plan
   operation table. Every declared semantic route must be backed by the
   current issued handler for that operation.

   The resulting map is a COPYABLE, recheckable explanation, not a capability.
   Equal operation IDs do NOT establish that the physical transaction has the
   transition semantics of the abstract operation. Actual router adoption,
   dynamic write-path completeness, initializer coverage, physical refinement,
   and checker soundness remain open. No operation is authorized here.

   Requires a current, in-process application assembly and current plan,
   certificate and dependency handles. The trusted-JVM boundary is inherited
   from those providers; malicious replacement of compiled JVM code is outside
   this checker's threat model."
  (:require [clojure.set :as set]
            [gesso.assurance.inductive-report :as inductive]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]))

(def contract-version 1)
(def report-type
  :gesso.live.application-preflight.inductive-contract/source-relative-report)

(def ^:private input-keys
  #{:inductive-source :application-assembly :semantic-route-handlers
    :certified-operations})
(def ^:private source-keys
  #{:catalog :certificate-handle :dependency-handle :current-graph})
(def ^:private source-boundaries
  #{:abstract-to-authoritative-transition-correspondence
    :abstract-initial-state-to-physical-initializer-correspondence
    :physical-writer-and-entry-point-completeness
    :actual-router-adopts-supplied-handler-snapshot
    :post-check-router-and-handler-stability
    :induction-checker-independent-soundness})

(defn- fail! [reason at details]
  (throw (ex-info "Invalid Gesso inductive application binding."
                  (merge {:error/type ::invalid-inductive-contract
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [m expected-keys at]
  (when-not (and (plain-map? m) (nil? (meta m)))
    (fail! :expected-unannotated-source-map at {}))
  (let [actual (set (keys m))]
    (when-not (= expected-keys actual)
      (fail! :wrong-source-keys at
             {:missing (set/difference expected-keys actual)
              :unexpected (set/difference actual expected-keys)})))
  m)

(defn- qualified-operation-id? [x]
  (and (keyword? x) (some? (namespace x))))

;; Retain the original source-deriving checkers. Public Var replacement must not
;; allow a fabricated report to replace checked evidence; equivalently shaped
;; maps supplied by a sibling are not treated as capabilities. This is not an
;; independent proof of those source implementations or JVM integrity.
(def ^:private original-inductive-report inductive/build-inductive-report!)
(def ^:private original-certified-contract certified/check-certified-contracts!)

(defn- checked-sibling! [name public-var original source]
  (let [public (var-get public-var)]
    (if (identical? public original)
      (original source)
      (let [reported (public source)
            derived (original source)]
        (when-not (= reported derived)
          (fail! :substituted-sibling-checker-report [name] {}))
        derived))))

(defn- source-bound-report! [source]
  (exact-map! source source-keys [:inductive-source])
  (when-not (every? some?
                    ((juxt :certificate-handle :dependency-handle
                           :current-graph) source))
    (fail! :source-bound-induction-required [:inductive-source]
           {:message "All three current live evidence inputs are required."}))
  (let [r (checked-sibling! :inductive-report
                            #'inductive/build-inductive-report!
                            original-inductive-report source)]
    (when-not (and (= inductive/report-type (:type r))
                   (= inductive/report-version (:version r))
                   (= :source-bound-conditional-abstract-safety (:verdict r))
                   (= :original-sources-and-both-live-handles-rechecked
                      (:checking r))
                   (= :checked-exact-source-relative
                      (:source-dependency-closure r))
                   (= :in-process-conditional-abstract-certificate
                      (get-in r [:classification :certificate :status]))
                   (true? (:all-declared-invariants-conditionally-inductive? r))
                   (set? (:declared-operation-ids r))
                   (seq (:declared-operation-ids r))
                   (every? qualified-operation-id? (:declared-operation-ids r))
                   (set? (:declared-invariant-ids r))
                   (seq (:declared-invariant-ids r))
                   (= :none (:authority r))
                   (= :none (:transaction-authority r)))
      (fail! :induction-not-source-bound-and-complete
             [:inductive-source] {}))
    r))

(defn- assembled-operation-ids! [assembly]
  ;; Existing v1253 checker revalidates the current assembly, browser
  ;; artifact, issued handlers and plan executors. Recheck the same assembly
  ;; on this side too; no caller-provided operation summary is accepted.
  (when-not (application/application-assembly? assembly)
    (fail! :stale-or-invalid-application-assembly
           [:application-assembly] {}))
  (let [summary (application/operation-summary
                 (:operation-acquisition-assembly assembly))]
    (when-not (and (map? summary) (not (record? summary)) (seq summary)
                   (every? qualified-operation-id? (keys summary)))
      (fail! :invalid-assembled-operations [:application-assembly] {}))
    (set (keys summary))))

(defn check-inductive-contract!
  "Recheck a single current app/route snapshot against one source-bound,
   conditionally admitted abstract operation catalog.

   Required exact input map:
     :inductive-source         original v1286 source package with BOTH handles
     :application-assembly     current Gesso ApplicationAssembly
     :semantic-route-handlers ACTUAL route-id -> handler snapshot supplied
                              by the installing host (not a claim of adoption)
     :certified-operations     operation-id -> {:executor :current-source},
                              passed without alteration to v1253 checker

   Requires exact equality of all three declared operation sets: abstract
   catalog, assembled operation summary, and v1253 certified-plan table.
   Every semantic route must have the issued handler for its own operation.
   This checks identity/binding, NOT abstract transition refinement."
  [source]
  (exact-map! source input-keys [])
  (let [{:keys [inductive-source application-assembly semantic-route-handlers
                certified-operations]} source
        abstract (source-bound-report! inductive-source)
        abstract-ids (:declared-operation-ids abstract)
        assembled-ids (assembled-operation-ids! application-assembly)]
    (when-not (= abstract-ids assembled-ids)
      (fail! :abstract-assembly-operation-set-mismatch
             [:application-assembly]
             {:missing-from-assembly (set/difference abstract-ids assembled-ids)
              :outside-abstract-catalog (set/difference assembled-ids abstract-ids)}))
    ;; This table is supplied by the host and every value is authenticated by
    ;; v1253's current issued-plan verifier. Exact catalog coverage is required
    ;; here, whereas the generic v1253 checker legitimately allows subsets.
    (when-not (and (plain-map? certified-operations)
                   (nil? (meta certified-operations)))
      (fail! :invalid-certified-operation-table [:certified-operations] {}))
    (let [certified-ids (set (keys certified-operations))]
      (when-not (= abstract-ids certified-ids)
        (fail! :abstract-certified-operation-set-mismatch
               [:certified-operations]
               {:uncertified (set/difference abstract-ids certified-ids)
                :outside-abstract-catalog
                (set/difference certified-ids abstract-ids)})))
    ;; Do not accept a copied :valid? report or a caller's claimed handler
    ;; descriptions. The underlying checker reexamines the issued callables.
    (let [routes (checked-sibling!
                  :certified-contract
                  #'certified/check-certified-contracts!
                  original-certified-contract
                  {:application-assembly application-assembly
                   :semantic-route-handlers semantic-route-handlers
                   :certified-operations certified-operations})
          certified-rows (:certified-operations routes)
          route-ids (:route-ids routes)
          matched (:matched-certified-route-ids routes)]
      (when-not (and (= certified/report-type (:type routes))
                     (= certified/adapter-version (:version routes))
                     (= :current-source-relative-semantic-route-bindings
                        (:checking routes))
                     (= abstract-ids (set (keys certified-rows)))
                     (empty? (:uncertified-operations routes))
                     (set? route-ids) (seq route-ids)
                     (= route-ids matched)
                     (= route-ids
                        (into #{} (mapcat :route-ids (vals certified-rows))))
                     (= :not-established (:physical-operation-correspondence routes))
                     (= :none (:transaction-authority routes)))
        (fail! :certified-route-contract-not-exact
               [:semantic-route-handlers] {}))
      ;; Keep the original, potentially heterogeneous proof obligations in a
      ;; *set*, never discharge them because a more limited identity check ran.
      {:type report-type
       :version contract-version
       :checking :current-abstract-source-and-issued-route-snapshot-rechecked
       :catalog-id (:catalog-id abstract)
       :induction-fingerprint (get-in abstract [:identity :induction-fingerprint])
       :catalog-fingerprint (get-in abstract [:identity :catalog-fingerprint])
       :certificate-fingerprint
       (get-in abstract [:identity :certificate-fingerprint])
       :dependency-fingerprint
       (get-in abstract [:identity :dependency-fingerprint])
       :abstract-operation-ids abstract-ids
       :assembled-operation-ids assembled-ids
       :certified-operation-ids (set (keys certified-rows))
       :semantic-route-ids route-ids
       :operation-route-ids
       (into (sorted-map)
             (map (fn [[op row]] [op (:route-ids row)]))
             certified-rows)
       :binding :exact-operation-identity-and-issued-handler-to-plan
       :original-open-obligation-ids-by-operation
       (get-in abstract [:classification :original-obligations :by-operation])
       :original-obligation-status :all-original-obligations-remain-open
       :scope :current-supplied-application-route-snapshot-only
       :open-premises (set/union (get-in abstract
                                           [:classification :certificate :open-premises])
                                 (:open-premises routes))
       :open-obligations
       (set/union source-boundaries
                  (get-in abstract [:classification :open :assumptions])
                  (:open-obligations routes))
       :abstract-invariant :conditionally-certified-for-declared-machine
       :abstract-to-physical-transition :not-established
       :physical-writer-completeness :not-established
       :deployed-router-adoption :not-established
       :initialization-correspondence :not-established
       :proof-kernel-independent-soundness :not-established
       :production-certification :not-established
       :transaction-authority :none
       :authority :none})))

(defn require-current-inductive-contract!
  "Recheck every live input and reject stale or fabricated COPYABLE reports.
   The return value is a fresh explanation, never a reusable capability."
  [prior current-source]
  (let [current (check-inductive-contract! current-source)]
    (when-not (and (plain-map? prior) (nil? (meta prior))
                   (= prior current))
      (fail! :stale-or-forged-inductive-contract [:report] {}))
    current))

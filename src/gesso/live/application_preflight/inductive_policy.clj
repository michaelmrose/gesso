(ns gesso.live.application-preflight.inductive-policy
  "v1315: OPT-IN, source-relative application startup gate.

   Compatibility mode preserves the old behavior without dereferencing any
   source or demanding new assurance configuration:
     (require-inductive-policy! nil)
     (require-inductive-policy! {:mode :disabled})

   Strict mode requires an ORIGINAL v1294 application source:
     (require-inductive-policy!
       {:mode :strict
        :application-source <current live v1294 input>
        :required #{:source-bound-conditional-induction
                    :exact-issued-handler-and-plan-binding
                    :no-observed-unclassified-entry-points}})

   The first two requirements are MANDATORY in every strict policy. The third
   is an OPTIONAL assertion about the host-supplied current inventory only;
   even if satisfied it DOES NOT establish complete physical writer coverage.

   This gate is an application *startup eligibility check*, not a certificate,
   proof of active router adoption, independent soundness, or authorization to
   submit XTDB transactions. The host must invoke require-inductive-policy!
   at its actual assembly/install boundary and must not reuse its COPYABLE
   report as a credential. Checking one snapshot cannot protect against later
   handler replacement or undisclosed writers. Existing Gesso startup does not
   automatically invoke this opt-in module."
  (:require [clojure.set :as set]
            [gesso.assurance.application-induction :as application]))

(def policy-version 1)
(def policy-type
  :gesso.live.application-preflight.inductive-policy/source-relative-startup-gate)

(def ^:private mandatory
  #{:source-bound-conditional-induction
    :exact-issued-handler-and-plan-binding})
(def ^:private optional
  #{:no-observed-unclassified-entry-points})
(def ^:private unavailable
  #{:authenticated-physical-observation
    :complete-physical-writer-coverage
    :universal-xtdb-refinement
    :independent-induction-checker-soundness
    :production-certification
    :deployed-router-adoption
    :runtime-transaction-authorization})
(def ^:private recognized (set/union mandatory optional unavailable))
(def ^:private strict-keys #{:mode :application-source :required})
(def ^:private compatibility-keys #{:mode})
(def ^:private fingerprint-keys
  [:catalog-fingerprint :induction-fingerprint :certificate-fingerprint
   :dependency-fingerprint])
(def ^:private original-application-check application/check-application-induction!)

(defn- reject! [reason at details]
  (throw (ex-info "Invalid Gesso inductive application policy."
                  (merge {:error/type ::invalid-inductive-policy
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map! [x expected at]
  (when-not (and (plain-map? x) (nil? (meta x)))
    (reject! :expected-unannotated-plain-map at {}))
  (let [keys-found (set (keys x))]
    (when-not (= keys-found expected)
      (reject! :wrong-policy-keys at
               {:missing (set/difference expected keys-found)
                :unexpected (set/difference keys-found expected)})))
  x)

(defn- stable-first [s] (first (sort-by pr-str s)))

(defn- report [mode decision required blocker application-report]
  (let [open (set/union
              #{:independent-induction-checker-soundness
                :host-authoritative-entry-point-inventory-is-complete
                :non-route-xtdb-writers-are-covered
                :deployed-router-and-executors-adopt-checked-snapshot
                :post-check-runtime-bindings-remain-stable
                :physical-prestate-and-initialization-correspondence
                :abstract-to-physical-transition-refinement}
              (or (:open-obligations application-report) #{}))]
    {:type policy-type
     :version policy-version
     :mode mode
     :decision decision
     :checking (if (= mode :disabled)
                 :not-requested-backwards-compatible
                 :fresh-original-application-induction-rechecked)
     :required required
     :first-blocker blocker
     :scope (if (= mode :disabled)
              :legacy-behavior-unchanged
              :one-current-host-supplied-application-and-entry-point-snapshot)
     :source-fingerprints (select-keys application-report fingerprint-keys)
     :abstract-operation-ids (or (:abstract-operation-ids application-report) #{})
     :observed-unclassified-entry-point-ids
     (or (:observed-unclassified-entry-point-ids application-report) #{})
     :original-open-obligation-ids-by-operation
     (or (:original-open-obligation-ids-by-operation application-report) {})
     :open-premises (or (:open-premises application-report) #{})
     :open-obligations open
     :startup-installation (if (= mode :disabled)
                             :not-required
                             :host-must-invoke-and-honor-gate)
     :post-check-runtime-binding-stability :not-established
     :physical-write-path-completeness :not-established
     :authenticated-physical-correspondence :not-established
     :universal-xtdb-refinement :not-established
     :independent-checker-soundness :not-established
     :production-certification :not-established
     :deployed-router-adoption :not-established
     :transaction-authority :none
     :authority :none}))

(defn- required! [required]
  (when-not (and (set? required) (nil? (meta required))
                 (every? keyword? required))
    (reject! :invalid-required-guarantees [:required] {}))
  (let [unknown (set/difference required recognized)
        missing (set/difference mandatory required)
        impossible (set/intersection required unavailable)]
    (when (seq unknown)
      (reject! :unknown-assurance-requirement [:required]
               {:unknown unknown}))
    ;; This is a declaration error, not a check that we can optimistically
    ;; accept. Gesso cannot offer these guarantees at any strict policy level.
    (when (seq impossible)
      (reject! :unavailable-assurance-requirement [:required]
               {:unavailable impossible
                :message "No currently implemented checker establishes this guarantee."}))
    (when (seq missing)
      (reject! :missing-mandatory-guarantees [:required]
               {:missing missing})))
  required)

(defn- checked-application! [source]
  ;; Never let a replacement of the public source checker inject a fabricated
  ;; success report. As elsewhere in Gesso, hostile mutation of private JVM
  ;; state or bytecode is outside this in-process boundary.
  (when-not (identical? original-application-check
                        (var-get #'application/check-application-induction!))
    (reject! :substituted-upstream-checker [:application-source] {}))
  (let [r (original-application-check source)
        operations (:abstract-operation-ids r)]
    (when-not (and (= application/report-type (:type r))
                   (= application/report-version (:version r))
                   (= :fresh-induction-route-coverage-and-issued-plans-rechecked
                      (:checking r))
                   (= :source-bound-conditionally-inductive (:abstract-invariant r))
                   (= :exact-issued-executor-and-abstract-construction
                      (:plan-correspondence r))
                   (set? operations) (seq operations)
                   (= operations (:issued-inductive-plan-ids r))
                   (= operations (:semantic-route-ids r))
                   (set? (:observed-unclassified-entry-point-ids r))
                   (= :exact-for-supplied-host-snapshot (:observational-coverage r))
                   (= :all-original-obligations-remain-open
                      (:original-obligation-status r))
                   (= :not-established (:physical-write-path-completeness r))
                   (= :not-established (:abstract-to-physical-transition-refinement r))
                   (= :not-established (:independent-checker-soundness r))
                   (= :not-established (:deployed-router-adoption r))
                   (= :none (:authority r) (:transaction-authority r)))
      (reject! :unexpected-source-relative-assurance-contract
               [:application-source] {}))
    r))

(defn check-inductive-policy!
  "Reevaluate the ORIGINAL source for this opt-in policy. Returns a COPYABLE,
   NON-AUTHORITATIVE explanation. :eligible-current-snapshot means only that
   this particular source-relative snapshot passed the declared *supported*
   checks; it is neither a reusable admission token nor execution permission.

   Disabled policy (nil or {:mode :disabled}) does not invoke proof machinery.
   Strict policy is EXACTLY {:mode :strict :application-source original
                             :required #{both mandatory guarantees, ...}}.
   Unsupported physical or soundness claims fail closed as typed policy errors.
   Missing or forged original evidence fails through the original source checker.
   When optional inventory classification fails, return :blocked and an exact
   first-blocker without confusing it with universal inventory completeness."
  [request]
  (cond
    (nil? request)
    (report :disabled :compatibility #{} nil nil)

    (and (plain-map? request) (nil? (meta request))
         (= :disabled (:mode request)))
    (do (exact-map! request compatibility-keys [])
        (report :disabled :compatibility #{} nil nil))

    :else
    (do
      (exact-map! request strict-keys [])
      (when-not (= :strict (:mode request))
        (reject! :unsupported-policy-mode [:mode] {:mode (:mode request)}))
      (let [{:keys [required application-source]} request
            _ (required! required)
            checked (checked-application! application-source)
            extras (:observed-unclassified-entry-point-ids checked)
            inventory-required? (contains? required
                                          :no-observed-unclassified-entry-points)
            blocker (when (and inventory-required? (seq extras))
                      {:class :entry-point-coverage
                       :code :observed-unclassified-entry-point
                       :at [:application-source :authoritative-coverage-source
                            :enumerate-entry-points!]
                       :entry-point-id (stable-first extras)})]
        (report :strict (if blocker :blocked :eligible-current-snapshot)
                required blocker checked)))))

(defn require-inductive-policy!
  "Enforce the policy NOW at a host-controlled startup/install boundary.
   Reruns every source check; does NOT accept a previously returned report.
   Throws a typed failure for observed-unclassified entries or invalid source,
   missing original evidence, unknown requirements, and unavailable proofs.
   Calling this once does not establish subsequent deployment stability."
  [request]
  (let [current (check-inductive-policy! request)]
    (when (= :blocked (:decision current))
      (reject! :strict-assurance-requirement-unsatisfied
               (get-in current [:first-blocker :at])
               {:first-blocker (:first-blocker current)
                :required (:required current)}))
    current))

(ns gesso.assurance.report
  "Root, source-bound assurance accounting for one Gesso proof-linked operation.

   v1257 is a REPORTING boundary, not an independent theorem prover, runtime
   authorization, test-runner receipt verifier, or production deployment gate.
   It rechecks the exact v1251 in-process executor and its original v1237
   conditional certificate and v1241 obligation declarations. An optional
   current v1253 ApplicationAssembly/handler snapshot is checked rather than
   trusting a copyable assembly report. Test observations are retained ONLY
   as caller-reported observations: no EDN map can authenticate a test run.

   (build-report!
    {:executor <issued v1251 callable>
     :current-source <fresh v1251 original source>
     :obligation-source <original v1241 obligation declarations>
     :application-binding nil ; OR original options for v1253 checker
     :test-observations []})

   :proved refers exclusively to a CONDITIONAL, source-relative abstract
   derivation. The matching-transition premise, initial-state and invariant
   obligations, kernel meta-soundness, physical effect/callback semantics,
   deployed route adoption, and independent test evidence are NOT discharged.
   A report fingerprint is a change detector over independently rechecked
   descriptions, not a credential or a transferable proof. A report can never
   authorize a command, assert full application certification, or replace a
   live certificate, registered executor, or an installed router.

   Calling the checker with an obsolete 'current' source/router snapshot proves
   only facts relative to that snapshot. This module cannot discover changes
   external to the supplied trusted boundaries or verify arbitrary Clojure."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.subject :as subject]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as route]
            [gesso.model.certified-plan :as plan]))

(def report-version 1)
(def report-type :gesso.assurance.report/source-bound-root-report)
(def ^:private report-id :gesso.assurance.report/checked-root-v1)
(def ^:private required-keys
  #{:executor :current-source :obligation-source
    :application-binding :test-observations})
(def ^:private observation-keys
  #{:id :scope :result :reference})
(def ^:private observation-scopes
  #{:embedded-xtdb :real-browser :node-cljs :jvm :other})
(def ^:private unsupported-claims
  #{:arbitrary-clojure-function-correctness
    :universal-xtdb-implementation-refinement
    :universal-invariant-preservation-from-transition-facts
    :automatically-complete-business-policy-catalog
    :physical-deployment-attestation-from-route-snapshot})
(def ^:private reporting-assumptions
  #{:current-plan-source-is-obtained-from-actual-assembly
    :current-application-routes-are-obtained-from-active-installation
    :all-authoritative-effect-paths-are-enumerated
    :opaque-callbacks-and-external-state-are-trusted
    :in-process-checker-and-jvm-boundary-is-trusted})

(defn- reject!
  [reason at details]
  (throw (ex-info "Invalid Gesso root assurance report source."
                  (merge {:error/type ::invalid-report
                          :reason reason :at at}
                         details))))

(defn- plain-map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn- exact-map!
  [x required at]
  (when-not (plain-map? x)
    (reject! :expected-plain-map at {}))
  (when (some? (meta x))
    (reject! :metadata-not-allowed at {}))
  (let [actual (set (keys x))]
    (when-not (= required actual)
      (reject! :wrong-keys at
               {:missing (set/difference required actual)
                :unexpected (set/difference actual required)})))
  x)

(defn- qualified-id? [x]
  (and (keyword? x) (some? (namespace x))))

(defn- checked-observations!
  [observations]
  (when-not (and (vector? observations)
                 (nil? (meta observations))
                 (<= (count observations) 64))
    (reject! :invalid-test-observations [:test-observations] {}))
  (doseq [[i observation] (map-indexed vector observations)]
    (exact-map! observation observation-keys [:test-observations i])
    (when-not (qualified-id? (:id observation))
      (reject! :invalid-test-observation-id [:test-observations i :id] {}))
    (when-not (contains? observation-scopes (:scope observation))
      (reject! :invalid-test-observation-scope
               [:test-observations i :scope] {}))
    (when-not (#{:passed :failed :inconclusive} (:result observation))
      (reject! :invalid-test-observation-result
               [:test-observations i :result] {}))
    (when-not (and (string? (:reference observation))
                   (not (str/blank? (:reference observation)))
                   (<= (count (:reference observation)) 512))
      (reject! :invalid-test-observation-reference
               [:test-observations i :reference] {})))
  (when-not (= (count observations) (count (set (map :id observations))))
    (reject! :duplicate-test-observation-id [:test-observations] {}))
  observations)

(defn- checked-assembly!
  [application-binding operation executor current-source]
  (when application-binding
    ;; Do not take an assembly *report* as evidence. Re-run the original
    ;; assembly/handler checker against original live objects and sources.
    (let [result (route/check-certified-contracts! application-binding)
          bound (get-in result [:certified-operations operation])]
      (when-not bound
        (reject! :operation-not-covered-by-checked-assembly
                 [:application-binding :certified-operations operation] {}))
      (let [binding (get-in application-binding [:certified-operations operation])]
        (when-not (and (identical? executor (:executor binding))
                       (= current-source (:current-source binding)))
          (reject! :assembly-operation-source-mismatch
                   [:application-binding :certified-operations operation] {})))
      result)))

(defn- sorted-names [xs]
  (vec (sort-by pr-str xs)))

(defn build-report!
  "Recheck live original inputs and issue an honest, COPYABLE root report.

   :test-observations is a bounded vector of descriptions, NOT authenticated
   test evidence. :application-binding is either nil or the exact original
   options accepted by v1253/check-certified-contracts!. A caller-provided
   :valid?, :proved?, :certified?, or copied report is not accepted.

   This module always returns :full-success? false: no supported input yet
   establishes the missing universal proof and production correspondence.
   Use :classification to distinguish the narrower established facts."
  [source]
  (exact-map! source required-keys [])
  (let [{:keys [executor current-source obligation-source
                application-binding test-observations]} source
        observations (checked-observations! test-observations)
        _ (when-not (plan/checked-plan-operation? executor)
            (reject! :unregistered-certified-executor [:executor] {}))
        ;; Freshness checks certificate, DAG, callback identities and current
        ;; model declaration. The returned report is not accepted as input.
        _ (plan/require-current-plan-operation! executor current-source)
        plan-description (plan/describe-plan-operation executor)
        operation (:operation plan-description)
        _ (when-not (= (:construction current-source)
                       (:construction obligation-source))
            (reject! :obligation-construction-mismatch
                     [:obligation-source :construction] {}))
        obligations (obligation/derive-obligations! obligation-source)
        _ (when-not (= (:transition-fingerprint obligations)
                       (:transition-fingerprint plan-description))
            (reject! :obligation-transition-mismatch
                     [:obligation-source] {}))
        certified-source (:certified-source current-source)
        certificate-handle (:certificate certified-source)
        _ (certificate/require-current-certificate!
           certificate-handle (:proof-source certified-source)
           (:current-graph certified-source))
        proof (certificate/describe-certificate certificate-handle)
        _ (when-not (and (= :guarded-transition (:proof-kind proof))
                         (= :matching-abstract-transition-only (:scope proof))
                         (= :source-relative-abstract-proof-only
                            (:certificate-scope proof))
                         (= :checked-conditional-abstract-derivation
                            (:proof-status proof))
                         (= :none (:authority proof))
                         (= :none (:authority plan-description))
                         (= (:subject-fingerprint obligations)
                            (:subject-fingerprint proof))
                         (= (:fingerprint proof)
                            (:proof-fingerprint plan-description)))
            (reject! :proof-plan-identity-mismatch [:executor] {}))
        assembly (checked-assembly! application-binding operation
                                     executor current-source)
        ;; The v1253 report does not include physical artifact bytes. Recheck
        ;; the actual assembly once more to bind this report identity to the
        ;; current verified browser artifact digest, not just route IDs.
        artifact-report (when assembly
                          (application/check-application-assembly
                           (select-keys
                            (:application-assembly application-binding)
                            [:name :operation-acquisition-assembly
                             :browser-artifact-path :browser-receipt-path
                             :rendered-surfaces])))
        _ (when (and assembly (not (true? (:valid? artifact-report))))
            (reject! :application-artifact-is-not-current
                     [:application-binding :application-assembly] {}))
        artifact-digest (get-in artifact-report
                                [:analysis :artifact-receipt :artifact :digest])
        _ (when (and assembly (not (string? artifact-digest)))
            (reject! :missing-physical-artifact-identity
                     [:application-binding :application-assembly] {}))
        open-premises (set/union (:open-premises proof)
                                 (:open-premises plan-description)
                                 (if assembly (:open-premises assembly) #{}))
        open-obligations
        (set/union (:open-obligations proof)
                   (:open-obligations plan-description)
                   (:open-obligation-ids obligations)
                   (if assembly (:open-obligations assembly) #{})
                   #{:source-to-physical-xtdb-correspondence
                     :invariant-preservation-not-yet-proved
                     :test-evidence-authentication-not-established})
        trusted (set/union reporting-assumptions
                           (if assembly (:open-assumptions assembly) #{}))
        facts {:operation operation
               :certificate-fingerprint (:fingerprint proof)
               :proof-source-fingerprint (:source-fingerprint proof)
               :proof-subject-fingerprint (:subject-fingerprint proof)
               :dependency-graph-fingerprint
               (:dependency-graph-fingerprint proof)
               :transition-fingerprint (:transition-fingerprint obligations)
               :obligation-fingerprint (:fingerprint obligations)
               :checked-assembly-route-ids
               (if assembly (:matched-certified-route-ids assembly) #{})
               :checked-assembly-operations
               (if assembly (set (keys (:certified-operations assembly))) #{})
               :test-observations observations
               :physical-artifact-digest artifact-digest
               :open-premises open-premises
               :open-obligations open-obligations
               :trusted-assumptions trusted
               :unsupported-claims unsupported-claims}
        identity (subject/describe-subject
                  {:kind :rule :id report-id :interface facts})]
    {:type report-type
     :version report-version
     :checking :original-sources-and-live-handles-rechecked
     :operation operation
     :identity {:report-fingerprint (:fingerprint identity)
                :certificate-fingerprint (:fingerprint proof)
                :proof-source-fingerprint (:source-fingerprint proof)
                :proof-subject-fingerprint (:subject-fingerprint proof)
                :dependency-graph-fingerprint
                (:dependency-graph-fingerprint proof)
                :transition-fingerprint (:transition-fingerprint obligations)
                :obligation-fingerprint (:fingerprint obligations)
                :artifact {:kind :application-route-snapshot
                           :status (if assembly :checked-source-relative
                                       :not-checked)
                           :browser-artifact-digest artifact-digest
                           :route-ids (if assembly
                                        (:matched-certified-route-ids assembly)
                                        #{})
                           :physical-deployment :not-attested}}
     :classification
     {:proved {:status :checked-conditional-abstract-derivation
               :scope :matching-abstract-transition-only
               :open-premises open-premises
               :invariant-preservation :not-proved
               :checker-meta-soundness :not-established}
      :runtime-enforced
      {:status :issued-pre-submit-abstract-plan-gate
       :checks #{:exact-mapped-domain-write
                 :guard-and-assignment-observation
                 :complete-physical-field-projection
                 :framed-fields-preserved-before-submit}
       :physical-commit-and-authorization :not-established}
      :tested {:status :caller-reported-only
               :authenticated? false
               :observations observations
               :claim :no-universal-production-correspondence}
      :trusted {:status :unverified-premises
                :assumptions trusted}
      :open {:premises open-premises
             :obligations open-obligations
             :initial-and-preservation
             (select-keys (:obligations obligations)
                          [:initial-state :preservation])}
      :unsupported {:status :not-established
                    :claims unsupported-claims}}
     :application {:status (if assembly
                             :source-relative-route-checked
                             :not-checked)
                   :certified-operation-ids (if assembly
                                               (set (keys (:certified-operations assembly)))
                                               #{})
                   :uncertified-operation-ids (if assembly
                                                 (:uncertified-operations assembly)
                                                 #{})
                   :deployment-adoption :not-established}
     :verdict :partial-conditional-assurance
     :full-success? false
     :production-certification :not-established
     :transaction-authority :none
     :certificate :not-issued}))

(defn format-report
  "Readable summary of a freshly generated root report. A copy of this text
   is informational only; it is NEVER a certificate or authorization input."
  [report]
  (when-not (and (plain-map? report)
                 (= report-type (:type report))
                 (= report-version (:version report)))
    (reject! :invalid-report-for-format [:report] {}))
  ;; Formatting is a convenience, not an authenticity check. Explicitly warn
  ;; readers that copied report data cannot validate proof or deployment.
  (str/join
   "\n"
   (concat
    [(str "Gesso assurance report: " (:operation report))
     (str "Identity: " (get-in report [:identity :report-fingerprint]))
     (str "Artifact: " (get-in report [:identity :artifact :status]))
     (str "Verified browser artifact digest: "
          (or (get-in report [:identity :artifact :browser-artifact-digest])
              "not checked"))
     "PROVED: conditional abstract derivation only; invariants NOT proved"
     "RUNTIME-ENFORCED: issued pre-submit plan gate; physical commit unproved"
     (str "TESTED: " (count (get-in report [:classification :tested :observations]))
          " caller-reported observations; not authenticated here")
     "TRUSTED: unresolved environmental and callback assumptions"
     "OPEN: current checker premises and production/initial/invariant obligations"
     "UNSUPPORTED: general Clojure correctness and universal refinement"
     "FULL SUCCESS: false; production certification NOT established"]
    [(str "Open premises: "
          (pr-str (sorted-names (get-in report [:classification :open :premises]))))
     (str "Open obligations: "
          (pr-str (sorted-names (get-in report [:classification :open :obligations]))))
     (str "Trusted assumptions: "
          (pr-str (sorted-names (get-in report [:classification :trusted :assumptions]))))
     (str "Unsupported claims: "
          (pr-str (sorted-names (get-in report [:classification :unsupported :claims]))))
     "Note: this is a copyable informational report, never a credential."])))

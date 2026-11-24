(ns gesso.assurance.release.operation-source
  "v2263: Source-RECOMPUTED candidate checks for one original operation catalog.

   Connect v2259 selected-release diagnostics to the actual v1680 original
   operation-set checker. The initial inventory, each candidate observation,
   positive gate, each individually omitted-declaration mutation, and final
   inventory are acquired through
   the supplied ORIGINAL catalog factory and the original source checker,
   rather than through user-provided success maps or a copied proof report.

   The factory is a host-selected *declared abstract catalog*, not an
   authenticated installed-app inventory. This adapter therefore evaluates
   ONLY that selected catalog's candidate source gates. Installation has no
   observed physical subject and MUST block; activation and completion MUST
   remain unexamined. An all-green release is impossible through this API.
   It never executes application commands, joins actual writers, issues a
   certificate, grants runtime effects, or certifies the completeness of an
   application's original source universe. Arbitrary in-process code mutation
   and untrusted catalog factories are outside its authority claims."
  (:require [gesso.assurance.proof.operation-set :as operation-set]
            [gesso.assurance.release.decision :as decision]))

(def report-type
  :gesso.assurance.release.operation-source/selected-catalog-diagnostic)
(def report-version 1)

(def ^:private original-operation-check operation-set/check-operation-set!)
(def ^:private source-class :gesso.release.source/selected-original-catalog)
(def ^:private source-mechanism :gesso.assurance.proof/operation-set)
(def ^:private source-version "operation-set-v1")
(def ^:private source-premise :gesso.release.premise/source-obligations-open)
(def ^:private source-scope :gesso.release/declared-abstract-catalog-only)
(def ^:private catalog-requirement :gesso.release.requirement/catalog-source)
(def ^:private installation-requirement :gesso.release.requirement/installation)
(def ^:private activation-requirement :gesso.release.requirement/activation)
(def ^:private completion-requirement :gesso.release.requirement/completion)

(defn- reject! [reason]
  (throw (ex-info "Selected release source is not established."
                  {:release-operation-source/rejection true :reason reason})))

(defn- current-catalog! [catalog!]
  (when-not (identical? original-operation-check
                        (var-get #'operation-set/check-operation-set!))
    (reject! :original-operation-checker-replaced))
  (let [catalog (catalog!)
        checked (original-operation-check catalog)
        ids (:declared-operation-ids checked)
        invariants (:invariant-ids checked)]
    (when-not (and (= operation-set/report-type (:type checked))
                   (= operation-set/operation-set-version (:version checked))
                   (= :declared-abstract-operation-set-checked
                      (:proof-status checked))
                   (= :not-issued (:certificate checked))
                   (= :not-admitted (:admission checked))
                   (= :none (:authority checked))
                   (string? (:fingerprint checked))
                   (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint checked))
                   (= ids (set (map :operation-id (:operation-results checked))))
                   (= invariants (set (keys (:invariants catalog))))
                   (seq ids) (seq invariants))
      (reject! :original-operation-report-not-source-checked))
    {:source catalog :checked checked}))

(defn- gate [phase id requirement kind mechanism scope boundary premises]
  {:id id :requirement requirement :kind kind
   :class (if (= :candidate phase) :structural :qualification)
   :mechanism mechanism :mechanism-version source-version :scope scope
   :allowed-premises premises :expected-boundary boundary})

(defn- positive-source-gate [index requirement]
  (gate :candidate
        (keyword "gesso.release.source" (format "declared-%03d" index))
        requirement :positive source-mechanism source-scope nil
        #{source-premise}))

(defn- negative-source-gate [suffix requirement]
  (gate :candidate
        (keyword "gesso.release.source" suffix)
        requirement :negative source-mechanism source-scope
        :gesso.release.source/omitted-original-declaration-detected #{}))

(defn- pending-gates [phase requirement]
  (let [mechanism :gesso.release/physical-observation-not-configured
        scope :gesso.release/installed-physical-boundary]
    [(gate phase
           (keyword "gesso.release.physical" (str (name phase) "-positive"))
           requirement :positive mechanism scope nil #{})
     (gate phase
           (keyword "gesso.release.physical" (str (name phase) "-negative"))
           requirement :negative mechanism scope
           :gesso.release.physical/adversarial-effect-prevented #{})]))

(defn- selected-intent [checked]
  (let [operations (vec (sort-by str (:declared-operation-ids checked)))
        invariants (vec (sort-by str (:invariant-ids checked)))
        requirements (into {catalog-requirement source-class
                            installation-requirement :gesso.release/installed-route
                            activation-requirement :gesso.release/installed-browser
                            completion-requirement :gesso.release/installed-release}
                           (concat (map-indexed (fn [i op]
                                                  [(keyword "gesso.release.operation"
                                                            (format "entry-%03d" i)) op])
                                                operations)
                                   (map-indexed (fn [i inv]
                                                  [(keyword "gesso.release.invariant"
                                                            (format "entry-%03d" i)) inv])
                                                invariants)))
        source-requirements (vec (concat
                                  (map-indexed (fn [i _]
                                                 (keyword "gesso.release.operation"
                                                          (format "entry-%03d" i)))
                                               operations)
                                  (map-indexed (fn [i _]
                                                 (keyword "gesso.release.invariant"
                                                          (format "entry-%03d" i)))
                                               invariants)))
        candidate (vec (concat
                         (map-indexed positive-source-gate source-requirements)
                         [(positive-source-gate (count source-requirements)
                                                catalog-requirement)
                          (negative-source-gate "omitted-operation"
                                                catalog-requirement)
                          (negative-source-gate "omitted-invariant"
                                                catalog-requirement)]))
        fingerprint (:fingerprint checked)]
    {:claim-id :gesso.release/selected-abstract-source-check
     :application-id (:catalog-id checked)
     :milestone :gesso.release/selected-candidate-source-only
     :profile-id :gesso.release/abstract-source-only
     :capabilities (set (vals requirements))
     :requirements requirements
     :subjects (into {}
                     (for [phase decision/phases]
                       [phase (str (name phase) "/" fingerprint)]))
     :gates {:candidate candidate
             :installation (pending-gates :installation installation-requirement)
             :activation (pending-gates :activation activation-requirement)
             :completion (pending-gates :completion completion-requirement)}}))

(defn- structural-inventory [checked]
  (let [intent (selected-intent checked)]
    {:capabilities (:capabilities intent)
     :requirements (set (keys (:requirements intent)))
     :gates (into #{} (map :id) (mapcat identity (vals (:gates intent))))}))

(def ^:private operation-set-invalid
  :gesso.assurance.proof.operation-set/invalid-operation-set)
(def ^:private obligation-source-invalid
  :gesso.assurance.proof.obligation/invalid-obligation-source)

(defn- expected-omission-rejection?
  "Recognize ONLY the validation failure forced by the missing declaration.
   A source-checker failure elsewhere is not evidence of omission detection."
  [source kind data]
  (let [type (:error/type data)
        reason (:reason data)
        at (:at data)]
    (case kind
      :operation (and (= operation-set-invalid type)
                      (= [:operations] at)
                      (if (= 1 (count (:operations source)))
                        (= :invalid-operation-sequence reason)
                        (= :operation-id-coverage-mismatch reason)))
      :invariant (and (= 1 (count (:invariants source)))
                      (= obligation-source-invalid type)
                      (= :invariant-count reason)
                      (= [:invariants] at))
      false)))

(defn- omission-detected? [source original-check kind id]
  (let [mutant (case kind
                 :operation (update source :operations
                                    (fn [entries]
                                      (into [] (remove #(= id (:id %))) entries)))
                 :invariant (update source :invariants dissoc id)
                 (reject! :unknown-mutation-kind))]
    (when (= source mutant)
      (reject! :original-declaration-mutation-did-not-change-source))
    (try
      (let [checked (original-operation-check mutant)]
        (and (= operation-set/report-type (:type checked))
             (= operation-set/operation-set-version (:version checked))
             (= :declared-abstract-operation-set-checked
                (:proof-status checked))
             (= :not-issued (:certificate checked))
             (= :not-admitted (:admission checked))
             (= :none (:authority checked))
             (not= (:fingerprint original-check)
                   (:fingerprint checked))
             (string? (:fingerprint checked))
             (re-matches #"sha256:[0-9a-f]{64}" (:fingerprint checked))))
      (catch clojure.lang.ExceptionInfo error
        ;; In a singleton-invariant catalog the omission is rejected by the
        ;; original obligation checker (before operation-set checks its
        ;; invariants). That SPECIFIC rejection is positive mutation evidence.
        ;; A missing operation instead fails operation-set's source coverage.
        ;; All unrelated exceptions are still rethrown / fail closed.
        (if (expected-omission-rejection? source kind (ex-data error))
          true
          (throw error))))))

(defn- omitted-declaration-detected? [source original-check kind]
  (let [ids (case kind
              :operation (sort-by str (map :id (:operations source)))
              :invariant (sort-by str (keys (:invariants source)))
              (reject! :unknown-mutation-kind))
        original-now (original-operation-check source)]
    ;; A failure of the ORIGINAL source must never be mistaken for successful
    ;; rejection of its mutant; its identity is checked before the try/catch.
    (when-not (and (= operation-set/report-type (:type original-now))
                   (= operation-set/operation-set-version (:version original-now))
                   (= (:fingerprint original-check)
                      (:fingerprint original-now))
                   (= (:declared-operation-ids original-check)
                      (:declared-operation-ids original-now))
                   (= (:invariant-ids original-check)
                      (:invariant-ids original-now)))
      (reject! :original-source-changed-before-negative-control))
    ;; One negative gate is retained for each kind, but it is all-declarations
    ;; coverage, not a first-entry sample. Short-circuit on any undetected ID.
    (and (seq ids)
         (every? #(omission-detected? source original-now kind %) ids))))

(defn- evidence [intent phase gate subject premises]
  {:status :passed
   :claim-id (:claim-id intent)
   :phase phase
   :gate-id (:id gate)
   :requirement-id (:requirement gate)
   :subject-id (:subject-id subject)
   :mechanism (:mechanism gate)
   :mechanism-version (:mechanism-version gate)
   :scope (:scope gate)
   :evidence-id (str (:subject-id subject) "/" (:id gate))
   :premises premises
   :observed-boundary (:expected-boundary gate)
   :forbidden-effects (when (= :negative (:kind gate)) :absent)})

(defn assess-original-catalog!
  "Reevaluate one *host-selected original catalog factory* without accepting
   downstream user-supplied inventory, subject or gate success callbacks.

   Candidate positive gates execute the production original-operation checker
   against a freshly fetched catalog with exact frozen source identity.
   Negative gates independently remove EVERY declared operation and invariant
   and require the original checker to reject the relevant omission or
   reidentify the changed source. Expected lower-layer structural rejections
   count as detected mutations, not unavailable evidence; unrelated failures
   never count as successful adversarial controls.
   This does NOT prove that the selected catalog enumerates all application
   behavior. Physical phase subjects remain unavailable, by construction.

   Returns a source-scoped diagnostic and the nested v2259 release diagnostic;
   neither is a proof, installation eligibility or release credential."
  [catalog!]
  (when-not (fn? catalog!)
    (reject! :original-catalog-factory-required))
  (let [{initial :checked} (current-catalog! catalog!)
        intent (selected-intent initial)
        original-fingerprint (:fingerprint initial)
        original-ids (:declared-operation-ids initial)
        original-invariants (:invariant-ids initial)
        current! #(current-catalog! catalog!)
        discover! (fn [] (structural-inventory (:checked (current!))))
        subject! (fn [phase]
                   ;; A source identity is NOT an observed installation,
                   ;; activation or completion subject. Never counterfeit it.
                   (when (= phase :candidate)
                     {:phase phase
                      :claim-id (:claim-id intent)
                      :subject-id (str "candidate/"
                                       (:fingerprint (:checked (current!))))}))
        run-gate! (fn [{:keys [phase gate subject]}]
                    (let [{source :source now :checked} (current!)
                          unchanged? (and (= original-fingerprint
                                             (:fingerprint now))
                                          (= original-ids
                                             (:declared-operation-ids now))
                                          (= original-invariants
                                             (:invariant-ids now)))]
                      (if-not (and (= :candidate phase) unchanged?)
                        {:status :failed}
                        (cond
                          (= :positive (:kind gate))
                          (evidence intent phase gate subject #{source-premise})

                          (= :gesso.release.source/omitted-operation (:id gate))
                          (if (omitted-declaration-detected? source now :operation)
                            (evidence intent phase gate subject #{})
                            {:status :failed})

                          (= :gesso.release.source/omitted-invariant (:id gate))
                          (if (omitted-declaration-detected? source now :invariant)
                            (evidence intent phase gate subject #{})
                            {:status :failed})

                          :else {:status :unsupported}))))
        selected-report (decision/assess! {:intent intent
                                           :discover! discover!
                                           :subject! subject!
                                           :run-gate! run-gate!})]
    {:type report-type
     :version report-version
     :selected-catalog-id (:catalog-id initial)
     :selected-catalog-fingerprint original-fingerprint
     :operation-ids original-ids
     :invariant-ids original-invariants
     :source-open-meta-obligations (:open-meta-obligations initial)
     :source-open-obligations (:open-obligation-ids-by-operation initial)
     :source-selection :host-supplied-original-catalog-not-installed-authenticated
     ;; The phase row records historical checks. The summary additionally
     ;; requires the final inventory and subject observations to match.
     :candidate-status (let [status (get-in selected-report [:decisions :candidate :status])]
                         (if (and (= :checks-passed-only status)
                                  (not (and (= :matched (get-in selected-report [:final-inventory :status]))
                                            (= :matched (get-in selected-report [:final-subjects :candidate])))))
                           :blocked
                           status))
     :installation-status (get-in selected-report [:decisions :installation :status])
     :activation-status (get-in selected-report [:decisions :activation :status])
     :completion-status (get-in selected-report [:decisions :completion :status])
     :status (:status selected-report)
     :conditional? (:conditional? selected-report)
     :selected-release-diagnostic selected-report
     :release-authority :none
     :candidate-admission :not-issued
     :installation-eligibility :not-issued
     :activation-eligibility :not-issued
     :completion-claim :not-issued
     :runtime-effect-permission :none}))

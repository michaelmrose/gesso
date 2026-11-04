(ns gesso.assurance.proof.unified-preservation
  "Source-derived union of the four restricted invariant-preservation families
   (v1267): exact frame (v1259), literal postcondition (v1261), independently
   typed frame/literal conjunction (v1263), and same-origin copy equality
   (v1265).

   One family must independently establish the WHOLE invariant. This module
   never assembles fragments from different families into a novel inference,
   rewrites a predicate, or expands the languages accepted by those checkers.
   In particular, a conjunction mixing a copy-equality clause with a literal
   clause remains OPEN unless an existing checker proves the entire rule.

   Each family is run afresh against the ORIGINAL v1241 source using its
   implementation captured when this checker loads. If the corresponding
   public sibling Var has been replaced, that result is accepted only if its
   COMPLETE report equals a fresh source-derived result from the captured
   implementation, including each underlying derivation and open judgment.
   This rejects consistent-looking forged success sets; it does NOT establish
   independent checker soundness. Returned SHA-256 fingerprints
   are COPYABLE, UNADMITTED observations, never certificate credentials.
   Even a supported judgment remains conditional on the declared abstract
   transition; its checker soundness, initial states, assumptions, physical
   implementation, and certificate admission are NOT discharged."
  (:require [clojure.set :as set]
            [gesso.assurance.proof.assignment-preservation :as literal]
            [gesso.assurance.proof.copy-preservation :as copy]
            [gesso.assurance.proof.mixed-preservation :as mixed]
            [gesso.assurance.proof.obligation :as obligation]
            [gesso.assurance.proof.preservation :as frame]
            [gesso.assurance.subject :as subject]))

(def unified-preservation-version 1)
(def report-type
  :gesso.assurance.proof.unified-preservation/unadmitted-unified-preservation)

(def ^:private identity-id :gesso.assurance.proof/unified-preservation-v1)
(def ^:private known-categories
  #{:initial-state :preservation :assumption :implementation})
(def ^:private checked-status :checked-conditional-inference)
(def ^:private open-status :open)

;; The order is a deterministic explanation preference, not a logical
;; priority: any ONE independently checked family may support a whole rule.
(def ^:private families
  [:frame :literal :mixed :copy])

(defn- fail!
  [reason details]
  (throw (ex-info "Invalid Gesso unified preservation inference."
                  (merge {:error/type ::invalid-unified-preservation
                          :reason reason}
                         details))))

(defn- sha256? [value]
  (and (string? value)
       (boolean (re-matches #"sha256:[0-9a-f]{64}" value))))

;; Freeze the real family function values at namespace initialization.
;; Report data are never credentials. When a public family Var is replaced,
;; its complete response must match the registered source-derived checker,
;; including derivations, open judgments, and fingerprint. This catches
;; with-redefs of a sibling that returns an internally consistent lie.
;;
;; Only replaced public sibling Vars trigger a second run. Ordinary calls use
;; the captured source-derived checker once. This is NOT an independent formal
;; soundness proof. Changes to these families require reloading this namespace
;; and auditing the exact report contract. Shared primitive checker defects and
;; a maliciously altered classpath remain separate trust assumptions.
(def ^:private reference-family-checkers
  {:frame frame/check-frame-preservation!
   :literal literal/check-literal-assignment-preservation!
   :mixed mixed/check-mixed-preservation!
   :copy copy/check-copy-preservation!})

(defn- public-family-var
  [family]
  (case family
    :frame #'frame/check-frame-preservation!
    :literal #'literal/check-literal-assignment-preservation!
    :mixed #'mixed/check-mixed-preservation!
    :copy #'copy/check-copy-preservation!
    (fail! :unknown-preservation-family {:family family})))

(defn- verified-report-for!
  "Evaluate the original source using the captured, source-deriving sibling.
   If its public Var has been substituted, require that substitute to produce
   the exact same complete report. The captured run is the source of truth."
  [family source]
  (let [reference (get reference-family-checkers family)
        public (var-get (public-family-var family))]
    (when-not reference
      (fail! :unknown-preservation-family {:family family}))
    (if (identical? public reference)
      (reference source)
      (let [reported (public source)
            expected (reference source)]
        (when-not (= reported expected)
          (fail! :family-report-contract-changed
                 {:family family :detail :source-derived-evidence-mismatch}))
        expected))))

(defn- contract-for
  [family]
  (case family
    :frame {:type frame/report-type
            :version frame/preservation-version
            :proof-rule :pure-typed-rule-frame-extensionality
            :checking :original-sources-and-independent-frame-derivations
            :open-meta-obligations
            #{:proof-checker-and-extensionality-rule-soundness
              :certificate-admission}}
    :literal {:type literal/report-type
              :version literal/assignment-preservation-version
              :proof-rule :closed-typed-literal-postcondition
              :checking :original-sources-independent-assignment-derivations-and-typed-evaluation
              :open-meta-obligations
              #{:proof-checker-and-literal-substitution-rule-soundness
                :certificate-admission}}
    :mixed {:type mixed/report-type
            :version mixed/mixed-preservation-version
            :proof-rule :separable-frame-and-literal-conjunction
            :checking :original-sources-independent-field-derivations-and-typed-evaluation
            :open-meta-obligations
            #{:proof-checker-frame-and-literal-conjunction-rule-soundness
              :certificate-admission}}
    :copy {:type copy/report-type
           :version copy/copy-preservation-version
           :proof-rule :same-total-origin-substitution-and-frame
           :checking :original-sources-independent-equality-substitution-and-frame
           :open-meta-obligations
           #{:proof-checker-substitution-and-frame-extensionality-soundness
             :certificate-admission}}))

(defn- validate-family-report!
  "Treat even a returned sibling report as an untrusted assertion about its
   completed check. It must have exactly one judgment for EACH source-derived
   preservation obligation and preserve the entire open ledger."
  [family report obligations expected-ids]
  (let [{:keys [type version proof-rule checking open-meta-obligations]}
        (contract-for family)
        outcomes (:preservation-outcomes report)
        reported-ids (map :obligation-id outcomes)
        checked (set (map :obligation-id
                          (filter #(= checked-status (:status %)) outcomes)))
        open (set (map :obligation-id
                       (filter #(= open-status (:status %)) outcomes)))
        declared (:invariants obligations)
        valid-outcomes?
        (and (vector? outcomes)
             (= (count expected-ids) (count outcomes))
             (= expected-ids (set reported-ids))
             (= (count outcomes) (count (distinct reported-ids)))
             (every?
              (fn [{:keys [obligation-id invariant readers status] :as outcome}]
                (and (map? outcome)
                     (= obligation-id [:preservation invariant])
                     (= readers (:readers (get declared invariant)))
                     (contains? #{checked-status open-status} status)))
              outcomes))]
    (when-not
        (and (map? report)
             (= type (:type report))
             (= version (:version report))
             (= proof-rule (:proof-rule report))
             (= checking (:checking report))
             (sha256? (:fingerprint report))
             (= (:fingerprint obligations) (:obligation-fingerprint report))
             (= (:transition-fingerprint obligations)
                (:transition-fingerprint report))
             (= (:subject-fingerprint obligations) (:subject-fingerprint report))
             (= :declared-abstract-state-projection-only (:scope report))
             (= :all-pre-invariants-and-matching-transition-imply-post-invariant
                (:claim report))
             valid-outcomes?
             (= checked (:conditionally-checked-preservation-ids report))
             (= open (:unsupported-preservation-ids report))
             (= expected-ids (set/union checked open))
             (empty? (set/intersection checked open))
             (= (:open-obligation-ids obligations)
                (:original-open-obligation-ids report))
             (= :restricted-conditional-preservation-checked
                (:proof-status report))
             (= :not-independently-proved (:checker-soundness report))
             (= open-meta-obligations (:open-meta-obligations report))
             (not-any? #(contains? report %)
                       [:full-success? :fully-certified? :certified?
                        :proof-established? :transaction-authority])
             (= :not-issued (:certificate report))
             (= :not-admitted (:admission report))
             (= :not-established (:runtime-enforcement report))
             (= :not-established (:implementation-correspondence report))
             (= :none (:authority report)))
      (fail! :family-report-contract-changed {:family family}))
    {:family family
     :fingerprint (:fingerprint report)
     :proof-rule proof-rule
     :conditionally-checked-preservation-ids checked
     :unsupported-preservation-ids open
     :open-meta-obligations (:open-meta-obligations report)
     :outcomes (into (sorted-map) (map (juxt :obligation-id identity)) outcomes)}))

(defn check-unified-preservation!
  "Recheck original obligation SOURCE through all four existing restricted
   checkers; source-rederive whole sibling reports before accepting judgments.

   Successful candidate status is NOT a discharged v1241 proof obligation.
   No inference is invented by combining open fragments of different families.
   The comparison is reproducibility against captured family implementations,
   NOT independent rule soundness. A later certificate gate must recheck
   these judgments and establish remaining proof and production obligations."
  [source]
  (let [obligations (obligation/derive-obligations! source)
        preservation (get-in obligations [:obligations :preservation])
        declared (:invariants obligations)
        expected-ids (set (map :id preservation))]
    (when-not (and (= known-categories (set (keys (:obligations obligations))))
                   (= expected-ids
                      (set (map (fn [id] [:preservation id]) (keys declared))))
                   (= (count preservation) (count expected-ids))
                   (every? #(= :open (:status %)) preservation)
                   (= :all-obligations-open (:status obligations))
                   (= :not-proved (:proof-status obligations))
                   (= :not-issued (:certificate obligations))
                   (= :not-admitted (:admission obligations))
                   (= :not-established (:runtime-enforcement obligations))
                   (= :not-established (:implementation-correspondence obligations))
                   (= :none (:authority obligations)))
      (fail! :obligation-source-contract-changed {}))
    (let [checked-families
          (into (array-map)
                (map (fn [family]
                       [family (validate-family-report!
                                family (verified-report-for! family source)
                                obligations expected-ids)]))
                families)
          outcomes
          (mapv (fn [id]
                  (let [obligation-id [:preservation id]
                        supporting
                        (into [] (filter (fn [family]
                                           (contains?
                                            (get-in checked-families
                                                    [family :conditionally-checked-preservation-ids])
                                            obligation-id))) families)
                        selected (first supporting)
                        chosen (when selected
                                 (get-in checked-families
                                         [selected :outcomes obligation-id]))]
                    {:obligation-id obligation-id
                     :invariant id
                     :readers (:readers (get declared id))
                     :supporting-families supporting
                     :selected-family selected
                     :selected-inference (:inference chosen)
                     :source-report-fingerprint
                     (when selected (get-in checked-families
                                            [selected :fingerprint]))
                     :status (if selected checked-status open-status)}))
                (keys declared))
          established (into #{} (map :obligation-id)
                            (filter #(= checked-status (:status %)) outcomes))
          open (set/difference expected-ids established)
          _ (when-not (and (= expected-ids (set/union established open))
                           (empty? (set/intersection established open)))
              (fail! :preservation-obligation-coverage-loss {}))
          component-fingerprints
          (into (sorted-map)
                (map (fn [[id report]] [id (:fingerprint report)]))
                checked-families)
          meta-obligations
          (reduce set/union
                  #{:unified-preservation-dispatch-soundness
                    :independent-proof-kernel-soundness
                    :certificate-admission}
                  (map :open-meta-obligations (vals checked-families)))
          identity
          (subject/describe-subject
           {:kind :rule :id identity-id
            :interface {:version unified-preservation-version
                        :obligation-fingerprint (:fingerprint obligations)
                        :transition-fingerprint (:transition-fingerprint obligations)
                        :component-fingerprints component-fingerprints
                        :outcomes outcomes
                        :open-meta-obligations meta-obligations}})]
      {:type report-type
       :version unified-preservation-version
       :fingerprint (:fingerprint identity)
       :obligation-fingerprint (:fingerprint obligations)
       :transition-fingerprint (:transition-fingerprint obligations)
       :subject-fingerprint (:subject-fingerprint obligations)
       :proof-rule :source-derived-union-of-whole-invariant-inferences
       :scope :declared-abstract-state-projection-only
       :claim :all-pre-invariants-and-matching-transition-imply-post-invariant
       :preservation-outcomes outcomes
       :component-report-fingerprints component-fingerprints
       :conditionally-checked-preservation-ids established
       :unsupported-preservation-ids open
       :original-open-obligation-ids (:open-obligation-ids obligations)
       :checking :original-source-and-four-independent-restricted-checkers
       :proof-status :restricted-conditional-preservation-checked
       :checker-soundness :not-independently-proved
       :open-meta-obligations meta-obligations
       :certificate :not-issued
       :admission :not-admitted
       :runtime-enforcement :not-established
       :implementation-correspondence :not-established
       :authority :none})))

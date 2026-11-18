(ns gesso.assurance.ansatz-observation-gate-pilot
  "Opt-in v1738 primitive-checked ABSTRACT observer boundary.

   After the original reservation's Ansatz source/step bridge is kernel-checked
   and dependency-audited, validate EVERY observed matching transition against
   that SAME generated induction-step formula. Cross-check the original and
   independent Gesso abstract observers and refuse fabricated/mismatched
   reports. This makes the check a CONDITION of acceptance for all calls
   through this test-only boundary rather than relying on a finite sample.

   Original interpreter forms are separately read from the current source
   resource, compared to reviewed, canonical form digests, and checked for the
   exact Boolean acceptance conjunction before the observation boundary can
   be constructed. This is source-identity/structural evidence, not a proof
   that the JVM executes those forms or that their primitives are sound.

   The guarded trace step is an OVERAPPROXIMATION: even two colluding
   observers could otherwise falsely accept a *different* permitted step.
   A separate, closed-source primitive oracle therefore recomputes the exact
   original guard, both literal writes, and audit-field framing on EVERY
   invocation, comparing each Boolean against BOTH observer reports. Invalid
   before/after projections are also refused independently of the observers.

   This is NOT a universal mathematical proof of either Clojure interpreter.
   It makes no assertion about calls that bypass this boundary, guard/assignment
   implementation correctness, installed handlers, XTDB writers or commits,
   initialization, or physical state. Return values are copyable and never
   carry admission, certification or runtime authority. No downstream type
   annotations or Ansatz source are needed."
  (:require [ansatz.core :as ansatz]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests testing]]
            [gesso.assurance.ansatz-transition-correspondence-pilot]
            [gesso.assurance.example.single-node-proving :as example]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.assurance.proof.transition :as abstract])
  (:import [java.io PushbackReader StringReader]
           [java.math BigInteger]
           [java.security MessageDigest]))

(def ^:private source-ns 'gesso.assurance.ansatz-transition-correspondence-pilot)
(def ^:private status-field :slot/status)
(def ^:private claims-field :slot/active-claims)
(def ^:private audit-field :slot/audit-tag)

(defn- from-source! [function & args]
  (let [v (ns-resolve source-ns function)]
    (when-not v
      (throw (ex-info "Missing original proof bridge" {:reason :pilot-contract-changed
                                                        :function function})))
    (apply @v args)))

(defn- fail! [reason details]
  (throw (ex-info "Unqualified Gesso abstract observation"
                  {:reason reason :details details})))

;; These are signatures of reviewed ORIGINAL source forms, not of separate
;; rewritten examples. They deliberately pin the semantic acceptance path,
;; its literal/read assignment interpretation, exact field framing and typed
;; input validation. Any functional implementation change requires fresh
;; review and a new explicitly versioned pilot policy.
(def ^:private expected-interpreter-form-digests
  {'same-field? "200a0a219bb50db33c443c5ff26e45705943e8444039d512546f5c35a98c45a5"
   'expected-assignment "e3665eea7e0d61204eff3393dd0255f5adb247d2a2005005e2857b3b1fc7c097"
   'valid-bindings! "3c0ae6db47618986787f54e0130637c6e5c80075436d0865e66dc2e0d5a9e7a6"
   'observe-transition! "e84f140ddffe37f8fd693fdd3a50d9c986108753a69ee7663a8f2fba5e418de9"})
(def ^:private expected-interpreter-ns-digest
  "a9b7aa4074d98fc72a06930b2d88b75e50c2e396a65290f36c95d5c986f92e92")
(def ^:private reviewed-acceptance-form
  '(boolean (and guard-value assignments-ok? frame-ok?)))

(defn- read-interpreter-forms! [source]
  ;; Clojure's reader parses the same source language used by the original
  ;; interpreter; read-time evaluation is disabled. Reader-expanded #()
  ;; argument names are normalized only for hashing, never for execution.
  (with-open [r (PushbackReader. (StringReader. source))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read {:eof ::end} r)]
          (if (= ::end form) forms (recur (conj forms form))))))))

(defn- form-digest [form]
  (let [serialized (-> (pr-str form)
                       (str/replace #"p([0-9]+)__[0-9]+#" "p$1__anon#"))
        bytes (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes serialized "UTF-8"))]
    (format "%064x" (BigInteger. 1 bytes))))

(defn- interpreter-source-attestation! [source]
  (let [forms (read-interpreter-forms! source)
        ns-forms (filter #(and (seq? %) (= 'ns (first %))) forms)
        defs (filter #(and (seq? %) (#{'defn 'defn-} (first %))
                           (contains? expected-interpreter-form-digests
                                      (second %))) forms)
        observed (into {} (map (fn [form] [(second form) (form-digest form)])
                               defs))
        acceptance (->> defs
                        (filter #(= 'observe-transition! (second %)))
                        first
                        (tree-seq coll? seq)
                        (filter map?)
                        (keep :matches?)
                        vec)]
    ;; Check both explicit acceptance structure and complete reviewed source
    ;; identity. Digests include complete forms, preventing a new omitted
    ;; validator/assignment/frame path from quietly inheriting this evidence.
    (when-not (and (= 1 (count ns-forms))
                   (= expected-interpreter-ns-digest
                      (form-digest (first ns-forms)))
                   (= (count defs) (count expected-interpreter-form-digests))
                   (= expected-interpreter-form-digests observed)
                   (= [reviewed-acceptance-form] acceptance))
      (fail! :unreviewed-interpreter-source
             {:expected-form-names (set (keys expected-interpreter-form-digests))
              :observed-form-names (set (keys observed))
              :accepted-form acceptance
              :changed-forms (set (for [[name hash] observed
                                        :when (not= hash
                                                    (get expected-interpreter-form-digests name))]
                                    name))}))
    {:policy :original-abstract-observer-source-forms-v1
     :reviewed-form-digests observed
     :scope :source-form-and-acceptance-attestation-only
     :compiled-runtime-correspondence :not-established
     :guard-evaluator-correctness :not-established}))

(defn- current-interpreter-source! []
  (let [file (or (io/resource "gesso/assurance/proof/transition.clj")
                 (fail! :missing-original-interpreter-source {}))]
    (interpreter-source-attestation! (slurp file))))

(defn- exact-report-shape! [result original independent-observation]
  (let [flags [:guard-holds? :assignments-hold? :frame-holds? :matches?]]
    (when-not (and (= :gesso.assurance.proof.transition/abstract-observation
                      (:type original))
                   (= :abstract-relation-observed (:checking original))
                   (= :gesso.assurance.proof.independent-transition-semantics/observation
                      (:type independent-observation))
                   (= :independent-typed-guarded-relation
                      (:checking independent-observation))
                   (= (:transition-fingerprint result)
                      (:transition-fingerprint original))
                   (= (:transition result)
                      (:source-declaration independent-observation))
                   (every? (fn [f]
                             (and (boolean? (get original f))
                                  (boolean? (get independent-observation f))
                                  (= (get original f)
                                     (get independent-observation f))))
                           flags))
      (fail! :observer-disagreement
             {:operation-id (:operation-id result)
              :original original :independent independent-observation}))
    (let [all? (every? true? (map original
                                  [:guard-holds? :assignments-hold? :frame-holds?]))]
      (when-not (= all? (:matches? original))
        (fail! :observer-acceptance-inconsistent
               {:operation-id (:operation-id result)})))))

(def ^:private closed-reservation-state-fields
  #{status-field claims-field audit-field})

(defn- plain-state? [state]
  (or (instance? clojure.lang.PersistentArrayMap state)
      (instance? clojure.lang.PersistentHashMap state)))

(defn- verify-closed-projection! [model label state]
  ;; Our original catalog has three REQUIRED state fields. This is NOT a
  ;; general Gesso type validator; it checks the exact source subset whose
  ;; transition relation and proofs were regenerated earlier in the pilot.
  ;; Revalidate independently so two corrupted observers cannot silently
  ;; accept malformed maps that the real typed interpreter would have thrown.
  (let [status (get state status-field)
        claims (get state claims-field)
        audit (get state audit-field)]
    (when-not (and (plain-state? state)
                   (nil? (meta state))
                   (= closed-reservation-state-fields (set (keys state)))
                   (contains? (:status-encoding model) status)
                   (integer? claims)
                   (<= (count (str claims)) 2048)
                   (or (nil? audit)
                       (and (keyword? audit) (<= (count (str audit)) 2048))))
      (fail! :invalid-original-state-projection
             {:field label :value state}))
    state))

(defn- source-primitive-flags! [model source before after]
  ;; These predicates are deliberately small and independent of BOTH Gesso
  ;; interpreters. Their constants come from the checked ORIGINAL transition
  ;; source, not a second handwritten operation catalog. All three facts are
  ;; necessary for the ORIGINAL relation; the induction model's step formula
  ;; alone is strictly weaker and must never be treated as a full guard.
  (verify-closed-projection! model :before before)
  (verify-closed-projection! model :after after)
  (let [encoding (:status-encoding model)
        guard? (= (get encoding (get before status-field)) (:before-code source))
        assignments? (and (= (get encoding (get after status-field))
                             (:after-code source))
                          (= (get after claims-field) (:claim-value source)))
        frame? (= (get before audit-field) (get after audit-field))]
    {:guard-holds? (boolean guard?)
     :assignments-hold? (boolean assignments?)
     :frame-holds? (boolean frame?)
     :matches? (boolean (and guard? assignments? frame?))}))

(defn- source-primitives-agree! [model source before after original other]
  (let [expected (source-primitive-flags! model source before after)
        keys [:guard-holds? :assignments-hold? :frame-holds? :matches?]
        actual (select-keys original keys)
        other-actual (select-keys other keys)]
    (when-not (and (= expected actual) (= expected other-actual))
      (fail! :primitive-semantics-disagreement
             {:operation-id (:operation-id source)
              :expected expected :original actual :independent other-actual}))
    expected))

(defn- audited-observation-boundary! [catalog]
  ;; This is a proof/check-time CONSTRUCTOR, not an HTTP-path proof invocation.
  ;; The three source-specific bridge theorems are checked BEFORE any witness
  ;; can be accepted. No dynamic application-supplied proof report is trusted.
  (let [source-attestation (current-interpreter-source!)
        bridge (from-source! 'check-original-bridge! catalog)
        {:keys [model sources]} (from-source! 'source-bridge-obligations! catalog)
        sources-by-id (into {} (map (juxt :operation-id identity)) sources)]
    (when-not (and (= :conditional-abstract-step-bridge (:assurance bridge))
                   (= 3 (:kernel-checked-bridge-count bridge))
                   (= 2 (count sources))
                   (= (:catalog-fingerprint bridge)
                      (:catalog-fingerprint model)))
      (fail! :incomplete-kernel-bridge bridge))
    {:checking :kernel-audited-abstract-observation-boundary-pilot
     :original-interpreter-source-attestation source-attestation
     :catalog-fingerprint (:catalog-fingerprint model)
     :observer-correspondence :per-invocation-checked-not-universally-proved
     :primitive-correspondence :closed-source-per-invocation-checked
     :physical-correspondence :not-established
     :certificate :not-issued
     :admission :not-admitted
     :authority :none
     :observe!
     (fn [operation-id before after]
       (let [{:keys [transition] :as source} (get sources-by-id operation-id)]
         (when-not source
           (fail! :unknown-source-operation {:operation-id operation-id}))
         ;; Both original interpreters validate the exact typed before/after
         ;; projection and reject malformed values; never accept caller reports.
         (let [original (abstract/observe-transition! transition before {} after)
               other (independent/observe-transition! transition before {} after)]
           (exact-report-shape! source original other)
           (source-primitives-agree! model source before after original other)
           (let [matches? (:matches? original)
                 represented? (when matches?
                                (from-source! 'step-holds? model before after))]
             (when (and matches? (not represented?))
               (fail! :accepted-abstract-transition-outside-proved-step
                      {:operation-id operation-id :before before :after after}))
             {:operation-id operation-id
              :transition-fingerprint (:transition-fingerprint source)
              :matches? matches?
              :step-checked? (boolean matches?)
              :step-represented? (boolean represented?)
              :assurance :per-observation-abstract-step-enforcement
              :certificate :not-issued
              :admission :not-admitted
              :authority :none}))))}))

(deftest every-accepted-observation-at-this-boundary-satisfies-the-proved-step
  (let [boundary (audited-observation-boundary! (example/reservation-catalog))
        check! (:observe! boundary)
        states (vec (for [status [:available :reserved]
                          claims [-1 0 1 2]
                          audit [nil :audit/tag]]
                      {status-field status claims-field claims audit-field audit}))
        hits (atom 0)]
    (is (= :kernel-audited-abstract-observation-boundary-pilot (:checking boundary)))
    (is (= :original-abstract-observer-source-forms-v1
           (get-in boundary [:original-interpreter-source-attestation :policy])))
    (is (= :not-established
           (get-in boundary [:original-interpreter-source-attestation
                             :compiled-runtime-correspondence])))
    (is (= :per-invocation-checked-not-universally-proved
           (:observer-correspondence boundary)))
    (is (= :closed-source-per-invocation-checked
           (:primitive-correspondence boundary)))
    (doseq [operation-id [:slot/reserve :slot/release]
            before states
            after states]
      (let [result (check! operation-id before after)]
        (is (boolean? (:matches? result)))
        (when (:matches? result)
          (swap! hits inc)
          (is (:step-checked? result))
          (is (:step-represented? result))
          (is (= :none (:authority result))))))
    (is (= 16 @hits))
    (is (= :not-issued (:certificate boundary)))
    (is (= :not-admitted (:admission boundary)))
    (is (= :none (:authority boundary)))))

(deftest observational-boundary-catches-false-acceptance-from-both-observers
  (let [check! (:observe! (audited-observation-boundary!
                           (example/reservation-catalog)))
        before {status-field :available claims-field 0 audit-field nil}
        bad {status-field :reserved claims-field 2 audit-field nil}
        valid {status-field :reserved claims-field 1 audit-field nil}
        primary abstract/observe-transition!
        secondary independent/observe-transition!
        fake-primary (fn [d b in a]
                       (assoc (primary d b in a)
                              :guard-holds? true :assignments-hold? true
                              :frame-holds? true :matches? true))
        fake-secondary (fn [d b in a]
                         (assoc (secondary d b in a)
                                :guard-holds? true :assignments-hold? true
                                :frame-holds? true :matches? true))
        reason (fn [f]
                 (try (f) nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (is (:matches? (check! :slot/reserve before valid)))
    (is (false? (:matches? (check! :slot/reserve before bad))))
    (testing "A single lying observer cannot bypass an independent observer"
      (with-redefs [abstract/observe-transition! fake-primary]
        (is (= :observer-disagreement
               (reason #(check! :slot/reserve before bad))))))
    (testing "Two colluding false-positive reports fail the exact write check"
      (with-redefs [abstract/observe-transition! fake-primary
                   independent/observe-transition! fake-secondary]
        (is (= :primitive-semantics-disagreement
               (reason #(check! :slot/reserve before bad))))))
    (testing "Unknown operation and invalid source projection fail closed"
      (is (= :unknown-source-operation
             (reason #(check! :slot/unlisted before valid))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (check! :slot/reserve
                           (assoc before :slot/unregistered 1) valid))))))

(deftest exact-source-primitives-reject-steps-that-induction-overapproximates
  (let [catalog (example/reservation-catalog)
        bridge (from-source! 'source-bridge-obligations! catalog)
        model (:model bridge)
        transition (:transition (first (:sources bridge)))
        step? (ns-resolve source-ns 'step-holds?)
        check! (:observe! (audited-observation-boundary! catalog))
        valid-before {status-field :available claims-field 0 audit-field nil}
        reserved-before {status-field :reserved claims-field 0 audit-field nil}
        valid-after {status-field :reserved claims-field 1 audit-field nil}
        ;; All three remain legal to the WEAKER trace relation because
        ;; that relation permits any declared write regardless of guard,
        ;; and does not track the framed audit field.
        cases {:false-guard [reserved-before valid-after]
               :wrong-literal-write [valid-before valid-before]
               :changed-audit-frame
               [valid-before (assoc valid-after audit-field :audit/changed)]}
        original abstract/observe-transition!
        secondary independent/observe-transition!
        fake-original (fn [d b in a]
                        (assoc (original d b in a)
                               :guard-holds? true :assignments-hold? true
                               :frame-holds? true :matches? true))
        fake-secondary (fn [d b in a]
                         (assoc (secondary d b in a)
                                :guard-holds? true :assignments-hold? true
                                :frame-holds? true :matches? true))
        reason (fn [before after]
                 (try (check! :slot/reserve before after) nil
                      (catch clojure.lang.ExceptionInfo e
                        (:reason (ex-data e)))))]
    (doseq [[case-id [before after]] cases]
      (is (step? model before after)
          (str case-id " fits the weaker Ansatz trace relation"))
      (is (false? (:matches? (check! :slot/reserve before after)))
          (str case-id " is NOT a valid guarded reservation"))
      (with-redefs [abstract/observe-transition! fake-original
                   independent/observe-transition! fake-secondary]
        (is (= :primitive-semantics-disagreement (reason before after))
            (str case-id " cannot inherit false observer acceptance"))))
    (testing "Two colluding false-negative reports are also detected"
      (let [fake-no (fn [observer]
                      (fn [d b in a]
                        (assoc (observer d b in a)
                               :guard-holds? false :assignments-hold? false
                               :frame-holds? false :matches? false)))]
        (with-redefs [abstract/observe-transition! (fake-no original)
                     independent/observe-transition! (fake-no secondary)]
          (is (= :primitive-semantics-disagreement
                 (reason valid-before valid-after))))))
    (testing "Forged acceptance cannot bypass independent source type checks"
      (let [bad (assoc valid-after :slot/extra :unproven)
            valid-original (original transition valid-before {} valid-after)
            valid-independent (secondary transition valid-before {} valid-after)]
        (with-redefs [abstract/observe-transition! (fn [& _] valid-original)
                     independent/observe-transition! (fn [& _] valid-independent)]
          (is (= :invalid-original-state-projection (reason valid-before bad))))))
    (testing "An invalid claim type cannot inherit a forged acceptance"
      (let [bad (assoc valid-after claims-field 1.0)
            valid-original (original transition valid-before {} valid-after)
            valid-independent (secondary transition valid-before {} valid-after)]
        (with-redefs [abstract/observe-transition! (fn [& _] valid-original)
                     independent/observe-transition! (fn [& _] valid-independent)]
          (is (= :invalid-original-state-projection (reason valid-before bad))))))))

(deftest per-observation-check-does-not-depend-on-a-bounded-integer-range
  (let [check! (:observe! (audited-observation-boundary!
                           (example/reservation-catalog)))
        before {status-field :available claims-field 1000000 audit-field nil}
        after {status-field :reserved claims-field 1 audit-field nil}
        mismatched (assoc after claims-field -1000000)]
    ;; Invalid initial invariant is irrelevant to this *conditional bridge*:
    ;; the guard/write match still implies the trace-step premise. A separate
    ;; initialization obligation is necessary for invariant safety.
    (is (:matches? (check! :slot/reserve before after)))
    (is (:step-represented? (check! :slot/reserve before after)))
    (is (false? (:matches? (check! :slot/reserve before mismatched))))))

(deftest original-interpreter-acceptance-source-is-pinned-and-fails-closed
  (let [source (slurp (io/resource "gesso/assurance/proof/transition.clj"))
        clean (interpreter-source-attestation! source)
        denial (fn [modified]
                 (try (interpreter-source-attestation! modified)
                      nil
                      (catch clojure.lang.ExceptionInfo ex
                        (:reason (ex-data ex)))))]
    (is (= :original-abstract-observer-source-forms-v1 (:policy clean)))
    (is (= (set (keys expected-interpreter-form-digests))
           (set (keys (:reviewed-form-digests clean)))))
    (testing "Whitespace and comments do not change the original source forms"
      (is (= (:reviewed-form-digests clean)
             (:reviewed-form-digests
              (interpreter-source-attestation!
               (str "; pilot-only comment\n" source))))))
    (testing "Mutation to the actual acceptance Boolean is never inherited"
      (let [original ":matches? (boolean (and guard-value assignments-ok? frame-ok?))"]
        (is (= :unreviewed-interpreter-source
               (denial (str/replace source original ":matches? true"))))))
    (testing "Dropping after-state validation changes the audited interpreter"
      (is (= :unreviewed-interpreter-source
             (denial (str/replace source
                                  "(valid-bindings! state-types after)"
                                  "(identity after)")))))
    (testing "Replacing the assigned-value semantics changes the audit"
      (is (= :unreviewed-interpreter-source
             (denial (str/replace source
                                  ":read (get pre-and-input operand)"
                                  ":read nil")))))
    (testing "Weakening presence-sensitive framing changes the audit"
      (is (= :unreviewed-interpreter-source
             (denial (str/replace source
                                  "(= (contains? before field) (contains? after field))"
                                  "true")))))
    (testing "Changing the source namespace's evaluator binding is reviewed"
      (is (= :unreviewed-interpreter-source
             (denial (str/replace source
                                  "[gesso.assurance.rule-eval :as rule-eval]"
                                  "[clojure.core :as rule-eval]")))))))

(defn -main [& _]
  (binding [ansatz/*verbose* false] (ansatz/load-init!))
  (let [{:keys [fail error]} (run-tests
                              'gesso.assurance.ansatz-observation-gate-pilot)]
    (when (pos? (+ fail error)) (System/exit 1))))

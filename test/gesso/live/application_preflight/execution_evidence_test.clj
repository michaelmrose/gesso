(ns gesso.live.application-preflight.execution-evidence-test
  "Permanent execution-evidence regressions for both application assurance APIs.

   Ordinary explain-with-execution-evidence is deterministic and does not run
   Recife. Opt-in explain-with-formal-execution-evidence checks an authenticated
   executor observation through the admitted finite transaction subject and
   the generic TLC-backed five-obligation checker.

   The canonical explanation must bind a process-local diagnostic handle to
   the EXACT executor recovered from a freshly checked ApplicationAssembly.
   Real embedded XTDB execution establishes only bounded correspondence of
   an observed transaction, not business authorization, complete guards,
   truthful projections, or universal refinement.

   Tests use their own authoritative fixtures. No fake certificate, mutated
   metadata, detached report, or alternate adapter may substitute for a handle
   produced by the actual registered Live executor. The source-derived
   comparison introduced in v921 and the bounded Recife check introduced in
   v929 remain diagnostic. Neither promotes an observed executor attempt to
   universal operation refinement or application certification."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [gesso.choreo.identity :as identity]
   [gesso.live.application-preflight :as application]
   [gesso.live.application-preflight.fixture :as fixture]
   [gesso.live.optimistic.protocol :as protocol]
   [gesso.live.optimistic.resolved-operation :as adapter]
   [gesso.live.optimistic.resolution-assurance :as assurance]
   [gesso.live.optimistic.server :as server]
   [gesso.model.command :as command]
   [gesso.model.formal-check :as formal-check]
   [gesso.model.formal-observation :as formal-observation]
   [gesso.model.resolved-operation :as resolved]
   [xtdb.api :as xt]
   [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f)
         (finally (reset! !node nil)))))

(use-fixtures :once with-node)

(def ^:private operation-id :request/claim)
(def ^:private now "2026-10-12T12:00:00Z")
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private principal (identity/principal "v817-principal-private"))

(defn- row [table id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- server-with [executor]
  (server/server
   {:principal-fn (fn [ctx] (:authenticated-principal ctx))
    :operations
    {operation-id
     (server/operation
      {:name :request/claim-optimistic
       :operation operation-id
       :execute! executor
       :settlement-contract
       {:confirmed {:outcomes #{:request/claimed} :commit/status :committed}
        :already-incorporated {:outcomes #{:request/claimed}
                               :commit/status :committed}}})}}))

(defn- test-operation
  ([] (test-operation {}))
  ([{:keys [false-projection?]}]
   (let [suffix (str (random-uuid))
         request-id (str "v817-request-private-" suffix)
         before {:xt/id request-id :revision 0
                 :created-at "t0" :updated-at "t0" :owner "unclaimed"}
         membership {:xt/id (str "v817-member-" suffix)
                     :revision 0 :created-at "t0" :updated-at "t0"
                     :active true}
         claimed (assoc before :revision 1 :updated-at now
                        :owner "v817-principal-private")
         cmd (protocol/command
              {:command-id (identity/command-id (str "v817-command-" suffix))
               :execution-id (identity/execution-id (str "v817-execution-" suffix))
               :operation operation-id
               :arguments {:request-id request-id}
               :observed-basis {:tx-id 1 :system-time now}
               :scope [:request request-id]})
         calls (atom {:construct 0 :disclose 0 :project 0 :read 0})
         model (resolved/build-operation
                {:operation operation-id
                 :read-record! (fn [_ id]
                                 (swap! calls update :read inc)
                                 (row :gesso_commands id))
                 :may-disclose? (fn [_ _ _]
                                  (swap! calls update :disclose inc)
                                  true)
                 :construct! (fn [_ _]
                               (swap! calls update :construct inc)
                               {:now now
                                :outcome {:status :claimed}
                                :plan {:commands [(command/update-command
                                                   :requests :claim before claimed
                                                   version)]
                                       :guards [(command/guard
                                                 :memberships membership version)]
                                       :emit false}})})
         executor (adapter/build-executor
                   {:operation operation-id
                    :resolved-operation model
                    :submission-mode! (fn [_] :submit)
                    :project! (fn [_ result]
                                (swap! calls update :project inc)
                                {:resolution (if (= :replay (:status result))
                                               :already-incorporated :confirmed)
                                 :authoritative
                                 (protocol/authoritative
                                  {:presence :present
                                   :basis {:tx-id 2 :system-time now}
                                   :projection
                                   {:request/owner
                                    (if false-projection?
                                      "deliberately-false-projection"
                                      (:owner (row :requests request-id)))}})
                                 :outcome :request/claimed})})]
     (xt/execute-tx @!node [[:put-docs :requests before]
                             [:put-docs :memberships membership]])
     {:request-id request-id
      :before before :claimed claimed :command cmd
      :model model :executor executor :calls calls
      :server (server-with executor)})))

(defn- invoke! [{:keys [server command]}]
  (assurance/run-and-check!
   {:server server
    :ctx {:biff.xtdb/node @!node :authenticated-principal principal}
    :command command}))

(defn- with-application [executor f]
  (fixture/with-closed-application
   {:operations
    (assoc (fixture/standard-operations)
           operation-id
           (fixture/trusted-operation
            operation-id
            {:published-change-topics #{:request}
             :execute! executor}))}
   f))

(defn- kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))

(defn- observed-card [explanation]
  (get-in explanation
          [:semantic-audit :operation-cards operation-id
           :runtime-execution-observations]))

(defn- observed-summary [explanation]
  (get-in explanation [:semantic-audit :execution-observation-summary]))

(defn- without-diagnostics [explanation]
  (-> explanation
      (update :semantic-audit
              (fn [audit]
                (-> audit
                    (dissoc :execution-observation-summary)
                    (update-in [:operation-cards operation-id]
                               dissoc :runtime-execution-observations))))))

(defn- assert-unproved-policy [explanation]
  (let [audit (:semantic-audit explanation)
        boundary (some #(when (= operation-id (:operation %)) %)
                       (:opaque-model-boundaries audit))
        coverage (some #(when (= [:operation operation-id
                                  :model-implementation-conformance]
                                 (:subject %)) %)
                       (:assurance-coverage audit))]
    (is (= :trusted (:class boundary)))
    (is (false? (:discharged? boundary)))
    (is (= :trusted (:class coverage)))
    (is (= :opaque-authoritative-model-code (:scope coverage)))
    (is (false? (get-in audit [:inspection-scope
                               :complete-business-policy-inventory?])))
    (is (= :unknown (get-in audit [:business-rule-catalog :count])))
    (is (not-any? #(contains? #{:proved :certified} (:class %))
                  (:assurance-coverage audit)))))

(deftest empty-observations-are-explicit-but-do-not-certify-test
  (let [f (test-operation)]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              ordinary (application/explain assembly)
              enhanced (application/explain-with-execution-evidence assembly {})
              summary (observed-summary enhanced)]
          (is (= :diagnostic-execution-observations-only (:status summary)))
          (is (= 0 (:observation-count summary)))
          (is (= 0 (:observed-operation-count summary)))
          (is (= :current-assembly-exact-executor-registry (:source summary)))
          (is (contains? (:not-established summary) :all-executions-observed))
          (is (nil? (observed-card enhanced))
              "Empty evidence cannot imply even one source-derived check.")
          (is (nil? (observed-summary ordinary)))
          (is (= ordinary (without-diagnostics enhanced)))
          (is (= before @(:calls f))
              "Explanation must not invoke domain or projection callbacks.")
          (assert-unproved-policy enhanced))))))

(deftest genuine-observation-is-bounded-and-redacted-test
  (let [f (test-operation)
        {:keys [prepared-send evidence]} (invoke! f)]
    (is (server/prepared-send? prepared-send))
    (is (= (:claimed f) (row :requests (:request-id f))))
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              normal (application/explain assembly)
              enriched (application/explain-with-execution-evidence
                        assembly {operation-id [evidence]})
              card (observed-card enriched)
              summary (observed-summary enriched)
              printed (pr-str {:card card :summary summary})]
          (is (= normal (without-diagnostics enriched)))
          (is (= :bounded-process-local-execution-observations (:status card)))
          (is (= :bounded-observation-not-proof (:assurance-class card)))
          (is (= :exact-registered-executor-diagnostic-handle
                 (:evidence-class card)))
          (is (= :not-established (:universal-guarantees card)))
          (is (= {:status :selected-attempts-correspond
                  :checked-attempt-count 1
                  :subject :gesso.model.formal-observation/single-executor-attempt-v1
                  :assurance-class :finite-reference-replay-not-formal-proof
                  :model-checked? false}
                 (:source-derived-reference-check card)))
          (is (= 1 (:observation-count card)))
          (is (= [{:index 0
                   :status :bounded-correspondence-observed
                   :physical-status :committed
                   :finite-reference-status :consistent}]
                 (:observations card)))
          (is (= assurance/assurance-scope (:scope card)))
          (is (false? (get-in card [:source-derived-reference-check
                                    :model-checked?])))
          (is (= :not-established (:universal-guarantees card)))
          (is (= 1 (:observation-count summary)))
          (is (= 1 (:observed-operation-count summary)))
          (is (contains? (:not-established summary)
                         :truthful-application-projections))
          (is (contains? (:not-established summary)
                         :universal-refinement-or-certification))
          (is (not (str/includes? printed (:request-id f))))
          (is (not (str/includes? printed "v817-principal-private")))
          (is (not (str/includes? printed (str (:command-id (:command f))))))
          (is (not (str/includes? printed ":receipt")))
          (is (not (str/includes? printed ":binding")))
          (is (not (str/includes? printed ":plan")))
          (is (not (identical? evidence card)))
          (is (= before @(:calls f))
              "Reporting must not execute or replay the operation.")
          (assert-unproved-policy enriched))))))

(deftest fabricated-or-detached-evidence-is-refused-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        detached (assurance/inspect-evidence evidence)]
    (with-application
      (:executor f)
      (fn [{:keys [assembly report]}]
        (doseq [forgery [(Object.) detached
                         (assoc detached :status :bounded-execution-correspondence)]]
          (is (= :unrecognized-execution-observation
                 (kind #(application/explain-with-execution-evidence
                         assembly {operation-id [forgery]})))))
        (is (= :invalid-or-stale-observation-assembly
               (kind #(application/explain-with-execution-evidence
                       report {operation-id [evidence]}))))
        (is (= :invalid-or-stale-observation-assembly
               (kind #(application/explain-with-execution-evidence
                       (assoc assembly :operation-acquisition-assembly {}) {operation-id [evidence]}))))
        (is (map? (observed-card
                   (application/explain-with-execution-evidence
                    assembly {operation-id [evidence]}))))))))

(deftest invalid-observation-shapes-are-rejected-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (doseq [value [nil [] :request/claim
                       {operation-id nil}
                       {operation-id []}
                       {operation-id (list evidence)}
                       {operation-id [nil]}
                       {:request/unknown [evidence]}
                       {operation-id [evidence] :request/unknown [evidence]}]]
          (is (= :invalid-execution-observation-request
                 (kind #(application/explain-with-execution-evidence
                         assembly value)))))))))

(deftest handle-is-bound-to-exact-current-adapter-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        original (:executor f)
        cloned (with-meta original (assoc (meta original) :copied true))
        impersonator (with-meta (fn [ctx] (original ctx)) (meta original))
        alternate (adapter/build-executor
                   {:operation operation-id
                    :resolved-operation (:model f)
                    :submission-mode! (fn [_] :submit)
                    :project! (fn [_ _]
                                (throw (ex-info "Never run on audit" {})))})]
    (is (nil? (adapter/construction-evidence impersonator)))
    (is (nil? (adapter/construction-evidence cloned)))
    (is (some? (adapter/construction-evidence alternate)))
    (doseq [other [cloned impersonator alternate]]
      (with-application
        other
        (fn [{:keys [assembly]}]
          (is (= :unrecognized-execution-observation
                 (kind #(application/explain-with-execution-evidence
                         assembly {operation-id [evidence]})))))))
    (with-application
      original
      (fn [{:keys [assembly]}]
        (is (= 1 (:observation-count
                  (observed-card (application/explain-with-execution-evidence
                                  assembly {operation-id [evidence]})))))))))

(deftest physical-witness-does-not-certify-false-projection-test
  (let [f (test-operation {:false-projection? true})
        {:keys [prepared-send evidence]} (invoke! f)]
    (is (= :confirmed (get-in prepared-send [:settlement :resolution])))
    (is (= {:request/owner "deliberately-false-projection"}
           (get-in prepared-send [:settlement :authoritative :projection])))
    (is (= "v817-principal-private"
           (:owner (row :requests (:request-id f)))))
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [result (application/explain-with-execution-evidence
                      assembly {operation-id [evidence]})]
          (is (= :committed
                 (get-in (observed-card result) [:observations 0 :physical-status])))
          (is (= :bounded-observation-not-proof
                 (:assurance-class (observed-card result))))
          (is (= :finite-reference-replay-not-formal-proof
                 (get-in (observed-card result)
                         [:source-derived-reference-check :assurance-class])))
          (is (false? (get-in (observed-card result)
                             [:source-derived-reference-check :model-checked?])))
          (is (= :not-established
                 (:universal-guarantees (observed-card result))))
          (is (contains? (:not-established (observed-summary result))
                         :truthful-application-projections))
          (assert-unproved-policy result))))))

(deftest source-derived-correspondence-fails-closed-test
  ;; These are boundary fault injections, not forged executor handles. Each
  ;; invocation uses a genuine process-local handle and the exact executor.
  ;; We replace ONLY the pure formal-observation result to test the assembly
  ;; consumer's refusal to treat invalid reference evidence as certification.
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        prepare-case formal-observation/prepared-event-case
        replay-path formal-observation/observed-path-report]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [explain #(application/explain-with-execution-evidence
                       assembly {operation-id [evidence]})
              before @(:calls f)]
          (testing "an inadmissible formal case is refused, not downgraded"
            (with-redefs [formal-observation/prepared-event-case
                          (fn [_]
                            (throw (ex-info "Malformed admitted observation"
                                            {:error/kind
                                             :invalid-executor-event-observation
                                             :untrusted-secret (:request-id f)})))]
              (is (= :invalid-execution-reference-observation
                     (kind explain)))
              (let [error (try (explain) nil
                               (catch clojure.lang.ExceptionInfo e e))]
                (is (some? error))
                (is (= :invalid-executor-event-observation
                       (get-in (ex-data error) [:cause-kind])))
                (is (not (str/includes? (pr-str (ex-data error))
                                          (:request-id f)))
                    "Original error details must not leak in the public diagnostic."))))
          (testing "an incomplete physical prefix cannot produce a card"
            (with-redefs [formal-observation/prepared-event-case
                          (fn [observation]
                            (assoc-in (prepare-case observation)
                                      [:physical :submission-trace 0 :plan-index]
                                      999))]
              (is (= :noncorresponding-execution-reference
                     (kind explain)))))
          (testing "an altered origin is not an authenticated executor attempt"
            (with-redefs [formal-observation/prepared-event-case
                          (fn [observation]
                            (assoc (prepare-case observation)
                                   :observation-origin :untrusted-detached))]
              (is (= :noncorresponding-execution-reference
                     (kind explain)))))
          (testing "a spurious replay order is not correspondence"
            (with-redefs [formal-observation/observed-path-report
                          (fn [case]
                            (assoc (replay-path case) :checked-order []))]
              (is (= :noncorresponding-execution-reference
                     (kind explain)))))
          (is (= before @(:calls f))
              "Even rejected evidence must not invoke application callbacks.")
          (testing "genuine evidence still provides only bounded diagnosis"
            (let [card (observed-card (explain))]
              (is (= :selected-attempts-correspond
                     (get-in card [:source-derived-reference-check :status])))
              (is (false? (get-in card
                                  [:source-derived-reference-check :model-checked?])))
              (is (= :not-established (:universal-guarantees card))))
            (assert-unproved-policy (explain))))))))


;; These tests exercise the *public application-assurance consumer* of the
;; generic checker. Positive checking uses actual embedded XTDB and Recife/TLC.
;; The deliberately malformed checker maps below are boundary fault injections,
;; not evidence or model-checking results accepted as proof.
(def ^:private expected-formal-obligations
  #{:action-accounting :observation-chain :rejected-is-atomic
    :committed-write-set :outcome-check-consistency})

;; Application assurance must independently require these minimum disclosures;
;; neither a checked model nor a diagnostic report grants whole-app certification.
;; Deliberately keep literals separate from formal-check/assurance-scope so
;; removing a premise in the checker cannot silently weaken this regression.
(def ^:private required-formal-assumptions
  #{:complete-selected-finite-prestate-and-plans
    :serial-atomic-reference-semantics
    :faithful-reversible-tlc-token-encoding
    :correct-recife-tlc-execution})

(def ^:private required-formal-nonclaims
  #{:complete-application-operation-enumeration
    :generic-inductive-theorem
    :actual-xtdb-implementation-refinement
    :authorization-or-application-policy
    :server-browser-composition
    :independent-application-certificate})

(defn- formal-card [explanation]
  (get-in (observed-card explanation)
          [:observations 0 :finite-model-safety-check]))

(deftest actual-executor-observation-is-bounded-tlc-diagnostic-test
  (let [f (test-operation)
        {:keys [evidence prepared-send]} (invoke! f)]
    (is (server/prepared-send? prepared-send))
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [calls-before @(:calls f)
              ordinary (application/explain-with-execution-evidence
                        assembly {operation-id [evidence]})
              formal (application/explain-with-formal-execution-evidence
                      assembly {operation-id [evidence]})
              ordinary-card (observed-card ordinary)
              checked (formal-card formal)
              card (observed-card formal)
              summary (observed-summary formal)
              finite-summary (:finite-model-safety-check summary)
              printed (pr-str {:card card :summary summary})]
          (is (nil? (get-in ordinary-card
                            [:observations 0 :finite-model-safety-check]))
              "Ordinary explanation must not acquire TLC conclusions.")
          (is (= (dissoc (first (:observations card))
                         :finite-model-safety-check)
                 (first (:observations ordinary-card))))
          (is (= (:source-derived-reference-check card)
                 (:source-derived-reference-check ordinary-card)))
          (is (= :checked-finite-model (:status checked)))
          (is (= :bounded-reference-model-check-not-certificate
                 (:evidence-class checked)))
          (is (= :gesso.model.formal-transition/prepared-serial-v1
                 (:subject-kind checked)))
          (is (= :recife-0.22.0/tlc (:checker checked)))
          (is (pos-int? (:distinct-states checked)))
          (is (= expected-formal-obligations (:obligations checked)))
          (is (set/subset? required-formal-assumptions
                                   (:assumed checked))
              "Real TLC results must disclose every admitted premise.")
          (is (set/subset? required-formal-nonclaims
                                   (:not-established checked))
              "Real TLC results must disclose every boundary not proved.")
          (is (contains? (:not-established checked)
                         :actual-xtdb-implementation-refinement))
          (is (contains? (:not-established checked)
                         :authorization-or-application-policy))
          (is (contains? (:not-established checked)
                         :server-browser-composition))
          (is (contains? (:not-established checked)
                         :independent-application-certificate))
          (is (= :selected-finite-subjects-checked
                 (:status finite-summary)))
          (is (= 1 (:checked-subject-count finite-summary)))
          (is (= :exact-executor-observed-attempt
                 (:subject-origin finite-summary)))
          (is (= :not-established (:universal-guarantees finite-summary)))
          (is (= 1 (:observation-count summary)))
          (is (= :bounded-observation-not-proof (:assurance-class card)))
          (is (= :finite-reference-replay-not-formal-proof
                 (get-in card [:source-derived-reference-check :assurance-class])))
          (is (false? (get-in card [:source-derived-reference-check
                                    :model-checked?])))
          (is (= :not-established (:universal-guarantees card)))
          (is (not (str/includes? printed (:request-id f))))
          (is (not (str/includes? printed "v817-principal-private")))
          (is (not (str/includes? printed
                                  (str (:command-id (:command f))))))
          (doseq [secret [:receipt :binding :plan :submission-trace
                          :initial-state :documents]]
            (is (not (str/includes? printed (str secret)))))
          (is (= calls-before @(:calls f))
              "Reporting must not execute application callbacks.")
          (assert-unproved-policy formal))))))

(deftest formal-path-rejects-impostors-before-checker-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        actual-checks (atom 0)]
    (with-application
      (:executor f)
      (fn [{:keys [assembly report]}]
        (with-redefs [formal-check/check-prepared!
                      (fn [_]
                        (swap! actual-checks inc)
                        (throw (ex-info "Checker was not supposed to run" {})))]
          (doseq [forgery [(Object.)
                          (assurance/inspect-evidence evidence)
                          (assoc (assurance/inspect-evidence evidence)
                                 :status :bounded-execution-correspondence)]]
            (is (= :unrecognized-execution-observation
                   (kind #(application/explain-with-formal-execution-evidence
                           assembly {operation-id [forgery]})))))
          (doseq [invalid [nil [] {operation-id []} {operation-id [nil]}
                           {:operation/unknown [evidence]}]]
            (is (= :invalid-execution-observation-request
                   (kind #(application/explain-with-formal-execution-evidence
                           assembly invalid)))))
          (is (= :invalid-or-stale-observation-assembly
                 (kind #(application/explain-with-formal-execution-evidence
                         report {operation-id [evidence]}))))
          (is (zero? @actual-checks)
              "An untrusted handle must never reach the model checker.")
          (let [ordinary (application/explain-with-execution-evidence
                          assembly {operation-id [evidence]})
                empty-result (application/explain-with-formal-execution-evidence
                              assembly {})]
            (is (= :selected-attempts-correspond
                   (get-in (observed-card ordinary)
                           [:source-derived-reference-check :status])))
            (is (= 0 (get-in (observed-summary empty-result)
                             [:finite-model-safety-check :checked-subject-count])))
            (is (= :not-established
                   (get-in (observed-summary empty-result)
                           [:finite-model-safety-check :universal-guarantees])))
            (is (zero? @actual-checks))))))))

(deftest formal-checker-failures-and-incomplete-reports-fail-closed-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        ;; A structurally plausible result supplies a starting point only for
        ;; negative boundary mutation. It must never be offered as positive
        ;; evidence that TLC actually executed.
        shape {:status :checked-finite-model
               :evidence-class :bounded-reference-model-check-not-certificate
               :subject-kind :gesso.model.formal-transition/prepared-serial-v1
               :checker :recife-0.22.0/tlc
               :distinct-states 2
               :obligations expected-formal-obligations
               :assumed #{:serial-atomic-reference-semantics}
               :not-established #{:actual-xtdb-implementation-refinement}}
        invalid [[:nil-result (constantly nil)]
                 [:missing-status #(dissoc % :status)]
                 [:false-status #(assoc % :status :failed)]
                 [:missing-evidence-class #(dissoc % :evidence-class)]
                 [:certificate-forgery #(assoc % :evidence-class
                                               :application-certified)]
                 [:wrong-subject-kind #(assoc % :subject-kind :arbitrary)]
                 [:wrong-checker #(assoc % :checker :assertions-only)]
                 [:missing-state-count #(dissoc % :distinct-states)]
                 [:zero-states #(assoc % :distinct-states 0)]
                 [:noninteger-states #(assoc % :distinct-states 1.5)]
                 [:missing-obligations #(dissoc % :obligations)]
                 [:partial-obligations #(update % :obligations disj
                                                :observation-chain)]
                 [:invented-obligation #(update % :obligations conj
                                                 :unverified-application-rule)]
                 [:missing-assumptions #(dissoc % :assumed)]
                 [:assumptions-not-a-set #(assoc % :assumed [])]
                 [:missing-limits #(dissoc % :not-established)]
                 [:limits-not-a-set #(assoc % :not-established [])]
                 [:unsupported-limit #(update % :not-established conj
                                             :arbitrary-assurance)]]]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [explain #(application/explain-with-formal-execution-evidence
                       assembly {operation-id [evidence]})
              before @(:calls f)]
          (doseq [[label alter] invalid]
            (testing (name label)
              (with-redefs [formal-check/check-prepared!
                            (fn [_] (alter shape))]
                (is (= :finite-model-check-failed (kind explain))))))
          (with-redefs [formal-check/check-prepared!
                        (fn [_]
                          (throw (ex-info "private XTDB document content"
                                          {:document (:request-id f)
                                           :principal "v817-principal-private"})))]
            (let [error (try (explain) nil
                             (catch clojure.lang.ExceptionInfo e e))]
              (is (some? error))
              (is (= :finite-model-check-failed
                     (:error/kind (ex-data error))))
              (is (not (str/includes? (pr-str (ex-data error))
                                      (:request-id f))))
              (is (not (str/includes? (pr-str (ex-data error))
                                      "v817-principal-private")))))
          (is (= before @(:calls f))
              "Checker faults must not execute domain code."))))))

(deftest model-checked-physical-witness-does-not-certify-false-projection-test
  (let [f (test-operation {:false-projection? true})
        {:keys [prepared-send evidence]} (invoke! f)]
    (is (= {:request/owner "deliberately-false-projection"}
           (get-in prepared-send [:settlement :authoritative :projection])))
    (is (= "v817-principal-private"
           (:owner (row :requests (:request-id f)))))
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [result (application/explain-with-formal-execution-evidence
                      assembly {operation-id [evidence]})
              checked (formal-card result)]
          (is (= :checked-finite-model (:status checked)))
          (is (= expected-formal-obligations (:obligations checked)))
          (is (= :not-established
                 (:universal-guarantees (observed-card result))))
          (is (contains? (:not-established (observed-summary result))
                         :truthful-application-projections))
          (assert-unproved-policy result))))))

(deftest formal-evidence-is-bound-to-one-registered-executor-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        original (:executor f)
        cloned (with-meta original (assoc (meta original) :copied true))
        impersonator (with-meta (fn [ctx] (original ctx)) (meta original))
        alternate (adapter/build-executor
                   {:operation operation-id
                    :resolved-operation (:model f)
                    :submission-mode! (fn [_] :submit)
                    :project! (fn [_ _]
                                (throw (ex-info "Must not invoke projection" {})))})
        checker-invocations (atom 0)]
    (is (nil? (adapter/construction-evidence cloned)))
    (is (nil? (adapter/construction-evidence impersonator)))
    (is (some? (adapter/construction-evidence alternate)))
    (with-redefs [formal-check/check-prepared!
                  (fn [_]
                    (swap! checker-invocations inc)
                    (throw (ex-info "Must not verify wrong executor" {})))]
      (doseq [[label wrong] [[:cloned-fn cloned]
                             [:copied-metadata impersonator]
                             [:separately-registered-adapter alternate]]]
        (testing (name label)
          (with-application
            wrong
            (fn [{:keys [assembly]}]
              (is (= :unrecognized-execution-observation
                     (kind #(application/explain-with-formal-execution-evidence
                             assembly {operation-id [evidence]}))))))))
      (is (zero? @checker-invocations)
          "Even a second legitimate executor may not replay the first handle."))))

(deftest missing-finite-check-scope-disclosures-fail-closed-test
  ;; Run the consumer against a genuine executor-bound observation. The mock
  ;; checker results below are boundary fault injections, never proof evidence.
  ;; Removing any single required premise/nonclaim must refuse the entire
  ;; enhanced explanation, instead of returning a weaker or certified card.
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        shape {:status :checked-finite-model
               :evidence-class :bounded-reference-model-check-not-certificate
               :subject-kind :gesso.model.formal-transition/prepared-serial-v1
               :checker :recife-0.22.0/tlc
               :distinct-states 2
               :obligations expected-formal-obligations
               :assumed required-formal-assumptions
               :not-established required-formal-nonclaims}]
    (is (= required-formal-assumptions
           (:assumed formal-check/assurance-scope))
        "The independent contract must track the checker's declared premises.")
    (is (= required-formal-nonclaims
           (:not-established formal-check/assurance-scope))
        "The independent contract must track the checker's declared limits.")
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [explain #(application/explain-with-formal-execution-evidence
                       assembly {operation-id [evidence]})
              before @(:calls f)]
          (doseq [assumption (sort (:assumed shape))]
            (testing (str "omitted required premise " assumption)
              (with-redefs [formal-check/check-prepared!
                            (fn [_] (update shape :assumed disj assumption))]
                (is (= :finite-model-check-failed (kind explain))))))
          (doseq [nonclaim (sort (:not-established shape))]
            (testing (str "omitted required limitation " nonclaim)
              (with-redefs [formal-check/check-prepared!
                            (fn [_] (update shape :not-established disj nonclaim))]
                (is (= :finite-model-check-failed (kind explain))))))
          (doseq [k [:assumed :not-established]]
            (testing (str "empty disclosures " k)
              (with-redefs [formal-check/check-prepared!
                            (fn [_] (assoc shape k #{}))]
                (is (= :finite-model-check-failed (kind explain))))))
          (testing "swapping premises and nonclaims cannot satisfy either"
            (with-redefs [formal-check/check-prepared!
                          (fn [_] (assoc shape
                                    :assumed required-formal-nonclaims
                                    :not-established required-formal-assumptions))]
              (is (= :finite-model-check-failed (kind explain)))))
          (testing "additional conservative disclosures survive reporting"
            (with-redefs [formal-check/check-prepared!
                          (fn [_] (-> shape
                                      (update :assumed conj :extra-assumption)
                                      (update :not-established conj
                                              :extra-unproved-boundary)))]
              (let [report (explain)
                    checked (formal-card report)]
                (is (= :checked-finite-model (:status checked)))
                (is (set/subset? required-formal-assumptions
                                 (:assumed checked)))
                (is (set/subset? required-formal-nonclaims
                                 (:not-established checked)))
                (is (contains? (:assumed checked) :extra-assumption))
                (is (contains? (:not-established checked)
                               :extra-unproved-boundary))
                (is (= :bounded-observation-not-proof
                       (:assurance-class (observed-card report))))
                (assert-unproved-policy report))))
          (is (= before @(:calls f))
              "A failed or successful diagnostic must not call business code."))))))

;; v959: external evidence is authentic, but the checker itself is deliberately
;; fault-injected. Its ex-data may contain private documents, an opaque local
;; diagnostic handle and hostile fields. The public application boundary may
;; forward ONLY the small checked vocabulary. None of these stubbed checker
;; outcomes constitutes actual TLC evidence or a production certificate.
(defn- caught-formal-failure [thunk]
  (try (thunk)
       nil
       (catch clojure.lang.ExceptionInfo e e)))

(deftest formal-counterexample-propagates-safe-attribution-not-private-state-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        secret (str "private-row-" (:request-id f))
        inspection-handle (java.util.UUID/randomUUID)
        backend (ex-info
                 (str "private-backend-message-" secret)
                 {:error/type :gesso.model.formal-check/invalid-formal-check
                  :error/kind :model-check-failed
                  :failure/class :invariant-counterexample
                  :diagnostic/handle inspection-handle
                  :private-documents [{:xt/id secret :owner secret}]
                  :diagnostic/summary
                  {:failed-obligation :outcome-check-consistency
                   :distinct-states 4
                   :private-plan {:secret secret}
                   :transition-prefix
                   [{:state-index 1
                     :private-document secret
                     :observations [{:plan-index 0 :status :committed
                                     :private-command secret}]}
                    {:state-index 2
                     :observations [{:plan-index 1 :status :rejected
                                     :private-reason secret}]}]}})]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              error (with-redefs [formal-check/check-prepared!
                                  (fn [_] (throw backend))]
                      (caught-formal-failure
                       #(application/explain-with-formal-execution-evidence
                         assembly {operation-id [evidence]})))
              data (some-> error ex-data)
              exposed (pr-str data)]
          (is (some? error))
          (is (identical? backend (.getCause error))
              "Only a trusted JVM caller may follow the local cause.")
          (is (= :finite-model-check-failed (:error/kind data)))
          (is (= operation-id (:operation data)))
          (is (= 0 (:observation-index data)))
          (is (= {:failure/class :invariant-counterexample
                  :failed-obligation :outcome-check-consistency
                  :distinct-states 4
                  :transition-prefix
                  [{:state-index 1
                    :observations [{:plan-index 0 :status :committed}]}
                   {:state-index 2
                    :observations [{:plan-index 1 :status :rejected}]}]}
                 (:formal/failure data)))
          (doseq [forbidden [secret "private-backend-message"
                             "private-documents" "private-plan"
                             "private-command" "private-reason"
                             "diagnostic/handle" (str inspection-handle)]]
            (is (not (str/includes? exposed forbidden))
                (str "The outer failure must not reveal " forbidden)))
          (is (= before @(:calls f))
              "Reporting checker failures must not re-invoke domain code."))))))

(deftest formal-failure-classification-spoofing-and-bounds-stay-closed-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        secret "not-for-application-errors"
        valid-type :gesso.model.formal-check/invalid-formal-check
        cases
        [[:unrecognized-error-type
          {:error/type :untrusted/forged
           :failure/class :invariant-counterexample
           :diagnostic/summary {:failed-obligation :action-accounting
                                :distinct-states 9}}
          {:failure/class :unclassified-checker-failure}]
         [:unknown-class-and-obligation
          {:error/type valid-type
           :failure/class :certified-application
           :diagnostic/summary {:failed-obligation :application/certified}}
          {:failure/class :unclassified-checker-failure}]
         [:recognized-backend-failure
          {:error/type valid-type
           :failure/class :backend-error
           :diagnostic/summary {:failed-obligation :unrecognized-rule
                                :distinct-states -1}}
          {:failure/class :backend-error}]
         [:oversized-finite-prefix
          {:error/type valid-type
           :failure/class :model-counterexample
           :diagnostic/summary
           {:failed-obligation :observation-chain
            :distinct-states 1000000001
            :transition-prefix
            (vec
             (repeat 40
                     {:state-index 2000000
                      :document secret
                      :observations
                      (vec (repeat 12 {:plan-index 1000
                                       :status :private-success
                                       :secret secret}))}))}}
          {:failure/class :model-counterexample
           :failed-obligation :observation-chain}]]]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [explain #(application/explain-with-formal-execution-evidence
                       assembly {operation-id [evidence]})
              before @(:calls f)]
          (doseq [[label backend-data expected] cases]
            (testing (name label)
              (let [backend (ex-info secret (assoc backend-data :secret secret))
                    error (with-redefs [formal-check/check-prepared!
                                        (fn [_] (throw backend))]
                            (caught-formal-failure explain))
                    data (some-> error ex-data)
                    failure (:formal/failure data)]
                (is (some? error))
                (is (identical? backend (.getCause error)))
                (is (= :finite-model-check-failed (:error/kind data)))
                (is (= expected (select-keys failure (keys expected))))
                (is (not (str/includes? (pr-str data) secret)))
                (when (= label :oversized-finite-prefix)
                  (is (not (contains? failure :distinct-states)))
                  (is (= 32 (count (:transition-prefix failure))))
                  (is (every? #(= {:observations (vec (repeat 6 {}))} %)
                              (:transition-prefix failure)))))))
          (let [error (with-redefs [formal-check/check-prepared!
                                    (fn [_] nil)]
                        (caught-formal-failure explain))]
            (is (some? error))
            (is (nil? (.getCause error))
                "Malformed checker results have no fabricated backend cause.")
            (is (= :invalid-checker-result
                   (get-in (ex-data error)
                           [:formal/failure :failure/class])))
            (is (= :finite-model-check-failed
                   (:error/kind (ex-data error)))))
          (is (= before @(:calls f))
              "Checker failures cannot cause a domain callback replay."))))))

;; v961: provenance is intentionally only a finite, recognized statement of
;; checker scope. These tests exercise the *public application boundary* with
;; genuine registered executor-observation handles and fault-injected checker
;; exceptions. A correctly spelled provenance keyword is not authenticated
;; external evidence or an application proof.
(deftest formal-failure-provenance-is-bounded-and-not-a-certificate-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        secret (str "private-provenance-" (:request-id f))
        handle (java.util.UUID/randomUUID)
        standard {:error/type :gesso.model.formal-check/invalid-formal-check
                  :error/kind :model-check-failed
                  :failure/class :invariant-counterexample
                  :diagnostic/handle handle
                  :diagnostic/summary
                  {:failure/class :invariant-counterexample
                   :failed-obligation :outcome-check-consistency
                   :distinct-states 7
                   :source-boundary :prepared-transaction-finite-model
                   :subject-kind :gesso.model.formal-transition/prepared-serial-v1
                   :checker :recife-0.22.0/tlc
                   :selected-plan-count 2
                   :private-document secret
                   :transition-prefix
                   [{:state-index 0
                     :private-document secret
                     :observations [{:plan-index 0 :status :committed
                                     :private-command secret}]}]}}
        expected {:failure/class :invariant-counterexample
                  :failed-obligation :outcome-check-consistency
                  :distinct-states 7
                  :source-boundary :prepared-transaction-finite-model
                  :subject-kind :gesso.model.formal-transition/prepared-serial-v1
                  :checker :recife-0.22.0/tlc
                  :selected-plan-count 2
                  :transition-prefix
                  [{:state-index 0
                    :observations [{:plan-index 0 :status :committed}]}]}]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              backend (ex-info (str "sensitive-" secret)
                               (assoc standard :private-data secret))
              error (with-redefs [formal-check/check-prepared!
                                  (fn [_] (throw backend))]
                      (caught-formal-failure
                       #(application/explain-with-formal-execution-evidence
                         assembly {operation-id [evidence]})))
              data (some-> error ex-data)]
          (is (some? error))
          (is (identical? backend (.getCause error)))
          (is (= :finite-model-check-failed (:error/kind data)))
          (is (= operation-id (:operation data)))
          (is (= 0 (:observation-index data)))
          (is (= expected (:formal/failure data))
              "All admitted provenance is named and bounded.")
          (doseq [private [secret "sensitive-" "diagnostic/handle"
                           "private-document" "private-command"
                           (str handle) "private-data"]]
            (is (not (str/includes? (pr-str data) private))
                (str "Provenance metadata must not reveal " private)))
          (is (= before @(:calls f))
              "Failure attribution must not re-execute business callbacks."))))))

(deftest formal-failure-provenance-spoofing-and-class-conflicts-fail-closed-test
  (let [f (test-operation)
        {:keys [evidence]} (invoke! f)
        secret "secret-provenance-field"
        valid-type :gesso.model.formal-check/invalid-formal-check
        base-summary {:failure/class :model-counterexample
                      :failed-obligation :action-accounting
                      :source-boundary :recife-tlc-execution
                      :subject-kind :gesso.model.formal-transition/prepared-serial-v1
                      :checker :recife-0.22.0/tlc
                      :selected-plan-count 6}
        base {:error/type valid-type
              :failure/class :model-counterexample
              :diagnostic/summary base-summary}
        cases
        [[:valid-upper-bound base
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc
           :selected-plan-count 6}]
         [:unrecognized-boundary
          (assoc-in base [:diagnostic/summary :source-boundary] :fake/application-certified)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc
           :selected-plan-count 6}]
         [:unrecognized-subject
          (assoc-in base [:diagnostic/summary :subject-kind] :fake/certified)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :checker :recife-0.22.0/tlc
           :selected-plan-count 6}]
         [:unrecognized-checker
          (assoc-in base [:diagnostic/summary :checker] :fake/formal-prover)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :selected-plan-count 6}]
         [:missing-plan-count
          (update base :diagnostic/summary dissoc :selected-plan-count)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc}]
         [:zero-plan-count
          (assoc-in base [:diagnostic/summary :selected-plan-count] 0)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc}]
         [:excessive-plan-count
          (assoc-in base [:diagnostic/summary :selected-plan-count] 7)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc}]
         [:noninteger-plan-count
          (assoc-in base [:diagnostic/summary :selected-plan-count] 2.0)
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting
           :source-boundary :recife-tlc-execution
           :subject-kind :gesso.model.formal-transition/prepared-serial-v1
           :checker :recife-0.22.0/tlc}]
         [:conflicting-class
          (assoc-in base [:diagnostic/summary :failure/class] :backend-error)
          {:failure/class :unclassified-checker-failure}]
         [:unrecognized-error-type
          (assoc base :error/type :fake/formal-check)
          {:failure/class :unclassified-checker-failure}]
         [:unknown-failure-class
          (assoc base :failure/class :certified-application)
          {:failure/class :unclassified-checker-failure}]
         [:legacy-no-summary-provenance
          (assoc base :diagnostic/summary
                 {:failed-obligation :action-accounting})
          {:failure/class :model-counterexample
           :failed-obligation :action-accounting}]]]
    (with-application
      (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              explain #(application/explain-with-formal-execution-evidence
                        assembly {operation-id [evidence]})]
          (doseq [[label supplied expected] cases]
            (testing (name label)
              (let [backend (ex-info secret (assoc supplied :hidden secret))
                    error (with-redefs [formal-check/check-prepared!
                                        (fn [_] (throw backend))]
                            (caught-formal-failure explain))
                    data (some-> error ex-data)]
                (is (some? error))
                (is (identical? backend (.getCause error)))
                (is (= :finite-model-check-failed (:error/kind data)))
                (is (= expected (:formal/failure data)))
                (is (not (str/includes? (pr-str data) secret))))))
          (is (= before @(:calls f))
              "Provenance classification never re-runs the domain operation."))))))

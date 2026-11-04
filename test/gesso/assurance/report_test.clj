(ns gesso.assurance.report-test
  "v1258: permanent adversarial end-to-end gate for the source-bound Claim
   assurance report. All positive paths use an actually issued v1251 executor,
   original v1255 Claim sources and a live v1237 certificate. A real closed
   ApplicationAssembly and a single-node XTDB submission are exercised here.

   Proof means only a conditional abstract derivation. An untrusted observation,
   an XTDB commit, a route snapshot, or a copied report NEVER turns that proof
   into universal invariant preservation, policy correctness, physical
   refinement or deployed-route attestation. Negative tests assert the error
   boundary, not merely that some exception occurred."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.certified-claim-fixture :as claim]
            [gesso.assurance.report :as report]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.certified-contract :as binding]
            [gesso.live.application-preflight.fixture :as application-fixture]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private report-error :gesso.assurance.report/invalid-report)
(def ^:private plan-error :gesso.model.certified-plan/invalid-certified-plan)
(def ^:private conditional-premise
  :gesso.assurance.proof.derivation/abstract-relation-match)

(defn- environment
  ([] (environment {}))
  ([overrides]
   (claim/new-claim-environment!
    (merge {:request-id 1258
            :actor :helper/alice
            :organization :organization/one
            :read-current! (fn [_ _] nil)
            :authorization! (fn [_ _] {:auth/decision :permit})
            :read-record! (fn [_ _] nil)
            :may-disclose? (fn [_ _ _] true)}
           overrides))))

(defn- report-source [env]
  {:executor (:executor env)
   :current-source (:source env)
   :obligation-source claim/obligation-source
   :application-binding nil
   :test-observations []})

(defn- checked [env]
  (report/build-report! (report-source env)))

(defn- observation
  ([id] (observation id :embedded-xtdb :passed "v1256 local observation"))
  ([id scope result reference]
   {:id id :scope scope :result result :reference reference}))

(defn- rejected!
  "Assert an exact failure class/reason; an unrelated exception is not success."
  [error-type reason thunk]
  (let [failure (try
                  (thunk)
                  (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo failure)
        (str "Expected typed rejection " reason ", got " (pr-str failure)))
    (when (instance? clojure.lang.ExceptionInfo failure)
      (let [data (ex-data failure)]
        (is (= error-type (:error/type data)))
        (is (= reason (:reason data)))
        data))))

(defn- assert-partial! [r]
  (is (= report/report-type (:type r)))
  (is (= report/report-version (:version r)))
  (is (= claim/claim-operation (:operation r)))
  (is (= :original-sources-and-live-handles-rechecked (:checking r)))
  (is (= :partial-conditional-assurance (:verdict r)))
  (is (false? (:full-success? r)))
  (is (= :not-established (:production-certification r)))
  (is (= :none (:transaction-authority r)))
  (is (= :not-issued (:certificate r)))
  (is (= :checked-conditional-abstract-derivation
         (get-in r [:classification :proved :status])))
  (is (= :matching-abstract-transition-only
         (get-in r [:classification :proved :scope])))
  (is (= :not-proved
         (get-in r [:classification :proved :invariant-preservation])))
  (is (= :not-established
         (get-in r [:classification :proved :checker-meta-soundness])))
  (is (= :issued-pre-submit-abstract-plan-gate
         (get-in r [:classification :runtime-enforced :status])))
  (is (= :not-established
         (get-in r [:classification :runtime-enforced
                    :physical-commit-and-authorization])))
  (is (= :caller-reported-only
         (get-in r [:classification :tested :status])))
  (is (false? (get-in r [:classification :tested :authenticated?])))
  (is (= :no-universal-production-correspondence
         (get-in r [:classification :tested :claim])))
  (is (= :unverified-premises
         (get-in r [:classification :trusted :status])))
  (is (= :not-established
         (get-in r [:classification :unsupported :status])))
  (is (contains? (get-in r [:classification :proved :open-premises])
                 conditional-premise))
  (is (contains? (get-in r [:classification :open :obligations])
                 :source-to-physical-xtdb-correspondence))
  (is (contains? (get-in r [:classification :open :obligations])
                 :invariant-preservation-not-yet-proved))
  (is (contains? (get-in r [:classification :open :obligations])
                 :test-evidence-authentication-not-established))
  (is (contains? (get-in r [:classification :trusted :assumptions])
                 :all-authoritative-effect-paths-are-enumerated))
  (is (contains? (get-in r [:classification :unsupported :claims])
                 :universal-xtdb-implementation-refinement))
  (is (= :not-established (get-in r [:application :deployment-adoption])))
  r)

(deftest genuine-source-chain-is-a-conditional-report
  (let [env (environment)
        r (assert-partial! (checked env))]
    (is (re-matches #"sha256:[a-f0-9]{64}"
                    (get-in r [:identity :report-fingerprint])))
    (is (= :not-checked (get-in r [:identity :artifact :status])))
    (is (nil? (get-in r [:identity :artifact :browser-artifact-digest])))
    (is (= :not-checked (get-in r [:application :status])))
    (is (= #{} (get-in r [:application :certified-operation-ids])))
    (is (seq (get-in r [:classification :open :initial-and-preservation
                       :initial-state])))
    (is (seq (get-in r [:classification :open :initial-and-preservation
                       :preservation])))))

(deftest repeated-checks-preserve-semantic-identity
  (let [env (environment)
        one (checked env)
        two (checked env)]
    (is (= one two))
    (is (= (get-in one [:identity :report-fingerprint])
           (get-in two [:identity :report-fingerprint])))
    (is (= (get-in one [:identity :certificate-fingerprint])
           (get-in two [:identity :certificate-fingerprint])))))

(deftest issued-executors-are-not-transferable-proof-reports
  (let [first-env (environment)
        second-env (environment)
        one (checked first-env)
        two (checked second-env)]
    (is (not (identical? (:executor first-env) (:executor second-env))))
    (is (= (get-in one [:classification :proved :scope])
           (get-in two [:classification :proved :scope])))
    (is (false? (:full-success? one)))
    (is (false? (:full-success? two)))))

(deftest formatting-identifies-each-trust-boundary
  (let [text (report/format-report (checked (environment)))]
    (doseq [token ["PROVED:" "RUNTIME-ENFORCED:" "TESTED:"
                   "TRUSTED:" "OPEN:" "UNSUPPORTED:"
                   "FULL SUCCESS: false" "NOT established"
                   "invariants NOT proved" "never a credential"]]
      (is (str/includes? text token) (str "Missing report warning " token)))))

(deftest format-is-not-a-validation-or-authentication-gate
  (let [r (checked (environment))]
    (is (string? (report/format-report (assoc r :full-success? true))))
    (rejected! report-error :invalid-report-for-format
               #(report/format-report {:type :other :version 1}))
    (rejected! report-error :invalid-report-for-format
               #(report/format-report nil))))

(deftest reported-passing-test-never-upgrades-theorem-or-authority
  (let [env (environment)
        before (checked env)
        after (report/build-report!
               (assoc (report-source env) :test-observations
                      [(observation :tests/claim-commit)]))]
    (assert-partial! after)
    (is (= [(observation :tests/claim-commit)]
           (get-in after [:classification :tested :observations])))
    (is (not= (get-in before [:identity :report-fingerprint])
              (get-in after [:identity :report-fingerprint])))
    (is (= (get-in before [:identity :certificate-fingerprint])
           (get-in after [:identity :certificate-fingerprint])))
    (is (= (get-in before [:classification :proved])
           (get-in after [:classification :proved])))))

(deftest failed-and-inconclusive-observations-are-reported-not-disguised
  (let [env (environment)
        observations [(observation :tests/failure :real-browser :failed "browser report")
                      (observation :tests/unknown :node-cljs :inconclusive "node report")]
        r (report/build-report!
           (assoc (report-source env) :test-observations observations))]
    (assert-partial! r)
    (is (= observations (get-in r [:classification :tested :observations])))
    (is (false? (get-in r [:classification :tested :authenticated?])))))

(deftest claimed-test-results-cannot-discharge-invariant-preservation
  (let [env (environment)
        all-scopes [:embedded-xtdb :real-browser :node-cljs :jvm :other]
        obs (mapv (fn [i scope] (observation (keyword "tests" (str i))
                                             scope :passed "not-a-receipt"))
                  (range) all-scopes)
        r (report/build-report! (assoc (report-source env)
                                       :test-observations obs))]
    (assert-partial! r)
    (is (= 5 (count (get-in r [:classification :tested :observations]))))))

(deftest invalid-observation-list-shapes-fail-closed
  (let [source (report-source (environment))]
    (doseq [bad [nil '() '({:id :tests/a}) (seq [(observation :tests/a)])
                 (with-meta [] {:trusted true})
                 (vec (repeat 65 (observation :tests/a)))]]
      (rejected! report-error :invalid-test-observations
                 #(report/build-report! (assoc source :test-observations bad))))))

(deftest duplicate-observation-identity-cannot-be-counted-twice
  (let [source (report-source (environment))]
    (rejected! report-error :duplicate-test-observation-id
               #(report/build-report!
                 (assoc source :test-observations
                        [(observation :tests/same)
                         (observation :tests/same :jvm :failed "other run")])))))

(deftest observational-identity-must-be-qualified
  (let [source (report-source (environment))]
    (doseq [id [:plain-id "test/id" 123 nil]]
      (rejected! report-error :invalid-test-observation-id
                 #(report/build-report! (assoc source :test-observations
                                               [(observation id)]))))))

(deftest observation-scope-and-result-must-be-supported
  (let [source (report-source (environment))]
    (rejected! report-error :invalid-test-observation-scope
               #(report/build-report! (assoc source :test-observations
                                             [(observation :tests/a :unspecified
                                                           :passed "case")])))
    (rejected! report-error :invalid-test-observation-result
               #(report/build-report! (assoc source :test-observations
                                             [(observation :tests/a :jvm
                                                           :certified "case")])))))

(deftest observation-reference-cannot-be-blank-or-unbounded
  (let [source (report-source (environment))]
    (doseq [reference [nil 4 "" "    " (apply str (repeat 513 "x"))]]
      (rejected! report-error :invalid-test-observation-reference
                 #(report/build-report! (assoc source :test-observations
                                               [(observation :tests/a :jvm
                                                             :passed reference)]))))))

(deftest observation-cannot-carry-forged-proof-or-receipt-keys
  (let [source (report-source (environment))]
    (doseq [k [:proof :certificate :validated? :authority :attestation]]
      (rejected! report-error :wrong-keys
                 #(report/build-report! (assoc source :test-observations
                                               [(assoc (observation :tests/a) k true)]))))))

(deftest observation-metadata-is-not-proof-identity
  (let [source (report-source (environment))]
    (rejected! report-error :metadata-not-allowed
               #(report/build-report! (assoc source :test-observations
                                             [(with-meta (observation :tests/a)
                                                {:certified true})])))))

(deftest forged-top-level-proof-flags-are-rejected
  (let [source (report-source (environment))]
    (doseq [k [:valid? :proved? :certified? :full-success? :certificate
               :production-certification :trusted-test-receipt]]
      (rejected! report-error :wrong-keys
                 #(report/build-report! (assoc source k true))))))

(deftest input-maps-with-metadata-are-not-authority
  (let [source (report-source (environment))]
    (rejected! report-error :metadata-not-allowed
               #(report/build-report! (with-meta source {:valid? true})))
    (rejected! report-error :expected-plain-map
               #(report/build-report! (java.util.HashMap. source)))))

(deftest copied-output-cannot-replace-the-original-input-source
  (let [env (environment)
        r (checked env)]
    (rejected! report-error :wrong-keys
               #(report/build-report! r))
    (rejected! report-error :wrong-keys
               #(report/build-report! (merge (report-source env)
                                             {:proof-report r})))))

(deftest unissued-executors-cannot-inherit-copied-report-identity
  (let [env (environment)
        original (:executor env)
        imitation (with-meta (fn [& _] nil) (meta original))
        source (report-source env)]
    (is (not (plan/checked-plan-operation? imitation)))
    (rejected! report-error :unregistered-certified-executor
               #(report/build-report! (assoc source :executor imitation)))
    (rejected! report-error :unregistered-certified-executor
               #(report/build-report! (assoc source :executor nil)))))

(deftest legitimate-other-executor-cannot-use-wrong-source
  (let [env (environment)
        other (environment)
        source (report-source env)]
    (is (plan/checked-plan-operation? (:executor other)))
    (rejected! plan-error :stale-plan-source
               #(report/build-report! (assoc source :executor (:executor other))))))

(deftest replacing-the-opaque-authorization-callback-invalidates-the-report
  (let [env (environment)
        source (report-source env)
        replaced (assoc-in source [:current-source :transition-inputs!]
                           (fn [_ _] {:input/authorized true}))]
    (rejected! plan-error :stale-plan-source
               #(report/build-report! replaced))))

(deftest valid-but-different-derivation-is-not-the-original-proof
  (let [env (environment)
        source (report-source env)
        changed (assoc-in source [:current-source :certified-source
                                  :proof-source :steps 0 :id] :step/another)]
    (rejected! plan-error :stale-plan-source
               #(report/build-report! changed))))

(deftest swapped-dependency-graph-invalidates-the-report
  (let [env (environment)
        source (report-source env)
        changed (assoc-in source [:current-source :certified-source
                                  :current-graph :roots]
                          #{:example/claim-authorization})]
    (rejected! plan-error :stale-plan-source
               #(report/build-report! changed))))

(deftest changed-construction-requires-rechecking-entire-chain
  (let [env (environment)
        source (report-source env)
        changed (assoc-in source [:current-source :construction
                                  :replacements :request/status]
                          [:literal :open])]
    (rejected! plan-error :stale-plan-source
               #(report/build-report! changed))))

(deftest missing-invariant-is-not-silently-treated-as-proved
  (let [env (environment)
        source (report-source env)
        fewer (update-in source [:obligation-source :invariants]
                         dissoc :claim/nonnegative-revision)]
    (let [r (report/build-report! fewer)]
      (assert-partial! r)
      (is (contains? (get-in r [:classification :open :obligations])
                     [:assumption :declared-invariant-set-coverage]))
      (is (not= (get-in (checked env) [:identity :obligation-fingerprint])
                (get-in r [:identity :obligation-fingerprint]))))))

(deftest changing-the-obligation-construction-is-not-a-certificate-reuse
  (let [env (environment)
        source (report-source env)]
    (rejected! report-error :obligation-construction-mismatch
               #(report/build-report!
                 (assoc-in source [:obligation-source :construction :writes]
                           #{:request/status})))))

(deftest forged-obligation-status-cannot-discharge-unproved-goals
  (let [env (environment)
        source (report-source env)]
    (rejected! :gesso.assurance.proof.obligation/invalid-obligation-source
               :wrong-keys
               #(report/build-report!
                 (assoc-in source [:obligation-source :invariants
                                   :claim/nonnegative-revision :proved?] true)))))

(deftest wrong-obligation-source-fails-even-with-valid-executor
  (let [env (environment)
        source (report-source env)
        changed (assoc source :obligation-source
                       (assoc claim/obligation-source
                              :construction (assoc claim/construction
                                                   :writes #{})))]
    (rejected! report-error :obligation-construction-mismatch
               #(report/build-report! changed))))

(deftest valid-plan-check-is-not-transaction-authority
  (let [env (environment)
        before {:xt/id 17 :request/status :open :request/owner nil
                :request/revision 0 :request/organization :organization/one
                :request/description "Fresh request"}
        witness (claim/check-candidate! before :helper/alice
                                        :organization/one {:auth/decision :permit})
        r (checked env)]
    (is (= :gesso.model.certified-plan/checked-pre-submit-candidate (:type witness)))
    (assert-partial! r)))

(deftest hidden-domain-write-cannot-produce-a-checked-candidate
  (let [before {:xt/id 17 :request/status :open :request/owner nil
                :request/revision 0 :request/organization :organization/one
                :request/description "Fresh request"}
        candidate (claim/candidate! before :helper/alice
                                    :organization/one {:auth/decision :permit})]
    (rejected! plan-error :frame-preservation-failed
               #(plan/check-candidate!
                 (assoc-in candidate [:plan :commands 0 :model/after
                                      :request/description] "hidden write")))))

(deftest stale-guard-never-becomes-a-proof-by-reporting
  (let [before {:xt/id 17 :request/status :claimed :request/owner :helper/bob
                :request/revision 1 :request/organization :organization/one
                :request/description "Fresh request"}
        candidate (claim/candidate! before :helper/alice
                                    :organization/one {:auth/decision :permit})]
    (rejected! plan-error :stale-update-check
               #(plan/check-candidate! candidate))
    (assert-partial! (checked (environment)))))

(deftest malformed-assignment-rejected-before-evidence-can-be-reported
  (let [before {:xt/id 17 :request/status :open :request/owner nil
                :request/revision 0 :request/organization :organization/one
                :request/description "Fresh request"}
        candidate (claim/candidate! before :helper/alice
                                    :organization/one {:auth/decision :permit})]
    (rejected! plan-error :declared-write-mismatch
               #(plan/check-candidate!
                 (assoc-in candidate [:plan :commands 0 :model/after
                                      :request/owner] :helper/bob)))))

(deftest full-application-assembly-still-leaves-deployment-open
  (let [env (environment)
        executor (:executor env)
        source (:source env)
        handler (binding/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result] result)})]
    (application-fixture/with-closed-application
      {:operations {claim/claim-operation
                    (application-fixture/trusted-operation
                     claim/claim-operation {:published-change-topics #{}})}}
      (fn [{:keys [assembly]}]
        (let [application-binding
              {:application-assembly assembly
               :semantic-route-handlers {claim/claim-operation handler}
               :certified-operations
               {claim/claim-operation {:executor executor :current-source source}}}
              r (report/build-report!
                 (assoc (report-source env) :application-binding
                        application-binding))]
          (assert-partial! r)
          (is (= :source-relative-route-checked (get-in r [:application :status])))
          (is (= #{claim/claim-operation}
                 (get-in r [:application :certified-operation-ids])))
          (is (re-matches #"[a-f0-9]{64}"
                          (get-in r [:identity :artifact :browser-artifact-digest])))
          (is (= :not-attested
                 (get-in r [:identity :artifact :physical-deployment]))))))))

(deftest replacement-handler-is-rejected-by-assembly-checker
  (let [env (environment)
        executor (:executor env)
        source (:source env)
        handler (binding/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result] result)})]
    (application-fixture/with-closed-application
      {:operations {claim/claim-operation
                    (application-fixture/trusted-operation
                     claim/claim-operation {:published-change-topics #{}})}}
      (fn [{:keys [assembly]}]
        (let [good {:application-assembly assembly
                    :semantic-route-handlers {claim/claim-operation handler}
                    :certified-operations
                    {claim/claim-operation {:executor executor :current-source source}}}
              bad (assoc-in good [:semantic-route-handlers claim/claim-operation]
                            (with-meta (fn [_] nil) (meta handler)))]
          (rejected! :gesso.live.application-preflight.certified-contract/invalid-certified-contract
                     :unissued-route-handler
                     #(report/build-report!
                       (assoc (report-source env) :application-binding bad))))))))

(deftest forged-assembly-report-is-not-a-valid-assembly-input
  (let [env (environment)
        fake {:application-assembly {:valid? true :operations #{claim/claim-operation}}
              :semantic-route-handlers {}
              :certified-operations {}}
        source (assoc (report-source env) :application-binding fake)]
    (rejected! :gesso.live.application-preflight.certified-contract/invalid-certified-contract
               :invalid-or-stale-application-assembly
               #(report/build-report! source))))

(deftest live-XTDB-claim-is-physical-evidence-not-a-proof
  ;; A real node and actual resolved-operation -> tx/transact! submission.
  ;; This test is intentionally NOT an entire concurrency/state-space proof.
  (with-open [db (xtn/start-node {})]
    (let [id 12580001
          original {:xt/id id
                    :request/status :open
                    :request/owner nil
                    :request/revision 0
                    :request/organization :organization/one
                    :request/description "physical fixture"}
          _ (xt/execute-tx db [[:put-docs :requests original]])
          rows (fn [table pk]
                 (first (xt/q db [(str "SELECT * FROM " (name table)
                                        " WHERE _id = ?") pk])))
          read-current (fn [_ _]
                         (when-let [raw (rows :requests id)]
                           ;; v1256 established this narrow, trusted normalization:
                           ;; XTDB SELECT * omits present-nil :request/owner.
                           (if (contains? raw :request/owner)
                             raw
                             (assoc raw :request/owner nil))))
          env (environment
               {:request-id id
                :read-current! read-current
                :read-record! (fn [_ command-id]
                                (rows :gesso_commands command-id))})
          issued {:command-id (identity/command-id
                               (str "v1258-" id "-" (random-uuid)))
                  :principal (identity/principal :helper/alice)
                  :operation claim/claim-operation
                  :arguments {:request-id id}}
          outcome ((:executor env) {:biff.xtdb/node db}
                   {:binding issued :mode :submit})
          physical (read-current nil nil)
          receipt (rows :gesso_commands (resolution/record-id issued))
          reported (report/build-report!
                    (assoc (report-source env) :test-observations
                           [(observation :tests/embedded-claim)]))]
      (is (= :incorporated (:status outcome)))
      (is (= :committed (get-in outcome [:transaction :commit/status])))
      (is (= :claimed (:request/status physical)))
      (is (= :helper/alice (:request/owner physical)))
      (is (= 1 (:request/revision physical)))
      (is (= :incorporated (:gesso.command/status receipt)))
      (is (= issued (:gesso.command/binding receipt)))
      (assert-partial! reported)
      (is (= :caller-reported-only
             (get-in reported [:classification :tested :status]))))))

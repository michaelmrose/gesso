(ns gesso.assurance.proof.xtdb-gateway-issued-application-observation-test
  "v1867: Real-XTDB original-executor candidate collection, one-use
   observation semantics and explicit non-certification boundaries."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.admission :as admission]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.checked-authorization-assembly :as checked-assembly]
            [gesso.assurance.proof.xtdb-gateway-issued-application-observation
             :as application-observation]
            [gesso.assurance.proof.xtdb-gateway-issued-application-recheck
             :as read-only-recheck]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial
             :as issued]
            [gesso.assurance.proof.xtdb-gateway-issued-snapshot-identity
             :as snapshot]
            [gesso.assurance.proof.xtdb-gateway-issued-row-history
             :as row-history]
            [gesso.assurance.proof.xtdb-gateway-issued-history-obligations
             :as history-obligations]
            [gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test]
            [gesso.live.application-preflight.fixture :as app-fixture]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt]))

(def ^:private parent-ns
  'gesso.assurance.proof.xtdb-gateway-issued-catalog-canonical-trial-test)
(def ^:private model-ns 'gesso.model.resolved-operation-test)
(defn- original [ns-name sym]
  (var-get (or (ns-resolve ns-name sym)
               (throw (ex-info "Missing original test fixture"
                               {:namespace ns-name :symbol sym})))))
(defn- data [] ((original parent-ns 'data)))
(defn- environment [d opts] ((original parent-ns 'environment) d opts))
(defn- original-issued-source [d e opts]
  ((original parent-ns 'original-issued-source) d e opts))
(defn- original-catalog [] ((original parent-ns 'original-catalog)))
(defn- physical-map [] ((original parent-ns 'physical-map)))
(defn- db [] ((original parent-ns 'db)))
(defn- seed! [& xs] (apply (original parent-ns 'seed!) xs))
(defn- read-request [id] ((original parent-ns 'read-request) id))
(defn- read-command [id] ((original parent-ns 'read-command) id))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- parent-fixture [run]
  ((original model-ns 'with-real-xtdb)
   (fn []
     (app-fixture/with-closed-application
      {:operations {:request/claim
                    (app-fixture/trusted-operation
                     :request/claim {:published-change-topics #{}})}}
      (fn [{:keys [assembly]}]
        (with-bindings {(ns-resolve parent-ns '*assembly*) assembly}
          (run)))))))
(use-fixtures :once parent-fixture)

(defn- v1867-alias-case-env
  ([] (v1867-alias-case-env nil nil))
  ([before-constructor!] (v1867-alias-case-env before-constructor! nil))
  ([before-constructor! document-state]
  ;; The original certified plan expressly permits this operation-ID alias.
  ;; Rebuild its current certificate and exact registered executor as in v1861.
  (let [d (data)
        e (environment d nil)
        base (original-issued-source d e nil)
        operation :request/claim
        construction (assoc-in (:construction base)
                               [:subject :interface :operation] operation)
        subject (:subject construction)
        transition (-> construction (dissoc :replacements :frame)
                       (assoc :assignments (:replacements construction)))
        proof {:transition transition
               :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
               :goal [:guard]}
        graph {:roots #{operation}
               :nodes [{:id operation :subject subject
                        :subject-handle (admission/admit-subject! subject)
                        :depends-on []}]}
        certificate (certificate/admit-certificate!
                     {:proof-kind :guarded-transition
                      :proof-source proof
                      :graph-handle (dependency/check-graph! graph)
                      :current-graph graph
                      :expected-open-premises
                      #{:gesso.assurance.proof.derivation/abstract-relation-match}
                      :expected-open-obligations
                      #{:production-transition-correspondence
                        :current-runtime-authorization
                        :atomic-transaction-enforcement
                        :checker-meta-soundness}})
        source (-> base
                   (assoc :construction construction)
                   (assoc-in [:certified-source :certificate] certificate)
                   (assoc-in [:certified-source :proof-source] proof)
                   (assoc-in [:certified-source :current-graph] graph))
        source (if before-constructor!
                 (update-in source
                            [:certified-source :resolved-configuration :construct!]
                            (fn [original]
                              (fn [ctx binding]
                                (before-constructor!)
                                (original ctx binding))))
                 source)
        source (if document-state
                 (assoc source :current-document! (fn [_ _] @document-state))
                 source)
        executor (plan/build-certified-plan-operation! source)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:binding (:binding d) :mode :submit})
                  :result->response! (fn [_ result] {:status 200 :body result})})
        contract {:application-assembly (var-get (ns-resolve parent-ns '*assembly*))
                  :semantic-route-handlers {operation handler}
                  :certified-operations {operation {:executor executor
                                                   :current-source source}}}
        catalog-source {:catalog (assoc-in (original-catalog)
                                           [:operations 0 :construction] construction)
                        :physical-field-map (physical-map)}
        ]
    {:data d :environment e :contract contract :catalog-source catalog-source})))

(deftest v1867-collected-original-executor-candidate-with-physical-read
  ;; The collector receives an operation+canonical invocation, NEVER a
  ;; caller-submitted observation. This fixture runs its real original executor
  ;; and pinned read and aborts before the canonical Gesso writer.
  (let [{:keys [data environment] :as env} (v1867-alias-case-env)
        operation :request/claim
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        initial ((:summary! observer))]
    (is (= :wrong-keys
           (reason #(application-observation/issue-current-issued-application-observer!
                     (:contract env)
                     {:catalog {:operations (get-in env [:catalog-source :catalog :operations])}
                      :physical-field-map {}})))
        "Route-only input must not substitute for the complete typed catalog")
    (is (= application-observation/report-type (:type initial)))
    (is (= application-observation/report-version (:version initial)))
    (is (= #{operation} (:certified-operation-ids initial)))
    (is (= #{operation} (set (keys (:per-operation-observations initial)))))
    (is (= :not-observed (get-in initial [:per-operation-observations operation])))
    (is (= #{} (:observed-operation-ids initial)))
    (is (= 0 (:observation-count initial)))
    (is (= {operation :not-attempted}
           (:per-operation-attempt-status initial)))
    (is (= #{} (:in-flight-observation-ids initial)
           (:failed-observation-ids initial)))
    (is (= :not-established (:physical-prestate-currentness initial)))
    (is (= :historical-individual-pinned-reads
           (:physical-observation-temporality initial)))
    (is (false? (:physical-state-rechecked-at-summary? initial)))
    (is (= :original-source-rechecked-at-summary
           (:source-stability initial)))
    (is (= {operation :not-eligible} (:per-operation-read-only-rechecks initial)))
    (is (= {operation :not-eligible}
           (:per-operation-historical-prestate-rechecks initial)))
    (is (= {operation :not-eligible}
           (:per-operation-reported-basis-relations initial)))
    (is (= false (:basis-observation-proves-no-aba? initial)))
    (is (= :original-observation-required-for-read-only-recheck
           (reason #((:read-only-recheck! observer) operation))))
    (is (false? (:all-certified-operations-individually-observed? initial)))
    (with-redefs [checked-assembly/check-declared-route-coverage!
                  (fn [& _] {:type checked-assembly/coverage-report-type})]
      (is (= :substituted-original-application-observation-checker
             (reason #((:summary! observer)))))
      (is (zero? @(:constructs environment))))
    (is (= :operation-not-originally-certified
           (reason #((:observe! observer) :request/cancel
                     {:biff.xtdb/node (db)}
                     {:binding (:binding data) :mode :submit}))))
    (is (zero? @(:constructs environment)))
    (seed! [:requests (:request data)])
    (let [result ((:observe! observer) operation
                  {:biff.xtdb/node (db)}
                  {:binding (:binding data) :mode :submit})
          selected (get-in result [:per-operation-observations operation])]
      (is (= #{operation} (:observed-operation-ids result)))
      (is (= 1 (:observation-count result)))
      (is (= {operation :qualified} (:per-operation-attempt-status result)))
      (is (= #{} (:in-flight-observation-ids result)
             (:failed-observation-ids result)))
      (is (= :individually-pinned-prestate-matched (:physical-before selected)))
      (is (= {operation :not-checked}
             (:per-operation-historical-prestate-rechecks result)))
      (is (= {operation :not-checked}
             (:per-operation-reported-basis-relations result)))
      (is (not-any? #(contains? result %)
                    [:private-original-before :historical-before :before
                     :original-context :original-invocation :target]))
      (is (= :original-and-independent-source-relative
             (:typed-transition selected)))
      (is (= (get-in result [:certified-operation-proof-origins operation])
             (:proof-origin selected)))
      (is (true? (:all-certified-operations-individually-observed? result)))
      (is (= (:original-open-proof-evidence initial)
             (:original-open-proof-evidence result)))
      (is (= :not-established (:joint-snapshot-basis result)
             (:physical-application-correspondence result)
             (:deployed-handler-adoption result)
             (:original-kernel-proof-discharge result)
             (:physical-refinement-proof-discharge result)))
      (is (= :none (:transaction-authority result)
             (:retry-authority result) (:authority result)))
      (is (= :not-issued (:certificate result)))
      (is (= :not-admitted (:admission result)))
      (is (not-any? #(contains? result %)
                    [:candidate :plan :binding :ctx :node :snapshot-token
                     :executor :observe-once! :submit! :current-document]))
      (is (= 1 @(:constructs environment)))
      (is (= (:request data) (read-request (:xt/id (:request data)))))
      (is (nil? (read-command
                 (resolution/record-id (:binding data)))))
      (is (= :issued-application-observation-already-attempted
             (reason #((:observe! observer) operation
                       {:biff.xtdb/node (db)}
                       {:binding (:binding data) :mode :submit}))))
      (is (= result ((:summary! observer)))))
    ;; New pinned follow-up reads must neither rerun the original command
    ;; constructor nor replace the historical checked observation.
    (let [follow ((:read-only-recheck! observer) operation)]
      (is (= :source-document-matched-at-read
             (get-in follow [:per-operation-read-only-rechecks operation])))
      (is (= :historical-prestate-matched-at-read
             (get-in follow [:per-operation-historical-prestate-rechecks
                             operation])))
      (is (= :same-reported-database-basis
             (get-in follow [:per-operation-reported-basis-relations
                             operation])))
      (is (= :reported-same-database-system-time-not-row-change-history
             (:basis-comparison-scope follow)))
      (is (= :original-checked-before-against-later-pinned-row
             (:historical-prestate-recheck-meaning follow)))
      (is (= 1 (:observation-count follow)))
      (is (= :not-established (:physical-prestate-currentness follow)))
      (is (= :later-source-accessor-against-new-pinned-read-not-historical-candidate
             (:read-only-recheck-meaning follow)))
      (is (= :none (:read-only-recheck-transaction-authority follow)))
      (is (= 1 @(:constructs environment)))
      (is (not-any? #(contains? follow %)
                    [:original-context :original-invocation :before :target
                     :private-original-before :historical-before
                     :node :snapshot-token :read-only-recheck! :executor]))
      (is (not (.contains (pr-str follow) ":private-original-before"))))
    ;; Replacing the read-only verifier must fail closed without modifying
    ;; the historical candidate or invoking an application constructor.
    (with-redefs [read-only-recheck/compare-original-acquired-prestate!
                  (fn [& _] {:comparison :source-document-matched-at-read})]
      (is (= :substituted-original-read-only-recheck-function
             (reason #((:read-only-recheck! observer) operation)))))
    (is (= :failed
           (get-in ((:summary! observer))
                   [:per-operation-read-only-rechecks operation])))
    (is (= :failed
           (get-in ((:summary! observer))
                   [:per-operation-historical-prestate-rechecks operation])))
    (is (= 1 @(:constructs environment)))
    (seed! [:requests (assoc (:request data) :owner "later-writer")])
    (let [changed ((:read-only-recheck! observer) operation)]
      (is (= :source-document-differed-at-read
             (get-in changed [:per-operation-read-only-rechecks operation])))
      (is (= :historical-prestate-differed-at-read
             (get-in changed [:per-operation-historical-prestate-rechecks
                              operation])))
      (is (= :later-reported-database-basis
             (get-in changed [:per-operation-reported-basis-relations
                              operation])))
      (is (= #{operation} (:observed-operation-ids changed)))
      (is (= :not-established (:physical-prestate-currentness changed)))
      (is (= 1 @(:constructs environment)))
      (is (= "later-writer" (:owner (read-request (:xt/id (:request data))))))
      (is (nil? (read-command (resolution/record-id (:binding data))))))
    (seed! [:requests (:request data)])
    (let [restored ((:read-only-recheck! observer) operation)]
      (is (= :source-document-matched-at-read
             (get-in restored [:per-operation-read-only-rechecks operation])))
      (is (= :historical-prestate-matched-at-read
             (get-in restored [:per-operation-historical-prestate-rechecks
                               operation])))
      (is (= :later-reported-database-basis
             (get-in restored [:per-operation-reported-basis-relations
                               operation]))
          "Restored equal data can coexist with an advanced database basis")
      (is (= :not-established (:physical-prestate-currentness restored)
             (:joint-snapshot-basis restored)))
      (is (= :none (:transaction-authority restored)
             (:retry-authority restored)))
      (is (= 1 (:observation-count restored)))
      (is (= :not-issued (:certificate restored))))
    (is (= 1 @(:constructs environment)))
    ;; Failed original attempts do not gain a fresh-read escape hatch.
    ;; Failed observation attempts are consumed and NEVER counted as evidence.
    (let [failed (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))]
      (is (= :invalid-original-executor-alias-invocation
             (reason #((:observe! failed) operation {:biff.xtdb/node (db)}
                       {:binding (:binding data) :mode :reconcile}))))
      (let [summary ((:summary! failed))]
        (is (= #{} (:observed-operation-ids summary)))
        (is (= #{operation} (:attempted-without-qualified-evidence summary)))
        (is (= {operation :failed} (:per-operation-attempt-status summary)))
        (is (= #{operation} (:failed-observation-ids summary)))
        (is (= #{} (:in-flight-observation-ids summary)))
        (is (= :not-observed
               (get-in summary [:per-operation-observations operation])))
        (is (= :original-observation-required-for-read-only-recheck
               (reason #((:read-only-recheck! failed) operation))))
        (is (= {operation :not-eligible}
               (:per-operation-historical-prestate-rechecks summary)))
        (is (= {operation :not-eligible}
               (:per-operation-reported-basis-relations summary)))
        (is (= :issued-application-observation-already-attempted
               (reason #((:observe! failed) operation {:biff.xtdb/node (db)}
                         {:binding (:binding data) :mode :submit}))))))))



(deftest v1871-historical-checked-prestate-versus-newly-acquired-physical-state
  ;; Same original issued executor. Its source accessor is a stable function
  ;; over mutable application state: later it can agree with a NEW physical
  ;; document that is NOT the originally captured prestate. An independent
  ;; historical witness must distinguish these two relations.
  (let [op :request/claim
        document-state (atom nil)
        env (v1867-alias-case-env nil document-state)
        actual (:data env)
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        observe! (:observe! observer)
        reread! (:read-only-recheck! observer)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding actual) :mode :submit}
        original (:request actual)
        revised (assoc original :owner "later-writer")
        source (get-in env [:contract :certified-operations op :current-source])
        executor (get-in env [:contract :certified-operations op :executor])]
    (is (= :invalid-private-checked-prestate-capture
           (reason #(issued/check-current-issued-executor-typed-command-alias!
                     (:contract env) (:catalog-source env) op ctx invocation
                     nil :no-external-read (fn [_] nil))))
        "Cannot capture an original witness outside pinned source-derived mode")
    (is (zero? @(:constructs (:environment env))))
    (reset! document-state original)
    (seed! [:requests original])
    (let [historical (observe! op ctx invocation)]
      (is (= #{op} (:observed-operation-ids historical)))
      (is (= {op :not-checked}
             (:per-operation-historical-prestate-rechecks historical)))
      (is (= 1 @(:constructs (:environment env))))
      (is (not-any? #(contains? historical %)
                    [:private-original-before :before :target :snapshot-token
                     :document-state :historical-prestate-witness])))
    (let [same (reread! op)]
      (is (= :source-document-matched-at-read
             (get-in same [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-matched-at-read
             (get-in same [:per-operation-historical-prestate-rechecks op])))
      (is (= :same-reported-database-basis
             (get-in same [:per-operation-reported-basis-relations op]))))
    (seed! [:requests revised])
    ;; Original source accessor still gives original document; both differ.
    (let [stale (reread! op)]
      (is (= :source-document-differed-at-read
             (get-in stale [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-differed-at-read
             (get-in stale [:per-operation-historical-prestate-rechecks op])))
      (is (= :later-reported-database-basis
             (get-in stale [:per-operation-reported-basis-relations op]))))
    ;; Change only mutable source DATA, not registered source/closure identity:
    ;; it now agrees with the newer physical row. The original still differs.
    (reset! document-state revised)
    (let [newly-agreeing (reread! op)]
      (is (= :source-document-matched-at-read
             (get-in newly-agreeing [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-differed-at-read
             (get-in newly-agreeing [:per-operation-historical-prestate-rechecks op])))
      (is (= #{op} (:observed-operation-ids newly-agreeing)))
      (is (= 1 (:observation-count newly-agreeing)))
      (is (= :not-established (:joint-snapshot-basis newly-agreeing)
             (:physical-prestate-currentness newly-agreeing))))
    ;; Accessor now differs, while the physical row has returned to the
    ;; original prestate: historical and current accessor comparisons diverge
    ;; in the opposite direction. ABA history is NOT excluded by equality.
    (seed! [:requests original])
    (let [returned (reread! op)]
      (is (= :source-document-differed-at-read
             (get-in returned [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-matched-at-read
             (get-in returned [:per-operation-historical-prestate-rechecks op])))
      (is (= :later-reported-database-basis
             (get-in returned [:per-operation-reported-basis-relations op]))))
    (reset! document-state original)
    (let [again (reread! op)]
      (is (= :source-document-matched-at-read
             (get-in again [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-matched-at-read
             (get-in again [:per-operation-historical-prestate-rechecks op])))
      (is (= :none (:transaction-authority again) (:retry-authority again)
             (:authority again)))
      (is (= :not-issued (:certificate again)))
      (is (= :later-reported-database-basis
             (get-in again [:per-operation-reported-basis-relations op])))
      (is (not (.contains (pr-str again) ":original-snapshot-point")))
      (is (not (.contains (pr-str again) ":snapshot-token")))
      (is (= 1 @(:constructs (:environment env))))
      (is (nil? (read-command
                 (resolution/record-id (:binding actual))))))
    ;; Calling the low-level read-only verifier with a FORGED witness cannot
    ;; make it compare a different original target or expose a privileged cmd.
    (is (= :invalid-private-original-prestate-witness
           (reason #(read-only-recheck/compare-original-acquired-prestate!
                     source executor op ctx invocation
                     {:operation-id op :target [:requests "other-row"]
                      :before original :source-identity {}}))))))

(deftest v1869-in-flight-and-failed-attempts-share-one-atomic-snapshot
  ;; Use the REAL registered executor, with a deliberately stalled original
  ;; application constructor. The permit is consumed before the constructor
  ;; callback starts, and summary must expose the in-flight operation.
  (let [entered (promise)
        release (promise)
        op :request/claim
        env (v1867-alias-case-env
             (fn []
               (deliver entered true)
               (when-not (= :released (deref release 20000 :timeout))
                 (throw (ex-info "Timed-out test constructor" {})))))
        d (:data env)
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        observe! (:observe! observer)
        summary! (:summary! observer)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding d) :mode :submit}]
    (seed! [:requests (:request d)])
    (let [result (future
                   (try {:value (observe! op ctx invocation)}
                        (catch Throwable failure {:failure failure})))]
      (try
        (is (= true (deref entered 20000 :timeout))
            "Original checked executor must enter the controlled constructor")
        (let [during (summary!)]
          (is (= {op :in-flight} (:per-operation-attempt-status during)))
          (is (= #{op} (:in-flight-observation-ids during)))
          (is (= #{op} (:attempted-without-qualified-evidence during)))
          (is (= #{} (:observed-operation-ids during)))
          (is (= 0 (:observation-count during)))
          (is (= :not-observed (get-in during [:per-operation-observations op])))
          (is (false? (:all-certified-operations-individually-observed? during)))
          (is (= :not-established (:physical-prestate-currentness during))))
        (is (= :issued-application-observation-already-attempted
               (reason #(observe! op ctx invocation)))
            "Concurrent duplicate must not start another constructor")
        (finally (deliver release :released)))
      (let [{:keys [value failure] :as outcome} (deref result 30000 :timeout)]
        (is (map? outcome))
        (is (nil? failure) (some-> failure .getMessage))
        (is (= {op :qualified} (:per-operation-attempt-status value)))
        (is (= #{} (:in-flight-observation-ids value)
               (:failed-observation-ids value)))
        (is (= :not-established (:physical-prestate-currentness value)))
        (is (= {op :not-checked} (:per-operation-read-only-rechecks value)))
        (is (= {op :not-checked}
               (:per-operation-historical-prestate-rechecks value)))
        (is (= {op :not-checked}
               (:per-operation-reported-basis-relations value)))
        (is (= value (summary!)))
        (is (= 1 @(:constructs (:environment env))))
        (is (nil? (read-command
                   (resolution/record-id (:binding d)))))))))

(defn- second-original-issued-alias [operation d]
  ;; An independently issued and admitted source, not a copied executor or
  ;; a caller-generated success report. Both operations use the same closed
  ;; Claim-shaped abstract state model but address distinct physical rows.
  (let [environment (environment d nil)
        base (original-issued-source d environment nil)
        construction (-> (:construction base)
                         (assoc-in [:subject :id] operation)
                         (assoc-in [:subject :interface :operation] operation))
        subject (:subject construction)
        transition (-> construction (dissoc :replacements :frame)
                       (assoc :assignments (:replacements construction)))
        proof {:transition transition
               :steps [{:id :step/guard :rule :guard :conclusion [:guard]}]
               :goal [:guard]}
        graph {:roots #{operation}
               :nodes [{:id operation :subject subject
                        :subject-handle (admission/admit-subject! subject)
                        :depends-on []}]}
        certificate (certificate/admit-certificate!
                     {:proof-kind :guarded-transition
                      :proof-source proof
                      :graph-handle (dependency/check-graph! graph)
                      :current-graph graph
                      :expected-open-premises
                      #{:gesso.assurance.proof.derivation/abstract-relation-match}
                      :expected-open-obligations
                      #{:production-transition-correspondence
                        :current-runtime-authorization
                        :atomic-transaction-enforcement
                        :checker-meta-soundness}})
        source (-> base
                   (assoc :construction construction)
                   (assoc-in [:certified-source :operation] operation)
                   (assoc-in [:certified-source :resolved-configuration :operation]
                             operation)
                   (assoc-in [:certified-source :certificate] certificate)
                   (assoc-in [:certified-source :proof-source] proof)
                   (assoc-in [:certified-source :current-graph] graph))
        executor (plan/build-certified-plan-operation! source)
        binding (assoc (:binding d) :operation operation)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_] {:binding binding :mode :submit})
                  :result->response! (fn [_ result] {:status 200 :body result})})]
    {:source source :executor executor :handler handler
     :data (assoc d :binding binding) :environment environment}))

(deftest v1868-two-independent-executor-observations-never-become-certification
  (let [primary :request/claim
        secondary :request/claim-secondary
        audit :request/audit
        primary-route :route/claim-primary
        secondary-route :route/claim-secondary
        audit-route :route/audit
        routes {primary-route {:operation primary :path "/request/claim-primary"}
                secondary-route {:operation secondary :path "/request/claim-secondary"}
                audit-route {:operation audit :path "/request/audit"}}
        operations (into {}
                         (for [op [primary secondary audit]]
                           [op (app-fixture/trusted-operation
                                op {:published-change-topics #{}})]))]
    (app-fixture/with-surfaced-application
      {:operations operations
       :route-capabilities (app-fixture/custom-route-capabilities routes)
       :rendered-surfaces {:proof [:div "safe"]}}
      (fn [{:keys [assembly report]}]
        (is (application/valid? report)
            (pr-str (:errors report)))
        (when assembly
          (let [first-source (v1867-alias-case-env)
                first-row (:data first-source)
                second-source (second-original-issued-alias secondary (data))
                second-row (:data second-source)
                contract {:application-assembly assembly
                          :semantic-route-handlers
                          {primary-route
                           (get-in first-source [:contract :semantic-route-handlers
                                                 primary])
                           secondary-route (:handler second-source)
                           audit-route (fn [_] {:status 202 :body "uncertified"})}
                          :certified-operations
                          {primary (get-in first-source
                                           [:contract :certified-operations primary])
                           secondary {:executor (:executor second-source)
                                      :current-source (:source second-source)}}}
                catalog-source
                {:catalog (-> (original-catalog)
                              (assoc :declared-operation-ids #{primary secondary}
                                     :operations
                                     [{:id primary
                                       :construction (get-in first-source
                                                             [:contract :certified-operations
                                                              primary :current-source :construction])}
                                      {:id secondary
                                       :construction (get-in second-source
                                                             [:source :construction])}]))
                 :physical-field-map (physical-map)}
                observer (application-observation/issue-current-issued-application-observer!
                          contract catalog-source)
                summary! (:summary! observer)
                observe! (:observe! observer)
                before (summary!)
                ctx {:biff.xtdb/node (db)}
                invoke-primary {:binding (:binding first-row) :mode :submit}
                invoke-secondary {:binding (:binding second-row) :mode :submit}]
            (is (not= (get-in first-row [:request :xt/id])
                      (get-in second-row [:request :xt/id])))
            (is (not= (:executor (get-in first-source
                                          [:contract :certified-operations primary]))
                      (:executor second-source)))
            (is (= #{primary secondary} (:certified-operation-ids before)))
            (is (= #{audit} (:uncertified-operation-ids before)))
            (let [history (:diagnostic-open-history-conditions before)]
              (is (= #{primary secondary}
                     (set (keys (:certified-operation-open-conditions history)))))
              (is (= #{audit} (:uncertified-operation-ids history)))
              (is (= #{audit-route} (:uncertified-route-ids history)))
              (is (= :none (:uncertified-history-proof-conditions history)))
              (is (every? #(= #{} (:discharged-conditions %))
                          (vals (:certified-operation-open-conditions history)))))
            (is (= #{audit-route} (:uncertified-route-ids before)))
            (is (= {:request/claim :not-observed
                    :request/claim-secondary :not-observed}
                   (:per-operation-observations before)))
            (is (= :operation-not-originally-certified
                   (reason #(observe! audit ctx invoke-primary))))
            (is (= {primary :not-attempted
                    secondary :not-attempted}
                   (:per-operation-attempt-status before)))
            (seed! [:requests (:request first-row)]
                   [:requests (:request second-row)])
            (let [first-result (observe! primary ctx invoke-primary)]
              (is (= #{primary} (:observed-operation-ids first-result)))
              (is (= {primary :qualified secondary :not-attempted}
                     (:per-operation-attempt-status first-result)))
              (is (= #{secondary}
                     (set (for [[operation status]
                                (:per-operation-observations first-result)
                                :when (= status :not-observed)]
                            operation))))
              (is (= :individually-pinned-prestate-matched
                     (get-in first-result
                             [:per-operation-observations primary :physical-before])))
              (is (false? (:all-certified-operations-individually-observed?
                           first-result)))
              (is (= :not-established (:joint-snapshot-basis first-result)))
              (is (= :issued-application-observation-already-attempted
                     (reason #(observe! primary ctx invoke-primary)))))
            ;; Between independent pinned observations, a new external XTDB
            ;; write invalidates the FIRST observation's current-row claim.
            ;; This intentionally rules out interpreting the final ledger as
            ;; one atomic shared snapshot, even though both entries succeed.
            (seed! [:requests (assoc (:request first-row) :owner "later-writer")])
            (is (= "later-writer"
                   (:owner (read-request (get-in first-row [:request :xt/id])))))
            (let [after (observe! secondary ctx invoke-secondary)]
              (is (= #{primary secondary} (:observed-operation-ids after)))
              (is (= 2 (:observation-count after)))
              (is (= {primary :qualified secondary :qualified}
                     (:per-operation-attempt-status after)))
              ;; The first row has CHANGED physically. The qualified entry
              ;; stays historical and cannot assert current-row freshness.
              (is (= :not-established (:physical-prestate-currentness after)))
              (is (= :historical-individual-pinned-reads
                     (:physical-observation-temporality after)))
              ;; Later independent reads of each ORIGINAL source do not
              ;; replay either executor and do not make their two snapshots
              ;; a shared or transaction-authoritative basis.
              (let [first-refresh ((:read-only-recheck! observer) primary)
                    second-refresh ((:read-only-recheck! observer) secondary)]
                (is (= :source-document-differed-at-read
                       (get-in first-refresh
                               [:per-operation-read-only-rechecks primary])))
                (is (= :historical-prestate-differed-at-read
                       (get-in first-refresh
                               [:per-operation-historical-prestate-rechecks
                                primary])))
                (is (= :later-reported-database-basis
                       (get-in first-refresh
                               [:per-operation-reported-basis-relations primary])))
                (is (= :source-document-matched-at-read
                       (get-in second-refresh
                               [:per-operation-read-only-rechecks secondary])))
                (is (= :historical-prestate-matched-at-read
                       (get-in second-refresh
                               [:per-operation-historical-prestate-rechecks
                                secondary])))
                (is (= :source-document-differed-at-read
                       (get-in second-refresh
                               [:per-operation-read-only-rechecks primary])))
                (is (= 2 (:observation-count second-refresh)))
                (is (= :not-established
                       (:joint-snapshot-basis second-refresh)
                       (:physical-prestate-currentness second-refresh))))
              (is (false? (:physical-state-rechecked-at-summary? after)))
              (is (= :original-source-rechecked-at-summary
                     (:source-stability after)))
              (is (true? (:all-certified-operations-individually-observed? after)))
              (is (= #{} (:attempted-without-qualified-evidence after)))
              (is (= :individually-pinned-prestate-matched
                     (get-in after
                             [:per-operation-observations secondary :physical-before])))
              (is (= (get-in before [:certified-operation-proof-origins primary])
                     (get-in after [:per-operation-observations primary :proof-origin])))
              (is (= (get-in before [:certified-operation-proof-origins secondary])
                     (get-in after [:per-operation-observations secondary :proof-origin])))
              (is (= (:original-open-proof-evidence before)
                     (:original-open-proof-evidence after)))
              (is (= (:diagnostic-open-history-conditions before)
                     (:diagnostic-open-history-conditions after)))
              (is (seq (get-in after [:original-open-proof-evidence :open-obligations])))
              (is (= #{audit} (:uncertified-operation-ids after)))
              (is (= #{audit-route} (:uncertified-route-ids after)))
              (is (not (contains? (:per-operation-observations after) audit)))
              (is (= :not-established
                     (:joint-snapshot-basis after)
                     (:physical-application-correspondence after)
                     (:deployed-handler-adoption after)
                     (:original-kernel-proof-discharge after)
                     (:physical-refinement-proof-discharge after)))
              (is (= :not-issued (:certificate after)))
              (is (= :not-admitted (:admission after)))
              (is (= :none (:authority after)
                     (:retry-authority after) (:transaction-authority after)))
              (is (= 1 @(:constructs (:environment first-source))))
              (is (= 1 @(:constructs (:environment second-source))))
              (is (= "later-writer"
                     (:owner (read-request (get-in first-row [:request :xt/id])))))
              (is (= (:request second-row)
                     (read-request (get-in second-row [:request :xt/id]))))
              (is (nil? (read-command
                         (resolution/record-id (:binding first-row)))))
              (is (nil? (read-command
                         (resolution/record-id (:binding second-row)))))
              (is (= :issued-application-observation-already-attempted
                     (reason #(observe! secondary ctx invoke-secondary))))
              (is (= (dissoc after :per-operation-read-only-rechecks
                                  :per-operation-historical-prestate-rechecks
                                  :per-operation-reported-basis-relations
                                  :per-operation-selected-row-history-boundaries
                                  :per-operation-selected-row-current-visibility
                                  :per-operation-visible-history-inventory)
                     (dissoc (summary!) :per-operation-read-only-rechecks
                                        :per-operation-historical-prestate-rechecks
                                        :per-operation-reported-basis-relations
                                        :per-operation-selected-row-history-boundaries
                                        :per-operation-selected-row-current-visibility
                                        :per-operation-visible-history-inventory)))
              (is (= {primary :visible-selected-row-system-boundary-observed
                      secondary :same-reported-basis-no-interval}
                     (:per-operation-selected-row-history-boundaries (summary!))))
              (is (= {primary :source-document-differed-at-read
                      secondary :source-document-matched-at-read}
                     (:per-operation-read-only-rechecks (summary!))))
              (is (= {primary :historical-prestate-differed-at-read
                      secondary :historical-prestate-matched-at-read}
                     (:per-operation-historical-prestate-rechecks (summary!)))))))))))

(deftest v1873-selected-row-system-boundaries-vs-unrelated-basis-advance
  ;; Same complete original certified plan and real XTDB fixture. The database
  ;; basis can advance without ANY selected-row system-time boundary, whereas
  ;; a change-and-restore leaves observable selected-row version boundaries.
  (let [op :request/claim
        {:keys [data environment] :as env} (v1867-alias-case-env)
        obs (application-observation/issue-current-issued-application-observer!
             (:contract env) (:catalog-source env))
        observe! (:observe! obs)
        reread! (:read-only-recheck! obs)
        original (:request data)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}
        status #(get-in % [:per-operation-selected-row-history-boundaries op])]
    (is (= {op :not-eligible}
           (:per-operation-selected-row-history-boundaries ((:summary! obs)))))
    (seed! [:requests original])
    (let [initial (observe! op ctx invocation)]
      (is (= #{op} (:observed-operation-ids initial)))
      (is (= :not-checked (status initial)))
      (is (= :not-established (:selected-row-history-completeness initial)))
      (is (false? (:selected-row-history-proves-no-aba? initial))))
    (let [same (reread! op)]
      (is (= :same-reported-basis-no-interval (status same)))
      (is (= :same-reported-database-basis
             (get-in same [:per-operation-reported-basis-relations op]))))
    (seed! [:requests {:xt/id (str "unrelated-" (random-uuid))
                       :owner "other-unrelated-request"}])
    (let [unrelated (reread! op)]
      (is (= :later-reported-database-basis
             (get-in unrelated [:per-operation-reported-basis-relations op])))
      (is (= :no-visible-selected-row-system-boundary-returned (status unrelated)))
      (is (= :historical-prestate-matched-at-read
             (get-in unrelated [:per-operation-historical-prestate-rechecks op])))
      (is (= :source-document-matched-at-read
             (get-in unrelated [:per-operation-read-only-rechecks op])))
      (is (= :not-established (:selected-row-history-completeness unrelated)))
      (is (false? (:selected-row-history-proves-no-aba? unrelated)))
      (is (= :visible-pinned-system-time-boundary-presence-only
             (:selected-row-history-meaning unrelated))))
    (seed! [:requests (assoc original :owner "some-other-owner")])
    (let [changed (reread! op)]
      (is (= :visible-selected-row-system-boundary-observed (status changed)))
      (is (= :historical-prestate-differed-at-read
             (get-in changed [:per-operation-historical-prestate-rechecks op]))))
    (seed! [:requests original])
    (let [restored (reread! op)]
      (is (= :visible-selected-row-system-boundary-observed (status restored)))
      (is (= :historical-prestate-matched-at-read
             (get-in restored [:per-operation-historical-prestate-rechecks op])))
      (is (= :source-document-matched-at-read
             (get-in restored [:per-operation-read-only-rechecks op])))
      (is (= :later-reported-database-basis
             (get-in restored [:per-operation-reported-basis-relations op])))
      (is (= :not-established (:selected-row-history-completeness restored)
             (:physical-application-correspondence restored)
             (:physical-prestate-currentness restored)
             (:joint-snapshot-basis restored)))
      (is (false? (:selected-row-history-proves-no-aba? restored)))
      (is (= :none (:transaction-authority restored)
             (:retry-authority restored) (:authority restored)))
      (is (= :not-issued (:certificate restored)))
      (is (not-any? #(contains? restored %)
                    [:snapshot-token :original-snapshot-point :xt/system-from
                     :xt/system-to :before :target :node :executor
                     :original-context :original-invocation
                     :private-original-before]))
      (is (not (.contains (pr-str restored) ":xt/system-from")))
      (is (= 1 @(:constructs environment)))
      (is (nil? (read-command (resolution/record-id (:binding data))))))
    ;; Rebinding the original query helper is a fail-closed diagnostic error,
    ;; not new evidence and never permission to retry the issued command.
    (with-redefs [row-history/observe-pinned-row-system-boundaries!
                  (fn [& _] :visible-selected-row-system-boundary-observed)]
      (is (= :substituted-original-read-only-recheck-function
             (reason #(reread! op)))))
    (let [failed ((:summary! obs))]
      (is (= :failed (status failed)))
      (is (= #{op} (:observed-operation-ids failed)))
      (is (= 1 (:observation-count failed)))
      (is (= 1 @(:constructs environment))))))


(deftest v1874-deletion-temporal-correction-and-erased-history-are-distinct
  ;; Real XTDB: DELETE leaves system-time closing boundaries; a retroactive
  ;; valid-time correction is a new system-time version but can leave the
  ;; current-valid document unchanged; ERASE removes visible row history.
  ;; None of these source-relative observations is an ERASE attestation.
  (let [op :request/claim
        {:keys [data environment] :as env} (v1867-alias-case-env)
        obs (application-observation/issue-current-issued-application-observer!
             (:contract env) (:catalog-source env))
        observe! (:observe! obs)
        reread! (:read-only-recheck! obs)
        request (:request data)
        id (:xt/id request)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}
        boundary #(get-in % [:per-operation-selected-row-history-boundaries op])
        visibility #(get-in % [:per-operation-selected-row-current-visibility op])]
    (is (= {op :not-eligible}
           (:per-operation-selected-row-current-visibility ((:summary! obs)))))
    (seed! [:requests request])
    (is (= #{op} (:observed-operation-ids (observe! op ctx invocation))))
    (is (= :not-checked (visibility ((:summary! obs)))))
    (is (= :one-current-valid-row (visibility (reread! op))))
    (xt/execute-tx (db) [[:delete-docs :requests id]])
    (let [deleted (reread! op)]
      (is (= :no-current-valid-row (visibility deleted)))
      (is (= :visible-selected-row-system-boundary-observed (boundary deleted))
          "DELETE closes a visible old row's system/valid-time interval")
      (is (= :historical-prestate-differed-at-read
             (get-in deleted [:per-operation-historical-prestate-rechecks op])))
      (is (= :source-document-differed-at-read
             (get-in deleted [:per-operation-read-only-rechecks op])))
      (is (false? (:selected-row-history-absence-means-unchanged? deleted))))
    (seed! [:requests request])
    (let [restored (reread! op)]
      (is (= :one-current-valid-row (visibility restored)))
      (is (= :historical-prestate-matched-at-read
             (get-in restored [:per-operation-historical-prestate-rechecks op])))
      (is (= :visible-selected-row-system-boundary-observed (boundary restored))))
    (xt/execute-tx (db)
                   [[:put-docs {:into :requests
                                :valid-from (java.time.Instant/parse "2020-01-01T00:00:00Z")
                                :valid-to (java.time.Instant/parse "2021-01-01T00:00:00Z")}
                     (assoc request :owner "corrected-previous-valid-time")]])
    (let [corrected (reread! op)]
      (is (= :one-current-valid-row (visibility corrected)))
      (is (= :visible-selected-row-system-boundary-observed (boundary corrected))
          "Past valid-time correction writes a new selected-row system version")
      (is (= :historical-prestate-matched-at-read
             (get-in corrected [:per-operation-historical-prestate-rechecks op])))
      (is (= :source-document-matched-at-read
             (get-in corrected [:per-operation-read-only-rechecks op]))))
    ;; ERASE intentionally destroys every system/valid-time version. The
    ;; diagnostic cannot tell this apart from other history unavailability.
    (xt/execute-tx (db) [[:erase-docs :requests id]])
    (let [erased (reread! op)]
      (is (= :no-current-valid-row (visibility erased)))
      (is (= :no-visible-selected-row-system-history-returned (boundary erased)))
      (is (= :later-reported-database-basis
             (get-in erased [:per-operation-reported-basis-relations op])))
      (is (= :not-established (:selected-row-history-completeness erased)
             (:physical-prestate-currentness erased)
             (:joint-snapshot-basis erased)))
      (is (= false (:selected-row-history-proves-no-aba? erased)
             (:selected-row-history-absence-means-unchanged? erased)))
      (is (= :not-issued (:certificate erased)))
      (is (= :none (:transaction-authority erased)
             (:retry-authority erased) (:authority erased)))
      (is (= #{op} (:observed-operation-ids erased)))
      (is (= 1 (:observation-count erased)))
      (is (= 1 @(:constructs environment)))
      (is (nil? (read-command (resolution/record-id (:binding data)))))
      (is (not-any? #(contains? erased %)
                    [:snapshot-token :original-snapshot-point :xt/system-from
                     :xt/system-to :before :target :node :executor
                     :original-context :original-invocation
                     :private-original-before])))))


(deftest v1875-visible-pinned-history-inventory-saturation-is-not-history-proof
  ;; The original checked plan's row can have many valid-time slices, even
  ;; within ONE system-time transaction. Count of visible records is not the
  ;; count of writes. A non-saturated query also cannot exclude ERASE.
  (let [op :request/claim
        {:keys [data environment] :as env} (v1867-alias-case-env)
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        observe! (:observe! observer)
        reread! (:read-only-recheck! observer)
        request (:request data)
        id (:xt/id request)
        ctx {:biff.xtdb/node (db)}
        invocation {:binding (:binding data) :mode :submit}
        inventory #(get-in % [:per-operation-visible-history-inventory op])]
    (is (= {op :not-eligible}
           (:per-operation-visible-history-inventory ((:summary! observer)))))
    (seed! [:requests request])
    (is (= #{op} (:observed-operation-ids (observe! op ctx invocation))))
    (is (= :not-checked (inventory ((:summary! observer)))))
    (let [single (reread! op)]
      (is (= :visible-pinned-history-inventory-below-cap (inventory single)))
      (is (= :same-reported-basis-no-interval
             (get-in single [:per-operation-selected-row-history-boundaries op])))
      (is (= :bounded-33-record-pinned-query
             (:visible-history-inventory-scope single)))
      (is (false? (:visible-history-inventory-proves-complete-history? single))))
    ;; 38 distinct PAST valid-time slices in one transaction force 33 visible
    ;; records in the capped snapshot inventory while preserving the NOW row.
    (xt/execute-tx
     (db)
     (mapv (fn [y]
             [:put-docs {:into :requests
                         :valid-from (java.time.Instant/parse
                                      (str y "-01-01T00:00:00Z"))
                         :valid-to (java.time.Instant/parse
                                    (str (inc y) "-01-01T00:00:00Z"))}
              (assoc request :owner (str "historical-" y))])
           (range 1960 1998)))
    (let [saturated (reread! op)]
      (is (= :visible-pinned-history-inventory-cap-reached
             (inventory saturated)))
      (is (= :one-current-valid-row
             (get-in saturated [:per-operation-selected-row-current-visibility op])))
      (is (= :source-document-matched-at-read
             (get-in saturated [:per-operation-read-only-rechecks op])))
      (is (= :historical-prestate-matched-at-read
             (get-in saturated [:per-operation-historical-prestate-rechecks op])))
      (is (= :visible-selected-row-system-boundary-observed
             (get-in saturated [:per-operation-selected-row-history-boundaries op])))
      (is (false? (:visible-history-inventory-proves-complete-history? saturated)))
      (is (= :not-established (:selected-row-history-completeness saturated)
             (:physical-application-correspondence saturated))))
    (xt/execute-tx (db) [[:erase-docs :requests id]])
    (let [erased (reread! op)]
      (is (= :no-visible-pinned-history-records (inventory erased)))
      (is (= :no-visible-selected-row-system-history-returned
             (get-in erased [:per-operation-selected-row-history-boundaries op])))
      (is (= :no-current-valid-row
             (get-in erased [:per-operation-selected-row-current-visibility op])))
      (is (= :not-established (:selected-row-history-completeness erased)
             (:joint-snapshot-basis erased)))
      (is (false? (:visible-history-inventory-proves-complete-history? erased)))
      (is (= :none (:transaction-authority erased) (:retry-authority erased)
             (:authority erased)))
      (is (= :not-issued (:certificate erased)))
      (is (= #{op} (:observed-operation-ids erased)))
      (is (= 1 (:observation-count erased)))
      (is (= 1 @(:constructs environment)))
      (is (nil? (read-command (resolution/record-id (:binding data)))))
      (is (not-any? #(contains? erased %)
                    [:snapshot-token :xt/system-from :xt/valid-from :xt/system-to
                     :original-context :private-original-before :target :before])))))

(deftest v1876-history-proof-requirements-remain-open-after-pinned-evidence
  ;; These conditions are generated from the genuinely checked original
  ;; source, but are NEW diagnostic requirements, NOT original kernel proofs.
  ;; Even a below-cap visible history, and especially a later ERASE, cannot
  ;; discharge retention or erasure-exclusion by inspecting row records.
  (let [op :request/claim
        {:keys [data environment] :as env} (v1867-alias-case-env)
        observer (application-observation/issue-current-issued-application-observer!
                  (:contract env) (:catalog-source env))
        summarize (:summary! observer)
        original-summary (summarize)
        ledger (:diagnostic-open-history-conditions original-summary)
        open history-obligations/required-conditions
        origin (get-in original-summary [:certified-operation-proof-origins op])
        entry (get-in ledger [:certified-operation-open-conditions op])
        invocation {:binding (:binding data) :mode :submit}
        ctx {:biff.xtdb/node (db)}]
    (is (= history-obligations/report-type (:type ledger)))
    (is (= history-obligations/report-version (:version ledger)))
    (is (= :new-diagnostic-conditions-not-original-kernel-obligations
           (:provenance ledger)))
    (is (= #{op} (set (keys (:certified-operation-open-conditions ledger)))))
    (is (= origin (:original-certified-proof-origin entry)))
    (is (= open (:required-conditions ledger) (:required-conditions entry)))
    (is (= 4 (count open)))
    (is (= #{} (:discharged-conditions ledger) (:discharged-conditions entry)))
    (is (= :open-unproven (:disposition entry)))
    (is (= :not-established (:history-retention ledger)
           (:erasure-exclusion ledger) (:full-interval-coverage ledger)
           (:snapshot-source-authenticity ledger)
           (:historical-continuity ledger)
           (:original-kernel-proof-discharge ledger)))
    (is (= :not-issued (:certificate ledger)))
    (is (= :none (:authority ledger) (:uncertified-history-proof-conditions ledger)))
    (is (= #{} (:uncertified-operation-ids ledger)
           (:uncertified-route-ids ledger)))
    (is (= :incomplete-source-bound-history-obligation-scope
           (reason #(history-obligations/derive-open-history-conditions!
                     {:certified-operation-ids #{op}
                      :certified-operation-proof-origins {op (dissoc origin :route-ids)}
                      :certified-route-ids #{op}
                      :uncertified-operation-ids #{}
                      :uncertified-route-ids #{}}))))
    (is (= :incomplete-source-bound-history-obligation-scope
           (reason #(history-obligations/derive-open-history-conditions!
                     {:certified-operation-ids #{op}
                      :certified-operation-proof-origins {op origin}
                      :certified-route-ids (:route-ids origin)
                      :uncertified-operation-ids #{op}
                      :uncertified-route-ids #{}}))))
    (seed! [:requests (:request data)])
    (let [first-observation ((:observe! observer) op ctx invocation)
          same ((:read-only-recheck! observer) op)]
      (is (= ledger (:diagnostic-open-history-conditions first-observation)
             (:diagnostic-open-history-conditions same)))
      (is (= :visible-pinned-history-inventory-below-cap
             (get-in same [:per-operation-visible-history-inventory op])))
      (is (= :not-established
             (get-in same [:diagnostic-open-history-conditions
                           :history-retention])))
      (is (= (:original-open-proof-evidence original-summary)
             (:original-open-proof-evidence same))))
    (xt/execute-tx (db) [[:erase-docs :requests
                           (:xt/id (:request data))]])
    (let [after-erasure ((:read-only-recheck! observer) op)]
      (is (= :no-visible-pinned-history-records
             (get-in after-erasure [:per-operation-visible-history-inventory op])))
      (is (= ledger (:diagnostic-open-history-conditions after-erasure)))
      (is (= #{} (get-in after-erasure
                         [:diagnostic-open-history-conditions
                          :certified-operation-open-conditions op
                          :discharged-conditions])))
      (is (= :not-established
             (get-in after-erasure
                     [:diagnostic-open-history-conditions :historical-continuity])))
      (is (= 1 @(:constructs environment)))
      (is (nil? (read-command (resolution/record-id (:binding data))))))))

(deftest v1872-basis-relation-classification-is-not-an-aba-proof
  (let [t0 (java.time.Instant/parse "2026-01-01T00:00:00Z")
        t1 (java.time.Instant/parse "2026-01-01T00:00:01Z")
        p0 {:database "xtdb" :system-time t0}
        p1 {:database "xtdb" :system-time t1}
        other {:database "different" :system-time t1}]
    (is (= p0 (snapshot/comparable-point {"xtdb" [t0]})))
    (is (= p1 (snapshot/comparable-point {"xtdb" [t1]})))
    (is (nil? (snapshot/comparable-point {"xtdb" [nil]})))
    (is (nil? (snapshot/comparable-point {"xtdb" [t0 t1]})))
    (is (nil? (snapshot/comparable-point {"xtdb" [t0]
                                          "other" [t1]})))
    (is (= :not-comparable (snapshot/reported-basis-relation nil p1)))
    (is (= :not-comparable (snapshot/reported-basis-relation p0 nil)))
    (is (= :different-reported-databases
           (snapshot/reported-basis-relation p0 other)))
    (is (= :same-reported-database-basis
           (snapshot/reported-basis-relation p0 p0)))
    (is (= :later-reported-database-basis
           (snapshot/reported-basis-relation p0 p1)))
    (is (= :earlier-reported-database-basis
           (snapshot/reported-basis-relation p1 p0)))
    (is (false? (snapshot/valid-private-point?
                 {:database "xtdb" :system-time "not-a-time"})))))

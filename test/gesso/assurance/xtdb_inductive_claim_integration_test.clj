(ns gesso.assurance.xtdb-inductive-claim-integration-test
  "v1304: Gesso-local, embedded-XTDB Claim pilot from conditional induction
   through an actually INVOKED induction-bound executor, a durable command,
   pinned physical observation and source-bound diagnostic correspondence.

   We use the real v1252 guarded Claim construction, v1290 issued executor,
   v1253 issued Ring handler, and v1294 assembled application. Unlike v1302
   smoke fixtures, successful submissions pass THROUGH the issued executor,
   gesso.model.tx/transact!, Gesso Live and embedded XTDB. The trusted, optional
   transaction observer captures the actual prepared incorporation and commit
   receipt; it is not a cryptographically authenticated provenance channel.

   The intentionally corrupted physical submission is injected ONLY in its
   negative test at the final tx boundary. Its durable command record must not
   make a missing domain write look conforming. The demonstrated invariant is
   the restricted status-domain property from the existing proof fixture;
   this is NOT a proof of uniqueness, authorization, concurrency safety,
   production writer coverage or universal XTDB refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.observed-correspondence :as joined]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.certified-contract :as certified]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.certified-plan-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction-plan]
            [gesso.model.inductive-plan-test]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:private claim-operation :example/claim)
(def ^:dynamic *pilot* nil)

(defn- fixture-value [test-ns symbol]
  (or (some-> (ns-resolve test-ns symbol) deref)
      (throw (ex-info "Missing original Gesso proving fixture"
                      {:namespace test-ns :symbol symbol}))))

(defn- source-package [node]
  (let [environment ((fixture-value 'gesso.model.certified-plan-test
                                    'environment))
        source (assoc-in (:source environment)
                         [:certified-source :resolved-configuration :read-record!]
                         (fn [_ record-id]
                           (first (xt/q node
                                        ["SELECT * FROM gesso_commands WHERE _id = ?"
                                         record-id]))))
        catalog ((fixture-value 'gesso.model.inductive-plan-test 'catalog-for)
                 (:construction source))
        proof ((fixture-value 'gesso.model.inductive-plan-test 'proof-package)
               catalog)
        induction-source {:inductive-source proof
                          :certified-plan-source source}
        executor (induction-plan/build-inductive-plan-operation!
                  induction-source)
        handler (certified/issue-certified-handler!
                 {:executor executor :current-source source
                  :request->input! (fn [_]
                                     {:mode :reconcile :binding {}})
                  :result->response! (fn [_ result]
                                       {:status 200 :body result})})]
    {:environment environment :plan-source source
     :induction-source induction-source :executor executor
     :handler handler :proof proof}))

(defn- source-for-application [assembly package]
  (let [{:keys [proof plan-source induction-source executor handler]} package]
    {:authoritative-coverage-source
     {:inductive-contract-source
      {:inductive-source proof :application-assembly assembly
       :semantic-route-handlers {claim-operation handler}
       :certified-operations
       {claim-operation {:executor executor :current-source plan-source}}}
      :enumerate-entry-points!
      (fn [] {claim-operation {:kind :semantic-route :handler handler}})}
     :inductive-plans
     {claim-operation {:executor executor :current-source induction-source}}}))

(defn- command-binding []
  {:command-id (identity/command-id (str "v1304-" (random-uuid)))
   :principal (identity/principal :helper/alice)
   :operation claim-operation :arguments {}})

(defn- receipt-key [receipt]
  (cond
    (instance? TransactionKey receipt)
    {:tx-id (.getTxId ^TransactionKey receipt)
     :system-time (.getSystemTime ^TransactionKey receipt)}
    (and (map? receipt) (= #{:tx-id :system-time} (set (keys receipt))))
    (select-keys receipt [:tx-id :system-time])
    :else (throw (ex-info "No definitive XTDB transaction receipt"
                          {:receipt-type (type receipt)}))))

(defn- physical-row [node table id]
  (first (xt/q node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- run-claim! [node package {:keys [record-only?]}]
  (let [{:keys [executor plan-source environment]} package
        before ((:current-document! plan-source) {} {})
        target ((:target! plan-source) {} {})
        inputs ((:transition-inputs! plan-source) {} {})
        binding (command-binding)
        evidence (atom {:events []})
        events! (fn [event]
                  (swap! evidence update :events conj event)
                  (case (:phase event)
                    :prepared
                    (let [incorporation {:plan (:plan event)
                                         :record (:record event)
                                         :scope resolution/assurance-scope}
                          cmd-target [:gesso_commands
                                      (:xt/id (:record event))]
                          capture (observation/begin-observation!
                                   {:node node :incorporation incorporation
                                    :targets [target cmd-target]})]
                      (swap! evidence assoc :capture capture
                             :incorporation incorporation))
                    :physical-outcome
                    (swap! evidence assoc :physical-outcome event)
                    nil))
        original-tx tx/transact!
        invoke (fn []
                 (executor {:biff.xtdb/node node
                            resolved/observation-context-key events!}
                           {:binding binding :mode :submit}))]
    ;; A fresh actual database prestate; the v1252 trusted acquisition callback
    ;; deliberately returns its fixed matching copy. Its correctness remains
    ;; a premise, not something induction verifies.
    (xt/execute-tx node [[:put-docs :requests before]])
    (let [result (if record-only?
                   (with-redefs [tx/transact!
                                 (fn [ctx plan]
                                   ;; Deliberately corrupt the actual physical
                                   ;; payload AFTER v1251 plan admission. The
                                   ;; prepared observation still says both
                                   ;; domain + command were planned.
                                   (original-tx
                                    ctx (update plan :commands
                                                (fn [commands]
                                                  (filterv #(= :create
                                                               (:model/operation %))
                                                           commands)))))]
                     (invoke))
                   (invoke))
          {:keys [events capture physical-outcome incorporation]} @evidence
          receipt (:physical/receipt physical-outcome)
          witness (when (and capture (= :committed (:physical/status physical-outcome)))
                    (observation/complete-observation!
                     capture {:tx-key (receipt-key receipt)}))]
      {:before before :result result :binding binding :target target
       :inputs inputs :witness witness :incorporation incorporation
       :events events :physical-outcome physical-outcome
       :observed-request (physical-row node :requests (second target))
       :observed-record (physical-row node :gesso_commands
                                      (resolution/record-id binding))})))

(defn- transition-source [package result]
  {:observation (:witness result)
   :construction (get-in package [:plan-source :construction])
   :field-map (get-in package [:plan-source :field-map])
   :inputs (:inputs result)
   :target (:target result)
   :command-operation :claim})

(defn- checked-joined [result]
  (joined/check-observed-correspondence!
   {:application-source (:application-source *pilot*)
    :transition-source (transition-source (:package *pilot*) result)}))

(defn- exception-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(use-fixtures
  :once
  (fn [run]
    (with-open [node (xtn/start-node {})]
      (let [package (source-package node)]
        (fixture/with-closed-application
          {:operations {claim-operation
                        (fixture/trusted-operation
                         claim-operation {:published-change-topics #{}})}}
          (fn [{:keys [assembly]}]
            (let [application-source (source-for-application assembly package)
                  conforming (run-claim! node package {})
                  divergent (run-claim! node package {:record-only? true})]
              (binding [*pilot* {:node node :package package
                                 :application-source application-source
                                 :conforming conforming :divergent divergent}]
                (run)))))))))

(defn- good [] (:conforming *pilot*))
(defn- wrong [] (:divergent *pilot*))

(deftest genuine-issued-executor-submits-a-real-claim
  (let [{:keys [result events physical-outcome binding observed-request
                observed-record before]} (good)]
    (is (= :incorporated (:status result)))
    (is (= :committed (get-in result [:transaction :commit/status])))
    (is (= [:prepared :physical-outcome] (mapv :phase events)))
    (is (= :committed (:physical/status physical-outcome)))
    (is (= :gesso-model-transaction-ack (:physical/source physical-outcome)))
    (is (= :claimed (:request/status observed-request)))
    (is (= (inc (:request/version before)) (:request/version observed-request)))
    (is (= (:request/score before) (:request/score observed-request)))
    (is (= :incorporated (:gesso.command/status observed-record)))
    (is (= binding (:gesso.command/binding observed-record)))))

(deftest same-issued-executor-has-source-bound-abstract-proof
  (let [{:keys [executor induction-source]} (:package *pilot*)
        r (induction-plan/require-current-inductive-plan! executor induction-source)]
    (is (= :conditionally-checked-in-declared-abstract-machine
           (:inductive-invariant r)))
    (is (= #{claim-operation} (:catalog-operation-ids r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:physical-correspondence r)))))

(deftest genuine-executor-prepared-event-agrees-with-persisted-observation
  (let [{:keys [witness incorporation events binding]} (good)]
    (is (= (:plan incorporation) (:declared-plan witness)))
    (is (= (:record incorporation) (:persisted-command-record witness)))
    (is (= binding (:command-binding witness)))
    (is (= :committed (:physical-status witness)))
    (is (nat-int? (:physical-tx-id witness)))
    (is (= :caller-linked-not-independently-authenticated
           (:witness-provenance witness)))
    (is (= :none (:authority witness)))
    (is (= (:plan (first events)) (:declared-plan witness)))))

(deftest actual-claim-corresponds-to-declared-abstract-step
  (let [r (checked-joined (good))]
    (is (true? (:diagnostic-conforms? r)) (pr-str (:observed-checks r)))
    (is (= :corresponds-to-declared-step (:diagnostic-correspondence r)))
    (is (= :exact-source-and-issued-executor-rechecked
           (:plan-correspondence r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:universal-xtdb-refinement r)))))

(deftest committed-command-without-claim-update-fails-correspondence
  (let [{:keys [result observed-request observed-record before
                physical-outcome]} (wrong)
        r (checked-joined (wrong))]
    (is (= :incorporated (:status result))
        "The deliberately corrupted committed command can fool the outcome")
    (is (= :committed (:physical/status physical-outcome)))
    (is (= before observed-request)
        "The physical request was NOT claimed")
    (is (= :incorporated (:gesso.command/status observed-record)))
    (is (false? (:diagnostic-conforms? r)))
    (is (= :diverges-from-declared-step (:diagnostic-correspondence r)))
    (is (false? (get-in r [:observed-checks :abstract-assignments])))
    (is (false? (get-in r [:observed-checks :model-poststate])))
    (is (= :none (:authority r)))))

(deftest a-false-physical-poststate-in-copyable-witness-stays-diagnostic
  (let [{:keys [target] :as sample} (good)
        changed (assoc-in sample [:witness :observed-after target
                                  :request/status] :open)
        r (checked-joined changed)]
    (is (false? (:diagnostic-conforms? r)))
    (is (= :not-established (:authenticated-physical-correspondence r)))
    (is (= :none (:transaction-authority r)))))

(deftest a-plausible-fabricated-witness-is-never-authenticated
  (let [sample (good)
        forged (assoc-in sample [:witness :physical-tx-id] 9000000)
        r (checked-joined forged)]
    (is (true? (:diagnostic-conforms? r)))
    (is (= :caller-supplied-unauthenticated
           (:physical-observation-provenance r)))
    (is (= :not-established (:authenticated-physical-correspondence r)))
    (is (= :none (:authority r)))))

(deftest old-abstraction-cannot-certify-new-physical-claim-semantics
  (let [sample (good)
        changed (assoc (transition-source (:package *pilot*) sample)
                       :construction (assoc-in
                                      (get-in *pilot* [:package :plan-source
                                                       :construction])
                                      [:guard :expression] [:literal true]))
        error (exception-data
               #(joined/check-observed-correspondence!
                 {:application-source (:application-source *pilot*)
                  :transition-source changed}))]
    (is (= :observation-not-bound-to-issued-plan (:reason error)))
    (is (= :gesso.assurance.observed-correspondence/invalid-observed-correspondence
           (:error/type error)))))

(deftest original-physical-and-mathematical-obligations-remain-open
  (let [r (checked-joined (good))]
    (is (= :all-original-obligations-remain-open
           (:original-obligation-status r)))
    (doseq [obligation
            [:physical-observation-origin-is-unauthenticated
             :single-observation-does-not-establish-universal-refinement
             :all-physical-writers-are-not-established
             :independent-induction-checker-soundness]]
      (is (contains? (:open-obligations r) obligation)))
    (doseq [k [:physical-write-path-completeness
               :authenticated-physical-correspondence
               :application-invariant-physical-preservation
               :deployed-router-adoption
               :production-certification]]
      (is (= :not-established (get r k))))))

(deftest actual-durable-command-replay-does-not-resubmit
  (let [{:keys [executor]} (:package *pilot*)
        binding (:binding (good))
        events (atom [])
        invoke (fn [mode]
                 (executor {:biff.xtdb/node (:node *pilot*)
                            resolved/observation-context-key
                            #(swap! events conj %)}
                           {:binding binding :mode mode}))
        replay (invoke :submit)
        reconcile (invoke :reconcile)]
    (is (= :replay (:status replay)))
    (is (= :replay (:status reconcile)))
    (is (= (:outcome replay) (:outcome reconcile)))
    (is (empty? @events) "A replay never issues a new physical attempt")
    (is (= (:observed-record (good))
           (physical-row (:node *pilot*) :gesso_commands
                         (resolution/record-id binding))))))

(deftest invalid-domain-candidate-is-stopped-before-any-xtdb-send
  (let [{:keys [executor environment plan-source]} (:package *pilot*)
        node (:node *pilot*)
        proposed (:proposed environment)
        original @proposed
        events (atom [])
        before ((:current-document! plan-source) {} {})
        _ (xt/execute-tx node [[:put-docs :requests before]])
        binding (command-binding)]
    (try
      (swap! proposed assoc :model/after
             (assoc (:model/after original) :request/status :open))
      (let [data (exception-data
                  #(executor {:biff.xtdb/node node
                              resolved/observation-context-key
                              (fn [event] (swap! events conj event))}
                             {:binding binding :mode :submit}))]
        (is (= :gesso.model.certified-plan/invalid-certified-plan
               (:error/type data)) (pr-str data))
        (is (empty? @events) "No prepared or physical event on rejection")
        (is (nil? (physical-row node :gesso_commands
                                 (resolution/record-id binding))))
        (is (= before (physical-row node :requests (:xt/id before)))))
      (finally
        (reset! proposed original)))))

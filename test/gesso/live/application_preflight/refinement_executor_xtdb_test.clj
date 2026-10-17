(ns gesso.live.application-preflight.refinement-executor-xtdb-test
  "v1045 real embedded-XTDB integration for the v1043 executor-bound
   candidate-evidence boundary. The source-derived physical observation,
   process-local registry, exact registered executor and application assembly
   are genuine. Proposal descriptors remain deliberately UNAUTHENTICATED;
   acceptance establishes only readiness for independent review of ONE
   executed transaction, never a theorem or application admission."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight :as application]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.optimistic.resolved-operation :as adapter]
            [gesso.live.optimistic.resolution-assurance :as assurance]
            [gesso.live.optimistic.server :as server]
            [gesso.model.command :as command]
            [gesso.model.resolved-operation :as resolved]
            [gesso.model.tx-refinement-premises :as premises]
            [gesso.model.tx-refinement-subject :as subject]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(def ^:private op :request/claim)
(def ^:private now "2026-10-16T20:00:00Z")
(def ^:private hash-a (apply str (repeat 64 "a")))
(def ^:private hash-b (apply str (repeat 64 "b")))
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(def ^:private principal (identity/principal "v1045-private-principal"))

(defn- row [table id]
  (first (xt/q @!node [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- fixture-operation []
  (let [suffix (str (random-uuid))
        request-id (str "v1045-private-request-" suffix)
        membership-id (str "v1045-private-member-" suffix)
        before {:xt/id request-id :revision 0 :created-at "t0"
                :updated-at "t0" :owner "unclaimed"}
        membership {:xt/id membership-id :revision 0 :created-at "t0"
                    :updated-at "t0" :active true}
        claimed (assoc before :revision 1 :updated-at now
                       :owner "v1045-private-principal")
        cmd (protocol/command
             {:command-id (identity/command-id (str "v1045-command-" suffix))
              :execution-id (identity/execution-id (str "v1045-execution-" suffix))
              :operation op :arguments {:request-id request-id}
              :observed-basis {:tx-id 1 :system-time now}
              :scope [:request request-id]})
        calls (atom 0)
        m (resolved/build-operation
           {:operation op
            :read-record! (fn [_ id] (row :gesso_commands id))
            :may-disclose? (fn [_ _ _] true)
            :construct! (fn [_ _]
                          (swap! calls inc)
                          {:now now :outcome {:status :claimed}
                           :plan {:commands [(command/update-command
                                              :requests :claim before claimed version)]
                                  :guards [(command/guard :memberships membership version)]
                                  :emit false}})})
        executor (adapter/build-executor
                  {:operation op :resolved-operation m
                   :submission-mode! (fn [_] :submit)
                   :project! (fn [_ result]
                               {:resolution (if (= :replay (:status result))
                                              :already-incorporated :confirmed)
                                :authoritative
                                (protocol/authoritative
                                 {:presence :present
                                  :basis {:tx-id 2 :system-time now}
                                  :projection {:request/owner
                                               (:owner (row :requests request-id))}})
                                :outcome :request/claimed})})
        srv (server/server
             {:principal-fn (fn [ctx] (:authenticated-principal ctx))
              :operations
              {op (server/operation
                   {:name :request/claim-optimistic :operation op :execute! executor
                    :settlement-contract
                    {:confirmed {:outcomes #{:request/claimed} :commit/status :committed}
                     :already-incorporated {:outcomes #{:request/claimed}
                                            :commit/status :committed}}})}})]
    (xt/execute-tx @!node [[:put-docs :requests before]
                            [:put-docs :memberships membership]])
    {:before before :membership membership :claimed claimed
     :request-id request-id :membership-id membership-id
     :command cmd :executor executor :server srv :calls calls}))

(defn- execute! [f]
  (assurance/run-and-check!
   {:server (:server f)
    :ctx {:biff.xtdb/node @!node :authenticated-principal principal}
    :command (:command f)}))

(defn- with-application [executor f]
  (fixture/with-closed-application
   {:operations
    (assoc (fixture/standard-operations)
           op (fixture/trusted-operation
               op {:published-change-topics #{:request} :execute! executor}))}
   f))

(defn- kinds [route]
  (case route
    :versioned-backend-contract-or-proof #{:versioned-backend-contract}
    :predicate-equivalence-proof #{:independent-predicate-proof}
    :versioned-backend-contract-and-representation-proof
    #{:versioned-backend-contract :representation-refinement-proof}
    :internal-read-state-refinement-proof #{:internal-read-state-proof}
    :closed-world-and-dependency-proof #{:application-dependency-closure-proof}
    :independent-prestate-provenance-proof #{:abstract-prestate-provenance-proof}
    (throw (ex-info "Unknown premise discharge route" {:route route}))))

(defn- observed-instance [handle executor]
  (let [result (assurance/inspect-evidence-for-executor handle executor)
        observation (:observation result)]
    (when-not (= :consistent (get-in observation [:evidence :status]))
      (throw (ex-info "Expected genuine consistent XTDB observation"
                      {:result-status (:status result)})))
    {:state (:initial-state observation)
     :plan (get-in observation [:submitted-incorporation :plan])}))

(defn- environment [plan]
  {:backend {:product :xtdb :version "2.2.0-beta1"
             :build-sha256 hash-a :settings {:clock :system}}
   :gesso {:source-sha256 hash-a}
   :application {:id :fixture/v1045 :operation op :source-sha256 hash-a
                 :dependency-targets (vec (distinct
                                          (concat (map command/target (:commands plan))
                                                  (map command/guard-target (:guards plan)))))
                 :writer-ids [:fixture/only-controlled-writer]
                 :policy-rule-ids [:fixture/membership-active-policy]}})

(defn- proposals [state plan env]
  (let [digest (:subject-sha256 (subject/derive-subject state plan env))
        ledger (premises/derive-obligation-ledger state plan)]
    (vec
     (mapcat (fn [{:keys [id discharge-route]}]
               (for [kind (sort (kinds discharge-route))]
                 {:premise-id id :evidence-kind kind
                  :subject (:obligation ledger) :artifact-sha256 hash-a
                  :claimed-issuer "v1045-secret-unverified-issuer"
                  :artifact-version "unverified-v1045"
                  :instance-subject-sha256 digest}))
             (:open-premises ledger)))))

(defn- selection [handle env ps]
  {op {:evidence-handle handle :environment env :proposals ps}})

(defn- reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (:reason (ex-data e)))))

(deftest real-xtdb-commit-exact-executor-and-aggregate-package-test
  (let [f (fixture-operation)
        {:keys [prepared-send evidence]} (execute! f)
        {:keys [state plan]} (observed-instance evidence (:executor f))
        env (environment plan)
        ps (proposals state plan env)]
    (is (server/prepared-send? prepared-send))
    (is (= (:claimed f) (row :requests (:request-id f))))
    (is (= 9 (count (:open-premises (premises/derive-obligation-ledger state plan)))))
    (is (= 10 (count ps)))
    (with-application (:executor f)
      (fn [{:keys [assembly]}]
        (let [before @(:calls f)
              result (application/require-executor-bound-refinement-candidates!
                      assembly (selection evidence env ps))
              printed (pr-str result)]
          (is (= :observed-executor-candidates-ready-for-independent-review
                 (:status result)))
          (is (= :exact-registered-executor-controlled-attempt (:scope result)))
          (is (= :process-local-executor-bound-observation (:evidence-origin result)))
          (is (= 1 (:observation-count result)))
          (is (= :complete-unverified-candidate-package
                 (get-in result [:selected-operations op :status])))
          (is (true? (get-in result [:selected-operations op
                                    :ready-for-independent-review?])))
          (doseq [flag [:evidence-authenticated? :premises-discharged?
                        :application-admitted? :application-guarantee-upgraded?]]
            (is (false? (flag result)) (str "No proof promotion: " flag)))
          (is (= before @(:calls f)) "Review must never reexecute Claim")
          (doseq [secret [(:request-id f) (:membership-id f)
                          "v1045-secret-unverified-issuer" hash-a]]
            (is (not (str/includes? printed secret))
                "Returned decision must not serialize private inputs")))))))

(deftest genuine-observation-rejects-forged-reused-and-incomplete-evidence
  (let [f (fixture-operation)
        {:keys [evidence]} (execute! f)
        {:keys [state plan]} (observed-instance evidence (:executor f))
        env (environment plan)
        ps (proposals state plan env)]
    (with-application (:executor f)
      (fn [{:keys [assembly]}]
        (let [review (fn [handle e p]
                       (application/require-executor-bound-refinement-candidates!
                        assembly (selection handle e p)))]
          (is (= :aggregate-package-not-ready
                 (reason #(review evidence env (vec (butlast ps))))))
          (is (= :aggregate-package-not-ready
                 (reason #(review evidence env (conj ps (first ps))))))
          (is (= :aggregate-package-not-ready
                 (reason #(review evidence
                                  (assoc-in env [:backend :build-sha256] hash-b)
                                  ps))))
          (is (= :aggregate-package-not-ready
                 (reason #(review evidence
                                  (assoc-in env [:application :writer-ids]
                                            [:fixture/another-writer])
                                  ps))))
          (doseq [forged [(Object.)
                          (assurance/inspect-evidence evidence)]]
            (is (= :unrecognized-executor-observation
                   (reason #(review forged env ps))))))))))

(deftest genuine-handle-cannot-authorize-a-different-registered-executor
  (let [f (fixture-operation)
        {:keys [evidence]} (execute! f)
        {:keys [state plan]} (observed-instance evidence (:executor f))
        env (environment plan)
        ps (proposals state plan env)
        other (adapter/build-executor
               {:operation op :resolved-operation
                (resolved/build-operation
                 {:operation op :read-record! (fn [_ _] nil)
                  :may-disclose? (fn [_ _ _] true)
                  :construct! (fn [_ _]
                                (throw (ex-info "Must not execute" {})))})
                :submission-mode! (fn [_] :submit)
                :project! (fn [_ _]
                            (throw (ex-info "Must not project" {})))})]
    (with-application other
      (fn [{:keys [assembly]}]
        (is (= :unrecognized-executor-observation
               (reason #(application/require-executor-bound-refinement-candidates!
                         assembly (selection evidence env ps)))))))))

(deftest real-claim-requires-command-ledger-in-declared-subject-footprint
  ;; The resolved-operation adapter adds a durable command-result write to
  ;; the domain plan. A developer copying only the Request/Membership targets
  ;; omits a real dependency; reject rather than issue an apparently complete
  ;; candidate-review result for that incomplete declaration.
  (let [f (fixture-operation)
        {:keys [evidence]} (execute! f)
        {:keys [state plan]} (observed-instance evidence (:executor f))
        env (environment plan)
        ps (proposals state plan env)
        command-targets (set (filter #(= :gesso_commands (first %))
                                     (get-in env [:application :dependency-targets])))
        incomplete (update-in env [:application :dependency-targets]
                              #(vec (remove command-targets %)))]
    (is (= 1 (count command-targets)))
    (is (= :incomplete-subject-contract
           (try (subject/derive-subject state plan incomplete) nil
                (catch clojure.lang.ExceptionInfo e
                  (:error/reason (ex-data e))))))
    (with-application (:executor f)
      (fn [{:keys [assembly]}]
        (is (= :invalid-instance-or-evidence-input
               (reason #(application/require-executor-bound-refinement-candidates!
                         assembly (selection evidence incomplete ps)))))))))

(deftest real-handle-cannot-override-stale-application-browser-artifact
  (let [f (fixture-operation)
        {:keys [evidence]} (execute! f)
        {:keys [state plan]} (observed-instance evidence (:executor f))
        env (environment plan)
        ps (proposals state plan env)]
    (with-application (:executor f)
      (fn [{:keys [assembly artifact-path]}]
        (is (= :observed-executor-candidates-ready-for-independent-review
               (:status (application/require-executor-bound-refinement-candidates!
                         assembly (selection evidence env ps)))))
        (spit artifact-path "console.log('v1045-tampered-artifact')\n"
              :encoding "UTF-8")
        (is (= :invalid-or-stale-application-assembly
               (reason #(application/require-executor-bound-refinement-candidates!
                         assembly (selection evidence env ps)))))))))

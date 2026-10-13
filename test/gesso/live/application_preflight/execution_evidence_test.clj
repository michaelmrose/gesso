(ns gesso.live.application-preflight.execution-evidence-test
  "Permanent v817 regressions for application/explain-with-execution-evidence.

   The canonical explanation must bind a process-local diagnostic handle to
   the EXACT executor recovered from a freshly checked ApplicationAssembly.
   Real embedded XTDB execution establishes only bounded correspondence of
   an observed transaction, not business authorization, complete guards,
   truthful projections, or universal refinement.

   Tests use their own authoritative fixtures. No fake certificate, mutated
   metadata, detached report, or alternate adapter may substitute for a handle
   produced by the actual registered Live executor."
  (:require
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
          (is (nil? (observed-card enhanced)))
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
          (is (= 1 (:observation-count card)))
          (is (= [{:index 0
                   :status :bounded-correspondence-observed
                   :physical-status :committed
                   :finite-reference-status :consistent}]
                 (:observations card)))
          (is (= assurance/assurance-scope (:scope card)))
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
          (is (contains? (:not-established (observed-summary result))
                         :truthful-application-projections))
          (assert-unproved-policy result))))))

(ns gesso.model.resolution-event-observation-test
  "Permanent v812 regressions for the single-attempt v811 event bridge.
   Tests use actual embedded XTDB and ordinary resolved operations.
   Successful evidence is bounded diagnostic consistency, not authenticated
   transaction history, proof of policy guards, or formal refinement."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as cr]
            [gesso.model.resolved-operation :as ro]
            [gesso.model.resolution-event-observation :as bridge]
            [gesso.model.resolution-passive-observation :as passive]
            [gesso.live.core :as live]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))
(defn- db [] @!node)
(defn- db-fixture [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try
      (f)
      (finally (reset! !node nil)))))
(use-fixtures :once db-fixture)

(defn- read-row [table id]
  (first (xt/q (db) [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))
(def ^:private version
  {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at})
(defn- fixture! []
  (let [suffix (str (random-uuid))
        req {:xt/id (str "v812-request-" suffix)
             :revision 0 :created-at "t0" :updated-at "t0" :owner "unclaimed"}
        member {:xt/id (str "v812-member-" suffix)
                :revision 0 :created-at "t0" :updated-at "t0" :active true}
        binding {:command-id (identity/command-id (str "v812-command-" suffix))
                 :principal (identity/principal "helper-a")
                 :operation :request/claim
                 :arguments {:request-id (:xt/id req)}}
        claimed (assoc req :owner "helper-a" :revision 1 :updated-at "t1")
        outcome {:status :claimed :owner "helper-a"}
        constructs (atom 0)
        op (ro/build-operation
            {:operation :request/claim
             :read-record! (fn [_ row-id] (read-row :gesso_commands row-id))
             :may-disclose? (fn [& _] true)
             :construct! (fn [_ _]
                           (swap! constructs inc)
                           {:now "t1"
                            :outcome outcome
                            :plan {:commands [(command/update-command
                                                :requests :claim req claimed version)]
                                   :guards [(command/guard :memberships member version)]
                                   :emit false}})})]
    (xt/execute-tx (db) [[:put-docs :requests req]
                         [:put-docs :memberships member]])
    {:request req :membership member :binding binding :claimed claimed
     :outcome outcome :constructs constructs :operation op}))
(defn- submit! [e session]
  ((:operation e)
   {:biff.xtdb/node (db) ro/observation-context-key (:observe! session)}
   {:binding (:binding e) :mode :submit}))
(defn- error-kind [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:error/kind (ex-data e)))))


(defn- session [e]
  (bridge/start-session! {:node (db) :binding (:binding e)}))

(defn- persisted-record [e]
  (read-row :gesso_commands (cr/record-id (:binding e))))

(defn- prepare-event [e]
  ;; Construct a structurally valid event for negative event-corruption tests.
  ;; Its mere existence is NOT proof that this plan was physically submitted.
  (let [req (:request e)
        member (:membership e)
        claimed (:claimed e)
        incorporated (cr/incorporation-plan
                      {:binding (:binding e)
                       :outcome (:outcome e)
                       :now "t1"
                       :plan {:commands [(command/update-command
                                           :requests :claim req claimed version)]
                              :guards [(command/guard :memberships member version)]
                              :emit false}})]
    {:phase :prepared
     :attempt-id (random-uuid)
     :operation :request/claim
     :binding (:binding e)
     :record (:record incorporated)
     :plan (:plan incorporated)
     :scope ro/execution-observation-scope}))

(defn- physical-event [prepared status source]
  (assoc prepared :phase :physical-outcome
         :physical/status status :physical/source source))

(defn- poisoned? [s]
  (= :observation-hook-failed (error-kind (:complete! s))))


(deftest scope-truthfully-describes-finite-diagnostic-evidence-test
  (let [{:keys [checked assumed not-established]} bridge/observation-scope]
    (is (contains? checked :exact-event-pair))
    (is (contains? checked :no-transaction-submission-by-observer))
    (is (contains? assumed :event-source-is-actual-resolved-operation))
    (is (contains? assumed :consistent-unpinned-xtdb-reads))
    (is (contains? not-established :authenticated-event-or-transaction-history))
    (is (contains? not-established :complete-policy-guards-or-domain-invariants))
    (is (contains? not-established :formal-refinement-proof))))

(deftest request-and-binding-are-closed-and-valid-test
  (let [e (fixture!)
        valid {:node (db) :binding (:binding e)}]
    (is (= :invalid-request (error-kind #(bridge/start-session! (assoc valid :extra true)))))
    (is (= :invalid-request (error-kind #(bridge/start-session! (dissoc valid :binding)))))
    (is (= :missing-node (error-kind #(bridge/start-session! (assoc valid :node nil)))))
    (is (= :invalid-binding (error-kind #(bridge/start-session! (assoc valid :binding {})))))
    (is (fn? (:observe! (bridge/start-session! valid))))))

(deftest acknowledged-xtdb-commit-produces-physical-correspondence-test
  (let [e (fixture!)
        s (session e)
        result (submit! e s)
        checked ((:complete! s))]
    (is (= :incorporated (:status result)))
    (is (= :consistent (get-in checked [:evidence :status])))
    (is (= [:committed] (get-in checked [:observations :statuses])))
    (is (= (:claimed e) (read-row :requests (get-in e [:request :xt/id]))))
    (is (= (cr/command-record (:binding e) (:outcome e) "t1") (persisted-record e)))
    (is (= :committed (get-in checked [:physical :status])))
    (is (= :gesso-model-transaction-ack (get-in checked [:physical :source])))
    (is (contains? (:physical checked) :receipt))
    (is (= (:binding e) (:binding checked)))
    (is (uuid? (:attempt-id checked)))
    (is (= bridge/observation-scope (:bridge-scope checked)))))

(deftest pre-revoked-membership-produces-checked-explicit-rejection-test
  (let [e (fixture!)
        member (:membership e)]
    (xt/execute-tx (db)
                   (:tx-ops (tx/prepare {}
                                        {:commands [(command/update-command
                                                    :memberships :revoke member
                                                    (assoc member :revision 1 :updated-at "t1" :active false)
                                                    version)]
                                         :emit false})))
    (let [s (session e)
          result (submit! e s)
          checked ((:complete! s))]
      (is (= :unresolved (:status result)))
      (is (= :consistent (get-in checked [:evidence :status])))
      (is (= [:rejected] (get-in checked [:observations :statuses])))
      (is (= :xtdb-explicit-assertion-failure (get-in checked [:physical :source])))
      (is (= :rejected (get-in checked [:physical :status])))
      (is (nil? (get-in checked [:physical :receipt])))
      (is (= (:request e) (read-row :requests (get-in e [:request :xt/id]))))
      (is (nil? (persisted-record e))))))

(deftest classified-post-commit-failure-still-checks-committed-physical-state-test
  (let [e (fixture!)
        s (session e)
        actual-transact! tx/transact!]
    (with-redefs [tx/transact!
                  (fn [ctx plan]
                    (let [ack (actual-transact! ctx plan)]
                      (throw (ex-info "delivery failed after durable commit"
                                      {:error/type live/post-commit-delivery-failure-type
                                       :commit/status :committed
                                       :failure/stage :post-commit-delivery
                                       :tx-result (:tx-result ack)}))))]
      ;; Reconciliation may return a replay: the physical result and the
      ;; application-level delivery outcome are intentionally distinct.
      (is (= :replay (:status (submit! e s)))))
    (let [result ((:complete! s))]
      (is (= :consistent (get-in result [:evidence :status])))
      (is (= [:committed] (get-in result [:observations :statuses])))
      (is (= :committed (get-in result [:physical :status])))
      (is (= :gesso-live-classified-post-commit-failure
             (get-in result [:physical :source])))
      (is (= (:claimed e) (read-row :requests (get-in e [:request :xt/id]))))
      (is (= (cr/command-record (:binding e) (:outcome e) "t1")
             (persisted-record e))))))

(deftest durable-replay-never-produces-a-new-physical-witness-test
  (let [e (fixture!)
        first-result ((:operation e) {:biff.xtdb/node (db)}
                      {:binding (:binding e) :mode :submit})
        s (session e)
        replay (submit! e s)]
    (is (= :incorporated (:status first-result)))
    (is (= :replay (:status replay)))
    (is (= :incomplete-event-pair (error-kind (:complete! s))))
    (is (= 1 @(:constructs e)))
    (is (= (:claimed e) (read-row :requests (get-in e [:request :xt/id]))))))

(deftest reconcile-mode-without-a-physical-attempt-is-not-evidence-test
  (let [e (fixture!)
        s (session e)
        result ((:operation e)
                {:biff.xtdb/node (db) ro/observation-context-key (:observe! s)}
                {:binding (:binding e) :mode :reconcile})]
    (is (= :unresolved (:status result)))
    (is (zero? @(:constructs e)))
    (is (= :incomplete-event-pair (error-kind (:complete! s))))
    (is (nil? (persisted-record e)))))

(deftest unknown-tx-outcome-is-never-an-explicit-rejection-test
  (let [e (fixture!)
        s (session e)]
    (with-redefs [tx/transact! (fn [& _] (throw (ex-info "lost acknowledgment" {})))]
      (is (= :unresolved (:status (submit! e s)))))
    (is (= :nondefinitive-outcome (error-kind (:complete! s))))
    (is (nil? (persisted-record e)))))

(deftest no-events-or-only-prepared-event-never-constitutes-a-witness-test
  (let [e (fixture!)
        missing (session e)
        incomplete (session e)]
    (is (= :incomplete-event-pair (error-kind (:complete! missing))))
    ((:observe! incomplete) (prepare-event e))
    (is (= :incomplete-event-pair (error-kind (:complete! incomplete))))
    (is (nil? (persisted-record e)))))

(deftest malformed-event-or-unknown-phase-poisons-session-test
  (doseq [event-fn [(constantly {:phase :prepared :junk true})
                    #(assoc (prepare-event %) :phase :arbitrary)
                    #(assoc (prepare-event %) :additional-field 42)]]
    (let [e (fixture!)
          s (session e)]
      (is (some? (error-kind #((:observe! s) (event-fn e)))))
      (is (poisoned? s)))))

(deftest identity-and-binding-tampering-poison-session-test
  (doseq [corrupt [(fn [event] (assoc event :scope {}))
                   (fn [event] (assoc event :attempt-id "fake-attempt"))
                   (fn [event] (assoc event :operation :request/cancel))
                   (fn [event] (assoc-in event [:record :gesso.command/outcome] {:status :forged}))
                   (fn [event] (assoc event :binding
                                      (assoc (:binding event) :arguments {:request-id "different"})))]]
    (let [e (fixture!)
          s (session e)]
      (is (some? (error-kind #((:observe! s) (corrupt (prepare-event e))))))
      (is (poisoned? s)))))

(deftest one-operation-cannot-use-another-operation-session-test
  (let [a (fixture!)
        b (fixture!)
        s (session a)]
    (is (= :event-identity-mismatch
           (error-kind #((:observe! s) (prepare-event b)))))
    (is (poisoned? s))))

(deftest physical-outcome-before-prepared-is-rejected-test
  (let [e (fixture!)
        s (session e)
        p (prepare-event e)]
    (is (= :unexpected-outcome
           (error-kind #((:observe! s)
                          (physical-event p :committed :gesso-model-transaction-ack)))))
    (is (poisoned? s))))

(deftest mismatched-outcome-attempt-or-plan-is-rejected-test
  (doseq [corrupt [(fn [event] (assoc event :attempt-id (random-uuid)))
                   (fn [event] (assoc event :plan (assoc (:plan event) :emit true)))
                   (fn [event] (assoc event :record
                                      (assoc (:record event) :gesso.command/outcome {:status :forged})))]]
    (let [e (fixture!)
          s (session e)
          p (prepare-event e)]
      ((:observe! s) p)
      (is (some? (error-kind #((:observe! s)
                               (corrupt (physical-event p :committed :gesso-model-transaction-ack))))))
      (is (poisoned? s)))))

(deftest duplicate-prepared-events-are-not-a-second-attempt-test
  (let [e (fixture!)
        s (session e)
        p (prepare-event e)]
    ((:observe! s) p)
    (is (= :extra-prepared-event (error-kind #((:observe! s) p))))
    (is (poisoned? s))))

(deftest duplicate-physical-outcome-is-not-reusable-evidence-test
  (let [e (fixture!)
        s (session e)
        p (prepare-event e)
        outcome (physical-event p :rejected :xtdb-explicit-assertion-failure)]
    ((:observe! s) p)
    ((:observe! s) outcome)
    (is (= :unexpected-outcome (error-kind #((:observe! s) outcome))))
    (is (poisoned? s))))

(deftest nondefinitive-physical-labels-and-sources-cannot-pass-test
  (doseq [[status source] [[:indeterminate :unclassified-transaction-exception]
                           [:rejected :gesso-model-transaction-ack]
                           [:committed :unclassified-transaction-exception]
                           [:rejected :gesso-live-classified-post-commit-failure]
                           [:committed :xtdb-explicit-assertion-failure]
                           [:replay :gesso-model-transaction-ack]]]
    (let [e (fixture!)
          s (session e)
          p (prepare-event e)]
      ((:observe! s) p)
      ((:observe! s) (physical-event p status source))
      (is (= :nondefinitive-outcome (error-kind (:complete! s)))))))

(deftest already-closed-session-refuses-further-events-test
  (let [e (fixture!)
        s (session e)
        _ (error-kind (:complete! s))]
    (is (= :session-closed (error-kind #((:observe! s) (prepare-event e)))))
    (is (= :observation-hook-failed (error-kind (:complete! s))))))

(deftest failed-initial-snapshot-does-not-change-authoritative-execution-test
  (let [e (fixture!)
        s (session e)]
    (with-redefs [passive/begin-observation!
                  (fn [& _] (throw (ex-info "injected read failure" {})))]
      (is (= :incorporated (:status (submit! e s)))))
    (is (poisoned? s))
    (is (= (:claimed e) (read-row :requests (get-in e [:request :xt/id]))))
    (is (= (cr/command-record (:binding e) (:outcome e) "t1")
           (persisted-record e)))))

(deftest failed-final-read-never-becomes-a-consistency-witness-test
  (let [e (fixture!)
        s (session e)]
    (is (= :incorporated (:status (submit! e s))))
    (with-redefs [passive/complete-observation!
                  (fn [& _] (throw (ex-info "injected final read failure" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"injected final read failure"
                            ((:complete! s)))))
    (is (= :incomplete-event-pair (error-kind (:complete! s))))
    (is (= (:claimed e) (read-row :requests (get-in e [:request :xt/id]))))))

(deftest unrelated-modification-of-a-tracked-document-produces-counterexample-test
  (let [e (fixture!)
        s (session e)
        _ (submit! e s)]
    ;; The capture covers this request; an extraneous write between execution
    ;; and final query invalidates the no-unrelated-writes assumption.
    (xt/execute-tx (db)
                   [[:put-docs :requests
                     (assoc (:claimed e) :owner "unexpected-external-writer")]])
    (let [result ((:complete! s))]
      (is (= :counterexample (get-in result [:evidence :status])))
      (is (= :persisted-state-disagrees (get-in result [:evidence :reason])))
      (is (= [:committed] (get-in result [:observations :statuses]))))))

(deftest recognized-committed-labels-still-require-state-correspondence-test
  (let [e (fixture!)
        s (session e)
        p (prepare-event e)]
    ;; Forged hook events can be structurally accepted. No physical tx occurs,
    ;; so the actual XTDB after-state MUST not be certified as consistent.
    ((:observe! s) p)
    ((:observe! s) (physical-event p :committed :gesso-model-transaction-ack))
    (let [result ((:complete! s))]
      (is (= :committed (get-in result [:physical :status])))
      (is (= :counterexample (get-in result [:evidence :status])))
      (is (= :persisted-state-disagrees (get-in result [:evidence :reason])))
      (is (nil? (persisted-record e))))))

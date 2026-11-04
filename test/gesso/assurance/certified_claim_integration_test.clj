(ns gesso.assurance.certified-claim-integration-test
  "v1256: physical XTDB evidence for the v1255 Gesso-local certified Claim.

   A real embedded XTDB node receives actual gesso.model.tx submissions through
   the issued v1251 certified-plan executor. No tx/transact!, xt/execute-tx,
   certificate, or proof-checker function is mocked. The persisted request AND
   durable command record are read back independently of the proposed plan.

   Negative inputs and stale guards must not mutate the request or create a
   command record. Two competing first Claims use different command IDs and
   the same old pre-state, and their physical outcomes must exhibit one winner.
   Rejected physical assertions are observed separately from the application
   result :unresolved; no missing record is called a business rejection.

   XTDB omits present-nil fields in SELECT * results. The fixture's original
   abstract state declares :request/owner as a required nullable field, so
   the application-owned acquisition adapter reconstructs an absent owner as
   nil. This is an EXPLICIT TRUSTED PROJECTION ASSUMPTION and is tested here;
   it does not establish general absence/nil equivalence or arbitrary-schema
   correspondence.

   These are bounded, single-node physical execution observations, NOT proofs
   of universal invariant preservation, callback correctness, authorization,
   deployment adoption, independent checker soundness, or linearizability."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.certified-claim-fixture :as claim]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation :as resolved]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defonce ^:private !db (atom nil))
(defonce ^:private !id (atom 12560000))

(defn- db []
  (or @!db (throw (ex-info "Embedded XTDB fixture not started" {}))))

(defn- with-real-xtdb [f]
  (with-open [database (xtn/start-node {})]
    (reset! !db database)
    (try (f)
         (finally (reset! !db nil)))))

(use-fixtures :once with-real-xtdb)

(defn- new-id [] (swap! !id inc))
(defn- read-one [table id]
  (let [rows (xt/q (db) [(str "SELECT * FROM " (name table)
                              " WHERE _id = ?") id])]
    (when (> (count rows) 1)
      (throw (ex-info "Expected at most one XTDB document" {:table table})))
    (first rows)))

(defn- persisted-request [id] (read-one :requests id))
(defn- persisted-record [binding]
  (read-one :gesso_commands (resolution/record-id binding)))

(defn- acquired-request [id]
  ;; The physical query discards nil-valued fields. The exact abstract state
  ;; requires a present nullable owner. This narrow projection is trusted.
  (when-let [raw (persisted-request id)]
    (if (contains? raw :request/owner)
      raw
      (assoc raw :request/owner nil))))

(defn- original-request [id]
  {:xt/id id
   :request/status :open
   :request/owner nil
   :request/revision 0
   :request/organization :organization/one
   :request/description "Help with groceries"})

(defn- seed-request! []
  (let [id (new-id)
        original (original-request id)]
    (xt/execute-tx (db) [[:put-docs :requests original]])
    original))

(defn- command-binding [request-id actor]
  {:command-id (identity/command-id (str "v1256-" request-id "-" (random-uuid)))
   :principal (identity/principal actor)
   :operation claim/claim-operation
   :arguments {:request-id request-id}})

(defn- environment
  ([request-id actor] (environment request-id actor {}))
  ([request-id actor {:keys [authorization! read-current!]
                      :or {authorization! (fn [_ _] {:auth/decision :permit})}}]
   (claim/new-claim-environment!
    {:request-id request-id
     :actor actor
     :organization :organization/one
     :read-current! (or read-current!
                        (fn [_ _] (acquired-request request-id)))
     :authorization! authorization!
     :read-record! (fn [_ record-id]
                     (read-one :gesso_commands record-id))
     :may-disclose? (fn [_ _ _] true)})))

(defn- invoke!
  ([env issued-binding mode]
   (invoke! env issued-binding mode nil))
  ([env issued-binding mode events]
   ((:executor env)
    (cond-> {:biff.xtdb/node (db)}
      events (assoc resolved/observation-context-key
                    #(swap! events conj %)))
    {:binding issued-binding :mode mode})))

(defn- exception-data [thunk]
  (try
    (thunk)
    nil
    (catch clojure.lang.ExceptionInfo error (ex-data error))))

(defn- physical [events]
  (let [outcomes (filter #(= :physical-outcome (:phase %)) @events)]
    (is (<= (count outcomes) 1) "At most one physical outcome per submission")
    (first outcomes)))

(defn- record-count [bindings]
  (count (filter some? (map persisted-record bindings))))

(defn- assert-request-unchanged! [before]
  (is (= before (acquired-request (:xt/id before)))))

(deftest real-claim-commits-domain-and-command-record-atomically
  (let [before (seed-request!)
        request-id (:xt/id before)
        actor :helper/alice
        issued (command-binding request-id actor)
        env (environment request-id actor)
        events (atom [])
        result (invoke! env issued :submit events)
        after (acquired-request request-id)
        record (persisted-record issued)]
    (is (= :incorporated (:status result)))
    (is (= :committed (get-in result [:transaction :commit/status])))
    (is (= {:request/status :claimed
            :request/owner actor
            :request/revision 1}
           (select-keys after [:request/status :request/owner
                               :request/revision])))
    (is (= (:request/organization before) (:request/organization after)))
    (is (= (:request/description before) (:request/description after)))
    (is (= (set (keys before)) (set (keys after))))
    (is (= :incorporated (:gesso.command/status record)))
    (is (= issued (:gesso.command/binding record)))
    (is (= {:status :claimed :request-id request-id}
           (:gesso.command/outcome record)))
    (is (= [:prepared :physical-outcome] (mapv :phase @events)))
    (is (= :committed (:physical/status (physical events))))
    (is (= :gesso-model-transaction-ack
           (:physical/source (physical events))))
    (is (contains? (:open-obligations env)
                   :physical-xtdb-execution-correspondence))
    (is (= :not-established (:physical-one-winner-proof env)))))

(deftest same-command-replay-does-not-repeat-domain-work
  (let [before (seed-request!)
        id (:xt/id before)
        invoked (atom 0)
        issued (command-binding id :helper/alice)
        env (environment id :helper/alice
                         {:authorization! (fn [_ _]
                                            (swap! invoked inc)
                                            {:auth/decision :permit})})
        first-result (invoke! env issued :submit)
        after-first (acquired-request id)
        second-events (atom [])
        replay (invoke! env issued :submit second-events)]
    (is (= :incorporated (:status first-result)))
    (is (= :replay (:status replay)))
    (is (= 1 @invoked) "Replay must not rerun the opaque authorization callback")
    (is (= after-first (acquired-request id)))
    (is (= 1 (record-count [issued])))
    (is (empty? @second-events) "Replay performs no new physical submission")))

(deftest denied-claim-never-submits-physical-plan
  (let [before (seed-request!)
        id (:xt/id before)
        issued (command-binding id :helper/alice)
        auth-calls (atom 0)
        events (atom [])
        env (environment id :helper/alice
                         {:authorization! (fn [_ _]
                                            (swap! auth-calls inc)
                                            {:auth/decision :deny})})
        error (exception-data #(invoke! env issued :submit events))]
    (is (= :current-state-guard-failed (:reason error)))
    (is (= 1 @auth-calls))
    (is (empty? @events) "There must be no prepared/physical event")
    (assert-request-unchanged! before)
    (is (nil? (persisted-record issued)))))

(deftest unknown-opaque-authorization-result-fails-before-send
  (let [before (seed-request!)
        id (:xt/id before)
        issued (command-binding id :helper/alice)
        events (atom [])
        env (environment id :helper/alice
                         {:authorization! (fn [_ _]
                                            {:auth/decision :unknown})})
        error (exception-data #(invoke! env issued :submit events))]
    (is (= :gesso.assurance.rule-eval/invalid-rule-evaluation
           (:error/type error)))
    (is (some? error))
    (is (empty? @events))
    (assert-request-unchanged! before)
    (is (nil? (persisted-record issued)))))

(deftest claimed-or-wrong-organization-prestates-fail-before-send
  (doseq [[update-state expected-reason]
          [[{:request/status :claimed :request/owner :helper/bob}
            :stale-update-check]
           [{:request/organization :organization/two}
            :current-state-guard-failed]]]
    (let [before (seed-request!)
          id (:xt/id before)
          changed (merge before update-state)
          _ (xt/execute-tx (db) [[:put-docs :requests changed]])
          issued (command-binding id :helper/alice)
          events (atom [])
          env (environment id :helper/alice)
          error (exception-data #(invoke! env issued :submit events))]
      (is (= expected-reason (:reason error)))
      (is (empty? @events))
      (is (= changed (acquired-request id)))
      (is (nil? (persisted-record issued))))))

(deftest stale-snapshot-reaches-xtdb-assertion-not-a-false-commit
  (let [before (seed-request!)
        id (:xt/id before)
        other (assoc before :request/status :claimed
                            :request/owner :helper/bob
                            :request/revision 1)
        _ (xt/execute-tx (db) [[:put-docs :requests other]])
        issued (command-binding id :helper/alice)
        events (atom [])
        ;; This deliberately stale callback is in the TRUSTED boundary. The
        ;; pre-submit checker cannot know its purported current state is old.
        env (environment id :helper/alice
                         {:read-current! (fn [_ _] before)})
        result (invoke! env issued :submit events)]
    (is (= :unresolved (:status result))
        "Absent command record is not evidence of business rejection")
    (is (= [:prepared :physical-outcome] (mapv :phase @events)))
    (is (= :rejected (:physical/status (physical events))))
    (is (= :xtdb-explicit-assertion-failure
           (:physical/source (physical events))))
    (is (= other (acquired-request id)))
    (is (nil? (persisted-record issued)))))

(deftest two-competing-claims-with-one-old-snapshot-have-one-physical-winner
  (dotimes [round 3]
    (testing (str "bounded single-node claim contention round " round)
      (let [before (seed-request!)
            id (:xt/id before)
            ready (CountDownLatch. 2)
            start (CountDownLatch. 1)
            mk-env (fn [actor]
                     (environment id actor
                                  {:read-current! (fn [_ _] before)
                                   :authorization!
                                   (fn [_ _]
                                     (.countDown ready)
                                     (when-not (.await start 30 TimeUnit/SECONDS)
                                       (throw (ex-info "Race gate timeout" {})))
                                     {:auth/decision :permit})}))
            a (mk-env :helper/alice)
            b (mk-env :helper/bob)
            bindings [(command-binding id :helper/alice) (command-binding id :helper/bob)]
            events [(atom []) (atom [])]
            tasks [(future (invoke! a (first bindings) :submit (first events)))
                   (future (invoke! b (second bindings) :submit (second events)))]]
        (try
          (is (.await ready 30 TimeUnit/SECONDS)
              "Both claimants must reach authorization before release")
          (.countDown start)
          (let [results (mapv #(deref % 60000 ::timeout) tasks)
                statuses (mapv :status results)
                outcomes (mapv physical events)
                physically (mapv :physical/status outcomes)
                winner (first (keep-indexed (fn [i s] (when (= s :committed) i))
                                            physically))
                persisted (acquired-request id)]
            (is (not-any? #{::timeout} results))
            (is (= #{:incorporated :unresolved} (set statuses)))
            (is (= #{:committed :rejected} (set physically)))
            (is (= 1 (count (filter #{:committed} physically))))
            (is (= :xtdb-explicit-assertion-failure
                   (:physical/source (nth outcomes (bit-xor winner 1)))))
            (is (= 1 (record-count bindings)))
            (is (= :claimed (:request/status persisted)))
            (is (= 1 (:request/revision persisted)))
            (is (= (if (= winner 0) :helper/alice :helper/bob)
                   (:request/owner persisted))))
          (finally
            (.countDown start)
            (doseq [task tasks]
              (when-not (realized? task) (future-cancel task)))))))))

(deftest claims-on-disjoint-requests-can-both-commit
  (let [a (seed-request!)
        b (seed-request!)
        a-id (:xt/id a)
        b-id (:xt/id b)
        a-binding (command-binding a-id :helper/alice)
        b-binding (command-binding b-id :helper/bob)
        result-a (invoke! (environment a-id :helper/alice) a-binding :submit)
        result-b (invoke! (environment b-id :helper/bob) b-binding :submit)]
    (is (= [:incorporated :incorporated]
           (mapv :status [result-a result-b])))
    (is (= :helper/alice (:request/owner (acquired-request a-id))))
    (is (= :helper/bob (:request/owner (acquired-request b-id))))
    (is (= 2 (record-count [a-binding b-binding])))))

(deftest durable-record-is-not-transferable-to-another-binding
  (let [before (seed-request!)
        id (:xt/id before)
        good-binding (command-binding id :helper/alice)
        env (environment id :helper/alice)
        _ (invoke! env good-binding :submit)
        stolen (assoc good-binding :principal (identity/principal :helper/bob))
        events (atom [])
        result (invoke! env stolen :reconcile events)]
    (is (= :incompatible (:status result)))
    (is (empty? @events))
    (is (= :helper/alice (:request/owner (acquired-request id))))
    (is (= 1 (record-count [good-binding])))))

(deftest physical-evidence-never-becomes-abstract-or-production-proof
  (let [description (claim/describe-claim!)]
    (is (= :not-proved (:invariant-preservation description)))
    (is (= :not-established (:one-winner-physical-enforcement description)))
    (is (= :source-relative-abstract-constraints-only (:evidence-class description)))
    (doseq [obligation [:single-commit-atomic-status-and-revision-guard
                        :opaque-authorization-policy-correctness
                        :physical-xtdb-execution-correspondence
                        :proof-checker-meta-soundness]]
      (is (contains? (:open-obligations description) obligation)))))

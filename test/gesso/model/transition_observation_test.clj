(ns gesso.model.transition-observation-test
  "v1299: permanent adversarial regressions for the bounded, read-only XTDB
   persisted-transition observer introduced in v1298.

   These tests use the real embedded XTDB node and gesso.model.tx/prepare
   where indicated. A successful witness asserts a visible committed row and
   two pinned physical observations, NOT transaction-payload provenance,
   abstract/physical refinement, isolation of unseen application writers,
   authorization, or production certification. The negative tests deliberately
   construct misleading acknowledgements, partial footprints, and forged
   evidence to ensure those distinctions remain visible."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant ZonedDateTime]))

(def ^:private version
  {:revision-key :revision
   :created-at-key :created-at
   :updated-at-key :updated-at})

(defonce ^:private current-node (atom nil))

(defn- node []
  (or @current-node
      (throw (ex-info "Embedded XTDB fixture unavailable." {}))))

(defn- with-embedded-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! current-node db)
    (try (f)
         (finally (reset! current-node nil)))))

(use-fixtures :once with-embedded-xtdb)

(defn- fixture! []
  (let [id (str (random-uuid))
        before {:xt/id id :revision 0 :created-at "t0"
                :updated-at "t0" :owner "open"}
        after (assoc before :revision 1 :updated-at "t1" :owner "claimed")
        binding {:command-id (identity/command-id (str (random-uuid)))
                 :principal (identity/principal "helper")
                 :operation :request/claim
                 :arguments {:request id}}
        incorporation (resolution/incorporation-plan
                       {:binding binding
                        :outcome {:status :claimed}
                        :now "t1"
                        :plan {:commands [(command/update-command
                                           :requests :claim before after version)]
                               :emit false}})
        command-target [:gesso_commands (:xt/id (:record incorporation))]
        request-target [:requests id]
        targets [request-target command-target]]
    (xt/execute-tx (node) [[:put-docs :requests before]])
    {:before before
     :after after
     :binding binding
     :incorporation incorporation
     :request-target request-target
     :command-target command-target
     :targets targets}))

(defn- source [fixture]
  {:node (node)
   :incorporation (:incorporation fixture)
   :targets (:targets fixture)})

(defn- begin! [fixture]
  (observation/begin-observation! (source fixture)))

(defn- submit! [fixture]
  (xt/execute-tx (node)
                 (:tx-ops (tx/prepare {} (get-in fixture [:incorporation :plan])))))

(defn- complete! [handle tx-key]
  (observation/complete-observation! handle {:tx-key tx-key}))

(defn- thrown [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reason [f]
  (:reason (thrown f)))

(defn- physical-row [[table id]]
  (first (xt/q (node) [(str "SELECT * FROM " (name table) " WHERE _id = ?") id])))

(defn- ledger-row [id]
  (first (xt/q (node)
               ["SELECT _id, committed, system_time FROM xt.txs WHERE _id = ?"
                id])))

(defn- fresh-key [id]
  {:tx-id id :system-time (Instant/now)})

(deftest observation-contract-does-not-claim-refinement-test
  (let [scope observation/observation-scope]
    (is (= 1 observation/observation-version))
    (is (= :gesso.model.transition-observation/committed-persisted-step
           observation/observation-type))
    (is (= :bounded-controlled-xtdb-commit-observation (:evidence-class scope)))
    (doseq [fact [:phase-local-pinned-document-snapshots
                  :visible-committed-xtdb-ledger-record
                  :adjacent-and-terminal-transaction-ledger-window
                  :persisted-canonical-command-record-and-binding
                  :explicit-before-and-after-absence-or-presence]]
      (is (contains? (:collected scope) fact)))
    (doseq [assumption [:trusted-execute-tx-key-from-actual-submission
                        :declared-plan-is-the-actually-submitted-plan
                        :observed-read-targets-cover-all-application-dependencies
                        :single-controlled-ledger-observation-window]]
      (is (contains? (:assumed scope) assumption)))
    (doseq [unproved [:authenticated-transaction-payload-provenance
                      :transaction-internal-prestate-correspondence
                      :absence-of-untracked-physical-writers
                      :business-authorization-or-guard-completeness
                      :unbounded-concurrency-or-serializability
                      :abstract-to-physical-transition-refinement
                      :production-certification-or-execution-authority]]
      (is (contains? (:not-established scope) unproved)))))

(deftest genuine-commit-has-complete-real-persistence-witness-test
  (let [f (fixture!)
        handle (begin! f)
        tx-key (submit! f)
        result (complete! handle tx-key)]
    (is (= observation/observation-type (:type result)))
    (is (= observation/observation-version (:version result)))
    (is (= observation/observation-scope (:scope result)))
    (is (= :bounded-controlled-xtdb-commit-observation (:evidence-class result)))
    (is (= :committed (:physical-status result)))
    (is (= (:tx-id tx-key) (:physical-tx-id result)))
    (is (true? (:committed (ledger-row (:tx-id tx-key)))))
    (is (= (set (:targets f)) (:observed-targets result)))
    (is (= (set (:targets f)) (set (keys (:observed-before result)))))
    (is (= (set (:targets f)) (set (keys (:observed-after result)))))
    (is (= (:before f) (get-in result [:observed-before (:request-target f)])))
    (is (contains? (:observed-before result) (:command-target f)))
    (is (nil? (get-in result [:observed-before (:command-target f)])))
    (is (= (:after f) (get-in result [:observed-after (:request-target f)])))
    (is (= (get-in f [:incorporation :record])
           (get-in result [:observed-after (:command-target f)])))
    (is (= (get-in f [:incorporation :record]) (:persisted-command-record result)))
    (is (= (:binding f) (:command-binding result)))
    (is (= (:command-target f) (:command-target result)))
    (is (= (get-in f [:incorporation :plan]) (:declared-plan result)))
    (is (= :adjacent-and-terminal-in-observed-snapshots (:ledger-window result)))
    (is (= :exact-for-declared-incorporation (:plan-footprint result)))
    (is (= :caller-linked-not-independently-authenticated
           (:witness-provenance result)))
    (is (= :not-established (:abstract-transition-correspondence result)))
    (is (= :not-established (:untracked-writer-coverage result)))
    (is (= :none (:certification result)))
    (is (= :none (:authority result)))
    (is (= :unknown-or-consumed-capture (reason #(complete! handle tx-key))))))

(deftest before-phase-must-not-submit-any-transaction-test
  (let [f (fixture!)
        prior (->> (xt/q (node) "SELECT _id FROM xt.txs ORDER BY _id DESC LIMIT 1")
                   first :xt/id)
        handle (begin! f)
        after (->> (xt/q (node) "SELECT _id FROM xt.txs ORDER BY _id DESC LIMIT 1")
                   first :xt/id)]
    (is (= prior after))
    (is (= (:before f) (physical-row (:request-target f))))
    (is (nil? (physical-row (:command-target f))))
    (is (= :missing-definitive-commit-key (reason #(complete! handle nil))))))

(deftest command-created-before-begin-is-not-a-new-incorporation-test
  (let [f (fixture!)
        _ (submit! f)]
    (is (= :command-already-present (reason #(begin! f))))
    (is (= (:after f) (physical-row (:request-target f))))))

(deftest rejected-or-unknown-outcome-must-not-become-success-test
  (let [f (fixture!) handle (begin! f)]
    (is (= :invalid-source-shape
           (reason #(observation/complete-observation! handle
                                                       {:tx-key nil :status :committed}))))
    (is (= :unknown-or-consumed-capture (reason #(complete! handle (fresh-key 0)))))
    (is (= (:before f) (physical-row (:request-target f))))
    (is (nil? (physical-row (:command-target f))))))

(deftest nil-or-fabricated-commit-key-is-rejected-test
  (let [f (fixture!) handle (begin! f)]
    (is (= :missing-definitive-commit-key (reason #(complete! handle nil))))
    (is (= :unknown-or-consumed-capture
           (reason #(complete! handle {:tx-id 42 :system-time (Instant/now)}))))))

(deftest claimed-commit-with-no-transaction-fails-ledger-window-test
  (let [f (fixture!) handle (begin! f)]
    (is (= :non-isolated-or-mismatched-ledger-window
           (reason #(complete! handle (fresh-key 999999)))))
    (is (= (:before f) (physical-row (:request-target f))))))

(deftest unrelated-committed-transaction-does-not-prove-command-test
  (let [f (fixture!) handle (begin! f)
        unrelated (xt/execute-tx
                   (node) [[:put-docs :unrelated {:xt/id (str (random-uuid))}]])]
    (is (= :missing-or-mismatched-persisted-command
           (reason #(complete! handle unrelated))))
    (is (nil? (physical-row (:command-target f))))))

(deftest prior-unrelated-transaction-breaks-isolation-test
  (let [f (fixture!) handle (begin! f)
        _ (xt/execute-tx (node)
                         [[:put-docs :unrelated {:xt/id (str (random-uuid))}]])
        actual (submit! f)]
    (is (= :non-isolated-or-mismatched-ledger-window
           (reason #(complete! handle actual))))
    (is (= (:after f) (physical-row (:request-target f))))
    (is (= (get-in f [:incorporation :record])
           (physical-row (:command-target f))))))

(deftest subsequent-unrelated-transaction-breaks-terminal-window-test
  (let [f (fixture!) handle (begin! f) actual (submit! f)
        _ (xt/execute-tx (node)
                         [[:put-docs :unrelated {:xt/id (str (random-uuid))}]])]
    (is (= :non-isolated-or-mismatched-ledger-window
           (reason #(complete! handle actual))))))

(deftest key-for-another-commit-is-not-accepted-test
  (let [f (fixture!) handle (begin! f) actual (submit! f)
        later (xt/execute-tx (node)
                             [[:put-docs :unrelated {:xt/id (str (random-uuid))}]])]
    (is (not= (:tx-id actual) (:tx-id later)))
    (is (= :non-isolated-or-mismatched-ledger-window
           (reason #(complete! handle later))))))

(deftest forged-system-time-cannot-prove-committed-ledger-row-test
  (let [f (fixture!) handle (begin! f) actual (submit! f)
        forged (assoc actual :system-time (.plusSeconds ^Instant (:system-time actual) 20))]
    (is (= :missing-or-noncommitted-ledger-proof
           (reason #(complete! handle forged))))
    (is (= (:after f) (physical-row (:request-target f))))))

(deftest command-record-mismatch-is-rejected-even-with-committed-ledger-test
  (let [f (fixture!) handle (begin! f)
        bad-record (assoc-in (get-in f [:incorporation :record])
                             [:gesso.command/outcome :status] :invented)
        actual (xt/execute-tx (node)
                              [[:put-docs :requests (:after f)]
                               [:put-docs :gesso_commands bad-record]])]
    (is (= :missing-or-mismatched-persisted-command
           (reason #(complete! handle actual))))
    (is (= bad-record (physical-row (:command-target f))))))

(deftest command-only-physical-write-remains-an-observation-not-proof-test
  (let [f (fixture!) handle (begin! f)
        actual (xt/execute-tx (node)
                              [[:put-docs :gesso_commands
                                (get-in f [:incorporation :record])]])
        result (complete! handle actual)]
    ;; No comparison to the abstract transition was performed by v1298.
    ;; This deliberately inconsistent physical write is observable but does
    ;; not become a valid abstract transition or authorization certificate.
    (is (= :committed (:physical-status result)))
    (is (= (:before f) (get-in result [:observed-after (:request-target f)])))
    (is (not= (:after f) (get-in result [:observed-after (:request-target f)])))
    (is (= :not-established (:abstract-transition-correspondence result)))
    (is (= :caller-linked-not-independently-authenticated
           (:witness-provenance result)))
    (is (= :none (:certification result)))
    (is (= :none (:authority result)))))

(deftest no-callers-can-reuse-or-invent-capture-handles-test
  (let [f (fixture!) handle (begin! f)]
    (is (= :unknown-or-consumed-capture
           (reason #(complete! (Object.) (fresh-key 0)))))
    (is (= :unknown-or-consumed-capture
           (reason #(complete! nil (fresh-key 0)))))
    (is (= :missing-definitive-commit-key (reason #(complete! handle nil))))
    (is (= :unknown-or-consumed-capture (reason #(complete! handle nil))))))

(deftest request-must-have-exact-unannotated-shape-test
  (let [f (fixture!) valid (source f)]
    (doseq [bad [nil {} (dissoc valid :node) (assoc valid :proved? true)
                 (with-meta valid {:checked true})
                 (into (sorted-map) valid)]]
      (is (= :invalid-source-shape (reason #(observation/begin-observation! bad)))
          (str "Invalid source must fail: " (pr-str (if (map? bad) (keys bad) bad)))))
    (is (= :missing-node
           (reason #(observation/begin-observation! (assoc valid :node nil)))))))

(deftest declaration-must-have-full-target-footprint-test
  (let [f (fixture!) valid (source f) [request-target command-target] (:targets f)]
    (doseq [targets [[] [request-target] [command-target]
                     [request-target command-target request-target]
                     [request-target command-target [:requests (str (random-uuid))]]
                     (list request-target command-target)]]
      (is (some? (reason #(observation/begin-observation!
                            (assoc valid :targets targets))))))
    (is (= :invalid-targets
           (reason #(observation/begin-observation!
                     (assoc valid :targets (with-meta (:targets f) {:proved true}))))))
    (is (= :invalid-targets
           (reason #(observation/begin-observation!
                     (assoc valid :targets (reverse (:targets f)))))))
    (is (= :invalid-target
           (reason #(observation/begin-observation!
                     (assoc valid :targets [request-target command-target
                                            [:requests/x "bad"]])))))))

(deftest targets-must-not-smuggle-unchecked-table-identities-test
  (let [f (fixture!) valid (source f)]
    (doseq [candidate [[:evil/table "id"] [nil "id"] ["requests" "id"]
                       [:requests nil] [:requests] [:requests "id" :extra]
                       [:requests (str (random-uuid)) :bogus]]]
      (is (= :invalid-target
             (reason #(observation/begin-observation!
                       (assoc valid :targets (conj (:targets f) candidate)))))))))

(deftest incorporation-scope-and-record-must-remain-canonical-test
  (let [f (fixture!) valid (source f) original (:incorporation f)]
    (is (= :invalid-command-scope
           (reason #(observation/begin-observation!
                     (assoc valid :incorporation
                            (assoc original :scope :fake))))))
    (is (= :invalid-source-shape
           (reason #(observation/begin-observation!
                     (assoc valid :incorporation
                            (assoc original :proof :trusted))))))
    (is (some? (reason #(observation/begin-observation!
                          (assoc valid :incorporation
                                 (assoc-in original [:record :gesso.command/outcome]
                                           {:status :fabricated}))))))))

(deftest missing-or-invalid-commit-keys-cannot-sneak-through-test
  (let [f (fixture!)]
    (doseq [bad [nil {} {:tx-id 1} {:system-time (Instant/now)}
                 {:tx-id -1 :system-time (Instant/now)}
                 {:tx-id "1" :system-time (Instant/now)}
                 {:tx-id 1 :system-time "now"}
                 {:tx-id 1 :system-time (Instant/now) :committed true}
                 (with-meta (fresh-key 1) {:trusted true})]]
      (let [handle (begin! f)]
        (is (= :missing-definitive-commit-key
               (reason #(complete! handle bad))))
        (is (= :unknown-or-consumed-capture
               (reason #(complete! handle bad))))))))

(deftest malformed-completion-payload-cannot-add-proof-claims-test
  (let [f (fixture!)]
    (doseq [bad [{:tx-key (fresh-key 1) :proved? true}
                 {:tx-key (fresh-key 1) :certification :valid}
                 {:tx-key (fresh-key 1) :physical-status :committed}
                 (with-meta {:tx-key (fresh-key 1)} {:issued true})
                 (into (sorted-map) {:tx-key (fresh-key 1)})]]
      (let [handle (begin! f)]
        (is (= :invalid-source-shape
               (reason #(observation/complete-observation! handle bad))))
        (is (= :unknown-or-consumed-capture
               (reason #(observation/complete-observation! handle bad))))))))

(deftest rejected-physical-transaction-cannot-produce-success-witness-test
  (let [f (fixture!) handle (begin! f)
        req-id (:xt/id (:before f))
        invalid-plan (assoc (get-in f [:incorporation :plan])
                            :assertions [(tx/assert-document-field-count
                                          :requests req-id 999)])
        error (try
                (xt/execute-tx (node) (:tx-ops (tx/prepare {} invalid-plan)))
                nil
                (catch Throwable e e))
        last-entry (first (xt/q (node)
                                "SELECT _id, committed, system_time FROM xt.txs ORDER BY _id DESC LIMIT 1"))]
    (is (instance? Throwable error))
    (is (= :xtdb/assert-failed (:xtdb.error/code (ex-data error))))
    (is (false? (:committed last-entry)))
    (is (nil? (physical-row (:command-target f))))
    (is (= (:before f) (physical-row (:request-target f))))
    (let [fake-key {:tx-id (:xt/id last-entry)
                    :system-time (.toInstant ^ZonedDateTime (:system-time last-entry))}]
      (is (= :missing-or-noncommitted-ledger-proof
             (reason #(complete! handle fake-key))))
      (is (= :unknown-or-consumed-capture (reason #(complete! handle fake-key)))))))

(deftest exception-data-does-not-expose-domain-record-test
  (let [f (fixture!) special (assoc-in f [:incorporation :record :gesso.command/outcome]
                                      {:status :very-private-value})
        error (thrown #(observation/begin-observation! (source special)))]
    (is (= :gesso.model.transition-observation/invalid-observation (:error/type error)))
    (is (keyword? (:reason error)))
    (is (not (.contains (pr-str error) "very-private-value")))))

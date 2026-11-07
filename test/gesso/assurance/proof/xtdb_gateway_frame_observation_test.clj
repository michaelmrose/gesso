(ns gesso.assurance.proof.xtdb-gateway-frame-observation-test
  "v1536: Permanent adversarial checks of the opt-in v1535 gateway-bound
   external frame observer. Exercise real XTDB writes and pinned reads rather
   than manufacturing reports; challenge source admission, capability identity,
   outside writers, stale transactions, and repeated observations.

   These tests deliberately distinguish bounded agreement between two external
   snapshots from physical causality, transaction-internal prestate, verified
   value types, or complete writer control. Direct XTDB calls below are
   deliberately UNMEDIATED writes, never attributed to the restricted gateway."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.physical-domain-acquisition :as acquisition]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.assurance.proof.xtdb-gateway-frame-observation :as observed]
            [gesso.assurance.proof.xtdb-physical-frame-preservation :as frame]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(def ^:private table :gesso_v1536_frame_observation_probe)
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private typed-construction
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types {:request/status {:type :enum :name :request/status
                                  :values #{:open :claimed :done}}
                 :request/owner {:type :keyword :optional? true}
                 :request/version {:type :integer}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/actor {:type :keyword}}
   :guard {:readers #{:request/status}
           :expression [:= [:read :request/status] [:literal :open]]}
   :reads #{:request/status :input/actor}
   :writes #{:request/status :request/owner}
   :replacements {:request/status [:literal :claimed]
                  :request/owner [:read :input/actor]}
   :frame #{:request/version :request/note}})
(def ^:private obligation
  {:construction typed-construction
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status]
                                    [:literal :open]]}
   :invariants {:request/status-valid
                {:readers #{:request/status}
                 :expression [:in [:read :request/status]
                              [:literal #{:open :claimed :done}]]}
                :request/version-valid
                {:readers #{:request/version}
                 :expression [:>= [:read :request/version] [:literal 0]]}}})

(defonce ^:private !node (atom nil))
(defn- node []
  (or @!node (throw (ex-info "Embedded XTDB fixture inactive" {}))))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(defn- fresh-id [] (str "v1536-" (random-uuid)))
(defn- document [id]
  {:xt/id id :revision 0 :created-at "v1536-private-created"
   :updated-at "v1536-private-before" :value "v1536-private-note"})
(defn- source [before]
  (let [id (:xt/id before)
        after (-> before
                  (assoc :revision 1 :updated-at "v1536-private-after"))]
    {:obligation-source obligation
     :protected-domain-source
     {:plan {:commands [(command/update-command table :claim
                                                before after version-keys)]
             :emit false}
      :protected-documents [{:target [table id] :fields :all}]}
     :field-locations
     {:request/status {:target [table id] :physical-field :revision}
      :request/owner {:target [table id] :physical-field :updated-at}
      :request/version {:target [table id] :physical-field :created-at}
      :request/note {:target [table id] :physical-field :value}}}))
(defn- raw-read [id]
  (first (xt/q (node)
               ["SELECT * FROM gesso_v1536_frame_observation_probe WHERE _id = ?"
                id])))
(defn- outside-put! [document]
  (xt/execute-tx (node) [[:put-docs table document]]))
(defn- initial! []
  (let [before (document (fresh-id))]
    (is (instance? TransactionKey (outside-put! before)))
    before))
(defn- prepared []
  (let [before (initial!)
        s (source before)
        gate (observed/prepare-observed-one-shot! (node) s)]
    {:before before :source s :gate gate}))
(defn- submitted []
  (let [p (prepared)
        result ((:submit! (:gate p)))]
    (assoc p :result result)))
(defn- observe [{:keys [gate result]}]
  ((:observe! gate) result))
(defn- rejection [thunk]
  (try
    (thunk)
    ::accepted
    (catch clojure.lang.ExceptionInfo e
      (:reason (ex-data e)))))
(defn- rejects [reason thunk]
  (is (= reason (rejection thunk))))
(defn- no-second-submission [gate]
  (rejects :one-shot-already-attempted (:submit! gate)))

(def ^:private explicit-nonclaims
  {:external-observation-order :not-authenticated
   :transaction-internal-prestate :not-authenticated
   :actual-transaction-caused-observed-difference :not-established
   :physical-frame-preservation :not-established
   :frame-outside-enumerated-domain :not-established
   :physical-value-type-correspondence :not-established
   :physical-poststate-causality :not-established
   :caller-node-authenticity :not-established
   :installed-host-adoption :not-established
   :outside-gateway-writer-closure :not-established
   :complete-deployment-writer-coverage :not-established
   :principal-authorization :not-established
   :universal-physical-refinement :not-established
   :proof-status :bounded-external-frame-observation-only
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})
(defn- nonclaims! [report]
  (doseq [[claim value] explicit-nonclaims]
    (is (= value (get report claim))
        (str "Overstated external-frame guarantee: " claim))))
(defn- redacted! [report]
  (is (map? report))
  (is (not (str/includes? (pr-str report) "v1536-private-")))
  (is (not-any? #(contains? report %)
                [:source :projection-source :plan :tx-ops :tx-key
                 :pre-acquisition :post-acquisition :observed-rows
                 :snapshot-token :read-node :node])))

(deftest genuine-gateway-return-produces-bounded-unchanged-frame
  (let [{:keys [before gate result] :as s} (submitted)
        report (observe s)]
    (is (instance? TransactionKey (:tx-key result)))
    (is (= observed/observation-type (:type report)))
    (is (= observed/observation-version (:version report)))
    (is (= :bounded-supplied-frame-agrees (:status report)))
    (is (true? (:finite-observed-frame-agrees? report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (= 1 (:owned-execute-tx-call-entries report)))
    (is (= 1 (:declared-write-count report)))
    (is (= 2 (:declared-typed-frame-field-count report)))
    (is (= [] (:changed-typed-frame-field-indices report)))
    (is (= (:value before) (:value (raw-read (:xt/id before)))))
    (is (= 1 (:revision (raw-read (:xt/id before)))))
    (nonclaims! report)
    (redacted! report)
    (no-second-submission gate)))

(deftest summary-before-and-after-submission-remains-redacted
  (let [{:keys [gate]} (prepared)
        early ((:summary! gate))
        _ (is (= :prepared (:status early)))
        _ (is (= 0 (:owned-execute-tx-call-entries early)))
        result ((:submit! gate))
        late ((:summary! gate))]
    (is (instance? TransactionKey (:tx-key result)))
    (is (= :returned-transaction-key (:status late)))
    (is (= 1 (:owned-execute-tx-call-entries late)))
    (is (= :bounded-external-frame-observation-only (:proof-status late)))
    (is (= :not-issued (:certificate late)))
    (is (= :not-established (:outside-gateway-writer-closure late)))
    (redacted! early)
    (redacted! late)
    (no-second-submission gate)))

(deftest no-node-and-malformed-projection-refuse-preparation
  (let [before (initial!)
        s (source before)]
    (rejects :missing-observation-node
             #(observed/prepare-observed-one-shot! nil s))
    (doseq [bad [nil {} (dissoc s :field-locations)
                 (assoc s :additional-host-claim true)
                 (with-meta s {:authenticated true})]]
      (rejects :invalid-physical-projection-source
               #(observed/prepare-observed-one-shot! (node) bad)))))

(deftest missing-mapped-prestate-refuses-to-expose-submission-capability
  (let [missing (document (fresh-id))]
    (rejects :external-prestate-unavailable
             #(observed/prepare-observed-one-shot! (node) (source missing)))
    (is (nil? (raw-read (:xt/id missing))))))

(deftest structurally-valid-but-nonexistent-mapped-field-refuses-preparation
  (let [before (initial!)
        s (assoc-in (source before)
                    [:field-locations :request/note :physical-field]
                    :not-a-real-physical-field)]
    ;; Admission of a *declaration* is not authentication of mapped values.
    (rejects :external-prestate-unavailable
             #(observed/prepare-observed-one-shot! (node) s))))

(deftest copied-mutated-and-caller-invented-return-cannot-observe
  (let [{:keys [gate result] :as s} (submitted)]
    (doseq [fake [(into {} result)
                  (assoc result :tx-key nil)
                  (assoc result :certificate :issued)
                  {:tx-key (:tx-key result)}
                  nil]]
      (rejects :not-this-gateway-local-return
               #((:observe! gate) fake)))
    (is (true? (:finite-observed-frame-agrees? (observe s))))
    (no-second-submission gate)))

(deftest foreign-gateway-result-does-not-belong-to-this-observer
  (let [a (submitted) b (submitted)]
    (rejects :not-this-gateway-local-return
             #((:observe! (:gate a)) (:result b)))
    (rejects :not-this-gateway-local-return
             #((:observe! (:gate b)) (:result a)))
    (is (true? (:finite-observed-frame-agrees? (observe a))))
    (is (true? (:finite-observed-frame-agrees? (observe b))))))

(deftest no-return-before-submission-and-one-shot-is-preserved
  (let [{:keys [gate]} (prepared)]
    (rejects :not-this-gateway-local-return
             #((:observe! gate) {}))
    (is (= :prepared (:status ((:summary! gate)))))
    (let [result ((:submit! gate))]
      (is (instance? TransactionKey (:tx-key result)))
      (is (true? (:finite-observed-frame-agrees?
                  ((:observe! gate) result))))
      (no-second-submission gate))))

(deftest outside-writer-after-submission-changes-a-typed-frame-field
  (let [{:keys [before gate] :as s} (submitted)
        outsider (assoc before :value "outside-writer-after")]
    (outside-put! outsider)
    (let [report (observe s)]
      (is (= outsider (raw-read (:xt/id before))))
      (is (= :bounded-supplied-frame-diverges (:status report)))
      (is (= [0] (:changed-typed-frame-field-indices report)))
      (is (false? (:finite-observed-frame-agrees? report)))
      (is (true? (:observed-ledger-joined-poststate? report)))
      (nonclaims! report)
      (no-second-submission gate))))

(deftest different-typed-frame-field-divergence-uses-stable-index
  (let [{:keys [before] :as s} (submitted)]
    (outside-put! (assoc before :created-at "outside-created-at"))
    (let [report (observe s)]
      (is (= :bounded-supplied-frame-diverges (:status report)))
      ;; Frame fields are sorted by pr-str: request/note, request/version.
      (is (= [1] (:changed-typed-frame-field-indices report)))
      (nonclaims! report))))

(deftest outside-writer-after-submission-can-keep-same-framed-values
  (let [{:keys [before] :as s} (submitted)]
    ;; Replacing unrelated business fields can leave both typed frame fields
    ;; unchanged. Agreement cannot exclude this independent writer.
    (outside-put! (assoc before :updated-at "outside-business-field"))
    (let [report (observe s)]
      (is (true? (:finite-observed-frame-agrees? report)))
      (is (= :not-established (:outside-gateway-writer-closure report)))
      (is (= "outside-business-field"
             (:updated-at (raw-read (:xt/id before)))))
      (nonclaims! report))))

(deftest writer-before-submission-can-be-erased-by-whole-document-write
  (let [{:keys [before gate]} (prepared)]
    ;; A writer changes the framed value AFTER the opt-in external pre-read.
    ;; The gateway's planned whole-document put restores the old value, and
    ;; external before/after observation looks identical in the typed frame.
    (outside-put! (assoc before :value "outside-between-pre-and-submit"))
    (is (= "outside-between-pre-and-submit"
           (:value (raw-read (:xt/id before)))))
    (let [result ((:submit! gate))
          report ((:observe! gate) result)]
      (is (= :bounded-supplied-frame-agrees (:status report)))
      (is (true? (:finite-observed-frame-agrees? report)))
      (is (= (:value before) (:value (raw-read (:xt/id before)))))
      (is (= :not-authenticated (:transaction-internal-prestate report)))
      (nonclaims! report))))

(deftest stale-guarded-command-cannot-be-observed-as-a-commit
  (let [{:keys [before gate]} (prepared)
        _ (outside-put! (assoc before :revision 11))
        outcome (try ((:submit! gate))
                     ::unexpected-return
                     (catch Exception e e))]
    ;; XTDB execute-tx throws a conflict when the stale SQL ASSERT fails.
    ;; A one-shot invocation remains consumed, and the observer has no genuine
    ;; TransactionKey return to authenticate. Do not manufacture a ledger or
    ;; frame report from the physical exception.
    (is (instance? Throwable outcome))
    (is (re-find #"Assert failed" (.getMessage outcome)))
    (is (= 11 (:revision (raw-read (:xt/id before)))))
    (is (= :threw-outcome-unknown (:status ((:summary! gate)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! gate)))))
    (rejects :not-this-gateway-local-return
             #((:observe! gate) {:tx-key nil}))
    (no-second-submission gate)))

(deftest outside-writer-delete-makes-poststate-comparison-unavailable
  (let [{:keys [before] :as s} (submitted)]
    (xt/execute-tx (node) [[:delete-docs table (:xt/id before)]])
    (is (nil? (raw-read (:xt/id before))))
    (let [report (observe s)]
      (is (= :external-comparison-unavailable (:status report)))
      (is (= :poststate-acquisition-unavailable (:poststate-status report)))
      (is (false? (:finite-observed-frame-agrees? report)))
      (nonclaims! report))))

(deftest outside-writer-adds-an-unmapped-physical-field
  (let [{:keys [before] :as s} (submitted)]
    ;; :fields :all declares full-document protection: a new extra key makes
    ;; the bounded acquisition unreadable rather than silently ignoring it.
    (outside-put! (assoc before :unmapped "another-writer"))
    (let [report (observe s)]
      (is (= :external-comparison-unavailable (:status report)))
      (is (false? (:finite-observed-frame-agrees? report)))
      (nonclaims! report))))

(deftest repeated-observations-are-new-reads-never-retries
  (let [{:keys [before gate] :as s} (submitted)
        first-report (observe s)]
    (is (true? (:finite-observed-frame-agrees? first-report)))
    (outside-put! (assoc before :value "outside-after-first-read"))
    (let [second-report (observe s)]
      (is (= :bounded-supplied-frame-diverges (:status second-report)))
      (is (= (:process-local-attempt-id first-report)
             (:process-local-attempt-id second-report)))
      (is (= 1 (:owned-execute-tx-call-entries second-report)))
      (is (false? (:finite-observed-frame-agrees? second-report)))
      (nonclaims! first-report)
      (nonclaims! second-report)
      (no-second-submission gate))))

(deftest external-write-to-unrelated-document-does-not-prove-writer-closure
  (let [{:keys [before gate] :as s} (submitted)
        outsider (document (fresh-id))]
    (outside-put! outsider)
    (let [report (observe s)]
      (is (true? (:finite-observed-frame-agrees? report)))
      (is (= outsider (raw-read (:xt/id outsider))))
      (is (= :not-established (:complete-deployment-writer-coverage report)))
      (no-second-submission gate))))

(deftest tampering-with-projection-or-external-read-seams-is-refused
  (let [{:keys [gate result] :as s} (submitted)]
    (with-redefs [projection/describe-physical-domain-projection!
                  (fn [& _] {:proof-status :proved})]
      (rejects :substituted-physical-observation-seam
               #(observe s)))
    (with-redefs [acquisition/acquire-physical-domain!
                  (fn [& _] {:status :forged})]
      (rejects :substituted-physical-observation-seam
               #(observe s)))
    (is (true? (:finite-observed-frame-agrees?
                ((:observe! gate) result))))))

(deftest tampering-with-poststate-or-frame-checkers-is-refused
  (let [s (submitted)]
    (with-redefs [poststate/acquire-poststate-basis!
                  (fn [& _] {:report {:status :forged} :handle :forged})]
      (rejects :substituted-physical-observation-seam
               #(observe s)))
    (with-redefs [frame/compare-external-physical-frame!
                  (fn [& _] {:status :bounded-supplied-frame-agrees})]
      (rejects :substituted-physical-observation-seam
               #(observe s)))
    (with-redefs [frame/redacted-summary
                  (fn [& _] {:finite-supplied-frame-agrees? true})]
      (rejects :substituted-physical-observation-seam
               #(observe s)))
    (is (true? (:finite-observed-frame-agrees? (observe s))))))

(deftest abandoned-preflight-attempt-is-not-execution-evidence
  (let [{:keys [before gate]} (prepared)]
    (with-redefs [xt/execute-tx (fn [& _]
                                  (throw (ex-info "tampered call site" {})))]
      (rejects :substituted-gateway-or-upstream-seam (:submit! gate)))
    (is (= :preflight-rejected (:status ((:summary! gate)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! gate)))))
    (is (= before (raw-read (:xt/id before))))
    (rejects :not-this-gateway-local-return
             #((:observe! gate) {}))
    (no-second-submission gate)))

(deftest closed-node-after-genuine-return-cannot-fabricate-poststate
  ;; A local node can be closed before :observe!, independently of the genuine
  ;; gateway submission. No later node may be silently substituted.
  (let [id (fresh-id)
        before (document id)
        report-or-gate
        (with-open [n (xtn/start-node {})]
          (xt/execute-tx n [[:put-docs table before]])
          (let [gate (observed/prepare-observed-one-shot! n (source before))
                result ((:submit! gate))]
            {:gate gate :result result}))
        report ((:observe! (:gate report-or-gate)) (:result report-or-gate))]
    (is (= :external-comparison-unavailable (:status report)))
    (is (false? (:finite-observed-frame-agrees? report)))
    (nonclaims! report)
    (no-second-submission (:gate report-or-gate))))

(deftest observation-result-is-a-copyable-diagnostic-not-an-authority
  (let [{:keys [gate] :as s} (submitted)
        original (observe s)
        spoofed (assoc original :certificate :issued :authority :all)]
    (is (= :issued (:certificate spoofed)))
    (is (= :not-issued (:certificate (observe s))))
    (is (= :none (:authority (observe s))))
    (is (= :not-established (:physical-poststate-causality original)))
    (no-second-submission gate)))

(deftest concurrent-callers-cannot-produce-two-gateway-submissions
  (let [{:keys [gate]} (prepared)
        start (promise)
        calls (doall (for [_ (range 6)]
                       (future
                         @start
                         (try
                           ((:submit! gate))
                           (catch clojure.lang.ExceptionInfo e
                             (:reason (ex-data e)))))))]
    (deliver start true)
    (let [outcomes (mapv #(deref % 30000 ::timed-out) calls)
          returned (filter #(and (map? %) (instance? TransactionKey (:tx-key %)))
                           outcomes)]
      (is (not-any? #{::timed-out} outcomes))
      (is (= 1 (count returned)))
      (is (= 5 (count (filter #{:one-shot-already-attempted} outcomes))))
      (is (= :bounded-supplied-frame-agrees
             (:status ((:observe! gate) (first returned)))))
      (is (= 1 (:owned-execute-tx-call-entries ((:summary! gate)))))
      (no-second-submission gate))))

(deftest concurrent-independent-observations-still-have-no-retry-authority
  (let [{:keys [gate result]} (submitted)
        reports (mapv deref
                      (doall (repeatedly 3
                                         #(future ((:observe! gate) result)))))]
    (is (= 3 (count reports)))
    (is (every? #(= :bounded-supplied-frame-agrees (:status %)) reports))
    (is (= 1 (count (set (map :process-local-attempt-id reports)))))
    (is (every? #(= 1 (:owned-execute-tx-call-entries %)) reports))
    (doseq [report reports]
      (nonclaims! report))
    (no-second-submission gate)))

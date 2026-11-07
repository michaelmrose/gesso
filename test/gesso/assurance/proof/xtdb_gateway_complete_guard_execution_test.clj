(ns gesso.assurance.proof.xtdb-gateway-complete-guard-execution-test
  "v1538: Permanent adversarial tests for the opt-in v1537 complete top-level
   compare-and-set XTDB gateway. The tested contract is limited to one
   admitted whole-document UPDATE and supplied, non-nil, scalar top-level
   fields. Actual XTDB assertions protect the supplied field values and the
   canonical field count at the execution boundary; they neither authenticate
   the caller's prior read nor prove an uninterrupted transaction history.

   External writes made directly through xtdb.api/execute-tx in this file are
   deliberate adversarial bypasses, not gateway-owned submissions. Passing
   these finite tests is NOT proof of deployment writer closure, authorization,
   XTDB's general isolation theorem, causality, or universal refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as strict]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.time Instant]
           [java.util UUID]
           [xtdb.api TransactionKey]))

(def ^:private table :gesso_v1538_complete_guard_probe)
(def ^:private version-keys
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})
(def ^:private secret "v1538-secret-private-value")

(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(defn- node []
  (or @!node (throw (ex-info "No embedded XTDB fixture" {}))))
(defn- fresh-id [] (str "gesso-v1538-" (random-uuid)))
(defn- before []
  {:xt/id (fresh-id)
   :revision 0
   :created-at "v1538-created"
   :updated-at "v1538-before"
   :owner "open"
   :note secret})
(defn- after [b]
  (-> b (assoc :revision (inc (:revision b))
               :updated-at "v1538-after"
               :owner "claimed")))
(defn- source [b]
  {:plan {:commands [(command/update-command
                     table :claim b (after b) version-keys)]
          :emit false}
   :protected-documents [{:target [table (:xt/id b)] :fields :all}]})
(defn- external-put! [document]
  (xt/execute-tx (node) [[:put-docs table document]]))
(defn- current [id]
  (first (xt/q (node)
               ["SELECT * FROM gesso_v1538_complete_guard_probe WHERE _id = ?"
                id])))
(defn- prepare! [b]
  (strict/prepare-complete-guard-one-shot! (node) (source b)))
(defn- staged! []
  (let [b (before)]
    (is (instance? TransactionKey (external-put! b)))
    {:before b :gate (prepare! b)}))
(defn- failure-code [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e
         (or (:reason (ex-data e)) (:xtdb.error/code (ex-data e))
             (:error/type (ex-data e)) :unknown-exception-info))
       (catch Throwable e
         ;; xtdb.error.Conflict is not a Clojure ExceptionInfo, but has
         ;; structured :xtdb.error/code data. Do not mask other failures.
         (if-let [code (:xtdb.error/code (ex-data e))]
           code
           (throw e)))))
(defn- rejects! [reason f]
  (is (= reason (failure-code f)) (str "Expected rejection: " reason)))
(defn- strict-source-rejects! [reason s]
  (rejects! reason #(strict/prepare-complete-guard-one-shot! (node) s)))
(defn- no-retry! [gate]
  (rejects! :one-shot-already-attempted (:submit! gate)))
(defn- redacted! [summary]
  (is (map? summary))
  (is (not (str/includes? (pr-str summary) secret)))
  (is (not (str/includes? (pr-str summary) "gesso-v1538-")))
  (is (not-any? #(contains? summary %)
                [:plan :source :node :tx-ops :before :after :tx-key
                 :protected-documents :snapshot-token :physical-state])))
(def ^:private explicit-nonclaims
  {:source-prestate-authenticity :caller-supplied-unattested
   :transaction-internal-read-basis :not-authenticated
   :backend-assert-semantics :trusted-bounded-premise
   :nested-and-external-fields :not-established
   :aba-history-detection :not-established
   :physical-poststate-causality :not-established
   :complete-deployment-writer-coverage :not-established
   :principal-authorization :not-established
   :universal-physical-refinement :not-established
   :proof-status :guarded-submission-only
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})
(defn- nonclaims! [report]
  (doseq [[field expected] explicit-nonclaims]
    (is (= expected (get report field))
        (str "Unsupported guarantee was promoted: " field)))
  (redacted! report))

(deftest supported-update-submits-through-existing-gateway
  (let [{:keys [before gate]} (staged!)
        early ((:summary! gate))
        returned ((:submit! gate))
        late ((:summary! gate))]
    (is (= strict/guarded-type (:type gate)))
    (is (= strict/guarded-version (:version gate)))
    (is (= :prepared (:status early)))
    (is (= 0 (:owned-execute-tx-call-entries early)))
    (is (= :returned-transaction-key (:status late)))
    (is (= :one-update-complete-top-level-cas (:scope late)))
    (is (= 5 (:compared-top-level-field-count late)))
    (is (= 3 (:added-snapshot-comparison-count late)))
    (is (= :canonical-prewrite (:field-count-assertion late)))
    (is (true? (:all-source-observed-fields-guarded? late)))
    (is (= 1 (:owned-execute-tx-call-entries late)))
    (is (instance? TransactionKey (:tx-key returned)))
    (is (= (after before) (current (:xt/id before))))
    (is (= :returned-transaction-key
           (:status (gateway/require-local-attempt! (:gateway gate)))))
    (is (string? (:process-local-attempt-id
                  (gateway/require-local-return! (:gateway gate) returned))))
    (nonclaims! early)
    (nonclaims! late)
    (no-retry! gate)))

(deftest original-unguarded-field-can-be-clobbered-by-ordinary-gateway
  (let [b (before)
        _ (external-put! b)
        ordinary (gateway/prepare-one-shot! (node) (source b))]
    (external-put! (assoc b :note "outside-wrote-this"))
    (is (instance? TransactionKey (:tx-key ((:submit! ordinary)))))
    (is (= secret (:note (current (:xt/id b)))))
    (is (= (after b) (current (:xt/id b))))
    (is (= :returned-transaction-key (:status ((:summary! ordinary)))))))

(deftest strict-guard-rejects-intervening-note-change
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :note "changed-by-outside")]
    (external-put! outside)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (is (= :threw-outcome-unknown (:status ((:summary! gate)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! gate)))))
    (no-retry! gate)))

(deftest strict-guard-rejects-intervening-owner-change
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :owner "other-helper")]
    (external-put! outside)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest strict-guard-rejects-intervening-created-at-change
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :created-at "changed-created-at")]
    (external-put! outside)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest count-guard-rejects-added-field
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :new-field "outside-new-value")]
    (external-put! outside)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest count-guard-rejects-removed-field
  (let [{:keys [before gate]} (staged!)
        outside (dissoc before :note)]
    (external-put! outside)
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest changed-field-with-unchanged-count-is-not-overlooked
  (let [{:keys [before gate]} (staged!)
        outside (-> before (dissoc :note)
                    (assoc :replacement-note "still-six-fields"))]
    (external-put! outside)
    (is (= (count before) (count outside)))
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest field-count-guard-protects-late-addition-with-unmodified-versions
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :late-field 7)]
    (external-put! outside)
    (is (= (:revision before) (:revision (current (:xt/id before)))))
    (is (= (:updated-at before) (:updated-at (current (:xt/id before)))))
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (no-retry! gate)))

(deftest aba-restoration-remains-undetected
  (let [{:keys [before gate]} (staged!)]
    (external-put! (assoc before :note "temporarily-modified"))
    (is (= "temporarily-modified" (:note (current (:xt/id before)))))
    (external-put! before)
    (is (= before (current (:xt/id before))))
    (let [returned ((:submit! gate))]
      (is (instance? TransactionKey (:tx-key returned)))
      (is (= (after before) (current (:xt/id before))))
      (is (= :not-established (:aba-history-detection ((:summary! gate)))))
      (nonclaims! ((:summary! gate)))
      (no-retry! gate))))

(deftest competing-concurrent-strict-guards-allow-only-one-claim
  (let [b (before)
        _ (external-put! b)
        first-gate (prepare! b)
        second-gate (prepare! b)
        release (promise)
        invoke (fn [gate]
                 (future
                   @release
                   (try
                     {:outcome :returned
                      :result ((:submit! gate))}
                     (catch Throwable e
                       {:outcome :threw
                        :error-code (:xtdb.error/code (ex-data e))}))))
        first-result (invoke first-gate)
        second-result (invoke second-gate)]
    (deliver release true)
    (let [results [(deref first-result 30000 ::timeout)
                   (deref second-result 30000 ::timeout)]
          outcomes (frequencies (map :outcome results))]
      (is (not-any? #{::timeout} results))
      (is (= {:returned 1 :threw 1} outcomes))
      (is (every? #(if (= :returned (:outcome %))
                     (instance? TransactionKey (get-in % [:result :tx-key]))
                     (= :xtdb/assert-failed (:error-code %)))
                  results))
      (is (= (after b) (current (:xt/id b))))
      (is (= #{:returned-transaction-key :threw-outcome-unknown}
             (set (map (fn [g] (:status ((:summary! g))))
                       [first-gate second-gate]))))
      (no-retry! first-gate)
      (no-retry! second-gate))))

(deftest fabricated-before-does-not-establish-authoritative-prestate
  (let [real (before)
        _ (external-put! real)
        fabricated (assoc real :note "fabricated-old-value")
        gate (prepare! fabricated)]
    ;; Source admission is syntactic; only XTDB's actual ASSERT constrains
    ;; the current physical value, not the authenticity of caller's read.
    (is (= :caller-supplied-unattested
           (:source-prestate-authenticity ((:summary! gate)))))
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= real (current (:xt/id real))))
    (no-retry! gate)))

(deftest root-source-and-extra-keys-reject-without-submission
  (let [b (before) s (source b)]
    (doseq [bad [nil {} [] (assoc s :extra true)
                 (dissoc s :protected-documents)
                 (with-meta s {:verified true})]]
      (strict-source-rejects! :invalid-gateway-source bad))))

(deftest plan-must-not-silently-ignore-any-extra-effects
  (let [b (before) s (source b)]
    (doseq [bad [(assoc-in s [:plan :guards] [])
                 (assoc-in s [:plan :assertions] [])
                 (assoc-in s [:plan :changes] [])
                 (assoc-in s [:plan :entry] :ignored)
                 (assoc-in s [:plan :tx-options] {})
                 (assoc-in s [:plan :entry-fn] (fn [] nil))
                 (assoc-in s [:plan :emit] true)
                 (assoc-in s [:plan :emit] :async)
                 (with-meta s {:asserted true})]]
      (strict-source-rejects!
       (if (meta bad) :invalid-gateway-source :unsupported-plan-scope)
       bad))))

(deftest plan-shape-and-command-count-must-be-exact
  (let [b (before) s (source b)]
    (doseq [bad [(assoc s :plan [])
                 (assoc-in s [:plan :commands] [])
                 (update-in s [:plan :commands] conj
                            (first (get-in s [:plan :commands])))
                 (assoc-in s [:plan :commands] nil)
                 (assoc-in s [:plan :commands] '())
                 (with-meta (get s :plan) {:claimed true})]]
      (let [candidate (if (and (map? bad) (contains? bad :plan))
                        bad (assoc s :plan bad))]
        (strict-source-rejects! :unsupported-plan-scope candidate)))))

(deftest protected-document-declarations-are-mandatory-and-exact
  (let [b (before) s (source b)]
    (doseq [bad [(assoc s :protected-documents [])
                 (assoc s :protected-documents nil)
                 (assoc s :protected-documents [{:target [table (:xt/id b)]
                                                 :fields :all}
                                                {:target [table (:xt/id b)]
                                                 :fields :all}])
                 (assoc s :protected-documents '())]]
      (strict-source-rejects! :unsupported-plan-scope bad))
    (doseq [bad [(assoc-in s [:protected-documents 0 :fields] #{:note})
                 (assoc-in s [:protected-documents 0 :fields] [:note])
                 (assoc-in s [:protected-documents 0 :target] [table (fresh-id)])
                 (assoc-in s [:protected-documents 0 :target] [:other/table (:xt/id b)])
                 (assoc-in s [:protected-documents 0 :claim] true)
                 (assoc s :protected-documents [{:target [table (:xt/id b)]}])]]
      (strict-source-rejects! :unsupported-single-document-update bad))))

(deftest before-and-after-must-be-complete-plain-versioned-documents
  (let [b (before) s (source b)]
    (doseq [bad [(assoc-in s [:plan :commands 0 :model/before :note] nil)
                 (assoc-in s [:plan :commands 0 :model/after :note] nil)
                 (update-in s [:plan :commands 0 :model/after] dissoc :note)
                 (update-in s [:plan :commands 0 :model/before] dissoc :note)
                 (assoc-in s [:plan :commands 0 :model/after :xt/id] (fresh-id))
                 (assoc-in s [:plan :commands 0 :model/before :owner] {})
                 (assoc-in s [:plan :commands 0 :model/after :owner] [:invalid])
                 (assoc-in s [:plan :commands 0 :model/before :owner]
                           (java.util.ArrayList.))]]
      (strict-source-rejects! :unsupported-single-document-update bad))))

(deftest rejected-value-types-include-mutable-nested-and-nil
  (let [b (before) s (source b)]
    (doseq [unsupported [nil {:nested 1} [:nested] #{:nested}
                         (byte-array [1 2 3]) (java.util.Date.)
                         (java.util.ArrayList.) (Object.)]]
      (strict-source-rejects!
       :unsupported-single-document-update
       (-> s
           (assoc-in [:plan :commands 0 :model/before :note] unsupported)
           (assoc-in [:plan :commands 0 :model/after :note] unsupported))))))

(deftest before-and-after-must-have-identical-keys-and-limited-size
  (let [b (before) s (source b)
        too-wide (into b (map (fn [i] [(keyword (str "other-" i)) i])
                              (range 70)))
        wide-source (source too-wide)]
    (strict-source-rejects! :unsupported-single-document-update wide-source)
    (strict-source-rejects! :unsupported-single-document-update
                            (update-in s [:plan :commands 0 :model/after]
                                       assoc :new-thing 11))
    (strict-source-rejects! :unsupported-single-document-update
                            (update-in s [:plan :commands 0 :model/before]
                                       assoc "string-key" "value"))))

(deftest original-version-checks-must-match-the-supplied-before-map
  (let [b (before) s (source b)]
    (doseq [bad [(assoc-in s [:plan :commands 0 :model/expected
                              :model/checks 0 1] -999)
                 (assoc-in s [:plan :commands 0 :model/expected
                              :model/checks] [[:revision 999]])
                 (assoc-in s [:plan :commands 0 :model/expected
                              :model/checks] [[:unrecognized :missing]])]]
      (strict-source-rejects! :version-check-not-bound-to-before bad))
    ;; Empty checks invalidate the canonical update shape before the
    ;; strengthened gateway attempts to bind them to the supplied before.
    (strict-source-rejects! :unsupported-single-document-update
                            (assoc-in s [:plan :commands 0 :model/expected
                                         :model/checks] []))))

(deftest metadata-on-critical-structures-is-not-trusted
  (let [b (before) s (source b)]
    (doseq [bad [(update s :plan with-meta {:claimed true})
                 (update-in s [:plan :commands] with-meta {:claimed true})
                 (update-in s [:plan :commands 0 :model/before]
                            with-meta {:claimed true})
                 (update-in s [:plan :commands 0 :model/after]
                            with-meta {:claimed true})
                 (update-in s [:protected-documents 0]
                            with-meta {:claimed true})]]
      (is (not= :no-error (failure-code
                           #(strict/prepare-complete-guard-one-shot!
                              (node) bad)))))))

(deftest ordinary-source-requires-single-update-not-create
  (let [b (before) s (source b)
        create {:model/entity-type table :model/operation :create
                :model/id (:xt/id b) :model/after b}]
    (strict-source-rejects! :unsupported-single-document-update
                            (assoc-in s [:plan :commands 0] create))
    (strict-source-rejects! :unsupported-single-document-update
                            (assoc-in s [:plan :commands 0]
                                      (assoc (first (get-in s [:plan :commands]))
                                             :model/operation :create)))))

(deftest substituted-namespace-seams-reject-before-constructing-gateway
  (let [b (before) s (source b)
        illegal (fn [& _] (throw (ex-info "forged seam" {})))]
    (with-redefs [command/update? illegal]
      (strict-source-rejects! :substituted-preparation-seam s))
    (with-redefs [tx/assert-document-field-count illegal]
      (strict-source-rejects! :substituted-preparation-seam s))
    (with-redefs [tx/prepare illegal]
      (strict-source-rejects! :substituted-preparation-seam s))
    (with-redefs [gateway/prepare-one-shot! illegal]
      (strict-source-rejects! :substituted-preparation-seam s))))

(deftest complete-guard-summary-is-conservative-and-redacted
  (let [{:keys [before gate]} (staged!)
        initial ((:summary! gate))]
    (nonclaims! initial)
    (is (= 0 (:owned-execute-tx-call-entries initial)))
    (let [ret ((:submit! gate))
          later ((:summary! gate))]
      (is (instance? TransactionKey (:tx-key ret)))
      (nonclaims! later)
      (is (= :returned-transaction-key (:status later)))
      (is (= 1 (:owned-execute-tx-call-entries later)))
      (is (= (after before) (current (:xt/id before))))
      (no-retry! gate))))

(deftest unadmitted-double-submit-is-rejected-even-if-first-conflicts
  (let [{:keys [before gate]} (staged!)]
    (external-put! (assoc before :note "out-of-date"))
    (rejects! :xtdb/assert-failed (:submit! gate))
    (no-retry! gate)
    (is (= :threw-outcome-unknown (:status ((:summary! gate)))))
    (nonclaims! ((:summary! gate)))))

(deftest external-writer-can-still-bypass-this-opt-in-boundary
  (let [{:keys [before gate]} (staged!)
        outside (assoc before :note "direct-xtdb-bypass")]
    (external-put! outside)
    (is (= outside (current (:xt/id before))))
    (is (= :not-established
           (:complete-deployment-writer-coverage ((:summary! gate)))))
    (rejects! :xtdb/assert-failed (:submit! gate))
    (is (= outside (current (:xt/id before))))
    (nonclaims! ((:summary! gate)))))

(deftest supported-scalar-prestate-types-admit-without-proving-type-semantics
  (let [b (assoc (before)
                 :visible true
                 :counter 23
                 :decimal (bigdec "12.5")
                 :when-at (Instant/parse "2020-01-02T03:04:05Z")
                 :public-uuid (UUID/randomUUID))
        s (source b)
        gate (strict/prepare-complete-guard-one-shot! (node) s)]
    (is (= 10 (:compared-top-level-field-count ((:summary! gate)))))
    (is (= 8 (:added-snapshot-comparison-count ((:summary! gate)))))
    (is (= :prepared (:status ((:summary! gate)))))
    (nonclaims! ((:summary! gate)))))

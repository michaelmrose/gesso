(ns gesso.assurance.proof.xtdb-gateway-conditional-transaction-effect-test
  "v1544: Permanent adversarial tests of v1543's conditional transaction-local
   write-effect inference, building on the v1541
   acquired-prestate -> guarded one-shot -> exact-return -> later pinned
   whole-document-and-ledger observation.

   Ledger matching is conditional evidence under TRUSTED XTDB atomic PUT
   semantics, not a new physical-causality theorem.
   Physical before/after reads are external to the transaction; a matching
   later document is not proof of causal attribution, transaction-internal
   prestate, an uninterrupted history, complete writer coverage, principal
   authority, or universal refinement. Deliberate direct XTDB writes in
   these tests are independent writers, never retroactively gateway-owned."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence :as correspondence]
            [gesso.assurance.proof.xtdb-poststate-basis-acquisition :as poststate]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.assurance.proof.physical-domain-projection :as projection]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private table :gesso_v1541_correspondence_probe)
(def ^:private versions {:revision-key :revision
                         :created-at-key :created-at
                         :updated-at-key :updated-at})
(def ^:private obligation
  {:construction
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
    :frame #{:request/version :request/note}}
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants {:request/status-valid
                {:readers #{:request/status}
                 :expression [:in [:read :request/status]
                              [:literal #{:open :claimed :done}]]}
                :request/version-valid
                {:readers #{:request/version}
                 :expression [:>= [:read :request/version] [:literal 0]]}}})
(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(defn- before [] {:xt/id (str "v1541-" (random-uuid))
                  :revision 0 :created-at "created" :updated-at "before"
                  :value "private-v1541"})
(defn- after [b] (-> b (assoc :revision 1 :updated-at "after")))
(defn- projection-source [b]
  (let [id (:xt/id b)]
    {:obligation-source obligation
     :protected-domain-source
     {:plan {:commands [(command/update-command table :claim b (after b) versions)]
             :emit false}
      :protected-documents [{:target [table id] :fields :all}]}
     :field-locations
     {:request/status {:target [table id] :physical-field :revision}
      :request/owner {:target [table id] :physical-field :updated-at}
      :request/version {:target [table id] :physical-field :created-at}
      :request/note {:target [table id] :physical-field :value}}}))
(defn- put! [doc] (xt/execute-tx @!node [[:put-docs table doc]]))
(defn- prep [b] (correspondence/prepare-acquired-poststate-one-shot!
                 @!node (projection-source b)))
(defn- rejection [f]
  (try (f) :no-error (catch Throwable e
                     (or (:reason (ex-data e)) (:xtdb.error/code (ex-data e))
                         (throw e)))))

(def ^:private positive :declared-postimage-under-trusted-xtdb-semantics)
(def ^:private required-premises
  {:source-vector :rederived-and-one-shot-owned
   :whole-document-put :exactly-one-target
   :transaction-ledger :same-basis-matching-committed-key
   :backend-atomic-put-semantics :trusted-not-proved})
(defn- verify-inference! [report]
  (is (= positive (:transaction-local-write-effect report)))
  (is (= required-premises (:transaction-local-write-effect-premises report)))
  (is (= :execution-only (:transaction-local-effect-time-scope report)))
  (is (= :not-independently-established (:transaction-local-effect-physical-verification report)))
  (is (= :not-established (:actual-transaction-caused-poststate report)))
  (is (= :not-issued (:certificate report)))
  (is (= :none (:authority report)))
  (is (not (str/includes? (pr-str report) "private-v1541"))))
(deftest trusted-effect-on-committed-transaction
  (let [b (before) _ (put! b) p (prep b)
        return ((:submit! p)) report ((:observe! p) return)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (verify-inference! report)))
(deftest later-divergence-preserves-transaction-local-inference
  (let [b (before) _ (put! b) p (prep b)
        return ((:submit! p)) _ (put! (assoc (after b) :value "outsider"))
        report ((:observe! p) return)]
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (verify-inference! report)))
(deftest later-deletion-preserves-transaction-local-inference
  (let [b (before) _ (put! b) p (prep b)
        return ((:submit! p)) _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        report ((:observe! p) return)]
    (is (not= :bounded-later-poststate-matches-after (:status report)))
    (is (= positive (:transaction-local-write-effect report)))
    (is (false? (:observed-ledger-joined-poststate? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (is (= :separately-pinned-matching-committed-key
           (get-in report [:transaction-local-write-effect-premises :transaction-ledger])))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (is (= :execution-only (:transaction-local-effect-time-scope report)))))
(deftest later-restoration-does-not-prove-read-causality
  (let [b (before) _ (put! b) p (prep b)
        return ((:submit! p)) _ (put! b) _ (put! (after b))
        report ((:observe! p) return)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (verify-inference! report)))
(deftest unreadable-ledger-withholds-the-effect-inference
  (with-open [foreign (xtn/start-node {})]
    (let [b (before) _ (xt/execute-tx foreign [[:put-docs table b]])
          p (correspondence/prepare-acquired-poststate-one-shot! foreign (projection-source b))
          return ((:submit! p))]
      (.close foreign)
      (let [report ((:observe! p) return)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (= :not-established (:transaction-local-write-effect report)))
        (is (= :not-established
               (get-in report [:transaction-local-write-effect-premises :transaction-ledger])))
        (is (= :trusted-not-proved
               (get-in report [:transaction-local-write-effect-premises :backend-atomic-put-semantics])))
        (is (= :not-established (:actual-transaction-caused-poststate report)))))))
(deftest substituted-ledger-query-withholds-the-inference
  (let [b (before) _ (put! b) p (prep b) return ((:submit! p))]
    (with-redefs [xt/q (fn [& _] [{:snapshot-token "impostor"}])]
      (let [report ((:observe! p) return)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (= :not-established (:transaction-local-write-effect report)))))
    (verify-inference! ((:observe! p) return))))
(deftest forged-return-cannot-claim-transaction-effect
  (let [b (before) _ (put! b) p (prep b) return ((:submit! p))]
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (into {} return)))))
    (verify-inference! ((:observe! p) return))))
(deftest stale-preconditions-block-effect-report
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "changed"))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) {}))))))

(deftest substituted-ledger-reader-is-rejected-not-accepted-as-effect
  (let [b (before) _ (put! b) p (prep b) return ((:submit! p))]
    (with-redefs [gateway/require-local-ledger-observation!
                  (fn [& _] {:ledger-status :reported-key-matches-committed-ledger-row})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) return)))))
    (verify-inference! ((:observe! p) return))))


;; An authenticated, committed transaction is not necessarily responsible for
;; the document seen at any subsequent read. These assertions are deliberately
;; checked both on positive and negative transaction-effect reports.
(def ^:private nonclaims
  {:transaction-internal-prestate :not-authenticated
   :backend-assert-semantics :trusted-bounded-premise
   :aba-history-detection :not-established
   :actual-transaction-caused-poststate :not-established
   :physical-frame-preservation :not-established
   :outside-gateway-writer-closure :not-established
   :complete-deployment-writer-coverage :not-established
   :caller-node-authenticity :not-established
   :principal-authorization :not-established
   :universal-physical-refinement :not-established
   :proof-status :source-bound-conditional-observation-only
   :certificate :not-issued
   :admission :not-admitted
   :authority :none})
(def ^:private prohibited-report-keys
  #{:node :plan :source :projection-source :protected-documents :before
    :after :document :physical-document :rows :physical-acquisition
    :tx-key :transaction-key :tx-id :tx-ops :snapshot-token :sql :principal})
(defn- verify-scope! [report]
  (doseq [[k v] nonclaims]
    (is (= v (get report k)) (str "Unexpected assurance at " k)))
  (is (= :execution-only (:transaction-local-effect-time-scope report)))
  (is (= :not-independently-established
         (:transaction-local-effect-physical-verification report)))
  (is (= :trusted-not-proved
         (get-in report [:transaction-local-write-effect-premises
                         :backend-atomic-put-semantics])))
  (is (not-any? #(contains? report %) prohibited-report-keys))
  (is (not (str/includes? (pr-str report) "private-v1541"))))
(defn- verify-no-effect! [report]
  (is (= :not-established (:transaction-local-write-effect report)))
  (is (= :not-established
         (get-in report [:transaction-local-write-effect-premises
                         :transaction-ledger])))
  (verify-scope! report))

(deftest conditional-inference-is-execution-scoped-not-poststate-proof
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p)) report ((:observe! p) response)]
    (is (= :returned-transaction-key (:status ((:summary! p)))))
    (is (= :one-rechecked-whole-document-replacement
           (:submitted-footprint report)))
    (is (= :source-matched-pinned-external-read (:prestate-origin report)))
    (is (= :exact-gateway-owned-transaction-key-object (:return-linkage report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (true? (:observed-complete-protected-document? report)))
    (verify-inference! report)
    (verify-scope! report)))

(deftest missing-document-uses-separate-ledger-but-never-invents-poststate
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        report ((:observe! p) response)]
    (is (= :declared-postimage-under-trusted-xtdb-semantics
           (:transaction-local-write-effect report)))
    (is (= :separately-pinned-matching-committed-key
           (get-in report [:transaction-local-write-effect-premises
                           :transaction-ledger])))
    (is (false? (:observed-complete-protected-document? report)))
    (is (false? (:observed-ledger-joined-poststate? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (is (not= :bounded-later-poststate-matches-after (:status report)))
    (verify-scope! report)))

(deftest a-second-physical-writer-can-delete-and-recreate-a-matching-document
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        deleted ((:observe! p) response)
        _ (put! (after b))
        recreated ((:observe! p) response)]
    (is (not= :bounded-later-poststate-matches-after (:status deleted)))
    (is (= :bounded-later-poststate-matches-after (:status recreated)))
    (is (= positive (:transaction-local-write-effect deleted)))
    (is (= positive (:transaction-local-write-effect recreated)))
    (is (= :separately-pinned-matching-committed-key
           (get-in deleted [:transaction-local-write-effect-premises
                            :transaction-ledger])))
    (is (= :same-basis-matching-committed-key
           (get-in recreated [:transaction-local-write-effect-premises
                              :transaction-ledger])))
    (is (= (:process-local-attempt-id deleted)
           (:process-local-attempt-id recreated)))
    (verify-scope! deleted)
    (verify-scope! recreated)))

(deftest unrelated-writes-do-not-turn-transaction-local-effect-into-writer-closure
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (put! (assoc (before) :value "independent"))
        report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (verify-inference! report)
    (is (= :not-established (:outside-gateway-writer-closure report)))
    (verify-scope! report)))

(deftest repeated-observation-can-change-status-but-never-reissues-put
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        matched ((:observe! p) response)
        _ (put! (assoc (after b) :value "outside-v1544-divergence"))
        diverged ((:observe! p) response)
        _ (put! (after b))
        restored ((:observe! p) response)]
    (is (= [:bounded-later-poststate-matches-after
            :bounded-later-poststate-diverges
            :bounded-later-poststate-matches-after]
           (mapv :status [matched diverged restored])))
    (is (= 1 (count (set (map :process-local-attempt-id
                            [matched diverged restored])))))
    (is (every? #(= positive (:transaction-local-write-effect %))
                [matched diverged restored]))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! p)))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))
    (doseq [report [matched diverged restored]]
      (verify-scope! report))))

(deftest copied-modified-and-absent-return-objects-cannot-claim-effect
  (let [b (before) _ (put! b) p (prep b)]
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) nil))))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) {}))))
    (let [response ((:submit! p))]
      (is (= :not-this-gateway-local-return
             (rejection #((:observe! p) (assoc response :extra :forged)))))
      (is (= :not-this-gateway-local-return
             (rejection #((:observe! p) (into {} response)))))
      (verify-inference! ((:observe! p) response)))))

(deftest another-gateways-authentic-return-is-not-this-gateways-return
  (let [b1 (before) b2 (before) _ (put! b1) _ (put! b2)
        p1 (prep b1) p2 (prep b2)
        response1 ((:submit! p1)) response2 ((:submit! p2))]
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p1) response2))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p2) response1))))
    (verify-inference! ((:observe! p1) response1))
    (verify-inference! ((:observe! p2) response2))))

(deftest stale-guard-conflict-never-produces-a-conditional-write-effect
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "conflicting-before-submit"))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :threw-outcome-unknown (:status ((:summary! p)))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) {}))))))

(deftest missing-prestate-never-exposes-a-capability-to-infer-effects
  (let [b (before)]
    (is (= :missing-or-ambiguous-external-document
           (rejection #(prep b))))
    (is (= :no-error (rejection #(put! b))))
    (is (= :source-before-disagrees-with-external-read
           (rejection #(prep (assoc b :value "forged-before")))))))

(deftest closed-node-blocks-both-poststate-and-independent-ledger-inference
  (with-open [foreign (xtn/start-node {})]
    (let [b (before) _ (xt/execute-tx foreign [[:put-docs table b]])
          p (correspondence/prepare-acquired-poststate-one-shot!
             foreign (projection-source b))
          response ((:submit! p))]
      (.close foreign)
      (let [report ((:observe! p) response)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (false? (:observed-ledger-joined-poststate? report)))
        (is (false? (:observed-whole-document-equals-source-after? report)))
        (verify-no-effect! report)))))

(deftest forged-backend-query-cannot-create-ledger-corroboration
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))]
    (with-redefs [xt/q (fn [& _] [{:tx-id 1 :committed true
                                  :system-time "fake"}])]
      (let [report ((:observe! p) response)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (verify-no-effect! report)))
    (verify-inference! ((:observe! p) response))))

(deftest replaced-ledger-and-poststate-reader-vars-fail-before-observation
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))]
    (with-redefs [gateway/require-local-ledger-observation!
                  (fn [& _] {:ledger-status :reported-key-matches-committed-ledger-row})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) response)))))
    (with-redefs [poststate/acquire-poststate-basis!
                  (fn [& _] {:report {:status :forged}})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) response)))))
    (verify-inference! ((:observe! p) response))))

(deftest conditional-report-never-contains-the-source-transaction-or-physical-row
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        report ((:observe! p) response) text (str (pr-str report) (pr-str ((:summary! p))))]
    (verify-scope! report)
    (is (not (str/includes? text (:xt/id b))))
    (is (not (str/includes? text (:value b))))
    (is (not (str/includes? text (:created-at b))))
    (is (not-any? #(contains? report %) prohibited-report-keys))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:authority report)))))

(deftest concurrently-reading-the-same-authentic-return-is-read-only
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        jobs (doall (repeatedly 4 #(future ((:observe! p) response))))
        reports (mapv #(deref % 90000 :timed-out) jobs)]
    (is (not-any? #{:timed-out} reports))
    (is (every? #(= positive (:transaction-local-write-effect %)) reports))
    (is (every? #(= :bounded-later-poststate-matches-after (:status %)) reports))
    (is (= 1 (count (set (map :process-local-attempt-id reports)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! p)))))
    (doseq [report reports]
      (verify-scope! report))))

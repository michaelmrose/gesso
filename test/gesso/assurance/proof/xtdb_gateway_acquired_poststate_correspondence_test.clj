(ns gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence-test
  "v1544: Permanent adversarial embedded-XTDB coverage for v1541's
   acquired-prestate -> guarded one-shot -> exact-return -> later pinned
   whole-document-and-ledger observation, including v1543's independently
   corroborated CONDITIONAL transaction-local effect under XTDB premises.

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
(deftest matched-later-document
  (let [b (before) _ (put! b)
        p (prep b) early ((:summary! p))
        result ((:submit! p)) report ((:observe! p) result)]
    (is (= :prepared (:status early)))
    (is (= :one-whole-document-replacement (:admitted-write-footprint early)))
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (true? (:observed-whole-document-equals-source-after? report)))
    (is (= 2 (:prewrite-assertion-count report)))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (is (= :not-authenticated (:transaction-internal-prestate report)))
    (is (= :not-issued (:certificate report)))
    (is (not (clojure.string/includes? (pr-str report) "private-v1541")))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))))
(deftest outside-writer-divergence-does-not-change-return
  (let [b (before) _ (put! b) p (prep b)
        result ((:submit! p)) _ (put! (assoc (after b) :value "outsider"))
        report ((:observe! p) result)]
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (is (= :not-established (:outside-gateway-writer-closure report)))))
(deftest return-identity-and-preflight-fail-closed
  (let [b (before) _ (put! b) p (prep b)
        _ (is (#{:not-this-gateway-local-return :invalid-local-gateway}
               (rejection #((:observe! p) {}))))
        result ((:submit! p))]
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (into {} result)))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p) result))))))
(deftest stale-update-cannot-create-result
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "changed"))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) {}))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))))
(deftest mismatched-source-is-rejected-before-issuance
  (let [b (before) _ (put! b)]
    (is (= :source-before-disagrees-with-external-read
           (rejection #(prep (assoc b :value "forged")))))
    (is (= :invalid-projection-source
           (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                        @!node {:protected-domain-source {} }))))))
(deftest foreign-writer-can-produce-apparently-matching-poststate
  (let [b (before) _ (put! b) p (prep b)
        result ((:submit! p))
        _ (put! (assoc (after b) :value "other-write"))
        _ (put! (after b))
        report ((:observe! p) result)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (is (= :not-established (:outside-gateway-writer-closure report)))))
(deftest read-failure-after-return-is-not-a-fabricated-commit-result
  (with-open [foreign (xtn/start-node {})]
    (let [b (before)
          _ (xt/execute-tx foreign [[:put-docs table b]])
          p (correspondence/prepare-acquired-poststate-one-shot!
             foreign (projection-source b))
          result ((:submit! p))]
      (.close foreign)
      (let [report ((:observe! p) result)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (false? (:observed-ledger-joined-poststate? report)))
        (is (false? (:observed-whole-document-equals-source-after? report)))
        (is (= :not-established (:actual-transaction-caused-poststate report)))))))


;; The following assertions distinguish an actual returned gateway object and
;; a later read from any claim that this transaction caused the later state.
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
(def ^:private summary-nonclaims
  (-> nonclaims
      (dissoc :actual-transaction-caused-poststate
              :physical-frame-preservation
              :outside-gateway-writer-closure)
      (assoc :physical-causality :not-established)))
(def ^:private forbidden-report-keys
  #{:node :plan :source :projection-source :protected-documents
    :before :after :document :physical-document :rows :physical-acquisition
    :tx-key :transaction-key :tx-id :tx-ops :snapshot-token :sql :principal})
(defn- assert-no-authority! [report expected]
  (doseq [[key value] expected]
    (is (= value (get report key))
        (str "Assurance overclaim at " key)))
  (is (not-any? #(contains? report %) forbidden-report-keys))
  (is (not (str/includes? (pr-str report) "private-v1541")))
  (is (not (str/includes? (pr-str report) "gesso_v1541_correspondence_probe"))))
(defn- assert-read-only! [p]
  (let [summary ((:summary! p))]
    (is (= 1 (:owned-execute-tx-call-entries summary)))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))))
(defn- raw [id]
  (first (xt/q @!node ["SELECT * FROM gesso_v1541_correspondence_probe WHERE _id = ?" id])))

(deftest whole-report-has-no-elevated-assurance
  (let [b (before) _ (put! b) p (prep b)
        summary-before ((:summary! p))
        result ((:submit! p)) report ((:observe! p) result)
        summary-after ((:summary! p))]
    (is (= correspondence/correspondence-type (:type p)))
    (is (= correspondence/correspondence-version (:version p)))
    (is (= correspondence/correspondence-type (:type report)))
    (is (= correspondence/correspondence-version (:version report)))
    (is (= :pinned-same-node-source-matched (:external-before summary-before)))
    (is (= :complete-top-level-cas (:transaction-time-preconditions summary-before)))
    (is (= :one-whole-document-replacement (:admitted-write-footprint summary-before)))
    (is (= :returned-transaction-key (:status summary-after)))
    (is (= :prepared (:status summary-before)))
    (is (= :returned-transaction-key (:status summary-after)))
    (is (= (after b) (raw (:xt/id b))))
    (is (= :exact-gateway-owned-transaction-key-object (:return-linkage report)))
    (is (= :source-matched-pinned-external-read (:prestate-origin report)))
    (is (= :later-pinned-external-read (:physical-poststate-origin report)))
    (is (= :one-rechecked-whole-document-replacement (:submitted-footprint report)))
    (is (= :snapshot-not-earlier-than-transaction (:poststate-basis-ordering report)))
    (is (= 1 (:owned-execute-tx-call-entries report)))
    (assert-no-authority! report nonclaims)
    (assert-no-authority! summary-before summary-nonclaims)
    (assert-no-authority! summary-after summary-nonclaims)
    (assert-read-only! p)))

(deftest report-does-not-expose-document-id-or-revision-values
  (let [b (before) _ (put! b) p (prep b)
        result ((:submit! p)) report ((:observe! p) result)
        printed (str (pr-str report) (pr-str ((:summary! p))))]
    (is (not (str/includes? printed (:xt/id b))))
    (is (not (str/includes? printed (:created-at b))))
    (is (not (str/includes? printed (:value b))))
    (is (not-any? #(contains? report %) forbidden-report-keys))))

(deftest observer-must-have-an-authentic-gateway-return
  (let [b (before) _ (put! b) p (prep b)]
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) nil))))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) {}))))
    (is (= :prepared (:status ((:summary! p)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! p)))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p) ((:submit! p))))))))

(deftest modified-or-copied-return-is-never-an-authentic-return
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        copied (into {} response)]
    (is (not (identical? response copied)))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p) copied))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (assoc response :diagnostic {})))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (dissoc response :tx-key)))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p) response))))
    (assert-read-only! p)))

(deftest one-gateways-return-does-not-authorize-another-gateways-observer
  (let [b1 (before) b2 (before) _ (put! b1) _ (put! b2)
        p1 (prep b1) p2 (prep b2)
        r1 ((:submit! p1)) r2 ((:submit! p2))]
    (is (= :not-this-gateway-local-return (rejection #((:observe! p2) r1))))
    (is (= :not-this-gateway-local-return (rejection #((:observe! p1) r2))))
    (is (= :bounded-later-poststate-matches-after (:status ((:observe! p1) r1))))
    (is (= :bounded-later-poststate-matches-after (:status ((:observe! p2) r2))))
    (assert-read-only! p1)
    (assert-read-only! p2)))

(deftest observers-can-repeat-without-resubmitting
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        reports (repeatedly 3 #((:observe! p) response))]
    (is (= [:bounded-later-poststate-matches-after
            :bounded-later-poststate-matches-after
            :bounded-later-poststate-matches-after]
           (mapv :status reports)))
    (is (= 1 (count (set (map :process-local-attempt-id reports)))))
    (is (= 1 (count (set (map :process-local-source-binding reports)))))
    (assert-read-only! p)))

(deftest repeated-observations-may-report-different-physical-states
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        first-report ((:observe! p) response)
        _ (put! (assoc (after b) :value "private-v1541-outsider"))
        second-report ((:observe! p) response)
        _ (put! (after b))
        third-report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status first-report)))
    (is (= :bounded-later-poststate-diverges (:status second-report)))
    (is (= :bounded-later-poststate-matches-after (:status third-report)))
    (is (true? (:observed-ledger-joined-poststate? second-report)))
    (doseq [report [first-report second-report third-report]]
      (assert-no-authority! report nonclaims))
    (assert-read-only! p)))

(deftest changing-one-mapped-nonversion-field-is-visible-in-whole-document-check
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (put! (assoc (after b) :created-at "outside-created-at"))
        report ((:observe! p) response)]
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (true? (:observed-complete-protected-document? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (assert-no-authority! report nonclaims)))

(deftest outside-writer-added-field-must-not-masquerade-as-exact-post-image
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (put! (assoc (after b) :intruder "other-writer"))
        report ((:observe! p) response)]
    (is (not= :bounded-later-poststate-matches-after (:status report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (assert-no-authority! report nonclaims)))

(deftest outside-writer-deletes-protected-record
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        report ((:observe! p) response)]
    (is (not= :bounded-later-poststate-matches-after (:status report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (assert-no-authority! report nonclaims)))

(deftest outside-writer-restores-before-image-after-committed-update
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        _ (put! b) report ((:observe! p) response)]
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (= b (raw (:xt/id b))))
    (is (= :not-established (:actual-transaction-caused-poststate report)))))

(deftest aba-restoration-before-submit-is-not-detected-by-complete-cas
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "outside-before-submit"))
        _ (put! b)
        response ((:submit! p)) report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (= :not-established (:aba-history-detection report)))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (assert-read-only! p)))

(deftest addition-between-acquisition-and-submit-is-rejected-by-count-assertion
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :intruder "outside"))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) {}))))
    (is (= :threw-outcome-unknown (:status ((:summary! p)))))))

(deftest removal-between-acquisition-and-submit-is-rejected-by-count-assertion
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (dissoc b :value))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))
    (is (= :threw-outcome-unknown (:status ((:summary! p)))))))

(deftest field-swap-with-same-count-is-rejected-by-individual-assertions
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (-> b (dissoc :value) (assoc :other "replacement")))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :threw-outcome-unknown (:status ((:summary! p)))))))

(deftest competing-gateways-cannot-both-commit-one-revision
  (let [b (before) _ (put! b)
        p1 (prep b) p2 (prep b)
        r1 ((:submit! p1))]
    (is (= :xtdb/assert-failed (rejection (:submit! p2))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p1) r1))))
    (is (= :one-shot-already-attempted (rejection (:submit! p2))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! p2)))))))

(deftest rejected-source-does-not-expose-an-observer-or-submit-capability
  (let [b (before) _ (put! b)]
    (doseq [bad [(assoc b :value "forged")
                 (dissoc b :value)
                 (assoc b :extra "unobserved")]]
      (is (= :source-before-disagrees-with-external-read
             (rejection #(prep bad)))))
    (is (= b (raw (:xt/id b))))))

(deftest absent-source-record-cannot-issue-correspondence-capability
  (let [b (before)]
    (is (= :missing-or-ambiguous-external-document
           (rejection #(prep b))))
    (is (nil? (raw (:xt/id b))))))

(deftest malformed-projection-shapes-are-fail-closed
  (let [b (before) _ (put! b) source (projection-source b)]
    (doseq [bad [nil {} [] (assoc source :node @!node)
                 (dissoc source :obligation-source)
                 (dissoc source :field-locations)
                 (with-meta source {:trusted true})]]
      (is (= :invalid-projection-source
             (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                           @!node bad)))))
    (is (= :missing-xtdb-node
           (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                         nil source))))))

(deftest mismatched-protected-target-cannot-issue-correspondence
  (let [b (before) _ (put! b) source (projection-source b)]
    (is (not= :no-error
              (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                           @!node
                           (assoc-in source
                                     [:protected-domain-source :protected-documents 0 :target]
                                     [table "another-id"])))))))

(deftest changed-field-mapping-cannot-forge-poststate-agreement
  (let [b (before) _ (put! b) source (projection-source b)
        bad (assoc-in source [:field-locations :request/note :physical-field]
                      :missing-but-declared)]
    ;; Projection declarations can be structurally valid while physical
    ;; acquisition fails or yields an unavailable diagnostic. They cannot
    ;; manufacture a positive exact-whole-document correspondence claim.
    (let [outcome (try
                    (let [p (correspondence/prepare-acquired-poststate-one-shot! @!node bad)
                          result ((:submit! p))]
                      (:status ((:observe! p) result)))
                    (catch Throwable e (or (:reason (ex-data e)) :threw)))]
      (is (not= :bounded-later-poststate-matches-after outcome)))))

(deftest checker-var-replacement-is-rejected-before-capability-exposure
  (let [b (before) _ (put! b) source (projection-source b)]
    (with-redefs [projection/describe-physical-domain-projection!
                  (fn [& _] {:type :forged :certificate :issued})]
      (is (= :substituted-correspondence-seam
             (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                           @!node source)))))))

(deftest checker-var-replacement-is-rejected-before-observing-a-return
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))]
    (with-redefs [poststate/redacted-summary (fn [& _] {:status :forged})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) response)))))
    (with-redefs [gateway/require-local-return! (fn [& _] {:verified true})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) response)))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p) response))))
    (assert-read-only! p)))

(deftest substituted-backend-read-cannot-manufacture-a-positive-observation
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))]
    (with-redefs [xt/q (fn [& _] [{:snapshot-token "forged"}])]
      (let [report ((:observe! p) response)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (false? (:observed-ledger-joined-poststate? report)))
        (is (false? (:observed-whole-document-equals-source-after? report)))))
    (is (= :bounded-later-poststate-matches-after
           (:status ((:observe! p) response))))
    (assert-read-only! p)))

(deftest foreign-node-prestate-is-not-implicitly-acquired-from-original-node
  (let [b (before) _ (put! b) source (projection-source b)]
    (with-open [foreign (xtn/start-node {})]
      (is (= :missing-or-ambiguous-external-document
             (rejection #(correspondence/prepare-acquired-poststate-one-shot!
                           foreign source))))
      (xt/execute-tx foreign [[:put-docs table b]])
      (let [p (correspondence/prepare-acquired-poststate-one-shot!
               foreign source)
            response ((:submit! p)) report ((:observe! p) response)]
        (is (= :bounded-later-poststate-matches-after (:status report)))
        (is (= :not-established (:caller-node-authenticity report)))
        (is (= b (raw (:xt/id b))))
        (assert-read-only! p)))))

(deftest parallel-observations-do-not-resubmit-or-gain-extra-authority
  (let [b (before) _ (put! b) p (prep b) response ((:submit! p))
        work (doall (repeatedly 4 #(future ((:observe! p) response))))
        reports (mapv #(deref % 70000 :timed-out) work)]
    (is (not-any? #{:timed-out} reports))
    (is (every? #(= :bounded-later-poststate-matches-after (:status %))
                reports))
    (is (= 1 (count (set (map :process-local-attempt-id reports)))))
    (assert-read-only! p)))


;; v1544: Keep transaction-LOCAL effect conditional on an exact gateway
;; return, a committed transaction-ledger match, and *trusted* XTDB atomic
;; PUT semantics. None of those premises attributes a later snapshot to the
;; gateway, authenticates transaction-internal prestate, or proves frame
;; preservation. Deliberate outside writes below are independent of the gate.
(def ^:private conditional-effect
  :declared-postimage-under-trusted-xtdb-semantics)
(def ^:private execution-premises
  {:source-vector :rederived-and-one-shot-owned
   :whole-document-put :exactly-one-target
   :backend-atomic-put-semantics :trusted-not-proved})
(defn- assert-conditional-effect! [report ledger-kind]
  (is (= conditional-effect (:transaction-local-write-effect report)))
  (is (= ledger-kind
         (get-in report [:transaction-local-write-effect-premises
                         :transaction-ledger])))
  (doseq [[k v] execution-premises]
    (is (= v (get-in report [:transaction-local-write-effect-premises k]))))
  (is (= :execution-only (:transaction-local-effect-time-scope report)))
  (is (= :not-independently-established
         (:transaction-local-effect-physical-verification report)))
  (assert-no-authority! report nonclaims))
(defn- assert-effect-withheld! [report]
  (is (= :not-established (:transaction-local-write-effect report)))
  (is (= :not-established
         (get-in report [:transaction-local-write-effect-premises
                         :transaction-ledger])))
  (is (= :trusted-not-proved
         (get-in report [:transaction-local-write-effect-premises
                         :backend-atomic-put-semantics])))
  (is (= :execution-only (:transaction-local-effect-time-scope report)))
  (is (= :not-independently-established
         (:transaction-local-effect-physical-verification report)))
  (assert-no-authority! report nonclaims))

(deftest v1543-conditional-effect-requires-committed-matching-ledger
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p)) report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (true? (:observed-whole-document-equals-source-after? report)))
    (assert-conditional-effect! report :same-basis-matching-committed-key)
    (assert-read-only! p)))

(deftest v1543-later-overwrite-does-not-undo-the-transaction-local-effect
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        _ (put! (assoc (after b) :value "private-v1541-outsider"))
        report ((:observe! p) response)]
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (true? (:observed-ledger-joined-poststate? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (assert-conditional-effect! report :same-basis-matching-committed-key)
    (is (= "private-v1541-outsider" (:value (raw (:xt/id b)))))))

(deftest v1543-later-deletion-can-retain-only-ledger-corroboration
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        report ((:observe! p) response)]
    (is (nil? (raw (:xt/id b))))
    (is (= :poststate-or-ledger-unavailable (:status report)))
    (is (false? (:observed-ledger-joined-poststate? report)))
    (is (false? (:observed-complete-protected-document? report)))
    (is (false? (:observed-whole-document-equals-source-after? report)))
    (assert-conditional-effect! report :separately-pinned-matching-committed-key)
    (assert-read-only! p)))

(deftest v1543-deletion-effect-does-not-imply-present-poststate
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        first-report ((:observe! p) response)
        _ (put! (after b))
        second-report ((:observe! p) response)]
    (is (= :poststate-or-ledger-unavailable (:status first-report)))
    (is (= :bounded-later-poststate-matches-after (:status second-report)))
    (assert-conditional-effect! first-report
                                :separately-pinned-matching-committed-key)
    (assert-conditional-effect! second-report
                                :same-basis-matching-committed-key)
    (is (= (:process-local-attempt-id first-report)
           (:process-local-attempt-id second-report)))
    (assert-read-only! p)))

(deftest v1543-outside-restoration-cannot-assert-later-causality
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        _ (put! b)
        _ (put! (after b))
        report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (= :not-established (:actual-transaction-caused-poststate report)))
    (is (= :not-established (:outside-gateway-writer-closure report)))
    (assert-conditional-effect! report :same-basis-matching-committed-key)))

(deftest v1543-aba-before-submission-does-not-become-history-proof
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "private-v1541-aba-intermediate"))
        _ (put! b)
        response ((:submit! p)) report ((:observe! p) response)]
    (is (= :bounded-later-poststate-matches-after (:status report)))
    (is (= :not-established (:aba-history-detection report)))
    (is (= :not-authenticated (:transaction-internal-prestate report)))
    (assert-conditional-effect! report :same-basis-matching-committed-key)))

(deftest v1543-unavailable-backend-cannot-supply-commit-evidence
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
        (assert-effect-withheld! report)))))

(deftest v1543-fake-query-cannot-fabricate-a-matching-committed-ledger
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))]
    (with-redefs [xt/q (fn [& _] [{:transaction-id "private-v1541-forged"}])]
      (let [report ((:observe! p) response)]
        (is (= :poststate-or-ledger-unavailable (:status report)))
        (is (false? (:observed-ledger-joined-poststate? report)))
        (assert-effect-withheld! report)))
    (assert-conditional-effect! ((:observe! p) response)
                                :same-basis-matching-committed-key)))

(deftest v1543-return-copies-and-cross-gateway-returns-cannot-claim-effect
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))]
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (into {} response)))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) (assoc response :extra :forged)))))
    (let [b2 (before) _ (put! b2) p2 (prep b2)
          response2 ((:submit! p2))]
      (is (= :not-this-gateway-local-return
             (rejection #((:observe! p) response2))))
      (is (= :not-this-gateway-local-return
             (rejection #((:observe! p2) response)))))
    (assert-conditional-effect! ((:observe! p) response)
                                :same-basis-matching-committed-key)))

(deftest v1543-stale-cas-does-not-expose-any-local-effect-report
  (let [b (before) _ (put! b) p (prep b)
        _ (put! (assoc b :value "private-v1541-stale"))]
    (is (= :xtdb/assert-failed (rejection (:submit! p))))
    (is (= :threw-outcome-unknown (:status ((:summary! p)))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! p) {}))))
    (is (= :one-shot-already-attempted (rejection (:submit! p))))))

(deftest v1543-ledger-observer-substitution-rejected-before-observation
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))]
    (with-redefs [gateway/require-local-ledger-observation!
                  (fn [& _]
                    {:ledger-status :reported-key-matches-committed-ledger-row
                     :ledger-checks {:tx-id true :committed true :system-time true}})]
      (is (= :substituted-correspondence-seam
             (rejection #((:observe! p) response)))))
    (assert-conditional-effect! ((:observe! p) response)
                                :same-basis-matching-committed-key)))

(deftest v1543-repeated-reads-remain-read-only-and-conditionally-scoped
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        r1 ((:observe! p) response)
        _ (put! (assoc (after b) :value "private-v1541-other"))
        r2 ((:observe! p) response)
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        r3 ((:observe! p) response)]
    (is (= [:bounded-later-poststate-matches-after
            :bounded-later-poststate-diverges
            :poststate-or-ledger-unavailable]
           (mapv :status [r1 r2 r3])))
    (doseq [r [r1 r2]]
      (assert-conditional-effect! r :same-basis-matching-committed-key))
    (assert-conditional-effect! r3 :separately-pinned-matching-committed-key)
    (is (= 1 (count (set (map :process-local-attempt-id [r1 r2 r3])))))
    (is (= 1 (count (set (map :process-local-source-binding [r1 r2 r3])))))
    (assert-read-only! p)))

(deftest v1543-parallel-ledger-observations-never-increase-submit-count
  (let [b (before) _ (put! b) p (prep b)
        response ((:submit! p))
        _ (xt/execute-tx @!node [[:delete-docs table (:xt/id b)]])
        futures (doall (repeatedly 4 #(future ((:observe! p) response))))
        reports (mapv #(deref % 70000 :timed-out) futures)]
    (is (not-any? #{:timed-out} reports))
    (doseq [r reports]
      (is (= :poststate-or-ledger-unavailable (:status r)))
      (assert-conditional-effect! r :separately-pinned-matching-committed-key))
    (assert-read-only! p)))

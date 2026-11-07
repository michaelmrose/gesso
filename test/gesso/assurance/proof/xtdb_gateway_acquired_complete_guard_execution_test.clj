(ns gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution-test
  "v1540: Permanent embedded-XTDB adversarial coverage for the opt-in
   acquired-prestate complete-guard one-shot gateway. The read is pinned and
   source-matched BEFORE issuance; the transaction's complete top-level CAS
   assertions enforce the supplied prestate at their own execution point.

   These finite tests do not establish an authenticated node, uninterrupted
   history, exclusivity of physical writers, transaction-internal read basis,
   principal authority, causality, or universal physical refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution :as acquired]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.assurance.proof.xtdb-gateway-complete-guard-execution :as strict]
            [xtdb.basis :as basis]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(def ^:private table :gesso_v1540_acquired_probe)
(def ^:private version {:revision-key :revision :created-at-key :created-at
                       :updated-at-key :updated-at})
(defn- before [] {:xt/id (str "v1540-" (random-uuid))
                  :revision 0 :created-at "created" :updated-at "before"
                  :owner "open" :note "private-v1540"})
(defn- after [b] (-> b (assoc :revision 1 :updated-at "after" :owner "helper")))
(defn- source [b]
  {:plan {:commands [(command/update-command table :claim b (after b) version)]
          :emit false}
   :protected-documents [{:target [table (:xt/id b)] :fields :all}]})
(defn- put! [doc] (xt/execute-tx @!node [[:put-docs table doc]]))
(defn- raw [id] (first (xt/q @!node ["SELECT * FROM gesso_v1540_acquired_probe WHERE _id = ?" id])))
(defn- prep [b] (acquired/prepare-acquired-complete-guard-one-shot! @!node (source b)))
(defn- code [f]
  (try (f) :no-error
       (catch Throwable e
         (if-let [reason (or (:reason (ex-data e))
                             (:xtdb.error/code (ex-data e)))]
           reason
           (throw e)))))
(defn- rejects! [expected f]
  (is (= expected (code f)) (str "Expected rejection: " expected)))
(defn- summary! [gate] ((:summary! gate)))
(defn- assert-nonclaims! [report]
  (doseq [[key expected]
          {:external-basis-authenticity :caller-node-unattested
           :transaction-internal-read-basis :not-authenticated
           :aba-history-detection :not-established
           :backend-assert-semantics :trusted-bounded-premise
           :physical-poststate-causality :not-established
           :complete-deployment-writer-coverage :not-established
           :principal-authorization :not-established
           :universal-physical-refinement :not-established
           :proof-status :externally-acquired-conditional-guard-only
           :certificate :not-issued
           :admission :not-admitted
           :authority :none}]
    (is (= expected (get report key))
        (str "Unexpected assurance promotion at " key)))
  (is (not-any? #(contains? report %)
                [:source :plan :node :before :after :physical-state
                 :snapshot-token :transaction-key :tx-key :tx-ops])))

(deftest same-node-physical-before-is-observed-before-gateway-is-exposed
  (let [b (before) _ (put! b) g (prep b) s ((:summary! g))
        ret ((:submit! g))]
    (is (= acquired/acquired-type (:type g)))
    (is (= acquired/acquired-version (:version g)))
    (is (= :same-node-pinned-read-matched-at-preparation (:external-before s)))
    (is (= :prepared (:status s)))
    (is (true? (:source-before-matched-external-read? s)))
    (is (= :externally-acquired-conditional-guard-only (:proof-status s)))
    (is (= :not-authenticated (:transaction-internal-read-basis s)))
    (is (= :not-established (:physical-poststate-causality s)))
    (is (= :not-established (:complete-deployment-writer-coverage s)))
    (is (not (str/includes? (pr-str s) (:xt/id b))))
    (is (not (str/includes? (pr-str s) "private-v1540")))
    (is (= :returned-transaction-key (:status ((:summary! g)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! g)))))
    (is (some? (:process-local-attempt-id
                (gateway/require-local-return! (:gateway g) ret))))
    (is (= (after b) (raw (:xt/id b))))
    (is (= :one-shot-already-attempted (code (:submit! g))))))

(deftest missing-record-cannot-be-used-to-issue-a-claim
  (let [b (before)]
    (is (= :missing-or-ambiguous-external-document
           (code #(prep b))))
    (is (nil? (raw (:xt/id b))))))

(deftest forged-caller-before-is-rejected-before-capability
  (let [b (before) _ (put! b)
        forged (assoc b :note "forged")]
    (is (= :source-before-disagrees-with-external-read
           (code #(prep forged))))
    (is (= b (raw (:xt/id b))))))

(deftest caller-before-must-include-every-external-field
  (let [b (before) _ (put! (assoc b :intruder 42))]
    (is (= :source-before-disagrees-with-external-read
           (code #(prep b))))))

(deftest source-with-absent-external-field-is-rejected
  (let [b (before) _ (put! (dissoc b :note))]
    (is (= :source-before-disagrees-with-external-read
           (code #(prep b))))))

(deftest fields-changing-after-pinned-read-must-fail-at-transaction
  (let [b (before) _ (put! b) g (prep b)
        outside (assoc b :note "outside")]
    (put! outside)
    (is (= :xtdb/assert-failed (code (:submit! g))))
    (is (= outside (raw (:xt/id b))))
    (is (= :threw-outcome-unknown (:status ((:summary! g)))))
    (is (= :one-shot-already-attempted (code (:submit! g))))))

(deftest late-field-addition-is-rejected
  (let [b (before) _ (put! b) g (prep b)
        outside (assoc b :intruder 20)]
    (put! outside)
    (is (= :xtdb/assert-failed (code (:submit! g))))
    (is (= outside (raw (:xt/id b))))))

(deftest late-field-removal-is-rejected
  (let [b (before) _ (put! b) g (prep b)
        outside (dissoc b :note)]
    (put! outside)
    (is (= :xtdb/assert-failed (code (:submit! g))))
    (is (= outside (raw (:xt/id b))))))

(deftest aba-restoration-still-possible
  (let [b (before) _ (put! b) g (prep b)]
    (put! (assoc b :note "changed"))
    (put! b)
    (is (= (after b) (do ((:submit! g)) (raw (:xt/id b)))))
    (is (= :not-established (:aba-history-detection ((:summary! g)))))))

(deftest malformed-plan-and-node-rejection-are-fail-closed
  (let [b (before) _ (put! b) s (source b)]
    (is (= :missing-xtdb-node
           (code #(acquired/prepare-acquired-complete-guard-one-shot! nil s))))
    (is (= :unsupported-plan-scope
           (code #(acquired/prepare-acquired-complete-guard-one-shot! @!node
                     (assoc-in s [:plan :commands] [])))))
    (is (= :unsupported-single-document-update
           (code #(acquired/prepare-acquired-complete-guard-one-shot! @!node
                     (assoc-in s [:plan :commands 0 :model/entity-type]
                               (keyword "untrusted-table-name;DROP"))))))))

(deftest supplied-foreign-node-can-never-claim-provenance-to-actual-deployment
  (let [b (before) _ (put! b) g (prep b)]
    (is (= :caller-node-unattested
           (:external-basis-authenticity ((:summary! g)))))
    (is (= :not-established
           (:principal-authorization ((:summary! g)))))
    (is (= :not-established
           (:universal-physical-refinement ((:summary! g)))))))

(deftest ordinary-xtdb-read-seam-substitution-rejects
  (let [b (before) _ (put! b)]
    (with-redefs [xt/q (fn [& _] [{:snapshot-token "forged"}])]
      (is (= :substituted-acquisition-or-execution-seam
             (code #(prep b)))))))

(deftest broken-node-read-rejects-before-submission-capability-escapes
  (let [b (before)]
    (is (= :external-snapshot-unavailable
           (code #(acquired/prepare-acquired-complete-guard-one-shot!
                   (Object.) (source b)))))))

(deftest foreign-node-does-not-impersonate-the-original-database
  (let [b (before) _ (put! b)]
    (with-open [foreign (xtn/start-node {})]
      (is (= :missing-or-ambiguous-external-document
             (code #(acquired/prepare-acquired-complete-guard-one-shot!
                     foreign (source b))))))))


(deftest acquired-summary-does-not-promote-unproved-properties
  (let [b (before) _ (put! b) g (prep b)
        pre (summary! g)
        ret ((:submit! g))
        post (summary! g)]
    (assert-nonclaims! pre)
    (assert-nonclaims! post)
    (is (= :one-update-acquired-external-before-and-complete-cas
           (:scope pre)))
    (is (= :canonical-complete-top-level-cas
           (:transaction-time-preconditions pre)))
    (is (= :returned-transaction-key (:status post)))
    (is (some? (:tx-key ret)))
    (doseq [report [pre post]]
      (is (not (str/includes? (pr-str report) (:xt/id b))))
      (is (not (str/includes? (pr-str report) "private-v1540"))))))

(deftest physical-before-matches-all-fields-not-only-version
  (let [b (before)
        _ (put! (assoc b :owner "another-helper"))]
    (rejects! :source-before-disagrees-with-external-read #(prep b))
    (is (= "another-helper" (:owner (raw (:xt/id b)))))))

(deftest physical-before-matches-created-at-and-revision
  (let [b (before)
        _ (put! (assoc b :created-at "changed-created-at"))]
    (rejects! :source-before-disagrees-with-external-read #(prep b)))
  (let [b (before)
        _ (put! (assoc b :revision 9))]
    (rejects! :source-before-disagrees-with-external-read #(prep b))))

(deftest missing-field-and-new-field-cannot-cancel-each-other
  (let [b (before)
        changed (-> b (dissoc :note)
                    (assoc :replacement-note "same-cardinality"))
        _ (put! changed)]
    (is (= (count b) (count changed)))
    (rejects! :source-before-disagrees-with-external-read #(prep b))
    (is (= changed (raw (:xt/id b))))))

(deftest omitted-id-and-mismatched-target-are-rejected
  (let [b (before) _ (put! b) s (source b)]
    (rejects! :unsupported-single-document-update
              #(acquired/prepare-acquired-complete-guard-one-shot!
                @!node (assoc-in s [:plan :commands 0 :model/before :xt/id]
                                 (str "foreign-" (random-uuid)))))
    (rejects! :unsupported-single-document-update
              #(acquired/prepare-acquired-complete-guard-one-shot!
                @!node (assoc-in s [:protected-documents 0 :target]
                                 [table (str "foreign-" (random-uuid))])))))

(deftest root-and-plan-shape-must-remain-exact
  (let [b (before) _ (put! b) s (source b)]
    (doseq [bad [nil {} [] (assoc s :unadmitted true)
                 (dissoc s :protected-documents)
                 (with-meta s {:certificate :forged})]]
      (rejects! :invalid-gateway-source
                #(acquired/prepare-acquired-complete-guard-one-shot! @!node bad)))
    (doseq [bad [(assoc-in s [:plan :assertions] [])
                 (assoc-in s [:plan :changes] [])
                 (assoc-in s [:plan :emit] true)
                 (assoc-in s [:plan :commands] [])
                 (update-in s [:plan :commands] conj
                            (first (get-in s [:plan :commands])))]]
      (rejects! :unsupported-plan-scope
                #(acquired/prepare-acquired-complete-guard-one-shot! @!node bad)))))

(deftest protected-scope-cannot-be-downgraded
  (let [b (before) _ (put! b) s (source b)]
    (doseq [value [#{:revision} [:owner] :none nil]]
      (rejects! :unsupported-single-document-update
                #(acquired/prepare-acquired-complete-guard-one-shot!
                  @!node (assoc-in s [:protected-documents 0 :fields] value))))
    (rejects! :unsupported-plan-scope
              #(acquired/prepare-acquired-complete-guard-one-shot!
                @!node (assoc s :protected-documents [])))))

(deftest mutable-and-opaque-prestate-values-do-not-become-guards
  (let [b (before) _ (put! b) s (source b)]
    (doseq [value [nil {:nested 1} [1 2] (java.util.ArrayList.)
                   (byte-array [1 2 3])]]
      (rejects! :unsupported-single-document-update
                #(acquired/prepare-acquired-complete-guard-one-shot!
                  @!node (-> s
                             (assoc-in [:plan :commands 0 :model/before :note]
                                       value)
                             (assoc-in [:plan :commands 0 :model/after :note]
                                       value)))))))

(deftest source-map-edits-after-preparation-do-not-replace-captured-operation
  (let [b (before) _ (put! b)
        s (source b)
        g (acquired/prepare-acquired-complete-guard-one-shot! @!node s)
        alternate (assoc-in s [:plan :commands 0 :model/after :owner]
                            "other-user")]
    (is (= "other-user" (get-in alternate
                                 [:plan :commands 0 :model/after :owner])))
    (is (= "helper" (:owner (after b))))
    (is (some? (:tx-key ((:submit! g)))))
    (is (= (after b) (raw (:xt/id b))))))

(deftest late-version-change-is-detected-at-transaction-boundary
  (let [b (before) _ (put! b) g (prep b)
        other (assoc b :revision 1)]
    (put! other)
    (rejects! :xtdb/assert-failed (:submit! g))
    (is (= other (raw (:xt/id b))))
    (is (= :threw-outcome-unknown (:status (summary! g))))
    (rejects! :one-shot-already-attempted (:submit! g))))

(deftest late-same-count-field-swap-is-detected-at-transaction-boundary
  (let [b (before) _ (put! b) g (prep b)
        other (-> b (dissoc :note)
                    (assoc :other-note "replaced"))]
    (put! other)
    (is (= (count b) (count other)))
    (rejects! :xtdb/assert-failed (:submit! g))
    (is (= other (raw (:xt/id b))))))

(deftest two-acquired-gateways-competing-on-one-prestate
  (let [b (before) _ (put! b)
        g1 (prep b) g2 (prep b)
        release (promise)
        invoke (fn [g]
                 (future @release
                         (try {:outcome :returned :ret ((:submit! g))}
                              (catch Throwable e
                                {:outcome :threw
                                 :reason (:xtdb.error/code (ex-data e))}))))
        f1 (invoke g1) f2 (invoke g2)]
    (deliver release true)
    (let [results [(deref f1 30000 ::timeout)
                   (deref f2 30000 ::timeout)]]
      (is (not-any? #{::timeout} results))
      (is (= {:returned 1 :threw 1}
             (frequencies (map :outcome results))))
      (is (every? (fn [{:keys [outcome reason ret]}]
                    (if (= :returned outcome)
                      (some? (:tx-key ret))
                      (= :xtdb/assert-failed reason)))
                  results))
      (is (= (after b) (raw (:xt/id b))))
      (is (= #{:returned-transaction-key :threw-outcome-unknown}
             (set (map (comp :status summary!) [g1 g2]))))
      (rejects! :one-shot-already-attempted (:submit! g1))
      (rejects! :one-shot-already-attempted (:submit! g2)))))

(deftest external-writer-remains-possible-after-gateway-returns
  (let [b (before) _ (put! b) g (prep b)
        ret ((:submit! g))
        outside (assoc (after b) :note "outside-after-return")]
    (is (some? (:tx-key ret)))
    (put! outside)
    (is (= outside (raw (:xt/id b))))
    (is (= :returned-transaction-key (:status (summary! g))))
    (assert-nonclaims! (summary! g))))

(deftest changing-field-and-restoring-it-is-not-an-uninterrupted-history
  (let [b (before) _ (put! b) g (prep b)]
    (put! (assoc b :owner "temporary-owner"))
    (put! b)
    (is (= b (raw (:xt/id b))))
    (is (some? (:tx-key ((:submit! g)))))
    (is (= (after b) (raw (:xt/id b))))
    (is (= :not-established (:aba-history-detection (summary! g))))
    (assert-nonclaims! (summary! g))))

(deftest query-seam-replacement-rejects-without-reading-or-issuing-capability
  (let [b (before) _ (put! b)]
    (with-redefs [xt/q (fn [& _] (throw (ex-info "substituted query" {})))]
      (rejects! :substituted-acquisition-or-execution-seam #(prep b)))
    (is (= b (raw (:xt/id b))))))

(deftest decode-seam-replacement-rejects-before-read
  (let [b (before) _ (put! b)]
    (with-redefs [basis/<-time-basis-str (fn [_] {})]
      (rejects! :substituted-acquisition-or-execution-seam #(prep b)))))

(deftest strict-gateway-seam-replacement-rejects-before-read
  (let [b (before) _ (put! b)]
    (with-redefs [strict/prepare-complete-guard-one-shot!
                  (fn [& _] (throw (ex-info "substituted strict" {})))]
      (rejects! :substituted-acquisition-or-execution-seam #(prep b)))))

(deftest fake-node-cannot-issue-a-capability
  (let [b (before) _ (put! b)]
    (rejects! :external-snapshot-unavailable
              #(acquired/prepare-acquired-complete-guard-one-shot!
                (Object.) (source b)))))

(deftest foreign-node-even-with-matching-document-is-unattested
  (let [b (before) _ (put! b)]
    (with-open [foreign (xtn/start-node {})]
      (xt/execute-tx foreign [[:put-docs table b]])
      (let [g (acquired/prepare-acquired-complete-guard-one-shot!
               foreign (source b))]
        (is (= :caller-node-unattested
               (:external-basis-authenticity (summary! g))))
        (is (= :prepared (:status (summary! g))))
        (is (some? (:tx-key ((:submit! g)))))
        (is (= b (raw (:xt/id b))))
        (assert-nonclaims! (summary! g))))))

(deftest no-preflight-rejection-exposes-an-executable-capability
  (let [b (before)
        bad (assoc-in (source b) [:plan :emit] true)]
    (rejects! :unsupported-plan-scope
              #(acquired/prepare-acquired-complete-guard-one-shot! @!node bad))
    (is (nil? (raw (:xt/id b))))))

(deftest preparation-read-does-not-authenticate-a-principal
  (let [b (before) _ (put! b) g (prep b)]
    (is (= :none (:authority (summary! g))))
    (is (= :not-admitted (:admission (summary! g))))
    (is (= :not-established (:principal-authorization (summary! g))))
    (is (= :not-authenticated (:transaction-internal-read-basis (summary! g))))
    (is (= :trusted-bounded-premise (:backend-assert-semantics (summary! g))))))

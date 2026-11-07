(ns gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution-test
  "v1548: Permanent adversarial embedded-XTDB tests for the opt-in v1547
   monotonic revision execution gateway. These tests separately challenge the
   admission policy, its acquired physical before-image, transaction-time
   complete-field assertions, one-shot submission, and the deliberately
   conditional claim about stale-version ABA among compliant writers.

   Direct XTDB writers remain outside the monotonic policy. The observed
   tests cannot certify deployment-wide writer closure, backend semantics,
   transaction-internal history, poststate causality, principal authorization
   or universal refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-acquired-complete-guard-execution
             :as acquired]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution
             :as monotonic]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway
             :as gateway]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [xtdb.api TransactionKey]))

(defonce ^:private !node (atom nil))
(def ^:private table :gesso_v1548_monotonic_probe)
(def ^:private version
  {:revision-key :revision :created-at-key :created-at
   :updated-at-key :updated-at})

(defn- with-node [f]
  (with-open [node (xtn/start-node {})]
    (reset! !node node)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)

(defn- before
  ([] (before 0))
  ([revision]
   {:xt/id (str "v1548-" (random-uuid))
    :revision revision :created-at "created" :updated-at "before"
    :owner "open" :secret "private-v1548-boundary-marker"}))

(defn- after [b]
  (-> b
      (update :revision inc)
      (assoc :updated-at (str "at-" (inc (:revision b)))
             :owner (if (= "open" (:owner b)) "claimed" "open"))))

(defn- source [b]
  {:plan {:commands [(command/update-command table :claim b (after b) version)]
          :emit false}
   :protected-documents [{:target [table (:xt/id b)] :fields :all}]})

(defn- put! [document]
  (xt/execute-tx @!node [[:put-docs table document]]))

(defn- physical [id]
  (first (xt/q @!node ["SELECT * FROM gesso_v1548_monotonic_probe WHERE _id = ?" id])))

(defn- prepare
  ([s] (prepare s :revision))
  ([s revision-field]
   (monotonic/prepare-monotonic-revision-one-shot!
    @!node s revision-field)))

(defn- rejection [f]
  (try (f) :no-error
       (catch Throwable e
         (or (:reason (ex-data e))
             (:xtdb.error/code (ex-data e))
             (throw e)))))

(defn- reject! [code f]
  (is (= code (rejection f)) (str "Expected rejection: " code)))

(defn- summary! [g] ((:summary! g)))

(defn- assert-nonclaims! [report]
  (doseq [[k v] {:revision-policy :source-checked-exactly-one-nonwrapping-successor
                 :original-revision-version-guard :required-and-source-matched
                 :external-prestate :same-node-pinned-read-matched-at-preparation
                 :transaction-time-revision-guard :complete-top-level-cas
                 :cooperating-writer-revision-aba
                 :excluded-only-under-monotonic-complete-writer-premise
                 :monotonic-complete-writer-premise :not-established
                 :external-writer-aba :not-excluded
                 :aba-history-detection :not-established
                 :complete-deployment-writer-coverage :not-established
                 :transaction-internal-prestate :not-authenticated
                 :backend-assert-semantics :trusted-bounded-premise
                 :caller-node-authenticity :not-established
                 :physical-poststate-causality :not-established
                 :principal-authorization :not-established
                 :universal-physical-refinement :not-established
                 :proof-status :conditional-revision-guard-only
                 :certificate :not-issued
                 :admission :not-admitted
                 :authority :none}]
    (is (= v (get report k)) (str "Unexpected claim at " k)))
  (is (not-any? #(contains? report %)
                [:source :plan :before :after :node :id :tx-ops :tx-key
                 :snapshot-token :physical-state :document]))
  (is (not (str/includes? (pr-str report)
                          "private-v1548-boundary-marker"))))

(deftest authentic-one-shot-submission-advances-revision
  (let [b (before) _ (put! b) g (prepare (source b))
        first-report (summary! g) response ((:submit! g))
        final-report (summary! g)]
    (is (= monotonic/monotonic-type (:type g)))
    (is (= monotonic/monotonic-version (:version g)))
    (is (= :prepared (:status first-report)))
    (assert-nonclaims! first-report)
    (is (instance? TransactionKey (:tx-key response)))
    (is (= :public-transaction-key
           (:observed-result-kind
            (gateway/require-local-return! (:gateway g) response))))
    (is (= (after b) (physical (:xt/id b))))
    (is (= :returned-transaction-key (:status final-report)))
    (is (= 1 (:owned-execute-tx-call-entries final-report)))
    (assert-nonclaims! final-report)
    (reject! :one-shot-already-attempted (:submit! g))))

(deftest normal-gateway-return-cannot-be-substituted-by-copy
  (let [b (before) _ (put! b) g (prepare (source b))
        response ((:submit! g))
        copied (into {} response)]
    (is (instance? TransactionKey (:tx-key response)))
    (is (not (identical? response copied)))
    (is (map? (gateway/require-local-return! (:gateway g) response)))
    (is (not= :no-error
              (rejection #(gateway/require-local-return! (:gateway g) copied))))))

(deftest configured-revision-field-can-be-customized
  (let [b (-> (before) (assoc :sequence 0) (dissoc :revision))
        config (assoc version :revision-key :sequence)
        a (-> b (assoc :sequence 1 :updated-at "changed" :owner "claimed"))
        s {:plan {:commands [(command/update-command table :claim b a config)]
                  :emit false}
           :protected-documents [{:target [table (:xt/id b)] :fields :all}]}
        _ (put! b) g (prepare s :sequence)]
    (is (instance? TransactionKey (:tx-key ((:submit! g)))))
    (is (= a (physical (:xt/id b))))
    (assert-nonclaims! (summary! g))))

(deftest missing-record-prevents-capability-issuance
  (let [b (before)]
    (reject! :missing-or-ambiguous-external-document
             #(prepare (source b)))
    (is (nil? (physical (:xt/id b))))))

(deftest forged-before-is-rejected-against-pinned-external-read
  (let [b (before) _ (put! b)
        forged (assoc-in (source b)
                         [:plan :commands 0 :model/before :secret] "forged")]
    (reject! :source-before-disagrees-with-external-read
             #(prepare forged))
    (is (= b (physical (:xt/id b))))))

(deftest missing-or-extra-before-fields-must-not-disappear-at-acquisition
  (let [b (before) _ (put! (assoc b :external-field "unclaimed"))]
    (reject! :source-before-disagrees-with-external-read
             #(prepare (source b)))
    (is (= "unclaimed" (:external-field (physical (:xt/id b)))))))

(deftest only-one-exact-old-revision-version-guard-is-allowed
  (let [b (before) s (source b)
        root [:plan :commands 0 :model/expected :model/checks]]
    (doseq [[checks reason] [[[[:updated-at "before"]]
                             :revision-not-originally-version-guarded]
                            [[[:revision 1] [:updated-at "before"]]
                             :revision-not-originally-version-guarded]
                            [[[:revision 0] [:revision 0] [:updated-at "before"]]
                             :revision-not-exact-successor]]]
      ;; Duplicate keys fail even earlier in canonical command validation.
      (reject! reason #(prepare (assoc-in s root checks))))))

(deftest original-version-checks-cannot-be-made-list-shaped
  (let [s (source (before))
        checks (get-in s [:plan :commands 0 :model/expected :model/checks])]
    ;; Canonical command validation rejects a list before the monotonic
    ;; gateway's own vector-only check is reached.
    (reject! :revision-not-exact-successor
             #(prepare (assoc-in s [:plan :commands 0 :model/expected :model/checks]
                                 (apply list checks))))))

(deftest successor-must-be-exact-and-not-skipped
  (let [s (source (before))]
    (doseq [value [-1 0 2 3 1.0 1N 1M "1" nil true false]]
      (reject! :revision-not-exact-successor
               #(prepare (assoc-in s [:plan :commands 0 :model/after :revision]
                                   value))))))

(deftest old-revision-must-be-an-exact-nonnegative-bounded-integer
  (let [s (source (before))]
    (doseq [value [-1 -2 0.5 1N 1M "0" nil true false
                   Long/MAX_VALUE (bigint Long/MAX_VALUE)]]
      (reject! :revision-not-exact-successor
               #(prepare (assoc-in s [:plan :commands 0 :model/before :revision]
                                   value))))))

(deftest missing-before-or-after-revision-rejected
  (let [s (source (before))]
    (reject! :revision-not-exact-successor
             #(prepare (update-in s [:plan :commands 0 :model/before]
                                  dissoc :revision)))
    (reject! :revision-not-exact-successor
             #(prepare (update-in s [:plan :commands 0 :model/after]
                                  dissoc :revision)))))

(deftest revision-field-name-is-restricted
  (let [s (source (before))]
    (doseq [field [nil :xt/id :model/revision :REV :has_underscore
                   "revision" (keyword (str "a" (apply str (repeat 65 "b"))))]]
      (reject! :unsupported-revision-field
               #(prepare s field)))
    (reject! :revision-not-exact-successor
             #(prepare s :unknown))))

(deftest exact-plan-shape-and-ordinary-data-structures-are-mandatory
  (let [s (source (before))]
    (doseq [candidate [nil {} [] (assoc s :extra true)
                       (dissoc s :protected-documents)
                       (assoc-in s [:plan :emit] true)
                       (assoc-in s [:plan :commands] [])
                       (assoc-in s [:plan :commands] '())
                       (update-in s [:plan :commands] conj
                                  (first (get-in s [:plan :commands])))
                       (with-meta s {:trusted true})
                       (update s :plan with-meta {:trusted true})
                       (update-in s [:plan :commands] with-meta {:trusted true})]]
      (reject! :unsupported-revision-plan
               #(prepare candidate)))))

(deftest immutable-command-shape-is-required
  (let [s (source (before))]
    (doseq [candidate [(update-in s [:plan :commands 0]
                                  with-meta {:trusted true})
                       (assoc-in s [:plan :commands 0] {})
                       (assoc-in s [:plan :commands 0 :model/before]
                                 (java.util.HashMap.))]]
      (reject! :revision-not-exact-successor #(prepare candidate)))))

(deftest external-node-cannot-be-missing
  (let [s (source (before))]
    (reject! :missing-xtdb-node
             #(monotonic/prepare-monotonic-revision-one-shot! nil s :revision))))

(deftest swapped-revision-check-with-incorrect-value-is-not-accepted
  (let [s (source (before))
        guards [:plan :commands 0 :model/expected :model/checks]]
    (reject! :revision-not-originally-version-guarded
             #(prepare (assoc-in s guards [[:revision 99]])))))

(deftest malformed-after-is-not-a-way-around-revision-admission
  (let [s (source (before))]
    (doseq [after-value [nil [] {:revision 1} (java.util.HashMap.)]]
      (reject! :revision-not-exact-successor
               #(prepare (assoc-in s [:plan :commands 0 :model/after]
                                   after-value))))))

(deftest substituted-command-validator-is-rejected-before-acquisition
  (let [s (source (before))]
    (with-redefs [command/update? (constantly true)]
      (reject! :substituted-revision-execution-seam #(prepare s)))))

(deftest substituted-acquired-gateway-is-rejected-before-acquisition
  (let [s (source (before))]
    (with-redefs [acquired/prepare-acquired-complete-guard-one-shot!
                  (fn [& _] {:type acquired/acquired-type
                             :version acquired/acquired-version
                             :submit! (fn [] :forged)
                             :summary! (fn [] {:status :prepared})})]
      (reject! :substituted-revision-execution-seam #(prepare s)))))

(deftest substituted-xtdb-query-function-cannot-fabricate-prestate
  (let [b (before) _ (put! b)]
    (with-redefs [xt/q (fn [& _] [b])]
      (reject! :substituted-acquisition-or-execution-seam
               #(prepare (source b))))))

(deftest independent-writer-field-change-rejected-at-submit
  (let [b (before) _ (put! b) g (prepare (source b))
        outside (assoc b :secret "outside")]
    (put! outside)
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= outside (physical (:xt/id b))))
    (is (= :threw-outcome-unknown (:status (summary! g))))
    (is (= 1 (:owned-execute-tx-call-entries (summary! g))))
    (reject! :one-shot-already-attempted (:submit! g))
    (assert-nonclaims! (summary! g))))

(deftest independent-writer-field-addition-rejected-by-complete-guards
  (let [b (before) _ (put! b) g (prepare (source b))
        outside (assoc b :unexpected "outside")]
    (put! outside)
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= outside (physical (:xt/id b))))))

(deftest independent-writer-field-removal-rejected-by-complete-guards
  (let [b (before) _ (put! b) g (prepare (source b))
        outside (dissoc b :secret)]
    (put! outside)
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= outside (physical (:xt/id b))))))

(deftest independent-writer-swapped-field-with-same-count-is-still-rejected
  (let [b (before) _ (put! b) g (prepare (source b))
        outside (-> b (dissoc :secret) (assoc :unexpected "outside"))]
    (is (= (count b) (count outside)))
    (put! outside)
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= outside (physical (:xt/id b))))))

(deftest two-competing-prepared-gateways-do-not-both-advance-same-revision
  (let [b (before) _ (put! b)
        g1 (prepare (source b)) g2 (prepare (source b))]
    (is (instance? TransactionKey (:tx-key ((:submit! g1)))))
    (reject! :xtdb/assert-failed (:submit! g2))
    (is (= 1 (:revision (physical (:xt/id b)))))
    (is (= (after b) (physical (:xt/id b))))
    (reject! :one-shot-already-attempted (:submit! g1))
    (reject! :one-shot-already-attempted (:submit! g2))))

(deftest concurrent-gateways-allow-only-one-guarded-winner
  (let [b (before) _ (put! b)
        gates [(prepare (source b)) (prepare (source b))]
        go (promise)
        workers (mapv (fn [g] (future @go
                                   (try {:outcome :returned
                                         :result ((:submit! g))}
                                        (catch Throwable e
                                          {:outcome :rejected
                                           :reason (or (:xtdb.error/code (ex-data e))
                                                       (:reason (ex-data e)))}))))
                      gates)]
    (deliver go true)
    (let [results (mapv #(deref % 45000 ::timeout) workers)]
      (is (not-any? #{::timeout} results))
      (is (= {:returned 1 :rejected 1}
             (frequencies (map :outcome results))))
      (is (every? (fn [r] (if (= :returned (:outcome r))
                           (instance? TransactionKey (get-in r [:result :tx-key]))
                           (= :xtdb/assert-failed (:reason r))))
                  results))
      (is (= 1 (:revision (physical (:xt/id b)))))
      (is (= #{:returned-transaction-key :threw-outcome-unknown}
             (set (map (comp :status summary!) gates))))
      (doseq [g gates]
        (reject! :one-shot-already-attempted (:submit! g))))))

(deftest compliant-writer-data-aba-does-not-reuse-an-old-revision
  (let [b (before) _ (put! b)
        stale (prepare (source b))
        g1 (prepare (source b))
        _ ((:submit! g1))
        first-after (physical (:xt/id b))
        g2 (prepare (source first-after))
        _ ((:submit! g2))
        later (physical (:xt/id b))]
    (is (= "open" (:owner later)))
    (is (= (:owner b) (:owner later)))
    (is (= 2 (:revision later)))
    (reject! :xtdb/assert-failed (:submit! stale))
    (is (= later (physical (:xt/id b))))
    (is (= :not-established
           (:monotonic-complete-writer-premise (summary! stale))))))

(deftest direct-xtdb-revision-rollback-defeats-compliant-writer-premise
  (let [b (before) _ (put! b)
        stale (prepare (source b))
        _ (put! (assoc b :revision 1 :secret "outside"))
        _ (put! b)
        returned ((:submit! stale))]
    (is (instance? TransactionKey (:tx-key returned)))
    (is (= (after b) (physical (:xt/id b))))
    (is (= :not-excluded (:external-writer-aba (summary! stale))))
    (is (= :not-established
           (:complete-deployment-writer-coverage (summary! stale))))
    (assert-nonclaims! (summary! stale))))

(deftest transient-physical-aba-remains-undetectable-even-with-complete-guards
  (let [b (before) _ (put! b) g (prepare (source b))]
    (put! (assoc b :secret "outsider"))
    (put! b)
    (is (instance? TransactionKey (:tx-key ((:submit! g)))))
    (is (= (after b) (physical (:xt/id b))))
    (is (= :not-established (:aba-history-detection (summary! g))))))

(deftest closure-of-physical-writers-cannot-be-proven-by-successful-submission
  (let [b (before) _ (put! b) g (prepare (source b))
        _ ((:submit! g))]
    (put! (assoc (after b) :secret "external-after-commit"))
    (is (= "external-after-commit" (:secret (physical (:xt/id b)))))
    (is (= :not-established
           (:physical-poststate-causality (summary! g))))
    (is (= :not-established
           (:complete-deployment-writer-coverage (summary! g))))
    (assert-nonclaims! (summary! g))))

(deftest conflicting-return-never-establishes-authoritative-commit
  (let [b (before) _ (put! b) g (prepare (source b))]
    (put! (assoc b :owner "outsider"))
    (reject! :xtdb/assert-failed (:submit! g))
    (is (= :threw-outcome-unknown (:status (summary! g))))
    (is (= :not-established
           (:universal-physical-refinement (summary! g))))
    (is (= :not-issued (:certificate (summary! g))))))

(deftest last-nonwrapping-successor-can-reach-long-max-value
  (let [b (before (dec Long/MAX_VALUE))
        _ (put! b)
        g (prepare (source b))
        returned ((:submit! g))]
    (is (instance? TransactionKey (:tx-key returned)))
    (is (= Long/MAX_VALUE (:revision (physical (:xt/id b)))))
    (reject! :one-shot-already-attempted (:submit! g))
    (assert-nonclaims! (summary! g))))

(deftest physical-protection-cannot-be-reduced-to-selected-fields
  (let [b (before) s (source b)]
    (doseq [bad [(assoc-in s [:protected-documents 0 :fields]
                          #{:revision :owner})
                 (assoc s :protected-documents [])
                 (update-in s [:protected-documents]
                            conj {:target [table (:xt/id b)] :fields :all})]]
      (is (not= :no-error (rejection #(prepare bad)))))))

(deftest an-unpopulated-foreign-node-cannot-impersonate-acquired-prestate
  (let [b (before) _ (put! b)]
    (with-open [foreign (xtn/start-node {})]
      (reject! :missing-or-ambiguous-external-document
               #(monotonic/prepare-monotonic-revision-one-shot!
                 foreign (source b) :revision))
      (is (= b (physical (:xt/id b)))))))

(deftest a-populated-foreign-node-is-not-automatically-the-deployed-authority
  (let [b (before) _ (put! b)]
    (with-open [foreign (xtn/start-node {})]
      (xt/execute-tx foreign [[:put-docs table b]])
      (let [g (monotonic/prepare-monotonic-revision-one-shot!
               foreign (source b) :revision)]
        (is (instance? TransactionKey (:tx-key ((:submit! g)))))
        (is (= b (physical (:xt/id b))))
        (is (= :not-established (:caller-node-authenticity (summary! g))))
        (is (= :not-established
               (:complete-deployment-writer-coverage (summary! g))))))))

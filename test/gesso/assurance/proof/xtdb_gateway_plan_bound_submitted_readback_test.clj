(ns gesso.assurance.proof.xtdb-gateway-plan-bound-submitted-readback-test
  "v1617: Permanent real-XTDB adversarial checks for v1616's opt-in
   certified-plan-bound atomic submission/return/readback bridge.

   A genuine certified-plan executor, Gesso-issued target callback and closed
   application fixture are used throughout. Positive results are strictly
   process-local return provenance plus bounded pinned physical co-observation;
   they do not certify physical causality, authorization, command arguments,
   outside-writer closure, or universal refinement. A readback failure AFTER
   submission never establishes rejection or grants a retry."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [gesso.assurance.proof.xtdb-gateway-atomic-submitted-readback :as sut]
            [gesso.assurance.proof.xtdb-gateway-atomic-readback :as readback]
            [gesso.assurance.proof.xtdb-gateway-monotonic-revision-execution :as mono]
            [gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
             :as fixture]
            [gesso.choreo.identity :as identity]
            [gesso.model.command-resolution :as resolution]
            [xtdb.api :as xt])
  (:import [xtdb.api TransactionKey]))

(def ^:private operation :example/claim)
(def ^:private request {:request-method :post :uri "/operations/example/claim"})
(def ^:private private-id "v1587-private-physical-target")
(def ^:private private-note "v1587-secret-must-not-be-disclosed")
(def ^:private private-outcome "v1617-secret-outcome-must-not-leak")
(def ^:private report-keys
  #{:type :version :status :process-local-return :submitted-gateway
    :readback-node-and-inputs :readback-status :domain-poststate-status
    :command-record-status :ledger-status :basis-order-status
    :three-observations-share-pinned-basis? :public-key-origin
    :backend-node-authenticity :transaction-causality :record-immutability
    :principal-and-command-authenticity :external-writer-closure
    :universal-refinement :retry-authorized? :live-settlement-authority
    :transaction-authority :certificate :admission :authority :proof-status})

;; Reuse a genuine certified plan and an embedded XTDB node. The fixture's
;; helpers are intentionally private to the test namespace, not production APIs.
(defn- helper [sym]
  (let [v (ns-resolve 'gesso.assurance.proof.xtdb-gateway-plan-bound-guarded-prestate-test
                      sym)]
    (when-not v
      (throw (ex-info "Required certified XTDB fixture is absent"
                      {:reason :missing-certified-xtdb-fixture})))
    (var-get v)))
(defn- node [] (helper '*node*))
(defn- certified [] (helper '*case*))
(defn- before [] ((helper 'before)))
(defn- after [b] ((helper 'after) b))
(defn- candidate [b] ((helper 'candidate) b))
(defn- put! [b] ((helper 'put!) b))
(defn- physical [] ((helper 'physical)))

(use-fixtures :once (fn [f] ((helper 'with-certified-xtdb) f)))

(defn- input
  ([] (input (str (random-uuid))))
  ([id]
   {:binding {:command-id (identity/command-id id)
              :principal (identity/principal "helper-v1617")
              :operation operation
              :arguments {:request-id private-id}}
    :outcome {:status :claimed :detail private-outcome}
    :now "2026-11-07T00:00:00Z"}))
(defn- record [incorporation]
  (first (xt/q (node)
               ["SELECT * FROM gesso_commands WHERE _id = ?"
                (resolution/record-id (:binding incorporation))])))
(defn- expected-record [incorporation]
  (resolution/command-record (:binding incorporation)
                             (:outcome incorporation) (:now incorporation)))
(defn- prepare! [source incorporation]
  (let [{:keys [contract issuance]} (certified)]
    (sut/prepare-plan-bound-atomic-return-readback-one-shot!
     (node) contract issuance operation request source :revision incorporation)))
(defn- stage! []
  (let [b (before)
        i (input)]
    (put! b)
    {:before b :input i :source (candidate b)
     :gateway (prepare! (candidate b) i)}))
(defn- code-of [f]
  (try (f) :not-rejected
       (catch Throwable e
         (if-let [code (or (:reason (ex-data e))
                           (:xtdb.error/code (ex-data e)))]
           (do
             (when (:xtdb-gateway-atomic-submitted-readback/rejection (ex-data e))
               (is (= #{:xtdb-gateway-atomic-submitted-readback/rejection :reason}
                      (set (keys (ex-data e))))
                   "The wrapper's typed rejections must not leak input state"))
             code)
           (throw e)))))
(defn- rejects! [expected f]
  (is (= expected (code-of f)) (str "Expected typed rejection " expected)))
(defn- no-authority! [r]
  (is (= sut/report-type (:type r)))
  (is (= sut/report-version (:version r)))
  (is (= report-keys (set (keys r)))
      "Readback reports must not acquire private fields or new authority")
  (doseq [[k expected] {:submitted-gateway
                        :certified-plan-bound-monotonic-atomic-two-write-one-shot
                        :process-local-return :exact-object-from-this-issued-submission
                        :public-key-origin :process-local-submission-return
                        :readback-node-and-inputs :same-captured-submission-inputs
                        :three-observations-share-pinned-basis? true
                        :transaction-causality :not-established
                        :backend-node-authenticity :caller-supplied-unattested
                        :record-immutability :not-established
                        :principal-and-command-authenticity :application-premise
                        :external-writer-closure :not-established
                        :universal-refinement :not-established
                        :retry-authorized? false
                        :live-settlement-authority :none
                        :transaction-authority :none
                        :certificate :not-issued
                        :admission :not-admitted
                        :authority :none
                        :proof-status :bounded-process-local-return-and-pinned-coobservation}]
    (is (= expected (get r k)) (str "Do not overstate assurance: " k)))
  (let [printed (pr-str r)]
    (is (not (str/includes? printed private-id)))
    (is (not (str/includes? printed private-note)))
    (is (not (str/includes? printed private-outcome)))
    (is (not (re-find #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
                      printed)))))
(defn- consumed! [gateway]
  (rejects! :one-shot-already-attempted (:submit! gateway))
  (is (= :permit-consumed (:submission-status ((:summary! gateway))))))

(deftest only-one-plan-bound-two-write-gateway-is-prepared
  (let [{:keys [before input gateway]} (stage!)
        s ((:summary! gateway))]
    (is (= #{:type :version :submit! :observe-return! :summary!}
           (set (keys gateway))))
    (is (= sut/plan-bound-gateway-type (:type gateway) (:type s)))
    (is (= sut/plan-bound-gateway-version (:version gateway) (:version s)))
    (is (= :prepared (:submission-status s)))
    (is (= :not-observed (:readback-status s)))
    (is (= :one-certified-plan-bound-monotonic-atomic-two-write-gateway
           (:underlying-submission s)))
    (is (= :requires-exact-local-return-object (:public-key-origin s)))
    (is (= :same-captured-node-and-source (:readback-identity s)))
    (is (= :not-established (:transaction-causality s)))
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) {:tx-key nil}))
    (is (= before (physical)))
    (is (nil? (record input)))))

(deftest successful-incorporation-and-single-basis-readback
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        report ((:observe-return! gateway) response)]
    (is (instance? TransactionKey (:tx-key response)))
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status report)))
    (is (= :bounded-poststate-command-and-ledger-coobservation
           (:readback-status report)))
    (is (= :matches-source-after (:domain-poststate-status report)))
    (is (= :matches-canonical-record (:command-record-status report)))
    (is (= :matching-committed-ledger-row (:ledger-status report)))
    (no-authority! report)
    (is (= (:status report) (:readback-status ((:summary! gateway)))))
    (consumed! gateway)
    (rejects! :readback-already-attempted
              #((:observe-return! gateway) response))))

(deftest return-must-be-identical-to-this-gateways-submission
  (let [{g1 :gateway} (stage!)
        response ((:submit! g1))
        copied (into {} response)
        foreign-key (xt/execute-tx
                     (node) [[:put-docs :gesso_v1617_foreign
                              {:xt/id (str (random-uuid)) :sentinel :foreign}]])
        forged (assoc response :tx-key foreign-key)]
    (is (= response copied))
    (is (not (identical? response copied)))
    (is (instance? TransactionKey foreign-key))
    (is (not= foreign-key (:tx-key response)))
    (doseq [other [nil copied forged foreign-key]]
      (rejects! :not-this-submission-local-return
                #((:observe-return! g1) other)))
    (is (= :not-observed (:readback-status ((:summary! g1)))))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status ((:observe-return! g1) response))))))

(deftest a-separate-issued-gateway-cannot-read-another-return
  (let [{g1 :gateway} (stage!)
        response ((:submit! g1))
        {g2 :gateway before2 :before input2 :input} (stage!)]
    (rejects! :not-this-submission-local-return
              #((:observe-return! g2) response))
    (is (= :not-observed (:readback-status ((:summary! g2)))))
    (is (= before2 (physical)))
    (is (nil? (record input2)))
    (is (= :bounded-local-return-and-pinned-readback-coobservation
           (:status ((:observe-return! g2) ((:submit! g2))))))))

(deftest forged-target-operation-and-prestate-reject-before-authority
  (let [b (before)
        i (input)
        source (candidate b)
        {:keys [contract issuance]} (certified)]
    (put! b)
    (rejects! :unissued-or-changed-target-resolver
              #(sut/prepare-plan-bound-atomic-return-readback-one-shot!
                (node) contract {:expected-target! (fn [_] [:requests private-id])}
                operation request source :revision i))
    (rejects! :incorporation-binding-operation-disagrees-with-route
              #(prepare! source (assoc-in i [:binding :operation] :example/cancel)))
    (rejects! :command-or-protection-disagrees-with-plan-target
              #(prepare! (assoc-in source [:plan :commands 0 :model/operation]
                                          :cancel) i))
    (rejects! :command-or-protection-disagrees-with-plan-target
              #(prepare! (assoc-in source [:protected-documents 0 :target]
                                          [:requests "wrong-target"]) i))
    (rejects! :incorporation-before-disagrees-with-external-read
              #(prepare! (assoc-in source [:plan :commands 0 :model/before :note]
                                          "forged-before") i))
    (is (= b (physical)))
    (is (nil? (record i)))))

(deftest exact-successor-monotonic-revision-remains-required
  (let [b (before) i (input) source (candidate b)]
    (put! b)
    (rejects! :revision-not-exact-successor
              #(prepare! (assoc-in source [:plan :commands 0 :model/after :revision]
                                          9) i))
    (is (= b (physical)))
    (is (nil? (record i)))))

(deftest same-revision-intervening-write-blocks-both-physical-writes
  (let [{:keys [before input gateway]} (stage!)
        changed (assoc before :note "intervening-same-revision")]
    (put! changed)
    (rejects! :xtdb/assert-failed (:submit! gateway))
    (is (= changed (physical)))
    (is (nil? (record input)))
    (consumed! gateway)
    (rejects! :not-this-submission-local-return
              #((:observe-return! gateway) {:tx-key nil}))))

(deftest duplicate-command-id-does-not-admit-another-domain-write
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))]
    (is (instance? TransactionKey (:tx-key response)))
    (is (= (expected-record input) (record input)))
    (let [fresh (prepare! (candidate (physical)) input)]
      (rejects! :xtdb/assert-failed (:submit! fresh))
      (is (= (after before) (physical)))
      (is (= (expected-record input) (record input)))
      (consumed! fresh))))

(deftest later-domain-mutation-prevents-positive-readback
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        outside (assoc before :revision 17 :owner "external")]
    (put! outside)
    (let [report ((:observe-return! gateway) response)]
      (is (= :postsubmission-correspondence-not-established (:status report)))
      (is (= :diverges (:domain-poststate-status report)))
      (no-authority! report)
      (is (= outside (physical)))
      (is (= (expected-record input) (record input))))))

(deftest later-command-record-mutation-prevents-positive-readback
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))
        outside (assoc (expected-record input) :foreign "outside-change")]
    (xt/execute-tx (node) [[:put-docs resolution/command-entity-type outside]])
    (let [report ((:observe-return! gateway) response)]
      (is (= :postsubmission-correspondence-not-established (:status report)))
      (is (not= :matches-canonical-record (:command-record-status report)))
      (no-authority! report)
      (is (= (after before) (physical)))
      (is (= outside (record input))))))

(deftest substituted-plan-bound-seam-before-issuance-fails-closed
  (let [b (before) i (input)]
    (put! b)
    (with-redefs [mono/prepare-plan-bound-monotonic-command-incorporation-one-shot!
                  (fn [& _] {:counterfeit true})]
      (rejects! :substituted-certified-plan-incorporation-seam
                #(prepare! (candidate b) i)))
    (is (= b (physical)))
    (is (nil? (record i)))))

(deftest substituted-plan-bound-seam-after-issuance-consumes-permit
  (let [{:keys [before input gateway]} (stage!)]
    (with-redefs [mono/prepare-plan-bound-monotonic-command-incorporation-one-shot!
                  (fn [& _] {:counterfeit true})]
      (rejects! :substituted-certified-plan-incorporation-seam
                (:submit! gateway)))
    (consumed! gateway)
    (is (= before (physical)))
    (is (nil? (record input)))))

(deftest readback-seam-failure-after-commit-remains-indeterminate
  (let [{:keys [before input gateway]} (stage!)
        response ((:submit! gateway))]
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))
    (with-redefs [readback/acquire-atomic-command-readback!
                  (fn [& _] {:status :counterfeit})]
      (rejects! :substituted-submission-or-readback-seam
                #((:observe-return! gateway) response)))
    (is (= :outcome-unknown-or-unobserved
           (:readback-status ((:summary! gateway)))))
    (rejects! :readback-already-attempted
              #((:observe-return! gateway) response))
    (consumed! gateway)
    (is (= (after before) (physical)))
    (is (= (expected-record input) (record input)))))

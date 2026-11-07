(ns gesso.assurance.proof.xtdb-restricted-submission-gateway-test
  "v1532: Permanent adversarial checks of the opt-in, one-use XTDB sink,
   exact-return linkage, local attempt observations, and the v1531
   same-node, read-only ledger correlation.

   These are bounded tests of the *actual owned execute-tx call*, not a proof
   of application authority, physical causality, transaction-internal prestate,
   complete writer coverage, or universal refinement. Bootstrap and hostile
   XTDB writes in this fixture deliberately bypass the gateway; they are
   separate physical writers and must never be hidden in an assurance report.

   A genuine TransactionKey means XTDB accepted the API call, not that the
   caller has established a certified abstract business transition. All
   uncertain outcomes remain uncertain; no automatic retries are performed.
   Attempt observations are process-local snapshots of this gateway's code
   path, never a durable XTDB receipt or authenticated deployment identity.
   A matching xt.txs record corroborates one returned key, not application
   poststate causality, writer completeness, or physical refinement."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gateway]
            [gesso.model.command :as command]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt])
  (:import [java.util Date]
           [java.util.concurrent CountDownLatch TimeUnit]
           [xtdb.api TransactionKey]))

(def ^:private secret "v1526-sensitive-payload")
(def ^:private version-keys physical/version-keys)

(defn- next-document [before]
  (-> before
      (assoc :status "reserved"
             :claims 1
             :updated-at (str "v1526-update-" (inc (:revision before))))
      (update :revision inc)))

(defn- plan-for
  ([docs] (plan-for docs true))
  ([docs with-count-guard?]
   {:commands (mapv (fn [before]
                      (command/update-command
                       physical/table :slot/reserve before
                       (next-document before) version-keys))
                    docs)
    :assertions (if with-count-guard?
                  (mapv (fn [before]
                          (tx/assert-document-field-count
                           physical/table (:xt/id before) (count before)))
                        docs)
                  [])
    :emit false}))

(defn- source-for
  ([docs] (source-for docs true))
  ([docs with-count-guard?]
   {:plan (plan-for docs with-count-guard?)
    :protected-documents
    (mapv (fn [before]
            {:target [physical/table (:xt/id before)] :fields :all})
          docs)}))

(defn- gate [node docs]
  (gateway/prepare-one-shot! node (source-for docs)))

(defn- rejection [f]
  (try
    (f)
    :not-rejected
    (catch clojure.lang.ExceptionInfo e
      (if (:xtdb-restricted-submission-gateway/rejection (ex-data e))
        (:reason (ex-data e))
        (throw e)))))

(defn- reject! [expected f]
  (is (= expected (rejection f))
      (str "Expected a gateway rejection: " expected)))

(defn- limited-summary! [g status]
  (let [s ((:summary! g))]
    (is (= gateway/gateway-type (:type s)))
    (is (= gateway/gateway-version (:version s)))
    (is (= status (:status s)))
    (is (true? (:gateway-owned-submission-call-site? s)))
    (is (= :source-derived-finite-admitted-vector
           (:submitted-operation-origin s)))
    (doseq [[k expected]
            [[:public-key-to-ledger-join :not-checked]
             [:transaction-internal-prestate :not-authenticated]
             [:physical-frame-preservation :not-established]
             [:installed-host-adoption :not-established]
             [:caller-node-authenticity :not-established]
             [:principal-authorization :not-established]
             [:outside-gateway-writer-closure :not-established]
             [:physical-refinement :not-established]
             [:proof-status :local-boundary-only]
             [:admission :not-admitted]
             [:certificate :not-issued]
             [:authority :none]]]
      (is (= expected (get s k)) (str "Unexpected claim: " k)))
    (doseq [forbidden [:plan :protected-documents :tx-ops :node :source
                       :before :after :tx-key :principal :sql
                       :statement-parameters :physical-document]]
      (is (not (contains? s forbidden))
          (str "Diagnostic exposed protected detail: " forbidden)))
    (is (not (str/includes? (pr-str s) secret)))
    s))

(defn- with-physical [f]
  (physical/with-proving-application!
   (fn [app]
     ;; The existing v1494 fixture deliberately seeds an XTDB document
     ;; through a separate trusted test path. That bootstrap is not evidence
     ;; that *all* application writers use the gateway.
     (f app ((:read! app))))))

(deftest preparation-does-not-submit-and-exposes-only-opaque-functions
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])]
        (is (= #{:type :version :submit! :summary!} (set (keys g))))
        (is (fn? (:submit! g)))
        (is (fn? (:summary! g)))
        (is (= gateway/gateway-type (:type g)))
        (is (= gateway/gateway-version (:version g)))
        (is (= before ((:read! app))) "Preparation has no physical write")
        (is (= 3 (:candidate-operation-count
                  (limited-summary! g :prepared))))
        (is (= 2 (:prewrite-assertion-count ((:summary! g)))))
        (is (= 1 (:whole-document-replacement-count ((:summary! g)))))
        (is (thrown? clojure.lang.ArityException
                     ((:submit! g) [[:erase-docs physical/table (:xt/id before)]])))
        (is (= :prepared (:status ((:summary! g))))
            "Wrong-arity invocation never consumes the capability")))))

(deftest genuine-embedded-xtdb-submission-returns-only-a-transaction-key
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))]
        (is (instance? TransactionKey (:tx-key result)))
        (is (= :returned-transaction-key
               (:status (:diagnostic result))))
        (is (= (:diagnostic result) (limited-summary! g :returned-transaction-key)))
        (is (= (next-document before) ((:read! app))))
        (reject! :one-shot-already-attempted (:submit! g))
        (is (= (next-document before) ((:read! app))))))))

(deftest two-document-transaction-is-one-owned-submission
  (with-physical
    (fn [app first-row]
      (let [second-row (physical/initial-document
                        (str "v1526-second-" (random-uuid)))
            second-id (:xt/id second-row)
            seed (xt/execute-tx (:node app)
                                [[:put-docs physical/table second-row]])
            ;; Seed is a separate, explicitly unmediated fixture writer.
            _ (is (instance? TransactionKey seed))
            g (gate (:node app) [first-row second-row])]
        (is (= 6 (:candidate-operation-count ((:summary! g)))))
        (is (= 2 (:whole-document-replacement-count ((:summary! g)))))
        (is (instance? TransactionKey (:tx-key ((:submit! g)))))
        (is (= (next-document first-row) ((:read! app))))
        (is (= (select-keys (next-document second-row)
                            [:revision :status :claims :updated-at])
               (select-keys
                (first (xt/q (:node app)
                             ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                              second-id]))
                [:revision :status :claims :updated-at])))
        (limited-summary! g :returned-transaction-key)))))

(deftest stale-prestate-rejects-without-claiming-known-commit
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            external (next-document before)]
        ;; Competing direct write happens AFTER preparation. Canonical
        ;; revision assertions must reject the old snapshot atomically.
        (xt/execute-tx (:node app) [[:put-docs physical/table external]])
        (let [failure (try ((:submit! g)) nil (catch Throwable t t))]
          (is (instance? Throwable failure))
          (is (= :xtdb/assert-failed
                 (:xtdb.error/code (ex-data failure))))
          (limited-summary! g :threw-outcome-unknown)
          (reject! :one-shot-already-attempted (:submit! g))
          (is (= external ((:read! app)))
              "Rejected conditional TX must not replace intervening state"))))))

(deftest no-caller-controlled-transaction-vector-or-xtdb-options
  (with-physical
    (fn [app before]
      (let [src (source-for [before])
            ops (:tx-ops (tx/prepare nil (:plan src)))]
        (doseq [extra [{:candidate-tx-ops ops}
                       {:candidate-tx-ops (conj ops [:delete-docs physical/table
                                                     (:xt/id before)])}
                       {:tx-ops (vec (reverse ops))}
                       {:execute-tx (fn [& _] :forged)}
                       {:submit! (fn [& _] :forged)}
                       {:tx-options {:system-time :forged}}
                       {:certificate :issued}
                       {:authority :certified}
                       {:admission :admitted}
                       {:principal :administrator}]]
          (reject! :invalid-gateway-source
                   #(gateway/prepare-one-shot!
                     (:node app) (merge src extra))))
        (is (= before ((:read! app)))
            "Caller-supplied ops and options must never execute")))))

(deftest inadmissible-edits-to-plan-and-protection-reject
  (with-physical
    (fn [app before]
      (let [src (source-for [before])
            plan (:plan src)
            command (first (:commands plan))
            candidates [(update-in src [:plan :commands] conj command)
                        (assoc-in src [:plan :commands] [])
                        (update-in src [:plan :commands 0 :model/after]
                                   assoc :xt/id "substituted-document")
                        (assoc-in src [:protected-documents 0 :target]
                                  [physical/table "substituted-target"])
                        (assoc-in src [:protected-documents 0 :fields]
                                  #{:status})]]
        (doseq [candidate candidates]
          (is (not= :not-rejected
                    (rejection #(gateway/prepare-one-shot!
                                 (:node app) candidate)))
              "A changed transaction or its protection must fail closed"))
        (is (= before ((:read! app))))))))

(deftest independently-admitted-alternative-plan-cannot-replace-prior-plan
  ;; The count assertion is optional in the supported subset. Its removal
  ;; can be valid when the remaining generated version guard still admits the
  ;; transaction. Such a plan is NOT a substitution inside the older closure:
  ;; it must acquire its own separate one-shot capability, and the old one
  ;; cannot be reused after another transaction changes the physical row.
  (with-physical
    (fn [app before]
      (let [original (gate (:node app) [before])
            changed-source (assoc-in (source-for [before])
                                     [:plan :assertions] [])
            alternative (gateway/prepare-one-shot! (:node app) changed-source)]
        (is (= 3 (:candidate-operation-count ((:summary! original)))))
        (is (= 2 (:candidate-operation-count ((:summary! alternative)))))
        (limited-summary! original :prepared)
        (limited-summary! alternative :prepared)
        (is (instance? TransactionKey (:tx-key ((:submit! alternative)))))
        (limited-summary! alternative :returned-transaction-key)
        (is (= (next-document before) ((:read! app))))
        (let [failure (try ((:submit! original)) nil
                           (catch Throwable t t))]
          (is (= :xtdb/assert-failed
                 (:xtdb.error/code (ex-data failure))))
          (limited-summary! original :threw-outcome-unknown)
          (reject! :one-shot-already-attempted (:submit! original)))))))

(deftest unsupported-xtdb-write-forms-and-arbitrary-sql-are-not-executed
  (with-physical
    (fn [app before]
      (let [src (source-for [before])
            plan (:plan src)
            invalid [(assoc-in src [:plan :assertions 0]
                               {:assert "ASSERT TRUE"})
                     (assoc-in src [:plan :assertions 0]
                               {:assert "DELETE FROM gesso_v1494_reservation_slots"})
                     (assoc-in src [:plan :assertions 0]
                               {:assert "UPDATE gesso_v1494_reservation_slots SET claims = 4"})
                     (assoc-in src [:plan :commands 0]
                               [:patch-docs physical/table {:xt/id (:xt/id before)}])
                     (assoc-in src [:plan :commands 0]
                               [:delete-docs physical/table (:xt/id before)])
                     (assoc-in src [:plan :commands 0]
                               [:erase-docs physical/table (:xt/id before)])
                     (assoc src :plan (assoc plan :tx-ops
                                            [[:erase-docs physical/table
                                              (:xt/id before)]]))]]
        (doseq [candidate invalid]
          (is (not= :not-rejected
                    (rejection #(gateway/prepare-one-shot!
                                 (:node app) candidate)))))
        (is (= before ((:read! app))))))))

(deftest mutable-and-unserializable-values-are-never-admitted
  (with-physical
    (fn [app before]
      (let [src (source-for [before])
            after-path [:plan :commands 0 :model/after]
            mutation [(assoc-in src (conj after-path :audit-tag)
                                (Date.))
                      (assoc-in src (conj after-path :audit-tag)
                                (byte-array [1 2 3]))
                      (assoc-in src (conj after-path :audit-tag)
                                (java.util.HashMap. {"secret" secret}))
                      (assoc-in src (conj after-path :audit-tag)
                                (java.util.ArrayList. [secret]))
                      (assoc-in src (conj after-path :audit-tag)
                                (lazy-seq (cons secret nil)))]]
        (doseq [candidate mutation]
          (is (not= :not-rejected
                    (rejection #(gateway/prepare-one-shot!
                                 (:node app) candidate)))
              "Mutable, lazy and foreign values cannot be bound by equality"))
        (is (= before ((:read! app))))))))

(deftest malformed-sources-and-live-effects-reject-instead-of-downgrading
  (with-physical
    (fn [app before]
      (let [src (source-for [before])]
        (doseq [candidate [nil [] {}
                           (with-meta src {:certified true})
                           (assoc src :protected-documents nil)
                           (assoc src :protected-documents [])
                           (assoc src :plan nil)
                           (assoc-in src [:plan :emit] true)
                           (assoc-in src [:plan :changes]
                                     [{:kind :id :id "live-notification"}])
                           (assoc-in src [:plan :emit] nil)]]
          (is (not= :not-rejected
                    (rejection #(gateway/prepare-one-shot!
                                 (:node app) candidate)))))
        (reject! :missing-xtdb-node
                 #(gateway/prepare-one-shot! nil src))
        (is (= before ((:read! app))))))))

(deftest source-checker-redefinition-cannot-forge-an-admission
  (with-physical
    (fn [app before]
      (let [src (source-for [before])
            original-result (gate (:node app) [before])]
        (reject! :substituted-gateway-or-upstream-seam
                 #(with-redefs [tx/prepare (fn [& _]
                                            {:tx-ops [[:erase-docs
                                                       physical/table
                                                       (:xt/id before)]]})]
                    (gateway/prepare-one-shot! (:node app) src)))
        ;; Tampering after preparation is detected at the pre-submission seam
        ;; and spends the capability. No XTDB write may be made.
        (reject! :substituted-gateway-or-upstream-seam
                 #(with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
                    ((:submit! original-result))))
        (limited-summary! original-result :preflight-rejected)
        (reject! :one-shot-already-attempted (:submit! original-result))
        (is (= before ((:read! app))))))))

(deftest one-shot-exception-is-consumed-without-inventing-rejection
  ;; Object is deliberately *not* an XTDB node. Validation only tests the
  ;; caller-supplied reference for nil; the sink throws during the real API
  ;; call. An exception does not establish whether an external sink committed.
  (let [before (physical/initial-document
                (str "v1526-unusable-node-" (random-uuid)))
        g (gate (Object.) [before])
        failure (try ((:submit! g)) nil (catch Throwable t t))]
    (is (instance? Throwable failure))
    (limited-summary! g :threw-outcome-unknown)
    (reject! :one-shot-already-attempted (:submit! g))))

(deftest racing-callers-get-exactly-one-physical-submission
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            ready (CountDownLatch. 10)
            fire (CountDownLatch. 1)
            tasks (mapv (fn [_]
                          (future
                            (.countDown ready)
                            (when-not (.await fire 20 TimeUnit/SECONDS)
                              (throw (ex-info "Race not released" {})))
                            (try
                              {:kind :returned :result ((:submit! g))}
                              (catch clojure.lang.ExceptionInfo e
                                (if (:xtdb-restricted-submission-gateway/rejection
                                     (ex-data e))
                                  {:kind :rejected :reason (:reason (ex-data e))}
                                  (throw e))))))
                        (range 10))]
        (try
          (is (.await ready 20 TimeUnit/SECONDS))
          (.countDown fire)
          (let [results (mapv #(deref % 30000 :timed-out) tasks)
                returns (filter #(= :returned (:kind %)) results)
                refusals (filter #(= :rejected (:kind %)) results)]
            (is (= 10 (count results)))
            (is (= 1 (count returns)))
            (is (instance? TransactionKey
                           (get-in (first returns) [:result :tx-key])))
            (is (= 9 (count refusals)))
            (is (every? #(= :one-shot-already-attempted (:reason %))
                        refusals))
            (is (not-any? #{:timed-out} results))
            (is (= (next-document before) ((:read! app))))
            (limited-summary! g :returned-transaction-key))
          (finally
            (.countDown fire)
            (doseq [task tasks] (future-cancel task))))))))

(deftest unmediated-xtdb-writer-remains-a-real-bypass
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            outside (-> before (assoc :audit-tag secret))
            key (xt/execute-tx (:node app)
                               [[:put-docs physical/table outside]])]
        (is (instance? TransactionKey key))
        (is (= outside ((:read! app)))
            "A caller with raw XTDB node access bypasses the gateway")
        (limited-summary! g :prepared)
        ;; This gateway may nevertheless submit the original source later,
        ;; because its guard does not check every persisted field. The fixture
        ;; intentionally leaves that physical assumption visible.
        (is (= :not-established
               (:outside-gateway-writer-closure ((:summary! g)))))))))


;; v1528: The exact returned map is evidence of THIS call site's normal
;; return. It is NOT a certificate of XTDB commitment or of abstract-model
;; refinement. The exported gateway map itself is data containing the two
;; original closures; copying that map without replacing either function is
;; allowed. Copying the *result* map is deliberately not allowed.

(def ^:private unestablished-local-claims
  {:source-binding-scope :process-local-nonauthenticating
   :local-return-linkage :exact-object-from-owned-call-site
   :operation-origin :readmitted-source-derived-finite-vector
   :committed-transaction :not-established
   :public-key-to-ledger-join :not-checked
   :physical-poststate-causality :not-established
   :transaction-internal-prestate :not-authenticated
   :complete-deployment-writer-coverage :not-established
   :principal-authorization :not-established
   :physical-refinement :not-established
   :proof-status :process-local-result-correspondence-only
   :admission :not-admitted
   :certificate :not-issued
   :authority :none})

(defn- bounded-return-evidence! [prepared result]
  (let [evidence (gateway/require-local-return! prepared result)]
    (is (= gateway/local-return-type (:type evidence)))
    (is (= gateway/local-return-version (:version evidence)))
    (is (= gateway/gateway-version (:gateway-version evidence)))
    (is (string? (:process-local-source-binding evidence)))
    (is (not (str/blank? (:process-local-source-binding evidence))))
    (is (= 1 (:owned-execute-tx-call-entries evidence)))
    (is (= 3 (:candidate-operation-count evidence)))
    (is (= 2 (:prewrite-assertion-count evidence)))
    (is (= 1 (:whole-document-replacement-count evidence)))
    (is (= :public-transaction-key (:observed-result-kind evidence)))
    (doseq [[k expected] unestablished-local-claims]
      (is (= expected (get evidence k)) (str "Overclaimed: " k)))
    (doseq [secret-key [:tx-key :node :plan :tx-ops :principal :document
                        :before :after :protected-documents :source]]
      (is (not (contains? evidence secret-key))))
    (is (not (str/includes? (pr-str evidence) secret)))
    evidence))

(deftest local-return-accepts-only-a-normal-return-from-owned-call
  (with-physical
    (fn [app before]
      (let [prepared (gate (:node app) [before])]
        (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return! prepared nil))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return! prepared {:tx-key nil}))
        (let [result ((:submit! prepared))
              evidence (bounded-return-evidence! prepared result)]
          (is (instance? TransactionKey (:tx-key result)))
          (is (= 1 (:owned-execute-tx-call-entries ((:summary! prepared)))))
          (is (= :returned-transaction-key (:status ((:summary! prepared)))))
          (is (= (:process-local-source-binding evidence)
                 (:process-local-source-binding
                  (bounded-return-evidence! (into {} prepared) result)))
              "Copying the gateway map with its ORIGINAL functions is fine")
          (is (= (next-document before) ((:read! app))))
          (reject! :one-shot-already-attempted (:submit! prepared)))))))

(deftest copied-or-altered-results-never-become-owned-local-returns
  (with-physical
    (fn [app before]
      (let [prepared (gate (:node app) [before])
            result ((:submit! prepared))
            fake-result (into {} result)]
        (is (= result fake-result))
        (is (not (identical? result fake-result)))
        (doseq [candidate [fake-result
                           (assoc result :tx-key nil)
                           (assoc result :diagnostic {:status :committed})
                           (assoc result :certificate :issued)
                           (dissoc result :diagnostic)
                           (with-meta result {:asserted-authority true})
                           (:tx-key result)
                           {:tx-key (:tx-key result)
                            :diagnostic (:diagnostic result)}]]
          (reject! :not-this-gateway-local-return
                   #(gateway/require-local-return! prepared candidate)))
        (bounded-return-evidence! prepared result)
        (is (= (next-document before) ((:read! app))))))))

(deftest other-gateway-cannot-claim-a-genuine-return-even-with-same-source
  (with-physical
    (fn [app before]
      (let [a (gate (:node app) [before])
            b (gate (:node app) [before])
            result ((:submit! a))]
        (is (= :prepared (:status ((:summary! b)))))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return! b result))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return!
                   (assoc a :submit! (:submit! b)) result))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return!
                   (assoc a :summary! (:summary! b)) result))
        (bounded-return-evidence! a result)
        (is (= 0 (:owned-execute-tx-call-entries ((:summary! b)))))
        (is (= (next-document before) ((:read! app))))))))

(deftest invalid-gateway-map-is-not-an-authority-token
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))]
        (doseq [candidate [nil {} []
                           (dissoc g :summary!)
                           (assoc g :type :wrong)
                           (assoc g :version (dec gateway/gateway-version))
                           (assoc g :extra :certificate)
                           (assoc g :submit! (fn [] result))
                           (assoc g :summary! (fn [] ((:summary! g))))
                           (with-meta g {:certified true})]]
          (let [reason (rejection #(gateway/require-local-return!
                                   candidate result))]
            (is (#{:invalid-local-gateway :not-this-gateway-local-return}
                 reason) (str "Invalid gate accepted: " (pr-str candidate)))))
        (bounded-return-evidence! g result)
        (is (= 1 (:owned-execute-tx-call-entries ((:summary! g)))))
        (is (= (next-document before) ((:read! app))))))))

(deftest preflight-failure-has-no-return-and-consumes-only-attempt
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])]
        (reject! :substituted-gateway-or-upstream-seam
                 #(with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
                    ((:submit! g))))
        (is (= :preflight-rejected (:status ((:summary! g)))))
        (is (= 0 (:owned-execute-tx-call-entries ((:summary! g)))))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-return!
                   g {:tx-key nil :diagnostic ((:summary! g))}))
        (reject! :one-shot-already-attempted (:submit! g))
        (is (= before ((:read! app))))))))

(deftest execution-exception-produces-no-owned-return-or-false-commit
  (let [before (physical/initial-document
                (str "v1528-bad-node-" (random-uuid)))
        g (gate (Object.) [before])
        failure (try ((:submit! g)) nil (catch Throwable t t))]
    (is (instance? Throwable failure))
    (is (= :threw-outcome-unknown (:status ((:summary! g)))))
    (is (= 1 (:owned-execute-tx-call-entries ((:summary! g)))))
    (reject! :not-this-gateway-local-return
             #(gateway/require-local-return! g
                {:tx-key nil :diagnostic ((:summary! g))}))
    (reject! :one-shot-already-attempted (:submit! g))
    (is (= :not-established (:physical-refinement ((:summary! g)))))))

(deftest a-public-key-and-a-visible-poststate-do-not-prove-causality
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))
            _ (bounded-return-evidence! g result)
            outside (assoc (next-document before) :audit-tag secret)
            outside-key (xt/execute-tx
                         (:node app) [[:put-docs physical/table outside]])
            evidence (gateway/require-local-return! g result)]
        (is (instance? TransactionKey outside-key))
        (is (= outside ((:read! app))))
        ;; A later physical mutation must not be interpreted as caused by
        ;; the original gateway simply because its TransactionKey was real.
        (doseq [k [:committed-transaction :physical-poststate-causality
                   :complete-deployment-writer-coverage :physical-refinement]]
          (is (= :not-established (get evidence k))))
        (is (= :not-issued (:certificate evidence)))
        (is (= :none (:authority evidence)))))))

(deftest racing-one-shot-has-one-valid-local-return-and-no-spares
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            ready (CountDownLatch. 6)
            go (CountDownLatch. 1)
            tasks (mapv (fn [_]
                          (future
                            (.countDown ready)
                            (when-not (.await go 15 TimeUnit/SECONDS)
                              (throw (ex-info "Race not released" {})))
                            (try
                              {:kind :return :value ((:submit! g))}
                              (catch clojure.lang.ExceptionInfo e
                                (if (:xtdb-restricted-submission-gateway/rejection
                                     (ex-data e))
                                  {:kind :refusal :reason (:reason (ex-data e))}
                                  (throw e))))))
                        (range 6))]
        (try
          (is (.await ready 15 TimeUnit/SECONDS))
          (.countDown go)
          (let [results (mapv #(deref % 30000 :timeout) tasks)
                successes (filter #(= :return (:kind %)) results)
                refusals (filter #(= :refusal (:kind %)) results)]
            (is (= 1 (count successes)))
            (is (= 5 (count refusals)))
            (is (every? #(= :one-shot-already-attempted (:reason %))
                        refusals))
            (is (not-any? #{:timeout} results))
            (is (= 1 (:owned-execute-tx-call-entries ((:summary! g)))))
            (when-let [result (:value (first successes))]
              (bounded-return-evidence! g result)
              (reject! :not-this-gateway-local-return
                       #(gateway/require-local-return! g (into {} result))))
            (is (= (next-document before) ((:read! app)))))
          (finally
            (.countDown go)
            (doseq [task tasks] (future-cancel task))))))))


;; v1530: the attempt journal is observed, not supplied. Test every finite
;; state as a whole coherent snapshot; no independent status/counter reads
;; should be needed to explain what the gateway has observed locally.
(def ^:private allowed-attempt-events
  {:prepared [:prepared]
   :checking [:prepared :checking]
   :preflight-rejected [:prepared :checking :preflight-rejected]
   :submitting [:prepared :checking :submitting]
   :returned-transaction-key
   [:prepared :checking :submitting :returned-transaction-key]
   :returned-unrecognized
   [:prepared :checking :submitting :returned-unrecognized]
   :threw-outcome-unknown
   [:prepared :checking :submitting :threw-outcome-unknown]})

(def ^:private attempt-nonclaims
  {:committed-transaction :not-established
   :xtdb-receipt :not-established
   :public-key-to-ledger-join :not-checked
   :physical-poststate-causality :not-established
   :transaction-internal-prestate :not-authenticated
   :principal-authorization :not-established
   :complete-deployment-writer-coverage :not-established
   :physical-refinement :not-established
   :proof-status :process-local-attempt-observation-only
   :admission :not-admitted
   :certificate :not-issued
   :authority :none})

(defn- bounded-attempt-evidence! [e]
  (is (= gateway/local-attempt-type (:type e)))
  (is (= gateway/local-attempt-version (:version e)))
  (is (= gateway/gateway-version (:gateway-version e)))
  (is (= :process-local-nonauthenticating (:source-binding-scope e)))
  (is (string? (:process-local-attempt-id e)))
  (is (not (str/blank? (:process-local-attempt-id e))))
  (is (string? (:process-local-source-binding e)))
  (is (not (str/blank? (:process-local-source-binding e))))
  (is (= (get allowed-attempt-events (:status e))
         (:observed-local-events e))
      (str "Inconsistent attempt lifecycle: " (:status e)))
  (is (<= 0 (:owned-execute-tx-call-entries e) 1))
  (let [entered? (#{:submitting :returned-transaction-key
                    :returned-unrecognized :threw-outcome-unknown}
                  (:status e))]
    (is (= (if entered? 1 0) (:owned-execute-tx-call-entries e)))
    (is (= (if entered? :entered-locally :not-entered)
           (:call-site-entry e))))
  (is (= (if (= :preflight-rejected (:status e))
           :no-gateway-call :not-established)
         (:physical-outcome e)))
  (is (= (case (:status e)
           :returned-transaction-key :public-transaction-key
           :returned-unrecognized :unrecognized-xtdb-api-return
           :none)
         (:observed-result-kind e)))
  (doseq [[k expected] attempt-nonclaims]
    (is (= expected (get e k)) (str "Attempt overclaimed " k)))
  (doseq [forbidden [:node :plan :tx-ops :tx-key :principal
                     :before :after :source :protected-documents
                     :physical-document :exception :error :sql]]
    (is (not (contains? e forbidden))
        (str "Attempt evidence exposed " forbidden)))
  (is (not (str/includes? (pr-str e) secret)))
  e)

(defn- checked-attempt! [prepared expected-status]
  (let [snapshot (bounded-attempt-evidence!
                  (gateway/require-local-attempt! prepared))]
    (is (= expected-status (:status snapshot)))
    snapshot))

(deftest local-attempt-preparation-is-read-only-and-unique-per-gateway
  (with-physical
    (fn [app before]
      (let [a (gate (:node app) [before])
            b (gate (:node app) [before])
            ae (checked-attempt! a :prepared)
            be (checked-attempt! b :prepared)]
        (is (not= (:process-local-attempt-id ae)
                  (:process-local-attempt-id be)))
        (is (= (:process-local-source-binding ae)
               (:process-local-source-binding be))
            "Equal admitted sources may share a source binding")
        (is (= 3 (:candidate-operation-count ae)))
        (is (= 2 (:prewrite-assertion-count ae)))
        (is (= 1 (:whole-document-replacement-count ae)))
        (is (= ae (gateway/require-local-attempt! (into {} a)))
            "A map copy retaining the same registered functions is valid")
        (is (= before ((:read! app))))
        (is (= :prepared (:status ((:summary! a)))))))))

(deftest local-attempt-normal-return-links-to-exact-return-without-a-commit-claim
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            prepared (checked-attempt! g :prepared)
            result ((:submit! g))
            finished (checked-attempt! g :returned-transaction-key)
            local-return (gateway/require-local-return! g result)]
        (is (instance? TransactionKey (:tx-key result)))
        (is (= (:process-local-attempt-id prepared)
               (:process-local-attempt-id finished)
               (:process-local-attempt-id local-return)))
        (is (= (:process-local-source-binding prepared)
               (:process-local-source-binding finished)
               (:process-local-source-binding local-return)))
        (is (= 1 (:owned-execute-tx-call-entries finished)))
        (is (= finished (gateway/require-local-attempt! g)))
        (reject! :one-shot-already-attempted (:submit! g))
        (is (= finished (gateway/require-local-attempt! g))
            "A refused retry cannot create a second journal event")
        (is (= (next-document before) ((:read! app))))))))

(deftest preflight-refusal-consumes-one-shot-without-an-xtdb-call
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            prepared (checked-attempt! g :prepared)]
        (reject! :substituted-gateway-or-upstream-seam
                 #(with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
                    ((:submit! g))))
        (let [rejected (checked-attempt! g :preflight-rejected)]
          (is (= (:process-local-attempt-id prepared)
                 (:process-local-attempt-id rejected)))
          (is (= :no-gateway-call (:physical-outcome rejected)))
          (is (= 0 (:owned-execute-tx-call-entries rejected)))
          (reject! :not-this-gateway-local-return
                   #(gateway/require-local-return! g
                      {:tx-key nil :diagnostic ((:summary! g))}))
          (reject! :one-shot-already-attempted (:submit! g))
          (is (= rejected (gateway/require-local-attempt! g)))
          (is (= before ((:read! app)))))))))

(deftest thrown-xtdb-invocation-remains-physically-unknown
  ;; An unsupported node deliberately throws AFTER entry into the gateway's
  ;; owned execute-tx call site, with no potentially successful API return.
  (let [before (physical/initial-document
                (str "v1530-throw-" (random-uuid)))
        g (gate (Object.) [before])
        prior (checked-attempt! g :prepared)
        failure (try ((:submit! g)) nil (catch Throwable t t))
        after (checked-attempt! g :threw-outcome-unknown)]
    (is (instance? Throwable failure))
    (is (= (:process-local-attempt-id prior)
           (:process-local-attempt-id after)))
    (is (= :not-established (:physical-outcome after)))
    (is (= :none (:observed-result-kind after)))
    (reject! :not-this-gateway-local-return
             #(gateway/require-local-return!
               g {:tx-key nil :diagnostic ((:summary! g))}))
    (reject! :one-shot-already-attempted (:submit! g))
    (is (= after (gateway/require-local-attempt! g)))))

(deftest caller-forged-gateway-parts-do-not-confer-attempt-provenance
  (let [before (physical/initial-document
                (str "v1530-forge-" (random-uuid)))
        a (gate (Object.) [before])
        b (gate (Object.) [before])
        authentic (checked-attempt! a :prepared)]
    (doseq [candidate [nil [] {}
                       (dissoc a :summary!)
                       (assoc a :type :forged)
                       (assoc a :version 0)
                       (assoc a :certificate :issued)
                       (assoc a :attempt-id (:process-local-attempt-id authentic))
                       (with-meta a {:certified true})]]
      (reject! :invalid-local-gateway
               #(gateway/require-local-attempt! candidate)))
    (doseq [candidate [(assoc a :submit! (:submit! b))
                       (assoc a :summary! (:summary! b))
                       (assoc a :submit! (fn [] :forged))
                       (assoc a :summary! (fn [] authentic))]]
      (reject! :not-this-gateway-local-attempt
               #(gateway/require-local-attempt! candidate)))
    (is (= authentic (gateway/require-local-attempt! a)))
    (is (not= (:process-local-attempt-id authentic)
              (:process-local-attempt-id
               (checked-attempt! b :prepared))))))

(deftest snapshots-are-only-observations-and-not-caller-writable-journals
  (let [before (physical/initial-document
                (str "v1530-snapshot-" (random-uuid)))
        g (gate (Object.) [before])
        snapshot (checked-attempt! g :prepared)
        invented (-> snapshot
                     (assoc :status :returned-transaction-key
                            :observed-result-kind :public-transaction-key
                            :owned-execute-tx-call-entries 4
                            :certificate :issued)
                     (update :observed-local-events conj :submitting))]
    (is (not= snapshot invented))
    (is (= snapshot (gateway/require-local-attempt! g))
        "Editing returned data does not edit the gateway's private atom")
    (reject! :invalid-local-gateway
             #(gateway/require-local-attempt! invented))
    (is (thrown? clojure.lang.ArityException
                 (gateway/require-local-attempt! g invented)))
    (is (not (contains? ((:summary! g)) :observed-local-events)))
    (is (not (contains? ((:summary! g)) :process-local-attempt-id)))))

(deftest preparation-rejects-invalid-source-without-issuing-attempt-evidence
  (let [before (physical/initial-document
                (str "v1530-invalid-" (random-uuid)))
        source (assoc (source-for [before]) :certificate :issued)]
    (reject! :invalid-gateway-source
             #(gateway/prepare-one-shot! (Object.) source))
    (reject! :invalid-local-gateway
             #(gateway/require-local-attempt! source))))

(deftest concurrent-submitters-expose-only-valid-journal-prefixes
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            prepared (checked-attempt! g :prepared)
            ready (CountDownLatch. 6)
            go (CountDownLatch. 1)
            tasks (mapv (fn [_]
                          (future
                            (.countDown ready)
                            (when-not (.await go 20 TimeUnit/SECONDS)
                              (throw (ex-info "Submission race not released" {})))
                            (let [observed-before
                                  (gateway/require-local-attempt! g)
                                  outcome (try
                                            {:kind :return
                                             :result ((:submit! g))}
                                            (catch clojure.lang.ExceptionInfo e
                                              (if (:xtdb-restricted-submission-gateway/rejection
                                                   (ex-data e))
                                                {:kind :refusal
                                                 :reason (:reason (ex-data e))}
                                                (throw e))))]
                              {:observed-before observed-before
                               :outcome outcome
                               :observed-after
                               (gateway/require-local-attempt! g)})))
                        (range 6))]
        (try
          (is (.await ready 20 TimeUnit/SECONDS))
          (.countDown go)
          (let [outcomes (mapv #(deref % 60000 :timeout) tasks)
                completed (remove #{:timeout} outcomes)
                returns (filter #(= :return (get-in % [:outcome :kind]))
                                completed)
                refused (filter #(= :refusal (get-in % [:outcome :kind]))
                                completed)
                ending (checked-attempt! g :returned-transaction-key)]
            (is (not-any? #{:timeout} outcomes))
            (is (= 1 (count returns)))
            (is (= 5 (count refused)))
            (is (every? #(= :one-shot-already-attempted
                            (get-in % [:outcome :reason]))
                        refused))
            (doseq [o completed
                    :when (map? o)
                    e [(:observed-before o) (:observed-after o)]]
              (bounded-attempt-evidence! e)
              (is (= (:process-local-attempt-id prepared)
                     (:process-local-attempt-id e)))
              (is (<= (count (:observed-local-events prepared))
                      (count (:observed-local-events e))
                      (count (:observed-local-events ending))))
              (is (= (:observed-local-events e)
                     (subvec (:observed-local-events ending)
                             0 (count (:observed-local-events e))))))
            (when-let [result (get-in (first returns) [:outcome :result])]
              (is (= (:process-local-attempt-id ending)
                     (:process-local-attempt-id
                      (gateway/require-local-return! g result)))))
            (is (= (next-document before) ((:read! app)))))
          (finally
            (.countDown go)
            (doseq [task tasks] (future-cancel task))))))))

(deftest unrelated-xtdb-writes-do-not-enter-this-gateways-journal
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            first-snapshot (checked-attempt! g :prepared)
            outside (assoc before :audit-tag secret)
            key (xt/execute-tx (:node app)
                               [[:put-docs physical/table outside]])
            later-snapshot (checked-attempt! g :prepared)]
        (is (instance? TransactionKey key))
        (is (= outside ((:read! app))))
        (is (= first-snapshot later-snapshot)
            "An outside writer is NOT an event in this gateway's lifecycle")
        (is (= :not-established
               (:complete-deployment-writer-coverage later-snapshot)))
        (is (= :not-established
               (:physical-poststate-causality later-snapshot)))))))


;; v1532: The ledger observation is an opt-in READ of a pinned xt.txs row,
;; not a second submission or a certificate. An exact return and valid local
;; attempt are prerequisites. Invalid/missing observations must never turn
;; into an invented committed business transition.

(def ^:private ledger-nonclaims
  {:source-binding-scope :process-local-nonauthenticating
   :return-linkage :exact-object-from-owned-call-site
   :ledger-observation-origin :read-only-pinned-xt-txs-on-submission-node
   :caller-node-authenticity :not-established
   :xtdb-receipt :not-established
   :committed-transaction :not-established
   :physical-poststate-causality :not-established
   :physical-frame-preservation :not-established
   :transaction-internal-prestate :not-authenticated
   :outside-gateway-writer-closure :not-established
   :complete-deployment-writer-coverage :not-established
   :principal-authorization :not-established
   :physical-refinement :not-established
   :universal-physical-refinement :not-established
   :proof-status :bounded-local-return-and-ledger-corroboration-only
   :admission :not-admitted
   :certificate :not-issued
   :authority :none})

(defn- checked-ledger! [g original-result expected-status expected-checks]
  (let [r (gateway/require-local-ledger-observation! g original-result)
        local (gateway/require-local-return! g original-result)
        attempt (gateway/require-local-attempt! g)]
    (is (= gateway/local-ledger-type (:type r)))
    (is (= gateway/local-ledger-version (:version r)))
    (is (= gateway/gateway-version (:gateway-version r)))
    (is (= (:process-local-attempt-id local)
           (:process-local-attempt-id attempt)
           (:process-local-attempt-id r)))
    (is (= (:process-local-source-binding local)
           (:process-local-source-binding attempt)
           (:process-local-source-binding r)))
    (is (= 1 (:owned-execute-tx-call-entries r)))
    (is (= expected-status (:ledger-status r)) (pr-str r))
    (is (= expected-checks (:ledger-checks r)))
    (doseq [[k expected] ledger-nonclaims]
      (is (= expected (get r k)) (str "Overstated ledger claim: " k)))
    (doseq [k [:node :plan :protected-documents :tx-ops :tx-key :tx-id
               :system-time :snapshot-token :sql :principal :result
               :physical-document :poststate :before :after :exception]]
      (is (not (contains? r k)) (str "Ledger leaked: " k)))
    (is (not (str/includes? (pr-str r) secret)))
    r))

(deftest genuine-key-correlates-with-ledger-without-certifying-its-effects
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            prepared (gateway/require-local-attempt! g)
            result ((:submit! g))
            expected {:tx-id true :committed true :system-time true}
            first-report (checked-ledger!
                          g result
                          :reported-key-matches-committed-ledger-row expected)]
        (is (= (next-document before) ((:read! app))))
        (is (= first-report (checked-ledger!
                             g result
                             :reported-key-matches-committed-ledger-row
                             expected)))
        (is (= (:process-local-attempt-id prepared)
               (:process-local-attempt-id first-report)))
        (reject! :one-shot-already-attempted (:submit! g))
        (is (= 1 (:owned-execute-tx-call-entries
                  (gateway/require-local-attempt! g))))))))

(deftest ledger-observation-corroborates-one-atomic-two-document-submission
  (with-physical
    (fn [app before]
      (let [other (physical/initial-document
                   (str "v1532-second-" (random-uuid)))
            other-id (:xt/id other)
            seeded (xt/execute-tx (:node app)
                                  [[:put-docs physical/table other]])
            g (gate (:node app) [before other])
            result ((:submit! g))
            observation (checked-ledger!
                         g result :reported-key-matches-committed-ledger-row
                         {:tx-id true :committed true :system-time true})]
        (is (instance? TransactionKey seeded))
        (is (= 1 (:owned-execute-tx-call-entries observation)))
        (is (= (next-document before) ((:read! app))))
        (is (= (select-keys (next-document other) [:revision :claims :status])
               (select-keys
                (first (xt/q (:node app)
                             ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                              other-id]))
                [:revision :claims :status])))))))

(deftest copied-forged-and-foreign-results-never-become-ledger-evidence
  (with-physical
    (fn [app before]
      (let [a (gate (:node app) [before])
            b (gate (:node app) [before])
            result ((:submit! a))
            wrong-source (assoc a :summary! (:summary! b))]
        (doseq [copy [(into {} result)
                      (assoc result :tx-key nil)
                      (assoc result :diagnostic {:status :returned-transaction-key})
                      {:tx-key (:tx-key result)
                       :diagnostic (:diagnostic result)}]]
          (reject! :not-this-gateway-local-return
                   #(gateway/require-local-ledger-observation! a copy)))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-ledger-observation! b result))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-ledger-observation!
                   wrong-source result))
        (doseq [forged [(assoc a :type :forged)
                        (assoc a :version 999)
                        (assoc a :certificate :issued)
                        (with-meta a {:authorized true})]]
          (reject! :invalid-local-gateway
                   #(gateway/require-local-ledger-observation! forged result)))
        (checked-ledger! a result :reported-key-matches-committed-ledger-row
                         {:tx-id true :committed true :system-time true})
        (is (= :prepared (:status (gateway/require-local-attempt! b))))))))

(deftest neither-preflight-refusal-nor-thrown-sink-can-produce-ledger-evidence
  (let [before (physical/initial-document
                (str "v1532-failure-" (random-uuid)))
        source (source-for [before])
        preflight (gateway/prepare-one-shot! (Object.) source)
        throwing (gateway/prepare-one-shot! (Object.) source)]
    (reject! :substituted-gateway-or-upstream-seam
             #(with-redefs [tx/prepare (fn [& _] {:tx-ops []})]
                ((:submit! preflight))))
    (is (= :preflight-rejected
           (:status (gateway/require-local-attempt! preflight))))
    (is (= 0 (:owned-execute-tx-call-entries
              (gateway/require-local-attempt! preflight))))
    (is (instance? Throwable
                   (try ((:submit! throwing)) nil (catch Throwable e e))))
    (is (= :threw-outcome-unknown
           (:status (gateway/require-local-attempt! throwing))))
    (doseq [g [preflight throwing]]
      (reject! :not-this-gateway-local-return
               #(gateway/require-local-ledger-observation!
                 g {:tx-key nil :diagnostic ((:summary! g))}))
      (reject! :one-shot-already-attempted (:submit! g)))))

(deftest stale-xtdb-conflict-cannot-be-misattributed-to-gateway-return
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            outsider (next-document before)
            external-key (xt/execute-tx
                          (:node app) [[:put-docs physical/table outsider]])
            failure (try ((:submit! g)) nil (catch Throwable e e))]
        (is (instance? TransactionKey external-key))
        (is (instance? Throwable failure))
        (is (= :threw-outcome-unknown
               (:status (gateway/require-local-attempt! g))))
        (reject! :not-this-gateway-local-return
                 #(gateway/require-local-ledger-observation!
                   g {:tx-key external-key :diagnostic ((:summary! g))}))
        (is (= outsider ((:read! app))))))))

(deftest external-writer-can-change-poststate-without-changing-ledger-join
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))
            prior (checked-ledger!
                   g result :reported-key-matches-committed-ledger-row
                   {:tx-id true :committed true :system-time true})
            outside (assoc before :status "outside" :audit-tag secret)
            outsider-key (xt/execute-tx
                          (:node app) [[:put-docs physical/table outside]])
            later (checked-ledger!
                   g result :reported-key-matches-committed-ledger-row
                   {:tx-id true :committed true :system-time true})
            row (first (xt/q (:node app)
                             ["SELECT * FROM gesso_v1494_reservation_slots WHERE _id = ?"
                              (:xt/id before)]))]
        (is (instance? TransactionKey outsider-key))
        (is (= "outside" (:status row)))
        (is (= secret (:audit-tag row)))
        (is (= (:ledger-status prior) (:ledger-status later)))
        (is (= (:ledger-checks prior) (:ledger-checks later)))
        (is (not= (next-document before) row))
        (is (= :not-established (:physical-poststate-causality later)))
        (is (= 1 (:owned-execute-tx-call-entries
                  (gateway/require-local-attempt! g))))))))

(deftest concurrent-ledger-reads-never-retry-one-shot-submission
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))
            expected {:tx-id true :committed true :system-time true}
            jobs (mapv (fn [_]
                         (future (gateway/require-local-ledger-observation!
                                  g result)))
                       (range 5))]
        (try
          (let [reports (mapv #(deref % 60000 :timed-out) jobs)]
            (is (not-any? #{:timed-out} reports))
            (is (= 5 (count reports)))
            (doseq [r reports]
              (is (= :reported-key-matches-committed-ledger-row
                     (:ledger-status r)))
              (is (= expected (:ledger-checks r)))
              (is (= :not-established (:committed-transaction r))))
            (reject! :one-shot-already-attempted (:submit! g))
            (is (= (next-document before) ((:read! app))))
            (is (= 1 (:owned-execute-tx-call-entries
                      (gateway/require-local-attempt! g)))))
          (finally
            (doseq [job jobs] (future-cancel job))))))))

(deftest closed-submission-node-produces-a-read-failure-not-a-fake-commit
  ;; The physical fixture closes its node AFTER the real gateway return.
  ;; A later ledger query must remain diagnostic and must never retry writes.
  (let [retained (atom nil)]
    (with-physical
      (fn [app before]
        (let [g (gate (:node app) [before])
              result ((:submit! g))]
          (is (instance? TransactionKey (:tx-key result)))
          (reset! retained [g result])
          (is (= (next-document before) ((:read! app)))))))
    (let [[g result] @retained
          evidence (checked-ledger! g result :ledger-query-failed {})]
      (is (= :not-established (:committed-transaction evidence)))
      (is (= :not-established (:xtdb-receipt evidence)))
      (is (= :not-established (:physical-poststate-causality evidence)))
      (is (= :returned-transaction-key
             (:status (gateway/require-local-attempt! g))))
      (reject! :one-shot-already-attempted (:submit! g)))))

(deftest substituted-ledger-query-cannot-invent-a-matching-or-divergent-row
  (with-physical
    (fn [app before]
      (let [g (gate (:node app) [before])
            result ((:submit! g))
            actual (checked-ledger!
                    g result :reported-key-matches-committed-ledger-row
                    {:tx-id true :committed true :system-time true})]
        (reject! :substituted-gateway-or-upstream-seam
                 #(with-redefs [xt/q (fn [& _]
                                       [{:xt/id -1 :committed false
                                         :system-time java.time.Instant/EPOCH}])]
                    (gateway/require-local-ledger-observation! g result)))
        (is (= actual (gateway/require-local-ledger-observation! g result)))
        (is (= (next-document before) ((:read! app))))))))

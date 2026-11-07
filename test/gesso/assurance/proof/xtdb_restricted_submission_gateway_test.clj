(ns gesso.assurance.proof.xtdb-restricted-submission-gateway-test
  "v1526: Permanent adversarial checks of v1525's opt-in, one-use XTDB sink.

   These are bounded tests of the *actual owned execute-tx call*, not a proof
   of application authority, physical causality, transaction-internal prestate,
   complete writer coverage, or universal refinement. Bootstrap and hostile
   XTDB writes in this fixture deliberately bypass the gateway; they are
   separate physical writers and must never be hidden in an assurance report.

   A genuine TransactionKey means XTDB accepted the API call, not that the
   caller has established a certified abstract business transition. All
   uncertain outcomes remain uncertain; no automatic retries are performed."
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

(ns gesso.assurance.proof.xtdb-gateway-owned-nonerase-boundary-test
  "v1886: Real-XTDB check of one actual guarded writer's negative ERASE
   footprint. A direct ERASE bypass remains outside that local scope."
  (:require [clojure.test :refer [deftest is]]
            [gesso.assurance.proof.xtdb-gateway-atomic-write-intent :as intent]
            [gesso.model.command :as command]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway :as gate]
            [gesso.assurance.proof.xtdb-restricted-submission-gateway-test]
            [gesso.assurance.example.physical-single-node-proving :as physical]
            [xtdb.api :as xt]))

(def ^:private fixture-ns
  'gesso.assurance.proof.xtdb-restricted-submission-gateway-test)
(defn- original [sym]
  (var-get (or (ns-resolve fixture-ns sym)
               (throw (ex-info "Missing original gateway fixture" {:symbol sym})))))
(defn- source [before] ((original 'source-for) [before]))
(defn- reason [f] ((original 'rejection) f))

(deftest v1886-one-owned-execute-tx-has-no-erase-while-direct-erase-remains-open
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (gate/prepare-one-shot! (:node app) (source before))
           audit! gate/require-local-nonerase-write-boundary!]
       (is (= :not-this-gateway-local-return
              (reason #(audit! g nil))))
       (let [result ((:submit! g))
             report (audit! g result)]
         (is (= gate/local-nonerase-type (:type report)))
         (is (= :this-one-source-derived-owned-xtdb-submission-only
                (:scope report)))
         (is (= 1 (:original-owned-call-entries report)))
         (is (= 3 (:source-derived-operation-count report)))
         (is (= 2 (:source-derived-assertion-count report)))
         (is (= 1 (:source-derived-put-count report)))
         (is (zero? (:source-derived-erase-count report)))
         (is (zero? (:source-derived-delete-count report)))
         (is (= :established-for-this-owned-vector-only
                (:owned-erase-op-exclusion report)))
         (is (= :not-established
                (:outside-gateway-erase-coverage report)
                (:history-erasure-exclusion report)
                (:transaction-commit report)))
         (is (= :not-installed (:physical-erasure-event-producer report)))
         (is (= :not-issued (:certificate report)))
         (is (= :none (:authority report) (:independent-audit-discharge report)))
         (is (= report (audit! (into {} g) result)))
         (is (= :not-this-gateway-local-return
                (reason #(audit! g (into {} result)))))
         (is (= :not-this-gateway-local-return
                (reason #(audit! (assoc g :submit! (fn [] nil)) result))))
         (doseq [forbidden [:tx-ops :tx-key :node :source :document
                            :physical-row-id :principal :signed-event]]
           (is (not (contains? report forbidden))))
         ;; An UNREGISTERED writer can ERASE the same physical row after
         ;; the genuine admitted PUT. Its deletion must never appear to be
         ;; covered, prevented, or cryptographically audited by this report.
         (xt/execute-tx (:node app)
                        [[:erase-docs physical/table (:xt/id before)]])
         (is (= :missing-or-duplicate-slot
                (try ((:read! app)) :still-present
                     (catch clojure.lang.ExceptionInfo e
                       (:reason (ex-data e))))))
         (is (= report (audit! g result)))
         (is (= :not-established
                (:complete-deployment-writer-coverage (audit! g result)))))))))

(deftest v1886-copied-or-foreign-return-cannot-claim-owned-nonerase
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           a (gate/prepare-one-shot! (:node app) (source before))
           b (gate/prepare-one-shot! (:node app) (source before))
           r ((:submit! a))]
       (is (= :not-this-gateway-local-return
              (reason #(gate/require-local-nonerase-write-boundary! b r))))
       (is (= :not-this-gateway-local-return
              (reason #(gate/require-local-nonerase-write-boundary!
                        a (assoc r :certificate :issued)))))
       (is (= 0 (:source-derived-erase-count
                 (gate/require-local-nonerase-write-boundary! a r))))))))

(defn- intent-reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(defn- intent-rows [node]
  (xt/q node "SELECT * FROM gesso_guarded_write_intents"))

(deftest v1887-intent-document-is-produced-with-same-original-guarded-write
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))]
       (is (= gate/gateway-type (:type g)))
       (is (= #{:type :version :submit! :summary!} (set (keys g))))
       (is (= :prepared (:status ((:summary! g)))))
       (is (empty? (intent-rows (:node app)))
           "Preparation must not create any audit document")
       (let [result ((:submit! g))
             local (gate/require-local-nonerase-write-boundary! g result)
             records (intent-rows (:node app))]
         (is (= :returned-transaction-key (:status ((:summary! g)))))
         (is (= :established-for-this-owned-vector-only
                (:owned-erase-op-exclusion local)))
         (is (= 2 (:source-derived-put-count local)))
         (is (zero? (:source-derived-erase-count local)))
         (is (= 1 (count records)))
         (let [record (first records)]
           (is (= intent/intent-kind (:intent-kind record)))
           (is (= 1 (:application-command-count record)))
           (is (= "one-admitted-xtdb-transaction-vector-only"
                  (:intent-scope record)))
           (is (= 0 (:revision record)))
           (is (string? (:xt/id record)))
           (doseq [forbidden [:tx-key :principal :source :physical-row-id
                              :audit-signature :certificate :authority]]
             (is (not (contains? record forbidden)))))
         (is (= "reserved" (:status ((:read! app)))))
         (is (= :not-established (:transaction-commit local)
                (:durable-audit-record local)
                (:complete-deployment-writer-coverage local)))
         (is (= :not-issued (:certificate local))))))))

(deftest v1887-rejected-protected-write-cannot-create-intent-alone
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))
           external (assoc before :updated-at "competing-writer"
                          :revision (inc (:revision before)))]
       (xt/execute-tx (:node app) [[:put-docs physical/table external]])
       (let [failure (try ((:submit! g)) nil
                          (catch Throwable x x))]
         (is (instance? Throwable failure))
         (is (= :xtdb/assert-failed
                (:xtdb.error/code (ex-data failure))))
         (is (= :threw-outcome-unknown (:status ((:summary! g)))))
         (is (empty? (intent-rows (:node app)))
             "The original guarded prewrite failure must abort the intent")
         (is (= external ((:read! app)))))))))

(deftest v1887-intent-table-is-not-original-caller-write-scope
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           base (source before)
           reason (fn [src]
                    (try (intent/prepare-one-shot! (:node app) src)
                         :accepted
                         (catch clojure.lang.ExceptionInfo x
                           (:reason (ex-data x)))))]
       (is (= :reserved-intent-table-or-unsupported-plan
              (reason (update-in base [:protected-documents]
                                 conj {:target [intent/intent-table "forged"]
                                       :fields :all}))))
       (is (= :invalid-original-source
              (reason (assoc base :audit-intent "caller-control"))))
       (is (= :reserved-intent-table-or-unsupported-plan
              (reason (update-in base [:plan :commands]
                                 conj (command/create
                                       intent/intent-table
                                       {:xt/id "forged" :revision 0
                                        :created-at "today" :updated-at "today"}
                                       {:revision-key :revision
                                        :created-at-key :created-at
                                        :updated-at-key :updated-at}))))))
       (is (empty? (intent-rows (:node app)))))))

(deftest v1888-original-generated-intent-and-own-key-share-pinned-readback
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))
           inspect! intent/inspect-pinned-intent!]
       (is (= :not-this-gateway-local-return
              (reason #(inspect! g nil))))
       (let [returned ((:submit! g))
             report (inspect! g returned)]
         (is (= intent/readback-type (:type report)))
         (is (= intent/readback-version (:version report)))
         (is (= :bounded-private-intent-and-ledger-coobservation
                (:status report)))
         (is (= :matches-private-generated-intent
                (:intent-record-status report)))
         (is (= :matches-original-returned-committed-row
                (:returned-ledger-status report)))
         (is (= :pinned-basis-not-before-original-return
                (:read-basis-order report)))
         (is (true? (:both-observations-share-pinned-basis? report)))
         (is (= :private-original-prepared-gateway
                (:expected-intent-origin report)))
         (is (= :one-open-visible-history-version-starts-at-returned-tx
                (:intent-history-origin-status report)))
         (is (= :bounded-same-pinned-basis-history-start-matches-returned-tx
                (:returned-tx-time-intent-history-correlation report)))
         (is (= 2 (:intent-history-observation-limit report)))
         (is (= :not-established (:intent-history-completeness report)))
         (is (= :exact-one-shot-gateway-return (:returned-key-origin report)))
         (is (= :not-established (:transaction-causality report)
                (:audit-event-producer report)
                (:outside-gateway-writer-coverage report)
                (:record-immutability report)))
         (is (= :none (:physical-erasure-proof report)
                (:transaction-authority report) (:retry-authority report)))
         (is (= :not-issued (:certificate report)))
         (is (= :not-this-gateway-local-return
                (reason #(inspect! g (into {} returned)))))
         (doseq [field [:xt/id :record :document :node :tx-key :snapshot-token
                        :physical-table :source :principal :intent-id]]
           (is (not (contains? report field))))
         (is (= report (inspect! g returned)))
         (let [row (first (intent-rows (:node app)))]
           (is (map? row))
           (xt/execute-tx (:node app)
                          [[:put-docs intent/intent-table
                            (assoc row :intent-kind "forged-intent-value")]])
           (let [tampered (inspect! g returned)]
             (is (= :intent-or-ledger-correspondence-not-established
                    (:status tampered)))
             (is (= :diverges-from-private-generated-intent
                    (:intent-record-status tampered)))
             (is (= :matches-original-returned-committed-row
                    (:returned-ledger-status tampered)))
             (is (= :not-established
                    (:returned-tx-time-intent-history-correlation tampered)))
             (is (= :none (:authority tampered))))
           ;; ERASE by an unrelated direct writer can remove the generated
           ;; record, while the original transaction's ledger row survives.
           ;; Absence at this later basis is not proof of original noncommit.
           (xt/execute-tx (:node app)
                          [[:erase-docs intent/intent-table (:xt/id row)]])
           (let [absent (inspect! g returned)]
             (is (= :intent-or-ledger-correspondence-not-established
                    (:status absent)))
             (is (= :not-visible (:intent-record-status absent)))
             (is (= :no-visible-pinned-intent-history
                    (:intent-history-origin-status absent)))
             (is (= :not-established
                    (:returned-tx-time-intent-history-correlation absent)))
             (is (= :matches-original-returned-committed-row
                    (:returned-ledger-status absent)))
             (is (= :not-issued (:certificate absent))))))))))

(deftest v1888-foreign-valid-gateway-has-no-private-generated-intent
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           plain (gate/prepare-one-shot! (:node app) (source before))
           returned ((:submit! plain))]
       (is (= :not-original-generated-intent-descriptor
              (intent-reason #(intent/inspect-pinned-intent! plain returned))))))))

(deftest v1889-same-value-external-put-invalidates-unique-history-origin
  ;; Reading the unchanged current document PLUS the original committed
  ;; xt.txs entry cannot establish that its current version still began at
  ;; the guarded transaction. A same-value outside PUT leaves those two
  ;; matches intact but creates additional bitemporal row history records.
  (physical/with-proving-application!
   (fn [app]
     (let [g (intent/prepare-one-shot! (:node app)
                                       (source ((:read! app))))
           returned ((:submit! g))
           first-read (intent/inspect-pinned-intent! g returned)
           row (first (intent-rows (:node app)))]
       (is (= :bounded-same-pinned-basis-history-start-matches-returned-tx
              (:returned-tx-time-intent-history-correlation first-read)))
       (is (= :bounded-private-intent-and-ledger-coobservation
              (:status first-read)))
       (is (map? row))
       ;; An UNREGISTERED, unadmitted writer rewrites exactly the same value.
       ;; Its different transaction changes system/valid-time history while
       ;; the current generated fields stay equal to the original intent.
       (xt/execute-tx (:node app)
                      [[:put-docs intent/intent-table row]])
       (let [later (intent/inspect-pinned-intent! g returned)]
         (is (= :matches-private-generated-intent
                (:intent-record-status later)))
         (is (= :matches-original-returned-committed-row
                (:returned-ledger-status later)))
         (is (= :bounded-private-intent-and-ledger-coobservation
                (:status later)))
         (is (= :multiple-visible-pinned-intent-history-records-at-bound
                (:intent-history-origin-status later)))
         (is (= :not-established
                (:returned-tx-time-intent-history-correlation later)
                (:intent-history-completeness later)
                (:transaction-causality later)
                (:outside-gateway-writer-coverage later)))
         (is (= :none (:transaction-authority later)
                (:authority later)))
         (is (= :not-issued (:certificate later)))
         (is (not-any? #(contains? later %)
                       [:xt/id :record :document :node :tx-key
                        :snapshot-token :system-from :system-to])))))))

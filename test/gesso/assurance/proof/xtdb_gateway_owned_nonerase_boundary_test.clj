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
         (is (= :matches-private-original-protected-after
                (:protected-original-poststate-status report)))
         (is (= :bounded-three-way-physical-coobservation
                (:protected-intent-ledger-coobservation report)))
         (is (= :one-original-canonical-command-at-same-pinned-basis
                (:protected-original-poststate-scope report)))
         (is (false? (:protected-original-poststate-proves-transaction-causality?
                      report)))
         (is (= :latest-open-protected-version-starts-at-returned-tx
                (:protected-original-history-origin-status report)))
         (is (= :bounded-dual-row-history-and-ledger-coobservation
                (:protected-intent-ledger-temporal-correlation report)))
         (is (= :private-original-before-matches-immediate-predecessor
                (:protected-original-before-history-status report)))
         (is (= :private-after-matches-latest-historical-version
                (:protected-original-after-history-status report)))
         (is (= :private-original-after-compared-with-pinned-latest-historical-version
                (:protected-after-history-scope report)))
         (is (= :not-established
                (:protected-historical-after-completeness report)))
         (is (= :bounded-private-before-after-intent-ledger-coobservation
                (:protected-before-after-intent-ledger-correlation report)))
         (let [e (:correspondence-premises report)]
           (is (= :gesso.assurance.proof.xtdb-gateway-atomic-write-intent/diagnostic-premises
                  (:type e)))
           (is (= 1 (:version e)))
           (is (= :protected-before-after-history-correlation
                  (:strongest-bounded-node e)))
           (is (= 11 (count (:premises e))))
           (is (= 5 (count (:dependency-nodes e))))
           (is (every? #(= :observational-match (:status %))
                       (vals (:premises e))))
           (is (every? #(= :bounded-premises-present (:status %))
                       (vals (:dependency-nodes e))))
           (is (= 7 (:open-obligation-count e)))
           (is (= [] (:closed-obligations e)))
           (is (= :not-established (:record-execution-causality e)
                  (:audit-producer-completeness e)
                  (:history-completeness e)))
           (is (= :diagnostic-plain-data-not-a-proof-object
                  (:proof-status e)))
           (is (= :not-issued (:certificate e)))
           (is (= :none (:authority e) (:transaction-authority e)))
           (let [frontier (:assurance-frontier e)]
             (is (= :gesso.assurance.proof.xtdb-gateway-atomic-write-intent/unmet-witness-contract
                    (:type frontier)))
             (is (= 7 (:open-count frontier)))
             (is (= [] (:closed-obligations frontier)))
             (is (every? #(= :unmet (:status %))
                         (vals (:requirements frontier))))
             (is (every? #(false? (:witness-checked? %))
                         (vals (:requirements frontier))))
             (is (= :not-implemented (:trusted-witness-validation frontier)))
             (is (false? (:requirements-sufficient-for-certification?
                          frontier)))
             (is (= :not-issued (:certificate frontier)))
             (is (= :none (:authority frontier)
                    (:proof-discharge frontier)))))
         (is (= :not-established (:protected-before-history-completeness report)))
         (is (false? (:protected-before-after-correlation-proves-causality?
                      report)))
         (is (= 2 (:protected-original-history-observation-limit report)))
         (is (= :not-established
                (:protected-original-history-completeness report)))
         (is (false? (:protected-temporal-correlation-proves-transaction-causality?
                      report)))
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
         (is (= :matches-private-original-protected-after
                (:protected-original-poststate-status later)))
         (is (= :not-established
                (:protected-intent-ledger-coobservation later)))
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

(deftest v1890-external-protected-write-breaks-three-way-with-unchanged-intent
  ;; An unrelated direct writer can change the actual application row while
  ;; the original generated intent, its single historical version and the
  ;; original committed transaction ledger all continue to match. A report
  ;; that checks ONLY intent/ledger would miss this divergence.
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))
           returned ((:submit! g))
           baseline (intent/inspect-pinned-intent! g returned)]
       (is (= :bounded-three-way-physical-coobservation
              (:protected-intent-ledger-coobservation baseline)))
       (is (= :matches-private-original-protected-after
              (:protected-original-poststate-status baseline)))
       (let [current ((:read! app))]
         ;; The unregistered direct writer is deliberately NOT admitted by
         ;; the original gateway and cannot alter its privately held source.
         (xt/execute-tx (:node app)
                        [[:put-docs physical/table
                          (assoc current :audit-tag "outside_writer")]])
         (let [later (intent/inspect-pinned-intent! g returned)]
           (is (= :bounded-private-intent-and-ledger-coobservation
                  (:status later)))
           (is (= :matches-private-generated-intent
                  (:intent-record-status later)))
           (is (= :matches-original-returned-committed-row
                  (:returned-ledger-status later)))
           (is (= :one-open-visible-history-version-starts-at-returned-tx
                  (:intent-history-origin-status later)))
           (is (= :bounded-same-pinned-basis-history-start-matches-returned-tx
                  (:returned-tx-time-intent-history-correlation later)))
           (is (= :diverges-from-private-original-after
                  (:protected-original-poststate-status later)))
           (is (= :newer-protected-system-version-than-returned-tx
                  (:protected-original-history-origin-status later)))
           (is (= :not-established
                  (:protected-intent-ledger-temporal-correlation later)))
           (is (= :not-established
                  (:protected-intent-ledger-coobservation later)
                  (:transaction-causality later)
                  (:outside-gateway-writer-coverage later)))
           (is (false?
                (:protected-original-poststate-proves-transaction-causality?
                 later)))
           (is (= :none (:transaction-authority later)
                  (:authority later)))
           (is (= :not-issued (:certificate later)))
           (is (not-any? #(contains? later %)
                         [:protected-document :protected-after :protected-row-id
                          :protected-table :source :node :tx-key :snapshot-token
                          :original-plan :original-command]))))))))

(deftest v1890-same-value-protected-rewrite-still-cannot-prove-causality
  ;; This is the deliberate LIMIT of current-document three-way readback:
  ;; a different physical writer can replay the same after-document. All
  ;; current-value and intent/ledger observations may remain positive even
  ;; though the application row received an unregistered second write.
  (physical/with-proving-application!
   (fn [app]
     (let [g (intent/prepare-one-shot! (:node app)
                                       (source ((:read! app))))
           returned ((:submit! g))
           original (intent/inspect-pinned-intent! g returned)
           after ((:read! app))]
       (is (= :bounded-three-way-physical-coobservation
              (:protected-intent-ledger-coobservation original)))
       (xt/execute-tx (:node app) [[:put-docs physical/table after]])
       (let [replayed (intent/inspect-pinned-intent! g returned)]
         (is (= :bounded-three-way-physical-coobservation
                (:protected-intent-ledger-coobservation replayed)))
         (is (= :matches-private-original-protected-after
                (:protected-original-poststate-status replayed)))
         (is (= :newer-protected-system-version-than-returned-tx
                (:protected-original-history-origin-status replayed)))
         (let [e (:correspondence-premises replayed)]
           (is (= :protected-current-coobservation (:strongest-bounded-node e)))
           (is (= :observational-match
                  (get-in e [:premises :protected-current-after :status])))
           (is (= :unestablished
                  (get-in e [:premises :protected-latest-origin :status])))
           (is (= :blocked
                  (get-in e [:dependency-nodes
                             :protected-before-after-history-correlation :status])))
           (is (some #{:protected-latest-origin}
                     (get-in e [:dependency-nodes
                                :protected-before-after-history-correlation
                                :blocked-by])))
           (is (= [] (:closed-obligations e))))
         (is (= :bounded-three-way-physical-coobservation
                (:protected-intent-ledger-coobservation replayed)))
         (is (= :not-established
                (:protected-intent-ledger-temporal-correlation replayed)))
         (is (= :latest-protected-version-not-origin-anchored
                (:protected-original-before-history-status replayed)))
         (is (= :latest-protected-version-not-origin-anchored
                (:protected-original-after-history-status replayed)))
         (is (= :not-established
                (:protected-before-after-intent-ledger-correlation replayed)))
         (is (false? (:protected-temporal-correlation-proves-transaction-causality?
                      replayed)))
         (is (= :bounded-same-pinned-basis-history-start-matches-returned-tx
                (:returned-tx-time-intent-history-correlation replayed)))
         (is (= :not-established
                (:transaction-causality replayed)
                (:outside-gateway-writer-coverage replayed)
                (:record-immutability replayed)))
         (is (false?
              (:protected-original-poststate-proves-transaction-causality?
               replayed)))
         (is (= :none (:transaction-authority replayed) (:authority replayed)))
         (is (= :not-issued (:certificate replayed))))))))

(deftest v1891-external-erase-removes-protected-history-but-not-private-intent
  ;; The positive baseline is observational only. An OUTSIDE writer can
  ;; ERASE the application row and its visible history, while the generated
  ;; intent and original transaction ledger remain visible and unchanged.
  (physical/with-proving-application!
   (fn [app]
     (let [before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))
           returned ((:submit! g))
           original (intent/inspect-pinned-intent! g returned)]
       (is (= :bounded-dual-row-history-and-ledger-coobservation
              (:protected-intent-ledger-temporal-correlation original)))
       (xt/execute-tx (:node app)
                      [[:erase-docs physical/table (:xt/id before)]])
       (let [after (intent/inspect-pinned-intent! g returned)]
         (is (= :matches-private-generated-intent
                (:intent-record-status after)))
         (is (= :matches-original-returned-committed-row
                (:returned-ledger-status after)))
         (is (= :not-visible
                (:protected-original-poststate-status after)))
         (is (= :no-visible-pinned-protected-history
                (:protected-original-history-origin-status after)))
         (is (= :latest-protected-version-not-origin-anchored
                (:protected-original-before-history-status after)))
         (is (= :latest-protected-version-not-origin-anchored
                (:protected-original-after-history-status after)))
         (is (= :not-established
                (:protected-before-after-intent-ledger-correlation after)))
         (is (= :not-established
                (:protected-intent-ledger-temporal-correlation after)
                (:protected-original-history-completeness after)
                (:transaction-causality after)
                (:outside-gateway-writer-coverage after)))
         (is (= :none (:transaction-authority after) (:authority after)))
         (is (= :not-issued (:certificate after)))
         (is (false? (:protected-temporal-correlation-proves-transaction-causality?
                      after))))))))

(deftest v1892-false-admitted-before-is-not-a-pinned-predecessor
  ;; The declared before can disagree in a NON-guarded field with the real
  ;; XTDB predecessor even though version checks, submitted PUT, intent and
  ;; the original transaction ledger succeed. This correspondence check
  ;; must not certify the invented before-document as physically observed.
  (physical/with-proving-application!
   (fn [app]
     (let [real-before ((:read! app))
           original-source (source real-before)
           wrong-before (assoc real-before :audit-tag "invented-prior-value")
           forged (assoc-in original-source
                            [:plan :commands 0 :model/before] wrong-before)
           g (intent/prepare-one-shot! (:node app) forged)
           returned ((:submit! g))
           report (intent/inspect-pinned-intent! g returned)]
       (is (= :bounded-dual-row-history-and-ledger-coobservation
              (:protected-intent-ledger-temporal-correlation report)))
       (is (= :matches-private-original-protected-after
              (:protected-original-poststate-status report)))
       (is (= :latest-open-protected-version-starts-at-returned-tx
              (:protected-original-history-origin-status report)))
       (is (= :immediate-predecessor-diverges-from-private-before
              (:protected-original-before-history-status report)))
       (let [e (:correspondence-premises report)]
         (is (= :protected-dual-history-correlation
                (:strongest-bounded-node e)))
         (is (= [:protected-historical-before]
                (get-in e [:dependency-nodes
                           :protected-before-after-history-correlation
                           :blocked-by])))
         (is (= :unestablished
                (get-in e [:premises :protected-historical-before :status])))
         (is (= 7 (:open-obligation-count e)))
         (is (= :not-issued (:certificate e))))
       (is (= :not-established
              (:protected-before-after-intent-ledger-correlation report)
              (:protected-before-history-completeness report)
              (:transaction-causality report)))
       (is (= :none (:transaction-authority report) (:authority report)))
       (is (= :not-issued (:certificate report)))
       (is (false? (:protected-before-after-correlation-proves-causality?
                    report)))
       (is (not-any? #(contains? report %)
                     [:expected-before :historical-before :original-before
                      :before-document :protected-document :snapshot-token]))))))

(deftest v1893-private-historical-after-rejects-mismatch-and-wrong-valid-start
  ;; Feed an intentionally divergent historical post-image to the PURE
  ;; classifier (not to an XTDB query seam). A current-row match and system
  ;; start timestamp must not silently stand in for historical payload.
  (let [check (var-get (or (ns-resolve
                             'gesso.assurance.proof.xtdb-gateway-atomic-write-intent
                             'historical-after-correspondence)
                           (throw (ex-info "Missing historical verifier" {}))))
        tx-time (java.time.Instant/parse "2026-11-19T19:00:00Z")
        private {:xt/id "selected-v1893" :revision 7 :status "accepted"}
        historical (assoc private :xt/valid-from
                          (java.time.ZonedDateTime/ofInstant
                           tx-time (java.time.ZoneId/of "UTC")))]
    (is (= :private-after-matches-latest-historical-version
           (check private historical tx-time)))
    (is (= :latest-historical-after-diverges-from-private-after
           (check private (assoc historical :status "wrong") tx-time)))
    (is (= :latest-historical-after-diverges-from-private-after
           (check private (assoc historical :unexpected-field true) tx-time)))
    (is (= :latest-historical-valid-start-differs-from-returned-tx
           (check private (assoc historical :xt/valid-from
                                 (.plusSeconds tx-time 1)) tx-time)))
    (is (= :latest-historical-valid-start-differs-from-returned-tx
           (check private (dissoc historical :xt/valid-from) tx-time)))))

(deftest v1893-real-xtdb-historical-after-matches-private-expected-write
  (physical/with-proving-application!
   (fn [app]
     (let [original ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source original))
           returned ((:submit! g))
           baseline (intent/inspect-pinned-intent! g returned)]
       (is (= :private-after-matches-latest-historical-version
              (:protected-original-after-history-status baseline)))
       (is (= :bounded-private-before-after-intent-ledger-coobservation
              (:protected-before-after-intent-ledger-correlation baseline)))
       ;; An unregistered same-value PUT changes the latest SYSTEM version,
       ;; so even matching application current values cannot preserve the
       ;; claimed original historical post-image.
       (xt/execute-tx (:node app)
                      [[:put-docs physical/table ((:read! app))]])
       (let [later (intent/inspect-pinned-intent! g returned)]
         (is (= :latest-protected-version-not-origin-anchored
                (:protected-original-after-history-status later)))
         (is (= :not-established
                (:protected-before-after-intent-ledger-correlation later)
                (:protected-historical-after-completeness later)
                (:transaction-causality later)))
         (is (false? (:protected-before-after-correlation-proves-causality?
                      later)))
         (is (= :none (:transaction-authority later) (:authority later)))
         (is (= :not-issued (:certificate later))))))))

(deftest v1894-composed-premises-fail-closed-and-never-certify
  (let [compose (var-get (or (ns-resolve
                              'gesso.assurance.proof.xtdb-gateway-atomic-write-intent
                              'compose-diagnostic-correspondence)
                            (throw (ex-info "Missing diagnostic composer" {}))))
        empty (compose {})
        observed (into {}
                       (for [[_ {:keys [source-field expected]}]
                             (:premises empty)]
                         [source-field expected]))
        spoofed (compose observed)
        missing (compose (dissoc observed :intent-record-status))
        mutated (compose (assoc observed :protected-original-after-history-status
                                :latest-historical-after-diverges-from-private-after))]
    (is (= :none (:strongest-bounded-node empty)))
    (is (= :missing-observation
           (get-in empty [:premises :committed-ledger :observed])))
    (is (= :blocked
           (get-in empty [:dependency-nodes :intent-ledger-coobservation :status])))
    (is (= :protected-before-after-history-correlation
           (:strongest-bounded-node spoofed))
        "Synthetic input can mimic all redacted observations, so the composer is not an attestor")
    (is (= :diagnostic-plain-data-not-a-proof-object (:proof-status spoofed)))
    (is (= :not-established (:evidence-authenticity spoofed)
           (:record-execution-causality spoofed)))
    (is (= :none (:authority spoofed) (:transaction-authority spoofed)))
    (is (= :not-issued (:certificate spoofed)))
    (is (= [] (:closed-obligations spoofed)))
    (is (= 7 (:open-obligation-count spoofed)))
    (is (= :none (:strongest-bounded-node missing)))
    (is (some #{:generated-intent}
              (get-in missing [:dependency-nodes
                               :protected-before-after-history-correlation
                               :blocked-by])))
    (is (= :protected-dual-history-correlation
           (:strongest-bounded-node mutated)))
    (is (= [:protected-historical-after]
           (get-in mutated [:dependency-nodes
                            :protected-before-after-history-correlation
                            :blocked-by])))
    (is (= :none (:transaction-authority mutated)))
    (is (= [] (:closed-obligations mutated)))))

(deftest v1895-required-independent-witnesses-cannot-be-forged-from-diagnostics
  (let [ns 'gesso.assurance.proof.xtdb-gateway-atomic-write-intent
        fn! (fn [name]
              (var-get (or (ns-resolve ns name)
                           (throw (ex-info "Missing contract implementation"
                                           {:name name})))))
        compose (fn! 'compose-diagnostic-correspondence)
        frontier! (fn! 'require-open-assurance-frontier)
        empty (compose {})
        synthetic-claims
        (into {} (for [[_ {:keys [source-field expected]}]
                       (:premises empty)]
                   [source-field expected]))
        matched (compose synthetic-claims)
        open (frontier! matched)
        missing (frontier! (compose {}))]
    ;; Even a synthetically perfect report grants no independent witness.
    (is (= :protected-before-after-history-correlation
           (:strongest-bounded-node matched)))
    (is (= 7 (count (:requirements open))))
    (is (= (:requirements open) (:requirements missing))
        "Missing and complete observational premises require identical independent witnesses")
    (is (= (set (:open-obligations matched))
           (set (keys (:requirements open)))))
    (is (= [] (:closed-obligations open)))
    (is (= :none (:proof-discharge open) (:transaction-authority open)
           (:retry-authority open) (:authority open)))
    (is (= :not-issued (:certificate open)))
    (is (= :not-implemented (:trusted-witness-validation open)))
    (doseq [[id {:keys [status requires-independent-witness independent-of
                       witness-checked? may-use-this-diagnostic-as-discharge?]}]
            (:requirements open)]
      (is (keyword? id))
      (is (= :unmet status))
      (is (keyword? requires-independent-witness))
      (is (keyword? independent-of))
      (is (false? witness-checked?))
      (is (false? may-use-this-diagnostic-as-discharge?)))
    ;; The bounded *observation* can be spoofed, but no mutation may make
    ;; this assurance requirement boundary treat it as authenticated.
    (doseq [forged [(assoc matched :closed-obligations
                          [:exclusive-transaction-causality])
                    (assoc matched :open-obligations [])
                    (assoc matched :open-obligation-count 0)
                    (assoc matched :certificate :issued)
                    (assoc matched :authority :write)
                    (assoc matched :proof-status :certified)
                    (assoc matched :evidence-authenticity :established)
                    (dissoc matched :proof-status)]]
      (is (= :inconsistent-open-assurance-obligations
             (intent-reason #(frontier! forged)))))))

(deftest v1895-physical-intent-and-ledger-still-leave-all-independent-witnesses-open
  (physical/with-proving-application!
   (fn [app]
     (let [g (intent/prepare-one-shot! (:node app)
                                       (source ((:read! app))))
           returned ((:submit! g))
           observed (intent/inspect-pinned-intent! g returned)
           requirements (get-in observed
                                [:correspondence-premises :assurance-frontier])]
       (is (= :bounded-private-before-after-intent-ledger-coobservation
              (:protected-before-after-intent-ledger-correlation observed)))
       (is (= 7 (:open-count requirements)))
       (is (= :not-implemented (:trusted-witness-validation requirements)))
       (is (= :unmet
              (get-in requirements [:requirements
                                    :exclusive-transaction-causality :status])))
       (is (= :unmet
              (get-in requirements [:requirements
                                    :complete-deployment-writer-coverage :status])))
       (is (= :none (:proof-discharge requirements)
              (:transaction-authority requirements) (:authority requirements)))
       (is (= :not-issued (:certificate requirements)))))))

(defn- v1896-sign [bytes private-key]
  (let [signer (java.security.Signature/getInstance "Ed25519")]
    (.initSign signer private-key)
    (.update signer ^bytes bytes)
    (.encodeToString (java.util.Base64/getEncoder) (.sign signer))))

(deftest v1896-signed-candidate-is-bound-to-real-gateway-but-not-a-proof
  (physical/with-proving-application!
   (fn [app]
     (let [issue (requiring-resolve
                  'gesso.assurance.proof.xtdb-gateway-witness-candidate/issue-candidate-challenge!)
           verify! (requiring-resolve
                    'gesso.assurance.proof.xtdb-gateway-witness-candidate/inspect-signed-candidate!)
           key-generator (java.security.KeyPairGenerator/getInstance "Ed25519")
           kp (.generateKeyPair key-generator)
           other (.generateKeyPair key-generator)
           g (intent/prepare-one-shot! (:node app) (source ((:read! app))))
           returned ((:submit! g))
           challenge (issue g returned)
           payload (.decode (java.util.Base64/getDecoder)
                            ^String (:signing-bytes-base64 challenge))
           signed {:signature-base64 (v1896-sign payload (.getPrivate kp))}]
       (is (= :exclusive-transaction-causality (:obligation challenge)))
       (is (= :transaction-coupled-apply-provenance (:witness-family challenge)))
       (is (= :none (:proof-discharge challenge) (:authority challenge)))
       (is (= :not-issued (:certificate challenge)))
       (is (= :not-an-original-outstanding-challenge
              (try (verify! (into {} challenge) signed (.getPublic kp))
                   nil
                   (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex))))))
       (let [checked (verify! challenge signed (.getPublic kp))]
         (is (= :ed25519-valid-for-caller-supplied-public-key
                (:cryptographic-check checked)))
         (is (= :not-established
                (:public-key-trust checked)
                (:signer-independence checked)
                (:physical-event-production checked)
                (:transaction-causality checked)))
         (is (false? (:witness-checked-for-obligation? checked)))
         (is (true? (:open-obligations-remain-open? checked)))
         (is (= :not-admitted (:witness-admission checked)))
         (is (= :none (:authority checked) (:proof-discharge checked)
                (:retry-authority checked) (:transaction-authority checked)))
         (is (= :not-issued (:certificate checked)))
         (is (not-any? #(contains? checked %)
                       [:tx-key :snapshot-token :original-document :private-key
                        :signed-payload :row-id :node :intent-id])))
       (is (= :not-an-original-outstanding-challenge
              (try (verify! challenge signed (.getPublic kp)) nil
                   (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex))))))
       (let [fresh (issue g returned)]
         (is (= :candidate-signature-not-verified
                (try (verify! fresh signed (.getPublic kp)) nil
                     (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))))
       (let [fresh (issue g returned)
             bytes (.decode (java.util.Base64/getDecoder)
                            ^String (:signing-bytes-base64 fresh))]
         (is (= :candidate-signature-not-verified
                (try (verify! fresh
                              {:signature-base64 (v1896-sign bytes (.getPrivate kp))}
                              (.getPublic other))
                     nil
                     (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))))
       (let [fresh (issue g returned)]
         (is (= :invalid-candidate-response
                (try (verify! fresh (assoc signed :certificate :issued)
                              (.getPublic kp)) nil
                     (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))))
       (let [fresh (issue g returned)]
         (is (= :invalid-candidate-signature
                (try (verify! fresh {:signature-base64 "not_base64$"}
                              (.getPublic kp)) nil
                     (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))))
       (let [fresh (issue g returned)]
         (is (= :not-an-original-outstanding-challenge
                (try (verify! (assoc fresh :authority :write) signed
                              (.getPublic kp)) nil
                     (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))))
       ;; The caller could generate both keys and signatures, so a valid
       ;; signature cannot independently attest transaction causality.
       (let [report (intent/inspect-pinned-intent! g returned)]
         (is (= 7 (get-in report
                         [:correspondence-premises :assurance-frontier :open-count])))
         (is (= :none (get-in report
                             [:correspondence-premises :assurance-frontier
                              :proof-discharge]))))))))

(deftest v1896-genuine-observation-required-to-issue-challenge
  (physical/with-proving-application!
   (fn [app]
     (let [issue (requiring-resolve
                  'gesso.assurance.proof.xtdb-gateway-witness-candidate/issue-candidate-challenge!)
           before ((:read! app))
           g (intent/prepare-one-shot! (:node app) (source before))]
       (is (= :not-this-gateway-local-return
              (reason #(issue g {}))))
       (let [returned ((:submit! g))]
         (xt/execute-tx (:node app)
                        [[:put-docs physical/table
                          (assoc ((:read! app)) :changed-by "external")]])
         (is (= :incomplete-private-observational-premises
                (try (issue g returned) nil
                     (catch clojure.lang.ExceptionInfo ex
                       (:reason (ex-data ex)))))))))))

(defn- v1897-key-config [pair]
  {:ed25519-spki-base64
   (.encodeToString (java.util.Base64/getEncoder)
                    (.getEncoded (.getPublic pair)))})

(deftest v1897-configured-key-must-be-canonical-ed25519-spki
  (let [prepare (requiring-resolve
                 'gesso.assurance.proof.xtdb-gateway-pinned-candidate-key/prepare-process-pinned-verifier!)
        generator (java.security.KeyPairGenerator/getInstance "Ed25519")
        pair (.generateKeyPair generator)
        config (v1897-key-config pair)
        rsa (.generateKeyPair (doto (java.security.KeyPairGenerator/getInstance "RSA")
                                 (.initialize 2048)))
        failed (fn [cfg] (try (prepare cfg) nil
                              (catch clojure.lang.ExceptionInfo ex
                                (:reason (ex-data ex)))))
        pinned (prepare config)]
    (is (= :gesso.assurance.proof.xtdb-gateway-pinned-candidate-key/process-pinned-verifier
           (:type pinned)))
    (is (= 16 (:challenge-cap pinned)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (:pinned-spki-fingerprint pinned)))
    (is (= :not-established (:trusted-key-provisioning pinned)
           (:signer-independence pinned) (:physical-execution-causality pinned)))
    (is (= :not-admitted (:witness-admission pinned)))
    (is (= :not-issued (:certificate pinned)))
    (is (= :none (:authority pinned)))
    (is (= :invalid-pinned-key-configuration (failed {})))
    (is (= :invalid-pinned-key-configuration
           (failed (assoc config :claim-independent true))))
    (is (= :invalid-pinned-ed25519-spki (failed {:ed25519-spki-base64 "@"})))
    (is (= :invalid-pinned-ed25519-spki
           (failed {:ed25519-spki-base64
                    (:ed25519-spki-base64 (v1897-key-config rsa))})))
    (is (= (:pinned-spki-fingerprint pinned)
           (:pinned-spki-fingerprint (prepare config))))))

(deftest v1897-pinned-verifier-rejects-swapped-and-foreign-challenges
  (physical/with-proving-application!
   (fn [app]
     (let [prepare (requiring-resolve
                    'gesso.assurance.proof.xtdb-gateway-pinned-candidate-key/prepare-process-pinned-verifier!)
           generator (java.security.KeyPairGenerator/getInstance "Ed25519")
           own (.generateKeyPair generator)
           other (.generateKeyPair generator)
           v (prepare (v1897-key-config own))
           foreign (prepare (v1897-key-config other))
           g (intent/prepare-one-shot! (:node app)
                                       (source ((:read! app))))
           returned ((:submit! g))
           issued ((:issue! v) g returned)
           bytes (.decode (java.util.Base64/getDecoder)
                          ^String (:signing-bytes-base64 issued))
           own-signature {:signature-base64
                          (v1896-sign bytes (.getPrivate own))}]
       (is (= :not-issued-by-this-pinned-verifier
              (intent-reason #((:inspect! foreign) issued own-signature))))
       (is (= :not-issued-by-this-pinned-verifier
              (intent-reason #((:inspect! v) (into {} issued) own-signature))))
       (let [verified ((:inspect! v) issued own-signature)]
         (is (= :ed25519-valid-for-process-configured-key
                (:cryptographic-check verified)))
         (is (= (:pinned-spki-fingerprint v)
                (:pinned-spki-fingerprint verified)))
         (is (= :excluded (:per-inspection-public-key-substitution verified)))
         (is (= :process-configuration-not-authenticated
                (:key-configuration-origin verified)))
         (is (= :not-established (:key-custody verified)
                (:key-provisioning-independence verified)
                (:signer-observation-of-physical-event verified)
                (:transaction-causality verified)
                (:separately-attested-witness-identity verified)))
         (is (false? (:witness-checked-for-obligation? verified)))
         (is (= :not-admitted (:witness-admission verified)))
         (is (= :none (:authority verified) (:transaction-authority verified)
                (:proof-discharge verified)))
         (is (= :not-issued (:certificate verified)))
         (is (not-any? #(contains? verified %)
                       [:public-key :private-key :tx-key :document :node
                        :snapshot-token :signed-payload :physical-row-id])))
       (is (= :not-issued-by-this-pinned-verifier
              (intent-reason #((:inspect! v) issued own-signature))))
       (let [fresh ((:issue! v) g returned)
             new-bytes (.decode (java.util.Base64/getDecoder)
                                ^String (:signing-bytes-base64 fresh))]
         (is (= :candidate-signature-not-verified
                (intent-reason #((:inspect! v) fresh
                                  {:signature-base64
                                   (v1896-sign new-bytes (.getPrivate other))}))))
         (is (= :not-issued-by-this-pinned-verifier
                (intent-reason #((:inspect! v) fresh own-signature)))))
       (let [fresh ((:issue! v) g returned)]
         (is (= :invalid-candidate-response
                (intent-reason #((:inspect! v) fresh
                                 {:signature-base64 "bogus" :certificate :issued})))))
       ;; The signer can configure its OWN key, so even correct verification
       ;; must not claim custody, identity, physical observation or discharge.
       (let [own-configured (prepare (v1897-key-config other))
             self-issued ((:issue! own-configured) g returned)
             self-bytes (.decode (java.util.Base64/getDecoder)
                                 ^String (:signing-bytes-base64 self-issued))
             self-verified ((:inspect! own-configured) self-issued
                            {:signature-base64
                             (v1896-sign self-bytes (.getPrivate other))})]
         (is (= :ed25519-valid-for-process-configured-key
                (:cryptographic-check self-verified)))
         (is (= :not-established (:key-provisioning-independence self-verified)))
         (is (= :none (:proof-discharge self-verified)))
         (is (= :not-issued (:certificate self-verified))))
       ;; An outside writer changes pinned correspondence: no new witness
       ;; challenges may be issued by this preconfigured key verifier.
       (xt/execute-tx (:node app)
                      [[:put-docs physical/table
                        (assoc ((:read! app)) :outside-write true)]])
       (is (= :incomplete-private-observational-premises
              (intent-reason #((:issue! v) g returned))))))))

(defn- v1898-grant [delegate]
  (let [now (java.time.Instant/now)]
    {:version 1
     :delegation-id (str (java.util.UUID/randomUUID))
     :audience "gesso.xtdb.transaction-causality-candidate.v1"
     :obligation :exclusive-transaction-causality
     :witness-family :transaction-coupled-apply-provenance
     :delegated-ed25519-spki-base64
     (:ed25519-spki-base64 (v1897-key-config delegate))
     :valid-from (str (.minusSeconds now 60))
     :valid-until (str (.plusSeconds now 3600))}))

(defn- v1898-delegation-config [root grant]
  (let [signing-bytes
        ((requiring-resolve
          'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegation-signing-bytes)
         grant)]
    {:root-ed25519-spki-base64
     (:ed25519-spki-base64 (v1897-key-config root))
     :delegation grant
     :root-signature-base64 (v1896-sign signing-bytes (.getPrivate root))}))

(deftest v1898-role-scoped-signed-key-delegation-is-not-independent-trust
  (let [prepare (requiring-resolve
                 'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/prepare-delegated-candidate-verifier!)
        generator (java.security.KeyPairGenerator/getInstance "Ed25519")
        root (.generateKeyPair generator)
        other-root (.generateKeyPair generator)
        delegate (.generateKeyPair generator)
        grant (v1898-grant delegate)
        config (v1898-delegation-config root grant)
        prepared (prepare config)
        invalid (fn [x] (intent-reason #(prepare x)))]
    (is (= :gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegated-candidate-verifier
           (:type prepared)))
    (is (= :valid-for-configured-root-key
           (:delegation-signature-check prepared)))
    (is (= :exclusive-transaction-causality-candidate-only
           (:delegation-scope prepared)))
    (is (not= (:configured-root-spki-fingerprint prepared)
              (:delegated-key-spki-fingerprint prepared)))
    (is (= :process-supplied-not-authenticated
           (:root-configuration-origin prepared)))
    (is (= :not-established
           (:root-independent-provisioning prepared)
           (:delegate-independent-physical-observation prepared)
           (:revocation-and-complete-issuance-inventory prepared)))
    (is (= :none (:authority prepared) (:proof-discharge prepared)))
    (is (= :not-issued (:certificate prepared)))
    ;; A valid root signature proves possession of a process-chosen root
    ;; key, NOT that its operator or custody is independent of the caller.
    (is (= :root-signature-not-verified
           (invalid (assoc config :root-ed25519-spki-base64
                           (:ed25519-spki-base64
                            (v1897-key-config other-root))))))
    (is (= :root-signature-not-verified
           (invalid (assoc-in config [:delegation :delegation-id]
                              (str (java.util.UUID/randomUUID))))))
    (is (= :root-signature-not-verified
           (invalid (assoc-in config [:delegation :delegated-ed25519-spki-base64]
                              (:ed25519-spki-base64
                               (v1897-key-config other-root))))))
    (is (= :invalid-delegation-scope
           (invalid (assoc-in config [:delegation :audience] "another-app"))))
    (is (= :invalid-delegation-scope
           (invalid (assoc-in config [:delegation :witness-family]
                              :unrelated-proof))))
    (is (= :invalid-delegation-scope
           (invalid (assoc-in config [:delegation :unreviewed] true))))
    (is (= :invalid-delegation-configuration
           (invalid (assoc config :admit-witness? true))))
    (is (= :root-must-differ-from-delegate
           (invalid (v1898-delegation-config root (v1898-grant root)))))
    (let [now (java.time.Instant/now)
          expired (assoc grant
                         :valid-from (str (.minusSeconds now 7200))
                         :valid-until (str (.minusSeconds now 3600)))
          not-yet (assoc grant
                         :valid-from (str (.plusSeconds now 3600))
                         :valid-until (str (.plusSeconds now 7200)))
          excessive (assoc grant
                           :valid-from (str (.minusSeconds now 60))
                           :valid-until (str (.plusSeconds now 90000)))]
      (is (= :delegation-not-currently-valid
             (invalid (v1898-delegation-config root expired))))
      (is (= :delegation-not-currently-valid
             (invalid (v1898-delegation-config root not-yet))))
      (is (= :invalid-delegation-window-or-id
             (intent-reason #(v1898-delegation-config root excessive))))
      (is (= :invalid-delegation-window-or-id
             (intent-reason #(v1898-delegation-config root
                                                       (assoc grant :delegation-id
                                                              "not-a-uuid"))))))))

(deftest v1898-delegated-key-candidate-stays-untrusted-on-real-xtdb
  (physical/with-proving-application!
   (fn [app]
     (let [prepare (requiring-resolve
                    'gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/prepare-delegated-candidate-verifier!)
           generator (java.security.KeyPairGenerator/getInstance "Ed25519")
           root (.generateKeyPair generator)
           delegate (.generateKeyPair generator)
           outside (.generateKeyPair generator)
           pinned (prepare (v1898-delegation-config root
                                                    (v1898-grant delegate)))
           g (intent/prepare-one-shot! (:node app)
                                       (source ((:read! app))))
           returned ((:submit! g))
           issued ((:issue! pinned) g returned)
           bytes (.decode (java.util.Base64/getDecoder)
                          ^String (:signing-bytes-base64 issued))
           response {:signature-base64
                     (v1896-sign bytes (.getPrivate delegate))}
           checked ((:inspect! pinned) issued response)]
       (is (= :gesso.assurance.proof.xtdb-gateway-delegated-candidate-key/delegated-candidate-check
              (:type checked)))
       (is (= :ed25519-valid-for-process-configured-key
              (:cryptographic-check checked)))
       (is (= :valid-for-configured-root-key
              (:delegation-signature-check checked)))
       (is (= :not-established
              (:delegation-root-independent-provisioning checked)
              (:delegate-key-custody checked)
              (:physical-event-correspondence checked)
              (:transaction-causality checked)
              (:signer-observation-of-physical-event checked)))
       (is (false? (:witness-checked-for-obligation? checked)))
       (is (= :not-admitted (:witness-admission checked)))
       (is (= :none (:authority checked) (:transaction-authority checked)
              (:retry-authority checked) (:proof-discharge checked)))
       (is (= :not-issued (:certificate checked)))
       (is (not-any? #(contains? checked %)
                     [:tx-key :node :snapshot-token :delegation-id
                      :root-private-key :delegate-private-key :signed-event]))
       (is (= :not-issued-by-this-pinned-verifier
              (intent-reason #((:inspect! pinned) issued response))))
       (let [fresh ((:issue! pinned) g returned)
             fresh-bytes (.decode (java.util.Base64/getDecoder)
                                  ^String (:signing-bytes-base64 fresh))]
         (is (= :candidate-signature-not-verified
                (intent-reason #((:inspect! pinned) fresh
                                 {:signature-base64
                                  (v1896-sign fresh-bytes
                                              (.getPrivate outside))})))))
       ;; A later physical change prevents a NEW challenge, independently
       ;; of the correctness of the root-delegation signature.
       (xt/execute-tx (:node app)
                      [[:put-docs physical/table
                        (assoc ((:read! app)) :external-change true)]])
       (is (= :incomplete-private-observational-premises
              (intent-reason #((:issue! pinned) g returned))))))))

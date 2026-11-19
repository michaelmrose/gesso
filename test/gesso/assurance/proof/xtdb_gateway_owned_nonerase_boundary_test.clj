(ns gesso.assurance.proof.xtdb-gateway-owned-nonerase-boundary-test
  "v1886: Real-XTDB check of one actual guarded writer's negative ERASE
   footprint. A direct ERASE bypass remains outside that local scope."
  (:require [clojure.test :refer [deftest is]]
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

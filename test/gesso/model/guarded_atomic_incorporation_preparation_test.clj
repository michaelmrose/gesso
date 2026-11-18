(ns gesso.model.guarded-atomic-incorporation-preparation-test
  "v1771: The real model.tx preparation path must retain every ASSERT and
   guarded domain+command-ledger write. This is NOT an installed writer."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.guarded-atomic-incorporation-preparation :as guarded]
            [gesso.model.atomic-incorporation-footprint-test]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.tx :as tx]))

(def ^:private fixture-ns 'gesso.model.atomic-incorporation-footprint-test)
(defn- fixture [name]
  (var-get (or (ns-resolve fixture-ns name)
               (throw (ex-info "Missing original fixture" {:name name})))))
(defn- source
  ([] (source {}))
  ([options]
   (let [{:keys [binding incorporated]} ((fixture 'case-source) options)
         record (:record incorporated)]
     {:binding binding
      :now (:gesso.command/created-at record)
      :outcome (:gesso.command/outcome record)
      :plan (update (:plan incorporated) :commands pop)})))
(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest canonical-full-atomic-preparation-has-guarded-xtdb-layout
  (let [submitted (atom 0)
        src (source)
        issued (with-redefs [tx/transact! (fn [& _]
                                           (swap! submitted inc)
                                           (throw (ex-info "must not transact" {})))]
                 (guarded/issue-guarded-atomic-preparer! src))
        initial ((:summary! issued))
        result (with-redefs [tx/transact! (fn [& _]
                                           (swap! submitted inc)
                                           (throw (ex-info "must not transact" {})))]
                 ((:prepare-once! issued)))]
    (is (= guarded/report-type (:type result) (:type initial)))
    (is (= :issued-not-submitted (:status initial)))
    (is (= :guarded-atomic-prepared-not-submitted (:status result)))
    (is (= :included-in-same-atomic-plan (:incorporated-ledger result)))
    (is (= :explicit-then-dependencies-then-command-preconditions
           (:guard-order result)))
    (is (= :domain-then-command-ledger (:write-order result)))
    (is (= :checked-by-independent-isolated-preparation
           (:formatted-xtdb-footprint result)))
    (is (= :preserved-without-publication (:live-options result)))
    (is (= :disabled (:physical-submission result)))
    (is (= :not-established (:xtdb-commit result)))
    (is (= :not-established (:actual-database-prestate result)))
    (is (= :not-established (:typed-complete-cas-gateway-adoption result)))
    (is (= :not-issued (:certificate result)))
    (is (= :none (:transaction-authority result)))
    (is (zero? @submitted))
    (is (not-any? #(contains? result %)
                  [:plan :tx-ops :candidate :writer :submit! :source
                   :record :binding :entry-fn :result :execution :ctx]))
    (is (= :preparation-attempt-consumed (:status ((:summary! issued)))))
    (is (= :preparation-already-attempted
           (reason (:prepare-once! issued))))))

(deftest independent-layout-check-detects-dropped-ledger-absence-assert
  (let [actual tx/transaction-ops
        altered (fn [plan]
                  (let [ops (actual plan)]
                    (if (< 1 (count (:commands plan)))
                      ;; Delete one precondition in the full plan only; each
                      ;; isolated one-command validation still sees the real
                      ;; precondition. The v1770 equality check alone would
                      ;; accept two identically altered prepared plans.
                      (vec (concat (butlast (take (- (count ops)
                                                     (count (:commands plan)))
                                                  ops))
                                   (drop (- (count ops)
                                            (count (:commands plan))) ops)))
                      ops)))]
    (with-redefs [tx/transaction-ops altered]
      (is (= :prepared-guard-assert-write-layout-diverged
             (reason #(guarded/issue-guarded-atomic-preparer! (source))))))))

(deftest full-plan-guard-cannot-be-removed-from-compiled-ops
  (let [actual tx/transaction-ops
        altered (fn [plan]
                  (let [ops (actual plan)]
                    (if (and (< 1 (count (:commands plan)))
                             (seq (:guards plan)))
                      (vec (concat [(first ops)] (drop 2 ops)))
                      ops)))]
    (with-redefs [tx/transaction-ops altered]
      (is (= :prepared-guard-assert-write-layout-diverged
             (reason #(guarded/issue-guarded-atomic-preparer! (source))))))))

(deftest original-construction-is-canonical-and-not-caller-fabricated
  (let [src (source)
        with-ledger (update-in src [:plan :commands] conj
                               (last (:commands
                                      (:plan (resolution/incorporation-plan src)))))
        cases {:extra (assoc src :prepared {:tx-ops []})
               :missing (dissoc src :outcome)
               :wrong-binding (assoc-in src [:binding :principal] :forged)
               :domain-ledger with-ledger}]
    (doseq [[label changed] cases]
      (testing (name label)
        (is (#{:invalid-original-incorporation-source
                :invalid-canonical-incorporation}
             (reason #(guarded/issue-guarded-atomic-preparer! changed))))))))

(deftest negative-preparer-substitution-consumes-single-attempt
  (let [issued (guarded/issue-guarded-atomic-preparer! (source))]
    (with-redefs [tx/prepare (fn [& _] {:plan :forged :tx-ops []})]
      (is (= :substituted-incorporation-or-preparation-boundary
             (reason (:prepare-once! issued)))))
    (is (= :preparation-already-attempted
           (reason (:prepare-once! issued))))))

(deftest source-callback-never-executed-by-guarded-preparation
  (let [calls (atom 0)
        src (source {:entry nil
                     :entry-fn (fn [& _]
                                 (swap! calls inc)
                                 {:event :should-not-run})})
        issued (guarded/issue-guarded-atomic-preparer! src)
        result ((:prepare-once! issued))]
    (is (= :guarded-atomic-prepared-not-submitted (:status result)))
    (is (zero? @calls))))

(deftest canonical-source-and-physical-ops-never-escape-preparation-report
  (let [issued (guarded/issue-guarded-atomic-preparer! (source))
        summary ((:summary! issued))
        prepared ((:prepare-once! issued))]
    (doseq [result [summary prepared]]
      (is (= :none (:authority result)))
      (is (= :not-admitted (:admission result)))
      (is (= :disabled (:physical-submission result)))
      (is (= #{:type :version :status :scope :incorporated-ledger
               :guard-order :write-order :formatted-xtdb-footprint
               :live-options :physical-submission
               :typed-complete-cas-gateway-adoption :actual-database-prestate
               :business-policy-authenticity :xtdb-commit
               :deployed-writer-coverage :certificate :admission
               :transaction-authority :authority}
             (set (keys result)))))))

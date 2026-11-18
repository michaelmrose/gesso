(ns gesso.model.atomic-incorporation-footprint-test
  "v1770: A future typed writer must preserve the full incorporated plan and
   Gesso Live options, not just a domain UPDATE or a matching tx-op vector."
  (:require [clojure.test :refer [deftest is testing]]
            [gesso.model.atomic-incorporation-footprint :as footprint]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.resolved-operation-test]
            [gesso.model.tx :as tx]))

(def ^:private originals 'gesso.model.resolved-operation-test)
(defn- fixture [symbol]
  (var-get (or (ns-resolve originals symbol)
               (throw (ex-info "Missing existing fixture" {:name symbol})))))

(defn- case-source
  ([] (case-source {}))
  ([options]
   (let [{:keys [request membership binding]} ((fixture 'fixture-data))
         domain (merge ((fixture 'domain-plan) request membership)
                       {:assertions [{:assert [:= 1 1]}]
                        :changes [{:kind :request/claimed :subject (:xt/id request)}]
                        :emit :sync
                        :entry {:event :request/claimed}
                        :tx-options {:metadata {:check :v1770}}}
                       options)
         incorporated (resolution/incorporation-plan
                       {:binding binding :outcome {:status :claimed}
                        :now "t1" :plan domain})]
     {:binding binding :incorporated incorporated})))

(defn- candidate [incorporated]
  (tx/prepare nil (:plan incorporated)))

(defn- reason [f]
  (try (f) :no-error
       (catch clojure.lang.ExceptionInfo ex (:reason (ex-data ex)))))

(deftest full-incorporation-plan-can-be-prepared-without-execution
  (let [{:keys [binding incorporated]} (case-source)
        submitted (atom 0)
        report (with-redefs [tx/transact! (fn [& _] (swap! submitted inc)
                                            (throw (ex-info "no writes" {})))]
                 (footprint/check-full-atomic-candidate!
                  binding incorporated (candidate incorporated)))]
    (is (= footprint/report-type (:type report)))
    (is (= footprint/report-version (:version report)))
    (is (= :source-relative-uninstalled-full-atomic-plan (:scope report)))
    (is (= :checked (:original-atomic-command-incorporation report)))
    (is (= :exact (:complete-normalized-plan report)))
    (is (= :exact (:prepared-assert-and-write-operations report)))
    (is (= :exact-plan-only (:live-publication-inputs report)))
    (is (= :not-established (:actual-live-publication report)))
    (is (= :not-established (:typed-complete-cas-writer-adoption report)))
    (is (= :not-established (:physical-xtdb-commit report)))
    (is (= :not-established (:original-business-policy-authenticity report)))
    (is (= :not-issued (:certificate report)))
    (is (= :not-admitted (:admission report)))
    (is (= :none (:transaction-authority report)))
    (is (zero? @submitted))
    (is (not-any? #(contains? report %)
                  [:binding :record :plan :tx-ops :command :receipt
                   :response :request :handler :writer :node :catalog]))))

(deftest omitting-the-ledger-is-not-atomic-incorporation
  (let [{:keys [binding incorporated]} (case-source)
        source (:plan incorporated)
        domain-only (update source :commands pop)]
    (is (= :candidate-changes-original-transaction-plan
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated (tx/prepare nil domain-only)))))
    (is (= :incomplete-or-nonatomic-original-ledger
           (reason #(footprint/check-full-atomic-candidate!
                     binding (assoc incorporated :plan domain-only)
                     (tx/prepare nil domain-only)))))))

(deftest preserving-the-prepared-write-vector-is-not-enough
  (let [{:keys [binding incorporated]} (case-source)
        source (candidate incorporated)]
    ;; Some of these options do not alter the physical :tx-ops, but losing
    ;; them changes live publication or transaction-time behavior.
    (doseq [[label altered]
            [[:guard (update-in source [:plan :guards] empty)]
             [:assertion (assoc-in source [:plan :assertions] [])]
             [:live-changes (assoc-in source [:plan :changes] [{:kind :changed}])]
             [:emit (assoc-in source [:plan :emit] false)]
             [:entry (assoc-in source [:plan :entry] {:event :other})]
             [:tx-options (assoc-in source [:plan :tx-options] {:metadata {:check :other}})]
             [:ordered-commands (update-in source [:plan :commands]
                                          #(vec (reverse %)))]]]
      (testing (name label)
        (is (= :candidate-changes-original-transaction-plan
               (reason #(footprint/check-full-atomic-candidate!
                         binding incorporated altered))))))
    (is (= :invalid-candidate-transaction-plan
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated
                     (assoc-in source [:plan :changes] [])))))))

(deftest forged-or-missing-formatted-operations-never-qualify
  (let [{:keys [binding incorporated]} (case-source)
        prepared (candidate incorporated)]
    (doseq [altered [(dissoc prepared :tx-ops)
                     (assoc prepared :tx-ops [])
                     (update prepared :tx-ops pop)
                     (assoc prepared :tx-ops (conj (:tx-ops prepared)
                                                   [:put-docs :rogue {:xt/id :rogue}]))]]
      (is (#{:invalid-full-atomic-candidate-shape
             :candidate-physical-operations-diverge}
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated altered)))))))

(deftest mismatched-command-binding-and-original-record-are-rejected
  (let [{:keys [binding incorporated]} (case-source)
        prepared (candidate incorporated)]
    (is (= :invalid-command-ledger-record
           (reason #(footprint/check-full-atomic-candidate!
                     binding (assoc-in incorporated [:record :gesso.command/status]
                                       :rejected) prepared))))
    (is (= :incomplete-or-nonatomic-original-ledger
           (reason #(footprint/check-full-atomic-candidate!
                     binding (update-in incorporated [:plan :commands]
                                        (fn [commands] (vec (reverse commands)))) prepared))))
    (is (= :invalid-original-binding
           (reason #(footprint/check-full-atomic-candidate!
                     (assoc binding :principal "forged") incorporated prepared))))))

(deftest entry-function-is-preserved-but-never-executed
  (let [calls (atom 0)
        effect (fn [& _] (swap! calls inc) {:event :should-not-run})
        {:keys [binding incorporated]} (case-source {:entry nil :entry-fn effect})
        prepared (candidate incorporated)]
    (is (= :exact (:complete-normalized-plan
                  (footprint/check-full-atomic-candidate!
                   binding incorporated prepared))))
    (is (zero? @calls))
    (is (= :candidate-changes-original-transaction-plan
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated
                     (assoc-in prepared [:plan :entry-fn] (fn [& _] :other))))))
    (is (zero? @calls))))

(deftest unsupported-source-and-candidate-are-not-published-as-proof
  (let [{:keys [binding incorporated]} (case-source)
        prepared (candidate incorporated)]
    (is (= :invalid-original-incorporation-shape
           (reason #(footprint/check-full-atomic-candidate!
                     binding (assoc incorporated :unchecked true) prepared))))
    (is (= :invalid-full-atomic-candidate-shape
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated (assoc prepared :submit! (fn [] :write))))))
    (is (= :invalid-candidate-transaction-plan
           (reason #(footprint/check-full-atomic-candidate!
                     binding incorporated
                     (assoc-in prepared [:plan :emit] :invalid)))))))

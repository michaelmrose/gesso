(ns gesso.assurance.proof.operation-writer-coverage-soundness-test
  "Permanent v1367 adversarial tests for the v1366 source-derived join
   between independent induction and a currently issued application.
   They use an actual closed application, issued executor and Ring handler,
   never a fabricated successful proof report. The host enumeration is an
   observed snapshot; hidden writers and physical refinement remain open."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as bridge]
            [gesso.assurance.proof.abstract-execution-induction :as induction]
            [gesso.assurance.application-induction :as application]
            [gesso.assurance.application-induction-test]
            [gesso.assurance.proof.abstract-execution-induction-test :as example]
            [gesso.assurance.proof.literal-assignment-soundness-test :as literals]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight.certified-contract :as certified]))

(def ^:private op :example/claim)
(def ^:private extra :example/direct-writer)
(def ^:dynamic *source* nil)
(def ^:dynamic *snapshot* nil)
(def ^:dynamic *handler* nil)

(defn- abstract-catalog [ids]
  (let [original (first (filter #(= :request/claim (:id %))
                                (:operations example/base)))]
    (assoc example/base
           :declared-operation-ids (set ids)
           :operations (mapv (fn [id]
                              (-> original
                                  (assoc :id id)
                                  (assoc-in [:declaration :subject :id] id)
                                  (assoc-in [:declaration :subject :interface :operation] id)))
                            ids))))

(use-fixtures :once
  (fn [run]
    (let [context (@(ns-resolve 'gesso.assurance.application-induction-test
                                'make-context))
          {:keys [proof plan-source induction-source executor handler]} context]
      (fixture/with-closed-application
        {:operations {op (fixture/trusted-operation
                          op {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [snapshot (atom {op {:kind :semantic-route :handler handler}})
                contract {:inductive-source proof
                          :application-assembly assembly
                          :semantic-route-handlers {op handler}
                          :certified-operations
                          {op {:executor executor :current-source plan-source}}}
                observed {:inductive-contract-source contract
                          :enumerate-entry-points! (fn [] @snapshot)}
                issued {:authoritative-coverage-source observed
                        :inductive-plans
                        {op {:executor executor :current-source induction-source}}}
                source {:induction-catalog (abstract-catalog [op])
                        :application-induction-source issued}]
            (binding [*source* source *snapshot* snapshot *handler* handler]
              (run))))))))

(defn- observe
  ([] (bridge/derive-observed-writer-coverage! *source*))
  ([source] (bridge/derive-observed-writer-coverage! source)))
(defn- fail [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- unclassified [] {:kind :unclassified :handler (fn [_] {:status 200})})
(defn- changed-catalog [ids]
  (assoc *source* :induction-catalog (abstract-catalog ids)))
(defn- no-authority! [r]
  (doseq [[key expected]
          [[:certificate :not-issued]
           [:admission :not-admitted]
           [:authority :none]
           [:transaction-authority :none]
           [:proof-status :conditional-source-join-only]
           [:actual-physical-writer-completeness :not-established]
           [:host-writer-enumerator-complete? :not-established]
           [:abstract-transition-to-issued-plan-equivalence :not-established]
           [:xtdb-refinement :not-established]]]
    (is (= expected (get r key)) (str key " is not granted"))))
(defn- assert-stale! [old source]
  (is (= :stale-or-forged-observed-coverage-report
         (:reason (fail #(bridge/require-current-observed-writer-coverage!
                          old source))))))

(deftest source-bound-real-assembly-matches-identifiers-only
  (let [r (observe)]
    (is (= bridge/report-type (:type r)))
    (is (= bridge/report-version (:version r)))
    (is (= :conditional-observed-operation-id-join (:status r)))
    (is (= :independent-v1362-induction-and-current-v1294-application-sources
           (:checking r)))
    (is (= (:induction-catalog *source*) (:source-induction-catalog r)))
    (is (= #{op} (:independent-operation-ids r)))
    (is (= #{op} (:issued-application-operation-ids r)))
    (is (= #{op} (:observed-semantic-route-ids r)))
    (is (= #{op} (:observed-entry-point-ids r)))
    (is (empty? (:observed-unclassified-entry-point-ids r)))
    (is (true? (:exact-operation-id-match? r)))
    (is (true? (:conditional-id-join? r)))
    (no-authority! r)))

(deftest entire-operation-row-remains-unproved-cross-source
  (let [r (observe) row (first (:operation-rows r))]
    (is (= 1 (count (:operation-rows r))))
    (is (= op (:operation-id row)))
    (is (true? (:in-independent-induction-catalog? row)))
    (is (true? (:in-checked-issued-application? row)))
    (is (= :conditional-composed-rule-instance (:independent-step-status row)))
    (is (= :not-established (:cross-source-transition-equivalence row)))
    (is (= :not-established (:abstract-transition-to-issued-plan-equivalence r)))))

(deftest observational-scope-not-deployment-proof
  (let [r (observe)]
    (is (= :current-source-bound-host-observed-entry-point-snapshot-only
           (:scope r)))
    (is (= :not-established (:deployed-router-adoption r)))
    (is (= :not-established (:initializer-coverage r)))
    (is (= :not-established (:runtime-enforcement r)))
    (is (= :not-established (:universal-checker-soundness r)))
    (no-authority! r)))

(deftest external-writer-obligations-are-not-erased
  (let [open (:open-obligations (observe))]
    (is (every? open [:independent-v1362-and-application-v1294-operation-semantics-match
                      :host-entry-point-enumerator-is-complete
                      :all-direct-and-nonroute-xtdb-writers-are-enumerated
                      :deployed-router-adopts-checked-snapshot
                      :post-check-router-and-handler-stability
                      :actual-application-initializers-are-covered
                      :abstract-to-physical-xtdb-transition-refinement
                      :projection-and-write-footprint-completeness
                      :universal-s1-s4-checker-soundness
                      :certificate-admission]))))

(deftest declared-abstract-operation-not-issued-remains-open
  (let [r (observe (changed-catalog [op :example/additional]))]
    (is (= :open-operation-id-mismatch (:status r)))
    (is (= #{:example/additional} (:missing-issued-operation-ids r)))
    (is (empty? (:unmodeled-issued-operation-ids r)))
    (is (false? (:conditional-id-join? r)))
    (no-authority! r)))

(deftest real-issued-operation-not-modeled-remains-open
  (let [r (observe (changed-catalog [:example/unrelated]))]
    (is (= :open-operation-id-mismatch (:status r)))
    (is (= #{:example/unrelated} (:missing-issued-operation-ids r)))
    (is (= #{op} (:unmodeled-issued-operation-ids r)))
    (is (false? (:conditional-id-join? r)))
    (no-authority! r)))

(deftest multiple-abstract-operations-missing-from-issued
  (let [r (observe (changed-catalog [op :example/a :example/b]))
        rows (into {} (map (juxt :operation-id identity) (:operation-rows r)))]
    (is (= #{:example/a :example/b} (:missing-issued-operation-ids r)))
    (is (= [:example/a :example/b op] (mapv :operation-id (:operation-rows r))))
    (doseq [id [:example/a :example/b]]
      (is (true? (:in-independent-induction-catalog? (rows id))))
      (is (false? (:in-checked-issued-application? (rows id))))
      (is (= :not-established (:cross-source-transition-equivalence (rows id)))))
    (no-authority! r)))

(deftest mismatched-rows-show-both-sides-without-false-join
  (let [r (observe (changed-catalog [:example/unrelated]))
        rows (into {} (map (juxt :operation-id identity) (:operation-rows r)))]
    (is (false? (:in-independent-induction-catalog? (rows op))))
    (is (true? (:in-checked-issued-application? (rows op))))
    (is (true? (:in-independent-induction-catalog? (rows :example/unrelated))))
    (is (false? (:in-checked-issued-application? (rows :example/unrelated))))
    (is (every? #(= :not-established (:cross-source-transition-equivalence %))
                (vals rows)))))

(deftest unclassified-observed-writer-prevents-positive-status
  (swap! *snapshot* assoc extra (unclassified))
  (try
    (let [r (observe)]
      (is (= :open-observed-unclassified-entry-points (:status r)))
      (is (= #{extra} (:observed-unclassified-entry-point-ids r)))
      (is (true? (:exact-operation-id-match? r)))
      (is (false? (:conditional-id-join? r)))
      (no-authority! r))
    (finally (swap! *snapshot* dissoc extra))))

(deftest multiple-unclassified-paths-are-visible
  (swap! *snapshot* assoc extra (unclassified) :example/another (unclassified))
  (try
    (let [r (observe)]
      (is (= #{extra :example/another} (:observed-unclassified-entry-point-ids r)))
      (is (= #{op extra :example/another} (:observed-entry-point-ids r)))
      (is (= #{op} (:issued-application-operation-ids r)))
      (no-authority! r))
    (finally (swap! *snapshot* dissoc extra :example/another))))

(deftest unclassified-entry-is-not-implicitly-issued-or-modeled
  (swap! *snapshot* assoc extra (unclassified))
  (try
    (let [r (observe)]
      (is (not (contains? (:issued-application-operation-ids r) extra)))
      (is (not (contains? (:independent-operation-ids r) extra)))
      (is (not (contains? (set (map :operation-id (:operation-rows r))) extra))))
    (finally (swap! *snapshot* dissoc extra))))

(deftest operation-id-disagreement-precedes-unclassified-snapshot
  (swap! *snapshot* assoc extra (unclassified))
  (try
    (let [r (observe (changed-catalog [:example/unrelated]))]
      (is (= :open-operation-id-mismatch (:status r)))
      (is (= #{extra} (:observed-unclassified-entry-point-ids r)))
      (is (false? (:conditional-id-join? r))))
    (finally (swap! *snapshot* dissoc extra))))

(deftest removing-observed-extra-restores-only-the-conditional-join
  (swap! *snapshot* assoc extra (unclassified))
  (try
    (is (= :open-observed-unclassified-entry-points (:status (observe))))
    (finally (swap! *snapshot* dissoc extra)))
  (let [r (observe)]
    (is (= :conditional-observed-operation-id-join (:status r)))
    (no-authority! r)))

(deftest independent-base-case-failure-is-not-covered-by-ids
  (let [changed (assoc-in *source* [:induction-catalog :declared-initial-states]
                          [literals/before])
        r (observe changed)]
    (is (= :open-independent-induction-premises (:status r)))
    (is (= :open-base-or-step (:independent-induction-status r)))
    (is (true? (:exact-operation-id-match? r)))
    (is (false? (:conditional-id-join? r)))
    (no-authority! r)))

(deftest clean-host-enumeration-cannot-prove-concealed-writers
  (let [r (observe)]
    (is (empty? (:observed-unclassified-entry-point-ids r)))
    (is (contains? (:open-obligations r)
                   :all-direct-and-nonroute-xtdb-writers-are-enumerated))
    (is (= :not-established (:actual-physical-writer-completeness r)))
    (no-authority! r)))

(deftest fresh-source-recheck-accepts-unchanged-diagnostic-report
  (let [r (observe)]
    (is (= r (bridge/require-current-observed-writer-coverage! r *source*)))
    (no-authority! r)))

(deftest altered-report-authority-is-not-current
  (let [r (observe)]
    (doseq [[k value] [[:authority :granted]
                       [:certificate :issued]
                       [:admission :admitted]
                       [:transaction-authority :granted]
                       [:actual-physical-writer-completeness :proved]
                       [:abstract-transition-to-issued-plan-equivalence :proved]
                       [:host-writer-enumerator-complete? true]
                       [:proof-status :proved]]]
      (assert-stale! (assoc r k value) *source*))))

(deftest altered-operation-rows-and-obligations-are-not-current
  (let [r (observe)]
    (doseq [changed [(assoc r :operation-rows [])
                     (assoc r :open-obligations #{})
                     (assoc r :status :proved)
                     (assoc r :conditional-id-join? false)
                     (assoc-in r [:operation-rows 0 :cross-source-transition-equivalence]
                               :proved)]]
      (assert-stale! changed *source*))))

(deftest prior-report-must-be-plain-unannotated-map
  (let [r (observe)]
    (doseq [changed [nil [] #{} (sorted-map)
                     (with-meta r {:proof true})]]
      (assert-stale! changed *source*))))

(deftest report-can-round-trip-as-data-not-as-a-proof-certificate
  (let [r (observe)
        copy (edn/read-string (pr-str r))]
    (is (= r copy))
    (is (= r (bridge/require-current-observed-writer-coverage! copy *source*)))
    (no-authority! copy)))

(deftest prior-report-from-earlier-host-snapshot-is-stale
  (let [r (observe)]
    (swap! *snapshot* assoc extra (unclassified))
    (try (assert-stale! r *source*)
         (finally (swap! *snapshot* dissoc extra)))))

(deftest prior-report-from-different-operation-catalog-is-stale
  (let [r (observe)]
    (assert-stale! r (changed-catalog [op :example/additional]))))

(deftest host-is-reenumerated-at-every-check
  (let [calls (atom 0)
        changed (assoc-in *source*
                          [:application-induction-source
                           :authoritative-coverage-source
                           :enumerate-entry-points!]
                          (fn [] (swap! calls inc) @*snapshot*))]
    (is (= :conditional-observed-operation-id-join (:status (observe changed))))
    (is (= 1 @calls))
    (is (= :conditional-observed-operation-id-join (:status (observe changed))))
    (is (= 2 @calls))))

(deftest source-wrapper-rejects-extra-claimed-authority
  (doseq [key [:certificate :proof-status :authority
               :host-enumerator-complete? :physical-writer-completeness]]
    (let [data (fail #(observe (assoc *source* key :proved)))]
      (is (true? (:operation-writer-coverage-soundness/rejection data)))
      (is (= :wrong-source-keys (:reason data))))))

(deftest source-wrapper-rejects-unannotated-shape-errors
  (doseq [bad [nil [] (sorted-map) (dissoc *source* :induction-catalog)
               (dissoc *source* :application-induction-source)
               (with-meta *source* {:trusted true})]]
    (is (contains? #{:expected-unannotated-source-map :wrong-source-keys}
                   (:reason (fail #(observe bad)))))))

(deftest source-wrapper-rejects-forged-cached-report
  (is (= :wrong-source-keys
         (:reason (fail #(observe (assoc *source* :cached-report {:authority :none})))))))

(deftest malformed-abstract-source-does-not-join
  (doseq [catalog [nil [] (dissoc (:induction-catalog *source*) :operations)
                   (assoc (:induction-catalog *source*) :certificate :issued)]]
    (is (some? (fail #(observe (assoc *source* :induction-catalog catalog)))))))

(deftest missing-issued-plan-cannot-be-counted
  (is (some? (fail #(observe
                    (assoc-in *source*
                              [:application-induction-source :inductive-plans]
                              {}))))))

(deftest substituted-issued-plan-binding-cannot-be-counted
  (is (some? (fail #(observe
                    (assoc-in *source*
                              [:application-induction-source :inductive-plans op]
                              {:executor (fn [_] :fake)
                               :current-source {:inductive-source {}}}))))))

(deftest missing-current-semantic-route-is-rejected-upstream
  (reset! *snapshot* {})
  (try (is (some? (fail observe)))
       (finally (reset! *snapshot*
                        {op {:kind :semantic-route :handler *handler*}}))))

(deftest wrong-current-semantic-handler-is-rejected-upstream
  (reset! *snapshot* {op {:kind :semantic-route
                           :handler (fn [_] {:status 200})}})
  (try (is (some? (fail observe)))
       (finally (reset! *snapshot*
                        {op {:kind :semantic-route :handler *handler*}}))))

(deftest substituted-independent-induction-checker-is-rejected
  (with-redefs [induction/derive-induction-schema!
                (fn [_] {:status :conditional-declared-induction-schema})]
    (is (= :substituted-induction-checker (:reason (fail observe))))))

(deftest substituted-application-induction-checker-is-rejected
  (with-redefs [application/check-application-induction!
                (fn [_] {:observational-coverage :exact-for-supplied-host-snapshot})]
    (is (= :substituted-application-checker (:reason (fail observe))))))

(deftest failing-host-enumerator-does-not-make-coverage-pass
  (let [source (assoc-in *source*
                         [:application-induction-source
                          :authoritative-coverage-source
                          :enumerate-entry-points!]
                         (fn [] (throw (ex-info "host enumeration unavailable"
                                                {:host-unavailable true}))))]
    (is (= true (:host-unavailable (fail #(observe source)))))))

(deftest metadata-bearing-report-can-never-be-required-current
  (let [r (observe)]
    (assert-stale! (with-meta r {:authority :proved}) *source*)))

(ns gesso.live.application-preflight.authoritative-coverage-test
  "v1293: permanent adversarial tests for host-observed authoritative entry
   points. The positive path uses an actual Gesso application assembly, issued
   Ring handler, certified-plan executor, and live conditional induction and
   dependency handles. No successful proof checker is stubbed.

   Observation of a host-supplied entry-point table is NOT evidence that the
   host enumerated every physical XTDB writer or that its deployed router
   uses the checked snapshot. Tests deliberately preserve those open claims."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.live.application-preflight.authoritative-coverage :as coverage]
            [gesso.live.application-preflight.inductive-contract :as contract]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.application-preflight.inductive-contract-test]))

(def ^:private operation :example/claim)
(def ^:private outside-operation :example/unguarded-write)
(def ^:private another-outside-operation :example/administrative-update)
(def ^:private coverage-error
  :gesso.live.application-preflight.authoritative-coverage/invalid-authoritative-coverage)
(def ^:dynamic *base* nil)
(def ^:dynamic *handler* nil)

(defn- test-fixture [symbol]
  (let [v (ns-resolve 'gesso.live.application-preflight.inductive-contract-test
                      symbol)]
    (when-not v
      (throw (ex-info "Missing existing v1289 integration fixture"
                      {:symbol symbol})))
    @v))

(use-fixtures
  :once
  (fn [run]
    (let [proof ((test-fixture 'proof-package))
          {:keys [executor source handler]}
          ((test-fixture 'certified-environment))]
      (fixture/with-closed-application
        {:operations
         {operation (fixture/trusted-operation operation
                                               {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (binding [*handler* handler
                    *base*
                    {:inductive-contract-source
                     {:inductive-source proof
                      :application-assembly assembly
                      :semantic-route-handlers {operation handler}
                      :certified-operations
                      {operation {:executor executor
                                  :current-source source}}}
                     :enumerate-entry-points!
                     (fn [] {operation {:kind :semantic-route
                                        :handler handler}})}]
            (run)))))))

(defn- entry [kind handler]
  {:kind kind :handler handler})

(defn- fresh-handler [] (fn [_] {:status 200 :body "not an issued handler"}))

(defn- with-observed [table]
  (assoc *base* :enumerate-entry-points! (fn [] table)))

(defn- validate-observed-only [enumerator]
  ;; Focus the many malformed-host-input examples on the actual v1292
  ;; validator, without unnecessarily repeating expensive proof admission.
  ;; Representative cases below also exercise the complete public boundary.
  (let [v (ns-resolve
           'gesso.live.application-preflight.authoritative-coverage
           'observed-entries!)]
    (when-not v
      (throw (ex-info "Missing v1292 host-input validator" {})))
    ((var-get v) enumerator)))

(defn- observed-with-extra
  ([] (observed-with-extra outside-operation))
  ([id] {operation (entry :semantic-route *handler*)
         id (entry :unclassified (fresh-handler))}))

(defn- check
  ([] (coverage/check-authoritative-coverage! *base*))
  ([source] (coverage/check-authoritative-coverage! source)))

(defn- failure [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reject! [reason thunk]
  (let [data (failure thunk)]
    (is (map? data) (str "Expected typed rejection " reason))
    (is (= coverage-error (:error/type data)) (pr-str data))
    (is (= reason (:reason data)) (pr-str data))
    data))

(def ^:private permanent-open
  #{:host-entry-point-enumerator-is-complete-and-current
    :non-route-and-direct-executor-writers-are-enumerated
    :all-xtdb-write-paths-refine-declared-abstract-operations
    :deployed-router-adopts-checked-snapshot
    :post-check-router-and-entry-point-stability
    :actual-runtime-authorization-and-transaction-atomicity})

(defn- assert-no-physical-authority! [r]
  (is (= :not-established (:physical-entry-point-exhaustiveness r)))
  (is (= :not-established (:physical-write-path-completeness r)))
  (is (= :not-established (:abstract-to-physical-transition-correspondence r)))
  (is (= :not-established (:active-router-adoption r)))
  (is (= :not-established (:production-certification r)))
  (is (= :none (:transaction-authority r)))
  (is (= :none (:authority r)))
  (is (= :only-host-enumerated-entry-points (:scope r)))
  (is (set/subset? permanent-open (:open-obligations r)))
  (is (= :all-original-obligations-remain-open
         (:original-obligation-status r)))
  (is (not (contains? r :valid?)))
  (is (not (contains? r :proved?))))

(deftest real-assembly-positive-case-is-only-observational
  (let [r (check)]
    (is (= coverage/report-type (:type r)))
    (is (= coverage/coverage-version (:version r)))
    (is (= :fresh-source-derived-contract-and-host-enumeration
           (:checking r)))
    (is (= :rechecked-current (:source-relative-application-binding r)))
    (is (= #{operation} (:abstract-operation-ids r)))
    (is (= #{operation} (:declared-semantic-route-ids r)))
    (is (= #{operation} (:observed-entry-point-ids r)))
    (is (= #{operation} (:observed-matched-semantic-route-ids r)))
    (is (= #{} (:observed-unclassified-entry-point-ids r)))
    (is (= :exact-for-supplied-snapshot
           (:observable-semantic-route-coverage r)))
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (assert-no-physical-authority! r)))

(deftest source-fingerprints-come-from-actual-inductive-contract
  (let [upstream (contract/check-inductive-contract!
                  (:inductive-contract-source *base*))
        r (check)]
    (doseq [k [:catalog-fingerprint :induction-fingerprint
               :certificate-fingerprint :dependency-fingerprint]]
      (is (= (get upstream k) (get r k)) (str k)))
    (is (= (:original-open-obligation-ids-by-operation upstream)
           (:original-open-obligation-ids-by-operation r)))
    (is (= (:open-premises upstream) (:open-premises r)))
    (is (set/subset? (:open-obligations upstream)
                     (:open-obligations r)))))

(deftest one-unclassified-entry-remains-uncovered
  (let [r (check (with-observed (observed-with-extra)))]
    (is (= #{outside-operation} (:observed-unclassified-entry-point-ids r)))
    (is (= #{operation outside-operation} (:observed-entry-point-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (is (contains? (:open-obligations r)
                   :observed-entry-points-outside-inductive-contract))
    (assert-no-physical-authority! r)))

(deftest several-unclassified-entries-remain-uncovered
  (let [table (assoc (observed-with-extra)
                     another-outside-operation
                     (entry :unclassified (fresh-handler)))
        r (check (with-observed table))]
    (is (= #{outside-operation another-outside-operation}
           (:observed-unclassified-entry-point-ids r)))
    (is (= #{operation} (:observed-matched-semantic-route-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (assert-no-physical-authority! r)))

(deftest host-enumerator-called-exactly-once-per-check
  (let [calls (atom 0)
        source (assoc *base* :enumerate-entry-points!
                      (fn [] (swap! calls inc)
                        {operation (entry :semantic-route *handler*)}))]
    (is (= :none-observed (:unclassified-entry-point-status (check source))))
    (is (= 1 @calls))
    (is (= :none-observed (:unclassified-entry-point-status (check source))))
    (is (= 2 @calls))))

(deftest new-observed-snapshot-does-not-inherit-old-clearance
  (let [snapshot (atom {operation (entry :semantic-route *handler*)})
        source (assoc *base* :enumerate-entry-points! #(deref snapshot))
        first-report (check source)]
    (swap! snapshot assoc outside-operation
           (entry :unclassified (fresh-handler)))
    (let [second-report (check source)]
      (is (= :none-observed (:unclassified-entry-point-status first-report)))
      (is (= #{outside-operation}
             (:observed-unclassified-entry-point-ids second-report)))
      (is (= :observed-uncovered (:unclassified-entry-point-status second-report)))
      (assert-no-physical-authority! first-report)
      (assert-no-physical-authority! second-report))))

(deftest route-removal-after-observation-rejects-on-fresh-check
  (let [snapshot (atom {operation (entry :semantic-route *handler*)})
        source (assoc *base* :enumerate-entry-points! #(deref snapshot))]
    (is (= #{operation} (:observed-entry-point-ids (check source))))
    (reset! snapshot {})
    (let [data (reject! :missing-observed-semantic-routes #(check source))]
      (is (= #{operation} (:missing-route-ids data))))))

(deftest deliberately-concealed-writer-does-not-establish-completeness
  ;; Host emits only the declared route even though it could be concealing an
  ;; additional direct database writer. The checker is deliberately honest:
  ;; it cannot discover the omission and MUST keep completeness unproved.
  (let [r (check (with-observed {operation (entry :semantic-route *handler*)}))]
    (is (= :none-observed (:unclassified-entry-point-status r)))
    (is (= :not-established (:physical-write-path-completeness r)))
    (is (contains? (:open-obligations r)
                   :host-entry-point-enumerator-is-complete-and-current))
    (is (contains? (:open-obligations r)
                   :non-route-and-direct-executor-writers-are-enumerated))))

(deftest empty-host-inventory-cannot-masquerade-as-empty-app
  (let [data (reject! :missing-observed-semantic-routes
                      #(check (with-observed {})))]
    (is (= #{operation} (:missing-route-ids data)))))

(deftest non-route-entry-cannot-replace-declared-semantic-route
  (let [data (reject! :misclassified-semantic-route
                      #(check (with-observed
                                {operation (entry :unclassified *handler*)}))) ]
    (is (= [:entry-points operation] (:at data)))))

(deftest matching-result-is-not-matching-handler-identity
  (let [data (reject! :substituted-semantic-route-handler
                      #(check (with-observed
                                {operation (entry :semantic-route
                                                  (fresh-handler))}))) ]
    (is (= [:entry-points operation] (:at data)))))

(deftest extra-route-cannot-self-declare-as-semantic
  (let [data (reject! :undeclared-semantic-route
                      #(check (with-observed
                                {operation (entry :semantic-route *handler*)
                                 outside-operation (entry :semantic-route
                                                          (fresh-handler))}))) ]
    (is (= [:entry-points outside-operation] (:at data)))))

(deftest extra-entry-with-issued-handler-stays-unclassified
  ;; Sharing the issued Ring function is not permission to invent an operation.
  (let [r (check (with-observed
                  {operation (entry :semantic-route *handler*)
                   outside-operation (entry :unclassified *handler*)}))]
    (is (= #{outside-operation} (:observed-unclassified-entry-point-ids r)))
    (is (= :observed-uncovered (:unclassified-entry-point-status r)))
    (assert-no-physical-authority! r)))

(deftest missing-route-is-not-excused-by-unrelated-extra
  (let [data (reject! :missing-observed-semantic-routes
                      #(check (with-observed
                                {outside-operation (entry :unclassified
                                                          (fresh-handler))})))]
    (is (= #{operation} (:missing-route-ids data)))))

(deftest malformed-top-level-sources-fail-closed
  (doseq [[source reason]
          [[nil :expected-unannotated-plain-map]
           [[] :expected-unannotated-plain-map]
           ['() :expected-unannotated-plain-map]
           [(sorted-map) :expected-unannotated-plain-map]
           [(with-meta *base* {:complete true})
            :expected-unannotated-plain-map]
           [(dissoc *base* :enumerate-entry-points!) :wrong-keys]
           [(dissoc *base* :inductive-contract-source) :wrong-keys]
           [(assoc *base* :valid? true) :wrong-keys]
           [(assoc *base* :physical-writers-covered? true) :wrong-keys]
           [(assoc *base* :authority :granted) :wrong-keys]]]
    (testing (str "source shape " (pr-str reason))
      (reject! reason #(check source)))))

(deftest unexpected-top-level-claims-show-exact-key-details
  (let [data (reject! :wrong-keys
                      #(check (assoc *base* :proved? true)))]
    (is (= #{:proved?} (:unexpected data)))
    (is (= #{} (:missing data)))))

(deftest invalid-enumerator-values-reject-before-observation
  (doseq [v [nil false true :lookup {} [] '() 1 "catalog"]]
    (reject! :invalid-entry-point-enumerator
             #(validate-observed-only v)))
  (reject! :invalid-entry-point-enumerator
           #(check (assoc *base* :enumerate-entry-points! nil))))

(deftest observed-inventory-must-be-unannotated-plain-map
  (doseq [inventory [nil false [] '() 42
                     (sorted-map operation (entry :semantic-route *handler*))
                     (with-meta {operation (entry :semantic-route *handler*)}
                       {:proof :trusted})]]
    (reject! :invalid-observed-entry-point-table
             #(validate-observed-only (fn [] inventory))))
  (reject! :invalid-observed-entry-point-table
           #(check (with-observed nil))))

(deftest inventory-keys-must-be-qualified-keywords
  (doseq [id [nil :claim "example/claim" 42 'example/claim]]
    (reject! :invalid-observed-entry-point-table
             #(validate-observed-only
               (fn [] {operation (entry :semantic-route *handler*)
                       id (entry :unclassified (fresh-handler))}))))
  (reject! :invalid-observed-entry-point-table
           #(check (with-observed
                     {operation (entry :semantic-route *handler*)
                      :unqualified (entry :unclassified (fresh-handler))}))))

(deftest malformed-declared-entry-map-rejects
  (doseq [candidate [nil []
                     (sorted-map :kind :semantic-route :handler *handler*)
                     (with-meta (entry :semantic-route *handler*)
                       {:certified true})]]
    (reject! :expected-unannotated-plain-map
             #(validate-observed-only (fn [] {operation candidate}))))
  (reject! :expected-unannotated-plain-map
           #(check (with-observed {operation nil}))))

(deftest malformed-extra-entry-map-rejects-too
  (doseq [candidate [nil []
                     (with-meta (entry :unclassified (fresh-handler))
                       {:read-only? true})]]
    (reject! :expected-unannotated-plain-map
             #(validate-observed-only
               (fn [] {operation (entry :semantic-route *handler*)
                       outside-operation candidate}))))
  (reject! :expected-unannotated-plain-map
           #(check (with-observed
                     {operation (entry :semantic-route *handler*)
                      outside-operation nil}))))

(deftest entry-declarations-must-have-exact-two-keys
  (doseq [candidate [{:kind :semantic-route}
                     {:handler *handler*}
                     (assoc (entry :semantic-route *handler*) :valid? true)
                     (assoc (entry :semantic-route *handler*) :read-only? true)]]
    (reject! :wrong-keys
             #(validate-observed-only (fn [] {operation candidate}))))
  (reject! :wrong-keys
           #(check (with-observed {operation {:kind :semantic-route}}))))

(deftest extra-entry-cannot-claim-to-be-read-only-or-trusted
  (doseq [field [:read-only? :certified? :not-a-writer? :proof :valid?]]
    (reject! :wrong-keys
             #(validate-observed-only
               (fn [] {operation (entry :semantic-route *handler*)
                       outside-operation (assoc (entry :unclassified
                                                       (fresh-handler))
                                                field true)}))))
  (reject! :wrong-keys
           #(check (with-observed
                     {operation (entry :semantic-route *handler*)
                      outside-operation (assoc (entry :unclassified
                                                      (fresh-handler))
                                               :read-only? true)}))))

(deftest unsupported-entry-kinds-reject
  (doseq [kind [nil false :read-only :writer :certified :not-a-writer]]
    (let [data (reject! :unsupported-entry-point-kind
                        #(validate-observed-only
                          (fn [] {operation (entry :semantic-route *handler*)
                                  outside-operation (entry kind
                                                           (fresh-handler))})))]
      (is (= [:entry-points outside-operation :kind] (:at data)))))
  (reject! :unsupported-entry-point-kind
           #(check (with-observed
                     {operation (entry :semantic-route *handler*)
                      outside-operation (entry :read-only (fresh-handler))}))))

(deftest even-declared-route-kind-cannot-be-fabricated
  (reject! :unsupported-entry-point-kind
           #(check (with-observed
                     {operation (entry :certified *handler*)}))))

(deftest all-entry-handlers-must-be-actual-functions
  (doseq [handler [nil false :keyword {:callable? true} [] 42 "handler"]]
    (reject! :noncallable-entry-point
             #(validate-observed-only
               (fn [] {operation (entry :semantic-route *handler*)
                       outside-operation (entry :unclassified handler)}))))
  (reject! :noncallable-entry-point
           #(check (with-observed
                     {operation (entry :semantic-route *handler*)
                      outside-operation (entry :unclassified nil)}))))

(deftest declared-entry-handler-must-be-callable-before-identity-check
  (reject! :noncallable-entry-point
           #(check (with-observed
                     {operation (entry :semantic-route nil)}))))

(deftest enumerator-errors-never-return-success-reports
  (let [failure (ex-info "router changed while enumerating"
                         {:host/error :router-race})
        source (assoc *base* :enumerate-entry-points! #(throw failure))]
    (is (identical? failure
                    (try (check source) nil
                         (catch clojure.lang.ExceptionInfo e e))))))

(deftest no-enumerator-call-if-inductive-source-invalid
  (let [calls (atom 0)
        source (-> *base*
                   (assoc-in [:inductive-contract-source :certified-operations]
                             {})
                   (assoc :enumerate-entry-points!
                          (fn [] (swap! calls inc) {})))]
    (is (map? (failure #(check source))))
    (is (= 0 @calls))))

(deftest substituted-public-inductive-checker-report-rejected
  (with-redefs [contract/check-inductive-contract!
                (fn [_] {:type contract/report-type
                         :version contract/contract-version
                         :checking :current-abstract-source-and-issued-route-snapshot-rechecked
                         :original-obligation-status :all-original-obligations-remain-open
                         :physical-writer-completeness :not-established
                         :authority :none :transaction-authority :none
                         :semantic-route-ids #{operation}
                         :valid? true})]
    (reject! :substituted-inductive-checker-report #(check))))

(deftest stale-source-proof-cannot-obtain-coverage-report
  (let [source (assoc-in *base*
                         [:inductive-contract-source :inductive-source :catalog
                          :operations 0 :construction :guard :expression]
                         [:= [:read :example/status] [:literal :claimed]])]
    (is (map? (failure #(check source))))))

(deftest stale-application-operation-binding-cannot-obtain-report
  (let [source (assoc-in *base*
                         [:inductive-contract-source :certified-operations]
                         {})]
    (is (map? (failure #(check source))))))

(deftest source-relative-report-is-copyable-data-not-fresh-proof
  (let [old (check)
        cloned (into {} old)]
    (is (= old cloned))
    (is (= :none (:authority cloned)))
    (reject! :wrong-keys #(check cloned))
    (is (= :none (:transaction-authority cloned)))))

(deftest copied-report-and-fabricated-status-cannot-become-source
  (let [copied (assoc (check) :valid? true :complete? true)]
    (reject! :wrong-keys #(check copied))
    (is (= :not-established (:physical-write-path-completeness copied)))
    (is (= :none (:authority copied)))))

(deftest source-relative-premises-never-disappear-if-no-extra-is-seen
  (let [r (check)]
    (is (seq (:open-premises r)))
    (is (seq (:original-open-obligation-ids-by-operation r)))
    (is (contains? (:open-obligations r)
                   :induction-checker-independent-soundness))
    (is (contains? (:open-obligations r)
                   :physical-writer-and-entry-point-completeness))
    (assert-no-physical-authority! r)))

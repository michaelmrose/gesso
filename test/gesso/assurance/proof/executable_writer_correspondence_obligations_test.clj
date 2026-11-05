(ns gesso.assurance.proof.executable-writer-correspondence-obligations-test
  "v1369 permanent adversarial verification of the nonauthoritative v1368
   executable-writer correspondence ledger. Exercises a REAL closed
   application, issued executor, Ring route handler and fresh host enumeration;
   no fabricated affirmative source reports or physical proof certificates.
   Correct identifier joins do not entail a universal transaction refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [gesso.assurance.proof.executable-writer-correspondence-obligations :as obligations]
            [gesso.assurance.proof.operation-writer-coverage-soundness :as bridge]
            [gesso.assurance.application-induction-test]
            [gesso.assurance.proof.abstract-execution-induction-test :as example]
            [gesso.live.application-preflight.fixture :as fixture]))

(def ^:private op :example/claim)
(def ^:dynamic *source* nil)
(def ^:dynamic *observed* nil)
(def ^:dynamic *handler* nil)

(defn- catalog [ids]
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
    (let [context (@(ns-resolve 'gesso.assurance.application-induction-test 'make-context))
          {:keys [proof plan-source induction-source executor handler]} context]
      (fixture/with-closed-application
        {:operations {op (fixture/trusted-operation op {:published-change-topics #{}})}}
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
                source {:induction-catalog (catalog [op])
                        :application-induction-source issued}]
            (binding [*source* source *observed* snapshot *handler* handler]
              (run))))))))

(defn- audited [] (obligations/derive-executable-writer-obligations! *source*))
(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- source-with-ids [ids]
  (assoc *source* :induction-catalog (catalog ids)))
(defn- no-authority! [r]
  (is (= :not-issued (:certificate r)))
  (is (= :not-admitted (:admission r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :obligations-only (:proof-status r)))
  (is (false? (:all-executable-refinement-obligations-discharged? r)))
  (is (= :not-established (:handler-transition-correspondence r)))
  (is (= :not-established (:physical-write-path-completeness r)))
  (is (= :not-established (:physical-snapshot-authenticity r))))

(deftest real-assembly-generates-open-ledger
  (let [r (audited)]
    (is (= obligations/report-type (:type r)))
    (is (= :refinement-obligations-generated (:status r)))
    (is (= #{op} (:operation-ids r)))
    (is (= 1 (count (:operation-rows r))))
    (no-authority! r)))

(deftest exact-ledger-obligation-identities
  (let [r (audited)
        row (first (:operation-rows r))]
    (is (= :open-executable-refinement (:status row)))
    (is (= 9 (count (:obligations row))))
    (is (= (:required-obligation-ids r)
           (set (map :id (:obligations row)))))
    (is (every? #(= :open (:status %)) (:obligations row)))
    (is (every? keyword? (map :required-evidence (:obligations row))))
    (is (= :not-established (:executable-refinement row)))
    (is (= :not-established (:cross-source-transition-equivalence row)))))

(deftest independent-declaration-field-inventory
  (let [r (audited)
        row (first (:operation-rows r))
        original (get-in *source* [:induction-catalog :operations 0 :declaration])
        contract (:abstract-contract row)]
    (is (= op (:subject-id contract)))
    (is (= (set (keys (:state-types original))) (:state-field-ids contract)))
    (is (= (set (keys (:input-types original))) (:input-field-ids contract)))
    (is (= (:reads original) (:read-field-ids contract)))
    (is (= (:writes original) (:write-field-ids contract)))
    (is (= (:expression (:guard original)) (:guard-expression contract)))
    (is (= (:readers (:guard original)) (:guard-readers contract)))
    (is (= (clojure.set/difference (set (keys (:state-types original)))
                                  (:writes original))
           (:frame-field-ids contract)))
    (is (= (set (keys (:assignments original)))
           (set (keys (:assignment-origins contract)))))))

(deftest assignment-origins-preserve-prestate-vs-input-identity
  (let [r (audited)
        origins (get-in r [:operation-rows 0 :abstract-contract :assignment-origins])
        original (get-in *source* [:induction-catalog :operations 0 :declaration])
        state (set (keys (:state-types original)))
        inputs (set (keys (:input-types original)))]
    (doseq [[field [op source]] (:assignments original)]
      (let [origin (origins field)]
        (case op
          :literal (do (is (= :literal (:kind origin)))
                       (is (= source (:value origin))))
          :read (do (is (= source (:field origin)))
                    (is (= (if (contains? inputs source)
                             :operation-input-read :prestate-read)
                           (:kind origin)))
                    (is (or (contains? state source)
                            (contains? inputs source)))))))))

(deftest obligation-footprints-bind-to-original-source
  (let [r (audited)
        c (:abstract-contract (first (:operation-rows r)))
        by-id (into {} (map (juxt :id identity)
                            (:obligations (first (:operation-rows r)))))]
    (is (= (:input-field-ids c) (:source-fields (by-id :typed-input-binding))))
    (is (= (:state-field-ids c) (:source-fields (by-id :transaction-internal-prestate))))
    (is (= (:guard-readers c) (:source-fields (by-id :guard-authorization))))
    (is (= (:write-field-ids c) (:source-fields (by-id :assignment-write-footprint))))
    (is (= (:frame-field-ids c) (:source-fields (by-id :frame-and-unmodeled-fields))))))

(deftest same-operation-ids-are-not-semantic-equivalence
  (let [row (first (:operation-rows (audited)))]
    (is (true? (:declared-abstract-operation? row)))
    (is (true? (:checked-issued-executor? row)))
    (is (true? (:induction-step-supported? row)))
    (is (= :not-established (:cross-source-transition-equivalence row)))
    (is (= :not-established (:executable-refinement row)))))

(deftest missing-issued-operation-keeps-open-ledger
  (let [r (obligations/derive-executable-writer-obligations!
           (source-with-ids [op :example/additional]))
        rows (into {} (map (juxt :operation-id identity) (:operation-rows r)))]
    (is (= :open-operation-catalog-join (:status r)))
    (is (= #{:example/additional} (:unmatched-abstract-ids r)))
    (is (= :open-unissued-abstract-operation
           (:status (rows :example/additional))))
    (is (false? (:checked-issued-executor? (rows :example/additional))))
    (no-authority! r)))

(deftest extra-issued-operation-keeps-open-ledger
  (let [r (obligations/derive-executable-writer-obligations!
           (source-with-ids [:example/other]))
        rows (into {} (map (juxt :operation-id identity) (:operation-rows r)))]
    (is (= :open-operation-catalog-join (:status r)))
    (is (= #{op} (:unmodeled-issued-ids r)))
    (is (= :open-undeclared-issued-writer (:status (rows op))))
    (is (nil? (:abstract-contract (rows op))))
    (is (every? #(= :open (:status %)) (:obligations (rows op))))))

(deftest unclassified-host-entry-point-is-never-proven
  (swap! *observed* assoc :example/hidden
         {:kind :unclassified :handler (fn [_] {:status 200})})
  (try
    (let [r (audited)]
      (is (= :open-unclassified-observed-writers (:status r)))
      (is (= #{:example/hidden} (:observed-unclassified-entry-point-ids r)))
      (no-authority! r))
    (finally (swap! *observed* dissoc :example/hidden))))

(deftest stale-current-host-observations-are-rejected
  (let [old (audited)]
    (swap! *observed* assoc :example/hidden
           {:kind :unclassified :handler (fn [_] {:status 200})})
    (try
      (is (= :stale-or-forged-writer-obligation-report
             (:reason (failure #(obligations/require-current-executable-writer-obligations!
                                  old *source*)))))
      (finally (swap! *observed* dissoc :example/hidden)))))

(deftest current-report-agrees-with-recomputation
  (let [r (audited)]
    (is (= r (obligations/require-current-executable-writer-obligations!
              r *source*)))
    (is (= :stale-or-forged-writer-obligation-report
           (:reason (failure #(obligations/require-current-executable-writer-obligations!
                               (assoc r :authority :forged) *source*)))))))

(deftest injected-coverage-checker-is-rejected
  (let [f bridge/derive-observed-writer-coverage!]
    (with-redefs [bridge/derive-observed-writer-coverage!
                  (fn [& args] (assoc (apply f args) :authority :forged))]
      (is (= :substituted-coverage-checker
             (:reason (failure #(audited))))))))

(deftest invalid-source-and-metadata-are-rejected
  (doseq [bad [nil [] {} (assoc *source* :extra :yes)
               (with-meta *source* {:forged true})]]
    (is (= :invalid-source (:reason
                             (failure #(obligations/derive-executable-writer-obligations!
                                        bad)))))))

(deftest forged-result-metadata-is-rejected
  (let [r (audited)]
    (is (= :stale-or-forged-writer-obligation-report
           (:reason (failure #(obligations/require-current-executable-writer-obligations!
                               (with-meta r {:trusted true}) *source*)))))))

(deftest nonroute-completeness-remains-open
  (let [r (audited)]
    (is (= :not-established (:deployed-router-adoption r)))
    (is (= :not-established (:universal-checker-soundness r)))
    (is (contains? (:open-obligations r) :all-physical-writers-enumerated))
    (is (contains? (:open-obligations r) :authenticated-transaction-observation))
    (is (contains? (:open-obligations r) :source-to-physical-transition-refinement))))

;; The expected obligation set is specified independently of the ledger.
;; A checker producing only one or two convenient obligations cannot pass.
(def ^:private expected-requirements
  {:typed-input-binding
   :executed-request-to-typed-input-projection
   :transaction-internal-prestate
   :authenticated-transaction-snapshot-and-typed-projection
   :guard-authorization
   :executed-guard-and-admitted-authorization-correspondence
   :assignment-write-footprint
   :executed-assignments-and-complete-physical-write-set
   :frame-and-unmodeled-fields
   :unchanged-state-and-physical-domain-frame-correspondence
   :serial-concurrency-validation
   :atomic-version-check-and-serial-commit-history
   :committed-outcome-incorporation
   :authenticated-commit-reject-or-unknown-resolution
   :writer-provenance-and-deployment
   :deployed-executor-identity-and-origin-attestation
   :universal-execution-refinement
   :sound-all-executions-refinement-argument})

(defn- assert-stale! [prior source]
  (is (= :stale-or-forged-writer-obligation-report
         (:reason (failure
                   #(obligations/require-current-executable-writer-obligations!
                     prior source))))))

(defn- operation-row [r id]
  (first (filter #(= id (:operation-id %)) (:operation-rows r))))

(deftest nine-specified-obligations-in-exact-order
  (let [r (audited)
        os (:obligations (operation-row r op))]
    (is (= obligations/report-version (:version r)))
    (is (= :fresh-v1366-source-rechecked-and-obligations-derived (:checking r)))
    (is (= [:typed-input-binding
            :transaction-internal-prestate
            :guard-authorization
            :assignment-write-footprint
            :frame-and-unmodeled-fields
            :serial-concurrency-validation
            :committed-outcome-incorporation
            :writer-provenance-and-deployment
            :universal-execution-refinement]
           (mapv :id os)))
    (is (= expected-requirements
           (into {} (map (juxt :id :required-evidence) os))))
    (is (= (set (keys expected-requirements)) (:required-obligation-ids r)))
    (is (every? #(and (= :open (:status %))
                     (contains? % :source-fields)) os))
    (no-authority! r)))

(deftest source-obligations-exactly-track-declared-reader-footprints
  (let [r (audited)
        decl (get-in *source* [:induction-catalog :operations 0 :declaration])
        os (into {} (map (juxt :id identity) (:obligations (operation-row r op))))]
    (doseq [[id fields]
            {:typed-input-binding (set (keys (:input-types decl)))
             :transaction-internal-prestate (set (keys (:state-types decl)))
             :guard-authorization (get-in decl [:guard :readers])
             :assignment-write-footprint (:writes decl)
             :frame-and-unmodeled-fields
             (clojure.set/difference (set (keys (:state-types decl)))
                                     (:writes decl))}]
      (is (= fields (:source-fields (os id))) (str id)))
    (is (= #{} (:source-fields (os :serial-concurrency-validation))))
    (is (= #{} (:source-fields (os :universal-execution-refinement))))))

(deftest literal-assignments-and-input-copies-remain-distinct
  (let [origins (get-in (audited)
                        [:operation-rows 0 :abstract-contract :assignment-origins])]
    (is (= {:kind :literal :value :claimed}
           (origins :request/status)))
    (is (= {:kind :literal :value "saved"}
           (origins :request/note)))
    (is (= {:kind :operation-input-read :field :input/actor}
           (origins :request/owner)))
    (is (= #{:request/version}
           (get-in (audited)
                   [:operation-rows 0 :abstract-contract :frame-field-ids])))))

(deftest prestate-read-origin-not-confused-with-input-read
  (let [changed (-> *source*
                    (assoc-in [:induction-catalog :operations 0 :declaration
                               :assignments :request/owner]
                              [:read :request/owner])
                    (update-in [:induction-catalog :operations 0 :declaration :reads]
                               #(-> % (disj :input/actor) (conj :request/owner)))
                    (assoc-in [:induction-catalog :operations 0 :declaration :input-types]
                              {}))
        row (operation-row
             (obligations/derive-executable-writer-obligations! changed) op)]
    (is (= {:kind :prestate-read :field :request/owner}
           (get-in row [:abstract-contract :assignment-origins :request/owner])))
    (is (= :not-established (:cross-source-transition-equivalence row)))
    (is (= :not-established (:executable-refinement row)))))

(deftest extra-independently-declared-operation-has-complete-open-ledger
  (let [r (obligations/derive-executable-writer-obligations!
           (source-with-ids [op :example/additional]))
        row (operation-row r :example/additional)]
    (is (= #{op :example/additional} (:operation-ids r)))
    (is (= 2 (count (:operation-rows r))))
    (is (= :open-unissued-abstract-operation (:status row)))
    (is (true? (:declared-abstract-operation? row)))
    (is (false? (:checked-issued-executor? row)))
    (is (= :not-established (:executable-refinement row)))
    (is (= 9 (count (:obligations row))))
    (is (every? #(= :open (:status %)) (:obligations row)))
    (no-authority! r)))

(deftest issued-but-undeclared-operation-cannot-borrow-abstract-contract
  (let [r (obligations/derive-executable-writer-obligations!
           (source-with-ids [:example/unrelated]))
        row (operation-row r op)]
    (is (= :open-undeclared-issued-writer (:status row)))
    (is (nil? (:abstract-contract row)))
    (is (false? (:declared-abstract-operation? row)))
    (is (true? (:checked-issued-executor? row)))
    (is (false? (:induction-step-supported? row)))
    (is (= :not-established (:executable-refinement row)))
    (is (= 9 (count (:obligations row))))))

(deftest exact-ids-but-failing-base-still-open
  (let [bad (assoc-in *source* [:induction-catalog :declared-initial-states]
                      [{:request/status :open :request/owner nil
                        :request/version 4}])
        r (obligations/derive-executable-writer-obligations! bad)]
    (is (= #{op} (:operation-ids r)))
    (is (= :open-induction-premises (:status r)))
    (is (= :open-executable-refinement
           (:status (operation-row r op))))
    (is (true? (:induction-step-supported? (operation-row r op))))
    (is (false? (:all-executable-refinement-obligations-discharged? r)))
    (no-authority! r)))

(deftest all-physical-premises-remain-open-when-host-snapshot-clean
  (let [r (audited)]
    (is (empty? (:observed-unclassified-entry-point-ids r)))
    (is (every? (:open-obligations r)
                [:all-physical-writers-enumerated
                 :authenticated-transaction-observation
                 :source-to-physical-transition-refinement
                 :deployed-executor-and-router-stability
                 :host-entry-point-enumerator-is-complete
                 :all-direct-and-nonroute-xtdb-writers-are-enumerated
                 :actual-application-initializers-are-covered
                 :certificate-admission]))
    (doseq [key [:physical-snapshot-authenticity
                 :physical-write-path-completeness
                 :handler-transition-correspondence
                 :deployed-router-adoption
                 :universal-checker-soundness]]
      (is (= :not-established (key r)) (str key)))
    (no-authority! r)))

(deftest a-host-reported-unclassified-route-cannot-discharge-anything
  (swap! *observed* assoc :example/nonroute-writer
         {:kind :unclassified :handler (fn [_] {:status 200})})
  (try
    (let [r (audited)]
      (is (= :open-unclassified-observed-writers (:status r)))
      (is (= #{:example/nonroute-writer}
             (:observed-unclassified-entry-point-ids r)))
      (is (= 9 (count (:obligations (operation-row r op)))))
      (no-authority! r))
    (finally (swap! *observed* dissoc :example/nonroute-writer))))

(deftest two-unclassified-entries-are-not-merged-away
  (swap! *observed* assoc
         :example/a {:kind :unclassified :handler (fn [_] {:status 200})}
         :example/b {:kind :unclassified :handler (fn [_] {:status 201})})
  (try
    (let [r (audited)]
      (is (= #{:example/a :example/b}
             (:observed-unclassified-entry-point-ids r)))
      (is (= :open-unclassified-observed-writers (:status r)))
      (is (= #{op} (:operation-ids r)))
      (no-authority! r))
    (finally (swap! *observed* dissoc :example/a :example/b))))

(deftest wrong-observed-handler-cannot-count-as-current-writer
  (swap! *observed* assoc op
         {:kind :semantic-route :handler (fn [_] {:status 200})})
  (try
    (is (some? (failure audited)))
    (finally
      (swap! *observed* assoc op
             {:kind :semantic-route :handler *handler*}))))

(deftest missing-observed-route-cannot-count-as-current-writer
  (swap! *observed* dissoc op)
  (try
    (is (some? (failure audited)))
    (finally
      (swap! *observed* assoc op
             {:kind :semantic-route :handler *handler*}))))

(deftest enumerator-called-freshly-on-each-derivation
  (let [calls (atom 0)
        source (assoc-in *source*
                         [:application-induction-source
                          :authoritative-coverage-source
                          :enumerate-entry-points!]
                         (fn [] (swap! calls inc) @*observed*))]
    (is (= :refinement-obligations-generated
           (:status (obligations/derive-executable-writer-obligations! source))))
    (is (= 1 @calls))
    (is (= :refinement-obligations-generated
           (:status (obligations/derive-executable-writer-obligations! source))))
    (is (= 2 @calls))))

(deftest failing-host-enumerator-is-never-treated-as-empty-enumeration
  (let [source (assoc-in *source*
                         [:application-induction-source
                          :authoritative-coverage-source
                          :enumerate-entry-points!]
                         (fn [] (throw (ex-info "host down" {:host-down true}))))]
    (is (= true
           (:host-down (failure #(obligations/derive-executable-writer-obligations!
                                  source)))))))

(deftest required-current-rechecks-original-source-not-copied-ledger
  (let [r (audited)]
    (is (= r (obligations/require-current-executable-writer-obligations!
              r *source*)))
    (assert-stale! r (source-with-ids [op :example/additional]))
    (assert-stale! r (source-with-ids [:example/unrelated]))))

(deftest forged-status-and-certificate-fields-rejected
  (let [r (audited)]
    (doseq [[key value] [[:proof-status :proved]
                         [:certificate :issued]
                         [:all-executable-refinement-obligations-discharged? true]
                         [:handler-transition-correspondence :proved]]]
      (assert-stale! (assoc r key value) *source*))))

(deftest forged-row-semantics-and-physical-claims-rejected
  (let [r (audited)]
    (doseq [changed [(assoc-in r [:operation-rows 0 :cross-source-transition-equivalence]
                              :proved)
                     (assoc-in r [:operation-rows 0 :executable-refinement]
                               :proved)
                     (assoc-in r [:operation-rows 0 :abstract-contract
                                  :frame-field-ids] #{})]]
      (assert-stale! changed *source*))))

(deftest forged-obligation-status-or-omission-rejected
  (let [r (audited)]
    (doseq [changed [(assoc-in r [:operation-rows 0 :obligations 0 :status]
                              :discharged)
                     (assoc-in r [:operation-rows 0 :obligations 1 :source-fields]
                               #{})
                     (assoc-in r [:operation-rows 0 :obligations 2 :required-evidence]
                               :fake)
                     (update-in r [:operation-rows 0 :obligations] pop)
                     (assoc r :required-obligation-ids #{})
                     (assoc r :open-obligations #{})]]
      (assert-stale! changed *source*))))

(deftest forged-application-fingerprints-rejected
  (let [r (audited)]
    (doseq [key [:catalog-id
                 :application-catalog-fingerprint
                 :application-induction-fingerprint
                 :application-certificate-fingerprint
                 :application-dependency-fingerprint]]
      (assert-stale! (assoc r key :forged) *source*))))

(deftest forged-unmatched-and-unclassified-inventories-rejected
  (let [r (audited)]
    (doseq [changed [(assoc r :unmatched-abstract-ids #{op})
                     (assoc r :observed-unclassified-entry-point-ids #{:example/hidden})
                     (assoc r :operation-rows [])]]
      (assert-stale! changed *source*))))

(deftest copied-report-is-data-not-authority
  (let [r (audited)
        copy (edn/read-string (pr-str r))]
    (is (= r copy))
    (is (= copy
           (obligations/require-current-executable-writer-obligations!
            copy *source*)))
    (no-authority! copy)))

(deftest prior-report-must-be-an-unannotated-plain-map
  (let [r (audited)]
    (doseq [changed [nil [] #{} (sorted-map)
                     (with-meta r {:certificate :issued})]]
      (assert-stale! changed *source*))))

(deftest source-must-have-exact-original-shape
  (doseq [bad [(dissoc *source* :induction-catalog)
               (dissoc *source* :application-induction-source)
               (assoc *source* :proof-status :proved)
               (assoc *source* :cached-ledger {:authority :none})
               (sorted-map :induction-catalog (:induction-catalog *source*)
                           :application-induction-source (:application-induction-source *source*))]]
    (is (= :invalid-source
           (:reason (failure #(obligations/derive-executable-writer-obligations!
                               bad)))))))

(deftest malformed-source-catalog-fails-before-ledger-generation
  (doseq [bad-catalog [nil [] (dissoc (:induction-catalog *source*) :operations)
                       (assoc (:induction-catalog *source*) :authority :proved)]]
    (is (some? (failure
                #(obligations/derive-executable-writer-obligations!
                  (assoc *source* :induction-catalog bad-catalog)))))))

(deftest malformed-abstract-writes-cannot-create-valid-ledger
  (let [bad (-> *source*
                (assoc-in [:induction-catalog :operations 0 :declaration :writes]
                          #{:request/status}) )]
    (is (some? (failure
                #(obligations/derive-executable-writer-obligations! bad))))))

(deftest substituted-source-coverage-validator-fails-closed
  (let [f bridge/require-current-observed-writer-coverage!]
    (with-redefs [bridge/require-current-observed-writer-coverage!
                  (fn [& args] (apply f args))]
      (is (= :substituted-coverage-checker
             (:reason (failure audited)))))))

(deftest temporary-guarantees-never-change-with-identified-writer
  (let [r (audited)
        row (operation-row r op)]
    (is (= :not-established (:cross-source-transition-equivalence row)))
    (is (= :not-established (:executable-refinement row)))
    (is (false? (:all-executable-refinement-obligations-discharged? r)))
    (is (not= :proved (:proof-status r)))
    (is (= :current-source-checked-writer-ledger-only (:scope r)))
    (is (not= :issued (:certificate r)))
    (no-authority! r)))

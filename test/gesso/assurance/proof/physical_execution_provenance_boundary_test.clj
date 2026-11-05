(ns gesso.assurance.proof.physical-execution-provenance-boundary-test
  "v1372: Permanent adversarial regression for the v1371 bounded, unauthenticated
   physical provenance boundary. Uses the actual issued executor and closed
   application source; the transaction observation is deliberately constructed
   from source data and is NOT a claim of real XTDB submission. Tests must
   never promote matching copyable observation maps into authenticated proof."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.edn :as edn]
            [gesso.assurance.application-induction-test]
            [gesso.assurance.observed-correspondence :as observed]
            [gesso.assurance.proof.abstract-execution-induction-test :as examples]
            [gesso.assurance.proof.executable-writer-correspondence-obligations :as writers]
            [gesso.assurance.proof.physical-execution-provenance-boundary :as boundary]
            [gesso.choreo.identity :as identity]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.transition-observation :as collector]
            [gesso.model.tx-model :as tx-model]))

(def ^:private operation :example/claim)
(def ^:dynamic *source* nil)
(def ^:dynamic *snapshot* nil)
(def ^:dynamic *handler* nil)

(defn- abstract-catalog [ids]
  (let [original (first (filter #(= :request/claim (:id %))
                                (:operations examples/base)))]
    (assoc examples/base
           :declared-operation-ids (set ids)
           :operations (mapv (fn [id]
                              (-> original
                                  (assoc :id id)
                                  (assoc-in [:declaration :subject :id] id)
                                  (assoc-in [:declaration :subject :interface :operation] id)))
                            ids))))

(defn- synthetic-observation [plan-source environment]
  ;; This is intentionally constructed WITHOUT reading or submitting to XTDB.
  ;; Being well-formed is evidence of no physical execution whatsoever.
  (let [before ((:current-document! plan-source) {} {})
        target ((:target! plan-source) {} {})
        inputs ((:transition-inputs! plan-source) {} {})
        domain @(:proposed environment)
        binding {:command-id (identity/command-id (str (random-uuid)))
                 :principal (identity/principal "helper")
                 :operation operation :arguments {}}
        incorporation (resolution/incorporation-plan
                       {:binding binding :outcome {:status :claimed}
                        :now "now" :plan {:commands [domain] :emit false}})
        record (:record incorporation)
        record-target [:gesso_commands (:xt/id record)]
        plan (tx-model/admitted-plan (:plan incorporation))
        witness {:type collector/observation-type
                 :version collector/observation-version
                 :scope collector/observation-scope
                 :evidence-class :bounded-controlled-xtdb-commit-observation
                 :observed-targets #{target record-target}
                 :declared-plan plan
                 :observed-before {target before record-target nil}
                 :observed-after {target (command/after domain)
                                  record-target record}
                 :command-target record-target
                 :persisted-command-record record
                 :command-binding (:gesso.command/binding record)
                 :physical-tx-id 42
                 :physical-status :committed
                 :ledger-window :adjacent-and-terminal-in-observed-snapshots
                 :plan-footprint :exact-for-declared-incorporation
                 :witness-provenance :caller-linked-not-independently-authenticated
                 :abstract-transition-correspondence :not-established
                 :untracked-writer-coverage :not-established
                 :certification :none
                 :authority :none}]
    {:observation witness
     :construction (:construction plan-source)
     :field-map (:field-map plan-source)
     :inputs inputs :target target :command-operation :claim}))

(use-fixtures :once
  (fn [run]
    (let [{:keys [proof plan-source induction-source executor handler] :as context}
          (@(ns-resolve 'gesso.assurance.application-induction-test 'make-context))
          environment (:environment context)]
      (fixture/with-closed-application
        {:operations {operation (fixture/trusted-operation
                                 operation {:published-change-topics #{}})}}
        (fn [{:keys [assembly]}]
          (let [snapshot (atom {operation {:kind :semantic-route :handler handler}})
                contract {:inductive-source proof
                          :application-assembly assembly
                          :semantic-route-handlers {operation handler}
                          :certified-operations
                          {operation {:executor executor :current-source plan-source}}}
                enumerated {:inductive-contract-source contract
                            :enumerate-entry-points! (fn [] @snapshot)}
                issued {:authoritative-coverage-source enumerated
                        :inductive-plans
                        {operation {:executor executor
                                    :current-source induction-source}}}
                writer-source {:induction-catalog (abstract-catalog [operation])
                               :application-induction-source issued}
                observation (synthetic-observation plan-source
                                                   (or environment
                                                       ((deref (ns-resolve
                                                                'gesso.model.certified-plan-test
                                                                'environment)))))
                source {:writer-source writer-source
                        :observed-source {:application-source issued
                                          :transition-source observation}}]
            (binding [*source* source *snapshot* snapshot *handler* handler]
              (run))))))))

(defn- audit
  ([] (boundary/audit-supplied-observation! *source*))
  ([source] (boundary/audit-supplied-observation! source)))
(defn- capture [] (boundary/derive-capture-requirements!
                   (:writer-source *source*)))
(defn- error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))
(defn- reason [f] (:reason (error f)))
(defn- altered-witness [f]
  (update-in *source* [:observed-source :transition-source :observation] f))
(defn- altered-transition [f]
  (update-in *source* [:observed-source :transition-source] f))
(defn- no-authority! [report]
  (doseq [[k expected] [[:certificate :not-issued]
                        [:admission :not-admitted]
                        [:transaction-authority :none]
                        [:authority :none]
                        [:all-executable-refinement-obligations-discharged? false]
                        [:claimed-transaction-id-authenticated? false]
                        [:actual-submitted-payload-provenance :not-established]
                        [:transaction-internal-state-provenance :not-established]
                        [:all-physical-writer-paths-enumerated :not-established]
                        [:abstract-to-physical-transition-equivalence :not-established]
                        [:universal-execution-refinement :not-established]]]
    (is (= expected (get report k)) (str "Must remain unproved: " k)))
  (is (= :bounded-unauthenticated-diagnostic-only (:proof-status report)))
  (is (= :caller-supplied-unattested (:observation-origin report)))
  (is (seq (:open-obligations report))))

(deftest matching-synthetic-observation-is-only-a-diagnostic
  (let [r (audit)]
    (is (= boundary/boundary-type (:type r)))
    (is (= boundary/boundary-version (:version r)))
    (is (= operation (:operation-id r)))
    (is (= :one-caller-supplied-bounded-xtdb-observation (:scope r)))
    (is (= :current-writer-ledger-and-original-observed-step-rechecked
           (:checking r)))
    (is (= :bounded-unauthenticated-observation-agreement (:status r))
        (str (:status r) " " (:diagnostic-checks r)))
    (is (true? (:diagnostic-conforms? r)))
    (no-authority! r)))

(deftest every-physical-obligation-remains-open
  (let [r (audit)]
    (is (= 9 (count (:operation-obligations r))))
    (is (= (writers/derive-executable-writer-obligations!
            (:writer-source *source*))
           (writers/require-current-executable-writer-obligations!
            (writers/derive-executable-writer-obligations!
             (:writer-source *source*))
            (:writer-source *source*))))
    (is (every? #(= :open (:status %)) (:operation-obligations r)))
    (is (= 9 (count (set (map :id (:operation-obligations r))))))
    (no-authority! r)))

(deftest capture-requirements-are-not-physical-captures
  (let [r (capture)]
    (is (= boundary/capture-type (:type r)))
    (is (= 8 (count (:capture-points r))))
    (is (= #{:issued-application :pre-submission :inside-transaction
             :commit-resolution :post-commit :deployment :global}
           (set (map :phase (:capture-points r)))))
    (is (= :capture-provenance-not-established (:status r)))
    (is (= :requirements-only (:proof-status r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))
    (is (= :not-established (:observed-execution-provenance r)))
    (is (= :not-established (:deployed-writer-provenance r)))
    (is (seq (:open-obligations r)))))

(deftest fabricated-transaction-id-remains-unattested
  (let [forged (altered-witness #(assoc % :physical-tx-id 999999999))
        r (audit forged)]
    (is (= 999999999 (:claimed-physical-tx-id r)))
    (is (= :bounded-unauthenticated-observation-agreement (:status r)))
    (no-authority! r)))

(deftest accepted-reports-are-recomputed-not-trusted
  (let [r (audit)]
    (is (= r (boundary/require-current-supplied-observation-audit!
              r *source*)))
    (no-authority! r)))

(deftest physical-divergence-is-not-accepted-as-agreement
  (let [target (get-in *source* [:observed-source :transition-source :target])
        changed (altered-witness #(assoc-in % [:observed-after target :request/status]
                                             :open))
        r (audit changed)]
    (is (= :observed-physical-divergence (:status r)))
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:diagnostic-checks :abstract-assignments])))
    (no-authority! r)))

(deftest incorrect-prestate-is-divergence-not-proof
  (let [target (get-in *source* [:observed-source :transition-source :target])
        changed (altered-witness #(assoc-in % [:observed-before target :request/score]
                                             15))
        r (audit changed)]
    (is (= :observed-physical-divergence (:status r)))
    (is (false? (:diagnostic-conforms? r)))
    (is (false? (get-in r [:diagnostic-checks :declared-prestate])))
    (no-authority! r)))

(deftest altered-transaction-id-invalidates-prior-report
  (let [old (audit)
        changed (altered-witness #(assoc % :physical-tx-id 5555555))]
    (is (= :stale-or-forged-provenance-audit
           (reason #(boundary/require-current-supplied-observation-audit!
                     old changed))))))

(deftest copied-prior-report-is-never-physical-attestation
  (let [r (audit)
        copied (edn/read-string (pr-str r))]
    (is (= r copied))
    (is (= copied (boundary/require-current-supplied-observation-audit!
                   copied *source*)))
    (no-authority! copied)))

(deftest stale-prior-reports-are-rejected
  (let [r (audit)]
    (doseq [changed [(assoc r :authority :granted)
                     (assoc r :status :authenticated)
                     (assoc r :open-obligations #{})
                     (with-meta r {:authorized true})]]
      (is (= :stale-or-forged-provenance-audit
             (reason #(boundary/require-current-supplied-observation-audit!
                       changed *source*)))
          (str "Altered report accepted: " (keys changed))))))

(deftest malformed-top-level-shapes-are-rejected-before-upstream-checks
  (doseq [source [nil [] {} (dissoc *source* :writer-source)
                  (dissoc *source* :observed-source)
                  (assoc *source* :certificate :yes)
                  (with-meta *source* {:trusted true})]]
    (is (= :invalid-source-shape (reason #(audit source))))))

(deftest malformed-observed-source-shapes-are-rejected
  (doseq [bad [nil [] {}
               (dissoc (:observed-source *source*) :transition-source)
               (assoc (:observed-source *source*) :authority :proof)
               (with-meta (:observed-source *source*) {:trusted true})]]
    (is (= :invalid-source-shape
           (reason #(audit (assoc *source* :observed-source bad)))))))

(deftest different-application-source-is-rejected-before-rechecking
  (let [different (assoc-in *source*
                            [:observed-source :application-source]
                            (assoc (:application-source (:observed-source *source*))
                                   :extra :other))]
    (is (= :different-application-sources (reason #(audit different))))))

(deftest operation-from-outside-independent-catalog-is-rejected
  (let [different (assoc-in *source* [:writer-source :induction-catalog]
                            (abstract-catalog [:example/other]))]
    (is (= :physical-observation-not-bound-to-checked-writer
           (reason #(audit different))))))

(deftest changed-issued-guard-source-is-rejected
  (let [different (altered-transition
                   #(assoc-in % [:construction :guard :expression]
                              [:literal true]))]
    (is (= :observation-not-bound-to-issued-plan
           (reason #(audit different))))))

(deftest changed-physical-field-map-source-is-rejected
  (let [different (altered-transition
                   #(assoc-in % [:field-map :request/status] :request/other))]
    (is (= :observation-not-bound-to-issued-plan
           (reason #(audit different))))))

(deftest unknown-observed-source-keys-fail-closed
  (let [different (altered-transition #(assoc % :prove? true))]
    (is (= :invalid-source-shape (reason #(audit different))))))

(deftest unexpected-observation-authority-is-rejected
  (let [different (altered-witness #(assoc % :authority :granted))]
    (is (= :invalid-observation-claims (reason #(audit different))))))

(deftest unknown-observation-claim-is-rejected
  (let [different (altered-witness #(assoc % :proof :proved))]
    (is (= :invalid-source-shape (reason #(audit different))))))

(deftest noncommitted-observation-cannot-be-used
  (let [different (altered-witness #(assoc % :physical-status :commit-unknown))]
    (is (= :invalid-observation-claims (reason #(audit different))))))

(deftest absent-transaction-identity-cannot-be-used
  (let [different (altered-witness #(assoc % :physical-tx-id nil))]
    (is (= :invalid-observation-claims (reason #(audit different))))))

(deftest unchecked-writer-obligations-cannot-be-disguised
  (let [requirements (capture)]
    (is (= :not-established (:actual-submitted-payload-provenance requirements)))
    (is (= :not-established (:transaction-internal-state-provenance requirements)))
    (is (= :not-established (:deployed-writer-provenance requirements)))
    (is (= :not-admitted (:admission requirements)))
    (is (= :none (:transaction-authority requirements)))))

(deftest unexpected-capture-source-fails-closed
  (doseq [bad [nil [] {} (assoc (:writer-source *source*) :extra :yes)
               (with-meta (:writer-source *source*) {:proved true})]]
    (is (map? (error #(boundary/derive-capture-requirements! bad))))))

(deftest status-order-prefers-real-divergence-over-open-catalog
  (let [target (get-in *source* [:observed-source :transition-source :target])
        diverging (altered-witness #(assoc-in % [:observed-after target :request/status]
                                                :open))
        extra (assoc-in diverging [:writer-source :induction-catalog]
                        (abstract-catalog [operation :example/other]))
        r (audit extra)]
    (is (= :observed-physical-divergence (:status r)))
    (no-authority! r)))

(deftest missing-abstract-operation-keeps-catalog-open
  (let [source (assoc-in *source* [:writer-source :induction-catalog]
                         (abstract-catalog [operation :example/other]))
        r (audit source)]
    (is (= :open-writer-catalog-or-induction (:status r)))
    (is (= :open-operation-catalog-join (:writer-ledger-status r)))
    (is (true? (:diagnostic-conforms? r)))
    (no-authority! r)))

(deftest changed-host-enumeration-invalidates-old-report
  (let [old (audit)]
    (swap! *snapshot* assoc :example/unclassified
           {:kind :unclassified :handler (fn [_] {:status 200})})
    (try
      (is (= :stale-or-forged-provenance-audit
             (reason #(boundary/require-current-supplied-observation-audit!
                       old *source*))))
      (is (= :open-writer-catalog-or-induction (:status (audit))))
      (finally (swap! *snapshot* dissoc :example/unclassified)))))

(deftest substituted-writer-checker-is-rejected
  (let [original writers/derive-executable-writer-obligations!]
    (with-redefs [writers/derive-executable-writer-obligations!
                  (fn [source] (assoc (original source) :authority :granted))]
      (is (= :substituted-upstream-checker (reason #(audit)))))))

(deftest substituted-observation-checker-is-rejected
  (let [original observed/check-observed-correspondence!]
    (with-redefs [observed/check-observed-correspondence!
                  (fn [source] (assoc (original source) :authority :granted))]
      (is (= :substituted-upstream-checker (reason #(audit)))))))

(deftest replaced-observation-result-does-not-grant-proof
  (let [r (audit)]
    (doseq [[k bad] [[:certificate :issued]
                     [:admission :admitted]
                     [:transaction-authority :allowed]
                     [:proof-status :proved]
                     [:universal-execution-refinement :proved]
                     [:abstract-to-physical-transition-equivalence :proved]]]
      (is (= :stale-or-forged-provenance-audit
             (reason #(boundary/require-current-supplied-observation-audit!
                       (assoc r k bad) *source*)))))))

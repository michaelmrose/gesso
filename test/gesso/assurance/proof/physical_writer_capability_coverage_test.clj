(ns gesso.assurance.proof.physical-writer-capability-coverage-test
  "v1524: Adversarial, permanent tests for v1523's finite, host-declared
   physical writer-capability diagnostic. Matching callable identities and
   protected targets are not enforced capabilities, an exhaustive registry,
   an authenticated XTDB sink, or an executable physical-refinement proof.

   The embedded-XTDB probe deliberately demonstrates an actual writer whose
   host declaration falsely advertises zero mutation forms. All other probes
   are source inspections and must never call writer functions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [gesso.assurance.proof.physical-writer-capability-coverage :as coverage]
            [gesso.assurance.proof.physical-writer-inventory :as inventory]
            [gesso.assurance.proof.protected-write-domain :as domain]
            [gesso.assurance.proof.operation-writer-coverage-soundness-test]
            [gesso.assurance.proof.xtdb-write-footprint-accounting-test]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(def ^:private issued-path :host/claim)
(def ^:private issued-operation :example/claim)
(def ^:private secret-id "v1524-private-document-id")
(def ^:private secret-marker "v1524-private-marker")
(def ^:dynamic *operation-source* nil)

;; Use the actual issued application/induction fixture from the permanent
;; v1367 suite, not a fabricated positive operation-coverage result.
(use-fixtures :once
  (fn [run]
    (let [owner (the-ns 'gesso.assurance.proof.operation-writer-coverage-soundness-test)
          fixture (-> owner meta :clojure.test/once-fixtures first)]
      (fixture
       (fn []
         (binding [*operation-source* (var-get (ns-resolve owner '*source*))]
           (run)))))))

(defn- protected-domain-source []
  (let [f (var-get
           (ns-resolve 'gesso.assurance.proof.xtdb-write-footprint-accounting-test
                       'source))]
    (select-keys (f secret-id) [:plan :protected-documents])))

(defn- callable
  ([] (callable (atom 0)))
  ([calls] (fn [& _] (swap! calls inc) :unexpected-writer-invocation)))

(defn- cap
  ([writer target] (cap writer target {}))
  ([writer target overrides]
   (merge {:writer writer
           :target-scope :enumerated
           :document-targets #{target}
           :table-targets #{}
           :mutation-forms #{:put-docs}
           :restriction :host-declared}
          overrides)))

(defn- entry
  ([writer] (entry :issued-operation issued-operation writer))
  ([kind operation-id writer]
   {:kind kind :operation-id operation-id :writer writer}))

(defn- scenario
  ([] (scenario (callable)))
  ([writer]
   (let [protected (protected-domain-source)
         target (get-in protected [:protected-documents 0 :target])
         registrations (atom {issued-path (entry writer)})
         enumerate (fn [] @registrations)]
     {:registrations registrations
      :writer writer
      :target target
      :source {:writer-inventory-source
               {:operation-writer-source *operation-source*
                :enumerate-write-paths! enumerate}
               :protected-domain-source protected
               :writer-capabilities {issued-path (cap writer target)}}})))

(defn- inspect! [s] (coverage/inspect-writer-capabilities! s))
(defn- rejection [f]
  (try (f) :not-rejected
       (catch clojure.lang.ExceptionInfo e
         (if (:physical-writer-capability-coverage/rejection (ex-data e))
           (:reason (ex-data e))
           (throw e)))))
(defn- rejects! [reason thunk]
  (is (= reason (rejection thunk)) (str "Expected rejection " reason)))

(defn- no-authority! [r]
  (doseq [[k expected]
          [[:source-binding-authenticates-execution? false]
           [:capability-restrictions-authenticated? false]
           [:host-writer-enumerator-exhaustiveness :not-established]
           [:complete-physical-writer-coverage :not-established]
           [:physical-write-footprint-completeness :not-established]
           [:physical-execution-binding :not-established]
           [:universal-refinement :not-established]
           [:proof-status :host-advertised-conditional-diagnostic-only]
           [:certificate :not-issued]
           [:admission :not-admitted]
           [:transaction-authority :none]
           [:authority :none]]]
    (is (= expected (get r k)) (str "No authority for " k)))
  (is (every? (:open-obligations r)
              [:enumerator-and-capability-declarations-exhaustive-and-accurate
               :callable-to-physical-sink-binding
               :all-processes-and-external-writers-mediated
               :host-target-and-mutation-restrictions-enforced
               :unlisted-documents-and-tables-covered
               :actual-submitted-transaction-vector-binding
               :transaction-internal-prestate-and-poststate-causality
               :physical-refinement-and-certification])))

(deftest genuine-registered-callable-still-only-gives-conditional-join
  (let [{:keys [source target]} (scenario)
        r (inspect! source)]
    (is (= coverage/report-type (:type r)))
    (is (= coverage/report-version (:version r)))
    (is (= :conditional-host-declared-capability-protection-join (:status r)))
    (is (= :conditional-observed-writer-path-id-join (:upstream-writer-status r)))
    (is (= :declared-command-targets-covered-in-finite-domain
           (:declared-protection-status r)))
    (is (= 1 (:observed-writer-count r)))
    (is (= 0 (:observed-nonissued-writer-count r)))
    (is (= 1 (:potential-protected-mutation-path-count r)))
    (is (= 0 (:possible-unlisted-document-access-path-count r)))
    (is (= 1 (:declared-protected-document-count r)))
    (is (= 1 (:potentially-accessible-protected-document-count
              (first (:writer-rows r)))))
    (is (= :not-established (:physical-sink-binding (first (:writer-rows r)))))
    (is (= r (coverage/require-current-writer-capabilities! source r)))
    (is (not (str/includes? (pr-str r) (str (second target)))))
    (no-authority! r)))

(deftest report-and-summary-are-redacted-and-rechecked
  (let [{:keys [source]} (scenario)
        report (inspect! source)
        summary (coverage/redacted-summary source report)]
    (is (string? (:process-local-source-binding report)))
    (is (not (contains? summary :writer-rows)))
    (is (not (contains? summary :process-local-source-binding)))
    (is (= (:status report) (:status summary)))
    (is (= (:open-obligations report) (:open-obligations summary)))
    (doseq [secret [secret-id secret-marker ":host/claim" ":gesso/example"]]
      (is (not (str/includes? (pr-str report) secret)) (str "Report leaked " secret))
      (is (not (str/includes? (pr-str summary) secret)) (str "Summary leaked " secret)))
    (no-authority! summary)))

(deftest declared-read-only-never-establishes-actual-read-only
  (let [calls (atom 0)
        {:keys [source]} (scenario (callable calls))
        source (assoc-in source [:writer-capabilities issued-path :mutation-forms] #{})
        r (inspect! source)]
    (is (= 0 @calls))
    (is (= 0 (:potential-protected-mutation-path-count r)))
    (is (= :conditional-host-declared-capability-protection-join (:status r)))
    (is (false? (-> r :writer-rows first :potential-protected-mutation?)))
    (no-authority! r)))

(deftest undeclared-hidden-writer-remains-invisible-to-finite-survey
  (let [{:keys [source]} (scenario)
        hidden (atom {})
        r (inspect! source)]
    (swap! hidden assoc :host/hidden (entry :raw-xtdb nil (callable)))
    (is (= 1 (count @hidden)))
    (is (= r (coverage/require-current-writer-capabilities! source r)))
    (is (= 1 (:observed-writer-count r)))
    (no-authority! r)))

(deftest every-observed-nonissued-writer-remains-an-open-physical-path
  (doseq [kind [:raw-xtdb :initializer :background-job
                :external-writer :other-writer :unknown]]
    (let [{:keys [source registrations target]} (scenario)
          w (callable)
          path (keyword "host" (name kind))
          _ (swap! registrations assoc path (entry kind nil w))
          source (assoc-in source [:writer-capabilities path] (cap w target))
          r (inspect! source)]
      (is (= :open-nonissued-physical-write-paths (:status r)) (str kind))
      (is (= 2 (:observed-writer-count r)) (str kind))
      (is (= 1 (:observed-nonissued-writer-count r)) (str kind))
      (is (= 2 (:potential-protected-mutation-path-count r)) (str kind))
      (is (contains? (:open-obligations r) :nonissued-write-paths-require-mediation)
          (str kind))
      (no-authority! r))))

(deftest raw-writer-advertising-no-mutations-is-still-nonissued
  (let [{:keys [source registrations target]} (scenario)
        raw (callable)
        _ (swap! registrations assoc :host/direct (entry :raw-xtdb nil raw))
        source (assoc-in source [:writer-capabilities :host/direct]
                         (cap raw target {:mutation-forms #{}}))
        r (inspect! source)]
    (is (= :open-nonissued-physical-write-paths (:status r)))
    (is (= 1 (:observed-nonissued-writer-count r)))
    (is (= 1 (:potential-protected-mutation-path-count r)))
    (no-authority! r)))

(deftest unrestricted-and-unknown-scopes-remain-open
  (doseq [scope [:unrestricted :unknown]]
    (let [{:keys [source]} (scenario)
          altered (-> source
                      (assoc-in [:writer-capabilities issued-path :target-scope] scope)
                      (assoc-in [:writer-capabilities issued-path :document-targets] #{}))
          r (inspect! altered)]
      (is (= :open-unbounded-or-unknown-writer-capabilities (:status r)))
      (is (= 1 (:unbounded-or-unknown-capability-path-count r)))
      (is (= 1 (:potential-protected-mutation-path-count r)))
      (is (= 1 (:possible-unlisted-document-access-path-count r)))
      (no-authority! r))))

(deftest unknown-mutation-forms-cannot-be-read-only
  (let [{:keys [source]} (scenario)
        r (inspect! (assoc-in source [:writer-capabilities issued-path :mutation-forms]
                              :unknown))]
    (is (= :open-unbounded-or-unknown-writer-capabilities (:status r)))
    (is (= :unknown (-> r :writer-rows first :mutation-form-count)))
    (is (true? (-> r :writer-rows first :unrestricted-or-unknown-mutations?)))
    (is (true? (-> r :writer-rows first :whole-document-replacement-advertised?)))
    (no-authority! r)))

(deftest every-noncanonical-physical-write-form-is-accounted-for
  (doseq [form [:patch-docs :delete-docs :erase-docs :sql-dml :other]]
    (let [{:keys [source]} (scenario)
          r (inspect! (assoc-in source [:writer-capabilities issued-path :mutation-forms]
                                #{form}))]
      (is (= :open-noncanonical-physical-mutation-capabilities (:status r)) (str form))
      (is (= 1 (:noncanonical-mutation-path-count r)) (str form))
      (is (= 1 (:potential-protected-mutation-path-count r)) (str form))
      (is (false? (-> r :writer-rows first :whole-document-replacement-advertised?))
          (str form))
      (no-authority! r))))

(deftest partial-field-protection-does-not-protect-replacement
  (doseq [fields [#{:status} #{:status :revision :claims :audit-tag
                              :created-at :updated-at :xt/id}]]
    (let [{:keys [source]} (scenario)
          altered (assoc-in source
                            [:protected-domain-source :protected-documents 0 :fields]
                            fields)
          r (inspect! altered)]
      (is (= :open-partially-protected-replacement-access (:status r)))
      (is (= 1 (:partially-protected-replacement-path-count r)))
      (is (= 1 (-> r :writer-rows first
                   :potentially-accessible-partially-protected-document-count)))
      (is (contains? (:open-obligations r)
                     :whole-document-writes-may-escape-field-protection))
      (no-authority! r))))

(deftest table-access-includes-unnamed-documents
  (let [{:keys [source target]} (scenario)
        table (first target)
        altered (-> source
                    (assoc-in [:writer-capabilities issued-path :document-targets] #{})
                    (assoc-in [:writer-capabilities issued-path :table-targets] #{table}))
        r (inspect! altered)]
    (is (= :open-outside-finite-protection-domain (:status r)))
    (is (= 1 (:potential-protected-mutation-path-count r)))
    (is (= 1 (:possible-unlisted-document-access-path-count r)))
    (is (= 1 (-> r :writer-rows first :potentially-accessible-protected-document-count)))
    (no-authority! r)))

(deftest finite-other-document-is-outside-protection
  (let [{:keys [source]} (scenario)
        r (inspect! (assoc-in source [:writer-capabilities issued-path :document-targets]
                              #{[:gesso/example "unknown-v1524"]}))]
    (is (= :open-outside-finite-protection-domain (:status r)))
    (is (= 0 (:potential-protected-mutation-path-count r)))
    (is (= 1 (:possible-unlisted-document-access-path-count r)))
    (no-authority! r)))

(deftest empty-enumerated-scope-is-no-write-proof
  (let [{:keys [source]} (scenario)
        r (inspect! (-> source
                        (assoc-in [:writer-capabilities issued-path :document-targets] #{})
                        (assoc-in [:writer-capabilities issued-path :mutation-forms] #{})))]
    (is (= :conditional-host-declared-capability-protection-join (:status r)))
    (is (= 0 (:potential-protected-mutation-path-count r)))
    (is (= 0 (:possible-unlisted-document-access-path-count r)))
    (no-authority! r)))

(deftest lack-of-declared-restriction-is-not-enforcement
  (doseq [restriction [:none :unknown]]
    (let [{:keys [source]} (scenario)
          r (inspect! (assoc-in source [:writer-capabilities issued-path :restriction]
                                restriction))]
      (is (= :open-undeclared-capability-restrictions (:status r)))
      (is (= 1 (:undeclared-restriction-path-count r)))
      (no-authority! r))))

(deftest missing-and-extra-capability-entries-reject
  (let [{:keys [source]} (scenario)]
    (rejects! :missing-or-extra-writer-capability
              #(inspect! (assoc source :writer-capabilities {})))
    (rejects! :missing-or-extra-writer-capability
              #(inspect! (assoc-in source
                                   [:writer-capabilities :host/unsurveyed]
                                   (get-in source [:writer-capabilities issued-path]))))))

(deftest callable-identity-mismatch-rejects-even-if-descriptor-matches
  (let [{:keys [source]} (scenario)]
    (rejects! :writer-callable-identity-mismatch
              #(inspect! (assoc-in source
                                   [:writer-capabilities issued-path :writer]
                                   (callable))))))

(deftest capability-shape-and-targets-are-strict
  (let [{:keys [source]} (scenario)
        loc [:writer-capabilities issued-path]]
    (doseq [change [(fn [s] (update-in s loc dissoc :mutation-forms))
                    (fn [s] (assoc-in s (conj loc :extra) :untrusted))]]
      (rejects! :invalid-source-shape #(inspect! (change source))))
    (doseq [[field value]
            [[:target-scope :certified]
             [:document-targets #{:invalid}]
             [:document-targets #{[:gesso/example]}]
             [:document-targets #{[:gesso/example {}]}]
             [:table-targets #{"table-name"}]
             [:table-targets #{:table/name}]
             [:mutation-forms :all]
             [:mutation-forms #{:nonexistent-dml}]
             [:restriction :enforced]
             [:document-targets []]
             [:table-targets []]]]
      (let [expected (if (and (= field :table-targets)
                              (= value #{:table/name}))
                       :not-rejected :invalid-or-ambiguous-capability)]
        (is (= expected
               (rejection #(inspect! (assoc-in source (conj loc field) value))))
            (str "Field " field " value " value))))))

(deftest unbounded-target-capability-is-rejected
  (let [{:keys [source]} (scenario)
        too-many (set (map (fn [i] [:gesso/example (str "target-" i)])
                           (range 257)))]
    (rejects! :invalid-or-ambiguous-capability
              #(inspect! (assoc-in source
                                   [:writer-capabilities issued-path :document-targets]
                                   too-many)))))

(deftest invalid-root-and-metadata-cannot-masquerade-as-sources
  (let [{:keys [source]} (scenario)]
    (doseq [[bad reason]
            [[(assoc source :certificate :issued) :invalid-source-shape]
             [(dissoc source :writer-capabilities) :invalid-source-shape]
             [(with-meta source {:forged true}) :invalid-source-shape]
             [(assoc source :writer-capabilities [])
              :missing-or-extra-writer-capability]
             [(update source :writer-capabilities with-meta {:forged true})
              :missing-or-extra-writer-capability]]]
      (rejects! reason #(inspect! bad)))))

(deftest invalid-or-substituted-upstream-source-rejects
  (let [{:keys [source]} (scenario)]
    (rejects! :invalid-writer-inventory-source
              #(inspect! (assoc-in source [:writer-inventory-source :surprise] true)))
    (rejects! :invalid-protected-domain-source
              #(inspect! (assoc-in source
                                   [:protected-domain-source :protected-documents 0 :fields]
                                   #{})))))

(deftest issued-registration-mismatch-stays-visible
  (let [{:keys [source registrations]} (scenario)
        writer (get-in source [:writer-capabilities issued-path :writer])]
    (reset! registrations {})
    (let [without (assoc source :writer-capabilities {})
          r (inspect! without)]
      (is (= :open-observed-issued-registration-mismatch (:status r)))
      (is (= 0 (:observed-writer-count r)))
      (no-authority! r))
    (reset! registrations {issued-path (entry :issued-operation
                                             :example/not-admitted writer)})
    (let [r (inspect! source)]
      (is (= :open-observed-issued-registration-mismatch (:status r)))
      (is (= 1 (:observed-writer-count r)))
      (no-authority! r))))

(deftest changing-registered-callable-between-surveys-fails-closed
  (let [{:keys [source writer]} (scenario)
        calls (atom 0)
        fresh (callable)
        forged (assoc-in source [:writer-inventory-source :enumerate-write-paths!]
                         (fn []
                           {issued-path
                            (entry (if (< (swap! calls inc) 3)
                                     writer fresh))}))]
    (rejects! :unstable-registered-writer-identity #(inspect! forged))
    (is (= 3 @calls))))

(deftest changed-host-table-is-not-mistaken-for-current-source
  (let [{:keys [source registrations]} (scenario)
        r (inspect! source)
        w (callable)]
    (swap! registrations assoc :host/raw (entry :raw-xtdb nil w))
    (rejects! :missing-or-extra-writer-capability
              #(coverage/require-current-writer-capabilities! source r))
    (let [updated (assoc-in source [:writer-capabilities :host/raw]
                            (cap w (get-in (scenario) [:target])))]
      (is (= :open-nonissued-physical-write-paths (:status (inspect! updated)))))))

(deftest same-observed-descriptors-but-new-callable-invalidate-old-report
  (let [{:keys [source registrations]} (scenario)
        old (inspect! source)
        replacement (callable)
        _ (swap! registrations assoc issued-path (entry replacement))
        changed (assoc-in source [:writer-capabilities issued-path :writer] replacement)
        current (inspect! changed)]
    (is (= (:status old) (:status current)))
    (is (= (:writer-rows old) (:writer-rows current)))
    (is (not= (:process-local-source-binding old)
              (:process-local-source-binding current)))
    (rejects! :stale-or-substituted-capability-report
              #(coverage/require-current-writer-capabilities! changed old))
    (rejects! :writer-callable-identity-mismatch #(inspect! source))))

(deftest mutated-capability-declaration-rejects-old-report
  (let [{:keys [source target]} (scenario)
        old (inspect! source)]
    (doseq [altered [(assoc-in source
                              [:writer-capabilities issued-path :mutation-forms] #{})
                     (assoc-in source
                               [:writer-capabilities issued-path :restriction] :none)
                     (update-in source
                                [:writer-capabilities issued-path :document-targets]
                                conj [:foreign/table "secret-foreign-id"])
                     (assoc-in source
                               [:writer-capabilities issued-path :table-targets]
                               #{(first target)})]]
      (rejects! :stale-or-substituted-capability-report
                #(coverage/require-current-writer-capabilities! altered old)))))

(deftest forged-coverage-and-certification-claims-cannot-be-rechecked
  (let [{:keys [source]} (scenario)
        original (inspect! source)]
    (doseq [forged [(assoc original :certificate :issued)
                    (assoc original :admission :admitted)
                    (assoc original :authority :certified)
                    (assoc original :physical-execution-binding :authenticated)
                    (assoc original :complete-physical-writer-coverage :established)
                    (assoc original :capability-restrictions-authenticated? true)
                    (assoc original :source-binding-authenticates-execution? true)
                    (assoc original :status :certified)
                    (assoc original :open-obligations #{})
                    (update original :writer-rows assoc-in [0 :physical-sink-binding]
                            :established)
                    (dissoc original :certificate)
                    (assoc original :unexpected :privilege)
                    (with-meta original {:forged true})]]
      (rejects! :stale-or-substituted-capability-report
                #(coverage/require-current-writer-capabilities! source forged)))))

(deftest substituted-upstream-checkers-are-rejected
  (let [{:keys [source]} (scenario)]
    (rejects! :substituted-source-checker
              #(with-redefs [inventory/describe-physical-writer-inventory!
                             (fn [_] (throw (ex-info "forged" {})))]
                 (inspect! source)))
    (rejects! :substituted-source-checker
              #(with-redefs [domain/derive-protected-write-domain!
                             (fn [_] {:status :certified})]
                 (inspect! source)))
    (rejects! :substituted-source-checker
              #(with-redefs [domain/require-current-protected-write-domain!
                             (fn [& _] {:status :certified})]
                 (inspect! source)))))

(deftest diagnostics-never-leak-a-failed-target-or-writer
  (let [{:keys [source]} (scenario)
        forged (assoc-in source
                         [:writer-capabilities issued-path :document-targets]
                         #{[:private/table secret-marker]})]
    (let [data (try (inspect! forged) nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))
          report (inspect! forged)]
      (is (nil? data))
      (is (not (str/includes? (pr-str report) secret-marker))))
    (let [bad (assoc-in source [:writer-capabilities issued-path :document-targets]
                        #{[:private/table {:secret secret-marker}]})]
      (try
        (inspect! bad)
        (is false "Expected a redacted target rejection")
        (catch clojure.lang.ExceptionInfo e
          (is (= :invalid-or-ambiguous-capability (:reason (ex-data e))))
          (is (not (str/includes? (pr-str (ex-data e)) secret-marker))))))))

(deftest a-falsely-read-only-writer-can-actually-write-to-xtdb
  ;; This deliberate counterexample is essential: host capability descriptors
  ;; are not sandboxed access-control lists. A clean v1523 join does not even
  ;; establish that a callable really respects an advertised empty write set.
  (with-open [node (xtn/start-node {})]
    (let [calls (atom 0)
          id (str "v1524-direct-writer-" (random-uuid))
          table :gesso_v1524_unmediated_writer_probe
          writer (fn []
                   (swap! calls inc)
                   (xt/execute-tx node [[:put-docs table {:xt/id id
                                                          :marker secret-marker}]]) )
          {:keys [source]} (scenario writer)
          declaration (-> source
                          (assoc-in [:writer-capabilities issued-path :mutation-forms] #{})
                          (assoc-in [:writer-capabilities issued-path :document-targets] #{}))
          r (inspect! declaration)]
      (is (= 0 @calls) "Inspection must not invoke host writer functions")
      (is (= 0 (:potential-protected-mutation-path-count r)))
      (is (= :conditional-host-declared-capability-protection-join (:status r)))
      (no-authority! r)
      (is (some? (writer)) "The same registered callable has a real XTDB sink")
      (is (= 1 @calls))
      (is (= secret-marker
             (:marker (first (xt/q node ["SELECT marker FROM gesso_v1524_unmediated_writer_probe WHERE _id = ?" id])))))
      (is (= r (coverage/require-current-writer-capabilities! declaration r))
          "Even after a real outside write, a source-only recheck is not enforcement"))))

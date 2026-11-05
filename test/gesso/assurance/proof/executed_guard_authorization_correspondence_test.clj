(ns gesso.assurance.proof.executed-guard-authorization-correspondence-test
  "v1389: Permanent adversarial checks for v1388 process-local guard correlation.
   Exercise the issued writer source, typed controller, and real resolved
   operation builder. Only the outgoing XTDB call is stubbed. An application
   reader link, callback agreement, or commit-shaped stub is not evidence of
   policy correctness, XTDB assertion evaluation, or physical refinement."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.assurance.proof.executed-guard-authorization-correspondence :as guard]
            [gesso.assurance.proof.executor-controlled-acquisition :as controlled]
            [gesso.assurance.proof.executor-typed-input-binding-test :as prior]
            [gesso.assurance.proof.executable-writer-correspondence-obligations :as writers]
            [gesso.assurance.proof.physical-execution-provenance-boundary-test :as fixture]
            [gesso.model.command :as command]
            [gesso.model.tx-model :as model]
            [gesso.model.tx :as tx]))

(use-fixtures :once
  (first (:clojure.test/once-fixtures
          (meta (the-ns
                 'gesso.assurance.proof.physical-execution-provenance-boundary-test)))))

(def ^:private actor {:app/actor :user/a})
(def ^:private guard-target [:memberships :membership/example])
(def ^:private guard-check [:memberships :membership/example :active true])
(def ^:private wrong-guard-check [:memberships :membership/example :active false])
(def ^:private required-open
  #{:typed-input-binding :transaction-internal-prestate :guard-authorization
    :assignment-write-footprint :frame-and-unmodeled-fields
    :serial-concurrency-validation :committed-outcome-incorporation
    :writer-provenance-and-deployment :universal-execution-refinement})

(defn- prior-typed-source []
  ;; The v1385 source is built from the original issued application fixture.
  ((var-get (ns-resolve 'gesso.assurance.proof.executor-typed-input-binding-test
                        'typed-source))))

(defn- source []
  (let [original (prior-typed-source)
        config (:configuration original)
        binding (:binding original)
        commands (:commands (:plan ((:construct! config) actor binding)))
        domain-targets (set (map command/target commands))
        domain-tables (set (map first domain-targets))
        update-checks
        (into #{}
              (mapcat (fn [entry]
                        (let [[table id] (command/target entry)]
                          (map (fn [[field value]] [table id field value])
                               (get-in entry [:model/expected :model/checks])))))
              (filter command/update? commands))
        membership {:xt/id :membership/example :revision 3
                    :created-at "t0" :updated-at "t1" :active true}
        version {:revision-key :revision :created-at-key :created-at
                 :updated-at-key :updated-at :compare-keys [:active]}
        physical-guard (command/guard :memberships membership version)
        configured
        (assoc config
               :construction-contract
               {:required-guard-tables #{:memberships}
                :allowed-guard-tables #{:memberships}
                :required-write-tables (conj domain-tables :gesso_commands)
                :allowed-write-tables (conj domain-tables :gesso_commands)}
               :target-contract!
               (fn [_ _]
                 {:required-guard-targets #{guard-target}
                  :allowed-guard-targets #{guard-target}
                  :required-write-targets domain-targets
                  :allowed-write-targets domain-targets})
               :predicate-contract!
               (fn [_ _]
                 {:required-guard-checks #{guard-check}
                  :allowed-guard-checks #{guard-check}
                  :required-update-checks update-checks
                  :allowed-update-checks update-checks})
               :construct!
               (fn [ctx command-binding]
                 (update-in ((:construct! config) ctx command-binding)
                            [:plan :guards] (fnil conj []) physical-guard)))]
    {:controlled-source (assoc original :configuration configured)
     :guard-reader-links {:request/status #{guard-check}}}))

(defn- submit-counting [calls response]
  (fn [_ _]
    (swap! calls inc)
    (if (fn? response) (response) response)))

(defn- submit! [src]
  (guard/run-guard-corresponded-typed-submit! src actor))

(defn- rejection-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- rejected-as [f]
  (let [data (rejection-data f)]
    (or (:reason data) (:error/kind data))))

(defn- committed [calls]
  (submit-counting calls {:commit/status :committed :tx-result {:tx-id 72}}))

(defn- negative-authority! [r]
  (is (= guard/guard-report-type (:type r)))
  (is (= guard/guard-report-version (:version r)))
  (is (= :guard-plan-correlation-diagnostic-only (:proof-status r)))
  (is (= :not-admitted (:admission r)))
  (is (= :not-issued (:certificate r)))
  (is (= :none (:authority r)))
  (is (= :none (:transaction-authority r)))
  (is (= :open (:guard-authorization-obligation r)))
  (is (= :not-established (:actual-guard-evaluation-by-xtdb r)))
  (is (= :not-established (:abstract-guard-to-authorization-policy-equivalence r)))
  (is (= :trusted-application-supplied-links (:reader-to-physical-check-origin r)))
  (is (= :registered-application-callback (:predicate-declaration-origin r)))
  (is (= :not-established (:complete-writer-mediation r)))
  (is (= :not-established (:universal-execution-refinement r)))
  (is (= required-open (set (map :id (:physical-refinement-obligations r)))))
  (is (every? #(= :open (:status %)) (:physical-refinement-obligations r)))
  (is (= :not-issued (get-in r [:controlled-result :certificate])))
  (is (= :none (get-in r [:controlled-result :authority]))))

(deftest required-guard-agreement-does-not-prove-authorization
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (committed calls)]
      (let [result (submit! (source))
            observed (:guard-observation result)]
        (is (= 1 @calls))
        (is (= :linked-required-guards-in-prepared-plan (:status observed)))
        (is (true? (:linked-guard-plan-agreement? observed)))
        (is (= 1 (:linked-abstract-reader-count observed)))
        (is (= 1 (:linked-check-count observed)))
        (is (= 1 (:predicate-callback-count observed)))
        (is (= 1 (:required-predicate-count observed)))
        (is (= 1 (:prepared-guard-check-count observed)))
        (is (true? (:complete-paired-plan-observed? observed)))
        (is (= :matched (get-in result [:controlled-result
                                       :typed-input-observation
                                       :construction-site-correlation])))
        (is (= :incorporated (get-in result [:controlled-result :resolution :status])))
        (is (uuid? (:attempt-id result)))
        (negative-authority! result)))))

(deftest incorrect-reader-link-is-a-negative-diagnostic-after-commit
  (let [calls (atom 0)
        wrong (assoc-in (source) [:guard-reader-links :request/status]
                        #{wrong-guard-check})]
    (with-redefs [tx/transact! (committed calls)]
      (let [result (submit! wrong)]
        (is (= 1 @calls))
        (is (= :abstract-link-or-guard-plan-divergence
               (get-in result [:guard-observation :status])))
        (is (false? (get-in result [:guard-observation
                                   :linked-guard-plan-agreement?])))
        (is (= :incorporated (get-in result [:controlled-result :resolution :status])))
        (negative-authority! result)))))

(deftest reader-links-must-be-complete-and-no-other-readers-admitted
  (let [calls (atom 0) original (source)]
    (with-redefs [tx/transact! (committed calls)]
      (doseq [links [{}
                     {:request/extra #{guard-check}}
                     {:request/status #{guard-check}
                      :request/extra #{wrong-guard-check}}
                     nil
                     []]]
        (is (= :incomplete-abstract-guard-links
               (rejected-as #(submit! (assoc original :guard-reader-links links))))))
      (is (= 0 @calls)))))

(deftest malformed-and-ambiguous-reader-links-fail-before-submission
  (let [calls (atom 0) original (source)]
    (with-redefs [tx/transact! (committed calls)]
      (doseq [group [#{} [] nil #{[:memberships :membership/example :active]}
                     #{[:memberships :membership/example :active nil]}
                     #{["memberships" :membership/example :active true]}
                     #{[:memberships nil :active true]}
                     #{[:memberships :membership/example "active" true]}
                     #{(with-meta guard-check {:forged true})}]]
        (is (= :invalid-or-ambiguous-guard-links
               (rejected-as #(submit! (assoc original :guard-reader-links
                                             {:request/status group}))))))
      (is (= :incomplete-abstract-guard-links
             (rejected-as #(submit! (assoc original :guard-reader-links
                                           (with-meta {:request/status #{guard-check}}
                                             {:forged true}))))))
      (is (= 0 @calls)))))

(deftest malformed-top-level-and-controlled-source-are-rejected
  (let [calls (atom 0) original (source)]
    (with-redefs [tx/transact! (committed calls)]
      (doseq [src [nil {} (assoc original :extra :value)
                   (dissoc original :controlled-source)
                   (with-meta original {:forged true})]]
        (is (= :invalid-source-shape (rejected-as #(submit! src)))))
      (is (= :invalid-source-shape
             (rejected-as #(submit! (assoc original :controlled-source
                                          (assoc (:controlled-source original)
                                                 :surprise true))))))
      (is (= 0 @calls)))))

(deftest absent-or-malformed-closed-predicate-registration-rejects
  (let [calls (atom 0) original (source)]
    (with-redefs [tx/transact! (committed calls)]
      (doseq [config (->> [(dissoc (get-in original [:controlled-source :configuration])
                                  :predicate-contract!)
                           (assoc (get-in original [:controlled-source :configuration])
                                  :predicate-contract! nil)
                           (assoc (get-in original [:controlled-source :configuration])
                                  :predicate-contract! :not-a-function)
                           (dissoc (get-in original [:controlled-source :configuration])
                                   :target-contract!)
                           (dissoc (get-in original [:controlled-source :configuration])
                                   :construction-contract)])]
        (is (= :missing-closed-predicate-registration
               (rejected-as #(submit!
                              (assoc-in original
                                        [:controlled-source :configuration] config))))))
      (is (= 0 @calls)))))

(deftest forged-upstream-checker-entry-points-refuse-before-execution
  (let [calls (atom 0) original (source)]
    (with-redefs [tx/transact! (committed calls)]
      (doseq [[v fake]
              [[#'controlled/run-controlled-typed-submit! (fn [& _] :fake)]
               [#'writers/derive-executable-writer-obligations! (fn [& _] :fake)]
               [#'model/admitted-plan (fn [& _] {:guards []})]
               [#'tx/guard-target (fn [& _] [:memberships :fake])]]]
        (with-redefs-fn {v fake}
          (fn []
            (is (= :substituted-upstream-checker
                   (rejected-as #(submit! original)))))))
      (is (= 0 @calls)))))

(deftest altered-application-callback-is-not-a-positive-guard-correlation
  (let [calls (atom 0) original (source)
        changed (assoc-in original
                          [:controlled-source :configuration :predicate-contract!]
                          (fn [_ _]
                            {:required-guard-checks #{wrong-guard-check}
                             :allowed-guard-checks #{wrong-guard-check}
                             :required-update-checks #{}
                             :allowed-update-checks #{}}))]
    (with-redefs [tx/transact! (committed calls)]
      (is (some? (rejected-as #(submit! changed))))
      (is (zero? @calls)))))

(deftest missing-physical-guard-blocks-submission-not-just-reporting
  (let [calls (atom 0) original (source)
        make-plan (get-in original [:controlled-source :configuration :construct!])
        changed (assoc-in original
                          [:controlled-source :configuration :construct!]
                          (fn [ctx binding]
                            (update (make-plan ctx binding) :plan dissoc :guards)))]
    (with-redefs [tx/transact! (committed calls)]
      (is (some? (rejected-as #(submit! changed))))
      (is (zero? @calls)))))

(deftest rejected-xtdb-assertion-is-never-promoted-to-authorization-success
  (let [calls (atom 0)]
    (with-redefs [tx/transact!
                  (submit-counting calls
                                   #(throw (ex-info "assert rejected"
                                                    {:xtdb.error/code :xtdb/assert-failed})))]
      (let [result (submit! (source))]
        (is (= 1 @calls))
        (is (not= :incorporated
                  (get-in result [:controlled-result :resolution :status])))
        (is (= :open (:guard-authorization-obligation result)))
        (is (not= :authenticated-policy-equivalence
                  (get-in result [:guard-observation :status])))
        (negative-authority! result)))))

(deftest indeterminate-physical-outcome-is-not-a-complete-guard-observation
  (let [calls (atom 0)]
    (with-redefs [tx/transact!
                  (submit-counting calls {:commit/status :unknown})]
      (let [result (submit! (source))]
        (is (= 1 @calls))
        (is (= :no-complete-transaction-observation
               (get-in result [:guard-observation :status])))
        (is (false? (get-in result [:guard-observation
                                   :linked-guard-plan-agreement?])))
        (is (= :open (:guard-authorization-obligation result)))
        (negative-authority! result)))))

(deftest distinct-submissions-do-not-reuse-attempt-identity
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (committed calls)]
      (let [first-report (submit! (source))
            second-report (submit! (source))]
        (is (= 2 @calls))
        (is (not= (:attempt-id first-report) (:attempt-id second-report)))
        (is (= :linked-required-guards-in-prepared-plan
               (get-in first-report [:guard-observation :status])))
        (is (= :linked-required-guards-in-prepared-plan
               (get-in second-report [:guard-observation :status])))
        (negative-authority! first-report)
        (negative-authority! second-report)))))

(deftest report-does-not-publish-guard-values-or-raw-callbacks
  (let [calls (atom 0)]
    (with-redefs [tx/transact! (committed calls)]
      (let [result (submit! (source))
            diagnostic (:guard-observation result)]
        (is (= 1 @calls))
        (is (not-any? #{guard-check wrong-guard-check guard-target actor}
                      (vals diagnostic)))
        (is (not-any? fn? (vals diagnostic)))
        (is (not (contains? result :guard-reader-links)))
        (is (not (contains? result :required-guard-checks)))
        (is (not (contains? diagnostic :declaration)))
        (is (not (contains? diagnostic :prepared-plan)))
        (negative-authority! result)))))

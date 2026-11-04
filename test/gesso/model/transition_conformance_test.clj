(ns gesso.model.transition-conformance-test
  "v1301: permanent adversarial checks of the v1300 diagnostic comparison.

   The real embedded-XTDB fixtures deliberately separate (a) a committed
   incorporation record, (b) its observed domain write, and (c) the typed
   abstract transition. These tests prove neither authenticated observation
   provenance nor universal XTDB refinement: the witness is a caller-owned,
   copyable map. That limitation is tested rather than concealed."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gesso.choreo.identity :as identity]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.transition-conformance :as conformance]
            [gesso.model.transition-observation :as observation]
            [gesso.model.tx :as tx]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))

(defonce ^:private !node (atom nil))
(defn- with-xtdb [f]
  (with-open [db (xtn/start-node {})]
    (reset! !node db)
    (try (f)
         (finally (reset! !node nil)))))
(use-fixtures :once with-xtdb)

(def ^:private version
  {:revision-key :request/version
   :created-at-key :request/created-at
   :updated-at-key :request/updated-at})

(def ^:private construction
  {:subject {:kind :operation :id :example/claim
             :interface {:operation :example/claim}}
   :state-types {:request/status {:type :enum :name :request/status
                                  :values #{:open :claimed}}
                 :request/version {:type :integer}
                 :request/score {:type :integer}
                 :request/created-at {:type :string}
                 :request/updated-at {:type :string}
                 :request/note {:type :string :optional? true :nullable? true}}
   :input-types {:input/next-version {:type :integer}}
   :guard {:readers #{:request/status :request/score}
           :expression [:and
                        [:= [:read :request/status] [:literal :open]]
                        [:> [:read :request/score] [:literal 7]]]}
   :reads #{:request/status :request/score :input/next-version}
   :writes #{:request/status :request/version :request/updated-at}
   :replacements {:request/status [:literal :claimed]
                  :request/version [:read :input/next-version]
                  :request/updated-at [:literal "t1"]}
   :frame #{:request/score :request/created-at :request/note}})

(def ^:private field-map
  {:request/status :request/status
   :request/version :request/version
   :request/score :request/score
   :request/created-at :request/created-at
   :request/updated-at :request/updated-at
   :request/note :request/note})

(defn- prepared-sample! []
  (let [id (str (random-uuid))
        before {:xt/id id :request/status :open :request/version 2
                :request/score 12 :request/created-at "t0"
                :request/updated-at "t0"}
        after (assoc before :request/status :claimed
                           :request/version 3 :request/updated-at "t1")
        binding {:command-id (identity/command-id (str (random-uuid)))
                 :principal (identity/principal "helper")
                 :operation :example/claim
                 :arguments {:request id}}
        incorporation (resolution/incorporation-plan
                       {:binding binding :outcome {:status :claimed} :now "t1"
                        :plan {:commands [(command/update-command
                                           :requests :claim before after version)]
                               :emit false}})
        target [:requests id]
        command-target [:gesso_commands (:xt/id (:record incorporation))]]
    (xt/execute-tx @!node [[:put-docs :requests before]])
    {:before before :after after :binding binding
     :target target :command-target command-target
     :incorporation incorporation :targets [target command-target]}))

(defn- committed-sample! [mode]
  (let [sample (prepared-sample!)
        handle (observation/begin-observation!
                {:node @!node :incorporation (:incorporation sample)
                 :targets (:targets sample)})
        plan (get-in sample [:incorporation :plan])
        submitted (cond
                    (= mode :record-only)
                    (assoc plan :commands (filterv command/create?
                                                   (:commands plan)))

                    (= mode :wrong-write)
                    (assoc plan :commands
                           (mapv (fn [cmd]
                                   (if (command/update? cmd)
                                     (command/update-command
                                      :requests :claim (:before sample)
                                      (assoc (:after sample) :request/status :open)
                                      version)
                                     cmd))
                                 (:commands plan)))

                    :else plan)
        tx-key (xt/execute-tx @!node (:tx-ops (tx/prepare {} submitted)))
        witness (observation/complete-observation! handle {:tx-key tx-key})]
    (assoc sample :witness witness :tx-key tx-key)))

(defn- source [sample]
  {:observation (:witness sample)
   :construction construction
   :field-map field-map
   :inputs {:input/next-version 3}
   :target (:target sample)
   :command-operation :claim})

(defn- check! [sample]
  (conformance/check-observed-transition! (source sample)))

(defn- error-data [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- reason [f]
  (:reason (error-data f)))

(defn- compare-witness [sample altered]
  (conformance/check-observed-transition!
   (assoc (source sample) :observation altered)))

(defn- nonconforming [sample altered expected-failed-check]
  (let [result (compare-witness sample altered)]
    (is (false? (:corresponds? result)))
    (is (false? (get-in result [:checks expected-failed-check])))
    (is (= :not-established (:general-refinement result)))
    (is (= :none (:transaction-authority result)))
    result))

(deftest public-scope-keeps-provenance-and-refinement-open
  (is (= 1 conformance/conformance-version))
  (is (= :gesso.model.transition-conformance/observed-abstract-step
         conformance/report-type))
  (let [scope conformance/comparison-scope]
    (is (= :diagnostic-bounded-observed-transition-comparison
           (:evidence-class scope)))
    (doseq [unproved [:authenticated-xtdb-submission-or-observation-provenance
                     :transaction-internal-prestate-correspondence
                     :all-production-writer-coverage
                     :universal-xtdb-refinement
                     :authorization-and-business-policy-correctness
                     :independent-checker-soundness
                     :application-invariant-proof-or-certification
                     :runtime-transaction-authority]]
      (is (contains? (:not-established scope) unproved)))))

(deftest correct-persisted-update-conforms-but-grants-no-authority
  (let [sample (committed-sample! :normal)
        r (check! sample)]
    (is (true? (:corresponds? r)) (pr-str (:checks r)))
    (is (every? true? (vals (:checks r))))
    (is (= 2 (:observed-target-count r)))
    (is (= (:tx-id (:tx-key sample)) (:physical-tx-id r)))
    (is (= :caller-supplied-copyable-v1298-report
           (:observation-provenance r)))
    (is (= :not-established (:general-refinement r)))
    (is (= :none (:certification r)))
    (is (= :none (:transaction-authority r)))))

(deftest recorded-commit-with-no-domain-write-is-not-conformance
  (let [sample (committed-sample! :record-only)
        r (check! sample)]
    (is (false? (:corresponds? r)))
    (is (false? (get-in r [:checks :model-poststate])))
    (is (false? (get-in r [:checks :abstract-assignments])))
    (is (true? (get-in r [:checks :command-incorporation])))
    (is (= :none (:certification r)))))

(deftest real-committed-wrong-domain-write-is-not-conformance
  (let [sample (committed-sample! :wrong-write)
        r (check! sample)]
    (is (= :committed (get-in sample [:witness :physical-status])))
    (is (false? (:corresponds? r)))
    (is (false? (get-in r [:checks :abstract-assignments])))
    (is (false? (get-in r [:checks :model-poststate])))
    (is (true? (get-in r [:checks :command-incorporation])))
    (is (= :none (:transaction-authority r)))))

(deftest wrong-enum-poststate-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-after target :request/status] :open)
                   :abstract-assignments)))

(deftest wrong-integer-poststate-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-after target :request/version] 4)
                   :abstract-assignments)))

(deftest wrong-field-type-is-a-mismatch-not-a-success
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-after target :request/version]
                             "three")
                   :abstract-assignments)))

(deftest missing-required-poststate-field-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (update-in witness [:observed-after target]
                              dissoc :request/score)
                   :complete-physical-domain-projection)))

(deftest unexpected-physical-field-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-after target :unmapped] "secret")
                   :complete-physical-domain-projection)))

(deftest additional-optional-field-is-a-presence-sensitive-change
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)
        altered (assoc-in witness [:observed-after target :request/note] "new")]
    (nonconforming sample altered :abstract-frame)
    (is (false? (get-in (compare-witness sample altered)
                        [:checks :model-poststate])))))

(deftest disappearing-physical-row-cannot-conform
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-after target] nil)
                   :complete-physical-domain-projection)))

(deftest altered-observed-prestate-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-before target :request/score] 11)
                   :declared-prestate)))

(deftest altered-observed-prestate-type-is-not-conformance
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (nonconforming sample
                   (assoc-in witness [:observed-before target :request/score]
                             "twelve")
                   :declared-prestate)))

(deftest observed-guard-failure-cannot-appear-as-correspondence
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)
        modified (assoc-in witness
                           [:observed-before target :request/status] :claimed)
        r (nonconforming sample modified :abstract-guard)]
    (is (false? (get-in r [:checks :declared-prestate])))))

(deftest physical-document-id-is-part-of-the-required-projection
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)
        malformed (assoc-in witness [:observed-after target :xt/id]
                            "different")]
    ;; The older serial refinement checker rejects impossible physical
    ;; document identities before the v1300 diagnostic can return checks.
    (is (= :gesso.model.tx-refinement/invalid-physical-document
           (:error/type (error-data #(compare-witness sample malformed)))))))

(deftest correct-physical-update-with-wrong-supplied-input-rejects
  (let [sample (committed-sample! :normal)]
    (is (= :declared-write-mismatch
           (reason #(conformance/check-observed-transition!
                     (assoc (source sample) :inputs {:input/next-version 999})))))))

(deftest changed-original-replacement-cannot-borrow-conformance
  (let [sample (committed-sample! :normal)
        modified (assoc-in construction [:replacements :request/status]
                           [:literal :open])]
    (is (= :declared-write-mismatch
           (reason #(conformance/check-observed-transition!
                     (assoc (source sample) :construction modified)))))))

(deftest changed-physical-field-mapping-rejects-candidate
  (let [sample (committed-sample! :normal)]
    (is (some? (error-data
                #(conformance/check-observed-transition!
                  (assoc (source sample) :field-map
                         (assoc field-map :request/status :wrong-field))))))))

(deftest mismatched-observed-targets-cannot-pass
  (let [{:keys [witness target] :as sample} (committed-sample! :normal)]
    (is (= :inconsistent-observed-footprint-or-command
           (reason #(compare-witness
                     sample (assoc witness :observed-targets #{target})))))))

(deftest removed-command-target-before-map-cannot-pass
  (let [{:keys [witness command-target] :as sample} (committed-sample! :normal)]
    (is (= :inconsistent-observed-footprint-or-command
           (reason #(compare-witness
                     sample (update witness :observed-before dissoc
                                  command-target)))))))

(deftest tampered-command-record-cannot-pass-footprint-check
  (let [{:keys [witness] :as sample} (committed-sample! :normal)]
    (is (some? (error-data
                #(compare-witness sample
                                  (assoc witness :persisted-command-record
                                         {:xt/id "forged"})))))))

(deftest fabricated-observation-claims-are-rejected
  (let [{:keys [witness] :as sample} (committed-sample! :normal)]
    (doseq [[k v] [[:physical-status :rejected]
                   [:ledger-window :not-observed]
                   [:witness-provenance :authenticated]
                   [:abstract-transition-correspondence :proved]
                   [:untracked-writer-coverage :complete]
                   [:certification :certified]
                   [:authority :execute]
                   [:physical-tx-id -1]]]
      (is (= :invalid-observation-claims
             (reason #(compare-witness sample (assoc witness k v))))
          (str k " cannot become authoritative")))))

(deftest mismatched-observation-version-or-scope-is-rejected
  (let [{:keys [witness] :as sample} (committed-sample! :normal)]
    (is (= :invalid-observation-claims
           (reason #(compare-witness sample (assoc witness :version 500)))))
    (is (= :invalid-observation-claims
           (reason #(compare-witness
                     sample (assoc witness :scope {:checking :proved})))))))

(deftest strict-source-shapes-reject-unrecognized-keys-and-metadata
  (let [{:keys [witness] :as sample} (committed-sample! :normal)]
    (is (= :invalid-source-shape
           (reason #(conformance/check-observed-transition!
                     (assoc (source sample) :proved? true)))))
    (is (= :invalid-source-shape
           (reason #(conformance/check-observed-transition!
                     (dissoc (source sample) :inputs)))))
    (is (= :invalid-source-shape
           (reason #(conformance/check-observed-transition!
                     (with-meta (source sample) {:certified true})))))
    (is (= :invalid-source-shape
           (reason #(compare-witness sample
                                     (assoc witness :valid? true)))))
    (is (= :invalid-source-shape
           (reason #(compare-witness sample
                                     (with-meta witness {:proof true})))))))

(deftest no-authenticated-provenance-can-be-inferred-from-copied-witness
  (let [{:keys [witness target after] :as sample}
        (committed-sample! :record-only)
        physical (get-in witness [:observed-after target])]
    (is (= (:before sample) physical))
    ;; This fabricated poststate has the exact expected structure, but the
    ;; actual transaction only inserted the command record. A copyable
    ;; observation cannot authenticate itself; the checker is diagnostic.
    (let [fabricated (assoc-in witness [:observed-after target] after)
          r (compare-witness sample fabricated)]
      (is (true? (:corresponds? r)))
      (is (= :caller-supplied-copyable-v1298-report
             (:observation-provenance r)))
      (is (= :not-established (:general-refinement r)))
      (is (contains? (get-in r [:scope :not-established])
                     :authenticated-xtdb-submission-or-observation-provenance))
      (is (= :none (:certification r)))
      (is (= :none (:transaction-authority r))))))

(deftest report-status-is-copyable-and-not-an-admission-handle
  (let [sample (committed-sample! :normal)
        r (check! sample)
        forged (assoc r :corresponds? true :certification :proof)]
    (is (map? forged))
    (is (= :none (:certification r)))
    (is (= :none (:transaction-authority r)))
    (is (= :not-established (:general-refinement r)))
    (is (= :caller-supplied-copyable-v1298-report
           (:observation-provenance r)))))

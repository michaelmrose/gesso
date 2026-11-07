(ns gesso.assurance.proof.xtdb-gateway-backend-compatibility-test
  "v1546: Permanent fail-closed regression tests for v1545's reviewed XTDB
   beta1 API and Gesso query/write option-surface prerequisite. Test the
   preparation, one-shot submission, and exact-return observation boundaries
   independently. These are version/interface checks, NOT backend semantics
   proofs, authenticated deployment identity, or physical causality evidence."
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [gesso.assurance.proof.xtdb-gateway-acquired-poststate-correspondence :as correspondence]
            [gesso.assurance.proof.xtdb-internal-prestate-capability :as backend]
            [gesso.live.consistency.xtdb :as live]
            [gesso.model.command :as command]
            [xtdb.api :as xt]
            [xtdb.node :as xtn]))
(def ^:private table :gesso_v1546_compatibility_probe)
(def ^:private versions {:revision-key :revision
                         :created-at-key :created-at
                         :updated-at-key :updated-at})
(def ^:private obligation
  {:construction
   {:subject {:kind :operation :id :example/claim
              :interface {:operation :example/claim}}
    :state-types {:request/status {:type :enum :name :request/status
                                   :values #{:open :claimed :done}}
                  :request/owner {:type :keyword :optional? true}
                  :request/version {:type :integer}
                  :request/note {:type :string :optional? true :nullable? true}}
    :input-types {:input/actor {:type :keyword}}
    :guard {:readers #{:request/status}
            :expression [:= [:read :request/status] [:literal :open]]}
    :reads #{:request/status :input/actor}
    :writes #{:request/status :request/owner}
    :replacements {:request/status [:literal :claimed]
                   :request/owner [:read :input/actor]}
    :frame #{:request/version :request/note}}
   :initial-condition {:readers #{:request/status}
                       :expression [:= [:read :request/status] [:literal :open]]}
   :invariants {:request/status-valid
                {:readers #{:request/status}
                 :expression [:in [:read :request/status]
                              [:literal #{:open :claimed :done}]]}
                :request/version-valid
                {:readers #{:request/version}
                 :expression [:>= [:read :request/version] [:literal 0]]}}})
(defonce ^:private !node (atom nil))
(defn- with-node [f]
  (with-open [n (xtn/start-node {})]
    (reset! !node n)
    (try (f) (finally (reset! !node nil)))))
(use-fixtures :once with-node)
(defn- before [] {:xt/id (str "v1546-" (random-uuid))
                  :revision 0 :created-at "created" :updated-at "before"
                  :value "private-v1546-secret-marker"})
(defn- after [b] (-> b (assoc :revision 1 :updated-at "after")))
(defn- projection-source [b]
  (let [id (:xt/id b)]
    {:obligation-source obligation
     :protected-domain-source
     {:plan {:commands [(command/update-command table :claim b (after b) versions)]
             :emit false}
      :protected-documents [{:target [table id] :fields :all}]}
     :field-locations
     {:request/status {:target [table id] :physical-field :revision}
      :request/owner {:target [table id] :physical-field :updated-at}
      :request/version {:target [table id] :physical-field :created-at}
      :request/note {:target [table id] :physical-field :value}}}))
(defn- put! [doc] (xt/execute-tx @!node [[:put-docs table doc]]))
(defn- prep [b] (correspondence/prepare-acquired-poststate-one-shot!
                 @!node (projection-source b)))
(defn- rejection [f]
  (try (f) :no-error (catch Throwable e
                     (or (:reason (ex-data e)) (:xtdb.error/code (ex-data e))
                         (throw e)))))

(def ^:private version-var
  (ns-resolve 'gesso.assurance.proof.xtdb-internal-prestate-capability
              'runtime-api-version))
(defn- under-unrecognized-version [f]
  (with-redefs-fn {version-var (fn [] nil)} f))

(deftest reviewed-backend-supports-authentic-submission-and-inference
  (let [b (before) _ (put! b)
        prepared (prep b)
        early ((:summary! prepared))
        response ((:submit! prepared))
        observation ((:observe! prepared) response)]
    (is (= :pinned-2_2_0-beta1-interface-only
           (:backend-api-check-at-preparation early)))
    (is (= :pinned-2_2_0-beta1-interface-only
           (:backend-api-check-at-observation observation)))
    (is (= :declared-postimage-under-trusted-xtdb-semantics
           (:transaction-local-write-effect observation)))
    (is (= :not-established (:actual-transaction-caused-poststate observation)))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))))

(deftest unknown-xtdb-api-version-rejects-before-capability-issuance
  (let [b (before) _ (put! b)]
    (is (= :unreviewed-backend-api-or-option-surface
           (under-unrecognized-version #(rejection (fn [] (prep b))))))
    (is (= b (first (xt/q @!node
                          ["SELECT * FROM gesso_v1546_compatibility_probe WHERE _id = ?"
                           (:xt/id b)]))))))

(deftest a-changed-version-after-preparation-consumes-outer-one-shot-permit
  (let [b (before) _ (put! b)
        prepared (prep b)]
    (is (= :unreviewed-backend-api-or-option-surface
           (under-unrecognized-version #(rejection (:submit! prepared)))))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))
    (is (= :backend-preflight-rejected (:status ((:summary! prepared)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))))

(deftest a-changed-version-during-observation-cannot-issue-positive-effect
  (let [b (before) _ (put! b)
        prepared (prep b)
        response ((:submit! prepared))]
    (is (= :unreviewed-backend-api-or-option-surface
           (under-unrecognized-version
            #(rejection (fn [] ((:observe! prepared) response))))))
    (is (= :declared-postimage-under-trusted-xtdb-semantics
           (:transaction-local-write-effect ((:observe! prepared) response))))))

(deftest changing-public-capability-checker-itself-fails-closed
  (let [b (before) _ (put! b)
        prepared (prep b)]
    (with-redefs [backend/capability-report! (fn [] {:compatibility :pinned-api-and-gesso-option-surface})]
      (is (= :substituted-correspondence-seam (rejection (:submit! prepared)))))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))))

(deftest changed-query-option-contract-rejects-before-submission
  (let [b (before) _ (put! b)
        prepared (prep b)]
    (with-redefs [live/query-option-keys #{}]
      (is (= :unreviewed-backend-api-or-option-surface
             (rejection (:submit! prepared)))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))))

(deftest changed-write-option-contract-rejects-before-capability
  (let [b (before) _ (put! b)]
    (with-redefs [live/tx-option-keys #{:snapshot-token}]
      (is (= :unreviewed-backend-api-or-option-surface
             (rejection #(prep b)))))))

(deftest backend-capability-error-fails-closed-and-consumes-one-shot-permit
  (let [b (before) _ (put! b)
        prepared (prep b)]
    (with-redefs-fn {version-var (fn [] (throw (ex-info "simulated manifest error" {})))}
      (fn [] (is (= :backend-capability-unavailable
                    (rejection (:submit! prepared))))))
    (is (= 0 (:owned-execute-tx-call-entries ((:summary! prepared)))))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))))


;; A compatible implementation-version manifest and reviewed option filters
;; are a PREREQUISITE to the bounded inference, not authentication of the
;; supplied node or a theorem about the XTDB implementation.
(def ^:private positive-effect :declared-postimage-under-trusted-xtdb-semantics)
(def ^:private unsupported :unreviewed-backend-api-or-option-surface)
(def ^:private nonclaims
  {:transaction-internal-prestate :not-authenticated
   :actual-transaction-caused-poststate :not-established
   :aba-history-detection :not-established
   :outside-gateway-writer-closure :not-established
   :complete-deployment-writer-coverage :not-established
   :caller-node-authenticity :not-established
   :principal-authorization :not-established
   :universal-physical-refinement :not-established
   :certificate :not-issued
   :authority :none})

(defn- in-version [v f]
  (with-redefs-fn {version-var (fn [] v)} f))

(defn- state! [prepared]
  ((:summary! prepared)))

(defn- raw [id]
  (first (xt/q @!node ["SELECT * FROM gesso_v1546_compatibility_probe WHERE _id = ?" id])))

(defn- assert-preserved! [doc prepared]
  (is (= doc (raw (:xt/id doc))))
  (is (= :backend-preflight-rejected (:status (state! prepared))))
  (is (zero? (:owned-execute-tx-call-entries (state! prepared))))
  (is (= :one-shot-already-attempted (rejection (:submit! prepared)))))

(defn- assert-no-proof! [report]
  (doseq [[key expected] nonclaims]
    (is (= expected (get report key)) (str "Nonclaim " key)))
  (is (= :source-bound-conditional-observation-only (:proof-status report)))
  (is (not (.contains (pr-str report) "private-v1546-secret-marker"))))

(deftest loaded-backend-capability-is-only-a-reviewed-interface-prerequisite
  (let [r (backend/capability-report!)]
    (is (= backend/report-type (:type r)))
    (is (= backend/report-version (:version r)))
    (is (= backend/pinned-backend-version (get-in r [:backend :expected-version])))
    (is (= backend/pinned-backend-version (get-in r [:backend :loaded-api-version])))
    (is (= :pinned-api-and-gesso-option-surface (:compatibility r)))
    (is (= #{:pinned-runtime-api-version
             :gesso-query-basis-options-present
             :gesso-write-options-present
             :gesso-does-not-forward-query-basis-as-write-options}
           (set (keys (:checks r)))))
    (is (every? true? (vals (:checks r))))
    (is (false? (:prior-experiments-reexecuted? r)))
    (is (= :not-established (:proof-status r)))
    (is (= :not-issued (:certificate r)))
    (is (= :none (:authority r)))))

(deftest incompatible-nonblank-api-version-refuses-preparation
  (let [b (before) _ (put! b)]
    (doseq [version ["2.2.0-beta2" "2.2.1" "0.0.0" "unknown" ""]]
      (is (= unsupported (in-version version #(rejection (fn [] (prep b))))))
      (is (= b (raw (:xt/id b)))))))

(deftest missing-required-query-options-refuse-preparation
  (let [b (before) _ (put! b)]
    (doseq [k [:snapshot-token :snapshot-time :current-time :await-token]]
      (with-redefs [live/query-option-keys (disj (set live/query-option-keys) k)]
        (is (= unsupported (rejection #(prep b))) (str "Required query option " k))))
    (is (= b (raw (:xt/id b))))))

(deftest missing-required-write-options-refuse-preparation
  (let [b (before) _ (put! b)]
    (doseq [k [:database :system-time :default-tz :metadata :authn]]
      (with-redefs [live/tx-option-keys (disj (set live/tx-option-keys) k)]
        (is (= unsupported (rejection #(prep b))) (str "Required write option " k))))
    (is (= b (raw (:xt/id b))))))

(deftest basis-options-becoming-write-options-refuse-preparation
  (let [b (before) _ (put! b)]
    (doseq [k [:snapshot-token :snapshot-time :current-time :await-token]]
      (with-redefs [live/tx-option-keys (conj (set live/tx-option-keys) k)]
        (is (= unsupported (rejection #(prep b))) (str "Leaked query-basis option " k))))
    (is (= b (raw (:xt/id b))))))

(deftest incompatible-version-at-submission-does-not-run-a-transaction
  (let [b (before) _ (put! b) prepared (prep b)]
    (is (= unsupported (in-version "unknown" #(rejection (:submit! prepared)))))
    (assert-preserved! b prepared)))

(deftest missing-query-option-at-submission-consumes-one-shot-permit
  (let [b (before) _ (put! b) prepared (prep b)]
    (with-redefs [live/query-option-keys #{}]
      (is (= unsupported (rejection (:submit! prepared)))))
    (assert-preserved! b prepared)))

(deftest missing-write-option-at-submission-consumes-one-shot-permit
  (let [b (before) _ (put! b) prepared (prep b)]
    (with-redefs [live/tx-option-keys #{}]
      (is (= unsupported (rejection (:submit! prepared)))))
    (assert-preserved! b prepared)))

(deftest forbidden-write-option-at-submission-consumes-one-shot-permit
  (let [b (before) _ (put! b) prepared (prep b)]
    (with-redefs [live/tx-option-keys (conj (set live/tx-option-keys) :snapshot-time)]
      (is (= unsupported (rejection (:submit! prepared)))))
    (assert-preserved! b prepared)))

(deftest vanished-version-at-submission-does-not-leave-a-reusable-capability
  (let [b (before) _ (put! b) prepared (prep b)]
    (is (= unsupported (in-version nil #(rejection (:submit! prepared)))))
    (assert-preserved! b prepared)
    (is (= :pinned-2_2_0-beta1-interface-only
           (:backend-api-check-at-preparation (state! prepared))))))

(deftest runtime-api-version-reader-throwing-at-preparation-refuses-issuance
  (let [b (before) _ (put! b)]
    (is (= :backend-capability-unavailable
           (with-redefs-fn
             {version-var (fn [] (throw (ex-info "manifest access denied" {})))}
             #(rejection (fn [] (prep b))))))
    (is (= b (raw (:xt/id b))))))

(deftest replacing-public-capability-reporter-refuses-preparation
  (let [b (before) _ (put! b)]
    (with-redefs [backend/capability-report!
                  (fn [] {:compatibility :pinned-api-and-gesso-option-surface})]
      (is (= :substituted-correspondence-seam (rejection #(prep b)))))
    (is (= b (raw (:xt/id b))))))

(deftest replacing-public-capability-reporter-consumes-submission-permit
  (let [b (before) _ (put! b) prepared (prep b)]
    (with-redefs [backend/capability-report! (fn [] {:checks {}})]
      (is (= :substituted-correspondence-seam
             (rejection (:submit! prepared)))))
    (assert-preserved! b prepared)))

(deftest transient-backend-incompatibility-cannot-be-retried-after-recovery
  (let [b (before) _ (put! b) prepared (prep b)]
    (is (= unsupported (in-version "future" #(rejection (:submit! prepared)))))
    (is (= :pinned-api-and-gesso-option-surface
           (:compatibility (backend/capability-report!))))
    (assert-preserved! b prepared)))

(deftest backend-incompatibility-after-real-submission-blocks-observation
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))]
    (is (= unsupported
           (in-version "future" #(rejection (fn [] ((:observe! prepared) returned))))))
    (is (= :returned-transaction-key (:status (state! prepared))))
    (let [report ((:observe! prepared) returned)]
      (is (= positive-effect (:transaction-local-write-effect report)))
      (assert-no-proof! report))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))))

(deftest changing-query-option-filter-after-return-blocks-observation
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))]
    (with-redefs [live/query-option-keys #{}]
      (is (= unsupported (rejection (fn [] ((:observe! prepared) returned))))))
    (is (= positive-effect (:transaction-local-write-effect
                           ((:observe! prepared) returned))))
    (is (= 1 (:owned-execute-tx-call-entries (state! prepared))))))

(deftest changing-write-option-filter-after-return-blocks-observation
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))]
    (with-redefs [live/tx-option-keys #{:snapshot-token}]
      (is (= unsupported (rejection (fn [] ((:observe! prepared) returned))))))
    (is (= positive-effect (:transaction-local-write-effect
                           ((:observe! prepared) returned))))))

(deftest runtime-api-reader-error-after-return-blocks-observation
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))]
    (is (= :backend-capability-unavailable
           (with-redefs-fn
             {version-var (fn [] (throw (ex-info "broken manifest" {})))}
             #(rejection (fn [] ((:observe! prepared) returned))))))
    (is (= positive-effect (:transaction-local-write-effect
                           ((:observe! prepared) returned))))))

(deftest substituted-backend-capability-report-blocks-observation
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))]
    (with-redefs [backend/capability-report! (fn [] {:type :fake})]
      (is (= :substituted-correspondence-seam
             (rejection (fn [] ((:observe! prepared) returned))))))
    (is (= positive-effect (:transaction-local-write-effect
                           ((:observe! prepared) returned))))))

(deftest genuine-return-can-be-observed-repeatedly-without-reexecution
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))
        first-report ((:observe! prepared) returned)
        second-report ((:observe! prepared) returned)]
    (is (= positive-effect (:transaction-local-write-effect first-report)))
    (is (= positive-effect (:transaction-local-write-effect second-report)))
    (is (= (:process-local-attempt-id first-report)
           (:process-local-attempt-id second-report)))
    (is (= 1 (:owned-execute-tx-call-entries (state! prepared))))
    (is (= :one-shot-already-attempted (rejection (:submit! prepared))))
    (assert-no-proof! second-report)))

(deftest copied-return-never-becomes-valid-after-backend-recovery
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))
        copy (into {} returned)]
    (is (= unsupported
           (in-version nil #(rejection (fn [] ((:observe! prepared) copy))))))
    (is (= :not-this-gateway-local-return
           (rejection (fn [] ((:observe! prepared) copy)))))
    (is (= positive-effect (:transaction-local-write-effect
                           ((:observe! prepared) returned))))))

(deftest invalid-backend-preflight-produces-no-effect-report-with-fake-return
  (let [b (before) _ (put! b) prepared (prep b)]
    (is (= unsupported (in-version nil #(rejection (:submit! prepared)))))
    (is (= :not-this-gateway-local-return
           (rejection #((:observe! prepared) {}))))
    (assert-preserved! b prepared)))

(deftest backend-error-does-not-leak-source-or-claim-proof
  (let [b (before) _ (put! b) prepared (prep b)]
    (is (= unsupported (in-version "unsupported" #(rejection (:submit! prepared)))))
    (let [summary (state! prepared)]
      (is (= :backend-preflight-rejected (:status summary)))
      (is (= :trusted-bounded-premise (:backend-assert-semantics summary)))
      (is (= :not-authenticated (:transaction-internal-prestate summary)))
      (is (= :not-established (:physical-causality summary)))
      (is (= :not-issued (:certificate summary)))
      (is (= :none (:authority summary)))
      (is (not (.contains (pr-str summary) (:xt/id b))))
      (is (not (.contains (pr-str summary) "private-v1546-secret-marker"))))))

(deftest poststate-divergence-does-not-upgrade-reviewed-interface-to-proof
  (let [b (before) _ (put! b) prepared (prep b)
        returned ((:submit! prepared))
        _ (put! (assoc (after b) :value "independent-writer"))
        report ((:observe! prepared) returned)]
    (is (= positive-effect (:transaction-local-write-effect report)))
    (is (= :bounded-later-poststate-diverges (:status report)))
    (is (= :pinned-2_2_0-beta1-interface-only
           (:backend-api-check-at-observation report)))
    (assert-no-proof! report)))

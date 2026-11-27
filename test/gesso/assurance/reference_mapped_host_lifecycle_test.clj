(ns gesso.assurance.reference-mapped-host-lifecycle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-installed-writer-classification :as installed]
            [gesso.assurance.reference-writer-isolation :as isolation]
            [gesso.assurance.reference-xtdb-provider-contract :as provider-contract]
            [gesso.assurance.reference-xtdb-provider-trust :as provider-trust]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.live.outbox.writer-policy :as policy]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.core :as live]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.claim-race-fixture :refer [mapping]]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.model.inductive-plan :as induction])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- with-entered-refusal [f]
  ;; Pause after entry counting, before genuine mapped admission.
  (let [boundary #'xtdb/invoke-admitted-xtdb-api!
        original @boundary
        ops [[:put-docs :requests {:xt/id "unadmitted-drain-test"}]]
        entered (promise) proceed (promise)
        call (atom nil)
        launch! (fn [node]
                  (reset! call (future (reason #(xtdb/execute-tx! node ops))))
                  (is (= true (deref entered 10000 ::timeout))))]
    (with-redefs-fn {boundary (fn [api operation node entry tx-ops opts]
                                (when (= ops tx-ops)
                                  (deliver entered true)
                                  (when-not (true? (deref proceed 20000 false))
                                    (throw (ex-info "Test drain wait timed out" {}))))
                                (original api operation node entry tx-ops opts))}
      (fn []
        (try (f launch! proceed call ops)
             (finally (deliver proceed true)
                      (when @call (deref @call 20000 ::timeout))))))))

(deftest assembled-host-owns-mapped-activation-and-request-adapter
  (let [root (Files/createTempDirectory "gesso-mapped-host-root-" (make-array FileAttribute 0))
        source-id "assembled-mapped-root"
        source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admission (induction/build-inductive-mapped-admission! source)
        declarations (claim/outcome-declarations)
        fingerprint (:catalog-fingerprint (induction/describe-mapped-admission admission))
        cases (:cases (outcome/derive-cases! source declarations))
        classify! (fn [node a] (installed/derive! node a source declarations fingerprint cases))
        request-row {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0"
                     :request/location-id "l1"}
        member (fn [helper] {:xt/id helper :active true :revision 0 :created-at "t0" :updated-at "t0"})
        seed [[:put-docs :requests request-row] [:put-docs :memberships (member "helper-1") (member "helper-2")]]
        policy-events (atom [])
        contexts (atom [])
        operations {:request/claim (fixture/trusted-operation :request/claim {:published-change-topics #{:request :request-assignment}})}
        surface [:main (fixture/rendered-operation-button (fixture/render-context operations) :request/claim "/operations/request/claim")]
        config {:xtdb-config {:log [:local {:path (str root "/log")}]
                              :storage [:local {:path (str root "/storage")}]}
                :source-id source-id :rules-context {} :live-options {}
                :stop-timeout-ms 500
                :provider-trust {:provider provider-contract/reviewed-provider
                                 :acceptance :trust-open-premises
                                 :premise-ids #{::provider-contract/decoded-assertions ::provider-contract/ordered-transaction-basis
                                                ::provider-contract/put-replacement ::provider-contract/commit-abort
                                                ::provider-contract/worker-clock ::provider-contract/pinned-read
                                                ::provider-contract/retained-history ::provider-contract/valid-time-boundaries}}
                :writer-isolation {:deployment-id :fixture/configured-owner
                                   :database-id :fixture/configured-database
                                   :isolation-boundary :fixture/configured-boundary
                                   :acceptance :trust-open-premises
                                   :premise-ids #{::isolation/database-owner-binding ::isolation/in-process-writer-exclusion
                                                  ::isolation/external-writer-exclusion ::isolation/storage-and-log-writer-exclusion
                                                  ::isolation/lifecycle-isolation}}
                :worker-options {:worker-id "mapped-host-root" :poll-ms 60000}
                :assembled-publication mapping
                :mapped-writer-policy {:admission admission :initializer seed
                                       :observe! #(swap! policy-events conj (:kind %))}}
        handler (fn [ctx]
                  (swap! contexts conj ctx)
                  (is (= [request-row] (xtdb/q-from ctx "SELECT * FROM requests")))
                  (g/html-response surface))]
    (try
      (fixture/with-surfaced-application
        {:operations operations :route-capabilities (fixture/route-capabilities operations)
         :rendered-surfaces {:board surface}}
        (fn [{:keys [assembly report]}]
          (is (some? assembly) (pr-str report))
          (when-not assembly (throw (ex-info "Fixture assembly failed" {})))
          (let [descriptor (:manifest (publication/profile! assembly mapping))
                inspect! (fn [started]
                           (let [node (:xtdb/connectable started)
                                 worker (get started managed/handle-key)
                                 ring-handler (:biff.ring/handler started)
                                 token (xtdb/current-submission-policy-token node)
                                 initial (correspondence/initialization-observation node admission)
                                 {:keys [observation capture]} (publication/capture-framed-submissions!
                                                                admission node #(ring-handler {:uri "/" :xtdb/connectable (Object.)
                                                                                               publication/preparation-key (Object.)}))
                                 ctx (peek @contexts)
                                 adapter (:xtdb/connectable ctx)
                                 observed (publication/read-framed-submission-observation! admission observation)
                                 history (coverage/observe-local-processed-retained-replay-inputs! node)
                                 qualified (classify! node admission)
                                 isolated (isolation/prepare! node admission source declarations fingerprint cases
                                                              {:deployment-id :fixture/owner :database-id :fixture/database
                                                               :isolation-boundary :fixture/deployment})
                                 isolation-handle (:handle isolated)
                                 declaration {:scope (:scope isolated) :acceptance :trust-open-premises
                                              :premises (mapv :assumption (:obligations isolated))}
                                 accepted (isolation/declare! node admission isolation-handle declaration)
                                 combined (host/adopt-implementation-trust! started source declarations)
                                 configured (:writer-isolation combined)
                                 provider (:provider-trust combined)
                                 binding (:original-clm-r2-binding combined)
                                 description (induction/describe-mapped-admission admission)
                                 join! (fn [i p] (#'host/compose-implementation-trust! description (:activation-root combined) i p))]
                             (is (= :conditional-configured-implementation-argument (:status combined)))
                             (is (= (:remaining-conditions binding) (:remaining-conditions combined)))
                             (is (= 39 (count (:remaining-conditions combined))))
                             (is (= (:original-open-obligations description) (:original-open-obligation-ids-by-operation binding)))
                             (is (= {:writer-isolation (:trusted-assumptions configured) :provider-trust (:trusted-assumptions provider)}
                                    (:trusted-assumptions-by-contract combined)))
                             (is (= :not-established (:universal-refinement combined) (:continuous-mediation combined)))
                             (is (= :not-performed (:proof-discharge combined)))
                             (is (= :open (:original-obligation-status combined)))
                             (is (= :related-not-entailing (:condition-link-semantics combined)))
                             (is (= :unissued-combined-trust-host
                                    (reason #(host/adopt-implementation-trust! (assoc started :forged true) source declarations))))
                             (is (= :unissued-combined-trust-host
                                    (reason #(host/adopt-implementation-trust! ctx source declarations))))
                             (with-redefs [host/adopt-provider-trust! (constantly provider)]
                               (is (= :combined-trust-boundary-replaced
                                      (reason #(host/adopt-implementation-trust! started source declarations)))))
                             (doseq [[i p] [[(assoc-in configured [:scope :catalog-fingerprint] "foreign") provider]
                                            [(assoc-in configured [:scope :activation-root :source-id] "foreign") provider]
                                            [(update configured :trusted-assumptions disj (first (:trusted-assumptions configured))) provider]
                                            [configured (assoc-in provider [:scope :remaining-conditions] #{})]
                                            [configured (assoc-in provider [:origin-report :contract :original-inventory :original-binding :original-open-obligation-ids-by-operation] {})]]]
                               (is (= :combined-trust-subject-or-premise-mismatch (reason #(join! i p)))))
                             (is (= :conditional-declared-provider-trust (:status provider)) (pr-str (dissoc provider :origin-report)))
                             (is (= :configured-local-host (:deployment-adoption provider)))
                             (is (= :open-owned-generation-at-adoption (:lifecycle-evidence provider)))
                             (is (= provider-contract/reviewed-provider (get-in provider [:scope :contract-scope :provider])))
                             (is (= 8 (count (:trusted-assumptions provider))))
                             (is (= :not-established (:provider-correspondence provider) (:loaded-provider-identity provider)
                                    (:deployment-isolation provider) (:writer-coverage provider)))
                             (is (= :not-performed (:proof-discharge provider)))
                             (is (= :unissued-provider-trust-host
                                    (reason #(host/adopt-provider-trust! (assoc started :forged true) source declarations))))
                             (is (= :unissued-provider-trust-host
                                    (reason #(host/adopt-provider-trust! ctx source declarations))))
                             (with-redefs [provider-trust/prepare! (constantly {})]
                               (is (= :provider-trust-host-boundary-replaced
                                      (reason #(host/adopt-provider-trust! started source declarations)))))
                             (is (= :conditional-declared-writer-isolation (:status configured)) (pr-str configured))
                             (is (= :configured-local-host (:deployment-adoption configured)))
                             (is (= :fixture/configured-owner (get-in configured [:scope :deployment-id])))
                             (is (= :fixture/configured-database (get-in configured [:scope :database-id])))
                             (is (= :fixture/configured-boundary (get-in configured [:scope :isolation-boundary])))
                             (is (= 5 (count (:trusted-assumptions configured))))
                             (is (= :not-established (:writer-coverage configured) (:continuous-mediation configured)
                                    (:provider-correspondence configured)))
                             (is (= :not-performed (:proof-discharge configured)))
                             (is (= :unissued-writer-isolation-host
                                    (reason #(host/adopt-writer-isolation! (assoc started :forged true) source declarations))))
                             (is (= :unissued-writer-isolation-host
                                    (reason #(host/adopt-writer-isolation! ctx source declarations))))
                             (is (= :unissued-storage-trust-host
                                    (reason #(host/adopt-storage-trust! started nil))))
                             (with-redefs [isolation/prepare! (constantly {})]
                               (is (= :writer-isolation-host-boundary-replaced
                                      (reason #(host/adopt-writer-isolation! started source declarations)))))
                             (is (= :open-isolation-requirements (:status isolated)) (pr-str (dissoc isolated :handle)))
                             (is (= :conditional-declared-writer-isolation (:status accepted)) (pr-str accepted))
                             (is (= 5 (count (:trusted-assumptions accepted))))
                             (is (= :explicit-declaration-only (:premise-acceptance accepted)))
                             (is (= :not-established (:writer-coverage accepted) (:continuous-mediation accepted)
                                    (:provider-correspondence accepted) (:deployment-authentication accepted)))
                             (is (= :not-performed (:proof-discharge accepted)))
                             (is (every? #(= :open (:discharge-state %)) (:obligations accepted)))
                             (is (= :unissued-writer-isolation-scope
                                    (:reason (isolation/declare! (Object.) admission isolation-handle declaration))))
                             (is (= :unissued-writer-isolation-scope
                                    (:reason (isolation/declare! node admission (Object.) declaration))))
                             (is (= :incorrect-isolation-premise-selection
                                    (:reason (isolation/declare! node admission isolation-handle (update declaration :premises pop)))))
                             (is (= :incorrect-isolation-premise-selection
                                    (:reason (isolation/declare! node admission isolation-handle
                                                                (assoc-in declaration [:scope :database-id] :fixture/other)))))
                             (is (= :conditional-current-installed-writer-classification (:status qualified)) (pr-str qualified))
                             (is (= :complete (:declared-alternative-coverage qualified)))
                             (is (= :open (:owned-policy-phase qualified)))
                             (is (= :open-owned-generation-at-qualification (:lifecycle-evidence qualified)))
                             (is (= :not-established (:continuous-mediation qualified)))
                             (is (= :open (:phase (xtdb/owned-submission-policy-state node token))))
                             (with-redefs [xtdb/owned-submission-policy-state (constantly {:phase :open :in-flight 0})]
                               (is (= :installed-classification-boundary-replaced (:reason (classify! node admission)))))
                             (is (= 8 (count (:alternative-identities qualified))))
                             (is (= :not-established (:writer-coverage qualified) (:universal-refinement qualified)))
                             (is (= :not-performed (:proof-discharge qualified)))
                             (is (= (select-keys initial [:source-id :manifest-tx-id :initialization-tx-id :system-time :initialization-fingerprint])
                                    (:activation-root qualified)))
                             (is (= :missing-current-mapped-activation (:reason (classify! node (induction/build-inductive-mapped-admission! source)))))
                             (is (= :not-established (:status (installed/derive! node admission source declarations "foreign" cases))))
                             (with-redefs [policy/activated-policy-root (constantly nil)]
                               (is (= :installed-classification-boundary-replaced (:reason (classify! node admission)))))
                             (is (= 200 (get-in capture [:result :status])))
                             (is (= :matched-local-policy-window (:policy-window capture)))
                             (is (some? token))
                             (is (not (identical? node adapter)))
                             (is (xtdb/same-connectable? node adapter))
                             (is (identical? adapter (:biff.xtdb/node ctx)))
                             (is (not (contains? ctx publication/preparation-key)))
                             (is (identical? (:gesso.live/system started) (:gesso.live/system ctx)))
                             (is (some? worker))
                             (is (= source-id (:source-id (lifecycle/status worker))))
                             (is (= :matched (:manifest-envelope initial) (:initialization-envelope initial)))
                             (is (= initial (get-in observed [:activation-root :observation])))
                             (is (= {:initializer seed :descriptor descriptor} (get-in observed [:activation-root :root])))
                             (is (= [{:xt/id source-id :profile-version 1 :descriptor descriptor}]
                                    (xtdb/q node "SELECT * FROM gesso_publication_manifests")))
                             (is (= :observed (:status history)))
                             (is (= 2 (count (:replay-inputs history))))
                             (is (= :submission-policy-refused
                                    (reason #(xtdb/execute-tx! adapter [[:put-docs :requests (assoc request-row :status :claimed)]]))))
                             (is (= [request-row] (xtdb/q adapter "SELECT * FROM requests")))
                             {:node node :worker worker :live (:gesso.live/system started) :handler ring-handler
                              :configured-scope (:scope configured) :configured-assumptions (:trusted-assumptions configured)
                              :provider-scope (:scope provider) :provider-assumptions (:trusted-assumptions provider)
                              :adapter adapter :token token :isolation-handle isolation-handle :declaration declaration :initial initial :observation observation :history history}))
                run! (fn []
                       (let [started (host/start! assembly handler config)]
                         (try
                           (let [{:keys [node adapter token] :as result} (inspect! started)]
                             (with-entered-refusal
                               (fn [launch! proceed call ops]
                                 (launch! adapter)
                                 (is (= :owned-submission-drain-timeout (reason #(host/stop! started))))
                                 (is (identical? token (xtdb/current-submission-policy-token node)))
                                 (is (= {:phase :fenced :in-flight 1} (xtdb/owned-submission-policy-state node token)))
                                 (is (= :installed-owned-policy-not-open (:reason (classify! node admission))))
                                 (is (= :combined-trust-host-retired
                                        (reason #(host/adopt-implementation-trust! started source declarations))))
                                 (is (= :provider-trust-host-retired
                                        (reason #(host/adopt-provider-trust! started source declarations))))
                                 (is (= :writer-isolation-host-retired
                                        (reason #(host/adopt-writer-isolation! started source declarations))))
                                 (is (= :writer-isolation-generation-not-open
                                        (:reason (isolation/declare! node admission (:isolation-handle result) (:declaration result)))))
                                 (is (= [{:x 1}] (xtdb/q node "SELECT 1 AS x")))
                                 (is (= :owned-submission-fenced (reason #(xtdb/execute-tx! adapter ops))))
                                 (is (= 503 (:status ((:biff.ring/handler started) {:uri "/"}))))
                                 (deliver proceed true)
                                 (is (= :submission-policy-refused (deref @call 20000 ::timeout)))))
                             result)
                           (finally (host/stop! started)))))
                first-run (run!)]
            (is (= [:manifest :initializer] @policy-events))
            (is (= :stopped (:status (lifecycle/status (:worker first-run)))))
            (is (live/closed? (:live first-run)))
            (is (= 503 (:status ((:handler first-run) {:uri "/"}))))
            (is (= :invalid-adapter-connectable (reason #(xtdb/q (:adapter first-run) "SELECT 1"))))
            (is (nil? (correspondence/initialization-observation (:node first-run) admission)))
            (is (= :not-established (:status (classify! (:node first-run) admission))))
            (is (nil? (xtdb/owned-submission-policy-state (:node first-run) (:token first-run))))
            (is (= :writer-isolation-generation-not-open
                   (:reason (isolation/declare! (:node first-run) admission (:isolation-handle first-run) (:declaration first-run)))))
            (with-entered-refusal
              (fn [launch! proceed call ops]
                (let [opened (atom nil)
                      original-register xtdb/register-owned-submission-policy!
                      failure (with-redefs [xtdb/register-owned-submission-policy!
                                            (fn [node admit?]
                                              (reset! opened node)
                                              (original-register node admit?))
                                            live/create (fn [& _]
                                                          (launch! @opened)
                                                          (throw (ex-info "Test startup failure" {})))]
                                (try (host/start! assembly handler config) nil
                                     (catch clojure.lang.ExceptionInfo e (ex-data e))))
                      cleanup (:cleanup/handle failure)]
                  (try
                    (is (= :gesso.live.outbox/checked-startup-failed (:error/type failure)))
                    (is (= :blocked (:rollback/status failure)))
                    (is (= host/xtdb-module-id (:rollback/blocked-module-id failure)))
                    (is (= [{:x 1}] (xtdb/q @opened "SELECT 1 AS x")))
                    (is (= :owned-submission-fenced (reason #(xtdb/execute-tx! @opened ops))))
                    (finally
                      (deliver proceed true)
                      (is (= :submission-policy-refused (deref @call 20000 ::timeout)))
                      (when cleanup
                        (is (= :drained (:status (managed/retry-startup-cleanup! cleanup)))))))
                  (is (nil? (xtdb/current-submission-policy-token @opened)))
                  (is (thrown? Exception (xtdb/q @opened "SELECT 1 AS x"))))))
            (let [second-run (run!)]
              (is (= [:manifest :initializer] @policy-events))
              (is (not (identical? (:token first-run) (:token second-run))))
              (is (not= (:provider-scope first-run) (:provider-scope second-run)))
              (is (= (:provider-assumptions first-run) (:provider-assumptions second-run)))
              (is (not= (:configured-scope first-run) (:configured-scope second-run)))
              (is (not= (:configured-assumptions first-run) (:configured-assumptions second-run)))
              (is (= (:history first-run) (:history second-run)))
              (is (= (:initial first-run) (:initial second-run)))
              (is (= (:initial first-run)
                     (get-in (publication/read-framed-submission-observation! admission (:observation first-run))
                             [:activation-root :observation])))
              (is (= :invalid-adapter-connectable (reason #(xtdb/q (:adapter second-run) "SELECT 1"))))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))


(deftest arbitrary-registered-policy-cannot-supply-mapped-activation
  (let [node (Object.)
        admission (Object.)
        admit? (constantly true)
        release (xtdb/register-submission-policy! node admit?)
        token (xtdb/submission-policy-token node admit?)]
    (try
      (correspondence/record-activation! node admit? token admission
                                         {:source-id "forged"} {:initializer [] :descriptor "forged"})
      (is (some? (correspondence/activation-root node admission token)))
      (is (nil? (policy/activated-policy-root node admission token)))
      (is (= :missing-current-mapped-activation
             (:reason (installed/derive! node admission {} {} "foreign" {}))))
      (finally (release)))))

(ns gesso.assurance.reference-mapped-host-recovery-test
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.xml :as xml]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.reference-mapped-observed-recovery :as observed-recovery]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-retained-acquisition :as acquisition]
            [gesso.assurance.reference-temporal :as temporal]
            [gesso.choreo.identity :as identity]
            [gesso.core :as g]
            [gesso.live.application-preflight.fixture :as fixture]
            [gesso.live.consistency.xtdb :as live-xtdb]
            [gesso.live.optimistic.protocol :as protocol]
            [gesso.live.outbox.biff :as managed]
            [gesso.live.outbox.claim-race-fixture :refer [mapping]]
            [gesso.live.outbox.claim-semantic-fixture :as claim]
            [gesso.live.outbox.host :as host]
            [gesso.live.outbox.history-coverage :as history-coverage]
            [gesso.live.outbox.http-operation :as transport]
            [gesso.live.outbox.lifecycle :as lifecycle]
            [gesso.live.outbox.model-publication :as publication]
            [gesso.live.outbox.physical-correspondence :as correspondence]
            [gesso.live.outbox.read-frontier :as frontier]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command :as command]
            [gesso.model.command-resolution :as resolution]
            [gesso.model.inductive-plan :as induction]
            [gesso.model.mapped-frame :as frame]
            [ring.util.codec :as codec])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- settlement! [response]
  (when-not (= 200 (:status response))
    (throw (ex-info "Expected command settlement" {:status (:status response)})))
  (let [marker (xml/parse (ByteArrayInputStream. (.getBytes ^String (:body response) StandardCharsets/UTF_8)))]
    (protocol/wire->settlement (edn/read-string (get-in marker [:attrs :data-gesso-live-optimistic-settlement])))))

(defn- await-empty-cycle! [worker]
  (let [deadline (+ (System/nanoTime) 10000000000)]
    (loop []
      (let [s (lifecycle/status worker)]
        (cond
          (and (= :idle (:status s)) (= 1 (:cycles s)) (zero? (:total-processed s))) s
          (or (= :failed (:status s)) (> (System/nanoTime) deadline))
          (throw (ex-info "Initial worker cycle did not settle" {:status (:status s) :cycles (:cycles s)}))
          :else (do (Thread/sleep 10) (recur)))))))

(defn- await-maintenance-cycle! [worker]
  (let [deadline (+ (System/nanoTime) 90000000000)]
    (loop []
      (let [s (lifecycle/status worker)]
        (cond
          (and (= :idle (:status s)) (<= 2 (:cycles s))) s
          (or (= :failed (:status s)) (> (System/nanoTime) deadline))
          (throw (ex-info "Worker maintenance cycle did not settle" {:status (:status s) :cycles (:cycles s)}))
          :else (do (Thread/sleep 50) (recur)))))))

(defn- selected-rows [node tables]
  (into {} (map (fn [table]
                  [table (set (live-xtdb/q node (str "SELECT *, _system_from FROM " (name table))))])) tables))

(defn- check-reopened-recovery!
  [assembly handler config tables admit mapped-source outcome-declarations
   policy-events {:keys [node worker ring-handler token sample rows observation steps initial comparison events retained-handle]}]
  (is (= :stopped (:status (lifecycle/status worker))))
  (is (= 503 (:status (ring-handler {:uri "/"}))))
  (is (nil? (correspondence/initialization-observation node admit)))
  (let [started (host/start! assembly handler config)
        reopened (:xtdb/connectable started)
        new-worker (get started managed/handle-key)]
    (try
      (is (not (identical? node reopened)))
      (let [new-token (live-xtdb/current-submission-policy-token reopened)]
        (is (some? new-token))
        (is (not (identical? token new-token))))
      (is (= :idle (:status (await-empty-cycle! new-worker))))
      (is (= events @policy-events))
      (is (= initial (correspondence/initialization-observation reopened admit)))
      (is (= sample (coverage/observe-local-processed-retained-replay-inputs! reopened)))
      (is (= rows (selected-rows reopened tables)))
      (let [r (observed-recovery/compare-activated-worker-interleaved!
               reopened tables admit mapped-source outcome-declarations steps observation)
            fresh (publication/capture-framed-submissions! admit reopened (constantly :read-only))
            bad (observed-recovery/compare-activated-worker-interleaved!
                 reopened tables admit mapped-source outcome-declarations steps (:observation fresh))]
        (is (= :matched (:status r)) (pr-str r))
        (is (= comparison r))
        ;; Content equality across reopening (or a byte copy) does not
        ;; transfer either locally issued capability.
        (let [new-handle (:acquisition (acquisition/capture! reopened))
              local! #(observed-recovery/compare-local-activated!
                       reopened tables admit mapped-source outcome-declarations steps %1 %2 true)]
          (is (some? new-handle))
          (is (nil? (acquisition/read-acquisition! reopened retained-handle)))
          (doseq [[obs handle reason] [[observation retained-handle :submission-origin-mismatch]
                                       [observation new-handle :submission-origin-mismatch]
                                       [(:observation fresh) retained-handle :unissued-retained-acquisition]]]
            (let [local (local! obs handle)]
              (is (= :unsupported (:status local)))
              (is (= reason (:reason local)))
              (is (not (contains? local :local-acquisition-binding))))))
        (is (= :not-established (:observation-storage-identity r)))
        (is (= :not-established (:worker-observation-provenance r)))
        (is (= :matched-local-policy-window (get-in fresh [:capture :policy-window])))
        (is (= initial (get-in (publication/read-framed-submission-observation! admit (:observation fresh))
                               [:activation-root :observation])))
        (is (not= :matched (:status bad)))
        (is (not-any? #(contains? bad %) [:activation-root-binding :submission-recovery-linkage :intervening-maintenance])))
      (is (= sample (coverage/observe-local-processed-retained-replay-inputs! reopened)))
      (is (= rows (selected-rows reopened tables)))
      (is (= events @policy-events))
      (finally (host/stop! started)))))

(defn- copy-closed-store! [root copy-root]
  ;; Copy bytes, not links: the two hosts must not share physical files.
  (let [source (.toFile root)
        files (vec (rest (file-seq source)))]
    (is (not (Files/isSameFile root copy-root)))
    (is (some #(.isFile %) files))
    (doseq [f files
            :let [path (.toPath f)
                  target (.resolve copy-root (.relativize root path))]]
      (when (Files/isSymbolicLink path)
        (throw (ex-info "Unexpected symlink in closed store fixture" {})))
      (if (.isDirectory f)
        (Files/createDirectories target (make-array FileAttribute 0))
        (do (io/make-parents (.toFile target))
            (io/copy f (.toFile target)))))
    (is (every? (fn [f]
                  (not (Files/isSameFile (.toPath f)
                                         (.resolve copy-root (.relativize root (.toPath f))))))
                files))))

(defn- check-substituted-store!
  [root assembly handler config tables admit mapped-source outcome-declarations policy-events original]
  (let [copy-root (Files/createTempDirectory "gesso-claim-recovery-copy-" (make-array FileAttribute 0))]
    (try
      (copy-closed-store! root copy-root)
      (let [copy-config (assoc config :xtdb-config
                               {:log [:local {:path (str copy-root "/log")}]
                                :storage [:local {:path (str copy-root "/storage")}]})]
        (is (not= (:xtdb-config config) (:xtdb-config copy-config)))
        ;; The exact historical comparison also succeeds for an independent
        ;; copy. It cannot identify which physical store supplied the bytes.
        (check-reopened-recovery! assembly handler copy-config tables admit mapped-source
                                  outcome-declarations policy-events original))
      (finally (doseq [f (reverse (file-seq (.toFile copy-root)))] (io/delete-file f true))))))

(defn- check-assembled-recovery! [interleaved?]
  (let [root (Files/createTempDirectory "gesso-claim-recovery-" (make-array FileAttribute 0))
        config {:log [:local {:path (str root "/log")}] :storage [:local {:path (str root "/storage")}]}
        source-id "assembled-recovery"
        before {:xt/id "r1" :status :open :revision 0 :created-at "t0" :updated-at "t0" :request/location-id "l1"}
        after (assoc before :status :claimed :revision 1 :updated-at "t1")
        member (fn [helper] {:xt/id helper :active true :revision 0 :created-at "t0" :updated-at "t0"})
        assignment (fn [helper] {:xt/id "primary-r1" :request-id "r1" :helper helper :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"})
        version {:revision-key :revision :created-at-key :created-at :updated-at-key :updated-at}
        binding (fn [helper id] {:command-id (identity/command-id id) :principal (identity/principal helper)
                                 :operation :request/claim :arguments {:request-id "r1"}})
        winner (binding "helper-1" "claim-1") loser (binding "helper-2" "claim-2")
        accepted (fn [helper] {:outcome :request/claimed :helper helper})
        refused {:outcome :request/not-claimable :reason :already-claimed}
        seed [[:put-docs :requests before] [:put-docs :memberships (member "helper-1") (member "helper-2")]]
        tables #{:gesso_initializations :gesso_publication_manifests :requests :request_assignments :memberships :gesso_commands :gesso_transition_evidence :gesso_live_outbox}
        writes #{:requests :request_assignments :gesso_commands}
        targets #{[:requests "r1"] [:request_assignments "primary-r1"]}
        updates #{[:requests "r1" :status :open] [:requests "r1" :revision 0] [:requests "r1" :updated-at "t0"]}
        operations #{[:requests "r1" :claim] [:request_assignments "primary-r1" :create]}
        refusal-guards #{[:requests "r1" :status :claimed] [:requests "r1" :revision 1] [:requests "r1" :updated-at "t1"]}
        snapshots (atom {}) recovered (atom 0)
        handler* (atom nil) winner-response (atom nil)
        worker* (atom nil) gap* (atom nil) restart* (atom nil)
        request! (fn [b allowed?]
                   (@handler* {:request-method :post :uri "/operations/request/claim"
                               :headers {"content-type" "application/x-www-form-urlencoded"
                                         "fixture-principal" (identity/raw-value (:principal b))
                                         "fixture-gate" (if allowed? "yes" "no")}
                               :body (codec/form-encode
                                      {transport/command-parameter
                                       (pr-str (protocol/command->wire
                                                (protocol/command (assoc (dissoc b :principal)
                                                                   :execution-id (identity/execution-id (str "e-" (resolution/record-id b)))
                                                                   :observed-basis fixture/rendered-basis))))})}))
        policy-events (atom [])
        actor #(identity/raw-value (:principal %))
        mapped-source (claim/physical-mapped-admission-source (claim/bound-inductive-source!))
        admit (induction/build-inductive-mapped-admission! mapped-source)
        catalog (induction/describe-mapped-admission admit)
        prepare! (fn [invocation branch]
                   (let [snapshot (claim/snapshot-at-frontier! (:ctx invocation))
                         id (claim/alternative-id (actor invocation) branch)]
                     (admit :prepare! id (select-keys invocation [:command-id :principal :operation :arguments]) snapshot {})
                     (swap! snapshots assoc id snapshot)))
        refusal-contract {:construction-contract {:required-guard-tables #{:requests} :allowed-guard-tables #{:requests}
                                                  :required-write-tables #{:gesso_commands} :allowed-write-tables #{:gesso_commands}}
                          :target-contract! (fn [& _] {:required-guard-targets #{[:requests "r1"]} :allowed-guard-targets #{[:requests "r1"]}
                                                       :required-write-targets #{} :allowed-write-targets #{}})
                          :predicate-contract! (fn [& _] {:required-guard-checks refusal-guards :allowed-guard-checks refusal-guards
                                                          :required-update-checks #{} :allowed-update-checks #{}})
                          :command-operation-contract! (fn [& _] {:required-command-operations #{} :allowed-command-operations #{}})
                          :post-state-contract! (fn [& _] {:required-post-state-values #{} :allowed-post-state-values #{}})
                          :outcome-contract! (fn [& _] {:expected-outcome refused})}
        opts {:operation :request/claim :submission-mode! (constantly :submit) :mapped-admission admit
              :single-command-per-principal? true :may-disclose? (constantly true)
              :construction-contract {:required-guard-tables #{:memberships} :allowed-guard-tables #{:memberships}
                                      :required-write-tables writes :allowed-write-tables writes}
              :target-contract! (fn [_ b] (let [g #{[:memberships (actor b)]}]
                                            {:required-guard-targets g :allowed-guard-targets g :required-write-targets targets :allowed-write-targets targets}))
              :predicate-contract! (fn [_ b] (let [h (actor b) g #{[:memberships h :active true] [:memberships h :revision 0] [:memberships h :updated-at "t0"]}]
                                               {:required-guard-checks g :allowed-guard-checks g :required-update-checks updates :allowed-update-checks updates}))
              :command-operation-contract! (fn [& _] {:required-command-operations operations :allowed-command-operations operations})
              :post-state-contract! (fn [_ b] (let [v (into #{} (mapcat (fn [[table row]] (map (fn [[k v]] [table (:xt/id row) k v]) row)))
                                                               [[:requests after] [:request_assignments (assignment (actor b))]])]
                                                {:required-post-state-values v :allowed-post-state-values v}))
              :outcome-contract! (fn [_ b] {:expected-outcome (accepted (actor b))})
              :project! (fn [{:keys [ctx]} result]
                          (let [{:keys [basis rows]} (frontier/q-with-basis! ctx ["SELECT r.status, a.helper FROM requests r JOIN request_assignments a ON r._id = a.request_id WHERE r._id = ? AND a._id = ?" "r1" "primary-r1"])]
                            {:resolution (if (= refused (:outcome result)) :rejected :confirmed)
                             :outcome (get-in result [:outcome :outcome])
                             :authoritative (protocol/authoritative {:presence :present :basis basis :projection (first rows)})}))}
        construct (fn [invocation]
                    (let [helper (actor invocation)]
                      (prepare! invocation :accept)
                      {:spec {:purpose :gesso.live/refresh}
                       :model-plan {:guards [(command/guard :memberships (member helper) (assoc version :compare-keys [:revision :updated-at :active]))]
                                    :commands [(command/update-command :requests :claim before after (assoc version :compare-keys [:revision :updated-at :status]))
                                               (command/create :request_assignments (assignment helper) version)]}
                       :outcome (accepted helper) :now "t1"}))
        outcome-declarations (into {} (for [helper (keys claim/helpers) branch [:accept :refuse]]
                                         [(claim/alternative-id helper branch)
                                          {:arguments {:request-id "r1"}
                                           :outcome (if (= :accept branch) (accepted helper) refused)}]))
        cases {["helper-1" :request/claim {:request-id "r1"} (accepted "helper-1")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} (accepted "helper-2")] :accepted
               ["helper-2" :request/claim {:request-id "r1"} refused] :rejected
               ["helper-1" :request/claim {:request-id "r1"} refused] :rejected}
        recovery-steps (fn [history]
                         (mapv (fn [[idx b branch outcome now]]
                                 (let [id (claim/alternative-id (actor b) branch)
                                       source (claim/source (actor b) branch)
                                       {:keys [document-rows records]} (get @snapshots id)
                                       journal (vec (vals records))
                                       prefix-count (count (frame/formatted-prestate-assertions!
                                                            (plan/check-target-mapping! source claim/target-map)
                                                            document-rows journal))]
                                   {:original source :targets claim/target-map :docs document-rows :journal journal
                                    :expectation (nth history idx) :prefix-count prefix-count
                                    :spec {:alternative id :inputs {} :effects (get claim/document-effects id)
                                           :binding b :outcome outcome :now now :outcome-cases cases}
                                    :context (assoc (select-keys catalog [:catalog-fingerprint :induction-fingerprint])
                                                    :selectors (get-in catalog [:input-derivations id]))
                                    :source-id source-id :declarations (:publication-derivations catalog)
                                    :residual-declarations (:residual-assertion-derivations catalog)}))
                               [[2 winner :accept (accepted "helper-1") "t1"]
                                [(if interleaved? 7 3) loser :accept (accepted "helper-2") "t1"]
                                [(if interleaved? 8 4) loser :refuse refused "t2"]]))
        executor (publication/build-executor
                  (assoc opts :abort-contract refusal-contract
                         :construct! (fn [invocation]
                                       (let [candidate (construct invocation)]
                                         (when (= "helper-2" (actor invocation))
                                           (reset! winner-response (request! winner true))
                                           (when interleaved?
                                             (let [ctx (:ctx invocation)
                                                   protected (disj tables :gesso_live_outbox)
                                                   prior (selected-rows (:xtdb/connectable ctx) protected)
                                                   pending (live-xtdb/q-from ctx "SELECT * FROM gesso_live_outbox")
                                                   done (await-maintenance-cycle! @worker*)]
                                               (reset! gap* {:before prior
                                                             :after (selected-rows (:xtdb/connectable ctx) protected)
                                                             :pending pending :done done}))))
                                         candidate))
                         :construct-after-abort! (fn [invocation]
                                                   (swap! recovered inc)
                                                   (prepare! invocation :refuse)
                                                   {:spec nil :model-plan {:guards [(command/guard :requests after (assoc version :compare-keys [:revision :updated-at :status]))]}
                                                    :outcome refused :now "t2"})))
        operation (fixture/trusted-operation :request/claim {:published-change-topics #{:request :request-assignment} :execute! executor})
        assembled-operations {:request/claim operation}
        surface [:main (fixture/rendered-operation-button (fixture/render-context assembled-operations) :request/claim "/operations/request/claim")]
        principal-fn (fn [ctx] (let [h (get-in ctx [:headers "fixture-principal"])]
                                 (when (#{"helper-1" "helper-2"} h) (identity/principal h))))
        host-config {:xtdb-config config :source-id source-id :rules-context {} :live-options {}
                     :worker-options {:worker-id "assembled-recovery" :poll-ms 60000}
                     :assembled-operations {:authorize-request? #(and (= "yes" (get-in % [:headers "fixture-gate"])) (some? (principal-fn %)))}
                     :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request} :request_assignments #{:request-assignment}}
                                                   :require-derived-model? true :operation-model-entities {:request/claim #{:requests :request_assignments}})
                     :mapped-writer-policy {:admission admit :initializer seed :observe! #(swap! policy-events conj (:kind %))}}]
    (try
      (fixture/with-surfaced-application
        {:operations assembled-operations :route-capabilities (fixture/route-capabilities assembled-operations)
         :rendered-surfaces {:board surface} :principal-fn principal-fn}
        (fn [{:keys [assembly report]}]
          (when-not assembly (throw (ex-info "Fixture assembly failed" {:report report})))
          (let [started (host/start! assembly (fn [_] (g/html-response surface)) host-config)
                node (:xtdb/connectable started)
                worker (get started managed/handle-key)]
            (try
              (reset! handler* (:biff.ring/handler started))
              (reset! worker* worker)
              (is (= :idle (:status (await-empty-cycle! worker))))
              (is (= 403 (:status (request! loser false))))
              (is (empty? @snapshots))
              (let [{:keys [observation capture]} (publication/capture-framed-submissions! admit node #(request! loser true))
                    winning (settlement! @winner-response)
                    losing (settlement! (:result capture))
                    sample (coverage/observe-local-processed-retained-replay-inputs! node)
                    history (:replay-inputs sample)]
                (is (= :returned (:outcome capture)))
                (is (= :matched-local-policy-window (:policy-window capture)))
                (is (= :confirmed (:resolution winning)))
                (is (= :rejected (:resolution losing)))
                (is (= (:command-id winner) (:command-id winning)))
                (is (= (:command-id loser) (:command-id losing)))
                (is (= :request/claimed (:outcome winning)))
                (is (= :request/not-claimable (:outcome losing)))
                (is (= 1 @recovered))
                (is (= (if interleaved?
                         [:manifest :initializer :mapped-operation :worker :worker :worker :worker :mapped-operation :mapped-operation]
                         [:manifest :initializer :mapped-operation :mapped-operation :mapped-operation]) @policy-events))
                (is (= [after] (live-xtdb/q node "SELECT * FROM requests")))
                (is (= [(assignment "helper-1")] (live-xtdb/q node "SELECT * FROM request_assignments")))
                (is (= #{(resolution/command-record winner (accepted "helper-1") "t1") (resolution/command-record loser refused "t2")}
                       (set (live-xtdb/q node "SELECT * FROM gesso_commands"))))
                (is (= :observed (:status sample)))
                (is (= (if interleaved? 9 5) (count history)))
                (let [steps (recovery-steps history)
                      handle (:acquisition (acquisition/capture! node))
                      local! #(observed-recovery/compare-local-activated!
                               node tables admit mapped-source outcome-declarations steps observation % interleaved?)
                      local (local! handle)
                      bad (local! sample)
                      r ((if interleaved? observed-recovery/compare-activated-worker-interleaved!
                             observed-recovery/compare-activated!) node tables admit mapped-source outcome-declarations steps observation)]
                  (is (= :matched (:status r)) (pr-str r))
                  (is (= sample (acquisition/read-acquisition! node handle)))
                  (is (= :matched (:status local)) (pr-str local))
                  (is (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding local)))
                  (is (= :not-established (:observation-storage-identity local) (:authorized-historical-lineage local)))
                  (is (= :none (:authority local)))
                  (is (= :not-issued (:certificate local)))
                  (is (= r (dissoc local :local-acquisition-profile :local-acquisition-binding :authorized-historical-lineage)))
                  (is (= :unissued-retained-acquisition (:reason bad)))
                  (is (not (contains? bad :local-acquisition-binding)))
                  (is (= :matched-local-activated-retained-root (:activation-root-binding r)))
                  (is (= :matched-local-observed-retained-steps (:submission-recovery-linkage r)))
                  (is (= [:committed :aborted :committed] (:selected-commitments r)))
                  (is (= :not-established (:submission-provenance r)))
                  (is (= :not-established (:aborted-receipt-identity r)))
                  (is (= :none (:authority r)))
                  (is (= :not-issued (:certificate r)))
                  (when interleaved?
                    (let [{:keys [before after pending done]} @gap*
                          ids (set (map :xt/id pending))
                          maintenance (:intervening-maintenance r)
                          audit (history-coverage/audit! node admit 16)
                          prediction (temporal/replay (mapv #(dissoc % :tx-id) history))
                          terminal (live-xtdb/q node "SELECT * FROM gesso_live_outbox")
                          adjacent (observed-recovery/compare-activated! node tables admit mapped-source outcome-declarations steps observation)]
                      (is (= observed-recovery/activated-interleaved-profile (:activated-recovery-profile r)))
                      (is (= 9 (:replayed-transaction-count r)))
                      (is (= (mapv :tx-id (map #(nth history %) [2 7 8])) (:selected-tx-ids r)))
                      (is (= (mapv :tx-id (subvec history 3 7)) (mapv :tx-id maintenance)))
                      (is (= [:lease-acquisition :dispatch-acknowledgment :lease-acquisition :dispatch-acknowledgment]
                             (mapv :maintenance maintenance)))
                      (is (= 2 (count pending) (count ids)))
                      (is (every? #(= :pending (:gesso.live.outbox/status %)) pending))
                      (is (= ids (set (map :intent-id maintenance)) (set (map :xt/id terminal))))
                      (is (every? #(apply = (map :intent-id %)) (partition 2 maintenance)))
                      (is (every? #(= :dispatch-accepted (:gesso.live.outbox/status %)) terminal))
                      (is (= before after))
                      (is (= 2 (:cycles done) (:total-processed done) (:total-dispatch-accepted done)))
                      (is (= :predicted (:status prediction)))
                      (is (= [true true true true true true true false true] (mapv :committed? (:history prediction))))
                      (is (= :observed-interval-checked (:status audit)))
                      (is (= [:verified-mapped-transition :verified-infrastructure-transition :verified-infrastructure-transition
                              :verified-infrastructure-transition :verified-infrastructure-transition :aborted-stutter :verified-mapped-transition]
                             (mapv :classification (:steps audit))))
                      (is (= :not-established (:worker-observation-provenance r)))
                      (is (= :nonadjacent-recovery-selection (:reason adjacent)))
                      (is (not (contains? adjacent :activation-root-binding)))
                      (is (= sample (coverage/observe-local-processed-retained-replay-inputs! node)))
                      (is (= 2 (:cycles (lifecycle/status worker))))
                      (is (= :idle (:status (lifecycle/status worker))))
                      (reset! restart* {:node node :worker worker :ring-handler @handler*
                                        :token (live-xtdb/current-submission-policy-token node)
                                        :sample sample :rows (selected-rows node tables)
                                        :observation observation :steps steps :comparison r :retained-handle handle
                                        :initial (correspondence/initialization-observation node admit)
                                        :events @policy-events}))))
                ;; This test explicitly exercises a quiescent interval between
                ;; real worker cycles, not arbitrary worker-interleaved history.
                (when-not interleaved?
                  (is (= 1 (:cycles (lifecycle/status worker))))
                  (is (= :idle (:status (lifecycle/status worker))))
                  (let [protected (disj tables :gesso_live_outbox)
                      prior (selected-rows node protected)
                      pending (live-xtdb/q node "SELECT * FROM gesso_live_outbox")
                      ids (set (map :xt/id pending))
                      _ (is (= 2 (count pending) (count ids)))
                      _ (is (every? #(= :pending (:gesso.live.outbox/status %)) pending))
                      completed (await-maintenance-cycle! worker)
                      sample2 (coverage/observe-local-processed-retained-replay-inputs! node)
                      full (:replay-inputs sample2)
                      audit (history-coverage/audit! node admit 16)
                      maintenance (vec (drop 3 (:steps audit)))
                      prediction (temporal/replay (mapv #(dissoc % :tx-id) full))
                      r (observed-recovery/compare-activated! node tables admit mapped-source outcome-declarations
                                                            (recovery-steps history) observation)
                      terminal (live-xtdb/q node "SELECT * FROM gesso_live_outbox")]
                  (is (= 2 (:cycles completed)))
                  (is (= 2 (:total-processed completed) (:total-dispatch-accepted completed)))
                  (is (= :observed (:status sample2)))
                  (is (= 9 (count full)))
                  (is (= history (vec (take 5 full))))
                  (is (= :predicted (:status prediction)))
                  (is (= [true true true false true true true true true]
                         (mapv :committed? (:history prediction))))
                  (is (= :observed-interval-checked (:status audit)) (pr-str (select-keys audit [:status :steps])))
                  (is (= [:verified-mapped-transition :aborted-stutter :verified-mapped-transition]
                         (mapv :classification (take 3 (:steps audit)))))
                  (is (= 4 (count maintenance)))
                  (is (= (mapv :tx-id (drop 5 full)) (mapv :tx-id maintenance)))
                  (is (every? #(and (= :verified-infrastructure-transition (:classification %))
                                    (= :matched (:worker-envelope %)) (true? (:committed? %))) maintenance))
                  (is (= [:lease-acquisition :dispatch-acknowledgment :lease-acquisition :dispatch-acknowledgment]
                         (mapv :maintenance maintenance)))
                  (is (= ids (set (map :intent-id maintenance))))
                  (is (every? #(apply = (map :intent-id %)) (partition 2 maintenance)))
                  (is (= prior (selected-rows node protected)))
                  (is (= ids (set (map :xt/id terminal))))
                  (is (every? #(= :dispatch-accepted (:gesso.live.outbox/status %)) terminal))
                  (is (= [:manifest :initializer :mapped-operation :mapped-operation :mapped-operation
                          :worker :worker :worker :worker] @policy-events))
                  (is (= :matched (:status r)) (pr-str r))
                  (is (= :matched-local-activated-retained-root (:activation-root-binding r)))
                  (is (= :matched-local-observed-retained-steps (:submission-recovery-linkage r)))
                  (is (= :not-established (:provider-refinement r)))
                  (is (= sample2 (coverage/observe-local-processed-retained-replay-inputs! node))))))
              (finally (host/stop! started)))
            (when interleaved?
              (check-reopened-recovery! assembly (fn [_] (g/html-response surface)) host-config
                                        tables admit mapped-source outcome-declarations policy-events @restart*)
              (check-substituted-store! root assembly (fn [_] (g/html-response surface)) host-config
                                        tables admit mapped-source outcome-declarations policy-events @restart*)))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest assembled-command-transport-observed-recovery
  (check-assembled-recovery! false))

(deftest assembled-command-transport-worker-interleaved-recovery
  (check-assembled-recovery! true))

(ns gesso.assurance.reference-mapped-host-recovery-test
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.xml :as xml]
            [clojure.test :refer [deftest is]]
            [gesso.assurance.assumption :as assumption]
            [gesso.assurance.reference-mapped-observed-recovery :as observed-recovery]
            [gesso.assurance.reference-installed-writer-history :as writer-history]
            [gesso.assurance.reference-recovery-inventory :as inventory]
            [gesso.assurance.reference-recovery-invariant :as invariant]
            [gesso.assurance.reference-recovery-invariant-test :as invariant-test]
            [gesso.assurance.reference-mapped-worker-submission :as worker-submission]
            [gesso.assurance.reference-mapped-worker-envelope :as worker-envelope]
            [gesso.assurance.reference-source-coverage :as coverage]
            [gesso.assurance.reference-historical-cut :as cut]
            [gesso.assurance.reference-storage-identity :as storage]
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

(defn- trust-problem [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- expected-recovery-subject [admit]
  (let [description (induction/describe-mapped-admission admit)
        abstract (induction/recheck-mapped-induction! admit)]
    (assoc (select-keys description [:catalog-fingerprint :induction-fingerprint])
           :abstract-induction-fingerprint (:fingerprint abstract)
           :abstract-induction-identity (:identity abstract)
           :catalog-id (:id (claim/catalog))
           :selected-operations
           (mapv (fn [[helper branch]]
                   (let [id (claim/alternative-id helper branch)
                         mapping (plan/check-target-mapping! (claim/source helper branch) claim/target-map)]
                     {:alternative-id id :command-operation :request/claim
                      :subject-fingerprint (:subject-fingerprint mapping)
                      :transition-fingerprint (:transition-fingerprint mapping)
                      :mapping-fingerprint (:fingerprint mapping)}))
                 [["helper-1" :accept] ["helper-2" :accept] ["helper-2" :refuse]]))))

(defn- check-recovery-judgment! [result scope original admit]
  (invariant-test/check-judgment! result scope original)
  (is (= observed-recovery/judgment-profile (:judgment-profile result)))
  (is (= scope (:judgment-scope result)))
  (is (= (expected-recovery-subject admit) (:judgment-subject result)))
  (let [abstract (:abstract-induction result)
        original (induction/recheck-mapped-induction! admit)]
    (is (= :matched-original-issued-induction (:abstract-induction-binding result)))
    (is (= original abstract))
    (is (= :source-bound-conditional-abstract-safety (:verdict abstract)))
    (is (= :closed-declared-abstract-machine-only (:scope abstract)))
    (is (= :not-performed (:assumption-discharge abstract)))
    (is (= :all-original-obligations-remain-open
           (get-in abstract [:classification :original-obligations :status])))
    (is (= (:original-open-obligations (induction/describe-mapped-admission admit))
           (get-in abstract [:classification :original-obligations :by-operation])))
    (is (seq (get-in abstract [:classification :open :assumptions])))
    (is (= (get-in abstract [:classification :certificate :open-premises])
           (get-in abstract [:classification :open :certificate-premises])))
    (is (= :not-established (:implementation-correspondence abstract)
           (:production-certification abstract)))
    (is (= :none (:authority abstract) (:transaction-authority abstract))))
  (let [trajectory (:original-recovery result)]
    (is (= :matched-issued-origin-and-retained-sample (:local-acquisition-binding trajectory)))
    (is (= :matched-local-activated-retained-root (:activation-root-binding trajectory)))
    (is (= :matched-local-observed-retained-steps (:submission-recovery-linkage trajectory)))
    (is (= [:committed :aborted :committed] (:selected-commitments trajectory)))
    (is (= (:selected-tx-ids original) (:selected-tx-ids trajectory)))
    (is (= :matched-issued-command-bindings (:command-origin-correspondence trajectory)))
    (is (= (:selected-command-origins original) (:selected-command-origins trajectory)))
    (is (= :same-observed-execution (:abort-recovery-execution trajectory)))
    (is (= :not-established (:command-authentication trajectory)))
    (is (= (:activated-recovery-profile original) (:activated-recovery-profile trajectory)))
    (is (= (:manifest-tx-id original) (:manifest-tx-id trajectory)))
    (is (= (:initialization-tx-id original) (:initialization-tx-id trajectory)))
    (is (= :not-established (:submission-provenance trajectory) (:provider-refinement trajectory)))
    (is (= (:worker-observation-provenance original) (:worker-observation-provenance trajectory)))
    (is (= (:worker-correspondence original) (:worker-correspondence trajectory)))
    (when-let [worker (:worker-correspondence trajectory)]
      (is (= :matched-local-issued-host-capture (:worker-observation-provenance worker)))
      (is (= :matched-issued-worker-retained-transactions (:maintenance-correspondence worker)))
      (is (= (:selected-tx-ids trajectory) (:selected-tx-ids worker)))
      (is (= (get-in scope [:historical-cut :payload-fingerprint]) (:retained-payload-fingerprint worker)))
      (is (= :not-established (:worker-authentication worker) (:dispatch-authentication worker)
             (:provider-refinement worker) (:complete-writer-coverage worker)))
      (is (= :none (:authority worker)))
      (is (= :not-issued (:certificate worker)))))
  (let [command (:command-authentication-premises result)
        original (:original-recovery result)
        interface (:scope command)
        [principal attribution] (:obligations command)]
    (is (= observed-recovery/command-authentication-profile (:profile command)))
    (is (= (:judgment-subject result) (:judgment-subject interface)))
    (is (= (:historical-cut scope) (:historical-cut interface)))
    (is (= (:publication-source-id scope) (:publication-source-id interface)))
    (is (= (:selected-tx-ids original) (:selected-tx-ids interface)))
    (is (= (:selected-commitments original) (:selected-commitments interface)))
    (is (= :matched-issued-command-bindings (:origin-correspondence interface)))
    (is (= (:selected-command-origins original) (:selected-command-origins command)))
    (is (= (:abort-recovery-execution original) (:abort-recovery-execution command)))
    (is (string? (:command-observation-ref interface)))
    (is (= [:reference-command/principal-resolution :reference-command/command-attribution]
           (mapv #(get-in % [:assumption :id]) (:obligations command))))
    (is (= #{} (get-in principal [:assumption :dependencies])))
    (is (= #{:reference-command/principal-resolution} (get-in attribution [:assumption :dependencies])))
    (is (every? #(= :open (:discharge-state %)) (:obligations command)))
    (is (every? #(= interface (get-in % [:assumption :subject :interface])) (:obligations command)))
    (is (= (:open-premise-fingerprints command) (set (map :premise-fingerprint (:obligations command)))))
    (is (not-any? (:trusted-assumptions result) (:open-premise-fingerprints command)))
    (is (= :not-performed (:premise-acceptance command)))
    (is (= :not-established (:command-authentication command)))
    (is (= :none (:authority command)))
    (is (= :not-issued (:certificate command))))
  (let [worker (:worker-authentication-premises result)
        interface (:scope worker)
        selected (:intervening-maintenance original)
        observed (get-in original [:worker-correspondence :selected-maintenance])
        command (:command-authentication-premises result)
        [identity attribution dispatch] (:obligations worker)]
    (is (= observed-recovery/worker-authentication-profile (:profile worker)))
    (is (= (:judgment-subject result) (:judgment-subject interface)))
    (is (= (:historical-cut scope) (:historical-cut interface)))
    (is (= (:publication-source-id scope) (:publication-source-id interface)))
    (is (= (get-in command [:scope :command-observation-ref]) (:recovery-cut-ref interface)))
    (is (= (:selected-tx-ids original) (:selected-tx-ids interface)))
    (is (= (:selected-commitments original) (:selected-commitments interface)))
    (is (= (mapv #(update % :intent-id str) selected) (:selected-maintenance interface)))
    (is (= (filterv #(= :dispatch-acknowledgment (:maintenance %)) (:selected-maintenance interface))
           (:selected-acknowledgments interface)))
    (is (= (mapv :tx-id observed) (mapv :tx-id (:observed-maintenance interface))))
    (is (= (mapv #(str (get-in % [:worker-origin :lifecycle-ref])) observed)
           (mapv #(get-in % [:worker-origin :lifecycle-ref]) (:observed-maintenance interface))))
    (is (= (mapv #(str (:system-time %)) observed) (mapv :system-time (:observed-maintenance interface))))
    (is (= (or (:worker-observation-provenance original) :not-established)
           (:worker-observation-provenance interface)))
    (is (= (:open-premise-fingerprints worker) (set (map :premise-fingerprint (:obligations worker)))))
    (is (not-any? (:trusted-assumptions result) (:open-premise-fingerprints worker)))
    (is (not-any? (:open-premise-fingerprints command) (:open-premise-fingerprints worker)))
    (is (every? #(= :open (:discharge-state %)) (:obligations worker)))
    (is (every? #(= interface (get-in % [:assumption :subject :interface])) (:obligations worker)))
    (is (= :not-performed (:premise-acceptance worker)))
    (is (= :none (:authority worker)))
    (is (= :not-issued (:certificate worker)))
    (if (seq selected)
      (do (is (= [:reference-worker/worker-identity :reference-worker/maintenance-attribution
                  :reference-worker/dispatch-acceptance]
                 (mapv #(get-in % [:assumption :id]) (:obligations worker))))
          (is (= #{} (get-in identity [:assumption :dependencies])))
          (is (= #{:reference-worker/worker-identity} (get-in attribution [:assumption :dependencies])))
          (is (= #{:reference-worker/maintenance-attribution} (get-in dispatch [:assumption :dependencies])))
          (is (= :not-established (:worker-authentication worker) (:dispatch-authentication worker))))
      (do (is (empty? (:obligations worker)))
          (is (= :not-applicable (:worker-authentication worker) (:dispatch-authentication worker))))))
  (let [physical (:physical-refinement-premises result)
        interface (:scope physical)
        obligations (:obligations physical)
        ids (mapv #(get-in % [:assumption :id]) obligations)
        fingerprints (:open-premise-fingerprints physical)]
    (is (= observed-recovery/physical-refinement-profile (:profile physical)))
    (is (= :selected-historical-cut-only (:scope-kind interface)))
    (is (= (:judgment-subject result) (:judgment-subject interface)))
    (is (= (:historical-cut scope) (:historical-cut interface)))
    (is (= (:publication-source-id scope) (:publication-source-id interface)))
    (is (= (get-in result [:command-authentication-premises :scope :command-observation-ref])
           (:recovery-cut-ref interface)))
    (is (= (select-keys original [:manifest-tx-id :initialization-tx-id :selected-tx-ids :selected-commitments])
           (select-keys interface [:manifest-tx-id :initialization-tx-id :selected-tx-ids :selected-commitments])))
    (is (= [(second (:selected-tx-ids original))] (:selected-aborted-tx-ids interface)))
    (is (= (mapv #(update % :intent-id str) (:intervening-maintenance original))
           (:selected-maintenance interface)))
    (is (= (cond-> [:reference-physical/source-log :reference-physical/ledger-outcome
                   :reference-physical/snapshot-continuity :reference-physical/guarded-atomic-write
                   :reference-physical/submission-attribution :reference-physical/abort-atomicity
                   :reference-physical/aborted-attempt-attribution]
             (seq (:intervening-maintenance original)) (conj :reference-physical/worker-clock)
             true (conj :reference-physical/writer-coverage :reference-physical/authoritative-projection
                        :reference-physical/invariant-expression-semantics)) ids))
    (is (= (set (drop-last 3 ids))
           (get-in (nth obligations (- (count obligations) 3)) [:assumption :dependencies])))
    (is (= #{:reference-physical/snapshot-continuity} (get-in (nth obligations (- (count obligations) 2)) [:assumption :dependencies])))
    (is (= #{} (get-in (peek obligations) [:assumption :dependencies])))
    (is (= fingerprints (set (map :premise-fingerprint obligations))))
    (is (every? #(= interface (get-in % [:assumption :subject :interface])) obligations))
    (is (every? #(= :open (:discharge-state %)) obligations))
    (is (not-any? (:trusted-assumptions result) fingerprints))
    (is (not-any? (get-in result [:command-authentication-premises :open-premise-fingerprints]) fingerprints))
    (is (not-any? (get-in result [:worker-authentication-premises :open-premise-fingerprints]) fingerprints))
    (is (= :not-performed (:premise-acceptance physical)))
    (is (= :not-established (:provider-refinement physical) (:complete-writer-coverage physical)
           (:universal-refinement physical)))
    (is (= :none (:authority physical)))
    (is (= :not-issued (:certificate physical))))
  (let [inventory (:obligation-inventory result)
        index (:premises-by-fingerprint inventory)
        groups (:premise-families inventory)
        order (:dependency-order inventory)
        positions (zipmap order (range))
        abstract (:abstract-induction result)
        expected {:storage (:obligations result)
                  :command (get-in result [:command-authentication-premises :obligations])
                  :worker (get-in result [:worker-authentication-premises :obligations])
                  :physical (get-in result [:physical-refinement-premises :obligations])}]
    (is (= inventory/profile (:profile inventory)))
    (is (= (into {} (map (fn [[family ds]] [family (set (map :premise-fingerprint ds))])) expected) groups))
    (is (= (set (keys index)) (set order) (:all-described-open-premise-fingerprints inventory)))
    (is (= (count index) (count order) (reduce + (map count (vals expected)))))
    (is (= (:trusted-assumptions result) (:deployment-declared-premise-fingerprints inventory)))
    (is (= (reduce disj (set (keys index)) (:trusted-assumptions result)) (:unaccepted-premise-fingerprints inventory)))
    (is (= (:deployment-acceptance result) (:deployment-acceptance inventory)))
    (is (= (:fingerprint abstract) (get-in inventory [:abstract :report-fingerprint])))
    (is (= (:classification abstract) (select-keys (:abstract inventory) (keys (:classification abstract)))))
    (doseq [[fp {:keys [family dependency-fingerprints acceptance discharge-state]}] index]
      (is (contains? (get groups family) fp))
      (is (every? #(< (positions %) (positions fp)) dependency-fingerprints))
      (is (= (if (= :storage family) :explicit-declaration-only :not-performed) acceptance))
      (is (= :open discharge-state)))
    (is (= :not-established (:cross-family-implications inventory) (:universal-refinement inventory)))
    (is (= :not-performed (:proof-discharge inventory)))
    (is (= :none (:authority inventory)))
    (is (= :not-issued (:certificate inventory)))
    (let [transfer (:physical-invariant-transfer result)
          by-id (into {} (map (juxt (comp :id :assumption) :premise-fingerprint)) (:physical expected))
          binding {:faithful-pinned-provider-readbacks (by-id :reference-physical/snapshot-continuity)
                   :faithful-authoritative-state-projection (by-id :reference-physical/authoritative-projection)
                   :faithful-invariant-expression-semantics (by-id :reference-physical/invariant-expression-semantics)}
          required (into (set (vals binding)) (map by-id) [:reference-physical/source-log :reference-physical/ledger-outcome])]
      (is (= inventory/transfer-profile (:profile transfer)))
      (is (= :bound (:status transfer)) (pr-str transfer))
      (is (= binding (:condition-premise-fingerprints transfer)))
      (is (= required (:required-open-premise-fingerprints transfer)))
      (is (= (filterv required order) (:dependency-order transfer)))
      (is (= (get-in result [:selected-invariant-evidence :recovery-cut-ref]) (:recovery-cut-ref transfer)))
      (is (= (:fingerprint abstract) (:abstract-report-fingerprint transfer)))
      (is (= (:declared-invariant-ids abstract) (:invariant-ids transfer)))
      (is (every? (:unaccepted-premise-fingerprints inventory) required))
      (is (= :not-performed (:proof-discharge transfer) (:premise-acceptance transfer)))
      (is (= :not-established (:sufficiency transfer) (:universal-refinement transfer)))
      (is (= :none (:authority transfer)))
      (is (= :not-issued (:certificate transfer)))))
  (is (= (set (map :premise-fingerprint (:obligations result)))
         (:open-premise-fingerprints result) (:trusted-assumptions result)))
  (is (every? #(= :open (:discharge-state %)) (:obligations result)))
  (is (= :not-performed (:proof-discharge result)))
  (is (= :none (:authority result)))
  (is (= :not-issued (:certificate result))))

(defn- check-reopened-recovery!
  [assembly handler config tables admit mapped-source outcome-declarations
   policy-events {:keys [node worker ring-handler token sample rows observation steps initial comparison events retained-handle cut-handle foreign-comparisons worker-capture]}]
  (is (= :stopped (:status (lifecycle/status worker))))
  (is (= 503 (:status (ring-handler {:uri "/"}))))
  (is (nil? (correspondence/initialization-observation node admit)))
  (let [started (host/start! assembly handler config)
        reopened (:xtdb/connectable started)
        new-worker (get started managed/handle-key)
        adopted-handle (volatile! nil)]
    (try
      (is (not (host/worker-outcome-capture-matches? started worker-capture)))
      ;; Identical history and policy do not transfer a target capability.
      (doseq [foreign foreign-comparisons]
        (is (= :storage-trust-target-mismatch
               (trust-problem #(host/adopt-storage-trust! started foreign)))))
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
        (is (= (dissoc comparison :worker-correspondence)
               (cond-> r (:worker-correspondence comparison)
                 (assoc :worker-observation-provenance (:worker-observation-provenance comparison)))))
        ;; Content equality across reopening (or a byte copy) does not
        ;; transfer either locally issued capability.
        (let [new-handle (:acquisition (acquisition/capture! reopened))
              local! #(observed-recovery/compare-local-activated!
                       reopened tables admit mapped-source outcome-declarations steps %1 %2 true)]
          (is (some? new-handle))
          (let [required (observed-recovery/read-local-recovery-cut! admit observation cut-handle)]
            (is (= :exact-content (:cut-correspondence (cut/compare-prefix-cut! required reopened new-handle))))
            (is (nil? (observed-recovery/read-local-recovery-cut! admit (:observation fresh) cut-handle)))
            (let [policy (get-in config [:storage-trust :policy])
                  prepare! #(observed-recovery/prepare-conditional-recovery!
                             admit observation cut-handle reopened %1 %2)
                  prepared (prepare! new-handle policy)
                  handle (:conditional-recovery prepared)
                  scope (observed-recovery/read-conditional-recovery-scope! handle)
                  premises (mapv :assumption (:obligations (storage/describe-obligations scope)))
                  select! #(observed-recovery/select-conditional-recovery-premises! %1 %2)
                  selected (select! handle premises)
                  other (:conditional-recovery (prepare! new-handle policy))
                  trusted (observed-recovery/declare-conditional-recovery-trust!
                           handle {:deployment-id (get-in config [:storage-trust :deployment-id])
                                   :identity-policy (:identity-policy policy)
                                   :acceptance :trust-open-premises :premises premises})
                  adopted (host/adopt-storage-trust! started handle)]
              (vreset! adopted-handle handle)
              (is (= :prepared (:status prepared)))
              (is (= :conditional (:status adopted)))
              (is (= (:relation policy) (:relation adopted)))
              (is (= :conditional-on-declared-trust (:historical-assurance adopted)))
              (is (= (assoc trusted :deployment-adoption :configured-local-host) adopted))
              (check-recovery-judgment! adopted scope comparison admit)
              (is (not (contains? selected :judgment-profile)))
              (is (not (contains? trusted :deployment-adoption)))
              (with-redefs [observed-recovery/compare-local-activated! (constantly comparison)]
                (let [r (host/adopt-storage-trust! started handle)]
                  (is (= :conditional-recovery-boundary-replaced (:reason r)))
                  (is (not (contains? r :judgment-profile)))))
              (is (= (:obligations selected) (:obligations adopted)))
              (is (every? #(= :open (:discharge-state %)) (:obligations adopted)))
              (is (= :not-established (:observation-storage-identity adopted)
                     (:authorized-historical-lineage adopted)))
              (is (= :none (:authority adopted)))
              (is (= :not-issued (:certificate adopted)))
              (is (= required (:historical-cut scope)))
              (is (= (:source-id (first steps)) (:publication-source-id scope)))
              (is (= :conditional (:status selected)))
              (is (= :conditional-on-open-premises (:historical-assurance selected)))
              (is (= :exact-content (:cut-correspondence selected)))
              (is (= :not-performed (:premise-acceptance selected)))
              (is (= premises (mapv :assumption (:obligations selected))))
              (is (every? #(= :open (:discharge-state %)) (:obligations selected)))
              (is (= :none (:authority selected)))
              (is (= :not-issued (:certificate selected)))
              (is (= :not-established (:observation-storage-identity selected)
                     (:authorized-historical-lineage selected)))
              (is (= :subject-mismatch (:reason (select! other premises))))
              (is (= :incorrect-premise-set (:reason (select! handle (pop premises)))))
              (is (= :unissued-conditional-recovery (:reason (select! prepared premises))))
              (is (= :unissued-retained-acquisition (:reason (prepare! retained-handle policy))))
              (is (= :invalid-lifecycle-policy (:reason (prepare! new-handle (assoc policy :historical-cut required)))))
              (with-redefs [storage/describe-selected-premises (constantly selected)]
                (is (= :conditional-recovery-boundary-replaced (:reason (select! handle premises)))))
              (with-redefs [cut/compare-prefix-cut! (constantly {:status :matched})]
                (is (= :conditional-recovery-boundary-replaced (:reason (prepare! new-handle policy)))))))
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
      (finally (host/stop! started)))
    (is (some? @adopted-handle))
    (is (= :storage-trust-host-retired
           (trust-problem #(host/adopt-storage-trust! started @adopted-handle))))
    @adopted-handle))

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

(defn- check-conditional-extension! [started node admit mapped-source observation cut-handle earlier target original]
  (let [required (observed-recovery/read-local-recovery-cut! admit observation cut-handle)
        extended (:historical-cut (cut/derive-cut! node target))
        other-admission (induction/build-inductive-mapped-admission! mapped-source)
        expected-subject (expected-recovery-subject admit)
        prepare! (fn [relation acquisition]
                   (observed-recovery/prepare-conditional-recovery!
                    admit observation cut-handle node acquisition
                    {:comparison-id :fixture/recovery :relation relation
                     :identity-policy :fixture/custody :origin-boundary :fixture/origin
                     :acquisition-boundary :fixture/acquisition :identity-boundary :fixture/controller}))
        selections (into {}
                         (for [relation storage/relations
                               [kind acquisition] [[:exact-content earlier] [:prefix-content target]]
                               :let [prepared (prepare! relation acquisition)
                                     handle (:conditional-recovery prepared)
                                     scope (observed-recovery/read-conditional-recovery-scope! handle)
                                     premises (mapv :assumption (:obligations (storage/describe-obligations scope)))
                                     selected (observed-recovery/select-conditional-recovery-premises! handle premises)
                                     declaration {:deployment-id :fixture/deployment
                                                  :identity-policy (:identity-policy scope)
                                                  :acceptance :trust-open-premises :premises premises}
                                     trusted (observed-recovery/declare-conditional-recovery-trust! handle declaration)]]
                           (do
                             (is (= :prepared (:status prepared)))
                             (is (= required (:historical-cut scope)))
                             (is (= relation (:relation selected)))
                             (is (= kind (:cut-correspondence selected)))
                             (is (= :conditional (:status selected)))
                             (is (= :conditional-on-open-premises (:historical-assurance selected)))
                             (is (= :not-performed (:premise-acceptance selected)))
                             (is (= :complete-for-declared-premises (:dependency-closure selected)))
                             (is (every? #(= :open (:discharge-state %)) (:obligations selected)))
                             (is (= :none (:authority selected)))
                             (is (= :not-issued (:certificate selected)))
                             (is (= :not-established (:observation-storage-identity selected)
                                    (:authorized-historical-lineage selected)))
                             (is (= :conditional (:status trusted)))
                             (is (= :conditional-on-declared-trust (:historical-assurance trusted)))
                             (check-recovery-judgment! trusted scope original admit)
                             (is (= trusted (observed-recovery/declare-conditional-recovery-trust!
                                             handle admit expected-subject declaration)))
                             (doseq [[candidate subject reason]
                                     [[other-admission expected-subject :recovery-admission-mismatch]
                                      [(induction/describe-mapped-admission admit) expected-subject :recovery-admission-mismatch]
                                      [admit (assoc expected-subject :catalog-id :fixture/other) :recovery-subject-mismatch]
                                      [admit (assoc expected-subject :induction-fingerprint "other") :recovery-subject-mismatch]
                                      [admit (assoc expected-subject :abstract-induction-fingerprint "other") :recovery-subject-mismatch]
                                      [admit (assoc-in expected-subject [:abstract-induction-identity :certificate-fingerprint] "other") :recovery-subject-mismatch]
                                      [admit (assoc-in expected-subject [:abstract-induction-identity :dependency-fingerprint] "other") :recovery-subject-mismatch]
                                      [admit (update expected-subject :selected-operations #(vec (reverse %))) :recovery-subject-mismatch]
                                      [admit (assoc-in expected-subject [:selected-operations 0 :transition-fingerprint] "other") :recovery-subject-mismatch]]]
                               (let [failure (observed-recovery/declare-conditional-recovery-trust!
                                              handle candidate subject declaration)]
                                 (is (= :unsupported (:status failure)))
                                 (is (= reason (:reason failure)))
                                 (is (not-any? #(contains? failure %)
                                               [:judgment-subject :judgment-profile :trusted-assumptions :original-recovery
                                                :abstract-induction :abstract-induction-binding :command-authentication-premises
                                                :worker-authentication-premises :physical-refinement-premises :obligation-inventory :selected-invariant-evidence]))))
                             (is (not (contains? trusted :deployment-adoption)))
                             (is (not (contains? selected :judgment-profile)))
                             (is (= :explicit-declaration-only (:premise-acceptance trusted)))
                             (is (= :not-established (get-in trusted [:deployment-acceptance :authentication])))
                             (is (= (:obligations selected) (:obligations trusted)))
                             (is (= (set (map :premise-fingerprint (:obligations selected))) (:trusted-assumptions trusted)))
                             (is (= kind (:cut-correspondence trusted)))
                             (is (= :none (:authority trusted)))
                             (is (= :not-issued (:certificate trusted)))
                             (is (= :not-established (:observation-storage-identity trusted)
                                    (:authorized-historical-lineage trusted)))
                             (is (= :unissued-conditional-recovery
                                    (:reason (observed-recovery/declare-conditional-recovery-trust! prepared declaration))))
                             (with-redefs [induction/recheck-mapped-induction!
                                           (constantly (:abstract-induction trusted))]
                               (let [failure (observed-recovery/declare-conditional-recovery-trust! handle declaration)]
                                 (is (= :conditional-recovery-boundary-replaced (:reason failure)))
                                 (is (not (contains? failure :abstract-induction)))))
                             (with-redefs [assumption/describe-assumption (constantly nil)]
                               (let [failure (observed-recovery/declare-conditional-recovery-trust! handle declaration)]
                                 (is (= :conditional-recovery-boundary-replaced (:reason failure)))
                                 (is (not (contains? failure :command-authentication-premises)))
                                 (is (not (contains? failure :worker-authentication-premises)))
                                 (is (not (contains? failure :physical-refinement-premises)))))
                             (with-redefs [invariant/audit-issued-selection! (constantly (:selected-invariant-evidence trusted))]
                               (let [failure (observed-recovery/declare-conditional-recovery-trust! handle declaration)]
                                 (is (= :conditional-recovery-boundary-replaced (:reason failure)))
                                 (is (not (contains? failure :selected-invariant-evidence)))))
                             (with-redefs [inventory/describe-transfer-binding (constantly (:physical-invariant-transfer trusted))]
                               (let [failure (observed-recovery/declare-conditional-recovery-trust! handle declaration)]
                                 (is (= :conditional-recovery-boundary-replaced (:reason failure)))
                                 (is (not (contains? failure :physical-invariant-transfer)))))
                             (with-redefs [storage/describe-deployment-acceptance (constantly trusted)]
                               (is (= :conditional-recovery-boundary-replaced
                                      (:reason (observed-recovery/declare-conditional-recovery-trust! handle declaration)))))
                             (if (= :authorized-historical-lineage relation)
                               (let [adopted (host/adopt-storage-trust! started handle)]
                                 (is (= (assoc trusted :deployment-adoption :configured-local-host) adopted))
                                 (is (= :unissued-storage-trust-host
                                        (trust-problem #(host/adopt-storage-trust! (into {} started) handle))))
                                 (is (= :storage-trust-target-mismatch
                                        (trust-problem #(host/adopt-storage-trust! started prepared))))
                                 (with-redefs [observed-recovery/conditional-recovery-target-matches? (constantly true)]
                                   (is (= :storage-trust-boundary-replaced
                                          (trust-problem #(host/adopt-storage-trust! started handle))))))
                               (is (= :storage-trust-policy-mismatch
                                      (trust-problem #(host/adopt-storage-trust! started handle)))))
                             [[relation kind] {:handle handle :premises premises :declaration declaration}])))]
    (is (not (identical? admit other-admission)))
    (is (= (induction/describe-mapped-admission admit)
           (induction/describe-mapped-admission other-admission)))
    (is (= 5 (count (:transaction-ids required))))
    (is (= 9 (count (:transaction-ids extended))))
    (is (= (:transaction-ids required) (subvec (:transaction-ids extended) 0 5)))
    (is (not= (:payload-fingerprint required) (:payload-fingerprint extended)))
    (is (= :prefix-content (:cut-correspondence (cut/compare-prefix-cut! required node target))))
    (doseq [[[relation kind] {:keys [handle]}] selections
            [[other-relation other-kind] other] selections
            :when (not= [relation kind] [other-relation other-kind])]
      (let [r (observed-recovery/select-conditional-recovery-premises! handle (:premises other))
            trusted (observed-recovery/declare-conditional-recovery-trust! handle (:declaration other))]
        (doseq [result [r trusted]]
          (is (= :unsupported (:status result)))
          (is (= (if (= relation other-relation) :subject-mismatch :incorrect-premise-set) (:reason result)))
          (is (not (contains? result :historical-assurance)))
          (is (not (contains? result :trusted-assumptions)))
          (is (not (contains? result :judgment-profile)))
          (is (not (contains? result :abstract-induction))))))
    (get-in selections [[:authorized-historical-lineage :prefix-content] :handle])))

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
        worker* (atom nil) host* (atom nil) gap* (atom nil) restart* (atom nil)
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
        policy-maintenance (atom [])
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
                                                   worker-capture (host/capture-worker-outcomes!
                                                                   @host* 16
                                                                   #(await-maintenance-cycle! @worker*))
                                                   done (:result worker-capture)]
                                               (reset! gap* {:before prior
                                                             :after (selected-rows (:xtdb/connectable ctx) protected)
                                                             :pending pending :done done :worker-capture worker-capture}))))
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
                     :storage-trust {:deployment-id :fixture/deployment :acceptance :trust-open-premises
                                     :policy {:comparison-id :fixture/recovery :relation :authorized-historical-lineage
                                              :identity-policy :fixture/custody :origin-boundary :fixture/origin
                                              :acquisition-boundary :fixture/acquisition :identity-boundary :fixture/controller}}
                     :assembled-operations {:authorize-request? #(and (= "yes" (get-in % [:headers "fixture-gate"])) (some? (principal-fn %)))}
                     :assembled-publication (assoc mapping :model-entity-topics {:requests #{:request} :request_assignments #{:request-assignment}}
                                                   :require-derived-model? true :operation-model-entities {:request/claim #{:requests :request_assignments}})
                     :mapped-writer-policy {:admission admit :initializer seed
                                            :observe! (fn [event]
                                                        (swap! policy-events conj (:kind event))
                                                        (when (= :worker (:kind event))
                                                          (swap! policy-maintenance conj event)))}}]
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
              (reset! host* started)
              (is (= :unissued-worker-capture-host
                     (trust-problem #(host/capture-worker-outcomes! (into {} started) 1 (constantly nil)))))
              (let [empty-capture (host/capture-worker-outcomes! started 1 (constantly :empty))]
                (is (= :empty (:result empty-capture)))
                (is (= :invalidated-local-worker-outcome-window (:policy-window empty-capture)))
                (is (not (host/worker-outcome-capture-matches? started empty-capture))))
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
                (let [claims (:claims (publication/read-framed-submission-observation! admit observation))
                      expected (fn [b]
                                 (assoc b :execution-id (identity/execution-id (str "e-" (resolution/record-id b)))
                                        :origin :registered-server-invocation
                                        :authentication :conditional-on-trusted-principal-resolver))]
                  (is (= (mapv expected [winner loser loser]) (mapv :command-origin claims)))
                  (is (= (mapv resolution/record-id [winner loser loser])
                         (mapv #(get-in % [:checked :identity :command-id]) claims))))
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
                      bound (observed-recovery/capture-local-recovery-cut!
                             node tables admit mapped-source outcome-declarations steps observation handle interleaved?)
                      cut-handle (:recovery-cut-observation bound)
                      required (observed-recovery/read-local-recovery-cut! admit observation cut-handle)
                      local (dissoc bound :recovery-cut-profile :recovery-cut-observation :recovery-cut-binding)
                      bad (local! sample)
                      r ((if interleaved? observed-recovery/compare-activated-worker-interleaved!
                             observed-recovery/compare-activated!) node tables admit mapped-source outcome-declarations steps observation)]
                  (is (= :matched (:status r)) (pr-str r))
                  (is (= sample (acquisition/read-acquisition! node handle)))
                  (is (= :matched (:status local)) (pr-str local))
                  (is (= :matched-local-recovery-cut (:recovery-cut-binding bound)))
                  (is (= (:historical-cut (cut/derive-cut! node handle)) required))
                  (is (some? required))
                  (is (not (contains? bound :historical-cut)))
                  (is (nil? (observed-recovery/read-local-recovery-cut! (Object.) observation cut-handle)))
                  (is (nil? (observed-recovery/read-local-recovery-cut! admit (Object.) cut-handle)))
                  (is (nil? (observed-recovery/read-local-recovery-cut! admit observation bound)))
                  (is (nil? (observed-recovery/read-local-recovery-cut! admit observation (observed-recovery/->RecoveryCutObservation))))
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
                  (is (= :matched-issued-command-bindings (:command-origin-correspondence r)))
                  (is (= :same-observed-execution (:abort-recovery-execution r)))
                  (is (= :not-established (:command-authentication r)))
                  (is (= (mapv (fn [b step]
                                 (assoc (select-keys b [:command-id :operation])
                                        :execution-id (identity/execution-id (str "e-" (resolution/record-id b)))
                                        :selected-tx-id (get-in step [:expectation :tx-id])))
                               [winner loser loser] steps)
                         (:selected-command-origins r)))
                  (is (= :not-established (:submission-provenance r)))
                  (is (= :not-established (:aborted-receipt-identity r)))
                  (is (= :none (:authority r)))
                  (is (= :not-issued (:certificate r)))
                  (when-not interleaved?
                    (reset! restart* {:retained-handle handle :cut-handle cut-handle :comparison r}))
                  (when interleaved?
                    (let [{:keys [before after pending done worker-capture]} @gap*
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
                      (let [calls (:calls worker-capture)]
                        (is (= :matched-local-policy-outcome-window (:policy-window worker-capture)))
                        (is (live-xtdb/policy-capture-origin-matches? worker-capture node))
                        (is (host/worker-outcome-capture-matches? started worker-capture))
                        (is (not (host/worker-outcome-capture-matches? (into {} started) worker-capture)))
                        (is (not (host/worker-outcome-capture-matches? started (into {} worker-capture))))
                        (is (not (live-xtdb/policy-capture-origin-matches? (into {} worker-capture) node)))
                        (is (= 4 (count calls)))
                        (is (every? #(and (= :execute-tx (:api %)) (= :returned (:outcome %))
                                          (empty? (:tx-options %))) calls))
                        (is (= @policy-maintenance (mapv :admission calls)))
                        (is (= (mapv :tx-id maintenance)
                               (mapv #(.getTxId ^xtdb.api.TransactionKey (:tx-result %)) calls)))
                        (is (= :none (:authority worker-capture)))
                        (is (= :not-issued (:certificate worker-capture)))
                        (let [compare! #(worker-submission/compare-issued!
                                         %1 tables admit mapped-source outcome-declarations steps observation %2 %3)
                              checked (compare! started handle worker-capture)
                              selected (:selected-maintenance checked)
                              compare-calls #'worker-envelope/compare-calls!]
                          (is (= :matched (:status checked)) (pr-str checked))
                          (is (= :matched-issued-worker-retained-transactions (:maintenance-correspondence checked)))
                          (is (= :matched-local-issued-host-capture (:worker-observation-provenance checked)))
                          (is (= :matched-xtdb-microsecond-worker-envelope (:payload-correspondence checked)))
                          (is (= maintenance (mapv #(select-keys % [:tx-id :intent-id :maintenance]) selected)))
                          (is (= (mapv :worker-origin @policy-maintenance) (mapv :worker-origin selected)))
                          (is (every? :committed? selected))
                          (is (= (:selected-tx-ids r) (:selected-tx-ids checked)))
                          (is (= (:payload-fingerprint sample) (:retained-payload-fingerprint checked)))
                          (is (= :not-established (:worker-authentication checked) (:dispatch-authentication checked)
                                 (:provider-refinement checked) (:complete-writer-coverage checked)))
                          (is (= :none (:authority checked)))
                          (is (= :not-issued (:certificate checked)))
                          (doseq [bad [(compare! started handle (into {} worker-capture))
                                       (compare! (into {} started) handle worker-capture)
                                       (compare! started sample worker-capture)]]
                            (is (= :unsupported (:status bad)))
                            (is (not (contains? bad :selected-maintenance))))
                          (doseq [changed [(pop calls) (conj calls (first calls)) (vec (reverse calls))
                                           (assoc-in calls [0 :outcome] :threw)
                                           (assoc-in calls [0 :tx-result] (:tx-result (second calls)))
                                           (assoc-in calls [0 :admission :worker-origin :maintenance] :dispatch-acknowledgment)
                                           (assoc-in calls [0 :tx-options] {:system-time (:system-time (first history))})
                                           (update-in calls [0 :tx-ops] #(assoc-in % [(dec (count %)) 2 :fixture/tampered] true))
                                           (update-in calls [0 :tx-ops 1 1] #(.plusSeconds ^java.time.Instant % 1))
                                           (update-in calls [0 :tx-ops]
                                                      (fn [ops]
                                                        (let [path [(dec (count ops)) 2 :gesso.live.outbox/lease-until]
                                                              changed (.plusSeconds ^java.time.Instant (get-in ops path) 1)]
                                                          (-> ops (assoc-in path changed)
                                                              (assoc-in [1 1] changed) (assoc-in [2 1] changed)))))]]
                            (let [bad (compare-calls node source-id sample maintenance changed)]
                              (is (= :mismatch (:status bad)) (pr-str bad))
                              (is (not (contains? bad :selected-maintenance)))))
                          (with-redefs [host/worker-outcome-capture-matches? (constantly true)]
                            (let [bad (compare! started handle worker-capture)]
                              (is (= :worker-comparison-boundary-replaced (:reason bad)))
                              (is (not (contains? bad :selected-maintenance)))))))
                      ;; These are actual installed writer-policy observations,
                      ;; not a test policy calling the provenance helper.
                      (let [origins (mapv :worker-origin @policy-maintenance)]
                        (is (= (mapv :intent-id maintenance) (mapv :intent-id @policy-maintenance)))
                        (is (= (mapv :maintenance maintenance) (mapv :maintenance origins)))
                        (is (= 4 (count origins)))
                        (is (every? #(and (= :matched-scheduled-maintenance (:call-chain %))
                                          (= :scheduled-local-cycle (:origin %))
                                          (= :not-established (:authentication %))
                                          (= source-id (:source-id %))
                                          (= "assembled-recovery" (:worker-id %))
                                          (= 2 (:cycle-number %))
                                          (uuid? (:lifecycle-ref %))) origins))
                        (is (apply = (map :lifecycle-ref origins))))
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
                      ;; The captured second cycle is exact; later read-only
                      ;; checks can overlap additional empty scheduler polls.
                      (let [status (lifecycle/status worker)]
                        (is (<= 2 (:cycles status)))
                        (is (= 2 (:total-processed status) (:total-dispatch-accepted status)))
                        (is (#{:idle :running} (:status status))))
                      (let [capture! #(observed-recovery/capture-local-worker-recovery-cut!
                                       %1 tables admit mapped-source outcome-declarations steps observation %2 %3)
                            qualified (writer-history/derive! started tables admit mapped-source outcome-declarations
                                                              (:catalog-fingerprint catalog) cases steps observation handle worker-capture)
                            worker-bound (:recovery qualified)
                            enhanced-cut (:recovery-cut-observation worker-bound)
                            worker-evidence (:worker-correspondence worker-bound)
                            enhanced (assoc r :worker-observation-provenance (:worker-observation-provenance worker-bound)
                                            :worker-correspondence worker-evidence)
                            policy (get-in host-config [:storage-trust :policy])
                            foreign (mapv (fn [relation]
                                            (let [prepared (observed-recovery/prepare-conditional-recovery!
                                                            admit observation enhanced-cut node handle
                                                            (assoc policy :relation relation))]
                                              (is (= :prepared (:status prepared)))
                                              (:conditional-recovery prepared)))
                                          [:authorized-historical-lineage :same-store-incarnation])]
                        (is (= :conditional-installed-retained-writer-history (:status qualified)) (pr-str (dissoc qualified :recovery)))
                        (is (= :complete-at-selected-cut (:retained-transaction-classification qualified)))
                        (is (= (mapv :tx-id (subvec history 2)) (mapv :tx-id (:post-initialization-steps qualified))))
                        (is (= [:committed-mapped :committed-worker :committed-worker :committed-worker :committed-worker :aborted-attempt :committed-mapped]
                               (mapv :class (:post-initialization-steps qualified))))
                        (is (= :not-established (:writer-coverage qualified) (:universal-refinement qualified) (:temporal-coverage qualified)))
                        (is (= :not-performed (:proof-discharge qualified)))
                        (is (= :matched (:status worker-bound)) (pr-str worker-bound))
                        (is (= :matched-local-issued-host-capture (:worker-observation-provenance worker-bound)))
                        (is (= maintenance (mapv #(select-keys % [:tx-id :intent-id :maintenance])
                                                 (:selected-maintenance worker-evidence))))
                        (is (= required (observed-recovery/read-local-recovery-cut! admit observation enhanced-cut)))
                        (is (not (contains? local :worker-correspondence)))
                        (doseq [bad [(capture! started handle (into {} worker-capture))
                                     (capture! started handle {:status :matched :worker-correspondence worker-evidence})
                                     (capture! started sample worker-capture)]]
                          (is (= :unsupported (:status bad)))
                          (is (not-any? #(contains? bad %) [:recovery-cut-observation :worker-correspondence :worker-observation-provenance])))
                        (with-redefs [worker-envelope/compare-issued-selection! (constantly worker-evidence)]
                          (let [bad (capture! started handle worker-capture)]
                            (is (= :recovery-cut-boundary-replaced (:reason bad)))
                            (is (not (contains? bad :recovery-cut-observation)))))
                        (with-redefs [worker-envelope/compare-issued-selection! (constantly worker-evidence)]
                          (let [bad (host/adopt-storage-trust! started (first foreign))]
                            (is (= :conditional-recovery-boundary-replaced (:reason bad)))
                            (is (not (contains? bad :judgment-profile)))))
                        (let [adopted (host/adopt-storage-trust! started (first foreign))]
                          (is (= :configured-local-host (:deployment-adoption adopted)))
                          (check-recovery-judgment! adopted
                                                   (observed-recovery/read-conditional-recovery-scope! (first foreign))
                                                   enhanced admit))
                        (reset! restart* {:foreign-comparisons foreign
                                         :worker-capture worker-capture
                                         :node node :worker worker :ring-handler @handler*
                                        :token (live-xtdb/current-submission-policy-token node)
                                        :sample sample :rows (selected-rows node tables)
                                        :observation observation :steps steps :comparison enhanced :retained-handle handle :cut-handle enhanced-cut
                                        :initial (correspondence/initialization-observation node admit)
                                        :events @policy-events})))))
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
                  (is (= sample2 (coverage/observe-local-processed-retained-replay-inputs! node)))
                  (let [{:keys [retained-handle cut-handle]} @restart*
                        target (:acquisition (acquisition/capture! node))]
                    (is (= sample2 (acquisition/read-acquisition! node target)))
                    (swap! restart* assoc :trust-handle
                           (check-conditional-extension! started node admit mapped-source observation cut-handle retained-handle target (:comparison @restart*)))))))
              (finally (host/stop! started)))
            (when-not interleaved?
              (is (= :storage-trust-host-retired
                     (trust-problem #(host/adopt-storage-trust! started (:trust-handle @restart*))))))
            (when interleaved?
              (is (= :worker-capture-host-retired
                     (trust-problem #(host/capture-worker-outcomes! started 1 (constantly nil)))))
              (is (host/worker-outcome-capture-matches? started (:worker-capture @restart*)))
              (doseq [foreign (:foreign-comparisons @restart*)]
                (is (= :storage-trust-host-retired
                       (trust-problem #(host/adopt-storage-trust! started foreign)))))
              (let [reopen-config (assoc-in host-config [:storage-trust :policy :relation]
                                            :same-store-incarnation)
                    reopened-handle (check-reopened-recovery!
                                     assembly (fn [_] (g/html-response surface)) reopen-config
                                     tables admit mapped-source outcome-declarations policy-events @restart*)]
                ;; The copy selects lineage, never same-incarnation trust.
                (check-substituted-store! root assembly (fn [_] (g/html-response surface)) host-config
                                          tables admit mapped-source outcome-declarations policy-events
                                          (update @restart* :foreign-comparisons conj reopened-handle)))))))
      (finally (doseq [f (reverse (file-seq (.toFile root)))] (io/delete-file f true))))))

(deftest assembled-command-transport-observed-recovery
  (check-assembled-recovery! false))

(deftest assembled-command-transport-worker-interleaved-recovery
  (check-assembled-recovery! true))

(deftest command-authentication-premises-are-exact-and-unaccepted
  (let [describe #'observed-recovery/command-authentication-premises
        entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
               :command-observation-ref "original-cut"
               :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
               :original-recovery {:selected-tx-ids [1 2 3] :selected-commitments [:committed :aborted :committed]
                                   :command-origin-correspondence :matched-issued-command-bindings
                                   :selected-command-origins [{:execution-id "one"} {:execution-id "two"} {:execution-id "two"}]
                                   :abort-recovery-execution :same-observed-execution}}
        r (describe entry)]
    (is (= r (describe (assoc-in entry [:scope :retained-acquisition-ref] "different-target"))))
    (doseq [changed [(assoc entry :command-observation-ref "different-cut")
                     (assoc-in entry [:scope :publication-source-id] "other-source")
                     (assoc-in entry [:scope :historical-cut :payload-fingerprint] "different-content")
                     (assoc-in entry [:judgment-subject :catalog-id] :fixture/other)
                     (assoc-in entry [:original-recovery :selected-tx-ids] [1 2 4])
                     (assoc-in entry [:original-recovery :command-origin-correspondence] :partial-issued-command-bindings)]]
      (let [other (describe changed)]
        (is (not-any? (:open-premise-fingerprints r) (:open-premise-fingerprints other)))
        (is (= :not-established (:command-authentication other)))
        (is (= :not-performed (:premise-acceptance other)))))
    (let [absent (describe (update entry :original-recovery assoc
                                   :command-origin-correspondence :not-observed
                                   :selected-command-origins [nil nil nil]
                                   :abort-recovery-execution :not-observed))]
      (is (= :not-observed (get-in absent [:scope :origin-correspondence])))
      (is (= [nil nil nil] (:selected-command-origins absent)))
      (is (= 2 (count (:obligations absent))))
      (is (every? #(= :open (:discharge-state %)) (:obligations absent)))
      (is (= :not-established (:command-authentication absent))))))

(deftest worker-deadline-projection-is-provider-specific
  (let [project #'worker-envelope/provider-worker-ops
        deadline (java.time.Instant/parse "2026-11-26T20:00:00.123456789Z")
        encoded (java.time.Instant/parse "2026-11-26T20:00:00.123456Z")
        old-deadline (java.time.Instant/parse "2026-11-26T19:59:00.123456789Z")
        ops [["old guard" old-deadline]
             ["ASSERT CURRENT_TIMESTAMP < ?" deadline]
             ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline]
             [:put-docs :gesso_live_outbox {:gesso.live.outbox/lease-until deadline
                                          :fixture/other old-deadline}]]
        expected (-> ops (assoc-in [1 1] encoded) (assoc-in [2 1] encoded)
                     (assoc-in [3 2 :gesso.live.outbox/lease-until] encoded))]
    (is (= expected (project :lease-acquisition ops)))
    (is (= expected (project :expired-lease-reclamation ops)))
    (is (= ops (project :dispatch-acknowledgment ops)))
    (is (nil? (project :lease-acquisition (assoc-in ops [1 1] encoded))))
    (is (nil? (project :lease-acquisition (assoc-in ops [2 0] "unexpected guard"))))))

(deftest worker-authentication-premises-bind-original-maintenance
  (let [describe #'observed-recovery/worker-authentication-premises
        intent (random-uuid)
        lifecycle (random-uuid)
        selected [{:tx-id 2 :intent-id intent :maintenance :lease-acquisition}
                  {:tx-id 3 :intent-id intent :maintenance :dispatch-acknowledgment}]
        observed (mapv #(assoc % :committed? true :system-time (java.time.Instant/parse "2026-11-26T20:00:00.123456Z")
                               :worker-origin {:source-id "claim" :worker-id "worker" :lifecycle-ref lifecycle
                                               :cycle-number 2 :maintenance (:maintenance %)
                                               :origin :scheduled-local-cycle :authentication :not-established
                                               :call-chain :matched-scheduled-maintenance}) selected)
        entry {:scope {:publication-source-id "claim" :historical-cut {:payload-fingerprint "fixture"}}
               :command-observation-ref "original-cut"
               :judgment-subject {:catalog-id :fixture/claim :selected-operations [:accept :accept :refuse]}
               :original-recovery {:selected-tx-ids [1 4 5] :selected-commitments [:committed :aborted :committed]
                                   :intervening-maintenance selected
                                   :worker-observation-provenance :matched-local-issued-host-capture
                                   :worker-correspondence {:selected-maintenance observed}}}
        r (describe entry)
        fingerprints (:open-premise-fingerprints r)]
    (is (= r (describe (assoc-in entry [:scope :retained-acquisition-ref] "new-target"))))
    (is (= 3 (count fingerprints)))
    (is (= :not-established (:worker-authentication r) (:dispatch-authentication r)))
    (is (= :not-performed (:premise-acceptance r)))
    (is (every? #(= :open (:discharge-state %)) (:obligations r)))
    (doseq [changed [(assoc entry :command-observation-ref "other-cut")
                     (assoc-in entry [:scope :publication-source-id] "other-source")
                     (assoc-in entry [:scope :historical-cut :payload-fingerprint] "other-content")
                     (assoc-in entry [:judgment-subject :catalog-id] :fixture/other)
                     (assoc-in entry [:original-recovery :selected-tx-ids] [1 4 6])
                     (assoc-in entry [:original-recovery :intervening-maintenance 0 :intent-id] (random-uuid))
                     (update-in entry [:original-recovery :intervening-maintenance] #(vec (reverse %)))
                     (assoc-in entry [:original-recovery :worker-correspondence :selected-maintenance 0 :worker-origin :lifecycle-ref] (random-uuid))
                     (assoc-in entry [:original-recovery :worker-correspondence :selected-maintenance 0 :worker-origin :cycle-number] 3)
                     (assoc-in entry [:original-recovery :worker-correspondence :selected-maintenance 0 :system-time] (java.time.Instant/parse "2026-11-26T20:00:01Z"))]]
      (let [other (describe changed)]
        (is (not-any? fingerprints (:open-premise-fingerprints other)))
        (is (= :not-established (:worker-authentication other) (:dispatch-authentication other)))
        (is (= :not-performed (:premise-acceptance other)))))
    (let [absent (describe (update entry :original-recovery #(-> % (dissoc :worker-correspondence)
                                                                  (assoc :worker-observation-provenance :not-established))))
          acquisition-only (describe (update entry :original-recovery assoc :intervening-maintenance [(first selected)]
                                             :worker-correspondence {:selected-maintenance [(first observed)]}))
          adjacent (describe (update entry :original-recovery dissoc :intervening-maintenance
                                     :worker-correspondence :worker-observation-provenance))]
      (is (= 3 (count (:obligations absent))))
      (is (empty? (get-in absent [:scope :observed-maintenance])))
      (is (= :not-established (get-in absent [:scope :worker-observation-provenance])))
      (is (= :not-established (:worker-authentication absent) (:dispatch-authentication absent)))
      (is (not-any? fingerprints (:open-premise-fingerprints absent)))
      (is (= 2 (count (:obligations acquisition-only))))
      (is (= :not-applicable (:dispatch-authentication acquisition-only)))
      (is (empty? (:obligations adjacent)))
      (is (= :not-applicable (:worker-authentication adjacent) (:dispatch-authentication adjacent)))
      (is (empty? (:open-premise-fingerprints adjacent))))))

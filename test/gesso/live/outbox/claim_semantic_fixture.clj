(ns gesso.live.outbox.claim-semantic-fixture
  "Independent typed source for the installed two-helper Claim fixture.
   One initial epoch and one command per helper; no production writer closure.
   Observations are diagnostic, not certificates or transaction authority."
  (:require [clojure.set :as set]
            [gesso.model.tx :as model-tx]
            [gesso.model.mapped-guard :as guard]
            [gesso.model.certified-plan :as plan]
            [gesso.model.command-resolution :as resolution]
            [gesso.assurance.reference-mapped-outcome :as outcome]
            [gesso.model.mapped-projection :as projection]
            [gesso.model.inductive-plan :as inductive-plan]
            [gesso.assurance.transition :as construction]
            [gesso.assurance.dependency :as dependency]
            [gesso.assurance.proof.induction :as induction]
            [gesso.assurance.proof.induction-dependency :as closure]
            [gesso.assurance.proof.certificate :as certificate]
            [gesso.assurance.proof.transition :as abstract]
            [gesso.assurance.proof.independent-transition-semantics :as independent]
            [gesso.choreo.identity :as identity]
            [gesso.live.consistency.xtdb :as xtdb]
            [gesso.live.outbox.read-frontier :as frontier])
  (:import [java.time Instant]))

(def helpers {"helper-1" [:journal/one :membership/one]
              "helper-2" [:journal/two :membership/two]})

(def state-types
  {:request/status {:type :keyword}
   :request/revision {:type :integer}
   :assignment/helper {:type :string :nullable? true}
   :journal/one {:type :keyword :nullable? true}
   :journal/two {:type :keyword :nullable? true}
   :membership/one {:type :boolean}
   :membership/two {:type :boolean}})

(def target-map
  {:request/status [:document :requests "r1" :status]
   :request/revision [:document :requests "r1" :revision]
   :assignment/helper [:nullable-document :request_assignments "primary-r1" :helper]
   :membership/one [:document :memberships "helper-1" :active]
   :membership/two [:document :memberships "helper-2" :active]
   :journal/one [:journal-slot "helper-1" :request/claim]
   :journal/two [:journal-slot "helper-2" :request/claim]})

(defn alternative-id [helper alternative]
  (keyword (str "fixture.claim." helper) (name alternative)))

(defn source [helper alternative]
  (let [[slot membership] (or (helpers helper) (throw (ex-info "Unknown helper" {})))
        accept? (= :accept alternative)
        _ (when-not (#{:accept :refuse :replay-accepted :replay-rejected} alternative) (throw (ex-info "Unknown alternative" {})))
        conditions (case alternative
                     :accept
                     {:request/status :open :request/revision 0 :assignment/helper nil
                      slot nil membership true}
                     :refuse {:request/status :claimed :request/revision 1 slot nil}
                     :replay-accepted {slot :accepted}
                     :replay-rejected {slot :rejected})
        replacements (cond
                       accept?
                       {:request/status [:literal :claimed] :request/revision [:literal 1]
                        :assignment/helper [:literal helper] slot [:literal :accepted]}
                       (= :refuse alternative) {slot [:literal :rejected]}
                       :else {})
        reads (set (keys conditions))
        checks (mapv (fn [[k v]] [:= [:read k] [:literal v]]) (sort-by key conditions))
        writes (set (keys replacements))]
    {:subject {:kind :operation :id (alternative-id helper alternative)
               :interface {:operation (alternative-id helper alternative)}}
     :state-types state-types :input-types {}
     :guard {:readers reads
             :expression (if (= 1 (count checks)) (first checks) (into [:and] checks))}
     :reads reads :writes writes :replacements replacements
     :frame (set/difference (set (keys state-types)) writes)}))

(defn observe! [helper alternative before after]
  (let [original (:transition (construction/build-guarded-replacement! (source helper alternative)))
        checked (abstract/observe-transition! original before {} after)
        other (independent/observe-transition! original before {} after)]
    (when-not (= (:matches? checked) (:matches? other))
      (throw (ex-info "Claim interpreters disagree" {})))
    checked))

(defn- require! [condition]
  (when-not condition (throw (ex-info "Outside closed Claim observation" {}))))

(declare catalog)

(defn outcome-declarations []
  (into {} (for [helper (keys helpers) branch [:accept :refuse]]
             [(alternative-id helper branch)
              {:arguments {:request-id "r1"}
               :outcome (if (= :accept branch)
                          {:outcome :request/claimed :helper helper}
                          {:outcome :request/not-claimable :reason :already-claimed})}])))

;; The fixture has one fixed catalog. Keep the decoder identity stable across
;; independently constructed mapped sources and use the same closed cases for
;; snapshot projection, preparation and installed admission.
(def ^:private claim-outcome-cases
  (delay (:cases (outcome/derive-cases! {:inductive-source {:catalog (catalog)}
                                       :target-map target-map}
                                      (outcome-declarations)))))

(defn- decode-claim-outcome! [record]
  (let [row (resolution/require-record record)
        {:keys [principal operation arguments]} (:gesso.command/binding row)
        k [(identity/raw-value principal) operation arguments (:gesso.command/outcome row)]
        cases @claim-outcome-cases]
    (if (contains? cases k)
      (get cases k)
      (throw (ex-info "Outside closed Claim outcome cases" {:reason :undeclared-claim-outcome})))))

(defn snapshot!
  "Acquire the closed fixture rows using the supplied query function; delegate
   typed projection and identity coverage to the mapped decoder. The extra
   assignment relationship and outcome meanings remain fixture-specific."
  [q]
  (let [requests (q ["SELECT * FROM requests"])
        assignments (q ["SELECT * FROM request_assignments"])
        memberships (q ["SELECT * FROM memberships"])
        records (q ["SELECT * FROM gesso_commands"])]
    (require! (every? #(and (= "r1" (:request-id %)) (helpers (:helper %))) assignments))
    (projection/project-state! (source "helper-1" :accept) target-map
                               {:requests requests :request_assignments assignments :memberships memberships}
                               records decode-claim-outcome!)))

(defn snapshot-at-frontier!
  "Capture one completed basis on the selected node and use that exact
   system-time snapshot and a single valid-time clock for every projection
   query. This observes a completed frontier, not transaction-internal state
   or evidence of complete writer coverage."
  [ctx]
  (let [node (xtdb/connectable-from ctx)
        {:keys [basis]} (frontier/q-with-basis! node ["SELECT 1 AS ready"])
        opts {:snapshot-token (xtdb/basis-snapshot-token basis)
              :current-time (Instant/now)}
        records (atom nil)
        document-rows (atom {})
        state (snapshot! (fn [query]
                           (let [rows (xtdb/q-from node query opts)]
                             (when (= query ["SELECT * FROM gesso_commands"])
                               (reset! records (into {} (map (juxt :xt/id clojure.core/identity)) rows)))
                             (when-let [table ({["SELECT * FROM requests"] :requests
                                                ["SELECT * FROM request_assignments"] :request_assignments
                                                ["SELECT * FROM memberships"] :memberships} query)]
                               (swap! document-rows assoc table rows))
                             rows)))]
    {:basis basis :state state :records @records :document-rows @document-rows}))

(defn- require-source-lowering!
  "Check the selected source's projected literal writes and exact pre-write
   assertions. The physical mapping and extra version/subject predicates are
   fixture declarations. Require both command-ID absence and principal/
   operation slot absence, so distinct IDs cannot fill the same slot."
  [helper alternative binding ops writes]
  (let [declaration (source helper alternative)
        mapping (plan/check-target-mapping! declaration target-map)
        [slot membership] (helpers helper)
        accept? (= :accept alternative)
        replacement-fields (if accept? #{:request/status :request/revision :assignment/helper slot} #{slot})
        _ (require! (and (= replacement-fields (:writes declaration))
                         (= replacement-fields (set (keys (:replacements declaration))))
                         (every? #(= :literal (first %)) (vals (:replacements declaration)))))
        after (into {} (map (fn [[k expression]] [k (second expression)])) (:replacements declaration))
        [_ request-table request-id status-field] (get target-map :request/status)
        [_ revision-table revision-id revision-field] (get target-map :request/revision)
        [_ assignment-table assignment-id helper-field] (get target-map :assignment/helper)
        [_ member-table member-id active-field] (get target-map membership)
        [_ slot-principal slot-operation] (get target-map slot)
        _ (require! (and (= request-table revision-table) (= request-id revision-id)
                         (= helper member-id slot-principal)
                         (= replacement-fields (set (keys (:write-coordinates mapping))))))
        request (second (first (get writes request-table)))
        assignment (second (first (get writes assignment-table)))
        intents (map second (get writes :gesso_live_outbox))
        layout (cond-> {[request-table request-id]
                         [[:state :request/revision] [:extra :updated-at (if accept? "t0" "t1")]
                          [:state :request/status]]}
                 accept? (assoc [member-table member-id]
                                [[:extra :revision 0] [:extra :updated-at "t0"] [:state membership]]))
        expected (cond-> (guard/lower-assertions! declaration target-map binding layout)
                   accept? (conj (model-tx/format-assertion (model-tx/assert-one :requests
                                                                      [:and [:= :xt/id [:lift "r1"]]
                                                                       [:= :request/location-id [:lift "l1"]]])))
                   accept? (into (map (fn [intent] ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ?)" (:xt/id intent)]) intents)))
        [assertions puts] (split-with #(string? (first %)) ops)]
    (require! (and (seq puts) (every? #(= :put-docs (first %)) puts)
                   (= (frequencies expected) (frequencies assertions))
                   (= (if accept? :accepted :rejected) (get after slot))
                   (or (not accept?)
                       (= (select-keys after [:request/status :request/revision :assignment/helper])
                          {:request/status (get request status-field) :request/revision (get request revision-field)
                           :assignment/helper (get assignment helper-field)}))))))

(defn submitted-alternative!
  "Classify the selected actual synchronous submission, not a claimed plan.
   Only the fixture's canonical ASSERT/put-docs forms are recognized.
   Diagnostic only: not a general SQL parser or gateway."
  [ops]
  (require! (and (vector? ops) (seq ops)))
  (let [writes (mapcat (fn [[head table & docs :as op]]
                         (cond
                           (and (vector? op) (string? head)
                                (re-find #"(?is)^\s*ASSERT\s+" head)
                                (not (re-find #";" head))) []
                           (and (vector? op) (= :put-docs head) (keyword? table)
                                (seq docs) (every? #(and (map? %) (some? (:xt/id %))) docs))
                           (map #(vector table %) docs)
                           :else (require! false))) ops)
        by-table (group-by first writes)
        records (map second (get by-table :gesso_commands))
        record (first records)
        binding (:gesso.command/binding record)
        helper (identity/raw-value (:principal binding))
        id (identity/raw-value (:command-id binding))
        outcome (:gesso.command/outcome record)
        alternative (cond (= {:outcome :request/claimed :helper helper} outcome) :accept
                          (= {:outcome :request/not-claimable :reason :already-claimed} outcome) :refuse)
        intents (map second (get by-table :gesso_live_outbox))
        requests (map second (get by-table :requests))
        assignments (map second (get by-table :request_assignments))]
    (require! (and (= 1 (count records)) (helpers helper) (= id (:xt/id record))
                   (= :request/claim (:operation binding))
                   (= {:request-id "r1"} (:arguments binding))
                   (= :incorporated (:gesso.command/status record))
                   (some #(string? (first %)) ops)))
    (case alternative
      :accept (require! (and (= #{:requests :request_assignments :gesso_commands :gesso_live_outbox}
                               (set (keys by-table)))
                            (= 1 (count requests) (count assignments))
                            (= "r1" (:xt/id (first requests)))
                            (= {:xt/id "primary-r1" :request-id "r1"}
                               (select-keys (first assignments) [:xt/id :request-id]))
                            (= 2 (count intents) (count (set (map :xt/id intents))))
                            (= #{:request :request-assignment} (set (map :gesso.live.outbox/topic intents)))
                            (every? #(and (= id (:gesso.live.outbox/command-id %))
                                          (= "l1" (:gesso.live.outbox/subject-id %))) intents)))
      :refuse (require! (= #{:gesso_commands} (set (keys by-table))))
      (require! false))
    (require-source-lowering! helper alternative binding ops by-table)
    {:alternative (alternative-id helper alternative) :command-id id}))

(defn writer-kind!
  "Classify synchronous adapter writes in this closed fixture. Background
   writes must be confined to one manifest or outbox document; the seed must
   match exactly. This is footprint accounting, not SQL or writer closure."
  [seed source-id worker-id expected-marker ops]
  (require! (and (vector? ops) (seq ops)))
  (let [puts (filterv #(= :put-docs (first %)) ops)
        tables (set (map second puts))
        [head table row :as last-op] (peek ops)
        guards (pop ops)]
    (cond
      (= (into [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_initializations WHERE _id = ?)" source-id]
                ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_commands)"]
                ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox)"]
                ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_transition_evidence)"]
                ["ASSERT 0 = (SELECT COUNT(*) FROM memberships)"]
                ["ASSERT 0 = (SELECT COUNT(*) FROM request_assignments)"]
                ["ASSERT 0 = (SELECT COUNT(*) FROM requests)"]]
               (conj seed [:put-docs :gesso_initializations expected-marker]))
         ops) {:kind :initializer}
      (contains? tables :gesso_commands)
      (assoc (submitted-alternative! ops) :kind :claim)
      :else
      (do
        (require! (and (= 1 (count puts)) (= :put-docs head)
                       (= 3 (count last-op)) (map? row)))
        (case table
          :gesso_publication_manifests
          (do (require! (and (= source-id (:xt/id row)) (= 1 (:profile-version row))
                             (string? (:descriptor row))
                             (= [["ASSERT 0 = (SELECT COUNT(*) FROM gesso_publication_manifests WHERE _id = ?)" source-id]
                                 ["ASSERT 0 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE gesso$live$outbox$source_id = ?)" source-id]] guards)))
              {:kind :manifest})
          :gesso_live_outbox
          (let [id (:xt/id row) status (:gesso.live.outbox/status row)
                guard (first guards) deadline (:gesso.live.outbox/lease-until row)]
            (require! (and (uuid? id) (= source-id (:gesso.live.outbox/source-id row))
                           (= worker-id (:gesso.live.outbox/lease-owner row))
                           (= :request/claim (:gesso.live.outbox/operation row))
                           (= "l1" (:gesso.live.outbox/subject-id row))
                           (#{:request :request-assignment} (:gesso.live.outbox/topic row))
                           (= id (second guard))))
            (case status
              :leased (require! (and (= 3 (count guards)) (= 3 (count guard))
                                     (= "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ?)" (first guard))
                                     (= ["ASSERT CURRENT_TIMESTAMP < ?" deadline] (nth guards 1))
                                     (= ["ASSERT CURRENT_TIMESTAMP + INTERVAL '5' MINUTE >= ?" deadline] (nth guards 2))))
              :dispatch-accepted (require! (and (= 1 (count guards)) (= 4 (count guard))
                                                (= "ASSERT 1 = (SELECT COUNT(*) FROM gesso_live_outbox WHERE _id = ? AND _system_from = ? AND CURRENT_TIMESTAMP < ?)" (first guard))
                                                (= deadline (nth guard 3))))
              (require! false))
            {:kind :worker :intent-id id :status status :command-id (:gesso.live.outbox/command-id row)})
          (require! false))))))

(defn- eq [field value] [:= [:read field] [:literal value]])
(defn- conjunction [fields]
  (into [:and] (map (fn [[field value]] (eq field value))) fields))

(def open-state
  {:request/status :open :request/revision 0 :assignment/helper nil
   :journal/one nil :journal/two nil})

(defn- winner-case [helper own other]
  [:and (conjunction {:request/status :claimed :request/revision 1
                      :assignment/helper helper own :accepted})
   [:or (eq other nil) (eq other :rejected)]])

(def ^:private ownership
  {:readers (set (keys open-state))
   :expression [:or (conjunction open-state)
                (winner-case "helper-1" :journal/one :journal/two)
                (winner-case "helper-2" :journal/two :journal/one)]})

(def revision
  {:readers #{:request/revision}
   :expression [:>= [:read :request/revision] [:literal 0]]})

(def alternatives [:accept :refuse :replay-accepted :replay-rejected])

(defn catalog []
  (let [operations (vec (for [helper (sort (keys helpers)) alternative alternatives]
                          {:id (alternative-id helper alternative)
                           :construction (source helper alternative)}))]
    {:id :fixture/claim-epoch
     :declared-operation-ids (set (map :id operations))
     :initial-condition {:readers (set (keys open-state))
                         :expression [:and (conjunction open-state) (:expression revision)]}
     :invariants {:claim/ownership-and-journal ownership :claim/nonnegative-revision revision}
     :operations operations}))


(defn check-prepared-candidate!
  "Check mapped puts against the preparation captured before submission.
   The caller must first check the full SQL and publication footprint. Do not
   reacquire current state here: a stale, valid candidate must reach XTDB's
   atomic guards. The supplied preparation is trusted fixture evidence."
  [helper alternative snapshot ops]
  (let [puts (filterv #(and (= :put-docs (first %))
                            (not= :gesso_live_outbox (second %))) ops)
        checked (projection/check-put-candidate! (source helper alternative) target-map
                                                  (:document-rows snapshot) (vec (vals (:records snapshot)))
                                                  decode-claim-outcome! {} puts)]
    (require! (= (:state snapshot) (:before checked)))
    checked))

(def document-effects
  (into {} (for [helper (keys helpers) alternative alternatives]
             [(alternative-id helper alternative)
              (if (= :accept alternative)
                {[:document :requests "r1"] {:updated-at "t1"}
                 [:document :request_assignments "primary-r1"]
                 {:request-id "r1" :revision 0 :created-at "t1" :updated-at "t1" :request/location-id "l1"}}
                {})])))

(defn mapped-admission-source [bound]
  (require! (= (catalog) (:catalog bound)))
  {:inductive-source bound :target-map target-map :decode-outcome! decode-claim-outcome!
   :transaction-footprint! submitted-alternative! :auxiliary-tables #{:gesso_live_outbox}
   :document-effects document-effects})

(defn publication-mapped-admission-source [bound]
  (assoc (mapped-admission-source bound) :publication-derivations
         (into {} (for [helper (keys helpers) alternative alternatives]
                    [(alternative-id helper alternative)
                     (when (= :accept alternative)
                       {:purpose :gesso.live/refresh
                        :contributions [{:target [:document :requests "r1"]
                                         :topic :request :field :request/location-id}
                                        {:target [:document :request_assignments "primary-r1"]
                                         :topic :request-assignment :field :request/location-id}]})]))))

(defn residual-assertion-derivations []
  (into {} (for [[helper [_ membership]] helpers alternative alternatives]
             [(alternative-id helper alternative)
              (when (#{:accept :refuse} alternative)
                {:document-layout
                 (cond-> {[:requests "r1"] [[:state :request/revision]
                                              [:prestate :updated-at]
                                              [:state :request/status]]}
                   (= :accept alternative)
                   (assoc [:memberships helper] [[:prestate :revision]
                                                [:prestate :updated-at]
                                                [:state membership]]))
                 :subject-checks (if (= :accept alternative)
                                   [[:requests "r1" :request/location-id]] [])})])))

(defn physical-mapped-admission-source [bound]
  (assoc (publication-mapped-admission-source bound)
         :residual-assertion-derivations (residual-assertion-derivations)))

(defn build-prepared-admission! [bound]
  (let [source (physical-mapped-admission-source bound)
        issued (inductive-plan/build-inductive-mapped-admission! source)]
    {:issued issued
     :prepare! (partial issued :prepare!) :admit! (partial issued :admit!)
     :discard! (partial issued :discard!) :pending? (partial issued :pending?)}))

(defn correlate-history!
  "Reconcile the selected fixture's pinned state/ledger windows and actual
   adapter submission results. A committed alternative must have exactly one
   window containing its returned basis and its exact new journal record.
   Replay must refer to that record and submit nothing during its call.
   Supplied history is diagnostic, not authenticated physical provenance."
  [steps submissions]
  (let [committed (filterv #(= :committed (:result %)) submissions)
        ids (mapv #(get-in % [:identity :command-id]) committed)
        by-id (zipmap ids committed)
        incorporated (filterv #(#{:accept :refuse} (:alternative %)) steps)]
    (require! (and (= (count ids) (count (distinct ids)))
                   (= (frequencies ids) (frequencies (map :command-id incorporated)))
                   (every? #(= (:identity %) (submitted-alternative! (:ops %))) submissions)
                   (every? #(or (= :committed (:result %))
                                (and (= :xtdb/assert-failed (:result %)) (nil? (:commit-basis %)))) submissions)))
    (mapv
     (fn [{:keys [helper alternative command-id before after replay-counts]}]
       (let [replay? (#{:replay-accepted :replay-rejected} alternative)
             original (case alternative :accept :accept :refuse :refuse
                            :replay-accepted :accept :replay-rejected :refuse)
             {:keys [identity ops commit-basis]} (get by-id command-id)
             record (first (mapcat #(drop 2 %) (filter #(and (= :put-docs (first %)) (= :gesso_commands (second %))) ops)))
             before-basis (:basis before)
             after-basis (:basis after)]
         (require! (and (= identity {:alternative (alternative-id helper original) :command-id command-id})
                        (some? record) (some? commit-basis)
                        (not (pos? (xtdb/compare-bases before-basis after-basis)))
                        (= record (get (:records after) command-id))))
         (if replay?
           (require! (and (vector? replay-counts) (= 2 (count replay-counts))
                          (every? nat-int? replay-counts) (apply = replay-counts)
                          (<= (first replay-counts) (count submissions))
                          (not (pos? (xtdb/compare-bases commit-basis before-basis)))
                          (= (:records before) (:records after))))
           (require! (and (nil? replay-counts)
                          (xtdb/basis-advances? before-basis commit-basis)
                          (not (pos? (xtdb/compare-bases commit-basis after-basis)))
                          (not (contains? (:records before) command-id))
                          (= (assoc (:records before) command-id record) (:records after)))))
         ;; The existing full submission classifier checks SQL and publication
         ;; writes separately. Here the actual mapped puts determine poststate.
         (let [candidate (check-prepared-candidate! helper alternative before (if replay? [] ops))]
           (require! (= [(:state before) (:state after)] [(:before candidate) (:after candidate)])))
         {:command-id command-id :operation-id (alternative-id helper alternative)
          :commit-basis commit-basis :before-basis before-basis :after-basis after-basis
          :submission (if replay? :no-new-submission :committed-in-window)
          :observation {:construction (source helper alternative)
                        :before (:state before) :inputs {} :after (:state after)}})) steps)))

(defn bound-inductive-source!
  "Issue the existing local abstract handles for the actual full Claim catalog.
   Acknowledges, rather than discharges, every required open obligation."
  []
  (let [catalog (catalog)
        graph (closure/build-induction-dependency-graph! catalog)
        proof (induction/check-induction! catalog)
        dependency-handle (closure/check-induction-dependencies! catalog graph)
        certificate-handle
        (certificate/admit-certificate!
         {:proof-kind :conditional-induction :proof-source catalog
          :graph-handle (dependency/check-graph! graph) :current-graph graph
          :expected-open-premises #{:gesso.assurance.proof.induction/matching-declared-abstract-machine}
          :expected-open-obligations
          (set/union #{:production-transition-correspondence :current-runtime-authorization
                       :atomic-transaction-enforcement :checker-meta-soundness}
                     (disj (:open-meta-obligations proof) :certificate-admission))})]
    {:catalog catalog :certificate-handle certificate-handle
     :dependency-handle dependency-handle :current-graph graph}))
